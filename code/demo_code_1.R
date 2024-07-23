# install.packages("readr", "RCurl", "ggh4x", "ggprism")

### Load the libraries
library(readr)
library(RCurl)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(ggprism)

### Load the self-defined functions
source("~/code_demos/code/functions.R") 


### Read in the dataset
# d.adis <- read_csv("~/code_demos/data/dummy_adis.csv")
d.adis <- read.csv(text = getURL("https://raw.githubusercontent.com/lyh07751/code_demos/main/data/dummy_adis.csv"))


######### Data Processing #########
###### Create a dummy group variable
df_group <- data.frame(SUBJID = unique(d.adis$SUBJID),
                       GROUP = sample(c("Group1", "Group2", "Group3", "Group4"), length(unique(d.adis$SUBJID)), replace = TRUE))
d.adis <- merge(d.adis, df_group, by = "SUBJID")


###### Filter the analysis set
###### Rename the visit and treatment columns
tbl_1 <- d.adis %>% 
  filter(ANALFG1 == "Y",
         AVISIT %in% c("Visit1", "Visit2", "Visit3", "Visit4")) %>% 
  mutate(AVISIT = case_when(
    AVISIT == "Visit1" ~ "V1",
    AVISIT == "Visit2" ~ "V2",
    AVISIT == "Visit3" ~ "V3",
    AVISIT == "Visit4" ~ "V4"
  )) %>% 
  mutate(TRT01P = case_when(
    TRT01P == "Placebo" ~ "Saline",
    T ~ TRT01P
  ))


###### Group by treatment, visit, parameter code and group
###### Count the number of objects
tbl_grp_count <- tbl_1 %>% 
  filter(!is.na(AVAL)) %>%
  group_by(TRT01P, AVISIT, PARAMCD, GROUP) %>%
  summarise(n.obj = n()) 


###### prepare the plot table
###### calculate the group mean, confidence interval,
###### and fold change compared with baseline visit
tbl_plot <- tbl_1 %>% 
  arrange(SUBJID, AVISIT) %>%
  group_by(GROUP, AVISIT, PARAMCD, TRT01P) %>%
  filter(!is.na(AVAL)) %>%
  summarise(
    N.subj = n(),
    Group_mean = mean(AVAL),
    LCI = tryCatch(
     tidy(t.test(AVAL))$conf.low,
      error = function(z)
        return(NA)),
    UCI = tryCatch(
      tidy(t.test(AVAL))$conf.high,
      error = function(z)
        return(NA))) %>% 
  mutate(group = factor(GROUP,levels=c("Group1", "Group2", "Group3", "Group4"))) %>% 
  mutate(AVISIT = factor(AVISIT,levels=c("V1", "V2", "V3", "V4"))) %>% 
  ungroup() %>% 
  arrange(GROUP, PARAMCD, TRT01P, AVISIT) %>%
  group_by(GROUP, PARAMCD, TRT01P) %>%
  mutate(Fold_chg = ifelse(AVISIT == "V1", 1, Group_mean / Group_mean[AVISIT == "V1"])) %>% 
  ungroup()




#' Generate a Barplot with Confidence Intervals Faceted by Group and Treatment
#'
#' @description The `plot_bar` function creates a barplot with confidence intervals, faceted by group and treatment. The plot is displayed on a log10 scale for better visualization.
#'
#' @param param Character vector. The parameter code(s) to filter the dataset for plotting.
#' @param plot_title Character. The title of the plot. Default is "Sample Barplot with Confidence Interval Facet by Group and Treatment".
#' @param plot_subtitle Character. The subtitle of the plot. Default is "Plots are displayed on log10 scale for better visualization".
#' @param plot_xlab Character. The label for the x-axis. Default is "Visit".
#' @param plot_ylab Character. The label for the y-axis. Default is "log10(Group Mean)".
#' @param plot_caption Character. The caption for the plot. Default is "The above plot is generated using dummy dataset".
#'
#' @return A `ggplot` object representing the barplot with confidence intervals.
#'
#' @examples
#' plot_bar(param = "Ab1")
#' plot_bar(param = c("Ab2", "Ab8"), plot_title = "Custom Title")
#'
#' @details
#' This function creates a barplot using `ggplot2`, displaying group means on a log10 scale 
#' with confidence intervals. The plot is faceted by parameter code (`PARAMCD`) and treatment group (`TRT01P`). 
#' Additional customization options are available for the plot's title, subtitle, axis labels, and caption.
#'
#' @export
  


plot_bar <- function(param, plot_title = "Sample Barplot with Confidence Interval Facet by Group and Treatment", 
                     plot_subtitle = "Plots are displayed on log10 scale for better visualization", 
                     plot_xlab = "Visit", plot_ylab = "log10(Group Mean)", 
                     plot_caption = "The above plot is generated using dummy dataset") {
  
  tbl_note = tbl_plot %>%
    mutate(group1 = "V1",
           group2 = AVISIT,
           y.position = case_when(
             group2 == "V2" ~ log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE))*1.1,
             group2 == "V3" ~ log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE))*1.2,
             group2 == "V4" ~ log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE))*1.3
           )) 
  
  p <- ggplot(data = tbl_plot %>% filter(PARAMCD %in% param), 
              aes(y = log10(Group_mean), x = AVISIT, color = TRT01P)) +
    ### plot the bar
    geom_col(data = tbl_plot %>% filter(PARAMCD %in% param), fill = NA) +
    ### plot the confidence interval
    geom_errorbar(data = tbl_plot %>% filter(PARAMCD %in% param), 
                  aes(ymin = log10(LCI), ymax = log10(UCI)), width = 0.2) +
    ### facet by group
    facet_nested(PARAMCD + factor(group,levels=c("Group1", "Group2", "Group3", "Group4")) ~ TRT01P) +
    ### add the values
    geom_text(data = tbl_plot %>% 
                filter(PARAMCD %in% param) %>%
                mutate(ylab = log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE)) * 1.55),
              aes(x = AVISIT,y = ylab, 
                  label = paste0(format(round_sas(Group_mean, 1), nsmall = 1), "\nn=", N.subj)), 
              color = "black", size = 3) +
    ### add the fold change
    add_pvalue(data = tbl_note %>%
                 filter(PARAMCD %in% param),
               label = "~{trimws(fmt_num(Fold_chg, digits=2))}x",
               fontface="bold",
               color = "black",
               label.size=2.5) +
    ### adjust y axis scales
    scale_y_continuous(limits = c(0, log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE)) * 1.7),
                       breaks = custom_breaks(log10(tbl_plot %>% filter(PARAMCD %in% param) %>% select(Group_mean) %>% max(na.rm = TRUE)), 3)) +
    scale_color_manual(values = c(
      "Saline"="#5995ED",
      "Treatment1"="#fbb7c0",
      "Treatment2"="#D76A03", 
      "Treatment3"="#44CF6C",
      "Treatment4"="#FDCA40",
      "Treatment5"="#8332AC",
      "Treatment6"="#034732",
      "Treatment7"="#d53e4f"
    )) + 
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = plot_caption,
         x = plot_xlab, y = plot_ylab) +
    theme_bw() +
    theme(legend.position = "none", 
          # remove the legend from the plot
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          # remove the major and minor grid lines
          panel.background = element_blank(),
          # remove the background of the panel
          strip.background = element_rect(fill = "white", colour = "black"),
          # remove the background color of the facet strip
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10),
          # customize the text of the strip labels in a faceted plot
          panel.spacing.x = unit(.1, "line"),
          # customize the spacing between two facets
          panel.border = element_part_rect(color = "gray" , fill = NA),
          # modify the appearance of the border around the facets
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          # customize the appearance of the axis lines
          axis.text.x = element_text(size = 8.5, angle = 45),
          axis.text.y = element_text(size = 8.5),
          # customize the text of the axis labels 
          plot.caption = element_text(hjust = 0)
          # customizes the caption text of the plot: 0-left, 0.5-center, 1-right
    )
  
  return(p)
  
}





plot_bar(param = "Ab1")  
plot_bar(param = "Ab2")    
plot_bar(param = "Ab3")
plot_bar(param = "Ab4")

