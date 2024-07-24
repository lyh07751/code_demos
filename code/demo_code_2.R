# This R script simulated data for a longitudinal study with a treatment effect and time effect on blood pressure. 
# It then fit a linear mixed-effects model to the data and plotted the results.
# By including both fixed and random effects, this model can provide more accurate estimates by accounting for 
# both the overall effects of treatment and time, as well as individual differences between patients.

install.packages("lme4", "Matrix")
library(lme4)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simulate data
n_patients <- 100
n_timepoints <- 5
patients <- factor(rep(1:n_patients, each = n_timepoints))
time <- rep(1:n_timepoints, times = n_patients)
treatment <- factor(rep(c("Control", "Treatment"), each = (n_patients * n_timepoints) / 2))

# Random effects
# Each patient has a random intercept (baseline blood pressure) 
# and a random slope (rate of change over time)
patient_intercepts <- rnorm(n_patients, mean = 0, sd = 2) # Random intercepts
patient_slopes <- rnorm(n_patients, mean = 0, sd = 0.5)  # Random slopes for time

# Fixed effects
beta_0 <- 120  # Average baseline blood pressure
beta_1 <- -10  # Treatment effect (drug lowers blood pressure by 10 units)
beta_2 <- -2   # Time effect (blood pressure decreases by 2 units per time point)

# Generate response variable (blood pressure)
blood_pressure <- beta_0 + 
  beta_1 * as.numeric(treatment == "Treatment") +
  beta_2 * time + 
  patient_intercepts[as.numeric(patients)] +
  patient_slopes[as.numeric(patients)] * time + 
  rnorm(n_patients * n_timepoints, mean = 0, sd = 1)

# Create a dataframe
data <- data.frame(patients, time, treatment, blood_pressure)

# Fit the linear mixed-effects model
# The model includes fixed effects for treatment, time, and their interaction
# It also includes random effects for intercept and slope for each patient
# Random effects are grouped by patients: Each patient is allowed to have their own intercept and slope for time.
# "1 + time": This indicates that we are including a random intercept (1) and a random slope (time) for each patient.

model <- lmer(blood_pressure ~ treatment * time + (1 + time | patients), data = data)

# Display the summary of the model
# The summary of the model will provide estimates for the fixed effects 
# (the treatment effect and time effect)
# It will also provide estimates for the variance of the random effects 
# (how much patients differ in their baseline blood pressure and rates of change).
summary(model)

# Plot the results
# Aggregate data for plotting
agg_data <- data %>%
  group_by(time, treatment) %>%
  summarize(mean_bp = mean(blood_pressure),
            se_bp = sd(blood_pressure) / sqrt(n())) %>% 
  ungroup() %>% 
  mutate(time = case_when(
    time == 1 ~ "Week 1",
    time == 2 ~ "Week 2",
    time == 3 ~ "Week 3",
    time == 4 ~ "Week 4",
    time == 5 ~ "Week 5"
  ))

# Plot
ggplot(agg_data, aes(x = time, y = mean_bp, color = treatment, group = treatment)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_bp - se_bp, ymax = mean_bp + se_bp), width = 0.2) +
  labs(title = "Blood Pressure over Time by Treatment",
       subtitle = "Linear Mixed-Effects Model",
       x = "",
       y = "Mean Blood Pressure") +
  scale_color_manual(values = c(
    "Control"="#5995ED",
    "Treatment"="#fbb7c0")) +
  scale_y_continuous(limits = c(95, 120),
                     breaks = c(95, 100, 105, 110, 115, 120),
                     labels = c(95, 100, 105, 110, 115, 120)) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
    )
