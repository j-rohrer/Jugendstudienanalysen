#########################
# How about fixed effects
#########################


load("prep.RData")


fe_model <- lm(scale(satis) ~ gender + as.factor(year) + gender:as.factor(year) + as.factor(unique_classroom), data = combined)
summary(fe_model)$coefficients[!grepl("as.factor\\(\\unique_classroom)", rownames(summary(fe_model)$coefficients)), ]

# fixed effects individual slopes maybe?

# Interaction with source?


# I think I will have to plot some means here

# mean by gender, by year, by source

library(dplyr)
library(tidyr)
library(haven)

compute_summary <- function(x) {
  n <- length(x)
  mean_val <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(n)  # Standard error
  ci <- qt(0.975, df = n - 1) * se    # 95% CI margin
  lower <- mean_val - ci
  upper <- mean_val + ci
  
  data.frame(
    mean = mean_val,
    ci_lower = lower,
    ci_upper = upper
  )
}

summary_data <- combined %>%
  group_by(gender, year, source) %>%
  summarise(
    mean = mean(satis, na.rm = TRUE),
    ci_lower = mean(satis, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(satis, na.rm = TRUE) / sqrt(n())),
    ci_upper = mean(satis, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(satis, na.rm = TRUE) / sqrt(n())),
    .groups = "drop"
  )

print(summary_data)
summary_data <- summary_data[1:8,]
library(ggplot2)

ggplot(summary_data, aes(y = mean, x = year, by = gender, color = gender)) +
  geom_point() +
  geom_line() +
  theme_minimal()


# okay, so they really do go down in parallel if we do only look at the paper
# format
         
