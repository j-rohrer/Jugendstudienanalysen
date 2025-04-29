#######################
# Redo age with spline
#######################


load("prep.RData")

library(splines)
library(marginaleffects)
library(ggplot2)

# Check whether marginaleffects generally works with splines

# Spline model
add_age <- lm(satis ~ gender*as.factor(year)*bs(age, df = 4), 
              data = combined)
summary(add_age)

# Generate predictions for combinations of gender and year and age
pred <- predictions(add_age,
                    by = c("gender", "year", "age"))

# Visualize
# 2010
cat_age_2010 <- ggplot(pred[pred$year == 2010,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("2010") +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender")
cat_age_2010

# 2015
cat_age_2015 <- ggplot(pred[pred$year == 2015,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("2015") +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender")

# 2023
cat_age_2023 <- ggplot(pred[pred$year == 2023,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("2023") +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender")

# Combine plots
library(patchwork)
combined_cat_age <- (cat_age_2010|cat_age_2015|cat_age_2023) + plot_layout(guides = "collect")
combined_cat_age


# Plot the results
# 2010
lin_age_2010 <- ggplot(pred[pred$year == 2010,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("2010") +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender")

# 2015
lin_age_2015 <- ggplot(pred[pred$year == 2015,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender") +
  ggtitle("2015")

# 2023
lin_age_2023 <- ggplot(pred[pred$year == 2023,], aes(x = age, group = gender, 
                                                     color = gender,
                                                     y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  geom_errorbar(width = .5, position = position_dodge(width = .5)) +
  coord_cartesian(ylim = c(mean(combined$satis) - sd(combined$satis), mean(combined$satis) + sd(combined$satis))) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean life satisfaction") +
  xlab("Age") +
  labs(color = "Gender") +
  ggtitle("2023")

# Combine into single plot
combined_lin_age <- (lin_age_2010|lin_age_2015|lin_age_2023) + plot_layout(guides = "collect")
combined_lin_age