########################################
# Additional outcomes
########################################

load("prep.RData")

#############################
# All shared importance items
#############################
library(haven)
library(marginaleffects)
library(ggplot2)
library(gridExtra)


# Create function that takes variable as input
# returns meansplot, separately for German at home versus not
# determine reasonable range for the plots
apply(combined[, imp_names], 2, function(x) mean(x, na.rm = TRUE))
apply(combined[, imp_names], 2, function(x) sd(x, na.rm = TRUE))

plot_means <- function(variable, yrange = c(2.5, 5)) {
  combined$var <- combined[, variable]
  german <- lm(var ~ as.factor(year)*as.factor(gender) + as.factor(school_id) + as.factor(source)*as.factor(gender), data = combined[combined$german_at_home == 1,], weights = wph)
  no_german <- lm(var ~ as.factor(year)*as.factor(gender) + as.factor(school_id) + as.factor(source)*as.factor(gender), data = combined[combined$german_at_home == 0,], weights = wph)
  
  
  pred_german <- predictions(german,
                      by = c("gender", "year", "source"))
  
  plot_german <- ggplot(pred_german, aes(x = year, group = interaction(gender, source), shape = gender, color = source, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_line(position = position_dodge(width = 0.4)) +
    geom_errorbar(width = .5, position = position_dodge(width = 0.4)) +
    scale_x_continuous(breaks = c(2010, 2015, 2023)) +
    coord_cartesian(ylim = yrange) +
    theme_classic() +
    ylab("Variable") +
    xlab("Survey year") +
    ggtitle(paste(variable, "German at home", sep = ", "))
  
  pred_no_german <- predictions(no_german,
                             by = c("gender", "year", "source"))
  
  plot_no_german <- ggplot(pred_no_german, aes(x = year, group = interaction(gender, source), shape = gender, color = source, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_line(position = position_dodge(width = 0.4)) +
    geom_errorbar(width = .5, position = position_dodge(width = 0.4)) +
    scale_x_continuous(breaks = c(2010, 2015, 2023)) +
    coord_cartesian(ylim = yrange) +
    theme_classic() +
    ylab("Variable") +
    xlab("Survey year") +
    ggtitle(paste(variable, "No German at home", sep = ", "))
  
  grid.arrange(plot_german, plot_no_german, ncol = 2)
  
  
}

plot_means("imp_law")
plot_means("imp_standard")
plot_means("imp_career")
plot_means("imp_politics")
plot_means("imp_enjoy")
plot_means("imp_health")
plot_means("imp_environment")
plot_means("imp_participate")
plot_means("imp_friends")
plot_means("imp_kids")

ggsave(plot = plot_means("imp_politics"), filename = "Plots/imp_politics.png", width = 8, height = 4)
ggsave(plot = plot_means("imp_enjoy"), filename = "Plots/imp_enjoy.png", width = 8, height = 4)
ggsave(plot = plot_means("imp_friends"), filename = "Plots/imp_friends.png", width = 8, height = 4)


check_friends <- lm(scale(imp_friends) ~ as.factor(year), data = combined, weights = wph)
summary(check_friends)




#############################
# Problem items
#############################


test_2023 <- lm(prob_parents ~ as.factor(gender)*as.factor(german_at_home),
                                                     data = combined[combined$year == 2023,])
summary(test_2023)

pred_parental_2023 <- predictions(test_2023,
                           by = c("gender", "german_at_home"))
# girls with migration background report most trouble


test_2015 <- lm(prob_parents ~ as.factor(gender)*as.factor(german_at_home),
           data = combined[combined$year == 2015,])
summary(test_2015)

pred_parental_2015 <- predictions(test_2015,
                             by = c("gender", "german_at_home"))
# Boys German at home reported most trouble

test_2010 <- lm(prob_parents ~ as.factor(gender)*as.factor(german_at_home),
                data = combined[combined$year == 2010,])
summary(test_2010)

pred_parental_2010 <- predictions(test_2010,
                                  by = c("gender", "german_at_home"))
# Boys german at home reported more trouble

#############################
# Discrimination items
#############################

plot_means("disc_city", yrange = c(1,3))
plot_means("disc_language", yrange = c(1,3))
plot_means("disc_sex", yrange = c(1,3))
plot_means("disc_school", yrange = c(1,3))


plot_means("satis_money")
plot_means("satis_friends")
plot_means("satis_dwell")
plot_means("satis_leisure")
plot_means("satis_mom")
plot_means("satis_dad")

# Wow: Online seems to make quite the differences for the other satisfaction variables

# money
# girls happier on paper
test_money <- lm(satis_money ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_money <- predictions(test_money, by = c("gender", "source"))
predictions(test_money, by = c("gender", "source"), hypothesis = "b1 = b2") # girls online lower
predictions(test_money, by = c("gender", "source"), hypothesis = "b3 = b4") # boys online same

# friends
# source makes no difference for either sex
test_friends <- lm(satis_friends ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_friends <- predictions(test_friends, by = c("gender", "source"))
predictions(test_friends, by = c("gender", "source"), hypothesis = "b1 = b2") 
predictions(test_friends, by = c("gender", "source"), hypothesis = "b3 = b4") 

# mom
# girls slightly unhappier online
test_mom <- lm(satis_mom ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_mom <- predictions(test_mom, by = c("gender", "source"))
predictions(test_mom, by = c("gender", "source"), hypothesis = "b1 = b2") # girls slightly unhappier online 
predictions(test_mom, by = c("gender", "source"), hypothesis = "b3 = b4") # boys same same

# dad
# girls unhappier online
test_dad <- lm(satis_dad ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_dad <- predictions(test_dad, by = c("gender", "source"))
summary(test_dad)
pred_dad
predictions(test_dad, by = c("gender", "source"), hypothesis = "b1 = b2")  # girls slightly unhappier online
predictions(test_dad, by = c("gender", "source"), hypothesis = "b3 = b4")  # boys no diff

# leisure
# boys happier on tablet
test_leisure <- lm(satis_leisure ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_leisure <- predictions(test_leisure, by = c("gender", "source"))
summary(test_leisure)
pred_leisure
predictions(test_leisure, by = c("gender", "source"), hypothesis = "b1 = b2") # no diff for girls
predictions(test_leisure, by = c("gender", "source"), hypothesis = "b3 = b4") # boys happier on tablet!

# dwell
test_dwell <- lm(satis_dwell ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_dwell <- predictions(test_dwell, by = c("gender", "source"))
summary(test_dwell)
pred_dwell
predictions(test_dwell, by = c("gender", "source"), hypothesis = "b1 = b2") # girls less satisfied online
predictions(test_dwell, by = c("gender", "source"), hypothesis = "b3 = b4") 


# grades
# girls less satisfied online, boys more satisfied online
test_grades <- lm(satis_grades ~ as.factor(gender)*as.factor(source), data = combined[combined$year == 2023,])
pred_grades <- predictions(test_grades, by = c("gender", "source"))
summary(test_grades)
pred_grades
predictions(test_grades, by = c("gender", "source"), hypothesis = "b1 = b2") 
predictions(test_grades, by = c("gender", "source"), hypothesis = "b3 = b4") 


# interact with migration background
