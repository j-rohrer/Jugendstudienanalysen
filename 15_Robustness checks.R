#####################################
# Various robustness checks
#####################################


# The original finding
library(marginaleffects)
library(ggplot2)

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)
# generate predictions for combinations of gender and year
pred <- predictions(simple_model,
                    by = c("gender", "year"))

ggplot(pred, aes(x = year, group = gender, color = gender, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_line() +
  geom_errorbar(width = .5) +
  scale_x_continuous(breaks = c(2010, 2015, 2023)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  theme_classic() +
  ylab("Mean life satisfaction in SD") +
  xlab("Survey year")

##############################################
# The explanation: Adding in migback and source
##############################################
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source), data = combined, weights = wph)
summary(simple_model)

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p < .001

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .361


############################################
# Does this also hold without mode in the model?
############################################
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home), data = combined, weights = wph)
summary(simple_model)

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home. p < .001

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# still a significant difference here! So we do need to account for source to make it go away .001

##############################################
# Let us try without weights
##############################################
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source), data = combined)
summary(simple_model)

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p = .001

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .343

##############################################
# Lets try with schooltype in model
##############################################
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source) +
                     as.factor(schooltype)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p < .001

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .22


##############################################
# Lets try with school fixed effects
##############################################
simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source) +
                     as.factor(school_id), data = combined, weights = wph)
summary(simple_model)

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p = .002

avg_slopes(simple_model, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .378


##############################################
# Lets try with a multilevel model
##############################################
library(lmerTest)
library(lme4)

# WHY DO I GET A PERFECT CORRELATION HERE
# Singular fit but it still fits my hypothesis here
# Perfect collinearity between source and gender?

# Again: when I add random slopes for gender plus the interaction between source and gender
# I get a perfect correlation between gendermale and the random intercepts

simple_mlm <- lmer(scale(satis) ~ as.factor(year)*gender*as.factor(german_at_home) + as.factor(source)*gender + (1 + gender|unique_classroom) ,
                 data = combined, weights = wph)
summary(simple_mlm)

avg_slopes(simple_mlm, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p = .002

avg_slopes(simple_mlm, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .378



# try another version
simple_mlm <- lmer(scale(satis) ~ as.factor(year)*gender*as.factor(german_at_home) + (1 + gender|unique_classroom) + (1 + gender|source),
                   data = combined, weights = wph)
summary(simple_mlm)
# but this is still singular

avg_slopes(simple_mlm, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p = .002

avg_slopes(simple_mlm, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .378

# Findings still work tho

##############################################
# Maybe source needs to be a nested predictor?
##############################################
# But this way does not seem to be working right

##############################################
# New try multilevel model
##############################################
