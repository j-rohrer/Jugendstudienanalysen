#####################################
# Various robustness checks
#####################################

load("prep.RData")

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
library(fixest)

simple_model <- lm(scale(satis) ~ as.factor(year)*as.factor(gender), data = combined, weights = wph)
summary(simple_model)
# generate predictions for combinations of gender and year
pred <- predictions(simple_model,
                    by = c("gender", "year"))

simple_model_clustered <- feols(scale(satis) ~ as.factor(year)*as.factor(gender), 
                      data = combined, 
                      cluster = ~ unique_classroom)
summary(simple_model_clustered)
summary(simple_model)
pred_clustered <- predictions(simple_model_clustered,
                    by = c("gender", "year"))
pred
pred_clustered


# With clustering
explanation_simple <- lm(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source), data = combined, weights = wph)

explanation_clustered <- feols(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + as.factor(gender)*as.factor(source), 
                            data = combined, 
                            cluster = ~ unique_classroom)
summary(explanation_clustered)
summary(explanation_simple)

avg_slopes(explanation_simple, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
# significant diff for no german at home, p < .001

avg_slopes(explanation_simple, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")
# no more significant diff german at home, p = .361

avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0")
avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0")

# Compare them against each other: p = .02

avg_slopes(explanation_clustered, 
           variables = "year", 
           by = c("german_at_home", "gender"), 
           hypothesis = "b7 - b8 = b5 - b6")

avg_slopes(explanation_simple, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = b5 - b6") # without clustering: .004

# Clustered standard error fixed effects
explanation_clustered <- feols(scale(satis) ~ as.factor(year)*as.factor(gender)*as.factor(german_at_home) + 
                                 as.factor(gender)*as.factor(source)|school_id, 
                               data = combined, 
                               cluster = ~ unique_classroom)
summary(explanation_clustered)

avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"))
avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = 0") # p = .017
avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = 0") # p = .35
avg_slopes(explanation_clustered, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 - b8 = b5 - b6") # .038


##################################################
# Ordinal, with clustered standard errors
##################################################
library(ordinal)
library(haven)

combined$satis <- as.factor(combined$satis)
combined$year <- as.factor(combined$year)
combined$gender <- as.factor(combined$gender)
simple_model_ordinal <- clm(satis ~ year*gender, data = combined)
summary(simple_model_ordinal)

avg_slopes(simple_model_ordinal, variables = "year", by = c("gender"),
           hypothesis = "b3 - b4 = 0") # p < .001
avg_slopes(simple_model_ordinal, variables = "year", by = c("gender"),
           hypothesis = "b7 - b8 = 0") # p < .001
avg_slopes(simple_model_ordinal, variables = "year", by = c("gender"),
           hypothesis = "b11 - b12 = 0") # p < .001
avg_slopes(simple_model_ordinal, variables = "year", by = c("gender"),
           hypothesis = "b15 - b16 = 0") # p < .001
avg_slopes(simple_model_ordinal, variables = "year", by = c("gender"),
           hypothesis = "b19 - b20 = 0") # p = 0176

combined$source <- as.factor(combined$source)
explanation_ordinal <- clm(satis ~ year*gender*german_at_home + 
                            gender*source, data = combined)
summary(explanation_ordinal)

explanation_slopes <- avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"))
test <- cbind(line = 1:nrow(explanation_slopes), explanation_slopes)

# group 1, no german: b5 = b6
# group 1, german: b7 = b8
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 = b6") # .006
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b7 = b8") # .009

avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b5 - b6 = b7 - b8") # .0865


# group 2, no german: 13 = 14
# group 2, german: 15 = 16
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b13 = b14") # .002
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b15 = b16") # .01

avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b13 - b14 = b15 - b16") # .063

# group 3, no german: 21 = 22
# group 3, german: 23 = 24
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b21 = b22") # .004
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b23 = b24") # .116

avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b21 - b22 = b23 - b24") # .029

# group 4, no german: 29 = 30
# group 4, german: 31 = 32
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b29 = b30") # .004
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b31 = b32") # <.001

avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b29 - b30 = b31 - b32") # .42

# group 5, no german: 37 = 38
# group 5, german: 39 = 40
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b37 = b38") # .019
avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b39 = b40") # .861

avg_slopes(explanation_ordinal, variables = "year", by = c("german_at_home", "gender"), hypothesis = "b37 - b38 = b39 - b40") # .023

print(explanation_slopes, nrows = 100)

##############################################
# Okay, we need to set up a full-blown model
##############################################




# best effort
# mixed effects with classroom random effects
# school fixed effects
