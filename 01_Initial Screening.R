######################
# Initial screening
######################


######################
# Jugendstudie 2023
######################
jugend2023 <- readRDS("Files/jugend2023.rds")

# Wie zufrieden mit Leben insgesamt?
# 1: sehr zufrieden, zufrieden, teils/teils, unzufrieden, sehr unzufrieden
table(jugend2023$f02a)
hist(jugend2023$f02a)

table(jugend2023$geschl)

summary(jugend2023$f02a[jugend2023$geschl == 1]) # maennlich
summary(jugend2023$f02a[jugend2023$geschl == 2]) # weiblich
summary(jugend2023$f02a[jugend2023$geschl == 3]) # divers

prop.table(table(jugend2023$f02a[jugend2023$geschl == 1])) # maennlich
prop.table(table(jugend2023$f02a[jugend2023$geschl == 2])) # weiblich
prop.table(table(jugend2023$f02a)) # alle

summary(jugend2023$wph) # standardgewicht
sum(jugend2023$wph) # not normalized it seems

# weighted frequency table
weighted_freq <- tapply(jugend2023$wph, jugend2023$f02a, sum)
rel_weighted_freq <- weighted_freq / sum(jugend2023$wph)
rel_weighted_freq

# weights dont seem to make a big difference here

######################
# Jugendstudie 2010
######################
jugend2010 <- readRDS("Files/umfj2010.rds")
schul2010 <- readRDS("Files/umfschule2010.rds") # Whats the difference here

which(jugend2010$pnum %in% schul2010$pnum)

erwerb2010 <- readRDS("Files/umfj2010_erwerbverlauf.rds")
summary(jugend2010$pnum)
summary(schul2010$pnum)

# Wie zufrieden zur Zeit
table(jugend2010$j02a)

table(jugend2010$geschl) # 1 maennlich, 2 weiblich

prop.table(table(jugend2010$j02a[jugend2010$geschl == 1])) # maennlich
prop.table(table(jugend2010$j02a[jugend2010$geschl == 2])) # weiblich

mean(jugend2010$j02a[jugend2010$geschl == 1], na.rm = TRUE) # maennlich, 2010
mean(jugend2010$j02a[jugend2010$geschl == 2], na.rm = TRUE) # weiblich, 2010

mean(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE) # maennlich, 2023
mean(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE) # weiblich, 2023

sd(jugend2010$j02a, na.rm = TRUE) # 0.87
sd(jugend2023$f02a, na.rm = TRUE) # 0.85

# Diff girls
(mean(jugend2010$j02a[jugend2010$geschl == 2], na.rm = TRUE) - mean(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE))/
  0.86
# - 0.3

# Diff boys
(mean(jugend2010$j02a[jugend2010$geschl == 1], na.rm = TRUE) - mean(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE))/
  0.86
# 0.13

######################
# Jugendstudie 2015
######################

library(haven)

#jugend2015_asc <- read_spss("Files/jugend2015_asc.sav")
#jugend2015_kl <- read_spss("Files/jugend2015_kl.sav")
#jugend2015_R <- read_spss("Files/jugend2015_R.sav")
jugend2015 <- read_spss("Files/jugend2015.sav")
#jugend2015schule <- read_spss("Files/jugend2015schule.sav")


table(jugend2015$f02a)

table(jugend2015$geschlecht) # 1 maennlich, 2 weiblich

prop.table(table(jugend2015$f02a[jugend2015$geschlecht == 1])) # maennlich
prop.table(table(jugend2015$f02a[jugend2015$geschlecht == 2])) # weiblich



######################
# Make some first plot
######################


mean(schul2010$s02a[schul2010$geschl == 2], na.rm = TRUE) # weiblich, 2010

mean(jugend2015$f02a[jugend2015$geschlecht == 1], na.rm = TRUE) # maennlich, 2015
mean(jugend2015$f02a[jugend2015$geschlecht == 2], na.rm = TRUE) # weiblich, 2015

mean(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE) # maennlich, 2023
mean(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE) # weiblich, 2023


mean_satis <- data.frame(matrix(NA, nrow = 6, ncol = 5))
names(mean_satis) <- c("Year", "Gender", "M", "lb", "ub")
mean_satis$Year <- rep(c(2010, 2015, 2023), each = 2)
mean_satis$Gender <- rep(c("m", "f"), times = 3)

mean_satis[mean_satis$Year == 2010 & mean_satis$Gender == "m",]$M <- t.test(schul2010$s02a[schul2010$geschl == 1], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2010 & mean_satis$Gender == "m", c("lb", "ub")] <- as.numeric(t.test(schul2010$s02a[schul2010$geschl == 1], na.rm = TRUE)$conf.int)

mean_satis[mean_satis$Year == 2010 & mean_satis$Gender == "f",]$M <- t.test(schul2010$s02a[schul2010$geschl == 2], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2010 & mean_satis$Gender == "f", c("lb", "ub")] <- t.test(schul2010$s02a[schul2010$geschl == 2], na.rm = TRUE)$conf.int

mean_satis[mean_satis$Year == 2015 & mean_satis$Gender == "m",]$M <- t.test(jugend2015$f02a[jugend2015$geschlecht == 1], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2015 & mean_satis$Gender == "m", c("lb", "ub")] <- t.test(jugend2015$f02a[jugend2015$geschlecht == 1], na.rm = TRUE)$conf.int

mean_satis[mean_satis$Year == 2015 & mean_satis$Gender == "f",]$M <- t.test(jugend2015$f02a[jugend2015$geschlecht == 2], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2015 & mean_satis$Gender == "f", c("lb", "ub")] <- t.test(jugend2015$f02a[jugend2015$geschlecht == 2], na.rm = TRUE)$conf.int

mean_satis[mean_satis$Year == 2023 & mean_satis$Gender == "m",]$M <- t.test(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2023 & mean_satis$Gender == "m", c("lb", "ub")] <- t.test(jugend2023$f02a[jugend2023$geschl == 1], na.rm = TRUE)$conf.int

mean_satis[mean_satis$Year == 2023 & mean_satis$Gender == "f",]$M <- t.test(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE)$estimate
mean_satis[mean_satis$Year == 2023 & mean_satis$Gender == "f", c("lb", "ub")] <- t.test(jugend2023$f02a[jugend2023$geschl == 2], na.rm = TRUE)$conf.int

flipped <- mean_satis
flipped$M <- 6 - mean_satis$M
flipped$lb <- 6 - mean_satis$ub
flipped$ub <- 6 - mean_satis$lb
View(flipped)

library(ggplot2)
dodge_width <- 0.4

ggplot(data = flipped, aes(x = Year, y = M, ymin = lb, ymax = ub, group = Gender, color = Gender)) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(position = position_dodge(width = dodge_width), width = .3) +
  theme_classic() +
  coord_cartesian(ylim = c(3, 4.5)) +
  ylab("Satisfied with life (Scale: 1-5)") +
  scale_x_continuous(breaks = c(2010, 2015, 2023))
ggsave("Plots/mean_satis.png", width = 4, height = 3)
