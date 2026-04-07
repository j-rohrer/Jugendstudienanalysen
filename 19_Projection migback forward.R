#################################
# Projecting Migback forward
#################################
# That does not seem to work

load("prep.RData")
library(haven)
library(marginaleffects)
library(ggplot2)
library(gridExtra)
library(dplyr)



# self born in Germany? if no, which country
table(youth2010$s23aa)
# father born in Germany? if no, which country
table(youth2010$s23ba)

# mother born in Germany? if no, which country
table(youth2010$s23ca)



unique_countries <- unique(c(youth2010$s23aa,
                             youth2010$s23ba,
                             youth2010$s23ca))
unique_countries <- unique_countries[!is.na(unique_countries)]
unique_countries <- sort(unique_countries)

key_table <- data.frame(matrix(NA, ncol = 2,
                               nrow = length(unique_countries)))
names(key_table) <- c("code", "country")
key_table$code <- unique_countries

key_table$n <- NA
for (i in 1:nrow(key_table)) {
  key_table$n[i] <- sum(youth2010$s23aa == key_table$code[i], na.rm = TRUE) +
    sum(youth2010$s23ba == key_table$code[i], na.rm = TRUE) +
    sum(youth2010$s23ca == key_table$code[i], na.rm = TRUE)
}

key_table$country[key_table$n < 5] <- "small"

key_table$code[is.na(key_table$country)]

key_table$country[key_table$code == "121"] <- "Albania"
key_table$country[key_table$code == "125"] <- "Bulgaria"
key_table$country[key_table$code == "126"] <- "Denmark"
key_table$country[key_table$code == "137"] <- "Italy"
key_table$country[key_table$code == "142"] <- "Lithuania"
key_table$country[key_table$code == "151"] <- "Austria"
key_table$country[key_table$code == "152"] <- "Poland"
key_table$country[key_table$code == "160"] <- "Russia"

key_table$country[key_table$code == "163"] <- "Turkey"
key_table$country[key_table$code == "165"] <- "Hungary"
key_table$country[key_table$code == "166"] <- "Ukraine"
key_table$country[key_table$code == "170"] <- "Serbia"

key_table$country[key_table$code == "221"] <- "Algeria"
key_table$country[key_table$code == "254"] <- "Mozambique"
key_table$country[key_table$code == "351"] <- "Cuba"
key_table$country[key_table$code == "361"] <- "Peru"
key_table$country[key_table$code == "423"] <- "Afghanistan"
key_table$country[key_table$code == "425"] <- "Azerbaijan"
key_table$country[key_table$code == "430"] <- "Georgia"
key_table$country[key_table$code == "432"] <- "Vietnam"
key_table$country[key_table$code == "436"] <- "India"
key_table$country[key_table$code == "438"] <- "Iraq"
key_table$country[key_table$code == "444"] <- "Kazakhstan"
key_table$country[key_table$code == "475"] <- "Syria"
key_table$country[key_table$code == "477"] <- "Usbekistan"
key_table$country[key_table$code == "998"] <- "small"


youth2010 <- youth2010 %>%
  left_join(key_table, by = c("s23aa" = "code")) %>%
  rename(s23aa_country = country) %>%
  left_join(key_table, by = c("s23ba" = "code")) %>%
  rename(s23ba_country = country) %>%
  left_join(key_table, by = c("s23ca" = "code")) %>%
  rename(s23ca_country = country)

youth2010$viet <- 0
youth2010$viet[youth2010$s23aa_country == "Vietnam" |
                 youth2010$s23ba_country == "Vietnam" |
                 youth2010$s23ca_country == "Vietnam" ] <- 1
table(youth2010$viet)

muslim_countries <- c("Afghanistan", "Albania", "Algeria", "Azerbaijan", "Iraq",
                      "Kazahkstan", "Syria", "Turkey", "Usbekistan")


youth2010$musl <- 0
youth2010$musl[youth2010$s23aa_country %in% muslim_countries |
                 youth2010$s23ba_country %in% muslim_countries |
                 youth2010$s23ca_country %in% muslim_countries] <- 1


# Vietnamese gender gap
mean(youth2010$satis[youth2010$viet == 1 & youth2010$gender == "male"], na.rm = TRUE)
mean(youth2010$satis[youth2010$viet == 1 & youth2010$gender == "female"], na.rm = TRUE)
round(mean(youth2010$satis[youth2010$viet == 1 & youth2010$gender == "male"], na.rm = TRUE) - mean(youth2010$satis[youth2010$viet == 1 & youth2010$gender == "female"], na.rm = TRUE), 2)
# -0.08
# [-0.67; 0.50]
t.test(satis ~ gender, data = youth2010[youth2010$viet == 1,])

# Muslim majority country gender gap
mean(youth2010$satis[youth2010$musl == 1 & youth2010$gender == "male"], na.rm = TRUE)
mean(youth2010$satis[youth2010$musl == 1 & youth2010$gender == "female"], na.rm = TRUE)
round(mean(youth2010$satis[youth2010$musl == 1 & youth2010$gender == "male"], na.rm = TRUE) - mean(youth2010$satis[youth2010$musl == 1 & youth2010$gender == "female"], na.rm = TRUE), 2)
# 0.47
# [0.04; 0.90]
t.test(satis ~ gender, data = youth2010[youth2010$musl == 1,])

# Other migration backgrounds gender gap
youth2010$migback_other <- 0
youth2010$migback_other[youth2010$mig_lang == 1 &
                          youth2010$viet == 0 &
                          youth2010$musl == 0] <- 1

mean(youth2010$satis[youth2010$migback_other == 1 & youth2010$gender == "male"], na.rm = TRUE)
mean(youth2010$satis[youth2010$migback_other == 1 & youth2010$gender == "female"], na.rm = TRUE)
round(mean(youth2010$satis[youth2010$migback_other == 1 & youth2010$gender == "male"], na.rm = TRUE) - mean(youth2010$satis[youth2010$migback_other == 1 & youth2010$gender == "female"], na.rm = TRUE), 2)
# 0.01
# [-0.32; 0.34]
t.test(satis ~ gender, data = youth2010[youth2010$migback_other == 1,])

