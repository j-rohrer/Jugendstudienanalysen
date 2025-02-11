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



# Fit a mixed effects model -- only on the 2010 data
# Use: gender, migback yes/no, migback country, interaction
# then project to a different population (based on external data)
# see whether the drop in life satisfaction we observe in girls
# fits what we would expect


youth2010$s23aa_country[youth2010$migback == "none"] <- "Germany"
youth2010$s23ba_country[youth2010$migback == "none"] <- "Germany"
youth2010$s23ca_country[youth2010$migback == "none"] <- "Germany"

youth2010 <- youth2010 %>%
  mutate(s23aa_country = ifelse(s23aa_country %in% names(which(table(s23aa_country) < 5)), 
                                "small", 
                                s23aa_country))
youth2010 <- youth2010 %>%
  mutate(s23ba_country = ifelse(s23ba_country %in% names(which(table(s23ba_country) < 5)), 
                                "small", 
                                s23ba_country))
youth2010 <- youth2010 %>%
  mutate(s23ca_country = ifelse(s23ca_country %in% names(which(table(s23ca_country) < 5)), 
                                "small", 
                                s23ca_country))

table(youth2010$s23aa_country)
table(youth2010$s23ba_country)
table(youth2010$s23ca_country)

youth2010$parents <- paste(youth2010$s23ba_country, youth2010$s23ca_country, sep = "_")
table(youth2010$parents)
youth2010 <- youth2010 %>%
  mutate(parents = ifelse(parents %in% names(which(table(parents) < 3)), 
                                "rare", 
                          parents))
table(youth2010$parents)


youth2010$satis <- as.numeric(youth2010$satis)
library(brms)
country <- brm(satis ~ gender + (1 + gender|parents), 
               data = youth2010,
               cores = 4)
summary(country)

cmp <- comparisons(country, variables = "gender", newdata = datagrid(parents = unique))
cmp <- sort_by(cmp, ~estimate) |>
  transform(state = factor(parents, levels = parents))
ggplot(cmp, aes(y = estimate, ymin = conf.low, ymax = conf.high, x = parents )) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_pointrange() +
  labs(x = "", y = "Average Satisfaction Difference") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
