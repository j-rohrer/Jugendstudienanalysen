#####################################
# Additional exploration
#####################################

# scale structure/measurement invariance?

# look at the associations between domain satisfaction and general satisfaction
# separately for the different years
# for the different genders
# for the different modes


# or maybe actually just do correlations matrices
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

heatmap <- function(data) {
  # Select the variables for the heatmap
  variables <- c("satis", "satis_money", "satis_friends", "satis_mom", 
                 "satis_dad", "satis_leisure", "satis_dwell", "satis_grades")
  
  # Compute the correlation matrix
  correlation_matrix <- data %>%
    select(all_of(variables)) %>%
    cor(use = "complete.obs") # Use complete observations to handle missing values
  
  # Convert the correlation matrix to a tidy format
  correlation_data <- as.data.frame(as.table(correlation_matrix)) %>%
    rename(Var1 = Var1, Var2 = Var2, Correlation = Freq)
  
  # Filter to include only the lower triangle
  correlation_data <- correlation_data %>%
    mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
    filter(as.numeric(factor(Var1)) >= as.numeric(factor(Var2)))
  
  # Plot the triangular heatmap with labels
  ggplot(correlation_data, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         limit = c(-1, 1), space = "Lab", name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(title = "Triangular Correlation Heatmap",
         x = "",
         y = "") +
    coord_fixed()
  
}

heatmap(combined)
ggsave("Plots/corrs/all.png", width = 5, height = 5)

# Girls
heatmap(combined[combined$gender == "female",])
ggsave("Plots/corrs/girls.png", width = 5, height = 5)


# Boys
heatmap(combined[combined$gender == "male",])
ggsave("Plots/corrs/boys.png", width = 5, height = 5)


# German
heatmap(combined[combined$german_at_home == 1,])
ggsave("Plots/corrs/german.png", width = 5, height = 5)


# No German
heatmap(combined[combined$german_at_home == 0,])
ggsave("Plots/corrs/no_german.png", width = 5, height = 5)


# by year
heatmap(combined[combined$year == 2010,])
ggsave("Plots/corrs/2010.png", width = 5, height = 5)

heatmap(combined[combined$year == 2015,])
ggsave("Plots/corrs/2015.png", width = 5, height = 5)

heatmap(combined[combined$year == 2023,])
ggsave("Plots/corrs/2023.png", width = 5, height = 5)


#######################
# alpha
#######################

library(psych)
alpha(combined[, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])

# gender
alpha(combined[combined$gender == "male", c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
alpha(combined[combined$gender == "female", c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])

# german at home
alpha(combined[combined$german_at_home == 0, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
alpha(combined[combined$german_at_home == 1, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])

# source
alpha(combined[combined$year == 2023 & combined$source == "online", c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
alpha(combined[combined$year == 2023 & combined$source == "paper", c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])

# year
alpha(combined[combined$year == 2010, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
alpha(combined[combined$year == 2015, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
alpha(combined[combined$year == 2023, c("satis", "satis_money", "satis_friends", "satis_mom", "satis_dad", "satis_leisure", "satis_dwell")])
