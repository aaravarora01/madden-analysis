library(dplyr)
library(tidyr)
library(ggplot2)

LBStats <- read.csv("/Users/aarav/Downloads/Data/linebackers.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
LBStats$Player <- LBStats$Player

FullLB <- left_join(LBStats, MaddenRatings, by = "Player")

# select linebacker features
lb_features <- FullLB %>%
  select(
    starts_with("stats/strength"),
    starts_with("stats/speed"),
    starts_with("stats/acceleration"),
    starts_with("stats/agility"),
    starts_with("stats/awareness"),
    starts_with("stats/toughness"),
    starts_with("stats/pursuit"),
    starts_with("stats/press"),
    starts_with("stats/tackle"),
    starts_with("stats/injury"),
    starts_with("stats/stamina"),
    starts_with("stats/zoneCoverage"),
    starts_with("stats/manCoverage"),
    starts_with("stats/impactBlocking")
  )

lb_model_data <- FullLB %>%
  filter(!is.na(Comb)) %>% 
  select(Comb, all_of(names(lb_features))) %>%
  na.omit()

# fit model
model_lb_comb <- lm(Comb ~ ., data = lb_model_data)

# predictions
predicted_Comb <- predict(model_lb_comb, newdata = lb_model_data)


FullLB$Predicted_Comb <- NA

valid_rows <- which(!is.na(FullLB$Comb))

FullLB$Predicted_Comb[valid_rows] <- predicted_Comb

# compute mae
total_errors <- FullLB %>%
  filter(!is.na(Comb), !is.na(Predicted_Comb)) %>%
  mutate(
    Comb_Error = abs(Predicted_Comb - Comb)
  )

avg_total_comb_error <- mean(total_errors$Comb_Error, na.rm = TRUE)

cat("Average total Combined Tackles (Comb) error across all linebackers:", avg_total_comb_error, "\n")
