library(dplyr)
library(tidyr)
library(ggplot2)

SecondaryStats <- read.csv("/Users/aarav/Downloads/Data/secondary.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
SecondaryStats$Player <- SecondaryStats$Player

FullSecondary <- left_join(SecondaryStats, MaddenRatings, by = "Player")

# select secondary features
secondary_features <- FullSecondary %>%
  select(
    starts_with("stats/speed"),
    starts_with("stats/acceleration"),
    starts_with("stats/agility"),
    starts_with("stats/awareness"),
    starts_with("stats/press"),
    starts_with("stats/zoneCoverage"),
    starts_with("stats/manCoverage"),
    starts_with("stats/toughness"),
    starts_with("stats/injury"),
    starts_with("stats/stamina"),
    starts_with("stats/pursuit"),
    starts_with("stats/impactBlocking")
  )

secondary_model_data <- FullSecondary %>%
  filter(!is.na(Int)) %>% 
  select(Int, all_of(names(secondary_features))) %>%
  na.omit()

# fit model
model_secondary_int <- lm(Int ~ ., data = secondary_model_data)

# predictions
predicted_Int <- predict(model_secondary_int, newdata = secondary_model_data)

FullSecondary$Predicted_Int <- NA

valid_rows <- which(!is.na(FullSecondary$Int))

FullSecondary$Predicted_Int[valid_rows] <- predicted_Int

# compute mae
total_errors <- FullSecondary %>%
  filter(!is.na(Int), !is.na(Predicted_Int)) %>%
  mutate(
    Int_Error = abs(Predicted_Int - Int)
  )

avg_total_int_error <- mean(total_errors$Int_Error, na.rm = TRUE)

cat("Average total Interception (Int) error across all secondary players:", avg_total_int_error, "\n")