library(dplyr)
library(tidyr)
library(ggplot2)

DlineStats <- read.csv("/Users/aarav/Downloads/Data/dline.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
DlineStats$Player <- DlineStats$Player  # assume full names present

FullDline <- left_join(DlineStats, MaddenRatings, by = "Player")

# select dline features
dline_features <- FullDline %>%
  select(
    starts_with("stats/strength"),
    starts_with("stats/powerMoves"),
    starts_with("stats/finesseMoves"),
    starts_with("stats/speed"),
    starts_with("stats/acceleration"),
    starts_with("stats/agility"),
    starts_with("stats/awareness"),
    starts_with("stats/toughness"),
    starts_with("stats/pursuit"),
    starts_with("stats/press"),
    starts_with("stats/breakSack"),
    starts_with("stats/blockShedding"),
    starts_with("stats/injury"),
    starts_with("stats/stamina")
  )

# combine w/ target variable
dline_model_data <- FullDline %>%
  filter(!is.na(Sk)) %>%  # filter rows with Sk data
  select(Sk, all_of(names(dline_features))) %>%
  na.omit()

# fit model
model_dline_sk <- lm(Sk ~ ., data = dline_model_data)

# predictions
predicted_Sk <- predict(model_dline_sk, newdata = dline_model_data)

FullDline$Predicted_Sk <- NA

valid_rows <- which(!is.na(FullDline$Sk))

FullDline$Predicted_Sk[valid_rows] <- predicted_Sk

# compute mae
total_errors <- FullDline %>%
  filter(!is.na(Sk), !is.na(Predicted_Sk)) %>%
  mutate(
    Sk_Error = abs(Predicted_Sk - Sk)
  )

avg_total_sk_error <- mean(total_errors$Sk_Error, na.rm = TRUE)

cat("Average total Sack (Sk) error across all defensive linemen:", avg_total_sk_error, "\n")
