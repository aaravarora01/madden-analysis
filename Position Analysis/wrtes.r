library(dplyr)
library(tidyr)
library(ggplot2)

WRTEStats <- read.csv("/Users/aarav/Downloads/Data/wrtes.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
WRTEStats$Player <- WRTEStats$Player

FullWRTE <- left_join(WRTEStats, MaddenRatings, by = "Player")

# select wr/te features
wrte_features <- FullWRTE %>%
  select(
    starts_with("stats/deepRouteRunning"),
    starts_with("stats/mediumRouteRunning"),
    starts_with("stats/shortRouteRunning"),
    starts_with("stats/catching"),
    starts_with("stats/catchInTraffic"),
    starts_with("stats/spectacularCatch"),
    starts_with("stats/speed"),
    starts_with("stats/acceleration"),
    starts_with("stats/agility"),
    starts_with("stats/release"),
    starts_with("stats/awareness"),
    starts_with("stats/strength"),
    starts_with("stats/toughness"),
    starts_with("stats/zoneCoverage"),
    starts_with("stats/blockShedding"),
    starts_with("stats/runBlock"),
    starts_with("stats/passBlock")
  )

wrte_model_data <- cbind(
  Yds = FullWRTE$Yds,
  TD = FullWRTE$TD,
  wrte_features
) %>%
  na.omit()

# fit model
model_wrte_yds <- lm(Yds ~ ., data = wrte_model_data)
model_wrte_td  <- lm(TD ~ ., data = wrte_model_data)

# predictions
predicted_Yds <- predict(model_wrte_yds, newdata = wrte_model_data)
predicted_TD  <- predict(model_wrte_td,  newdata = wrte_model_data)

FullWRTE$Predicted_Yds <- NA
FullWRTE$Predicted_TD <- NA

valid_rows <- as.numeric(rownames(wrte_model_data))
FullWRTE$Predicted_Yds[valid_rows] <- predicted_Yds
FullWRTE$Predicted_TD[valid_rows]  <- predicted_TD

FullWRTE_clean <- FullWRTE %>%
  filter(!is.na(Yds), !is.na(Predicted_Yds), !is.na(TD), !is.na(Predicted_TD))

# compute mae
total_errors <- FullWRTE_clean %>%
  mutate(
    Yds_Error = abs(Predicted_Yds - Yds),
    TD_Error = abs(Predicted_TD - TD)
  )

avg_total_yds_error <- mean(total_errors$Yds_Error, na.rm = TRUE)
avg_total_td_error  <- mean(total_errors$TD_Error,  na.rm = TRUE)

cat("Average total Yards error across all players:", avg_total_yds_error, "\n")
cat("Average total TD error across all players:", avg_total_td_error, "\n")
