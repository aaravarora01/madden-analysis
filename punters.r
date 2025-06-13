library(dplyr)
library(tidyr)
library(ggplot2)

PunterStats <- read.csv("/Users/aarav/Downloads/Data/punters.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
PunterStats$Player <- PunterStats$Player

FullPunter <- left_join(PunterStats, MaddenRatings, by = "Player")

# select punter features
punter_model_data <- FullPunter %>%
  filter(!is.na(YardsPer), !is.na(TB)) %>%
  select(
    YardsPer,
    TB,
    starts_with("stats/kickPower"),
    starts_with("stats/kickAccuracy"),
    starts_with("stats/kickReturn"),
    starts_with("stats/toughness"),
    starts_with("stats/awareness"),
    starts_with("stats/stamina"),
    starts_with("stats/impactBlocking")
  ) %>%
  na.omit()

# fit model
model_punter_yp <- lm(YardsPer ~ ., data = punter_model_data)
model_punter_tb <- lm(TB ~ ., data = punter_model_data)

# predictions
predicted_YardsPer <- predict(model_punter_yp, newdata = punter_model_data)
predicted_TB <- predict(model_punter_tb, newdata = punter_model_data)


FullPunter$Predicted_YardsPer <- NA
FullPunter$Predicted_TB <- NA

valid_rows <- which(!is.na(FullPunter$YardsPer) & !is.na(FullPunter$TB))

FullPunter$Predicted_YardsPer[valid_rows] <- predicted_YardsPer
FullPunter$Predicted_TB[valid_rows] <- predicted_TB

# compute mae
total_errors <- FullPunter %>%
  filter(!is.na(YardsPer), !is.na(Predicted_YardsPer), !is.na(TB), !is.na(Predicted_TB)) %>%
  mutate(
    YardsPer_Error = abs(Predicted_YardsPer - YardsPer),
    TB_Error = abs(Predicted_TB - TB)
  )

avg_total_yardsper_error <- mean(total_errors$YardsPer_Error, na.rm = TRUE)
avg_total_tb_error <- mean(total_errors$TB_Error, na.rm = TRUE)

cat("Average total YardsPer error across all punters:", avg_total_yardsper_error, "\n")
cat("Average total TB error across all punters:", avg_total_tb_error, "\n")