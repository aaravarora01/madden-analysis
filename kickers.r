library(dplyr)
library(tidyr)
library(ggplot2)

KickerStats <- read.csv("/Users/aarav/Downloads/Data/kickers.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
KickerStats$Player <- KickerStats$Player 

FullKicker <- left_join(KickerStats, MaddenRatings, by = "Player")

# select kicker features
kicker_features <- FullKicker %>%
  select(
    starts_with("stats/kickAccuracy"),
    starts_with("stats/kickPower"),
    starts_with("stats/kickReturn"),
    starts_with("stats/stamina"),
    starts_with("stats/toughness"),
    starts_with("stats/awareness"),
    starts_with("stats/impactBlocking")
  )

# combine w/ target variable
kicker_model_data <- cbind(
  FGM = FullKicker$FGM,
  XPM = FullKicker$XPM,
  kicker_features
) %>%
  na.omit()

# fit model
model_kicker_fgm <- lm(FGM ~ ., data = kicker_model_data)
model_kicker_xpm <- lm(XPM ~ ., data = kicker_model_data)

# predictions
predicted_FGM <- predict(model_kicker_fgm, newdata = kicker_model_data)
predicted_XPM <- predict(model_kicker_xpm, newdata = kicker_model_data)

FullKicker$Predicted_FGM <- NA
FullKicker$Predicted_XPM <- NA

valid_rows <- as.numeric(rownames(kicker_model_data))
FullKicker$Predicted_FGM[valid_rows] <- predicted_FGM
FullKicker$Predicted_XPM[valid_rows] <- predicted_XPM

FullKicker_clean <- FullKicker %>%
  filter(!is.na(FGM), !is.na(Predicted_FGM), !is.na(XPM), !is.na(Predicted_XPM))

# compute mae
total_errors <- FullKicker_clean %>%
  mutate(
    FGM_Error = abs(Predicted_FGM - FGM),
    XPM_Error = abs(Predicted_XPM - XPM)
  )

avg_total_fgm_error <- mean(total_errors$FGM_Error, na.rm = TRUE)
avg_total_xpm_error <- mean(total_errors$XPM_Error, na.rm = TRUE)

cat("Average total FGM error across all kickers:", avg_total_fgm_error, "\n")
cat("Average total XPM error across all kickers:", avg_total_xpm_error, "\n")