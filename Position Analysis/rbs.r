library(dplyr)
library(tidyr)
library(ggplot2)

RBStats <- read.csv("/Users/aarav/Downloads/Data/rbs.csv")
MaddenRatings <- read.csv("/Users/aarav/Downloads/Data/madden25.csv")

MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
RBStats$Player <- RBStats$Player

FullRB <- left_join(RBStats, MaddenRatings, by = "Player")

# select rb features
rb_features <- FullRB %>%
  select(
    starts_with("stats/bCVision"),
    starts_with("stats/carrying"),
    starts_with("stats/speed"),
    starts_with("stats/agility"),
    starts_with("stats/acceleration"),
    starts_with("stats/breakTackle"),
    starts_with("stats/jukeMove"),
    starts_with("stats/spinMove"),
    starts_with("stats/stiffArm"),
    starts_with("stats/trucking"),
    starts_with("stats/changeOfDirection"),
    starts_with("stats/strength")
  )

rb_model_data <- cbind(
  Yds = FullRB$Yds,
  TD = FullRB$TD,
  rb_features
) %>%
  na.omit()

# fit model
model_rb_yds <- lm(Yds ~ ., data = rb_model_data)
model_rb_td  <- lm(TD ~ ., data = rb_model_data)

# predictions
predicted_Yds <- predict(model_rb_yds, newdata = rb_model_data)
predicted_TD  <- predict(model_rb_td,  newdata = rb_model_data)

FullRB$Predicted_Yds <- NA
FullRB$Predicted_TD <- NA

valid_rows <- as.numeric(rownames(rb_model_data))
FullRB$Predicted_Yds[valid_rows] <- predicted_Yds
FullRB$Predicted_TD[valid_rows]  <- predicted_TD

FullRB_clean <- FullRB %>%
  filter(!is.na(Yds), !is.na(Predicted_Yds), !is.na(TD), !is.na(Predicted_TD))


total_errors <- FullRB_clean %>%
  mutate(
    Yds_Error = abs(Predicted_Yds - Yds),
    TD_Error = abs(Predicted_TD - TD)
  )

# compute mae
avg_total_yds_error <- mean(total_errors$Yds_Error, na.rm = TRUE)
avg_total_td_error  <- mean(total_errors$TD_Error,  na.rm = TRUE)

cat("Average total Yards error across all players:", avg_total_yds_error, "\n")
cat("Average total TD error across all players:", avg_total_td_error, "\n")


# Create graph
FullRB_long <- FullRB_clean %>%
  pivot_longer(cols = c(Yds, Predicted_Yds), 
               names_to = "Type", 
               values_to = "Yards")

rb_order <- FullRB_clean %>%
  group_by(Player) %>%
  summarize(AvgPredicted = mean(Predicted_Yds)) %>%
  arrange(AvgPredicted) %>%
  pull(Player)

FullRB_long$Player <- factor(FullRB_long$Player, levels = rb_order)

ggplot(FullRB_long, aes(x = Player, y = Yards, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(
    data = filter(FullRB_long, Type == "Predicted_Yds"),
    method = "lm", se = FALSE, linetype = "solid", size = 1
  ) +
  labs(
    title = "Actual vs Predicted Rushing Yards by RB",
    x = "Player (Sorted by Predicted Yards)",
    y = "Yards",
    color = "Yards Type"
  ) +
  scale_color_manual(values = c("Yds" = "red", "Predicted_Yds" = "blue"),
                     labels = c("Yds" = "Actual", "Predicted_Yds" = "Predicted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
