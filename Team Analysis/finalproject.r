library(dplyr)
getwd()
MaddenRatings = read.csv("madden25.csv")   
QBStats = read.csv("qbs.csv")  
RBStats = read.csv("rbs.csv")
WRStats = read.csv("wrtes.csv")
MaddenRatings$Player <- paste(MaddenRatings$firstName, MaddenRatings$lastName, sep = " ")
FullQB <- left_join(QBStats, MaddenRatings, by = "Player")
FullRB <- left_join(RBStats, MaddenRatings, by = "Player")
FullWR <- left_join(WRStats, MaddenRatings, by = "Player")
head(FullQB)
head(FullRB)
head(FullWR)

library(pROC)
library(car)
model <- lm(Yds ~ ., data = FullQB %>% select(Yds, starts_with("stats.throw")))
model2 <- lm(Yds ~ stats.acceleration.value + stats.agility.value + stats.strength.value, data = FullRB)
model3 <- lm(Yds ~ stats.catching.value + stats.deepRouteRunning.value + stats.mediumRouteRunning.value + stats.shortRouteRunning.value + stats.spectacularCatch.value, data = FullWR)
summary(model)
summary(model2)
summary(model3)

FullQB$Predicted_Yds <- predict(model, newdata = FullQB)
FullQB <- FullQB %>% distinct(Player, .keep_all = TRUE)

FullRB$Predicted_Yds <- predict(model2, newdata = FullRB)
FullRB <- FullRB %>% distinct(Player, .keep_all = TRUE)

FullWR$Predicted_Yds <- predict(model3, newdata = FullWR)
FullWR <- FullWR %>% distinct(Player, .keep_all = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)

FullQB_long <- FullQB %>%
  pivot_longer(cols = c(Yds, Predicted_Yds), 
               names_to = "Type", 
               values_to = "Yards")

player_order <- FullQB %>%
  group_by(Player) %>%
  summarize(AvgPredicted = mean(Predicted_Yds, na.rm = TRUE)) %>%
  arrange(AvgPredicted) %>%
  pull(Player)

FullQB_long$Player <- factor(FullQB_long$Player, levels = unique(player_order))

ggplot(FullQB_long, aes(x = Player, y = Yards, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(data = filter(FullQB_long, Type == "Predicted_Yds"),
              method = "lm", se = FALSE, linetype = "solid", size = 1) +
  labs(
    title = "Actual vs Predicted Yards by Player",
    x = "Player (Sorted by Predicted Yards)",
    y = "Yards",
    color = "Yards Type"
  ) +
  scale_color_manual(values = c("Yds" = "#D4435D", "Predicted_Yds" = "#3195AF"),
                     labels = c("Yds" = "Actual", "Predicted_Yds" = "Predicted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill = "#f1e9e8"), panel.background = element_rect(fill = "#d4c2bf", color = NA))

FullRB_long <- FullRB %>%
  pivot_longer(cols = c(Yds, Predicted_Yds), 
               names_to = "Type", 
               values_to = "Yards")

player_order <- FullRB %>%
  group_by(Player) %>%
  summarize(AvgPredicted = mean(Predicted_Yds, na.rm = TRUE)) %>%
  arrange(AvgPredicted) %>%
  pull(Player)

FullRB_long$Player <- factor(FullRB_long$Player, levels = unique(player_order))

ggplot(FullRB_long, aes(x = Player, y = Yards, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(data = filter(FullRB_long, Type == "Predicted_Yds"),
              method = "lm", se = FALSE, linetype = "solid", size = 1) +
  labs(
    title = "Actual vs Predicted Yards by Player",
    x = "Player (Sorted by Predicted Yards)",
    y = "Yards",
    color = "Yards Type"
  ) +
  scale_color_manual(values = c("Yds" = "#D4435D", "Predicted_Yds" = "#3195AF"),
                     labels = c("Yds" = "Actual", "Predicted_Yds" = "Predicted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill = "#f1e9e8"), panel.background = element_rect(fill = "#d4c2bf", color = NA))

player_order <- FullWR %>%
  group_by(Player) %>%
  summarize(AvgPredicted = mean(Predicted_Yds, na.rm = TRUE)) %>%
  arrange(desc(AvgPredicted)) %>%
  slice_head(n = 32) %>%
  pull(Player)

FullWR_long <- FullWR_long %>%
  filter(Player %in% player_order)

FullWR_long$Player <- factor(FullWR_long$Player, levels = player_order)

ggplot(FullWR_long, aes(x = Player, y = Yards, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(data = filter(FullWR_long, Type == "Predicted_Yds"),
              method = "lm", se = FALSE, linetype = "solid", size = 1) +
  labs(
    title = "Top 32 Receivers: Actual vs Predicted Yards",
    x = "Player (Sorted by Predicted Yards)",
    y = "Yards",
    color = "Yards Type"
  ) +
  scale_color_manual(values = c("Yds" = "#D4435D", "Predicted_Yds" = "#3195AF"),
                     labels = c("Yds" = "Actual", "Predicted_Yds" = "Predicted")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#f1e9e8"),
        panel.background = element_rect(fill = "#d4c2bf", color = NA))


library(dplyr)
library(purrr)

training_years <- c(19, 21)
training_results <- list()
test_result <- NULL

win_totals <- list(
  "19" = c(
    "Patriots" = 12, "Bills" = 10, "Dolphins" = 5, "Jets" = 7,
    "Ravens" = 14, "Bengals" = 2, "Browns" = 6, "Steelers" = 8,
    "Texans" = 10, "Colts" = 7, "Jaguars" = 6, "Titans" = 9,
    "Broncos" = 7, "Chiefs" = 12, "Raiders" = 7, "Chargers" = 5,
    "Cowboys" = 8, "Giants" = 4, "Eagles" = 9, "Washington" = 3,
    "Bears" = 8, "Lions" = 3, "Packers" = 13, "Vikings" = 10,
    "Falcons" = 7, "Panthers" = 5, "Saints" = 13, "Buccaneers" = 7,
    "Cardinals" = 5, "Rams" = 9, "49ers" = 13, "Seahawks" = 11
  ),
  "21" = c(
    "Patriots" = 10, "Bills" = 11, "Dolphins" = 9, "Jets" = 4,
    "Ravens" = 8, "Bengals" = 10, "Browns" = 8, "Steelers" = 9,
    "Texans" = 4, "Colts" = 9, "Jaguars" = 3, "Titans" = 12,
    "Broncos" = 7, "Chiefs" = 12, "Raiders" = 10, "Chargers" = 9,
    "Cowboys" = 12, "Giants" = 4, "Eagles" = 9, "Washington" = 7,
    "Bears" = 6, "Lions" = 3, "Packers" = 13, "Vikings" = 8,
    "Falcons" = 7, "Panthers" = 5, "Saints" = 9, "Buccaneers" = 13,
    "Cardinals" = 11, "Rams" = 12, "49ers" = 10, "Seahawks" = 7
  ),
  "23" = c(
    "Patriots" = 4, "Bills" = 11, "Dolphins" = 9, "Jets" = 7,
    "Ravens" = 13, "Bengals" = 9, "Browns" = 11, "Steelers" = 10,
    "Texans" = 10, "Colts" = 9, "Jaguars" = 9, "Titans" = 6,
    "Broncos" = 8, "Chiefs" = 11, "Raiders" = 8, "Chargers" = 5,
    "Cowboys" = 12, "Giants" = 6, "Eagles" = 11, "Commanders" = 4,
    "Bears" = 7, "Lions" = 12, "Packers" = 9, "Vikings" = 7,
    "Falcons" = 7, "Panthers" = 2, "Saints" = 9, "Buccaneers" = 9,
    "Cardinals" = 4, "Rams" = 10, "49ers" = 12, "Seahawks" = 9
  )
)

get_top_players <- function(df, pos, n = 1) {
  df <- as_tibble(df)
  df %>%
    filter(Position == pos) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      "{pos}_Overall_{n}" := mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_players_avg <- function(df, pos, n) {
  df <- as_tibble(df)
  df %>%
    filter(Position == pos) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      "{pos}_avg_overall_{n}" := mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_oline <- function(df, n = 5) {
  df <- as_tibble(df)
  df %>%
    filter(Position %in% c("LT", "LG", "C", "RG", "RT")) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      OLine_avg_overall_5 = mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_dline_avg <- function(df, n = 5) {
  df <- as_tibble(df)
  df %>%
    filter(Position %in% c("DT", "RE", "LE")) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      DLine_avg_overall_5 = mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_lb_avg <- function(df, n = 2) {
  df <- as_tibble(df)
  df %>%
    filter(Position %in% c("MLB", "LOLB", "ROLB")) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      LB_avg_overall_2 = mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

get_top_db_avg <- function(df, n = 4) {
  df <- as_tibble(df)
  df %>%
    filter(Position %in% c("CB", "FS", "SS")) %>%
    arrange(desc(Overall)) %>%
    slice_head(n = n) %>%
    summarise(
      DB_avg_overall_4 = mean(Overall, na.rm = TRUE),
      .groups = "drop"
    )
}

for (year in training_years) {
  file_name <- paste0("madden", year, ".csv")
  player_df <- read.csv(file_name)
  player_df <- as_tibble(player_df)

  cat("\nUnique Team values in Madden", year, ":\n")
  print(unique(player_df$Team))

  player_df <- player_df %>%
    mutate(Team = as.character(Team))

  player_df <- player_df %>%
    mutate(wins = win_totals[[as.character(year)]][Team])

  cat("\nTeams with NA wins in Madden", year, ":\n")
  print(player_df %>% filter(is.na(wins)) %>% select(Team) %>% distinct())

  result <- player_df %>%
    group_by(Team) %>%
    nest() %>%
    mutate(
      top_QB = map(data, ~get_top_players(.x, "QB", 1)),     # QB: top 1, concatenate name and rating
      top_RB = map(data, ~get_top_players(.x, "HB", 1)),     # RB: top 1, concatenate name and rating
      top_WR = map(data, ~get_top_players_avg(.x, "WR", 2)), # WR: top 2, average Overall
      top_TE = map(data, ~get_top_players_avg(.x, "TE", 2)), # TE: top 2, average Overall
      top_OLine = map(data, ~get_top_oline(.x, 5)),          # OLine: top 5, average Overall
      top_DLine = map(data, ~get_top_dline_avg(.x, 5)),      # DLine: top 5, average Overall
      top_LB = map(data, ~get_top_lb_avg(.x, 2)),            # LB: top 2, average Overall
      top_DB = map(data, ~get_top_db_avg(.x, 4)),            # DB: top 4, average Overall
      wins = map_dbl(data, ~first(.x$wins))                  # Extract the wins column
    ) %>%
    unnest(c(top_QB, top_RB, top_WR, top_TE, top_OLine, top_DLine, top_LB, top_DB), names_sep = "_") %>%
    select(-data)

  training_results[[paste0("madden", year)]] <- result
}

test_file_name <- "madden23.csv"
player_df <- read.csv(test_file_name)
player_df <- as_tibble(player_df)

cat("\nUnique Team values in Madden 23:\n")
print(unique(player_df$Team))

player_df <- player_df %>%
  mutate(Team = as.character(Team))

player_df <- player_df %>%
  mutate(wins = win_totals[["23"]][Team])

cat("\nTeams with NA wins in Madden 23:\n")
print(player_df %>% filter(is.na(wins)) %>% select(Team) %>% distinct())

test_result <- player_df %>%
  group_by(Team) %>%
  nest() %>%
  mutate(
    top_QB = map(data, ~get_top_players(.x, "QB", 1)),     # QB: top 1, concatenate name and rating
    top_RB = map(data, ~get_top_players(.x, "HB", 1)),     # RB: top 1, concatenate name and rating
    top_WR = map(data, ~get_top_players_avg(.x, "WR", 2)), # WR: top 2, average Overall
    top_TE = map(data, ~get_top_players_avg(.x, "TE", 2)), # TE: top 2, average Overall
    top_OLine = map(data, ~get_top_oline(.x, 5)),          # OLine: top 5, average Overall
    top_DLine = map(data, ~get_top_dline_avg(.x, 5)),      # DLine: top 5, average Overall
    top_LB = map(data, ~get_top_lb_avg(.x, 2)),            # LB: top 2, average Overall
    top_DB = map(data, ~get_top_db_avg(.x, 4)),            # DB: top 4, average Overall
    wins = map_dbl(data, ~first(.x$wins))                  # Extract the wins column
  ) %>%
  unnest(c(top_QB, top_RB, top_WR, top_TE, top_OLine, top_DLine, top_LB, top_DB), names_sep = "_") %>%
  select(-data)

for (year in training_years) {
  cat("\nResults for Madden", year, ":\n")
  print(training_results[[paste0("madden", year)]], n = Inf)
}

cat("\nResults for Madden 23 (Test):\n")
print(test_result, n = Inf)
 
all_results <- bind_rows(
  training_results[["madden19"]],
  training_results[["madden21"]],
  test_result
)  

model <- lm(wins ~ top_QB_QB_Overall_1 + top_RB_HB_Overall_1 + top_WR_WR_avg_overall_2 +
              top_TE_TE_avg_overall_2 + top_OLine_OLine_avg_overall_5 +
              top_DLine_DLine_avg_overall_5 + top_LB_LB_avg_overall_2 +
              top_DB_DB_avg_overall_4, data = all_results)

summary(model)

# Testing

test_result$predicted_wins <- predict(model, newdata = test_result)

team_order <- test_result %>%
  arrange(predicted_wins) %>%
  pull(Team)

plot_df <- test_result %>%
  select(Team, actual_wins = wins, predicted_wins) %>%
  pivot_longer(cols = c(actual_wins, predicted_wins),
               names_to = "Type",
               values_to = "Wins")

plot_df$Team <- factor(plot_df$Team, levels = unique(team_order))

ggplot(plot_df, aes(x = Team, y = Wins, color = Type)) +
  geom_point(size = 3) +
  geom_smooth(data = filter(plot_df, Type == "predicted_wins"),
              method = "lm", se = FALSE, linetype = "solid", size = 1) +
  labs(
    title = "Actual vs Predicted Wins by Team",
    x = "Team (Sorted by Predicted Wins)",
    y = "Wins",
    color = "Wins Type"
  ) +
  scale_color_manual(values = c("actual_wins" = "#D4435D", "predicted_wins" = "#3195AF")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill = "#f1e9e8"), panel.background = element_rect(fill = "#d4c2bf", color = NA))
