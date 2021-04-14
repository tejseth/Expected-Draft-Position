devtools::install_github(repo = "saiemgilani/cfbfastR")

library(tidyverse)
library(cfbfastR)
library(gt)

Sys.setenv(CFBD_API_KEY = "ApQdNgB5W5aYNiEqFYUe3FWOkfGKz1ZGAN1gVj8YAZWqEDRvtyx0XNgwOgXETtj1")

pbp_2020 <- data.frame()
for(i in 1:15){
  data <- cfbd_pbp_data(year = 2020, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2020 <- bind_rows(pbp_2020, df)
}

pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfbd_pbp_data(year = 2019, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

pbp_2018 <- data.frame()
for(i in 1:15){
  data <- cfbd_pbp_data(year = 2018, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2018 <- bind_rows(pbp_2018, df)
}

pbp_2017 <- data.frame()
for(i in 1:15){
  data <- cfbd_pbp_data(year = 2017, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2017 <- bind_rows(pbp_2017, df)
}

pbp_2016 <- data.frame()
for(i in 1:15){
  data <- cfbd_pbp_data(year = 2016, season_type = "both", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2016 <- bind_rows(pbp_2016, df)
}

pbp_2020_rp <- pbp_2020 %>%
  filter(pass == 1| rush == 1)

wr_stats_game_20 <- pbp_2020_rp %>%
  filter(!is.na(EPA)) %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, game_id, pos_team, def_pos_team) %>%
  summarize(rec = n(),
            total_yards = sum(yards_gained),
            total_epa = sum(EPA),
            epa_per_play = total_epa/rec) %>%
  ungroup()

wr_stats_total_20 <- wr_stats_game_20 %>%
  group_by(receiver_player_name, pos_team) %>%
  summarize(games = n(),
            season_rec = sum(rec),
            season_yards = sum(total_yards),
            yards_per_game = sum(total_yards)/games,
            season_epa = sum(total_epa),
            epa_per_rec = sum(total_epa)/sum(rec),
            epa_per_game = season_epa/games) %>%
  mutate(season = 2020) %>%
  filter(season_rec >= 10) %>%
  arrange(desc(yards_per_game))

pbp_2019_rp <- pbp_2019 %>%
  filter(pass == 1| rush == 1)

wr_stats_game_19 <- pbp_2019_rp %>%
  filter(!is.na(EPA)) %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, game_id, pos_team, def_pos_team) %>%
  summarize(rec = n(),
            total_yards = sum(yards_gained),
            total_epa = sum(EPA),
            epa_per_play = total_epa/rec) %>%
  ungroup()

wr_stats_total_19 <- wr_stats_game_19 %>%
  group_by(receiver_player_name, pos_team) %>%
  summarize(games = n(),
            season_rec = sum(rec),
            season_yards = sum(total_yards),
            yards_per_game = sum(total_yards)/games,
            season_epa = sum(total_epa),
            epa_per_rec = sum(total_epa)/sum(rec),
            epa_per_game = season_epa/games) %>%
  mutate(season = 2019) %>%
  filter(season_rec >= 10) %>%
  arrange(desc(yards_per_game))

pbp_2018_rp <- pbp_2018 %>%
  filter(pass == 1| rush == 1)

wr_stats_game_18 <- pbp_2018_rp %>%
  filter(!is.na(EPA)) %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, game_id, pos_team, def_pos_team) %>%
  summarize(rec = n(),
            total_yards = sum(yards_gained),
            total_epa = sum(EPA),
            epa_per_play = total_epa/rec) %>%
  ungroup()

wr_stats_total_18 <- wr_stats_game_18 %>%
  group_by(receiver_player_name, pos_team) %>%
  summarize(games = n(),
            season_rec = sum(rec),
            season_yards = sum(total_yards),
            yards_per_game = sum(total_yards)/games,
            season_epa = sum(total_epa),
            epa_per_rec = sum(total_epa)/sum(rec),
            epa_per_game = season_epa/games) %>%
  mutate(season = 2018) %>%
  filter(season_rec >= 10) %>%
  arrange(desc(yards_per_game))

pbp_2017_rp <- pbp_2017 %>%
  filter(pass == 1| rush == 1)

wr_stats_game_17 <- pbp_2017_rp %>%
  filter(!is.na(EPA)) %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, game_id, pos_team, def_pos_team) %>%
  summarize(rec = n(),
            total_yards = sum(yards_gained),
            total_epa = sum(EPA),
            epa_per_play = total_epa/rec) %>%
  ungroup()

wr_stats_total_17 <- wr_stats_game_17 %>%
  group_by(receiver_player_name, pos_team) %>%
  summarize(games = n(),
            season_rec = sum(rec),
            season_yards = sum(total_yards),
            yards_per_game = sum(total_yards)/games,
            season_epa = sum(total_epa),
            epa_per_rec = sum(total_epa)/sum(rec),
            epa_per_game = season_epa/games) %>%
  mutate(season = 2017) %>%
  filter(season_rec >= 10) %>%
  arrange(desc(yards_per_game))

pbp_2016_rp <- pbp_2016 %>%
  filter(pass == 1| rush == 1)

wr_stats_game_16 <- pbp_2016_rp %>%
  filter(!is.na(EPA)) %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, game_id, pos_team, def_pos_team) %>%
  summarize(rec = n(),
            total_yards = sum(yards_gained),
            total_epa = sum(EPA),
            epa_per_play = total_epa/rec) %>%
  ungroup()

wr_stats_total_16 <- wr_stats_game_16 %>%
  group_by(receiver_player_name, pos_team) %>%
  summarize(games = n(),
            season_rec = sum(rec),
            season_yards = sum(total_yards),
            yards_per_game = sum(total_yards)/games,
            season_epa = sum(total_epa),
            epa_per_rec = sum(total_epa)/sum(rec),
            epa_per_game = season_epa/games) %>%
  mutate(season = 2016) %>%
  filter(season_rec >= 10) %>%
  arrange(desc(yards_per_game))

all_stats <- rbind(wr_stats_total_20, wr_stats_total_19)
all_stats <- rbind(all_stats, wr_stats_total_18)
all_stats <- rbind(all_stats, wr_stats_total_17)
all_stats <- rbind(all_stats, wr_stats_total_16)

wr_metrics <- read.csv("~/Downloads/wr_metrics.csv")
player_vector <- wr_metrics[['player']]

all_stats <- all_stats %>%
  filter(receiver_player_name %in% player_vector)

career_stats <- all_stats %>%
  group_by(receiver_player_name) %>%
  summarize(total_rec = sum(season_rec),
            total_epa = sum(season_epa),
            epa_per_rec = total_epa/total_rec)

wr_metrics <- wr_metrics %>%
  left_join(career_stats, by = c("player" = "receiver_player_name"))

write.csv(wr_metrics, "wr_metrics2.csv")
wr_metrics2 <- read.csv("~/Draft Pos/wr_metrics2.csv")
