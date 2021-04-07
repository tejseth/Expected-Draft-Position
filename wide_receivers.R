library(tidyverse)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(na.tools)
library(ggimage)
library(nflfastR)
library(gt)
library(mgcv)
library(scales)
library(ggforce)
library(remotes)
library(ggtext)

all_wr_years <- read.csv("~/Downloads/all_wr_years.csv")

all_wr_years$on_tgt_rate <- gsub("\\%","",all_wr_years$on_tgt_rate)
all_wr_years$on_tgt_rate <- as.numeric(all_wr_years$on_tgt_rate)

wr_grouped <- all_wr_years %>%
  group_by(player, team) %>%
  summarize(seasons = n(),
            total_targets = sum(targets),
            total_catchables = sum(catch_targets),
            total_rec = sum(receptions),
            total_drops = sum(drops),
            total_yards = sum(season_yards),
            total_yac = sum(yac),
            total_tds = sum(tds),
            comp_perc = mean(comp_perc),
            on_tgt_rate = mean(on_tgt_rate),
            yards_per_target = mean(yards_per_target),
            yards_per_rec = (total_yards / total_rec),
            yards_per_game = mean(yards_per_game),
            broken_tackles = sum(broken_tackles),
            total_yac = sum(yards_after_contact),
            total_first_downs = sum(first_downs),
            first_down_perc = mean(first_down_perc),
            drop_rate = mean(drop_rate),
            rec_rating = mean(rec_rating))
  