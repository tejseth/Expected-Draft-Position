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


