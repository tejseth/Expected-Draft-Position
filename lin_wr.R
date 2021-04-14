wr_metrics2 <- read.csv("~/Draft Pos/wr_metrics2.csv")

drafted_wrs <- wr_metrics2 %>%
  filter(round!= 1000)

fit <- lm(pick ~ seasons  + total_targets + total_catchables + total_rec + 
            total_drops + total_yards + total_yac + total_tds + comp_perc +
            on_tgt_rate + yards_per_target + yards_per_rec + yards_per_game +
            broken_tackles + total_first_downs + first_down_perc + drop_rate +
            rec_rating + vertical + height + weight + is_power_five + total_epa +
            epa_per_rec, data = drafted_wrs)

summary(fit)

preds <- predict(fit, wr_metrics2) %>%
  as_tibble() %>%
  rename(prediction = value) %>%
  round(1) %>%
  bind_cols(wr_metrics)

draft_pred <- preds %>%
  select(player, team, prediction, round, pick)

#write.csv(draft_pred, "draft_pred.csv")
draft_pred <- read.csv("~/Draft Pos/draft_pred.csv")

prev_draft_pred <- draft_pred %>%
  filter(round != 1000) %>%
  mutate(draft_pos_oe = pick - prediction)

draft_2020 <- draft_pred %>%
  filter(round == 1000) %>%
  arrange(prediction) %>%
  mutate(proj_draft_order = row_number())

draft.lm = lm(prediction ~ round, data=prev_draft_pred)
summary(draft.lm)$r.squared 

prev_draft_pred %>% 
  ggplot(aes(x = round, y = prediction, fill = prediction)) + 
  geom_quasirandom(pch = 21, size = 5.5, width = 0.2) + 
  scale_fill_viridis_c("",  guide = FALSE) + 
  theme_classic(15) +  
  labs(
    title = "Each Receiver's Projected Draft Position and Actual Round Drafted",
    subtitle = "Receivers drafted from 2016 to 2020, r-squared = 0.44",
    x = "Actual Round Drafted",
    y = "My Projected Pick",
    caption = "By Tej Seth | @mfbanalytics | @StatsbyLopez"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )
ggsave('wr_clusters_4.png', width = 14, height = 10, dpi = "retina")

draft_2020 <- draft_2020 %>%
  mutate(actual_pick = prediction)

draft_pred <- draft_pred %>%
  mutate(actual_pick = ifelse(pick == 1000, prediction, pick))

draft_pred %>% 
  filter(round == 1 | round == 2 | round == 1000) %>%
  ggplot() +
  geom_smooth(aes(x = prediction, y = pick), method = "lm", color = "grey", data = prev_draft_pred) +
  ggrepel::geom_text_repel(
    aes(x = prediction, y = actual_pick, label = player),
    box.padding = 0.3, size = 4
  ) + 
  geom_point(
    aes(x = prediction, y = actual_pick, size = 4, fill = "black", color = round), 
    shape = 21
  ) +
  geom_point(
    aes(x = prediction, y = actual_pick, size = 4, fill = "orange"), 
    shape = 21, data = draft_2020
  ) +
  scale_color_identity(aesthetics =  c("fill", "color")) +
  theme_tej() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Predicted Draft Pick",
       y = "'Actual' Draft Pick",
       title = "Actual and Predicted Draft Positions for Wide Receivers",
       subtitle = "All receivers drafted in the 1st round since 2016 (black), all draft elgible receivers this year (orange)",
       caption = "By Tej Seth | @mfbanalytics") +
  annotate("text", x = 20, y = 100, label = "Drafted after \n expected", color = "blue", size = 3) +
  annotate("text", x = 100, y = 0, label = "Drafted before \n expected", color = "blue", size = 3) 
ggsave('wr_clusters_5.png', width = 14, height = 10, dpi = "retina")

write.csv(draft_2020, "draft_2020.csv")
draft_2020 <- read.csv("~/Draft Pos/draft_2020.csv")

draft_2020 <- draft_2020 %>%
  mutate(composite = (my_prediction + edp_pred)/2) %>%
  arrange(composite) %>%
  mutate(rank = row_number())

tab_edp <- draft_2020 %>%
  select(rank, player, headshot, team, my_prediction, edp_pred, composite)

tab_edp <- tab_edp %>%
  mutate_if(is.numeric, ~round(., 1))

edp_gt <- tab_edp %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(headshot)),
    fn = function(x){
      web_image(
        url = x,
        height = px(25)
      )
    }
  ) %>% 
  cols_label(
    rank = "RK",
    player = "Receiver",
    headshot = "",
    team = "College Team",
    my_prediction = "Stats Pred.",
    edp_pred = "Mock Pred.",
    composite = "EDP Composite") %>%
  data_color(
    columns = vars(composite),
    colors = scales::col_numeric(
      palette = c("#7fbf7b", "#f7f7f7", "#af8dc3"),
      domain = c(0, 250)
    )
  ) %>% 
  tab_header(
    title = "NFL Draft Projected Draft Order of Receivers",
    subtitle = "Predictions made by a stats-based prediction (@mfbanalytics) combined with 'Grinding the Mocks' (@benj_robinson)"
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "middle",
    heading.title.font.size = 22
  ) %>%
  opt_table_font(
    font = list(
      default_fonts()
    )
  ) 
gtsave(edp_gt, "wr_clusters_6.png")




