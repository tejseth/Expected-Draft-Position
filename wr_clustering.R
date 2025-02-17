library(ggthemes)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(ggsci) 
library(broom)
library(igraph)
library(tidyverse)
library(ggrepel)
library(gt)
library(ggbeeswarm)

theme_tej <- function() {
  theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15)
    )
}

X <- wr_metrics2 %>% 
  mutate(broken_tackle_rate = broken_tackles / total_rec,
         td_rate = total_tds / total_targets,
         yac_perc = total_yac / total_yards) %>%
  dplyr::select(seasons, comp_perc, yards_per_target, yards_per_rec, first_down_perc,
                td_rate, fourty_time, vertical, height, weight, drop_rate, yards_per_game, yac_perc) %>%
  scale()

set.seed(222)
MAX_K <- 20
sse <- c() 

for (k in 1:MAX_K) {
  algo_k <- kmeans(X, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_tej() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(22)
# re-run K-Means with 6 clusters
K <- 6
kmeans6 <- kmeans(X, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans6$centers) # SCALED cluster centers/means

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6')


km_centers <- km_centers %>%
  rename(c('EXP'='seasons', 'CP'='comp_perc', # give predictors a shorter name for plotting
           'YPT'='yards_per_target', 'YPR'='yards_per_rec',
           'FDP'='first_down_perc', 'TD'='td_rate',
           '40'='fourty_time', 'VT'='vertical', 'HT'='height',
           'WGT'='weight', 'DR'='drop_rate', 'YPG'='yards_per_game', "YAC" = 'yac_perc')) %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6'))

clusters <- tibble(cluster=kmeans6$cluster, player=wr_metrics$player)

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point(size = 5) + # plot points
  scale_color_brewer(palette="Dark2") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups for Receivers in the NFL Draft",
       caption = 'By Tej Seth | @mfbanalytics | @_AlexStern') + 
  theme_minimal() + theme_tej() + 
  theme(legend.position = "none", strip.text = element_text(face='bold', size = 15),
        axis.text.x = element_text(angle=90, size=12), # alter axis text
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color="black", fill="#add8e6", size=1.5, linetype="solid"))
ggsave('wr_clusters_1.png', width = 14, height = 10, dpi = "retina")

wr_metrics2 <- wr_metrics2 %>%
  left_join(clusters)

pca <- prcomp(X) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans6$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

pc2 <- pc2 %>%
  arrange(Cluster) %>%
  mutate(id = row_number())
wr_metrics2 <- wr_metrics2 %>%
  arrange(cluster) %>%
  mutate(id = row_number())
wr_metrics2$cluster <- as.factor(wr_metrics2$cluster)

wr_metrics2 <- wr_metrics2 %>%
  left_join(pc2, by = c("id" = "id"))

p <- wr_metrics2 %>% 
  filter(round == 1 | round == 2 | round == 1000) %>%
  ggplot(aes(x=PC1, y=PC2, color=cluster, shape=cluster)) + 
  geom_point(alpha=0.3) + 
  geom_text_repel(aes(label = player)) +
  scale_color_brewer(palette="Dark2") +
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + stat_ellipse(level=(2/3)) + # set ellipse value to one standard deviation
  scale_shape_manual(values=seq(0,15)) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = "NFL Draft Wide Receiver Clusters",
       subtitle = 'Receivers drafted in the 1st or 2nd round since 2016 and all draft elgible receivers this year',
       caption = 'By Tej Seth | @mfbanalytics | @_AlexStern') + 
  theme_tej() +
  theme(legend.position = "right") 
p + coord_cartesian(xlim = c(-4.2, 6), ylim = c(-4.5, 3.5))
ggsave('wr_clusters_2.png', width = 14, height = 10, dpi = "retina")

cluster_stats_drafted <- wr_metrics2 %>%
  filter(round != 1000) %>%
  group_by(cluster) %>%
  summarize(total_epa_avg = mean(total_epa),
            epa_per_rec_avg = mean(epa_per_rec),
            avg_fourty = mean(fourty_time),
            avg_height = mean(height),
            avg_weight = mean(weight),
            avg_round = mean(round),
            avg_pick = mean(pick))

cluster_stats_drafted <- cluster_stats_drafted %>%
  mutate_if(is.numeric, ~round(., 2)) %>%
  arrange(avg_pick)

write.csv(cluster_stats_drafted, 'cluster_stats_drafted.csv')

tab_data <- read.csv("~/Draft Pos/cluster_stats_drafted.csv", comment.char="#")

cluster_gt <- tab_data %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(highest_headshot, proj_headshot)),
    fn = function(x){
      web_image(
        url = x,
        height = px(35)
      )
    }
  ) %>% 
  cols_label(
    cluster = "Cluster",
    highest_drafted_player = "Example Player",
    highest_headshot = "",
    proj_player = "Draft Player",
    proj_headshot = "",
    total_epa_avg = "Avg. Total EPA",
    epa_per_rec_avg = "EPA Per Rec.",
    avg_fourty = "Avg. 40",
    avg_round = "Avg. Round",
    avg_pick = "Avg. Pick") %>%
  data_color(
    columns = vars(avg_pick),
    colors = scales::col_numeric(
      palette = c("#7fbf7b", "#f7f7f7", "#af8dc3"),
      domain = c(75, 150)
    )
  ) %>% 
  tab_header(
    title = "NFL Draft Wide Receiver Cluster Breakdown"
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

gtsave(cluster_gt, "wr_clusters_3.png")




