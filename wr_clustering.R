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
         td_rate = total_tds / total_targets) %>%
  dplyr::select(seasons, comp_perc, yards_per_target, yards_per_rec, first_down_perc,
                td_rate, fourty_time, vertical, height, weight, drop_rate, yards_per_game) %>%
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
  rename(c('S'='seasons', 'CP'='comp_perc', # give predictors a shorter name for plotting
           'YPT'='yards_per_target', 'YPR'='yards_per_rec',
           'FDP'='first_down_perc', 'TD'='td_rate',
           '40'='fourty_time', 'VT'='vertical', 'H'='height',
           'W'='weight', 'DR'='drop_rate', 'YPG'='yards_per_game')) %>%
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
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())
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

wr_metrics2 %>% 
  filter(round == 1 | round == 2 | round == 1000) %>%
  ggplot(aes(x=PC1, y=PC2, color=cluster, shape=cluster)) + 
  geom_point(alpha=0.3) + 
  geom_text_repel(aes(label = player), label.padding = 0.35) +
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('wr_clusters_2.png', width = 14, height = 10, dpi = "retina")





