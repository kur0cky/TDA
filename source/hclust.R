library(tidyverse)
library(dtw)
library(class)
library(TSclust)
library(ggdendro)

betti <- read_csv("data/mazda/processed/betti_seq.csv")


betti1 <- betti %>% 
  filter(dimension == 1) %>% 
  select(-dimension) %>% 
  group_by(name, type, number) %>% 
  nest() %>% 
  arrange(name, type,number) %>% 
  mutate(ID = 1:n())


data <- betti1 %>% 
  unnest() %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(dist = map(data,
                    ~ .x %>%
                      select(-number, -type) %>% 
                      spread(ID, Betti) %>% 
                      select(-radius) %>% 
                      t() %>% 
                      TSclust::diss("DTWARP")),
         clust = map(dist, 
                     ~ .x %>% 
                       hclust(method = "complete")))

for(i in 1:15){
  data$clust[[i]]$labels <- rep(c("after", "before"), each=10)
}

saveRDS(data, "data/mazda/processed/clust2.RDS")
for(i in 1:15){
  plot(data$clust[[i]], hang = -1)
}
for(i in 1:15){
  ggdendrogram(data$clust[[i]], rotate=TRUE)+
    theme_classic()+
    labs(y = "DTW", x="")
  ggsave()
}
