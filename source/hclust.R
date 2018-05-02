library(tidyverse)
library(dtw)
library(class)

betti <- read_csv("data/mazda/processed/betti_seq.csv")


betti1 <- betti %>% 
  filter(dimension == 1) %>% 
  select(-dimension) %>% 
  group_by(name, type, number) %>% 
  nest() %>% 
  arrange(name, type,number) %>% 
  mutate(ID = 1:n())

data <- betti1  %>% 
  unnest() %>% 
  select(ID, radius, Betti) %>% 
  spread(ID, Betti) %>% 
  select(-radius)


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
                       hclust()))

for(i in 1:15){
  data$clust[[i]]$labels <- rep(c("after", "before"), each=10)
}

plot(data$clust[[15]], hang = -1)

colnames(data) <- betti1$type

dist <- diss(data[1:20], "DTWARP")

h <- hclust(dist)
par(cex=0.6)
plot(h, hang = -1)


cl <- kmeans(data[1:20,], 2)

par(mar = c(9,9,9,9))
plot(data[,1:20], col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)


#SOM
gr <- somgrid(topo = "rectangular")
res <- SOM(data, gr)

plot(res)
