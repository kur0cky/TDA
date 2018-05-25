library(tidyverse)
library(TDA)
library(TSclust)
library(Rmisc)

MDS_betti <- readRDS("data/mazda/processed/MDS_betti.RDS")
MDS_lands <- readRDS("data/mazda/processed/MDS_lands.RDS")
ts <- read_csv("data/mazda/processed/data.csv")


tmp <- MDS_betti %>% 
  full_join(MDS_lands, by = "name") %>% 
  mutate(mat = map2(MDS_land1, MDS_betti1,
                    ~ .x %>% 
                      left_join(.y, by = "number"))) %>% 
  mutate(mds_plot = map(mat,
                        ~ .x %>% 
                          ggplot(aes(MDS_lands11, MDS_betti11))+
                          geom_text(aes(label = number, colour = str_sub(number, 1, 1)), size=2)+
                          theme_bw()+
                          theme(legend.position = 'none')+
                          theme(text = element_text(size = 5))),
         mds_land_plot = map(mat,
                             ~ .x %>% 
                               ggplot(aes(MDS_lands11, MDS_lands12))+
                               geom_text(aes(label = number, colour = str_sub(number, 1, 1)), size=2)+
                               theme_bw()+
                               theme(legend.position = 'none')+
                               theme(text = element_text(size = 5))),
         mds_betti_plot = map(mat,
                             ~ .x %>% 
                               ggplot(aes(MDS_betti11, MDS_betti12))+
                               geom_text(aes(label = number, colour = str_sub(number, 1, 1)), size=2)+
                               theme_bw()+
                               theme(legend.position = 'none')+
                               theme(text = element_text(size = 5))))

tmp2 <- ts %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(ts_plot = map(data,
                       ~ .x %>% 
                         ggplot(aes(index, value))+
                         geom_line(size=.3)+
                         facet_grid(number~type)+
                         theme_bw()+
                         theme(text = element_text(size = 5))))

tmp3 <- tmp2 %>% 
  left_join(tmp, by = "name")


layout <- matrix(c(1,1,1,1,2,4,3,4), byrow=TRUE, ncol=2)
plots <- list()
for(i in 1:15){
  plots[[i]] <- grid.arrange(tmp3$ts_plot[[i]], tmp3$mds_land_plot[[i]], tmp3$mds_betti_plot[[i]], tmp3$mds_plot[[i]],
                             layout_matrix = layout)
  ggsave(plot = plots[[i]],
         str_c("report/fig/MDS_plot", i, ".png"),
         units = "cm",
         height = 12,
         width = 8)
}


tmp <- MDS_betti %>% 
  full_join(MDS_lands, by = "name") %>% 
  mutate(mat = map2(MDS_land2, MDS_betti2,
                    ~ .x %>% 
                      left_join(.y, by = "number"))) %>% 
  mutate(mds_plot = map(mat,
                        ~ .x %>% 
                          ggplot(aes(MDS_lands21, MDS_betti21))+
                          geom_text(aes(label = number, colour = str_sub(number, 1, 1)))+
                          theme_bw()+
                          theme(legend.position = 'none')+
                          theme(text = element_text(size = 5))),
         mds_land_plot = map(mat,
                             ~ .x %>% 
                               ggplot(aes(MDS_lands21, MDS_lands22))+
                               geom_text(aes(label = number, colour = str_sub(number, 1, 1)))+
                               theme_bw()+
                               theme(legend.position = 'none')+
                               theme(text = element_text(size = 5))),
         mds_betti_plot = map(mat,
                              ~ .x %>% 
                                ggplot(aes(MDS_betti21, MDS_betti22))+
                                geom_text(aes(label = number, colour = str_sub(number, 1, 1)))+
                                theme_bw()+
                                theme(legend.position = 'none')+
                                theme(text = element_text(size = 5))))


DTW_betti1 <- MDS_betti %>% 
  select(name, betti1) %>% 
  mutate(betti1 = map(betti1,
                      ~ .x %>% 
                        mutate(index = 1:nrow(.x)) %>% 
                        gather(key, value, -index))) %>% 
  unnest() %>% 
  filter(str_sub(key, 1, 1) == "a") %>% 
  mutate(name = fct_anon(as.factor(name))) %>% 
  transmute(name = str_c(name, key), index, betti1 = value) %>% 
  spread(name, betti1) %>% 
  select(-index) %>% 
  diss(METHOD = "DTWARP")


DTW_land1 <- MDS_lands %>% 
  mutate(name = fct_anon(as.factor(name))) %>% 
  select(name, land1) %>% 
  mutate(land1 = map(land1, 
                     ~ .x %>% 
                       mutate(index = 1:nrow(.x)) %>% 
                       gather(key, value, -index))) %>% 
  unnest() %>% 
  filter(str_sub(key, 1, 1) == "a") %>% 
  transmute(name = str_c(name, key), index, land1 = value) %>% 
  spread(name, land1) %>% 
  select(-index) %>% 
  diss(METHOD = "DTWARP")


MDS_betti1 <- cmdscale(DTW_betti1, k = 1) %>% 
  as.data.frame() %>% 
  rownames_to_column("name") 
MDS_land1 <- cmdscale(DTW_land1, k = 1) %>% 
  as.data.frame() %>% 
  rownames_to_column("name") 

MDS_land1 %>% 
  left_join(MDS_betti1, by = "name") %>% 
  mutate(arm = str_sub(name, 1,2)) %>% 
  as_tibble() %>% 
  ggplot(aes(V1.x, V1.y, colour = arm))+
  geom_text(aes(label = arm), size=2)+
  theme_bw()+
  theme(text = element_text(size = 5))

ggsave("report/fig/aftre_MDS.png",
       units = "cm",
       height = 8,
       width = 8)




## before ----
DTW_betti1 <- MDS_betti %>% 
  select(name, betti1) %>% 
  mutate(betti1 = map(betti1,
                      ~ .x %>% 
                        mutate(index = 1:nrow(.x)) %>% 
                        gather(key, value, -index))) %>% 
  unnest() %>% 
  filter(str_sub(key, 1, 1) == "b") %>% 
  mutate(name = fct_anon(as.factor(name))) %>% 
  transmute(name = str_c(name, key), index, betti1 = value) %>% 
  spread(name, betti1) %>% 
  select(-index) %>% 
  diss(METHOD = "DTWARP")


DTW_land1 <- MDS_lands %>% 
  mutate(name = fct_anon(as.factor(name))) %>% 
  select(name, land1) %>% 
  mutate(land1 = map(land1, 
                     ~ .x %>% 
                       mutate(index = 1:nrow(.x)) %>% 
                       gather(key, value, -index))) %>% 
  unnest() %>% 
  filter(str_sub(key, 1, 1) == "b") %>% 
  transmute(name = str_c(name, key), index, land1 = value) %>% 
  spread(name, land1) %>% 
  select(-index) %>% 
  diss(METHOD = "DTWARP")


MDS_betti1 <- cmdscale(DTW_betti1, k = 1) %>% 
  as.data.frame() %>% 
  rownames_to_column("name") 
MDS_land1 <- cmdscale(DTW_land1, k = 1) %>% 
  as.data.frame() %>% 
  rownames_to_column("name") 

MDS_land1 %>% 
  left_join(MDS_betti1, by = "name") %>% 
  mutate(arm = str_sub(name, 1,2)) %>% 
  as_tibble() %>% 
  ggplot(aes(V1.x, V1.y, colour = arm))+
  geom_text(aes(label = arm), size=2)+
  theme_bw()+
  theme(text = element_text(size = 5))

ggsave("report/fig/before_MDS.png",
       units = "cm",
       height = 8,
       width = 8)
