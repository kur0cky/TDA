library(tidyverse)
library(TDA)
library(TSclust)

betti <- read_csv("data/mazda/processed/betti_seq.csv")



tmp <- betti %>% 
  mutate(dimension = str_c("betti_", dimension)) %>% 
  spread(dimension, Betti) %>% 
  mutate(number = str_c(type, number)) %>% 
  group_by(name) %>% 
  nest() %>%
  mutate(betti1 = map(data,
                     ~ .x %>% 
                       select(number, betti_1, radius) %>% 
                       spread(number, betti_1) %>% 
                       select(-radius)),
         betti2 = map(data,
                     ~ .x %>% 
                       select(number, betti_2, radius) %>% 
                       spread(number, betti_2) %>% 
                       select(-radius)),
         DTW_betti1 = map(betti1, ~ TSclust::diss(.x, "DTWARP")),
         DTW_betti2 = map(betti2, ~ TSclust::diss(.x, "DTWARP")),
         MDS_betti1 = map(DTW_betti1, ~ data.frame(scale(cmdscale(.x, k=2))) %>% 
                            as_tibble() %>% 
                            dplyr::rename(MDS_betti11 = X1, MDS_betti12 = X2) %>%
                            rownames_to_column("number")),
         MDS_betti2 = map(DTW_betti2, ~ data.frame(scale(cmdscale(.x, k=2))) %>% 
                            as_tibble() %>% 
                            dplyr::rename(MDS_betti21 = X1, MDS_betti22 = X2) %>%
                            rownames_to_column("number")))


tmp %>% 
  saveRDS("data/mazda/processed/MDS_betti.RDS")

