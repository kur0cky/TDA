library(tidyverse)
library(TDA)
library(TSclust)

lands <- read_csv("data/mazda/processed/lands.csv")

# DTW----

tmp <- lands %>% 
  mutate(number = str_c(type, number)) %>% 
  group_by(name) %>% 
  nest() %>%
  mutate(land1 = map(data,
                     ~ .x %>% 
                       select(number, land_1, tseq) %>% 
                       spread(number, land_1) %>% 
                       select(-tseq)),
         land2 = map(data,
                    ~ .x %>% 
                      select(number, land_2, tseq) %>% 
                      spread(number, land_2) %>% 
                      select(-tseq)),
         DTW_land1 = map(land1, ~ TSclust::diss(.x, "DTWARP")),
         DTW_land2 = map(land2, ~ TSclust::diss(.x, "DTWARP")),
         MDS_land1 = map(DTW_land1, ~ data.frame(scale(cmdscale(.x, k=2))) %>% 
                           as_tibble() %>%
                           dplyr::rename(MDS_lands11 = X1, MDS_lands12 = X2) %>%
                           rownames_to_column("number")),
         MDS_land2 = map(DTW_land2, ~ data.frame(scale(cmdscale(.x, k=2))) %>%
                           as_tibble() %>%
                           dplyr::rename(MDS_lands21 = X1, MDS_lands22 = X2) %>%
                           rownames_to_column("number")))

tmp %>% saveRDS("data/mazda/processed/MDS_lands.RDS")
