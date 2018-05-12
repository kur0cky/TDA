library(tidyverse)

lands <- read_csv("data/mazda/processed/lands.csv")


lands %>% 
  ggplot(aes(tseq, land_2, group=number))+
  geom_line()+
  theme_bw()+
  facet_grid(name~type, scales = "free_y")

lands %>% 
  ggplot(aes(tseq, land_1, group=number))+
  geom_line()+
  theme_bw()+
  facet_grid(name~type, scales = "free_y")
