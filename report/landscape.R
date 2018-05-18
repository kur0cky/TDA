library(tidyverse)
library(TDA)
library(TSclust)

lands <- read_csv("data/mazda/processed/lands.csv")


lands %>% 
  ggplot(aes(tseq, land_1, group=number))+
  geom_line(size=.3)+
  facet_grid(name ~ type, scales="free_y")+
  labs(x = "radius", y="landscape")+
  theme_bw()+
  theme(text = element_text(size=5))
ggsave("report/fig/land_1.png",
       height = 12,
       width = 8,
       units = "cm")

lands %>% 
  ggplot(aes(tseq, land_2, group=number))+
  geom_line(size=.3)+
  facet_grid(name ~ type, scales="free_y")+
  labs(x = "radius", y="landscape")+
  theme_bw()+
  theme(text = element_text(size=5))
ggsave("report/fig/land_2.png",
       height = 12,
       width = 8,
       units = "cm")
