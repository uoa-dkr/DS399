library(tidyverse)

heather <- vroom::vroom('data/heather.csv');heather

heather %>% 
  ggplot(aes(Heather)) + 
  geom_histogram() + 
  facet_grid(factor(Year)~Treat)

heather %>% 
  ggplot(aes(Heather)) + 
  geom_boxplot() + 
  facet_grid(Treat~.)
heather %>% 
  filter(Year !=2008) %>% 
  ggplot(aes(Heather)) + 
  geom_histogram() + 
  facet_grid(factor(Block)~Treat)


heather %>% 
  janitor::clean_names() %>% 
  ggplot(aes(native_dicots)) + 
  geom_histogram() + 
  facet_grid(factor(year)~treat)

heather %>% 
  janitor::clean_names() %>% 
  ggplot(aes(native_monocots)) + 
  geom_histogram() + 
  facet_grid(factor(year)~treat)

heather09_12 <- heather %>% 
  janitor::clean_names() %>% 
  mutate(year = as_factor(year), block = as_factor(block)) %>% 
  filter(year != 2008)

heather09_12 %>% 
  lm(heather~year+treat+block,data=.) %>% summary(.)

heather09_12 %>% 
  lm(heather~year+treat,data=.) %>% summary(.)


f <- heather09_12 %>%
  lm(cbind(heather,native_dicots,native_monocots)~year+treat+block,data=.)

car::Manova(f) %>% summary(.)





