library(tidyverse)
library(tidytext)
library(glue)

words <- read_csv('data/sentiment-data.csv', col_names = F);words

col1_words <- words %>% 
  slice(-1) %>% 
  select(1) %>% 
  unnest_tokens(output = word, input = X1, token = str_split, pattern = ',|、|\\*|\\. |，') %>% 
  mutate(word = str_trim(word)) %>% 
  filter(str_count(word, '\\ ') < 4 & str_length(word) > 2) %>%
  mutate(word = ifelse(str_count(word, '\\ ') == 3, str_split(word, '\\ '), word)) %>% 
  unnest(word) %>% 
  mutate(word = str_remove(word, '.{8}\\)|\\ to$|^be\\ |^bit\\ |.{18}'),
         word = str_split(word, '\\/')) %>%
  unnest(word) %>% 
  filter(str_length(word) > 0)
col1_words %>% pull()

col2_words <- words %>% 
  slice(-1) %>% 
  select(2) %>%
  unnest_tokens(output = word, input = X2, token = str_split, pattern = ', |\\*') %>% 
  mutate(word = ifelse(str_count(word, '\\ ') == 2, str_split(word, '\\ '), word)) %>% 
  unnest(cols = word) %>% 
  filter(str_length(word) > 3) %>% 
  mutate(word = str_remove(word, '^h.{9}'))
col2_words %>% pull()

col3_words <- words %>% 
  slice(-1) %>% 
  select(3) %>% 
  mutate(word = X3,
         word = str_remove(word, '\\(.{1,}\\)|^\\*.')) %>% 
  select(-1) %>% 
  filter(str_length(word) > 1 & !(str_count(word, '\\ ') > 1))
col3_words %>% pull()

walk2(list(col3_words,col2_words,col1_words),glue('data/col{3:1}_words.csv'),write_csv)

