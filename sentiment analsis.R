library(tidyverse)
library(tidytext)
library(ggrepel)
library(plotly)

words <- read_csv('data/sentiment-data.csv', col_names = F);words


# complicated data cleaning :(
col1_words <- words %>% 
  slice(-1) %>% 
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


# top 20 most frequent words in question one with its visualization
top_20 <- col1_words %>% 
  count(word, sort = T) %>% 
  head(20);top_20

top_20 %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = word)) + 
  geom_col(show.legend = F, width = .5) + 
  scale_fill_viridis_d() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_text(angle = 0, vjust = .5, size = rel(1.2))) +
  xlab('frequent') +
  xlim(c(0,70)) + 
  geom_text(aes(label=word), hjust = -.5)


# sentiment words comparison
p <- col1_words %>% 
  count(word) %>% 
  inner_join(get_sentiments()) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 9) %>% 
  ungroup() %>% 
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(n, word, fill = sentiment)) +
  labs(x = "Contribution to sentiment")
ggplotly(p)

col1_words %>% 
  count(word) %>% 
  inner_join(get_sentiments()) %>% 
  ggplot(aes(0,0)) +
  geom_text_repel(aes(label = word, size = n, colour = sentiment),
                  force_pull = 0, max.overlaps = Inf,
                  segment.color = NA, point.padding = NA, seed = 399, show.legend = F) +
  facet_grid(~ sentiment) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = "", y = "")


(df <- read_csv('data/kaka-i-cleandata.csv'))

df %>% 
  group_by(instructor_feelings) %>% 
  summarise(count = n()) %>% 
  inner_join(get_sentiments(), by = c('instructor_feelings' = 'word')) %>% 
  ggplot(aes(0,0)) +
  geom_text_repel(aes(label = instructor_feelings, size = count*10, colour = sentiment),
                  force_pull = 0, max.overlaps = Inf,
                  segment.color = NA, point.padding = NA, seed = 399, show.legend = F) +
  facet_grid(~ sentiment) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = "", y = "")

df %>% 
  group_by(instructor_feelings) %>% 
  summarise(count = n()) %>% 
  mutate(instructor_feelings = fct_reorder(instructor_feelings, count)) %>% 
  ggplot(aes(count, instructor_feelings, fill = instructor_feelings)) + 
  geom_col(show.legend = F, width = .5) + 
  scale_fill_viridis_d()

data %>% 
  select(3) %>% 
  inner_join(get_sentiments('bing'), by=c('instructor_feelings' = 'word')) %>% 
  count(instructor_feelings, sort = T) %>%
  rename(word = instructor_feelings) %>% 
  inner_join(get_sentiments("bing")) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green2"), match.colors = T,max.words = 100)
