library(tidytext)



stop <- get_stopwords()
stopwords <- stop$word


remove_reg <- "&amp;|&lt;|&gt;"



text <- eds %>%
  select(text, screen_name, retweet_status_id) %>%
  mutate(line = row_number()) 

tidy.df <- text %>%
  filter(is.na(retweet_status_id) ) %>%  #removes retweets 
  select(-retweet_status_id) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%  # removes ampersands
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!str_detect(word,"t.co")) %>%   # remove urls
  filter(!word %in% stop$word) %>%
  left_join(emojis, by = c("word" = "code")) %>% # replaces emojis with descriptive name in < > 
  mutate(word = case_when(is.na(description) ~ word,
                              !is.na(description) ~ paste0("<",description,">")) )



inverse.doc.freq <- tidy.df %>%
  count(screen_name, word, sort = TRUE) %>%
  bind_tf_idf(word, screen_name, n) %>%
  arrange(-tf_idf)



top.words <- tidy.df %>%
  anti_join(stop) %>%
  count(screen_name, word, sort = TRUE) %>%
  arrange(-n)



top.words %>%
  group_by(screen_name) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, n),
             n, 
             fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~screen_name, scales = "free") +
  theme_hc() +
  scale_fill_few("Medium")



inverse.doc.freq %>%
  group_by(screen_name) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, tf_idf),
             tf_idf, 
             fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~screen_name, scales = "free")+
  theme_hc() +
  scale_fill_few("Medium")



# 
# emotest <- top.words %>% left_join(emojis, by = c("word" = "code"))
# 
# emotest <- emotest %>% 
#   mutate(replaced = case_when(is.na(description) ~ word,
#                               !is.na(description) ~ paste0("<",description,">")) )
