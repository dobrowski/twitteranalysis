library(tidytext)



stop <- get_stopwords()

remove_reg <- "&amp;|&lt;|&gt;"



text <- eds %>%
  select(text, screen_name) %>%
  mutate(line = row_number()) 

tidy.df <- text %>%
  filter(!str_detect(text, "^RT")) %>%  #removes retweets 
  mutate(text = str_remove_all(text, remove_reg)) %>%  # removes ampersands
  unnest_tokens(word, text, token = "tweets")

inverse.doc.freq <- tidy.df %>%
  count(screen_name, word, sort = TRUE) %>%
  bind_tf_idf(word, screen_name, n) %>%
  arrange(-tf_idf)



top.words <- tidy.df %>%
  anti_join(stop) %>%
  count(screen_name, word, sort = TRUE) %>%
  arrange(-n)



inverse.doc.freq %>%
  group_by(screen_name) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, tf_idf),
             tf_idf, 
             fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~screen_name, scales = "free")


top.words %>%
  group_by(screen_name) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(word, n),
             n, 
             fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~screen_name, scales = "free")



