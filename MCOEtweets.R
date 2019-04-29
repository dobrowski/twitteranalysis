

### Load Libraries ----------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(  
  rtweet
  ,tidyverse
  ,tidygraph
  ,ggraph
  ,igraph
  #        ,hrbrthemes
  ,visNetwork
)



MCOE_all <- read_rds("MCOEtweets.rds")


MCOE_now <- search_tweets("MCOE", n=1500)

MCOE_all <- MCOE_all %>%
  bind_rows(MCOE_now) %>%
  unique() %>% 
  as.data.frame() %>%
  group_by(status_id) %>% 
  filter(row_number() == 1)


write_rds(MCOE_all, "MCOEtweets.rds")
