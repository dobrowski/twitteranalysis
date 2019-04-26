


### Load Libraries ----------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(  
          rtweet
        ,tidyverse
        ,tidygraph
        ,ggraph
        ,igraph
        ,hrbrthemes
        ,visNetwork
        )


# create_token(
#   app = "MCOE-Twitter",
#   consumer_key = "xxx",
#   consumer_secret = "xxx",
#   access_token = "x-xx",
#   access_secret = "xxx")
# 



#### Follower information   -------

rt <- search_tweets(
  "@MCOE_Now", n = 18000, include_rts = FALSE
)


cl_flw <- get_followers("pk12innovation", n = 75000)

je_flw <- get_followers("ElGovEcon", n = 75000)


core_flw <- intersect(cl_flw$user_id,je_flw$user_id)


cl_flw_data <- lookup_users(cl_flw$user_id)

cl_flw_data_filtered <- cl_flw_data %>% filter(friends_count > 0)


friends <- list()

# start loop
for (a in 1:length(core_flw)){
  friends[[a]] <- get_friends(core_flw[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*62) # must enter time in seconds
  }
}

# Combine data tables in list
friends.combo <- bind_rows(friends) %>% 
  rename(friend = user_id)


friends.too <- c("35122227","761639994708533248", "868598238596571136", "727122895", "711786917939916801" )

write_rds(friends.combo, "friends.rds")

net <- friends.combo %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

glimpse(net)


g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g


ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()


mat <-net %>%
  select(-count) %>%
  as.matrix()

routes_igraph <- graph_from_edgelist(mat)

plot(routes_igraph)


fds <- get_friends(c("pk12innovation", "ElGovEcon", "MCOE_Now"))
## frequency count of accounts followed by the users queried above
tbl <- table(fds$user_id)
## subset fds data to only those followed by 3 or more
fds3 <- subset(fds, user_id %in% names(tbl[tbl > 2L]))
## convert fds3 to matrix
mat <- as.matrix(fds3)
## convert to graph object
mat2 <- igraph::graph_from_edgelist(mat)
## plot network
plot(mat2)







####  Retweet Networks ------

MCOE_now <- search_tweets("MCOE", n=1500)

rt_g <- MCOE_now %>%
#  filter(retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() 


summary(rt_g)


ggplot(data_frame(y=degree_distribution(rt_g), x=1:length(y))) +
  geom_segment(aes(x, y, xend=x, yend=0), color="slateblue") +
  scale_y_continuous(expand=c(0,0), trans="sqrt") +
  labs(x="Degree", y="Density (sqrt scale)", title="#rstats Retweet Degree Distribution") # +
#  theme_ipsum_rc(grid="Y", axis="x")

V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 1, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 1, degree(rt_g), 0)) 


ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_link(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE,  fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted screen names labeled. Darkers edges == more retweets. Node size == larger degree") +
#  theme_graph(base_family=font_rc) +
  theme(legend.position="none")


ggraph(rt_g) +
  geom_edge_link(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_point() +
  theme_graph() +
  geom_node_text(aes(label = node_label, size=node_size), repel = TRUE) +
  # geom_node_label(aes(label=node_label, size=node_size),
  #                 label.size=0, fill="#ffffff66", segment.colour="springgreen",
  #                 color="slateblue", repel=TRUE,  fontface="bold")

  
visIgraph(rt_g)
data <- toVisNetworkData(rt_g)
visNetwork(nodes = data$nodes, edges = data$edges)