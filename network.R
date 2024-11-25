network =  z  %>% 
  group_by(Partido) %>%
  summarise(text = paste(text,collapse = " ")) %>% 
  ungroup() 
  unnest_tokens(bigram,text,token = "ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) %>% 
  #   unite(bigram, word1, word2, sep = " ") %>%
  #   anti_join(omision2)  %>% 
  # separate(bigram, c("word1", "word2"), sep = " ")  %>% 
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) %>% 
  filter(weight > 10) %>%
  graph_from_data_frame()


V(network)$degree <- strength(graph = network)
E(network)$width <- E(network)$weight/max(E(network)$weight)
library(networkD3)
library(magrittr)
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 15,
  zoom = TRUE, 
  opacityNoHover = 1,
  linkDistance = 45,
  arrows = TRUE
)
clusters(graph = network)

network =  z  %>% 
  group_by(Partido) %>%
  summarise(text = paste(text,collapse = " ")) %>% 
  ungroup() %>% 
  unnest_tokens(bigram,text,token = "ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(! is.na(word1)) %>% 
  filter(! is.na(word2)) %>% 
  #   unite(bigram, word1, word2, sep = " ") %>%
  #   anti_join(omision2)  %>% 
  # separate(bigram, c("word1", "word2"), sep = " ")  %>% 
  count(word1, word2, sort = TRUE) %>%
  rename(weight = n) %>% 
  filter(weight > 20) %>%
  graph_from_data_frame(directed = FALSE)

V(network)$cluster <- clusters(graph = network)$membership

cc.network <- induced_subgraph(
  graph = network,
  vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
)


# Store the degree.
V(cc.network)$degree <- strength(graph = cc.network)
# Compute the weight shares.
E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)

comm.det.obj <- cluster_louvain(
  graph = cc.network, 
  weights = E(cc.network)$weight
)

V(cc.network)$membership <- membership(comm.det.obj)

network.D3$nodes$Group <- clusters(graph = network)$membership

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 12,
  zoom = TRUE, 
  opacityNoHover = 1
)


# Correlaciones -----------------------------------------------------------

network = z %>%
  mutate(section = row_number() %/% 15) %>%
  filter(section > 0) %>% 
  unnest_tokens(word,text) %>% #Función unnest, cada palabra es una observación (sin signos)
  anti_join(stop_words) %>% #Elimino stop words
  filter(word != "fuerza",word != "popular",word !="morado") %>% 
  filter(grepl("^[A-Za-z]+$",word)) %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  filter(Partido == "Partido_Morado") %>% 
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(correlation > .40) %>%
  rename(weight = correlation) %>% 
  graph_from_data_frame(directed = FALSE) 

  # network %>% 
  #   ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  # geom_node_point(color = "lightblue", size = 5) +
  # geom_node_text(aes(label = name), repel = TRUE) +
  # theme_void()

V(network)$degree <- strength(graph = network)
E(network)$width <- E(network)$weight/max(E(network)$weight)
library(networkD3)
library(magrittr)
network.D3 <- igraph_to_networkD3(g = network)
# Define node size.
network.D3$nodes %<>% mutate(Degree = (1E-2)*V(network)$degree)
# Degine color group (I will explore this feature later).
network.D3$nodes %<>% mutate(Group = 1)
# Define edges width. 
network.D3$links$Width <- 10*E(network)$width

forceNetwork(
  Links = network.D3$links, 
  Nodes = network.D3$nodes, 
  Source = 'source', 
  Target = 'target',
  NodeID = 'name',
  Group = 'Group', 
  opacity = 0.9,
  Value = 'Width',
  Nodesize = 'Degree', 
  # We input a JavaScript function.
  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
  fontSize = 15,
  zoom = TRUE, 
  opacityNoHover = 1,
  linkDistance = 45,
  arrows = TRUE
)
