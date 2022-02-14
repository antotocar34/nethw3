library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)


edge_df = read_csv("./test.csv") %>% tibble()
node_ids = tibble(node_id = unique(c(edge_df$from, edge_df$to)))

graph = graph_from_data_frame(edge_df, directed=FALSE) %>% as_tbl_graph()

graph = graph %>% activate(nodes) %>% mutate(informed = 0) %>% 
                  activate(edges) %>% mutate(v = 0)
          


runOnePeriod = function(lambda, graph) {
    graph = graph %>% activate(nodes) %>% 
                      mutate(tau = rnorm(gorder(graph), mean=lambda, sd=sqrt(0.5))) %>% 
                      mutate(tau = ifelse(tau < 0, 0, tau))
}


