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


function pairwise_search(graph, lambda) {
    V <- gorder(graph)
    df <- data.frame(matrix(nrow=(V*(V-1))/2 , ncol=3))
    i = 1
    for (j in 1:(V-1)) {
        for (k in (j+1):V) {
            pair = c(j,k)
            measure = map_dbl(1:60, function(x) diffusion(graph, pair, lambda)) %>% mean
            df[i, ] = list(j=j, k=k, measure=measure) 
            print(str_interp("${i} out of ${V*(V-1)/2} done."))
            if (i == V*(V-1)/2) break
            i = i + 1
        }
    }
    return(df)
}
