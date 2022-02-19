library(tidyverse)
library(igraph)
library(tidygraph)

df <- read.csv(
               header=FALSE,
               skip=2,
               #sep=" ",
               file="./data/moreno_lesmis/data_lesmis.csv"
)

lesmis_graph <- graph_from_data_frame(df) %>% as_tbl_graph()

save(lesmis_graph, file="./data/lesmis_graph.RData")
