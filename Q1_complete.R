library(igraph)
library(ggraph)
library(tidyverse)
set.seed(0)

#sample a graph
sample_ppm <- function(memb, p, q) {
  mat <- t(combn(seq_along(memb), 2))
  prob <- c(q, p)[1 + (memb[mat[, 1]] == memb[mat[, 2]])]
  el <- mat[which(runif(nrow(mat)) < prob), ]
  graph_from_edgelist(el, directed = FALSE)
}
memb <- sample(1:2,20, replace = TRUE)
graph <- sample_ppm(memb, 0.14, 0.06)


graph %>%
  ggraph() +
  geom_edge_link0(colour = 'grey75') +
  geom_node_point(aes(col = factor(memb)), show.legend = FALSE) +
  scale_colour_brewer(palette = 'Set1') +
  theme_void()+ggtitle('p=0.14 and q=0.06')


#load graph

load("C:\\Users\\karre\\OneDrive\\Documentos\\GitHub\\hw_3\\data\\lesmis_graph.RData")

V(lesmis_graph)

seed=sample(1:20,2)
#the SI_model function simulates the data generating process, the network creation and the infection process, for different infection probabilities, n, d, e and c
diffusion <- function(graph, lambda,seed){
  #create a vector with the vertices
  vertices<-V(graph)
  #count the number of vertices
  n<-length(vertices)
  #create a unit vector of that indicates the diffusions status of a vertex
  state<-vector(length=n)
  #sample tau for every vertex
  tauvector=rnorm(n, mean=lambda, sd=0.5)
  tauvector=unlist(lapply(tauvector, function(x) ifelse(x>0, x, 0)))
  #bind three vectors into dataframe
  infection_state=cbind(vertices, state,tauvector)
  infection_state=as.data.frame(infection_state)
  #set the seed and infect first pair
  infection_state[seed,2]=1
  state[seed]=1
  
  #run four times through all the vertices
  for (i in 1:4) {
    for (vertex in vertices){
      #get the vertex neighbours
      vertex_friends=unique(unlist(adjacent_vertices(graph,vertex)))
      #get the diffusion status of all friends
      friends_status=infection_state[vertex_friends,2]
      #sum the diffusion status of the vertex´ friends
      infection_sum=sum(friends_status)
      #check if this sum is greater than 
      if (infection_sum>tauvector[vertex]){state[vertex]=1}
    }
  #change temporary infection state to permanent
  infection_state$state=state}
 
  
  #return proportion of nodes infected at the end of the four runs
  return(sum(infection_state$state)/n)
}


pairwise_search<- function(graph, lambda) {
  V <- gorder(graph)
  df <- data.frame(matrix(nrow=(V*(V-1))/2 , ncol=3))
  i = 1
  for (j in 1:(V-1)) {
    for (k in (j+1):V) {
      pair = c(j,k)
      measure = map_dbl(1:30, function(x) diffusion(graph, lambda, pair)) %>% mean
      df[i, ] = list(j=j, k=k, measure=measure) 
      print(str_interp("${i} out of ${V*(V-1)/2} done."))
      if (i == V*(V-1)/2) break
      i = i + 1
    }
  }
  return(df)
}
result_lambda2 <-pairwise_search(lesmis_graph, 2)
save(result_lambda2, file="C:\\Users\\karre\\OneDrive\\Documentos\\GitHub\\hw_3\\data\\lambda_2")