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


lesmis_graph %>%
  ggraph() +
  geom_edge_link0(colour = 'grey75') +
  geom_node_point( show.legend = FALSE) +
  scale_colour_brewer(palette = 'Set1') +
  theme_void()+ggtitle('Les Miserables Network with 76 Vertices and 250 Edges ')


#load graph

load("C:\\Users\\karre\\OneDrive\\Documentos\\GitHub\\hw_3\\data\\lesmis_graph.RData")


seed=sample(1:20,2)
#the diffusion function simulates a simple diffusion process that follows the model in the excercise. 
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

load("C:\\Users\\karre\\OneDrive\\Documentos\\GitHub\\hw_3\\data\\lambda_2")

optimal_pair=result_lambda2[which.max(result_lambda2$X3),]
pair=c(41,42)

g2 <- induced.subgraph(graph=lesmis_graph,vids=pair)
lesmis_graph %>%
  ggraph() +
  geom_edge_link0(colour = 'grey75') +
  geom_node_point( show.legend = FALSE) +
  geom_node_point(aes(colour="red"),g2,show.legend = FALSE)+
  scale_colour_brewer(palette = 'Set1') +
  theme_void()+ggtitle('Les Miserables Network with 76 Vertices and 250 Edges ')
#result, pair= 41 and 42 with an information rate of 0.2122807
#the following function performs the same diffusion simulation but calculates the mean information rate of 1000 seed-pairs sampled at random
random_search<- function(graph, lambda) {
 result=c()
 n=gorder(graph)
   for (i in 1:500) {
    seed=sample(1:n,2)
    information_measure=diffusion(graph, lambda, seed)
    result=c(result, information_measure)
    
  } 
 
  
  return(mean(result))
}

# run the random search with lambda=1 and lambda=2
random_search(lesmis_graph, 1)
# result=0.1959211
random_search(lesmis_graph, 2)
#result= 0.03736842
