# Intro Network Exercise with R

# 0. Set up working directory

setwd("c:/Users/bleng/Documents/tanitas/ELTE_ANET")

# 0. Install and call in package
install.packages("igraph") # In case you have not installed the package yet. 
                            #It is worth to repeat installation on a -say- yearly basis. Igraph R syntax is changing frequently.

install.packages("sand")

library(igraph) # Call this package in case you work with network objects.
library(sand) # Sand is important for teaching purposes only: it contains built-in data.


# 1. NETWORK DATA

# Undirected network
  g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                   4-7, 5-6, 6-7) # We manually create a new network object in igraph by adding the edges

  V(g) # We call the vertex sequence of g.

  E(g) # We call the edge sequence of g.

  plot(g) # Simple plot for simple networks.

  get.adjacency(g) # to see how storing data in matrices would look like
  
  # Delete vertex
  g = delete.vertices(g, 6)
  
  
# Directed network
  dg <- graph.formula(1-+2, 1+-3, 2++3) # We create the directed network object dg.
  plot(dg)

  V(dg)$name <- c("Sam", "Mary", "Tom") # We add name attributes to the vertex sequence.
  plot(dg)
  
  dg = delete.vertices(dg, "Sam")
  

# Weighted network 
  set.seed(42) # Seed is set so that randomization in the next line always gives the same result.
  E(g)$weight <- runif(ecount(g)) # runif: we set edge weight to be a random number from the uniform [0,1] distribution. 
                                  # ecount: the number of edges in g
  
  is.weighted(g)
  plot(g, edge.width=E(g)$weight*10)
  
  
# Simplify networks
  is.simple(g) # Simple networks do not contain self-loops and redundant edges.
  
  mg <- g + edge(2,3) # We create a non-simple network by adding a redundant edge.
  is.simple(mg)
  
  E(g)
  E(mg)
  
  E(mg)$weight <- 1 # The non-simple network is given a uniform edge weight.
  wg2 <- simplify(mg) # unify redundant edges / in this case, summing their weights
  is.simple(wg2)
  
  E(wg2)$weight
  
  ?simplify

  
# 2. BIPARTITE NETWORKS

  kevin=read.table("kevin_bacon.csv", header=T, sep=",") # read in movie-actor data in a data.frame format
  
  kevin_edgelist=merge(kevin, kevin, by="movie", all=T) # create co-occurrence edgelist by merging data to itself
  names(kevin_edgelist)=c("movie", "actor1", "actor2") # give names to the columns
  
  kevin_edgelist=kevin_edgelist[which(kevin_edgelist$actor1!=kevin_edgelist$actor2),c(2:3)] 
                                                        # keep only those edges where actors are not identical
                                                        # drop the column of movie names
  
  kevin_net=graph_from_data_frame(kevin_edgelist, directed = T, vertices = NULL)

  plot(kevin_net)
  
  kevin_net=simplify(kevin_net)  

  
# 3. NETWORK AND NODE CHARACTERISTICS
  
# Degree distribution
  data(karate) # very famous data collected from the Zachary karate club
  plot(karate)
  
  hist(degree(karate), col="lightblue", xlim=c(0,50),
       xlab="Vertex Degree", ylab="Frequency", main="") # Degree distribution with histogram
  
  hist(graph.strength(karate), col="pink",
       xlab="Vertex Strength", ylab="Frequency", main="") # Weighted degree is also called "Strenght" of the vertex
  
  
  library(igraphdata)
  data(yeast)
  
  ecount(yeast)
  vcount(yeast)
  
  is.weighted(yeast)
  is.directed(yeast)
  
  d.yeast <- degree(yeast) # degree gives back a vector of same length as V
  hist(d.yeast,col="blue",
       xlab="Degree", ylab="Frequency",
       main="Degree Distribution")
  
  dd.yeast <- degree.distribution(yeast) # degree.distribution calculates relative frequency of a given degree
                                         # result is a numeric vector of the same length as maximum degree plus one (which is zero degree)
  d <- 1:max(d.yeast)-1                 # a sequence that goes from 1 to the maximum degree
  ind <- (dd.yeast != 0)                # create an index for dropping the zero from degree distribution
  plot(d[ind], dd.yeast[ind], log="xy", col="blue",
       xlab=c("Log Degree"), ylab=c("Log Probability"),
       main="Log-Log Degree Distribution") # Plot degree probability on a log-log scale
  
# Node Centralities
  V(karate)$degree=degree(karate) # node characteristics can be added to the nodelist in the network object / useful to store
  V(karate)$betweenness=betweenness(karate, vids = V(karate))
  V(karate)$closeness=closeness(karate, vids = V(karate))
  
# Edge betweenness and Coreness / See motivation in the Lecture example!

  eb <- edge.betweenness(karate)
  E(karate)$edge_b= edge.betweenness(karate) # edge characteristics can be added to the edgelist in the network object
  E(karate)[order(eb, decreasing=T)[1:3]]

  plot(karate, edge.width=edge.betweenness(karate)/15)
  
  cores <- graph.coreness(karate) # calculatees k-coreness. 
        #The k-core of graph is a maximal subgraph in which each vertex has at least degree k
  plot(karate, edge.width=edge.betweenness(karate)/15, vertex.size=graph.coreness(karate)*5)
  
  
  
# Network measures: Density, Transitivity, Average Path Length
  
  graph.density(karate)
  
  transitivity(karate) ## [1] 0.2556818
  
  transitivity(karate, "local", vids=c(1,34))  ## [1] 0.1500000 0.1102941
  
  average.path.length(yeast)
  diameter(yeast)
  

# 4. EXERCISE: CO-INVENTOR NETWORK
  
  patents=read.table("patents.csv", header=T, sep=",") # read in patent data

  # 4.1 Create the network
    ## edgelist
    ## weighting and simplification
  
  # 4.2 Network characteristics
    ## degree distribution
    ## density and transitivity
  
  # 4.3 USA-Germany comparison
    ## keep those links in the network, in which both inventors are in the same country
    ## repeat 4.2 separately for the two countries
  
# 5. Community structure
  
  kc <- fastgreedy.community(karate) # running the fast greedy algorithm to identify communities
  
  # kc is a community structure object containing the following elements:
  
  length(kc) # the number of communities
  sizes(kc) # size distribution of community structure
  membership(kc) # node characteristics 
  
  modularity(kc) # calculates the Q index of modularity
  
  plot(kc,karate) # simple but nice plot of communities
  

  # Community finding algorithms and their running times: 
  # https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6/
    
    
    
# 6. Exercise: the global airline network
setwd("c:/Users/bleng/Documents/tanitas/ELTE_ANET")
library(igraph)

  # 6.0 Read, clean the data and plot the network
  air_nodes=read.table("airlines_nodes.csv", header=T, sep=",")
  air_edges=read.table("airlines_edges.csv", header=T, sep=";")
  
  air_g=graph_from_data_frame(air_edges, vertices=air_nodes, directed=F) # network object from edgelist and nodelist
  air_g=simplify(air_g) # remove self-loops and multiple edges

  coordinates<-matrix(c(V(air_g)$lng, V(air_g)$lat),nrow=length(V(air_g)$lat),ncol=2) # coordinate matrix to locate nodes in geographical networks
  
  # plot the network into a PNG file
  png(filename="air_net.png", width=6000, height=2000, units="px")
  plot(air_g,
              layout=coordinates, 
              # vertex.color=V(air_g)$col, 
              # vertex.frame.color=V(air_g)$fc, 
              vertex.size=0.5,
              # edge.color=adjustcolor("gray50", alpha.f=0.8), edge.curved=F,
              # #           edge.width=E(air_g)$weight/1000,
              axes=F, frame=F, rescale=T,
              vertex.label=NA
  )
  dev.off()
  
  
  # Run community finding algorithm 
  
  air_c=cluster_louvain(air_g) # the Louvain algorithm is a very popular method that is fast on large graphs
  
  # plot the network with built-in community visualization
  png(filename="air_comm.png", width=6000, height=2000, units="px")
  plot(air_c, air_g,
       layout=coordinates, 
       # vertex.color=V(air_g)$col, 
       # vertex.frame.color=V(air_g)$fc, 
       vertex.size=0.5,
       # edge.color=adjustcolor("gray50", alpha.f=0.8), edge.curved=F,
       # #           edge.width=E(air_g)$weight/1000,
       axes=F, frame=F, rescale=T,
       vertex.label=NA
  )
  dev.off()
  
  # 6.1 Calculate network parameters that can be used to estimate the speed of virus diffusion
  # 6.2 Identify the 5-10-20 most important airports that should be blocked in order to stop the virus
  # 6.3 Remove the blocked airports from the network and re-calculate measures in 6.1
  
  
  
# CODES SHOULD BE SAVED AS "NAME_ELTA_ANET.R". PLEASE SEND THE FILE TO lengyel.balazs@krtk.mta.hu.