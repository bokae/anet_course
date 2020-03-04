## simple script for seminar 04_networks_plots

# packages
library(igraph)




### 2 - Network basics --reminder

# network from adjacency matrix
Knet <- graph.adjacency(as.matrix(read.csv("clusternetwork2012.csv", sep = ";",
                                           header = TRUE, row.names = 1)))
Knet


# degree distr plot
hist(degree(Knet), col = "darkgreen", main = "Degree distribution plot")


# plot the network
set.seed(10) # for reproduction
layout1 <- layout_with_graphopt(Knet, niter=800, charge=0.2)# set up the layout

plot(Knet, layout=layout1,
     vertex.color="lightgray", edge.color="black",
     vertex.size=25, edge.arrow.size=0.4)


# data frame
df <- data.frame(V(Knet)$name, degree(Knet))
colnames(df) <- c("firm_id", "degree")
head(df)




### 3 - Network measures

# lc - local clustering
lc <- transitivity(Knet, type="local")
lc


# brokerage based on Burt's constraint measure
brokerage <- constraint(Knet)
brokerage


# check the distribution of constraint based brokerage
hist(lc, col="darkblue", main='', xlab='Local clustering', xlim=c(0,1))


# data frame
df <- cbind(df, lc, brokerage)


# create a color column based on above median brokerage
df$broker_col <- "lightgray"
df$broker_col[which(df$brokerage > median(df$brokerage))] <- "darkblue"


# plot - brokerage
set.seed(10) # for reproduction
layout1 <- layout_with_graphopt(Knet, niter=800, charge=0.2) # set up the layout

plot(Knet, layout=layout1,
     vertex.color=df$broker_col, edge.color="black",
     vertex.size=25, edge.arrow.size=0.4)


# simple correlations
cor(df$degree, df$brokerage) 

cor(df$degree, df$lc)

cor(df$brokerage, df$lc)



#### TASK 1
Knet <- graph.adjacency(as.matrix(read.csv("clusternetwork2012.csv", sep = ";",
                                           header = TRUE, row.names = 1)))

df$betw <- betweenness(Knet)
df$betw_col <- "lightgray"
df$betw_col[which(df$betw > mean(df$betw))] <- "red"


set.seed(10) # for reproduction
layout1 <- layout_with_graphopt(Knet, niter=800, charge=0.2) # set up the layout

plot(Knet, layout=layout1,
     vertex.color=df$betw_col, edge.color="black",
     vertex.size=25, edge.arrow.size=0.4)

cor(df$betw, df$brokerage)

#### TASK 1 - END





### 5 - Create random graphs

# Erdos-Renyi random graph
set.seed(10) # run together with the function for reproduction
ernet <- erdos.renyi.game(n = 100, p.or.m = 350, type = 'gnm')
ernet


# degree dist plot
hist(degree(ernet), main='Erdos-Renyi random - degree distribution',
     col='blue', xlab='Degree',
     xlim=c(0, max(degree(ernet))))


# Barabasi-Albert random graph
set.seed(10)
banet <- barabasi.game(n = 100, p = 0.15)
banet


# degree dist plot
hist(degree(banet), main='Barabasi-Albert scale-free network - degree distribution',
     col='blue', xlab='Degree',
     xlim=c(0, max(degree(banet))))


# Watts-Strogatz small-world random network
set.seed(10)
wsnet <- sample_smallworld(dim = 1, size = 100, nei = 3, p = 0.1)
wsnet


# clustering
transitivity(wsnet, type='global')

# average path length
mean_distance(wsnet)




#### TASK 2
# A

Knet
set.seed(10) # run together with the function for reproduction
er_Knet <- erdos.renyi.game(n = vcount(Knet), p.or.m = ecount(Knet), type = 'gnm')

mean_distance(Knet)
mean_distance(er_Knet)

transitivity(Knet)
transitivity(er_Knet)

#### TASK 2 - END







### 7 - Dynamic networks (intro)

# import the dataset
film_df <- data.frame(read.csv("filmdata9806.csv", sep = ";", header = TRUE))
head(film_df)

# number of movies
length(unique(film_df$film_id))

# number of movie creators
length(unique(film_df$creator_id))

# years
table(film_df$year)



# filter data for 1998 only
data1998 <- subset(film_df, year==1998)

# table from the list
mat <- table(data1998$creator_id, data1998$film_id)

# create an n x n matrix = adjacency matrix
matrix <- mat %*% t(mat)

# set the diagonal to 0
diag(matrix) <- 0

# check the first 10x10 matrix (it is BIG!)
matrix[1:10, 1:10]


# create a graph object
graph1998 <- graph.adjacency(matrix, mode='undirected')

# check the graph object
graph1998


## graphs for every year
graphs <- list() # store graphs in a list
years <- c(1998:2006) # iterator

for(i in 1:length(years)){
  dt <- subset(film_df, year==years[i]) # filter data by time
  mat <- table(dt$creator_id, dt$film_id)
  mat <- mat %*% t(mat)
  diag(mat) <- 0
  graphs[[i]] <- graph.adjacency(mat, mode='undirected')
}

# add names to list elements
names(graphs) <- paste0("graph", c(1998:2006))


# check the number of components of the first network
components(graphs$graph2003)

# plot for visual check
plot(graphs$graph2003, vertex.size=2, vertex.label="")




## 3-year moving window
graphs <- list() # store graphs in a list
years <- c(2000:2006) # iterator

for(i in 1:length(years)){
  dt <- subset(film_df, year>=(years[i]-2) & year<=years[i])
  mat <- table(dt$creator_id, dt$film_id)
  mat <- mat %*% t(mat)
  diag(mat) <- 0
  graphs[[i]] <- graph.adjacency(mat, mode='undirected')
}

# add names to list elements
names(graphs) <- paste0("graph", c(2000:2006))


# loop to collect data on nodes and edges
nodes <- c()
edges <- c()

for(i in 1:length(years)){
  nodes[i] <- vcount(graphs[[i]])
  edges[i] <- ecount(graphs[[i]])
}

# data frame
networks_df <- data.frame(years, nodes, edges)

# set up barplots
par(mfrow=c(1,2)) # 2 barplots next to each other

barplot(networks_df$nodes, names.arg = years, main="Number of nodes 2000-2006")
barplot(networks_df$edges, names.arg = years, main="Number of nodes 2000-2006")


# components of graph2003
components(graphs$graph2003)

# visual representation
plot(graphs$graph2003, vertex.size=2, vertex.label="")



#### TASK 3
# 5 years
graphs <- list() # store graphs in a list
years <- c(2002:2006) # iterator

for(i in 1:length(years)){
  dt <- subset(film_df, year>=(years[i]-4) & year<=years[i])
  mat <- table(dt$creator_id, dt$film_id)
  mat <- mat %*% t(mat)
  diag(mat) <- 0
  graphs[[i]] <- graph.adjacency(mat, mode='undirected')
}

# loop to collect data on clustering and avg path length
clust <- c()
pathlength <- c()

for(i in 1:length(years)){
  clust[i] <- transitivity(graphs[[i]], type="global")
  pathlength[i] <- mean_distance(graphs[[i]])
}

smallworld_df <- data.frame(years, clust, pathlength)

# set up barplots
par(mfrow=c(1,2)) # 2 barplots next to each other

barplot(smallworld_df$clust, names.arg = years, main="Clustering 2000-2006")
barplot(smallworld_df$pathlength, names.arg = years, main="Avg path length 2000-2006")


#### TASK 3 - END
