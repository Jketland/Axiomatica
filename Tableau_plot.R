## Tableau plot

initialize.graph <- function(string){
  g <- make_empty_graph(n=0,directed = TRUE)
  g <- add.vertices(g,1)
  V(g)$label <- pad.infix(string,L.0)
  g
}
leaves <- function(g, i){
  leaves <- V(g)[degree(g, mode="out")==0]
  test <- (i %in% leaves)
  B <- all_simple_paths(g, from = i, to = leaves)
  n <- length(B)
  Out <- 0
  if (n == 0 & test == FALSE) Out <- 0
  if (n == 0 & test == TRUE) Out <- i
  if (n != 0){Out <- rep(0,n); for (j in 1:n){X <- B[[j]]; Out[j] <- X[length(X)]}}
  Out
}
add.node <- function(g, node, lang){
  labels <- V(g)$label
  label <- labels[node]
  string <- tab.children(label, lang)
  new.label <- c(pad.infix(string,lang))
  L <- leaves(g, node)
  n <- length(L)
  new.labels <- rep(new.label,n)
  g <- add.vertices(g,n)
  id <- V(g)
  k <- length(id)
  new.vertices <- id[(k+1-n):k]
  for (j in 1:n) { g <- add.edges(g,c(L[j],new.vertices[j]))}
  V(g)$label <- c(labels, new.labels)
  g
}  
add.nonbranch.nodes <- function(g, node, lang){
  labels <- V(g)$label
  label <- labels[node]
  C <- tab.children(label,lang)
  string1 <- C[1]
  string2 <- C[2]
  new.labels <- c(pad.infix(string1,lang),pad.infix(string2,lang))
  L <- leaves(g,node)
  n <- length(L)
  g <- add.vertices(g,2*n)
  id <- as.numeric(V(g))
  k <- length(id)
  new.vertices <- id[(k+1-(2*n)):k]
  new.labels <- rep(new.labels,n)
  for (i in 1:n) { g <- add.edges(g,c(L[i],new.vertices[(2*i)-1]))
  g <- add.edges(g,c(new.vertices[(2*i)-1],new.vertices[(2*i)]))}
  V(g)$label <- c(labels, new.labels)
  g
}
add.branch.nodes <- function(g, node, lang){
  labels <- V(g)$label
  label <- labels[node]
  C <- tab.children(label,lang)
  string1 <- C[1]
  string2 <- C[2]
  new.labels <- c(pad.infix(string1,lang),pad.infix(string2,lang))
  L <- leaves(g,node)
  n <- length(L)
  g <- add.vertices(g,2*n)
  id <- as.numeric(V(g))
  k <- length(id)
  new.vertices <- id[(k+1-(2*n)):k]
  new.labels <- rep(new.labels,n)
  for (i in 1:n) { g <- add.edges(g,c(L[i],new.vertices[(2*i)-1]))
  g <- add.edges(g,c(L[i],new.vertices[(2*i)]))}
  V(g)$label <- c(labels, new.labels)
  g
}
node.class <- function(g, node){
  labels <- V(g)$label
  label <- labels[node]
  tab.classifier(label)
}
process.node <- function(g, node, lang){
  if (node.class(g, node)=="DN"){g <- add.node(g,node,lang)}
  if (node.class(g, node)=="Alpha"){g <- add.nonbranch.nodes(g,node,lang)}
  if (node.class(g, node)=="Beta"){g <- add.branch.nodes(g,node,lang)}
  g  
}
process.graph <- function(g, status, lang){
  id <- as.numeric(V(g))
  n <- length(id)
  w <- which(status == 0)
  S <- id[w]
  k <- length(S)
  for (i in 1:k){g <- process.node(g, S[i], lang)}
  g
}
compute.new.status <- function(g,status){
  n <- length(status)
  status <- rep(1,n)
  id <- as.numeric(V(g))
  k <- length(id)
  new <- rep(0,k-n)
  c(status,new)
}
tableau.graph <- function(string,lang){
  g <- initialize.graph(string)
  status <- rep(0,1)
  fin <- !any(status == 0)
  while(fin == 0){
    g <- process.graph(g,status,lang)
    status <- compute.new.status(g,status);
    fin <- !any(status == 0)}
  g
}
plot.tableau <- function(string, lang){
  g <- tableau.graph(string, lang)
  plot(g, layout=layout.reingold.tilford, vertex.label.degree=0, vertex.shape = "circle",
       vertex.color = "yellow", vertex.label.dist=1.3, vertex.size=2, 
       edge.arrow.size=0.1, edge.color = "light blue",
       main = paste("Semantic Tableau:",pad.infix(string,lang)))
}

# string <- "(p -> (q -> p))"
# g <- MAKE.TAB(string,L.0)
# plot.tableau(string,L.0)

## These older functions use the full syntax parser to compute node classification

node.class.p <- function(g, node, lang){
  labels <- V(g)$label
  label <- labels[node]
  tab.classifier(label, lang)
}
process.node.p <- function(g, node, lang){
  if (node.class(g, node, lang)=="DN"){g <- add.node(g,node,lang)}
  if (node.class(g, node, lang)=="Alpha"){g <- add.nonbranch.nodes(g,node,lang)}
  if (node.class(g, node, lang)=="Beta"){g <- add.branch.nodes(g,node,lang)}
  g  
}
process.graph.p <- function(g, status, lang){
  id <- as.numeric(V(g))
  n <- length(id)
  w <- which(status == 0)
  S <- id[w]
  k <- length(S)
  for (i in 1:k){g <- process.node(g, S[i], lang)}
  g
}
MAKE.TAB.p <- function(string,lang){
  g <- initialize.graph(string)
  status <- rep(0,1)
  fin <- !any(status == 0)
  while(fin == 0){
    g <- process.graph(g,status,lang)
    status <- compute.new.status(g,status);
    fin <- !any(status == 0)}
  g
}
plot.tableau.p <- function(string, lang){
  g <- MAKE.TAB.p(string, lang)
  plot(g, layout=layout.reingold.tilford, vertex.label.degree=0, vertex.shape = "circle",
       vertex.color = "yellow", vertex.label.dist=1.3, vertex.size=2, edge.arrow.size=0.1, edge.color = "light blue")
}



