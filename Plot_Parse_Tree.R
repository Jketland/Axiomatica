## Plot Parsing Tree
library(igraph)
plot.parse.tree <- function(string, lang){
  P <- parser(string, lang);
  t <- is.wellformed.cs(P);
  text <- as.character(P$'String');
  N <- length(text);
  parent <- as.vector(P$`Parent ID`);
  Nodes <- 1:N
  to <- Nodes[2:N]
  from <- as.numeric(parent[2:N])
  Rel <- as.data.frame(cbind(from,to))
  if (t == TRUE) {g <- graph_from_data_frame(Rel, directed=TRUE, vertices=Nodes)
  labels <- as.character(P$'Parent Constructor')[2:N]
  V(g)$label <- text
  E(g)$label <- labels
   plot(g, layout=layout.reingold.tilford,vertex.shape = "circle", 
       vertex.size=2, vertex.color = "blue", 
       vertex.label.degree=0, vertex.label.dist=1.2,
       edge.arrow.size=0.1, edge.color = "light blue",
       main = paste("Parsing Tree:",pad.infix(string,lang)))}
  if (t == FALSE) plot(0,type='n',axes=FALSE,xlab="", ylab="",main="Is not wellformed")
}
plot.parse.tree.cs <- function(P, lang){
  text <- as.character(P$'String');
  string <- text[1];
  t <- is.wellformed.cs(P);
  N <- length(text);
  parent <- as.vector(P$`Parent ID`);
  Nodes <- 1:N
  to <- Nodes[2:N]
  from <- as.numeric(parent[2:N])
  Rel <- as.data.frame(cbind(from,to))
  if (t == TRUE) {g <- graph_from_data_frame(Rel, directed=TRUE, vertices=Nodes)
  labels <- as.character(P$'Parent Constructor')[2:N]
  V(g)$label <- text
  E(g)$label <- labels
  plot(g, layout=layout.reingold.tilford,vertex.shape = "circle", 
       vertex.size=2, vertex.color = "blue", 
       vertex.label.degree=0, vertex.label.dist=1.2,
       edge.arrow.size=0.1, edge.color = "light blue",
       main = paste("Parsing tree: ", pad.infix(string,lang)))}
  if (t == FALSE) plot(0,type='n',axes=FALSE,xlab="", ylab="",main="Is not wellformed")
}

plot.parse.tree.diff <- function(string, lang){
  P <- parser(string, lang);
  t <- is.wellformed.cs(P);
  text <- as.character(P$'String');
  N <- length(text);
  parent <- as.vector(P$`Parent ID`);
  Nodes <- 1:N
  to <- Nodes[2:N]
  from <- as.numeric(parent[2:N])
  Rel <- as.data.frame(cbind(from,to))
  if (t == TRUE) {g <- graph_from_data_frame(Rel, directed=TRUE, vertices=Nodes)
  labels <- as.character(P$'Parent Constructor')[2:N]
  V(g)$label <- text
  E(g)$label <- labels
  plot(g, layout=layout.reingold.tilford,
       vertex.label.degree=0, vertex.shape = "circle",
       vertex.color = "yellow", vertex.label.dist=1.3,
       vertex.size=2,
       edge.arrow.size=0.1, edge.color = "light blue",
       main = paste("Parsing Tree:",pad.infix(string,lang)))}
  if (t == FALSE) plot(0,type='n',axes=FALSE,xlab="", ylab="",main="Is not wellformed")
}

