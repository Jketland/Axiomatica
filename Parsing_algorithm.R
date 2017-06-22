## Parsing algorithm
unary.delete <- function(txt, symb, lang){
  if (symb %in% n.ary.L.constructors(1,lang)){
    if (symb != "A" & symb != "E") delete.unary.prefix(txt,symb) else delete.var.prefix(delete.unary.prefix(txt, symb),lang)}
}
unary.matrix <- function(txt, symb, lang){
  rbind(unary.delete(txt, symb, lang),0,txt)
}
binary.matrix <- function(txt, symb, lang){
  rbind(c(split.infix(txt, symb)[1],split.infix(txt, symb)[2]),c(0,0),c(pad.infix(txt,lang), pad.infix(txt,lang)))
}

sweep <- function(D,X1,X2,lang){
  N1 <- length(X1)
  N2 <- length(X2)
  w <- which(D[2,] == 0); 
  Component <- function(i){D[1,w[i]]};
  len <- length(w);
  C <- rep(0,len);
  for (i in 1:len){C[i] <- Component(i)}
  for (i in 1:len){
    t <- C[i];
    for (j in 1:N1){
      constr <- X1[j];
      if (has.unary.prefix(t,constr) == TRUE) {
        D <- cbind(D,rbind(unary.matrix(t,constr, lang),w[i],constr))}};
    for (j in 1:N2){
      constr <- X2[j];
      if (test.binary.symb(t,constr) == TRUE) {
        D <- cbind(D,rbind(binary.matrix(t,constr, lang),w[i],constr))}};
    D[2,w[i]] <- 1;
  }
  D
}
initialize.parsing.table <- function(txt, lang){
  txt <- clean.text(txt);
  String <- c(txt);
  Status <- c(0);
  Parent <- c(0);
  Parent.id <- c(0);
  Constr <- c(0);
  Table <- rbind(String, Status, Parent, Parent.id, Constr)
}
build.parsing.data.frame <- function(table, lang){
  X <- pad.infix(as.vector(table[1,]),lang);
  N <- length(X);
  ID <- 1:N;
  Status <- as.vector(table[2,]);
  Y <- as.vector(table[3,]);
  W <- as.vector(table[5,]);
  Z <- rep(0,N)
  if (N > 1){Z <- as.numeric(table[4,])};
  if (N == 1){Z <- c(1)};
  Y[1] <- "Root";
  Z[1] <- "NULL";
  W[1] <- "NULL";
  AT <- rep(0,N);
  for (i in 1:N){if (is.L.atom(X[i], lang)) AT[i] <- TRUE};
  M <- as.data.frame(cbind(X, ID, AT, Y, Z, W));
  colnames(M) <- c("String", "ID", "Atom", "Parent", "Parent ID","Parent Constructor");
  M
}
parser <- function(txt, lang){
  Table <- initialize.parsing.table(txt, lang)
  Comp <- complexity(txt, lang);
  X1 <- n.ary.L.constructors(1,lang)
  X2 <- n.ary.L.constructors(2,lang)[-c(5,6,10,11,12,13)]
  for (j in 1:(Comp+1)){Table <- sweep(Table,X1,X2,lang)};
  build.parsing.data.frame(Table, lang)
}