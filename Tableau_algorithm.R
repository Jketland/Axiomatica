## Tableau algorithm
is.DN.formula <- function(txt){  has.unary.prefix(delete.unary.prefix(txt,"~"), "~") | has.unary.prefix(delete.unary.prefix(txt,"!"), "!")}
has.unary.prefix.tab <- function(txt, constr, lang){
  (constr == "~~" & is.DN.formula(txt)) | (constr == "A" & has.A.prefix(txt, lang)) | (constr == "E" & has.E.prefix(txt, lang))}
unary.delete.tab <- function(txt, symb, lang){
  if (symb %in% c("~~", "!!", "A", "E")){
    if (symb != "A" & symb != "E") delete.unary.prefix(delete.unary.prefix(txt,symb),symb) else delete.var.prefix(delete.unary.prefix(txt, symb),lang)}
}
unary.tab.matrix <- function(txt, constr, lang){
  rbind(unary.delete.tab(txt, constr, lang),0,0,txt)
}
binary.matrix.tab <- function(txt, constr, lang){
  if(constr == "&") P <- rbind(c(split.infix(txt, constr)[1],split.infix(txt, constr)[2]),c(0,0),c(0,0),c(pad.infix(txt,lang), pad.infix(txt,lang)))
  if(constr == "v") P <- rbind(c(split.infix(txt, constr)[1],split.infix(txt, constr)[2]),c(0,0),c("L","R"), c(pad.infix(txt,lang), pad.infix(txt,lang)))
  if(constr == "->") P <- rbind(c(NEG.1(split.infix(txt, constr)[1]),split.infix(txt, constr)[2]),c(0,0),c("L","R"), c(pad.infix(txt,lang), pad.infix(txt,lang)))
  P
}
neg.binary.matrix.tab <- function(txt, constr, lang){
  f <- extract.first.symbol(txt)
  t <- delete.unary.prefix(txt, f)
  P <- 0
  if((f == "~" | f == "!") & constr == "&") P <- rbind(c(apply.L.constructor(split.infix(t, constr)[1],f,"x",lang), 
                                                         apply.L.constructor(split.infix(t, constr)[2],f,"x",lang)),
                                                       c(0,0),
                                                       c("L","R"),
                                                       c(pad.infix(txt,lang), 
                                                         pad.infix(txt,lang)))
  if((f == "~" | f == "!")  & constr == "v") P <- rbind(c(apply.L.constructor(split.infix(t, constr)[1],f,"x",lang), 
                                                          apply.L.constructor(split.infix(t, constr)[2],f,"x",lang)),
                                                        c(0,0),
                                                        c(0,0),
                                                        c(pad.infix(txt,lang), 
                                                          pad.infix(txt,lang)))
  if((f == "~" | f == "!")  & constr == "->") P <- rbind(c(split.infix(t, constr)[1], 
                                                           apply.L.constructor(split.infix(t, constr)[2],f,"x",lang)),
                                                         c(0,0),
                                                         c(0,0),
                                                         c(pad.infix(txt,lang), pad.infix(txt,lang)))
  P
}
test.binary.symb.tab <- function(txt, symb){
  t1 <- delete.unary.prefix(txt,"~")
  t2 <- delete.unary.prefix(txt,"!")
  test1 <- test.binary.symb(txt, symb)
  test2 <- test.binary.symb(t1,symb)
  test3 <- test.binary.symb(t2,symb)
  Out <- "Not compound"
  if (test1 == TRUE) Out <- "Compound"
  if (test1 != TRUE & (test2 == TRUE |test3 == TRUE)) Out <- "Neg compound"
  Out
}

tab.children <- function(string, lang){
  f <- extract.first.symbol(string)
  t <- delete.unary.prefix(string, f)
  C.Out <- "NULL"
  if (is.DN.formula(string)==TRUE){
    C.Out <- delete.unary.prefix(delete.unary.prefix(string,"~"),"~")}
  if(is.conj(string,lang)) C.Out <- c(split.infix(string, "&")[1],split.infix(string, "&")[2])
  if(is.disj(string,lang)) C.Out <- c(split.infix(string, "v")[1],split.infix(string, "v")[2])
  if(is.cond(string,lang)) C.Out <- c(NEG.1(split.infix(string, "->")[1]),split.infix(string, "->")[2])
  if(is.neg.conj(string,lang)) C.Out <- c(apply.L.constructor(split.infix(t, "&")[1],f,"x",lang), 
                                          apply.L.constructor(split.infix(t, "&")[2],f,"x",lang))
  if(is.neg.disj(string,lang)) C.Out <- c(apply.L.constructor(split.infix(t, "v")[1],f,"x",lang), 
                                          apply.L.constructor(split.infix(t, "v")[2],f,"x",lang))
  if(is.neg.cond(string,lang)) C.Out <- c(split.infix(t, "->")[1], 
                                          apply.L.constructor(split.infix(t, "->")[2],f,"x",lang))
  C.Out
}
tab.classifier.p <- function(string, lang){
  Class <- "NULL"
  if (is.DN.formula(string)==TRUE){Class <- "DN"}
  if(is.conj(string,lang) == TRUE | is.neg.disj(string,lang) == TRUE | is.neg.cond(string,lang) == TRUE) Class <- "Alpha"
  if (is.disj(string,lang) == TRUE | is.cond(string,lang) == TRUE | is.neg.conj(string,lang) == TRUE) Class <- "Beta"
  Class
}
tab.classifier <- function(string){
  Class <- "NULL"
  f <- extract.first.symbol(string)
  t <- delete.unary.prefix(string,"~")
  if(is.DN.formula(string)==TRUE){Class <- "DN"}
  if (test.binary.symb(string,"&") | (f == "~") & (test.binary.symb(t,"v") | test.binary.symb(t,"->")) )  Class <- "Alpha"
  if (test.binary.symb(string,"v") | test.binary.symb(string,"->") | (f == "~") & test.binary.symb(t,"&")) Class <- "Beta"
  Class
}

tableau.sweep <- function(D,X1,X2,lang){
  N1 <- length(X1)
  N2 <- length(X2)
  w <- which(D[2,] == 0); 
  Component <- function(i){D[1,w[i]]};
  len <- length(w);
  if (len > 0) {C <- rep(0,len);
  for (i in 1:len){C[i] <- Component(i)};
  for (i in 1:len){
    t <- C[i];
    D[2,w[i]] <- 1;
    for (j in 1:N1){
      constr <- X1[j];
      if (has.unary.prefix.tab(t,constr, lang) == TRUE) {P <- rbind(unary.tab.matrix(t, constr, lang),w[i],constr)
      D <- cbind(D,P)}
      if (has.unary.prefix.tab(t,constr, lang) == FALSE) D <- D};
    for (j in 1:N2){
      constr <- X2[j];
      test <- test.binary.symb.tab(t, constr)
      P1 <- binary.matrix.tab(t, constr, lang)
      P2 <- neg.binary.matrix.tab(t, constr, lang)
      if(test == "Compound") D <- cbind(D,rbind(P1,w[i],constr));
      if(test == "Neg compound") D <- cbind(D,rbind(P2,w[i],NEG.1(constr)));
      if(test == "Not compound") D <- D}
  }
  }
  D
}
initialize.tableau.table <- function(txt, lang){
  txt <- clean.text(txt);
  String <- c(txt);
  Status <- c(0);
  Branch <- c(0)
  Parent <- c(0);
  Parent.id <- c(0);
  Constr <- c(0);
  Table <- rbind(String, Status, Branch, Parent, Parent.id, Constr)
}
build.tableau.data.frame <- function(table, lang){
  X <- pad.infix(as.vector(table[1,]),lang);
  N <- length(X);
  ID <- 1:N;
  Status <- as.vector(table[2,]);
  Branch <- as.vector(table[3,]);
  Y <- as.vector(table[4,]);
  W <- as.vector(table[6,]);
  Z <- rep(0,N)
  if (N > 1){Z <- as.numeric(table[5,])};
  if (N == 1){Z <- c(1)};
  Y[1] <- "Root";
  Z[1] <- "NULL";
  W[1] <- "NULL";
  AT <- rep(0,N);
  for (i in 1:N){if (is.L.literal(X[i], lang)) AT[i] <- TRUE};
  M <- as.data.frame(cbind(X, ID, AT, Branch, Y, Z, W));
  colnames(M) <- c("String", "ID", "Literal", "Branch", "Parent", "Parent ID","Parent Constructor");
  M
}
tableau <- function(txt, lang){
  Table <- initialize.tableau.table(txt, lang)
  Comp <- complexity(txt, lang);
  X1 <- c("~~", "!!", "A", "E")
  X2 <- c("&", "v", "->")
  for (j in 1:(Comp+1)){Table <- tableau.sweep(Table,X1,X2,lang)};
  build.tableau.data.frame(Table, lang)
}
tableau.p <- function(txt, lang){
  D <- tableau(txt, lang)
  D[-c(5)]
}
