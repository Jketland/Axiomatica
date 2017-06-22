## Syntax Processor IV
is.wellformed <- function(txt, lang){
  P <- parser(txt, lang)
  X <- as.numeric(as.vector(P[,3]));
  Y <- as.vector(P[,5]);
  w <- which(X == 0)
  n <- length(w)
  Reduced <- rep(0, n)
  if (n > 0) {for (i in 1:n){
    t <- w[i]
    if (any(Y == t)) Reduced[i] <- 1
  }
  }
  if (any(Reduced == 0)) Out <- FALSE else Out <- TRUE
  if (txt == "") Out <- FALSE
  Out
} 
is.wellformed.cs <- function(P){
  P <- as.matrix(P)
  X <- as.numeric(as.vector(P[,3]));
  Y <- as.vector(P[,5]);
  w <- which(X == 0)
  n <- length(w)
  Reduced <- rep(0, n)
  if (n > 0) {for (i in 1:n){
    t <- w[i]
    if (any(Y == t)) Reduced[i] <- 1
  }
  }
  if (any(Reduced == 0)) Out <- FALSE else Out <- TRUE
  Out
} 
atoms.in.expression <- function(txt, lang){
  P <- parser(txt, lang)
  I <- P[,3];
  w <- which(I == 1);
  X <- as.character(P[,1]);
  n <- length(w);
  a <- rep(0,n);
  if (n != 0) {for (i in 1:n) {a[i] <- as.character(X[w[i]])}}
  if (n != 0) Out <- a else Out <- NULL;
  rev(Out)
}
atoms.in.expression.cs <- function(C){
  I <- C[,3];
  w <- which(I == 1);
  X <- as.character(C[,1]);
  n <- length(w);
  a <- rep(0,n);
  if (n != 0) {for (i in 1:n) {a[i] <- as.character(X[w[i]])}}
  if (n != 0) Out <- a else Out <- NULL;
  rev(Out)
}
main.constructor <- function(txt, lang){
  if (!is.na(as.character(parser(txt, lang)[2,6]))) as.character(parser(txt, lang)[2,6]) else "NULL"
}
main.constructor.cs <- function(C){
  if (!is.na(as.character(C[2,6]))) as.character(C[2,6]) else "NULL"
}
imm.pred <- function(txt, lang){
  X1 <- n.ary.L.constructors(1,lang)
  X2 <- n.ary.L.constructors(2,lang)
  X2 <- X2[X2 != "<-" & X2 != "<->" & X2 != "<" & X2 != "!=" & X2 != "|-" & X2 != "|="]
  N1 <- length(X1)
  N2 <- length(X2)
  Out <- "NULL"
  for (i in 1:N1){constr <- X1[i]
  if (has.unary.prefix(txt,constr) == TRUE) Out <- pad.infix(unary.delete(txt,constr, lang),lang)}
  for (i in 1:N2){constr <- X2[i]
  if (test.binary.symb(txt,constr) == TRUE) Out <- pad.infix(split.infix(txt,constr), lang)}
  Out
}
syntax.test <- function(txt, lang){
  Table <- matrix(0,3,3);
  WF <- is.wellformed(txt, lang);
  Main <- main.constructor(txt, lang)
  Imm <- pad.infix(imm.pred(txt, lang), lang);
  num.pred <- length(Imm)
  At <- atoms.in.expression(txt, lang);
  num.atoms <- length(At)
  p1 <- 4 + num.pred - 1;
  p2 <- p1 + 1;
  p3 <- p2 + num.atoms - 1;
  Output <- c(c(pad.infix(clean.text(txt),lang)), WF, Main, Imm, At);
  n <- length(Output);
  Data <- rep(0,n)
  Data[1] <- "String";
  Data[2] <- "Is Wellformed?";
  Data[3] <- "Main Constructor";
  for (i in 4:p1){Data[i] <- "Immediate Predecessor"}
  for (i in p2:p3){Data[i] <- "Atom"}
  if (WF == FALSE) Data <- Data[1:3];
  if (WF == FALSE) Output <- Output[1:3];
  M <- as.data.frame(cbind(Data,Output))
  colnames(M) <- c("Test","Output")
  M
}

is.conj <- function(string, lang){ main.constructor(string, lang) == "&"  }
is.disj <- function(string, lang){ main.constructor(string, lang) == "v"  }
is.cond <- function(string, lang){ main.constructor(string, lang) == "->"  }
is.neg.conj <- function(string, lang){ 
  f <- extract.first.symbol(string);
  f == "~" & main.constructor(delete.unary.prefix(string, "~"), lang) == "&"  }
is.neg.disj <- function(string, lang){ 
  f <- extract.first.symbol(string);
  f == "~" & main.constructor(delete.unary.prefix(string, "~"), lang) == "v"  }
is.neg.cond <- function(string, lang){ 
  f <- extract.first.symbol(string);
  f == "~" & main.constructor(delete.unary.prefix(string, "~"), lang) == "->"  }
