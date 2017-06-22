## Semantics
null.assign <- function(n, lang){
  C <- L.constants(lang); c <- length(C); 
  S <- L.sentenceletters(lang); s <- length(S)
  V1 <- prim.L.variables(lang); v1 <- length(V1)
  if (n > 0) {V2 <- rep(0, n); for (i in 1:n){V2[i] <- x.variable(i)}} else V2 <- NULL
  String <- c(C,S,V1,V2)
  X <- rep(0,(c + s + v1 + n))
  X[1] <- TRUE
  X[2] <- FALSE
  X[3] <- 0
  X[4] <- 1
  Y <- c(rep("Constant",c), rep("Sentence letter",s), rep("Prim variable",v1), rep("Variable",n))
  D <- as.data.frame(cbind(String,Y,X))
  colnames(D) <- c("String", "Type", "Value")
  D
}
reassign <- function(assignment, strings, values){
  assignment <- as.matrix(assignment)
  n <- length(values); k <- length(strings)
  w <- rep(0,n)
  if (n == k) {for (i in 1:n){
    v <- as.character(strings[i]);
    w[i] <- which(assignment[,1] == v);
    assignment[w[i],3] <- as.numeric(values[i])}
  }
  if (n == k) as.data.frame(assignment) else return("String list and value list not matched")
}
val.prim <- function(string, assignment){
  v <- as.numeric(as.vector(subset(assignment, String == string)[,3]))
  if (length(v) == 0) return("Not assigned value") else return(v)
}
val <- function(string, assignment, lang){
  P <- parser(string, lang)
  wf <- is.wellformed.cs(P)
  main <- main.constructor.cs(P)
  if (is.wellformed(string, lang) == TRUE){
    if (is.L.variable(string, lang) == TRUE) return(val.prim(string,assignment))
    if (is.L.constant(string, lang) == TRUE) return(val.prim(string,assignment))
    if (is.L.sentenceletter(string, lang) == TRUE) return(val.prim(string,assignment))
    if (main == "S") return(val(delete.unary.prefix(string,"S"),assignment,lang) + 1)
    if (main == "+") return(val(split.infix(string,"+")[1],assignment,lang) + val(split.infix(string,"+")[2],assignment,lang))
    if (main == "*") return(val(split.infix(string,"*")[1],assignment,lang) * val(split.infix(string,"*")[2],assignment,lang))
    if (main == "!") return(1 - val(delete.unary.prefix(string,"!"),assignment,lang))
    if (main == "~") return(1 - val(delete.unary.prefix(string,"~"),assignment,lang))
    if (main == "&") return(min(c(val(split.infix(string,"&")[1],assignment,lang),val(split.infix(string, "&")[2],assignment,lang))))
    if (main == "v") return(max(c(val(split.infix(string,"v")[1],assignment,lang),val(split.infix(string,"v")[2],assignment,lang))))
    if (main == "->") return(max(c(1 - val(split.infix(string,"->")[1],assignment,lang),val(split.infix(string,"->")[2],assignment,lang))))
    if (main == "->") return(max(c(1 - val(split.infix(string,"->")[1],assignment,lang),val(split.infix(string,"->")[2],assignment,lang))))
    if (main == "=") return((val(split.infix(string,"=")[1],assignment,lang) == val(split.infix(string,"=")[2],assignment,lang)))
  }
  else return("Either not assigned value or illformed")
}
SEQ <- function(N){
  p <- 2^N; 
  q <- p-1;
  X <- matrix(0,p,N); 
  for (j in 1:q){
    for (i in 1:N){X[j+1,i]=as.numeric(intToBits(j)[i])
    }
  };
  X
}
truth.table <- function(string, lang){
  P <- syntax.test(string, lang)
  w <- which(P == "Atom")
  at <- as.character(P[w,2])
  at <- as.character(unique(at))
  w2 <- which(at == "True")
  w3 <- which(at == "False")
  n <- length(at)
  p <- 2^n
  tt <- cbind(SEQ(n),rep(0,p))
  colnames(tt) <- as.character(c(at[1:n],string))
  A.0 <- null.assign(0, lang)
  for (i in 1:p) {
    A <- reassign(A.0, at[1:n], tt[i,1:n]);
    tt[i,n+1] <- val(string, A, lang)
  }
  tt
}
SAT <- function(string, lang){
  tt <- truth.table(string, lang)
  n <- length(tt[1,])
  v <- tt[,n]
  if (any(v == 1)) return(TRUE) else return(FALSE)
}
SAT.1 <- function(string, lang){
  tt <- truth.table(string, lang)
  n <- length(tt[1,])
  v <- tt[,n]
  if (any(v == 1)) return("satisfiable") else return("unsatisfiable")
}
TAUT <- function(string, lang){
  tt <- truth.table(string, lang)
  n <- length(tt[1,])
  v <- tt[,n]
  if (!any(v == 0)) return(TRUE) else return(FALSE)
}
TAUT.1 <- function(string, lang){
  tt <- truth.table(string, lang)
  n <- length(tt[1,])
  v <- tt[,n]
  if (!any(v == 0)) return("a tautology") else return("not a tautology")
}
IMPLY <- function(string1,string2, lang){
  string <- IF(string1,string2)
  TAUT(string, lang)
}

EQUIV <- function(string1,string2, lang){
  stringa <- IF(string1,string2)
  stringb <- IF(string2,string1)
  TAUT(stringa, lang) & TAUT(stringb, lang)
}
MOD <- function(string, lang){
  tt <- truth.table(string, lang)
  n <- length(tt[1,])
  v <- tt[,n]
  w <- which(v == 1)
  if (length(w) != 0) {M <- tt[w,1:(n-1)];
                      k <- length(M[,1])
                      Model <- rep(0,k);
                      for (i in 1:k){Model[i] <- paste0("mod.",i)}
                      N <- cbind(Model,M)
                      Out <- as.data.frame(N)}
  if (length(w) == 0) Out <- "Unsatisfiable"
  Out
}
syntax.semantics.test <- function(txt, lang){
  Table <- matrix(0,3,3);
  WF <- is.wellformed(txt, lang);
  Sat <- SAT(txt, lang);
  Taut <- TAUT(txt, lang);
  Main <- main.constructor(txt, lang)
  Imm <- imm.pred(txt, lang);
  num.pred <- length(Imm)
  At <- atoms.in.expression(txt, lang);
  num.atoms <- length(At)
  p1 <- 6 + num.pred - 1;
  p2 <- p1 + 1;
  p3 <- p2 + num.atoms - 1;
  Output <- c(c(clean.text(txt)), WF, Sat, Taut, Main, Imm, At);
  n <- length(Output);
  Data <- rep(0,n)
  Data[1] <- "String";
  Data[2] <- "Is Wellformed?";
  Data[3] <- "Is Satisfiable?";
  Data[4] <- "Is a Tautology?";
  Data[5] <- "Main Constructor";
  for (i in 6:p1){Data[i] <- "Immediate Predecessor"}
  for (i in p2:p3){Data[i] <- "Atom"}
  if (WF == FALSE) Data <- Data[1:3];
  if (WF == FALSE) Output <- Output[1:3];
  M <- as.data.frame(cbind(Data,Output))
  colnames(M) <- c("Test","Output")
  M
}