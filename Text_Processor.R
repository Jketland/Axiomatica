## Text Processor
library(stringr)
library(tokenizers)
clean.text <- function(txt){gsub(" ", "", txt, fixed = TRUE)}
pad.text <- function(txt, substring){
  gsub(substring, paste0(" ", substring," "), txt)
}
normalize.text <- function(txt){
  txt <- clean.text(txt);
  l <- length(txt);
  if (l == 0) txt <- as.character(FALSE);
  if (is.na(txt)) txt <- as.character(FALSE);
  if (txt == "") txt <- as.character(FALSE);
  if (is.null(txt)) txt <- as.character(FALSE);
  txt
}
tok <- function(txt){
  txt <- normalize.text(txt);
  tokenize_characters(txt, lowercase = FALSE, strip_non_alphanum = FALSE)[[1]]
}
rep.symbol <- function(symbol,n){
  seq <- rep(0,n+1);
  seq[1] <- "";
  if (n != 0){for (i in 1:n){seq[i+1] <- paste0(seq[i],symbol)}};
  seq[n+1]
}
extract.first.symbol <- function(txt){
  txt <- normalize.text(txt);
  X <- tok(txt);
  n <- length(X);
  Out <- FALSE
  if (txt == FALSE) Out <- NULL;
  if (txt != FALSE) Out <- X[1];
  Out
}
extract.final.symbol <- function(txt){
  txt <- normalize.text(txt);
  X <- tok(txt);
  n <- length(X);
  Out <- FALSE
  if (txt == FALSE) Out <- NULL;
  if (txt != FALSE) Out <- X[n];
  Out
}
extract.firstandfinal.symbols <- function(txt){
  txt <- normalize.text(txt);
  X <- tok(txt);
  n <- length(X);
  Out <- FALSE
  if (txt == FALSE) Out <- NULL;
  if (txt != FALSE) Out <- c(X[1],X[n]);
  Out
}
has.open.par.prefix <- function(txt){
  txt <- extract.first.symbol(txt);
  if (is.null(txt)) FALSE else if (txt == "(") TRUE else FALSE;
}
has.close.par.suffix <- function(txt){
  txt <- extract.final.symbol(txt);
  if (is.null(txt)) FALSE else if (txt == ")") TRUE else FALSE;
}
has.unary.prefix <- function(txt,symbol){
  X <- tok(txt);
  n <- length(X); 
  if (n > 0) {f1 <- X[1]; f2 <- paste0(X[1],X[2])};
  if (f1 == symbol | f2 == symbol){TRUE} else FALSE
}
delete.unary.prefix <- function(txt,symbol){
  X <- tok(txt);
  n <- length(X); 
  if (n > 0) {f1 <- X[1]; f2 <- paste0(X[1],X[2])};
  d <- 1
  if (f1 == symbol) d <- 2
  if (f2 == symbol) d <- 3
  if (n > 0){paste(X[d:n],collapse = "")} else FALSE
}

enclose.with.parentheses <- function(txt){
  g <- paste("(",txt,")");
  gsub(" ", "", g, fixed=TRUE); 
}
is.parenthesized <- function(txt){
  P1 <- extract.firstandfinal.symbols(txt)[1]; P2 <- extract.firstandfinal.symbols(txt)[2];
  if (is.null(P1)) P1 <- "a"
  if (is.null(P2)) P2 <- "b"
  if (P1 == "(" & P2 == ")" | P1 == "[" & P2 == "]" | P1 == "{" & P2 == "}") return(TRUE) else return(FALSE)
}
deparenthesize <- function(txt){
  if (is.parenthesized(txt) == FALSE) FALSE;
  X <- tok(txt);
  n <- length(X); 
  if (n == 0)FALSE else txt.1 <- substr(txt,2,n-1);
  if (is.parenthesized(txt) == TRUE) txt.1 else FALSE}
paren.test <- function(txt){
  X <- tok(txt);
  w.open <- which(X == "(");
  w.close <- which(X == ")");
  if (length(w.open) == length(w.close))1 else 0
}
split.infix <- function(txt,split){
  k <- is.parenthesized(txt);
  X <- tok(txt);
  txt <- deparenthesize(clean.text(txt));
  n <- length(X);
  if (n == 1) Out <- "FALSE";
  vec <- strsplit(as.character(txt), split, fixed = TRUE)[[1]]
  m <- length(vec); T <- matrix(0,m-1,2)
  if ((m-1) != 0 & n != 0) for (i in 1:(m-1)){
    T[i,1] = paste(vec[1:i],collapse = split); T[i,2] = paste(vec[(i+1):m],collapse = split)
  }
  P <- matrix(0,m-1,2);
  if ((m-1) != 0 & n != 0) for (i in 1:(m-1)){
    P[i,1] <- paren.test(T[i,1]); P[i,2] <- paren.test(T[i,2])
  }
  i <- which(P[,1] == P[,2] & P[,1] == 1);
  if (length(i) == 0) Out <- FALSE else Out <- c(T[i,1],T[i,2]);
  if (k == FALSE) Out <- FALSE;
  Out
}
apply.prefix <- function(txt,prefix){clean.text(paste(prefix,txt))};
apply.suffix <- function(txt,suffix){clean.text(paste(txt,suffix))};
has.prefix <- function(txt,prefix){
  X <- tok(txt);
  n <- length(X); 
  if (n > 0) f <- X[1] else f <- 0;
  if (f == prefix){TRUE} else FALSE
}
delete.prefix <- function(txt,prefix){
  X <- tok(txt);
  n <- length(X); 
  if (has.prefix(txt,prefix) == TRUE) {substr(clean.text(txt),2,n)} else FALSE
}
has.suffix <- function(txt,prefix){
  X <- tok(txt);
  n <- length(X); 
  if (n > 0) f <- X[n] else f <- 0;
  if (f == prefix){TRUE} else FALSE
}
delete.suffix <- function(txt,suffix){
  X <- tok(txt);
  n <- length(X); 
  if (has.suffix(txt,suffix) == TRUE) {substr(clean.text(txt),n-1,n)} else FALSE
}
apply.infix <- function(txt1,txt2,infix){
  clean.text(enclose.with.parentheses(paste(txt1,C,txt2)))
}
prefix.numeral <- function(x,root,prefix){
  if(x==0)
    return(root)
  else
    return(apply.prefix(prefix.numeral(x-1,root,prefix),prefix))
}
suffix.numeral <- function(x,root,suffix){
  if(x==0)
    return(root)
  else
    return(apply.suffix(suffix.numeral(x-1,root,suffix),suffix))
}
esc <- function(txt){
  if (txt == "+"|txt == "*"|txt == "|"|txt == "[]") paste0("\\",txt) else txt
}
count.occurrences <- function(txt, symbol){
  txt <- normalize.text(txt); str_count(as.character(txt), esc(symbol))
}