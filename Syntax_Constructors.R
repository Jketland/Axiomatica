## Syntax Processor I: Constructors
## 1. General
## 2. Constructors
## 3. Processing Compound Formulas
## 4. Complexity

## 1. General: Classification of strings
arity <- function(txt, lang){
  w <- which(lang == txt)
  if (length(w) != 0)return(as.numeric(as.vector(lang[w,5])))
  if (length(w) == 0)return("Not a symbol in given language")
}
syntax <- function(txt, lang){
  w <- which(lang == txt)
  if (length(w) != 0)return(as.character(as.vector(lang[w,4])))
  if (length(w) == 0)return("Not a symbol in given language")  
}
type <- function(txt, lang){
  w <- which(lang == txt)
  if (length(w) != 0)return(as.character(as.vector(lang[w,3])))
  if (length(w) == 0)return("Not a symbol in given language")  
}
## 2. Constructors
L.constructors.arity.type <- function(ar, ty, lang) {
  as.character(subset(lang, Arity == ar & Type == ty)[, 1])
}
n.ary.L.constructors <- function(ar, lang) {
  as.character(subset(lang, Arity == ar & Syntax != "Prefix pred")[, 1])
}
L.constructors <- function(lang){
  c(n.ary.L.constructors(1,lang), n.ary.L.constructors(2,lang)) 
}
is.L.constructor <- function(txt,lang){txt %in% L.constructors(lang)}
apply.L.constructor <- function(X, constr, var, lang){
  n <- length(X)
  t <- type(constr, lang)
  s <- syntax(constr, lang)
  a <- arity(constr, lang)
  # 1 for unary prefix; 2 for binary infix; 3 is unary commafiy; 4 is n-ary commafy 
  if (n == 1 & a == 1 & s == "Prefix") Out <- clean.text(paste0(constr,X))
  if (n == 2 & a == 2 & s == "Infix") Out <- clean.text(enclose.with.parentheses(paste(X[1],constr,X[2])))
  if (n == 1 & a == 1 & s == "Prefix quant") Out <- clean.text(paste0(constr,var,X))
  Out
}
pad.infix <- function(txt,lang){
  txt <- clean.text(txt)
  X <- n.ary.L.constructors(2,lang)
  X <- X[X != "<-" & X != "<->" & X != "<" & X != "!=" & X != "|-" & X != "|="]
  n <- length(X)
  for (i in 1:n){txt <- pad.text(txt,esc(X[i]))}
  txt
}
NEG <- function(txt){apply.L.constructor(txt,"!","x",L.0)}
NEG.1 <- function(txt,lang){apply.L.constructor(txt,"~","x",L.0)}
SUCC <- function(txt){apply.L.constructor(txt,"S","x", L.0)}
AND <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"&", "x",L.0)}
OR <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"v", "x",L.0)}
IF <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"->", "x",L.0)}
PLUS <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"+", "x",L.0)}
PROD <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"*", "x",L.0)}
EQ <- function(txt1,txt2){apply.L.constructor(c(txt1,txt2),"=", "x",L.0)}
UNQ <- function(txt,var){apply.L.constructor(txt,"A", var, L.0)}
EXQ <- function(txt,var){apply.L.constructor(txt,"E", var, L.0)}

## 3. Processing Compound Formulas
has.neg.prefix <- function(txt){
  t <- extract.first.symbol(normalize.text(txt))
  if (is.null(t)) t <- "F"
  if (t == "!" | t == "~") Out <- TRUE else Out <- FALSE
  Out
}
has.E.prefix <- function(txt,lang){
  X <- tok(txt);
  n <- length(X); 
  if (n > 1) f <- c(X[1],X[2]) else f <- 0;
  if (f[1] == "E" & is.L.variable(X[2],lang)){TRUE} else FALSE
}
delete.E.prefix <- function(txt){
  X <- tok(txt);
  n <- length(X); 
  if (has.E.prefix(txt) == TRUE) {substr(clean.text(txt),2,n)} else FALSE
}
has.A.prefix <- function(txt, lang){
  X <- tok(txt);
  n <- length(X); 
  if (n > 1) f <- c(X[1],X[2]) else f <- 0;
  if (f[1] == "A" & is.L.variable(X[2],lang)){TRUE} else FALSE
}
delete.A.prefix <- function(txt){
  X <- tok(txt);
  n <- length(X); 
  if (has.A.prefix(txt) == TRUE) {substr(clean.text(txt),2,n)} else FALSE
}

## 4. Complexity
complexity.ar <- function(txt,ar,lang){
  X <- n.ary.L.constructors(ar,lang)
  if (ar == 2){X <- X[X != "<-" & X != "<->" & X != "<" & X != "!=" & X != "|-" & X != "|="]}
  n <- length(X);
  comp <- rep(0,n);
  for (i in 1:n){constr <- X[i];
  comp[i] <- str_count(txt,esc(constr))}
  sum(comp)
}
complexity <- function(txt,lang){complexity.ar(txt,1,lang) + complexity.ar(txt,2,lang)}
complexity.ar("<>p",1,L.0)
test.binary.symb <- function(txt,symb){
  count.occurrences(txt,symb)>0 & is.parenthesized(txt) & split.infix(txt,symb)[1] != FALSE}