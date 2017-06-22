## Syntax: Terms and Formulas

## 1. Atomic terms: constants, variables 
## 2. Compound terms
## 3. Atomic formulas and literals

## 1. Atomic terms: constants and variables 
prim.atom.L.term <- function(lang){
  as.character(subset(lang, Arity == 0 & Type == "Atom.term")[, 1])
}
is.prim.atom.L.term <- function(txt, lang){txt %in% prim.atom.L.term(lang)}
L.constants <- function(lang){
  as.character(subset(lang, Arity == 0 & Syntax == "Constant")[, 1])
}
is.L.constant <- function(txt,lang){txt %in% L.constants(lang)}
L.sentence.constants <- function(lang){
  as.character(subset(lang, Arity == 0 & Syntax == "Sentence constant")[, 1])
}
is.L.sentence.constant <- function(txt,lang){txt %in% L.sentence.constants(lang)}
prim.L.variables <- function(lang){
  as.character(subset(lang, Arity == 0 & (Type == "Atom.term"|Type == "Atom.form") & (Syntax == "Variable"|Syntax == "Sentence variable"))[, 1])
}
is.prim.L.variable <- function(txt,lang){txt  %in% prim.L.variables(lang)}
x.variable <- function(x){
  if(x==0)
    return("x")
  else
    return(clean.text(paste(x.variable(x-1),"^")))
}
is.L.variable <- function(txt,lang){
  txt <- normalize.text(txt);
  X <- tok(txt);
  n <- length(X)+2
  test <- rep(0,n)
  for (i in 1:n){test[i] <- x.variable(i-1)}
  any(test == txt)|txt %in% prim.L.variables(lang)
}
is.atomic.L.term <- function(txt, lang){
  is.prim.atom.L.term(txt, lang) | is.L.variable(txt, lang)
}

## 2. Compound Terms
# succ.numeral <- function(x){
#  if(x==0)
#    return("0")
#  else
#    return(SUCC(numeral(x-1)))
# }
# is.succ.numeral <- function(txt){
#  txt <- normalize.text(txt);
#  X <- tok(txt);
#  n <- length(X)+2
#  test <- rep(0,n)
#  for (i in 1:n){test[i] <- succ.numeral(i)}
#  any(test == txt)
# }
delete.var.prefix <- function(txt, lang){
  X <- tok(txt);
  n <- length(X);
  U <- rep(0,n); 
  V <- rep(0,n);
  for (i in 1:n){U[i] <- substr(clean.text(txt),1,i)};
  for (i in 1:n){V[i] <- is.L.variable(U[i],lang)};
  w1 <- which(V == TRUE);
  w2 <- which(V == FALSE);
  a <- min(w2)-1
  if (length(w1) == 0) Out <- FALSE;
  if (length(w1) != 0) Out <- substr(clean.text(txt),min(w2),n);
  Out
}
is.L.term <- function(txt,lang){
  Out <- FALSE
  if (is.atomic.L.term(txt,lang) == TRUE) Out <- TRUE
  else
    if (has.unary.prefix(txt,"S")==TRUE) Out <- (is.L.term(delete.unary.prefix(txt,"S"),lang))
    if (test.binary.symb(txt,"+") == TRUE) Out <- (is.L.term(split.infix(txt,"+")[1],lang) & is.L.term(split.infix(txt,"+")[2],lang))
    if (test.binary.symb(txt,"*") == TRUE) Out <- (is.L.term(split.infix(txt,"*")[1],lang) & is.L.term(split.infix(txt,"*")[2],lang))
    Out
}

## 3. Atomic formulas and literals
L.sentenceletters <- function(lang){
  as.character(subset(lang, Arity == 0 & Type == "Atom.form" & Syntax == "Sentence letter")[, 1])
}
is.L.sentenceletter <- function(txt,lang){txt  %in% L.sentenceletters(lang)}
L.sentencevariables <- function(lang){
  as.character(subset(lang, Arity == 0 & Type == "Atom.form" & Syntax == "Sentence variable")[, 1])
}
is.L.sentencevariable <- function(txt,lang){txt  %in% L.sentencevariables(lang)}
L.prefix.predicates.ar <- function(ar, lang){
  as.character(subset(lang, Syntax == "Prefix pred" & Arity == ar)[,1])}
unary.L.prefix.predicates <- function(lang){L.prefix.predicates.ar(1,lang)}
binary.L.prefix.predicates <- function(lang){L.prefix.predicates.ar(2,lang)}
is.L.eq.formula <- function(txt,lang){
  S <- split.infix(txt,"=");
  test <- (is.L.term(S[1],lang) & is.L.term(S[2],lang))
  if (length(S)>1)return(test) else return(FALSE)
}
is.prefix.unary.atomic.L.formula <- function(txt, lang){
  First <- extract.first.symbol(txt)
  test <- (First %in% unary.L.prefix.predicates(lang))
  d <- deparenthesize(delete.prefix(txt,First))
  test & is.L.term(d,lang)
}
is.prefix.binary.atomic.L.formula <- function(txt, lang){
  First <- extract.first.symbol(txt)
  txt <- normalize.text(txt)
  test1 <- (First %in% binary.L.prefix.predicates(lang))
  if (test1 == TRUE) d <- deparenthesize(delete.prefix(txt, First)) else d <- "FALSE"
  L <- unlist(strsplit(d, split =","))
  n <- length(L)
  test2 <- rep(0,n)
  for (i in 1:n){test2[i] = is.L.term(L[i], lang)}
  test1 & !any(test2 == FALSE)
}
is.atomic.L.formula <- function(txt,lang){
  is.L.sentence.constant(txt,lang)|is.L.sentenceletter(txt,lang)|is.prefix.unary.atomic.L.formula(txt,lang)|is.prefix.binary.atomic.L.formula(txt,lang)|is.L.eq.formula(txt, lang) 
}
is.neg.atomic.L.formula <- function(txt, lang){
  a <- has.unary.prefix(txt,"~")
  b <- has.unary.prefix(txt,"!")
  c <- delete.unary.prefix(txt,"~")
  d <- delete.unary.prefix(txt,"!")
  (a|b) & (is.atomic.L.formula(c,lang)|is.atomic.L.formula(d,lang))
}
is.L.literal <- function(txt,lang){
  is.atomic.L.formula(txt, lang)|is.neg.atomic.L.formula(txt, lang)
}
is.L.atom <- function(txt,lang){
  is.atomic.L.term(txt,lang)|is.atomic.L.formula(txt,lang)
}