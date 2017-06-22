## Language environments

extend.L <- function(lang, newsymbol){
  as.data.frame(rbind(as.matrix(lang), newsymbol))
}

## Prop.Logic
prop.logic.nonsymbols <- c("[","]","{","}",",","@",".",":","%"," ' ","#","?","[]","<>","A","E","<-","<->")
Prop.Logic <- subset(
  L.0, 
  Type != "Term -> Term" & Type != "Term^2 -> Term" &
  Type != "Term -> Form" & Type != "Term^2 -> Form" & Type != "Atom.term" &
  !(Symbol %in% prop.logic.nonsymbols))

## Algebraic terms
algebraic.terms.nonsymbols <- c("[","]","{","}",",","@",".",":","%"," ' ","#","?","[]","<>","A","E","<-","<->")
Algebraic.Terms <- subset(L.0, Type != "Form -> Form" & Type != "Form^2 -> Form" &
                    Type != "Term^2 -> Form" & Type != "Atom.form" & !(Symbol %in% algebraic.terms.nonsymbols))


## Algebra
algebra.nonsymbols <- c("[","]","{","}",",","@",".",":","%"," ' ","#","?","[]","<>","A","E","<-","<->")
Algebra <- subset(L.0, 
                  Type != "Form -> Form" & Type != "Form^2 -> Form" & Type != "Atom.form" & !(Symbol %in% algebra.nonsymbols))

## FOL
FOL.nonsymbols <- c("[","]","{","}",",","@",".",":","%"," ' ","#","?","[]","<>","<-","<->")

# letter.P <- c("P","letter.P","Term -> Form", "Prefix pred", 1)
# letter.F <- c("F","letter.F","Term^2 -> Form", "Prefix pred", 2)
# FOL <- extend.L(L.0, letter.P); rownames(FOL) <- c()
# FOL <- extend.L(FOL, letter.F); rownames(FOL) <- c()
FOL <- subset(L.0,!(Symbol %in% FOL.nonsymbols))

## Language function for Shiny app
L <- function(n){
  if (n == 1) lang <- L.0
  if (n == 2) lang <- Prop.Logic
  if (n == 3) lang <- Algebraic.Terms
  if (n == 4) lang <- Algebra
  if (n %in% 5:7) lang <- FOL
  lang
}

