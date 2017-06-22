## Tableau prover
is.theorem <- function(string, lang){ is.closed(tableau(NEG.1(string), lang), lang) }