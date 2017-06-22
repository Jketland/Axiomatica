## Base Language: L.0
op.paren <- c("(", "op.paren", "Punct", "Punct", "NULL")
cl.paren <- c(")",  "cl.paren", "Punct", "Punct", "NULL")
op.square.paren <- c("[", "op.square.paren", "Punct", "Punct", "NULL")
cl.square.paren <- c("]", "cl.square,paren", "Punct", "Punct", "NULL")
op.curly.paren <- c("{","op.curly.paren", "Punct", "Punct", "NULL")
cl.curly.paren <- c("}", "cl.curly.paren", "Punct", "Punct", "NULL")
comma <- c(",", "comma",  "Punct", "Punct", "NULL")
caret <- c("^", "caret", "Punct", "Punct", "NULL")
at <- c("@", "at", "Punct", "Punct", "NULL")
stop <- c(".", "stop", "Punct", "Punct", "NULL")
colon <- c(":", "colon",  "Punct", "Punct", "NULL")
percent <- c("%", "percent", "Punct", "Punct", "NULL")
singlequote <- c(" ' ", "singlequote", "Punct", "Punct", "NULL")
hash <- c("#", "hash", "Punct", "Punct", "NULL")
question <- c("?", "question", "Punct", "Punct", "NULL")
neg <- c("!", "neg", "Form -> Form", "Prefix", 1)
tilde <- c("~", "tilde", "Form -> Form", "Prefix", 1)
nec  <- c("[]", "nec", "Form -> Form", "Prefix", 1)
pos  <- c("<>", "pos", "Form -> Form", "Prefix", 1)
forall <- c("A", "forall", "Form -> Form", "Prefix quant", 1)
exists <- c("E", "exists", "Form -> Form", "Prefix quant", 1)
and <- c("&", "and", "Form^2 -> Form", "Infix", 2)
or <- c("v", "or", "Form^2 -> Form", "Infix", 2)
pipe <- c("|", "pipe", "Form^2 -> Form", "Infix", 2)
right.arrow <- c("->", "right.arrow", "Form^2 -> Form", "Infix", 2)
left.arrow <- c("<-", "left.arrow", "Form^2 -> Form", "Infix", 2)
leftright.arrow <- c("<->", "leftright.arrow","Form^2 -> Form", "Infix", 2)
succ <- c("S", "succ", "Term -> Term", "Prefix", 1)
plus <- c("+", "plus", "Term^2 -> Term", "Infix", 2)
prod <- c("*", "prod", "Term^2 -> Term", "Infix", 2)
eq <- c("=", "eq", "Term^2 -> Form", "Infix", 2)
gt <- c(">", "gt", "Term^2 -> Form", "Infix", 2)
lt <- c("<", "lt", "Term^2 -> Form", "Infix", 2)
neq <- c("!=", "neq", "Term^2 -> Form", "Infix", 2)
turnstile <- c("|-", "turnstile", "Term^2 -> Form", "Infix", 2)
double.turnstile <- c("|=", "double.turnstile", "Term^2 -> Form", "Infix", 2)
thetrue <- c("True", "thetrue", "Atom.form", "Sentence constant", 0)
thefalse <- c("False", "thefalse", "Atom.form", "Sentence constant", 0)
zero <- c("0", "zero", "Atom.term", "Constant", 0)
one <- c("1", "one", "Atom.term", "Constant", 0)
letter.x <- c("x", "letter.x", "Atom.term", "Variable", 0)
letter.y <- c("y", "letter.y", "Atom.term", "Variable", 0)
letter.z <- c("z", "letter.z", "Atom.term", "Variable", 0)
letter.X <- c("X", "letter.X", "Atom.form", "Sentence variable", 0)
letter.Y <- c("Y", "letter.Y", "Atom.form", "Sentence variable", 0)
letter.Z <- c("Z", "letter.Z", "Atom.form", "Sentence variable", 0)
letter.p <- c("p", "letter.p", "Atom.form", "Sentence letter", 0)
letter.q <- c("q", "letter.q", "Atom.form", "Sentence letter", 0)
letter.r <- c("r", "letter.r", "Atom.form", "Sentence letter", 0)
letter.F <- c("F","letter.F","Term -> Form", "Prefix pred", 1)
letter.P <- c("P","letter.P","Term -> Form", "Prefix pred", 1)
letter.Q <- c("Q","letter.Q","Term -> Form", "Prefix pred", 1)
letter.R <- c("R","letter.R","Term^2 -> Form", "Prefix pred", 2)

L.0 <- as.data.frame(rbind(op.paren, cl.paren, op.square.paren,
                           cl.square.paren, op.curly.paren, cl.curly.paren, 
                           comma, caret, at, stop, colon, percent, singlequote,
                           hash, question,
                           neg, tilde, nec, pos, forall, exists,
                           and, or, pipe, right.arrow, left.arrow,
                           leftright.arrow, 
                           succ, plus, prod, eq, lt, neq, 
                           turnstile, double.turnstile,
                           thetrue, thefalse, zero, one, 
                           letter.x, letter.y, letter.z, 
                           letter.X, letter.Y, letter.Z, 
                           letter.p, letter.q, letter.r, 
                           letter.F,letter.P, letter.Q, letter.R))
colnames(L.0) <- c("Symbol", "Name", "Type", "Syntax", "Arity")
rownames(L.0) <- c()

# View(L.0)
