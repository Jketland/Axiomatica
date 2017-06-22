## Load logic package
source("/Users/jeffreyketland2016/Desktop/Data Science/Axiomatica/Logic_Package/Master.R")
## Examples semantics
A <- null.assign(0,L.0)
# View(A)
A <- reassign(null.assign(0,L.0), c("x","y"),c(2,2))
A <- reassign(null.assign(0, L.0), "p", 7)
val("x^",A,L.0)
val.prim("x",A)
val.prim("x", reassign(null.assign(0, L.0), "x", 7))
val("~(p & q)",A,L.0)

string <- "((x * Sy) = ((x * y) + x))"
val(string, A, L.0)
string <- "((x + 0) = x)"
val(string, A, L.0)
A <- null.assign(0,L.0)
string <- "((x * 1) = x)"
val(string, A,L.0)
val("SSS(z * (x + y))", A, L.0)
string <- "(p -> (q -> r))"
s <- split.infix(string, "->")[1]
s
val(string,A,L.0)
val("p", A, L.0)

parser(string, L.0)
syntax.test(string, L.0)
plot.parse.tree(string, L.0)
truth.table(string, L.0)
SAT("((p -> q) & ~(~p v r))", L.0)
SAT("p", L.0)
TAUT("(p v ~p)", L.0)
IMPLY("~X", "(X -> Y)", Prop.Logic)
IMPLY("p","q", Prop.Logic)
IMPLY("X","X", L.0)

truth.table("(False -> p)",L.0)
is.L.atom("False",L.0)
string <- "(True -> X)"
parser(string, L.0)
IMPLY("True","False", L.0)
string <- "(~(p & q) = (~p v ~q))"
parser(string, L.0)
plot.parse.tree(string, L.0)
TAUT(string, L.0)

EQUIV("p","p", Prop.Logic)
EQUIV("~(p & q)","(~p v ~q)", Prop.Logic)
EQUIV("~(p v q)","(~p & ~q)", L.0)
EQUIV("~(p -> q)","(p & ~q)", L.0)
TAUT("(~(p -> q) = (p & ~q))", L.0)
string <- "(p v ~p)"
parser(string, L.0)
plot.parse.tree(string, L.0)
truth.table(string, L.0)
TAUT("(p v ~p)", L.0)
