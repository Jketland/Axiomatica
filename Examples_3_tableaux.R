## Load logic package
source("/Users/jeffreyketland2016/Desktop/Data Science/Axiomatica/Logic_Package/Master.R")
## Examples
## Tableaux
test.binary.symb.tab("(p -> q)", "->")
neg.binary.matrix.tab("~(p & q)", "&", L.0)
unary.delete.tab("~~(p v q)","~~", L.0)
is.DN.formula("!!(!p & q)")
unary.tab.matrix("AxP(x)", "A", L.0)
tableau("Ax(P(x) & Ey R(x,y))",L.0)
string <- "p"
lang <- L.0
D <- tableau(NEG.1(string), lang); D
is.matrix(make.routes(D))
route <- make.routes(D); route
make.branch(D,route)
is.theorem("~p", Prop.Logic)
is.theorem("(~(p & q) -> (~p v ~q))", Prop.Logic)
is.theorem("((p & q) -> p)", Prop.Logic)
is.theorem("((p -> q) -> (p -> (p -> (p -> q))))", Prop.Logic)
is.theorem("(AxP(x) -> P(x))", FOL)
is.theorem("(p -> p)", Prop.Logic)
is.theorem("(p -> q)", Prop.Logic)
string <- "(p -> q)"
lang <- Prop.Logic
is.theorem(string, Prop.Logic)
D <- tableau(string, L.0)
Route <- make.routes(D)
make.branch.withoutstrings(D,Route[1,])
make.branch.withoutstrings(D,Route[2,])
make.branches.withoutstrings(D)

is.theorem("(p v ~p)", Prop.Logic)
is.theorem("(((p & q) & (~p & q)) -> (~p & q))", Prop.Logic)
is.theorem("(~(p & q) -> (~p v ~q))", Prop.Logic)
is.theorem("(~(p v q) -> (~p & ~q))", L.0)
is.theorem("(~(p -> q) -> (p & ~q))", L.0)
string <- "(p -> (q -> p))"; 
lang <- Prop.Logic
is.theorem(string, lang)

string <- "~~~p"
g <- tableau.graph(string, L.0)
g
plot.tableau(string,L.0)

string <- "~((p & (p & p)) -> p)"
plot.tableau(string,L.0)

string <- "((p v (q & r)) -> ((p v q) & (p v r)))"
string <- AND(string, string)
string <- AND(string, string)
parser(string,L.0)
string <- AND(string, string)
parser(string,L.0)
syntax.test(string,L.0)
plot.tableau(NEG.1(string),L.0)

string <- AND(string, string)
parser(string,L.0)
length(tok(string))
plot.tableau(NEG.1(string),L.0)
is.theorem(string,L.0)

string <- "~(((p & q) & (~p & q)) -> (~p & q))"
lang <- L.0
parser(string, lang)
plot.parse.tree(string, lang)
syntax.test(string, lang)
tableau.p(string, lang)
g <- tableau.graph(string, L.0)
M <- as_data_frame(g, what = "vertices")
B <- all_simple_paths(g, from = i, to = j)
V <- as.vector(B[[1]]); V

plot.tableau("~(((p & q) & ~~(~p & q)) -> ~~(~p & q))", Prop.Logic)
is.theorem("(((p & q) & ~~(~p & q)) -> (~p & q))", Prop.Logic)

string <- "(p & q)"
string <- AND(string, string)
string
string <- AND(string, string)
string
string <- IF(string,"p")
string
is.theorem(string, lang)
tableau.p(NEG.1(string),lang)
plot.tableau(NEG.1(string),lang)

parser("((p & (q v r)) -> ((p & q) v (p & r)))",Prop.Logic)
is.theorem("((p v (q & r)) -> ((p v q) & (p v r)))", Prop.Logic)
plot.tableau("~((p v (q & r)) -> ((p v q) & (p v r)))", Prop.Logic)
syntax.test

dev.off()
