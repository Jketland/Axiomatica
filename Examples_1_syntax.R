## Load logic package
source("/Users/jeffreyketland2016/Desktop/Data Science/Axiomatica/Logic_Package/Master.R")
## Examples
# View(L.0)
L.constants(L.0)
# prim.L.variables(L.0)
arity("~", L.0)
type("->",L.0)
syntax("->",L.0)
n.ary.L.constructors(1,FOL)
n.ary.L.constructors(2,L.0)

# View(L.1)
arity("P",L.0)
is.L.variable("p",L.0)
is.L.term("(1 * S(x^^^ + 0))",L.0)
apply.L.constructor("p", "A", "x",L.0)
pad.text(apply.L.constructor(c("p","q"), "->", "x",L.0),"->")
syntax("~", L.0)
apply.L.constructor("p", "~", "x", L.0) 
complexity.ar("~~(p&(qv~r))",1,L.0)
complexity("p",L.0)
pad.text(pad.text("~~(p&(qv~r))","&"),"v")
has.unary.prefix("Ax","A")
pad.text(EQ("p","q"),"=")
arity("<>",L.0)
delete.unary.prefix("Ax","Ax")
is.prefix.unary.atomic.L.formula("P(x)",L.0)
is.L.literal("~(x+Sy)",L.0)
is.prefix.unary.atomic.L.formula("F(x)",L.0)
is.atomic.L.formula("P(x)",L.0)
is.neg.atomic.L.formula("~P(Sx)",L.0)
is.prefix.unary.atomic.L.formula("P(Sx)",L.0)
is.prefix.binary.atomic.L.formula("R(Sx,(z+x))",L.0)
is.prefix.binary.atomic.L.formula("p",L.0)

parser("p", L.0)

## Some examples of parsing
string <- "AxEy((x + y) = (y + x))"
parser(string, L.0)
parser("AxEy(P(x) -> F(x,y))",FOL)
delete.unary.prefix(string,"Ax")
syntax.test(string, L.0)
string <- "<>(p -> (Sx + x))"
string <- "(P((x+z)) -> F(x,Sy))"
P <- parser(string, L.0)
P
is.wellformed(string, FOL)
is.wellformed.cs(P)

split.infix("(p -> q", "->")
complexity.ar("(p -> q)",2,L.0)
has.prefix("SS0","S")
prefix.numeral(7,"x","PQ")
suffix.numeral(3,"x","^")
string <- "(p v (~p & (True v False)))"
parser(string, L.0)
is.wellformed(string, Prop.Logic)
imm.pred(string, L.0)
main.constructor(string, L.0)
main.constructor("p",L.0)
syntax.test("p",L.0)
imm.pred("",L.0)
parser("((x + Sy) = (Sy + x))",L.0)
is.wellformed("((x + y) = (y + x))",L.0)
main.constructor("(S(x + y) = (y + x))",L.0)
syntax.test("((x+y) = SSSSSSSSSSx)",L.0)
parser("~((x + y) = x)",L.0)
atoms.in.expression("((~P + y) = z)",L.0)

## Some examples of plotting

string <- "[]((x+y) =(y+x))"
plot.parse.tree(string, L.0)
string <- "Ax(P(x) -> EyR(x,y))"
plot.parse.tree(string, L.0)
string <- "(X -> (Sx + x))"
plot.parse.tree(string, L.0)
P <- parser(PLUS("x",as.numeric(zero[1])),L.0);
plot.parse.tree.cs(P,L.0)
string <- EQ(PROD("x",SUCC("y")),PLUS(PROD("x","y"),"x")); plot.parse.tree(string, L.0)
string <- "(~p -> ~(~q -> ~r))"
parser(string, L.0)
plot.parse.tree(string, L.0)
plot.parse.tree.diff(string, L.0)
P <- parser(string, L.0);
plot.parse.tree.cs(P, L.0)


string <- "((p -> q) -> (q - > r))"
parser(string, L.0)
