## Tableau processing

delete.route.rows <- function(tab,route){
  K <- length(route)
  Branch <- tab[,c(1,2,4,6)]
  B <- as.character(Branch[,3])
  w.L <- which(B == "L")
  w.R <- which(B == "R")
  deletion.row <- rep(0,K)
  for (i in 1:K){
    if (route[i] == "L") deletion.row[i] <- w.R[i] 
    if (route[i] == "R") deletion.row[i] <- w.L[i] 
  }
  if (!is.na(deletion.row[1])){
    for (i in K:1){
      Branch <- Branch[-deletion.row[i],] 
    }
  }
  Branch
}
delete.mismatched.P.ID <- function(Branch){
  ID <- as.numeric(as.vector(Branch$`ID`))
  P.ID <- as.vector(Branch$`Parent ID`)
  n <- length(ID)
  g <- length(P.ID)
  if(n > 1){P.ID <- P.ID[2:g]}
  if(n == 1){P.ID <- 0}
  P.ID <- as.numeric(P.ID)
  t <- rep(0,length(P.ID))
  for (j in 1:length(P.ID)){t[j] <- P.ID[j] %in% ID}
  w <- which(t == 0)
  w <- w+1
  if (length(w) != 0) Branch <- Branch[-w,]
  if (length(w) == 0) Branch <- Branch
  Branch
}
make.branch <- function(tab,route){
  X <- delete.route.rows(tab,route)
  n <- length(X[,1])
  for (i in 1:n){X <- delete.mismatched.P.ID(X)}
  X
}
make.branch.withoutstrings <- function(tab,route){
  X <- delete.route.rows(tab,route)
  n <- length(X[,1])
  for (i in 1:n){X <- delete.mismatched.P.ID(X)}
  X[,-1]
}
make.routes <- function(tab){
  Branch <- tab[,c(1,2,4,6)]
  B <- as.character(Branch[,3])
  w <- which(B == "L")
  n <- length(w)
  if(n >0){S <- SEQ(n); S[S==0] <- "L"; S[S==1] <- "R"; Routes <- S}
  if(n == 0){Routes <- c("L")}
  Routes
}
is.closed.branch <- function(X, lang){
  n <- length(X)
  test <- rep(0,n)
  for(i in 1:n){test[i] <- is.L.literal(X[i],lang)}
  w <- which(test == 1)
  X <- X[w]
  n <- length(X)
  M <- matrix(0,n,n)
  for(i in 1:n){for(j in 1:n){M[i,j] <- (X[i] == NEG(X[j]) | X[i] == NEG.1(X[j])) }}
  any(M == 1)
}
is.closed <- function(tab,lang){
  Routes <- make.routes(tab)
  if (is.matrix(Routes) == TRUE){n <- length(Routes[,1]);  test <- rep(0,n)
  for(i in 1:n){
    R <- Routes[i,]
    B <- as.character(make.branch(tab,R)[,1])
    test[i] <- is.closed.branch(B, lang)
  } 
  Out <- !any(test == 0)}
  if (is.vector(Routes) == TRUE){B <- as.character(make.branch(tab,Routes)[,1]); 
  Out <- is.closed.branch(B, lang)}
  Out
}
make.branches <- function(tab){
  R <- make.routes(tab)
  if (is.vector(R) == TRUE){B <- as.character(make.branch(tab,R))} 
  if (is.matrix(R) == TRUE){n <- length(R[,1]); 
  B <- list(list(),list());   for(i in 1:n){ B[[i]] <- as.vector(make.branch(tab,R[i,]))}}
  B
}
make.branches.withoutstrings <- function(tab){
  R <- make.routes(tab)
  if (is.vector(R) == TRUE){B <- as.vector(make.branch.withoutstrings(tab,R))} 
  if (is.matrix(R) == TRUE){n <- length(R[,1]); 
  B <- list(list(),list());   for(i in 1:n){ B[[i]] <- as.vector(make.branch.withoutstrings(tab,R[i,]))}}
  B
}
num.branches <- function(tab){
  R <- make.routes(tab)
  if (is.vector(R) == TRUE){num <- 1}
  if (is.matrix(R) == TRUE){num <- length(R[,1])}
  num
}
branch.lengths <- function(tab){
  R <- make.routes(tab)
  B <- make.branches.withoutstrings(tab)
  n <- num.branches(tab)
  if (n > 1){L <- rep(0,n); for (i in 1:n){L[i] <- length(B[[i]][,1])}}
  if (n == 1){L <- length(B)}
  L
}
extend <- function(X,n){
  X <- as.matrix(X)
  k <- length(X[,1])
  Y <- matrix(0,n,3)
  if (n > k){for (i in 1:k){for (j in 1:3){Y[i,j] <- X[i,j]}}}
  if (n > k){for (i in (k+1):n){for (j in 1:3){Y[i,j] <- "NA"}}}
  if (n <= k){Y <- X}
  colnames(Y) <- c()
  as.data.frame(Y)
}
equiv.matrix <- function(B,i,j){!any(B[[i]]!=B[[j]])}
build.amalgmated.branch.table <- function(tab){
    R <- make.routes(tab)
    B <- make.branches.withoutstrings(tab)
    L <- branch.lengths(tab)
    n <- num.branches(tab)
    m <- max(L)
    A.Table <- matrix(0,m,(3*n))
    if (n == 1){A.Table <- B}
    if (n > 1){for (i in 1:n){B[[i]] <- extend(B[[i]],m)}}
    if (n > 1){A.Table <- B[[1]]; 
              for (j in 2:n){A.Table <- cbind(A.Table, B[[j]])}}
    A.Table
}
make.equiv.test <- function(B,i){
  test <- rep(0,i-1); 
  for (j in 1:(i-1)){test[j] <- equiv.matrix(B,j,i)}
  test
}

