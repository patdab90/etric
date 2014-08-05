source('etriutils.R')

etric.init <- function(performances, profiles, assignments, monotonicity, th, cardinalities, pCk, pCl){
  stopifnot(ncol(performances) == ncol(profiles))
  stopifnot(is.null(assignments) || ncol(assignments) == 3)
  stopifnot((is.null(pCk)) || (ncol(pCk) == 3))
  stopifnot(is.null(pCl) || ncol(pCl) == 3)
  stopifnot(nrow(assignments) < nrow(performances))
  stopifnot(nrow(th) == ncol(performances))
  stopifnot(nrow(th) == length(monotonicity))
  
  message("--- Constructing model")
  
  n <- nrow(performances)
  p <- nrow(profiles)-1
  m <- ncol(performances)
  
  A <- 1:n ## an
  B <- 0:p ## profile
  H <- 1:p ## klasy 
  J <- 1:m ## j
  
  etric <- list()
  etric$n <- n
  etric$p <- p
  etric$m <- m
  
  varnames <- etricutils.createVarnames(n, p, J)
  
  etric$constr$lhs <- intiLHS(varnames)
  etric$constr$dir <- initDIR()
  etric$constr$rhs <- initRHS()
}

intiLHS <- function(names){
  lhs <- matrix(0, ncol=length(names), nrow=1,dimnames=list("E",names))
  lhs["E",etricutils.e()] <- 1
  return(lhs)
}

initDIR <- function(){
  dir <- matrix(c(">="))
  rownames(dir) <- c("E")
  return(dir)
}

initRHS <- function(){
  rhs <- matrix(MINEPS)
  rownames(rhs) <- "E"
  return(rhs)
}


