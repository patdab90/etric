source('etricutils.R')
source('etricommon.R')
source('etricbase.R')
source('etricassignment.R')
source('etricpaiwisecmp.R')

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
  p <- nrow(profiles)-2 # p - liczba klas. Klas jest o 2 mniej niż profili
  m <- ncol(performances)
  
  A <- 1:n ## an
  B <- 0:(p+1) ## profile
  H <- 1:p ## klasy 
  J <- 1:m ## j
  
  etric <- list()
  etric$n <- n
  etric$p <- p
  etric$m <- m
  
  var <- etricutils.createVarnames(n, p, J, pCk, pCl)
  
  etric$binaryvar <- var$binary
  
  etric$constr$lhs <- intiLHS(c(var$float, var$binary))
  etric$constr$dir <- initDIR()
  etric$constr$rhs <- initRHS()
  
  etric <- etric.buildBaseModel(etric, performances, profiles, th, monotonicity)
  etric <- etric.buildAEModel(etric, assignments)
  etric <- etric.buildPCModel(etric, pCk, pCl )
  return(etric)
}

intiLHS <- function(names){
  lhs <- matrix(0, ncol=length(names), nrow=1,dimnames=list("E", names))
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


