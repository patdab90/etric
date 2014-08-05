
etricutils.createVarnames <- function(n, p, J){
  varnames <- etricutils.e()
  varnames <- c(varnames, etricutils.L())
  varnames <- c(varnames, etricutils.w(J))
  
  for (a in 1:n) {
    for (b in 0:p) {
      varnames <- c(varnames, etricutils.cAB(J, a, b))
    }
  }
  
  for (b in 0:p) {    
    for (a in 1:n) {
      varnames <- c(varnames, etricutils.cBA(J, b, a))
    }
  }
  
  
}
#----------------
#variable names:

etricutils.cAB <- function(j, a, b){
  return(paste0('c', j, '(a', a, ',b', b, ')'))
}

etricutils.cBA <- function(j, b, a){
  return(paste0('c', j, '(b', b, ',a', a, ')'))
}

etricutils.cBB <- function(j, b){
  return(paste0('c', j, '(b', h, ',b', h+1, ')'))
}

etricutils.w <- function(j){
  return(paste0('w',j))
}

etricutils.L <- function(){
  return("L")
}

etricutils.e <- function(){
  return("e")
}

