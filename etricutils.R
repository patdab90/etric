
etricutils.createVarnames <- function(n, p, J, pcK, pcL){
  varnames <- etricutils.e()
  varnames <- c(varnames, etricutils.L())
  varnames <- c(varnames, etricutils.w(J))
  
  binaryvar <- c()
  
  for (a in 1:n) {
    for (b in 0:(p+1)) {
      varnames <- c(varnames, etricutils.cAB(J, a, b))
    }
  }
  
  for (b in 0:(p+1)) {    
    for (a in 1:n) {
      varnames <- c(varnames, etricutils.cBA(J, b, a))
    }
  }
  
  for (b in 1:(p+1)) {    
    varnames <- c(varnames, etricutils.cBB(J, b))
  }
  
  if(!is.null(pcK)){
    for(i in 1:nrow(pcK)){
      binaryvar <- c(binaryvar, etricutils.vABKH(pcK[i, 1], pcK[i, 2], pcK[i,3], 1:(p-pcK[i, 3])))
    }
  }
  
  if(!is.null(pcL)){
    for(i in 1:nrow(pcL)){
      binaryvar <- c(binaryvar, etricutils.vABLH(pcL[i, 1], pcL[i, 2], pcL[i,3], 1:(p-pcL[i, 3])))
    }
  }
  
  for(a in 1:n){
    binaryvar <- c(binaryvar, etricutils.vAH(a,1:etric$p))
  }
  
  for(a in 1:n){
    binaryvar <- c(binaryvar, etricutils.vAH1(a,1:etric$p))
  }
  
  for(a in 1:n){
    binaryvar <- c(binaryvar, etricutils.vAH2(a,1:etric$p))
  }
  
  for(a in 1:n){
    binaryvar <- c(binaryvar, etricutils.vAH3(a,1:etric$p))
  }
  
  return(list(float=varnames, binary=binaryvar))
}
#----------------
#variable names:

etricutils.cAB <- function(j, a, b){
  return(paste0('c', j, '(a', a, ',b', b, ')'))
}

etricutils.cBA <- function(j, b, a){
  return(paste0('c', j, '(b', b, ',a', a, ')'))
}

etricutils.cBB <- function(j, h){
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

etricutils.vABKH <- function(a, b, k, h){
  return(paste0('v(a',a,',b',b,'>=',k,'h',h,')'))
}

etricutils.vABLH <- function(a, b, l, h){
  return(paste0('v(a',a,',b',b,'<=',l,'h',h,')'))
}

etricutils.vAH <- function(a, h){
  return(paste0('v(a',a,',h',h,')'))
}


etricutils.vAH1 <- function(a, h){
  return(paste0('v(a',a,',h',h,',1)'))
}


etricutils.vAH2 <- function(a, h){
  return(paste0('v(a',a,',h',h,',2)'))
}

etricutils.vAH3 <- function(a, h){
  return(paste0('v(a',a,',h',h,',3)'))
}

etricutils.vAH4 <- function(a, h){
  return(paste0('v(a',a,',h',h,',4)'))
}


etricutils.vAH5 <- function(a, h){
  return(paste0('v(a',a,',h',h,',5)'))
}

etricutils.vAH6 <- function(a, h){
  return(paste0('v(a',a,',h',h,',6)'))
}


