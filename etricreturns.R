createPAModel <- function(etric, a, h){
  if(h < etric$p){
    etric <- createPA14Constraint(etric, a, h)
  }
  if(h > 1){
    etric <- createPA24Constraint(etric, a, h)
  }
  return(etric)
}

createPA14Constraint <- function(etric, a, h){
  rownames <- paste0('PA14')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=1, dimnames=list(rownames))
  rhs <- matrix(1, nrow=1, dimnames=list(rownames))
  lhs[,etricutils.vAH1(a, h)] <- 1
  lhs[,etricutils.vAH2(a, h)] <- 1
  lhs[,etricutils.vAH3(a, h)] <- 1
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPA24Constraint <- function(etric, a, h){
  rownames <- paste0('PA24')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=1, dimnames=list(rownames))
  rhs <- matrix(1, nrow=1, dimnames=list(rownames))
  lhs[,etricutils.vAH4(a, h)] <- 1
  lhs[,etricutils.vAH5(a, h)] <- 1
  lhs[,etricutils.vAH6(a, h)] <- 1
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createNA1Model <- function(etric, a, h){
  etric <- createNA11Constraint(etric, a, h)
  etric <- createNA12Constraint(etric, a, h)
  etric <- createNA13Constraint(etric, a, h)
  return(etric)
}


createNA2Model <- function(etric, a, h){
  etric <- createNA21Constraint(etric, a, h)
  etric <- createNA22Constraint(etric, a, h)
  etric <- createNA23Constraint(etric, a, h)
  return(etric)
}

createNA11Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA11')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  lhs[,etricutils.cAB(J, a, h)] <- 1
  lhs[,etricutils.L()] <- -1
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createNA12Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA12')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, a, h)] <- 1
  lhs[,etricutils.L()] <- -1
  lhs[,etricutils.e()] <- 1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createNA13Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA13')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, a, h+1)] <- 1
  lhs[,etricutils.cBA(J, h, a)] <- -1
  lhs[,etricutils.e()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}




createNA21Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA21')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cBA(J, h, a)] <- 1
  lhs[,etricutils.L()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createNA22Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA22')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, a, h)] <- 1
  lhs[,etricutils.e()] <- 1
  lhs[,etricutils.L()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}



createNA23Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('NA23')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cBA(J, h -1,a)] <- 1
  lhs[,etricutils.cAB(J, a, h)] <- -1
  lhs[,etricutils.e()] <- -1
    
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createPLModel <- function(etric, a, b, h){
  etric <- createPL11Constraint(etric, b, h)
  etric <- createPL12Constraint(etric, b, h)
  etric <- createPL13Constraint(etric, b, h)
  etric <- createPL21Constraint(etric, a)
  etric <- createPL22Constraint(etric, a)
  etric <- createPL23Constraint(etric, a)
  etric <- createPL24Constraint(etric, a)
  return(etric)
}


createPRModel <- function(etric, a, b, h){
  etric <- createPR11Constraint(etric, a, h)
  etric <- createPR12Constraint(etric, a, h)
  etric <- createPR13Constraint(etric, a, h)
  etric <- createPR21Constraint(etric, b)
  etric <- createPR22Constraint(etric, b)
  etric <- createPR23Constraint(etric, b)
  etric <- createPR24Constraint(etric, b)
  return(etric)
}

createPL11Constraint <- function(etric, b, h){
  J <- etric$m
  rownames <- paste0('PL11')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, b, h-1)] <- 1
  lhs[,etricutils.L()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPL12Constraint <- function(etric, b, h){
  J <- etric$m
  rownames <- paste0('PL12')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cBA(J, h-1, b)] <- 1
  lhs[,etricutils.L()] <- -1
  lhs[,etricutils.e()] <- 1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createPL13Constraint <- function(etric, b, h){
  J <- etric$m
  rownames <- paste0('PL13')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  
  lhs[,etricutils.cAB(J, b, h)] <- 1
  lhs[,etricutils.cBA(J, h-1, b)] <- -1
  lhs[,etricutils.e()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}



createPL21Constraint <- function(etric, a){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PL21.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cAB(J, a, h-1)] <- 1
    lhs[,etricutils.vAH1(a, h)] <- M
    lhs[,etricutils.e()] <- 1
    lhs[,etricutils.L()] <- -1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}



createPL22Constraint <- function(etric, a){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PL22.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cBA(J, h-1, a)] <- 1
    lhs[,etricutils.vAH2(a, h)] <- -M
    lhs[,etricutils.L()] <- 1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPL23Constraint <- function(etric, a){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PL23.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cAB(J, a, h)] <- 1
    lhs[,etricutils.vAH3(a, h)] <- M
    lhs[,etricutils.cBA(J, h-1, a)] <- -1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPL24Constraint <- function(etric, a){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PL24.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(1, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.vAH1(a, h)] <- 1
    lhs[,etricutils.vAH2(a, h)] <- 1
    lhs[,etricutils.vAH3(a, h)] <- 1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

#--------------------------------------------------------------------

createPR11Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('PR11')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cBA(J, h+1, a)] <- 1
  lhs[,etricutils.L()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPR12Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('PR12')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, a, h+1)] <- 1
  lhs[,etricutils.L()] <- -1
  lhs[,etricutils.e()] <- 1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}

createPR13Constraint <- function(etric, a, h){
  J <- etric$m
  rownames <- paste0('PR13')
  lhs <- matrix(0, nrow=1, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=1, dimnames=list(rownames))
  rhs <- matrix(0, nrow=1, dimnames=list(rownames))
  
  lhs[,etricutils.cAB(J, a, h+1)] <- -1
  lhs[,etricutils.cBA(J, h, a)] <- 1
  lhs[,etricutils.e()] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}



createPR21Constraint <- function(etric, b){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PR21.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cBA(J, h+1, b)] <- 1
    lhs[,etricutils.vAH4(b, h)] <- M
    lhs[,etricutils.e()] <- 1
    lhs[,etricutils.L()] <- -1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}



createPR22Constraint <- function(etric, b){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PR22.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cAB(J, b, h+1)] <- 1
    lhs[,etricutils.vAH5(b, h)] <- -M
    lhs[,etricutils.L()] <- 1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPR23Constraint <- function(etric, b){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PR23.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.cAB(J, b, h+1)] <- -1
    lhs[,etricutils.vAH6(b, h)] <- M
    lhs[,etricutils.cBA(J, h, b)] <- 1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}


createPR24Constraint <- function(etric, b){
  J <- etric$m
  nrows <- 1:etric$p
  rownames <- paste0('PR24.',1:nrows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(1, nrow=nrows, dimnames=list(rownames))
  
  for(h in 1:nrows){
    lhs[,etricutils.vAH4(b, h)] <- 1
    lhs[,etricutils.vAH5(b, h)] <- 1
    lhs[,etricutils.vAH6(b, h)] <- 1
  }
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric)
}