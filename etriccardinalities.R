etric.createCCModel <- function(etric, card){
  
  return(etric)
}

createCC1Constraint <- function(etric){
  nrows <- etric$n
  rownames <- paste0('CC1.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(1, nrow=nrows, dimnames=list(rownames))
  for(a in 1:nrows){
    lhs[a,etricutils.vAH(a, 1:etric$p)] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC21Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC21.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH1(a, h)] <- M
      lhs[a,etricutils.cAB(J, a, h)] <- 1
      lhs[a,etricutils.e()] <- 1
      lhs[a,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}



createCC22Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC22.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH2(a, h)] <- -M
      lhs[a,etricutils.cBA(J, h, a)] <- 1
      lhs[a,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC23Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC23.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH3(a, h)] <- M
      lhs[a,etricutils.cAB(J, a, h+1)] <- 1
      lhs[a,etricutils.cBA(J, h, a)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC24Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC24.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  lhs[a,etricutils.vAH1(a, h)] <- 1
  lhs[a,etricutils.vAH2(a, h)] <- 1
  lhs[a,etricutils.vAH3(a, h)] <- 1
  lhs[a,etricutils.vAH(a, h)] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

###----------


createCC31Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC31.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH4(a, h)] <- M
      lhs[a,etricutils.cBA(J, h, a)] <- 1
      lhs[a,etricutils.e()] <- 1
      lhs[a,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}



createCC32Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC32.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(-M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH5(a, h)] <- -M
      lhs[a,etricutils.cAB(J, a, h)] <- 1
      lhs[a,etricutils.L()] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}


createCC33Constraint <- function(etric){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC33.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  for(a in 1:etr){
    for(h in 1:etric$p){
      lhs[a,etricutils.vAH6(a, h)] <- M
      lhs[a,etricutils.cBA(J, h-1, a)] <- 1
      lhs[a,etricutils.cAB(J, a, b)] <- -1
    }
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC34Constraint <- function(etric, card){
  J <- 1:etric$m
  nrows <- etric$n * etric$p
  rownames <- paste0('CC34.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("==", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(M, nrow=nrows, dimnames=list(rownames))
  
  lhs[a,etricutils.vAH4(a, h)] <- 1
  lhs[a,etricutils.vAH5(a, h)] <- 1
  lhs[a,etricutils.vAH6(a, h)] <- 1
  lhs[a,etricutils.vAH(a, h)] <- -1
  
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC4Constraint <- function(etric, card){
  nrows <- nrow(card)
  rownames <- paste0('CC4.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(card[,2], nrow=nrows, dimnames=list(rownames))
  for(h in 1:nrows){
    lhs[h,etricutils.vAH(1:etric$n, card[h,1])] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}

createCC5Constraint <- function(etric, card){
  nrows <- nrow(card)
  rownames <- paste0('CC5.',1:rows)
  lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix("<=", nrow=nrows, dimnames=list(rownames))
  rhs <- matrix(card[,3], nrow=nrows, dimnames=list(rownames))
  for(h in 1:nrows){
    lhs[h,etricutils.vAH(1:etric$n, card[h,1])] <- 1
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  return(etric) 
}