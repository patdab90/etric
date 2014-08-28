etric.buildPCModel <- function(etric, pck, pcl){
  etric <- createPCL11Constraint(etric, pck)
  etric <- createPCL12Constraint(etric, pck)
  etric <- createPCL13Constraint(etric, pck)
  etric <- createPCL21Constraint(etric, pck)
  etric <- createPCL22Constraint(etric, pck)
  etric <- createPCL23Constraint(etric, pck)
  etric <- createPCL3Constraint(etric, pck)

  etric <- createPCU11Constraint(etric, pcl)
  etric <- createPCU12Constraint(etric, pcl)
  etric <- createPCU13Constraint(etric, pcl)
  etric <- createPCU21Constraint(etric, pcl)
  etric <- createPCU22Constraint(etric, pcl)
  etric <- createPCU23Constraint(etric, pcl)
  etric <- createPCU3Constraint(etric, pcl)
  return(etric)
}

createPCL11Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL11.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      C <- etricutils.cAB(J, a, h-1+k)
      lhs[h, C] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}



createPCL12Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL12.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix("<=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      C <- etricutils.cBA(J, h-1+k, a)
      lhs[h, C] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.e()] <-  1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCL13Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL13.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h-1+k, a)
      CAB <- etricutils.cAB(J, a, h+k) 
      lhs[h, CAB] <- 1
      lhs[h, CBA] <- -1
      lhs[h, etricutils.e()] <-  -1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}



createPCL21Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL21.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h+1, b) 
      lhs[h, CBA] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCL22Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL22.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix("<=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cAB(J, b, h+1) 
      lhs[h, CBA] <- 1
      lhs[h, etricutils.e()] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCL23Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    nrows <- etric$p-k
    rownames <- paste0("PCL23.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h, b)
      CAB <- etricutils.cAB(J, a, h+1) 
      lhs[h, CAB] <- -1
      lhs[h, CBA] <- 1
      lhs[h, etricutils.e()] <-  -1
      lhs[h, etricutils.vABKH(a, b, k, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCL3Constraint <- function(etric, pck){
  J <- 1:etric$m
  comparisons <- nrow(pck)
  rownames <- paste0("PCL3.",1:comparisons)
  lhs <- matrix(0, nrow=comparisons, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=comparisons, ncol=1, dimnames=list(rownames))
  rhs <- matrix(1, nrow=comparisons, ncol=1, dimnames=list(rownames))
  for(r in 1:comparisons){
    pc <- pck[r,]
    a <- pck[r,1]
    b <- pck[r,2]
    k <- pck[r,3]
    
    lhs[r, etricutils.vABKH(a, b, k, 1:(etric$p-k))] <- 1  
    
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  
  return(etric)
}



## --- PCU ----------------------------------------------------------------------------------------------------

createPCU11Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU11.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      C <- etricutils.cBA(J, h+1+l, a)
      lhs[h, C] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}



createPCU12Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU12.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix("<=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      C <- etricutils.cAB(J, a, h+1+l)
      lhs[h, C] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.e()] <-  1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCU13Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU13.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h+l, a)
      CAB <- etricutils.cAB(J, a, h+1+l) 
      lhs[h, CAB] <- -1
      lhs[h, CBA] <- 1
      lhs[h, etricutils.e()] <-  -1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}



createPCU21Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU21.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CAB <- etricutils.cAB(J, b, h-1) 
      lhs[h, CAB] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCU22Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU22.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix("<=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h-1, b) 
      lhs[h, CBA] <- 1
      lhs[h, etricutils.e()] <- 1
      lhs[h, etricutils.L()] <- -1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCU23Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    nrows <- etric$p-l
    rownames <- paste0("PCU23.",1:nrows)
    lhs <- matrix(0, nrow=nrows, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
    dir <- matrix(">=", nrow=nrows, ncol=1, dimnames=list(rownames))
    rhs <- matrix(-M, nrow=nrows, ncol=1, dimnames=list(rownames))
    for(h in 1:nrows){
      CBA <- etricutils.cBA(J, h-1, b)
      CAB <- etricutils.cAB(J, a, h) 
      lhs[h, CAB] <- 1
      lhs[h, CBA] <- -1
      lhs[h, etricutils.e()] <-  -1
      lhs[h, etricutils.vABLH(a, b, l, h)] <- -M
    }
    
    etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
    etric$constr$dir <- rbind(etric$constr$dir, dir)
    etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
    
  }
  return(etric)
}


createPCU3Constraint <- function(etric, pcl){
  J <- 1:etric$m
  comparisons <- nrow(pcl)
  rownames <- paste0("PCU3.",1:comparisons)
  lhs <- matrix(0, nrow=comparisons, ncol=ncol(etric$constr$lhs), dimnames=list(rownames, colnames(etric$constr$lhs)))
  dir <- matrix(">=", nrow=comparisons, ncol=1, dimnames=list(rownames))
  rhs <- matrix(1, nrow=comparisons, ncol=1, dimnames=list(rownames))
  for(r in 1:comparisons){
    pc <- pcl[r,]
    a <- pcl[r,1]
    b <- pcl[r,2]
    l <- pcl[r,3]
    
    lhs[r, etricutils.vABLH(a, b, l, 1:(etric$p-l))] <- 1  
    
  }
  etric$constr$lhs <- rbind(etric$constr$lhs, lhs)
  etric$constr$dir <- rbind(etric$constr$dir, dir)
  etric$constr$rhs <- rbind(etric$constr$rhs, rhs)
  
  return(etric)
}
