library(ROI.plugin.glpk)
library(ROI)
library(stringr) # for tests cases

solvers <- ROI_installed_solvers()
if (!is.na(solvers['symphony'])) {
  .solver <<- 'symphony'
} else if (!is.na(solvers['glpk'])) {
  .solver <<- 'glpk'
} else {
  stop("No ROI Symphony or GLPK plugin installed")
}
source('etric.R')
#warianty
alts <- read.table(file="alts.csv", sep="\t", header=TRUE)
rownames(alts) = alts[,1]
alts <- alts[,2:ncol(alts)]

#granice klas
profs <- read.table(file="profs.csv", sep="\t", header=FALSE)
rownames(profs) = profs[,1]
profs <- profs[,2:ncol(profs)]
colnames(profs) <- colnames(alts)
#przykładowe progi
thresholds <- matrix(c(
  0, 4, 0, 12,
  0, 1, 0, 2,
  0, 100, 0, 200),ncol=4, byrow=TRUE)

# przykładowe przdziały do klas
assigs1 <- matrix(c(2, 1, 2,
                    5, 3, 3),ncol=3, byrow=TRUE)

monotonicity <- c(TRUE, TRUE, FALSE)

cardinalities <- matrix(c(1, 1, -1, 
                        #  2, 2, 3,
                          3, 1, -1), ncol=3, byrow=TRUE)

pairwiseComparisionsK <- matrix(c(1, 2, 1), ncol=3, byrow=TRUE)

pairwiseComparisionsL <- matrix(c(5, 2, 2), ncol=3, byrow=TRUE)

message("--- starting tests, iteration 1")


constraintsToString <- function(lhs, dir, rhs){
  res <- matrix("", nrow=nrow(lhs), ncol=1, dimnames=list(rownames(lhs)))
  for(j in 1:nrow(lhs)){
    for(i in 1:ncol(lhs)){
      if(lhs[j,i] != 0){
        if(lhs[j,i] > 0){
          sign <- "+"
          if(res[j,] == "") {
            sign <- "" 
          }
          if(lhs[j,i] == 1){
            res[j,] <- paste(res[j,], sign ,colnames(lhs)[i])
          }else{
            res[j,] <- paste(res[j,],sign,lhs[j,i],colnames(lhs)[i])
          }
        }else{
          if(lhs[j,i] == -1){
            res[j,] <- paste(res[j,],"-",colnames(lhs)[i])
          }else{
            res[j,] <- paste(res[j,],lhs[j,i],colnames(lhs)[i])
          }
        }
      }
    }
    res[j,] <- paste(res[j,],dir[j,],rhs[j,])
  }
  return(res)
}


etri <- etric.init(alts, profs, assigs1, monotonicity, th=thresholds,
                   cardinalities, pairwiseComparisionsK, pairwiseComparisionsL)

c <- constraintsToString(etri$constr$lhs, etri$constr$dir, etri$constr$rhs)
c
etric.isFeasible(etri, .solver)
p <- etric.possibleAssigment(etric=etri, .solver)
p
#n <- etric.necessaryAssigment(etri, .solver)
#n
r <- etric.necessaryPreferenceRelation(etri, .solver)
r
cc <- etric.classCardinalities(etri, FALSE, .solver)
cc
#etri$constr$lhs[str_detect(rownames(etri$constr$lhs),"PCL12.*"),]