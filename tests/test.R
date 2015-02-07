library(ROI.plugin.glpk)
library(ROI)

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
alts <- read.table(file="alts_test.csv", sep=",", header=TRUE)
rownames(alts) = alts[,1]
alts <- alts[,2:ncol(alts)]

#granice klas
profs <- read.table(file="profs_test.csv", sep=",", header=FALSE)
rownames(profs) = profs[,1]
profs <- profs[,2:ncol(profs)]
colnames(profs) <- colnames(alts)

thresholds <- matrix(c(
  0, 0.01, 0, 0.02,
  0, 0, 1.9, 0, 
  0, 0, 1.9, 0, 
  0, 0, 1.9, 0, 
  0, 0, 2, 0),ncol=4, byrow=TRUE)

as <- matrix(c(
  1, 1,
  8, 2,
  13, 2,
  16, 3,
  40, 4)
  , ncol=2, byrow=TRUE)

assigns <- matrix(c(as[,1], as[,2], as[,2]), ncol=3, nrow=nrow(as))


monotonicity <- c(FALSE, FALSE, FALSE, FALSE, FALSE)

cardinalities <- NULL#matrix(c(1, 1, -1, 
#  2, 2, 3,
                         # 3, 1, -1), ncol=3, byrow=TRUE)

pairwiseComparisionsK <- NULL#matrix(c(1, 2, 1), ncol=3, byrow=TRUE)

pairwiseComparisionsL <- NULL#matrix(c(5, 2, 2), ncol=3, byrow=TRUE)

message("--- starting tests, iteration 1")
profs <- etric.createBorderProfiles(alts,profs,monotonicity)
etri <- etric.init(alts, profs, assigns, monotonicity, th=thresholds,
                   cardinalities, pairwiseComparisionsK, pairwiseComparisionsL)

f <- etric.isFeasible(etri, .solver)
f
if(f){
  p <- etric.possibleAssigment(etric=etri, .solver)
  p
  n <- etric.necessaryAssigment(etri, .solver)
  n
  #r <- etric.necessaryPreferenceRelation(etri, .solver)
  #r
  cc <- etric.classCardinalities(etri, FALSE, .solver)
  cc
}
#etri$constr$lhs[str_detect(rownames(etri$constr$lhs),"PCL12.*"),]