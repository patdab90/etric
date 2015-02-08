library(RXMCDA)
# Get the list of arguments behind the --args option
pathnames <- list.files(pattern="[.]R$", path="R", full.names=TRUE);
pathnames
sapply(pathnames, FUN=source);
#args <- commandArgs(trailingOnly = TRUE)

#alternatives
#performanceTale
#klasy/kategorie
#linearConstraints
alternativesFile <- "tests/alternatives.xml"
criteriaFile <- "tests/criteria.xml"
performanceTableFile <- "tests/performanceTable.xml"

err <- c()

treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
treeCriteria<-xmlTreeParse(criteriaFile,useInternalNodes=TRUE)
treePerformanceTable<-xmlTreeParse(performanceTableFile,useInternalNodes=TRUE)

if(treeAlternatives){
  stop()
}
if(treeCriteria){
  stop()
}
if(treePerformanceTable){
  stop()
}
if(checkXSD(treeAlternatives)){
  err <- "error"
}
if(checkXSD(treeCriteria)){
  err <- "error"
}
if(checkXSD(treePerformanceTable)){
  err <- "error"
}

if(length(err) > 0){
  stop()
}

critIDs <- getCriteriaIDs(treeCriteria)
altIDs <- getAlternativesIDs(treeAlternatives)[[1]]
perfTable <- getPerformanceTables(treePerformanceTable,altIDs = altIDs, critIDs = critIDs)

perfTable
th <- getCriteriaThreshold(treeCriteria, critIDs)
th
