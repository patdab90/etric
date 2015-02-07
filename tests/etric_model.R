library(RXMCDA)
# Get the list of arguments behind the --args option
source("../RXMCDAextended.R")
#args <- commandArgs(trailingOnly = TRUE)

#alternatives
#performanceTale
#klasy/kategorie
#linearConstraints
alternativesFile <- "alternatives.xml"
criteriaFile <- "criteria.xml"
performanceTableFile <- "performanceTable.xml"

err <- c()

treeAlternatives<-xmlTreeParse(alternativesFile,useInternalNodes=TRUE)
treeCriteria<-xmlTreeParse(criteriaFile,useInternalNodes=TRUE)
treePerformanceTable<-xmlTreeParse(performanceTableFile,useInternalNodes=TRUE)


if(checkXSD(treeAlternatives)){
  
}
if(checkXSD(treeCriteria)){
  
}
if(checkXSD(treePerformanceTable)){
  
}


critIDs <- getCriteriaIDs(treeCriteria)
altIDs <- getAlternativesIDs(treeAlternatives)[[1]]
perfTable <- getPerformanceTables(treePerformanceTable,altIDs = altIDs, critIDs = critIDs)

perfTable
th <- getCriteriaThreshold(treeCriteria, critIDs)
th
