getBianAndVariance <- function(allModelsAndData){
    
    dataOrigin <- allModelsAndData$dataOrigin
    allModelsList <- allModelsAndData$allModelsList
    allDataList <- allModelsAndData$allDataList
    
    nModels <- length(allModelsList)
    nFlexs <- length(allModelsList[[1]])
    nTestObs <-  length(allModelsList[[1]][[1]]$fittedTest)
    
    MSETrainList <- list()
    MSETestList <- list()
    fittedTestList <- list()
    
    for(nFlex in 1:nFlexs){
        MSETestList[[nFlex]] <- numeric(length = nModels)
        fittedTestList[[nFlex]] <- matrix(nrow = nTestObs, ncol = nModels)
    }
    
    for(nModel in 1:nModels){
        for(nFlex in 1:nFlexs){
            modelResult <- allModelsList[[nModel]][[nFlex]]
            MSETestList[[nFlex]][nModel] <- modelResult$MSETest
            fittedTestList[[nFlex]][, nModel] <- modelResult$fittedTest
        }
    }
    
    meanMSETest <- numeric()
    meanBias <- numeric()
    meanVar <- numeric()

    for(nFlex in 1:nFlexs){
        meanYPred <- apply(fittedTestList[[nFlex]], 1, mean)
        meanMSETest[[nFlex]] <- mean(MSETestList[[nFlex]])
        meanBias[[nFlex]] <- mean( (dataOrigin$Y - meanYPred) ** 2)
        meanVar[[nFlex]] <- mean( apply(fittedTestList[[nFlex]], 1, var) )
    }
    
    list(meanMSETest = meanMSETest,
         meanBias = meanBias,
         meanVar = meanVar)
}