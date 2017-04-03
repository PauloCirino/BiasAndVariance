reactiveGenerateAll <- function(input){
    allModelsAndData <- genTrainTestAllData(input = input)
    biasAndVarianceList <- getBianAndVariance(allModelsAndData = allModelsAndData)
    
    
    list(dataOrigin = allModelsAndData$dataOrigin,
         dataNoiseTrain = allModelsAndData$allDataList[[1]],
         modelsResults = allModelsAndData$allModelsList[[1]],
         meanMSETest = biasAndVarianceList$meanMSETest,
         meanBias = biasAndVarianceList$meanBias,
         meanVar = biasAndVarianceList$meanVar,
         noise = input$noiseRange)
}