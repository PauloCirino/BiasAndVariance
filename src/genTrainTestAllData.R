genTrainTestAllData <- function(input){
    allModels <- list()
    allData <- list()
    
    for(i in 1:input$numTrainedModels) allModels[[i]] <- list()
    
    dataOrigin = generateData(input)
    modelFun <- getModelFunc(input = input)
    
    for(i in 1:input$numTrainedModels){
        model <- generateOneModelAllFlex(  input = input,
                                           dataOrigin = dataOrigin,
                                           modelFun = modelFun,
                                           SEED = i)
        
        allModels[[i]] <- model$modelsResults
        allData[[i]] <- model$dataNoiseTrain 
    }
    
    list(dataOrigin = dataOrigin,
         allModelsList = allModels,
         allDataList = allData)
}