generateOneModelAllFlex <- function(input, dataOrigin, modelFun, SEED = NULL){
    if(! is.null(SEED) ) set.seed(seed = SEED)
    
    dataNoiseTrain = generateNoiseData(input, trainData = TRUE)

    modelsResults <- list()
    
    flexValues <- seq(from = input$modelFlexibility[1],
                      to = input$modelFlexibility[2],
                      by = 1)
    
    for(i in 1:length(flexValues)){
        flexibility = flexValues[i]
        modelResult <- modelFun(Xtrain = dataNoiseTrain$X,
                                Ytrain = dataNoiseTrain$Y,
                                flexibility = flexibility,
                                Xtest = dataOrigin$X)
        
        mseTrain <- mse(modelResult$fittedTrain,  dataNoiseTrain$Y)
        mseTest <- mse(modelResult$fittedTest, dataOrigin$Y)
            
        modelsResults[[i]] <- list(flexibility = flexibility,
                                   fittedTrain = modelResult$fittedTrain,
                                   fittedTest = modelResult$fittedTest,
                                   MSETrain = mseTrain,
                                   MSETest = mseTest)
    }
    
    list(dataNoiseTrain = dataNoiseTrain,
         modelsResults = modelsResults)
}