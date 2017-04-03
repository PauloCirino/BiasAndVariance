reactiveGenerateAll <- function(input){
    set.seed(Sys.time())
    baseSeed <- sample(10000, 1)
    
    funInput <- input$funInput
    range <- input$range
    numPoints <- input$numPoints
    noiseRange <- input$noiseRange
    model <- input$model
    modelFlexibility <- input$modelFlexibility
    numTrainedModels <- input$numTrainedModels
    
    fun <- getUserFun(funInput = funInput)
    modelFun <- getModelFunc(model = model)
    
    resultDF <- data.frame()
    testData <- list()
    trainDataSample <- list()
    testDataSample <- list()
    
    testData <- generateData(fun = fun, range = range, by = 0.1)
    YTestExpected <- testData$Data$Y
    XTest <- testData$Data$X
    
    for (flex in modelFlexibility[1]:modelFlexibility[2]){
        YTestPredictMatrix <- matrix(ncol = numTrainedModels, nrow = length(YTestExpected))
        noiseVar <- numeric(length = numTrainedModels)
        
        for(nModel in 1:numTrainedModels){
            set.seed(baseSeed + nModel)
            trainData <- generateData(fun = fun,
                                      range = range,
                                      nPoints = numPoints,
                                      noiseRange = noiseRange)
            
            noiseVar[nModel] <- trainData$noiseVar
            
            modelResult <- modelFun( Xtrain = trainData$Data$X,
                                     Ytrain = trainData$Data$Y,
                                     flexibility = flex,
                                     Xtest = XTest)
            
            YTestPredictMatrix[, nModel] <- modelResult
            
            if(nModel == 1){
                YpredTrain <- modelFun(  Xtrain = trainData$Data$X,
                                         Ytrain = trainData$Data$Y,
                                         flexibility = flex,
                                         Xtest = trainData$Data$X)
                
                trainDataSample[[flex]] <- data.frame(X = trainData$Data$X,
                                                      Y = trainData$Data$Y,
                                                      Ypred = YpredTrain)
                
                testDataSample[[flex]] <- data.frame(  X = XTest,
                                                       Y = modelResult)
            }
        }
        
        meanMSE <- mean( apply(YTestPredictMatrix, 2, function(x){
            mean( (x - YTestExpected) ** 2 )
        }))
        meanBias <- mean( (YTestExpected - apply(YTestPredictMatrix, 1, mean)) ** 2 )
        meanVariance <- mean( apply(YTestPredictMatrix, 1, var) )
        meanErrorVar <- mean(noiseVar)
        
        resultDF <- resultDF %>%
            dplyr::bind_rows(
                data.frame(MSE = meanMSE,
                           Bias = meanBias,
                           Variance = meanVariance,
                           erroVar = meanErrorVar,
                           Flexibility = flex)
            )
        
    }
    
    
    list(dataOrigin = testData$Data,
         trainSample = trainDataSample,
         testSample = testDataSample,
         meanMSETest = resultDF$MSE,
         meanBias = resultDF$Bias,
         meanVar = resultDF$Variance,
         noiseVar = mean(resultDF$erroVar)
    )
}