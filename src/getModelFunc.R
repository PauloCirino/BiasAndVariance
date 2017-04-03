# 'Regression Tree' 'Neural Network' 'Gradient Boosting'

getModelFunc <- function(input){
    model <- input$model
    fun <- switch (model,
                    'KNN' = callKNNFun(),
                    'SVM' = callSVNFun(),
                    'Polinomial Regression' = callPolinomialRegressionModel(),
                    'Neural Network' = callNeuralNetwork(),
                    'Gradient Boosting' = callGradientBoosting()
            )
    
    eval(fun)

}

callKNNFun <- function(){
    require('FNN')
    function(Xtrain, Ytrain, flexibility, Xtest) {
        fittedTrain <- FNN::knn.reg(train = as.matrix(Xtrain),
                                    test = as.matrix(Xtrain),
                                    y = Ytrain,
                                    k = flexibility,
                                    algorithm = "kd_tree")
        
        fittedTest <- FNN::knn.reg(train = as.matrix(Xtrain),
                                    test = as.matrix(Xtest),
                                    y = Ytrain,
                                    k = flexibility,
                                    algorithm = "kd_tree")
        
        
        list(fittedTrain = fittedTrain$pred,
             fittedTest = fittedTest$pred)
        }
}


callSVNFun <- function(){
    require('e1071')
    function(Xtrain, Ytrain, flexibility, Xtest) {
        model <- e1071::svm( x = as.numeric(Xtrain),
                             y = Ytrain,
                             kernel = "polynomial",
                             type = "eps-regression",
                             degree = flexibility)
        
        fittedTrain <- predict(model, as.numeric(Xtrain))
        fittedTest <- predict(model, as.numeric(Xtest))
        
        list(fittedTrain = fittedTrain,
             fittedTest = fittedTest)
    }
}


callGradientBoosting <- function(){
    require('xgboost')
    function(Xtrain, Ytrain, flexibility, Xtest) {
        params <- list(booster = 'gbtree',
                       objective = 'reg:linear',
                       silent = FALSE,
                       max_depth = flexibility)
        
        
        model <- xgboost::xgboost( params = params,
                                   data = matrix(as.numeric(Xtrain)),
                                   label = as.numeric(Ytrain),
                                   nrounds = 50
                                   )
        
        fittedTrain <- predict(model, matrix(as.numeric(Xtrain)))
        fittedTest <- predict(model, matrix(as.numeric(Xtest)))
        
        list(fittedTrain = fittedTrain,
             fittedTest = fittedTest)
    }
}
# 
# callNeuralNetwork <- function(){
#     require('RSNNS')
#     function(Xtrain, Ytrain, flexibility, Xtest) {
#         model <- RSNNS::mlp( x = as.matrix(Xtrain),
#                              y = as.matrix(Ytrain),
#                              size = c(flexibility, flexibility),
#                              learnFunc="Rprop",
#                              linOut=TRUE
#         )
#         
#         fittedTrain <- as.numeric( predict(model, as.matrix(Xtrain)) )
#         fittedTest <- as.numeric( predict( model, as.matrix(Xtest) ) )
#         
#         list(fittedTrain = fittedTrain,
#              fittedTest = fittedTest,
#              model = model)
#     }
# }



callNeuralNetwork <- function(){
    require('nnet')
    function(Xtrain, Ytrain, flexibility, Xtest) {
        model <- nnet::nnet( x = as.matrix(Xtrain),
                             y = as.matrix(Ytrain),
                             size = flexibility,
                             maxit = 1000,
                             linout = TRUE
        )
        
        fittedTrain <- as.numeric( predict(model, as.matrix(Xtrain)) )
        fittedTest <- as.numeric( predict( model, as.matrix(Xtest) ) )
        
        list(fittedTrain = fittedTrain,
             fittedTest = fittedTest)
    }
}

callPolinomialRegressionModel <- function(){
    function(Xtrain, Ytrain, flexibility, Xtest){
        trainDF <- data.frame(Y = Ytrain, X = Xtrain)
        testDF <- data.frame(X = Xtest)
        
        model <- lm( Y ~ poly(X, flexibility), data = trainDF )
        
        fittedTrain <- fitted(model)
        fittedTest <- predict(model, testDF)
        
        list(fittedTrain = fittedTrain,
             fittedTest = fittedTest)
    }
}