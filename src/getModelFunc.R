# 'Regression Tree' 'Neural Network' 'Gradient Boosting'

getModelFunc <- function(model, modelFlexibility){
    fun <- switch (model,
                    'KNN' = callKNNFun(modelFlexibility = modelFlexibility),
                    'SVM' = callSVNFun(),
                    'Polinomial Regression' = callPolinomialRegressionModel(),
                    'Neural Network' = callNeuralNetwork(),
                    'Gradient Boosting' = callGradientBoosting(),
                    'Radom Forest' = callRandomForest(),
                    'Regression Tree' = callRegressionTree(),
                    'Spline' = callSpline()
                   
            )
    
    eval(fun)

}

callKNNFun <- function(modelFlexibility){
    require('FNN')
    function(Xtrain, Ytrain, flexibility, Xtest,
             kRange = modelFlexibility[2] : modelFlexibility[1] ) {
        model <- FNN::knn.reg(train = as.matrix(Xtrain),
                              test = as.matrix(Xtest),
                              y = Ytrain,
                              k = kRange[flexibility - min(kRange) + 1],
                              algorithm = "kd_tree")
        
        
        model$pred
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
        
        predict(model, as.numeric(Xtest))
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
                                   nrounds = 25
                                   )
        
        predict(model, matrix(as.numeric(Xtest)))
        
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
        
        as.numeric( predict( model, as.matrix(Xtest) ) )
        
    }
}

callPolinomialRegressionModel <- function(){
    function(Xtrain, Ytrain, flexibility, Xtest){
        trainDF <- data.frame(Y = Ytrain, X = Xtrain)
        testDF <- data.frame(X = Xtest)
        
        model <- lm( Y ~ poly(X, flexibility), data = trainDF )
        
        predict(model, testDF)
    }
}

callRandomForest <- function(){
    require('randomForest')
    function(Xtrain, Ytrain, flexibility, Xtest){
        trainDF <- data.frame(Y = Ytrain, X = Xtrain)
        testDF <- data.frame(X = Xtest)
        
        model <- randomForest( Y ~ X, data = trainDF, ntree = flexibility)
        
        predict(model, testDF)
    }
}

callRegressionTree <- function(){
    require('rpart')
    function(Xtrain, Ytrain, flexibility, Xtest){
        trainDF <- data.frame(Y = Ytrain, X = Xtrain)
        testDF <- data.frame(X = Xtest)
        
        model <- rpart( Y ~ X, data = trainDF,
                        control = rpart.control( maxdepth = flexibility))
        
        predict(model, testDF)
    }
}

callSpline <- function(){
    function(Xtrain, Ytrain, flexibility, Xtest){
        model <- smooth.spline(x = Xtrain, y = Ytrain, df = flexibility)
        predict(model, Xtest)
    }
}