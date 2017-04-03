generateData <- function(fun, range, nPoints = NULL, by = NULL, noiseRange = 0){
    
    if(is.null(nPoints)){
        if(is.null(by)) by = 0.1
        X <- seq(from = range[1], to = range[2], by = by)
    } else {
        X <- runif(n = nPoints, min = range[1], max = range[2])
    }
    
    Y <- fun(X)
    
    noiseVar <- 0
    if(noiseRange != 0){
        noiseFun <- getNoiseFunc(input = input)
        noise <- ( noiseFun(length(Y), noiseRange))
        noiseVar <- var(noise)
        Y <- Y + noise
    }
    
    list (  Data = data.frame(X = X, Y = Y),
            noiseVar = noiseVar
         )
}