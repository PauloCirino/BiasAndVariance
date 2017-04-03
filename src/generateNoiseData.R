generateNoiseData <- function(input, trainData = TRUE){
    funInput <- input$funInput
    range <- input$range
    numPoints <- input$numPoints
    noiseRange <- input$noiseRange
    noiseFun <- getNoiseFunc(input = input)
    
    fun <- eval( parse( text = paste( 'function(x){ ',
                                      funInput,
                                      ' } ',
                                      sep = '')))
    
     X <- runif(n = numPoints, min = range[1], max = range[2])
    
    Y <- fun(X)
    Y <- Y + ( noiseFun(length(Y), noiseRange))
    
    data.frame(X = X, Y = Y)
}