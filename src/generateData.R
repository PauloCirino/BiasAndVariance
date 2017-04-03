generateData <- function(input){
    
    funInput <- input$funInput
    range <- input$range
    BY <- 0.01
    
    fun <- eval( parse( text = paste( 'function(x){ ',
                                      funInput,
                                      ' } ',
                                      sep = '')))
    
    X <- seq(from = range[1], to = range[2], 
             by = BY
             )

    Y <- fun(X)
    Y <- Y
    
    data.frame(X = X, Y = Y)
}