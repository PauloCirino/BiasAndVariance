getNoiseFunc <- function(input){
    
    noiseDistribution  <- input$noiseDistribution
    
    f <- switch (noiseDistribution,
                 'Normal' = rnorm,
                 'Exponential' = rexp,
                 'Log Normal' = rlnorm, 
                 'Uniform' = runif,
                 'Cauchy' = rcauchy
    )
    
    f
}