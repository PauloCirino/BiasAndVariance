getNoiseFunc <- function(input){
    
    noiseDistribution  <- 'Normal'
    
    f <- switch (noiseDistribution,
                 'Normal' = function(n, sd) {rnorm(n = n, sd = sd)},
                 'Exponential' = rexp,
                 'Log Normal' = rlnorm, 
                 'Uniform' = runif,
                 'Cauchy' = rcauchy
    )
    
    f
}