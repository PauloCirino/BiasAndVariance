
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# 
# funInput <- input$funInput
# range <- input$range
# numPoints <- input$numPoints
# noiseRange <- input$noiseRange
# noiseDistribution  <- input$noiseDistribution
# model <- input$model
# modelFlexibility <- input$modelFlexibility



library(shiny)

shinyServer(function(input, output) {
    
    reactiveRefresh <- reactive({ 
        input$refreshButton
        shiny::isolate(reactiveGenerateAll(input = input))
    })
    
    output$plotData <- plotly::renderPlotly({ 
        reactiveResults <- reactiveRefresh()
        
        dataOrigin <- reactiveResults$dataOrigin
        dataNoise <- reactiveResults$dataNoiseTrain
        
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Function',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) %>%
            plotly::add_markers(data = dataNoise,
                                x = ~X, y = ~Y,
                                name = 'Train Data',
                                marker = list(size = 5,
                                              color = 'rgba(109, 237, 109, .9)',
                                              line = list(color = 'rgba(109, 109, 109, 1)',
                                                          width = 2)))
        p
    })
    
    output$plotModelFitsTrain <- plotly::renderPlotly({ 
        reactiveResults <- reactiveRefresh()
        
        dataOrigin <- reactiveResults$dataOrigin
        dataNoiseTrain <- reactiveResults$dataNoiseTrain
        modelsResults <- reactiveResults$modelsResults
        
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Function',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) %>%
            plotly::add_markers(data = dataNoiseTrain,
                                x = ~X, y = ~Y,
                                name = 'Train Data',
                                marker = list(size = 5,
                                              color = 'rgba(109, 237, 109, .9)',
                                              line = list(color = 'rgba(109, 109, 109, 1)',
                                                          width = 2)))
        
        for(i in 1:length(modelsResults)){
            p <- p %>%
                plotly::add_lines(data = dataNoiseTrain,
                                  x = ~X, y = modelsResults[[i]]$fittedTrain,
                                  name = paste('Flex.', modelsResults[[i]]$flexibility),
                                  type = 'scatter', mode = 'lines',
                                  line = list(width = 1.5),
                                  inherit = FALSE
                )
        }
            
        p
    })
    
    output$plotModelFitsTest <- plotly::renderPlotly({ 
        reactiveResults <- reactiveRefresh()
        
        dataOrigin <- reactiveResults$dataOrigin
        dataNoiseTrain <- reactiveResults$dataNoiseTrain
        modelsResults <- reactiveResults$modelsResults
       
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Function',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) 
            
        for(i in 1:length(modelsResults)){
            p <- p %>%
                plotly::add_lines(data = dataOrigin,
                                  x = ~X, y = modelsResults[[i]]$fittedTest,
                                  name = paste('Flex.',
                                               modelsResults[[i]]$flexibility),
                                  type = 'scatter', mode = 'lines',
                                  line = list(width = 1.5),
                                  inherit = FALSE
                )
        }
        
        p
    })
    
    output$plotViesVsVariance <- plotly::renderPlotly({ 
        reactiveResults <- reactiveRefresh()
        p <- plotly::plot_ly() %>%
            plotly::add_trace(x = 1:length(reactiveResults$meanMSETest),
                              y = reactiveResults$meanMSETest,
                              name = 'MSE',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 5,
                                          dash = 'dash')
                              ) %>%
            plotly::add_trace(x = 1:length(reactiveResults$meanBias),
                              y = reactiveResults$meanBias,
                              name = 'Bias2',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 3)
                              ) %>%
            plotly::add_trace(x = 1:length(reactiveResults$meanVar),
                              y = reactiveResults$meanVar,
                              name = 'Variance',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 3)
                              ) %>%
            plotly::add_trace(x = 1:length(reactiveResults$meanVar),
                              y = rep(reactiveResults$noise,
                                  length(reactiveResults$meanVar)),
                              name = 'Noise',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 5,
                                          dash = 'dash')
                              ) %>%
            plotly::layout(xaxis = list(title = "Error"),
                           yaxis = list (title = "Flexibility",
                                         rangemode = "tozero")
                           )
        
        p
    })
})