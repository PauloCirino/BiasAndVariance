
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



library(shiny)

shinyServer(function(input, output) {
    
    reactiveRefresh <- reactive({   
        input$refreshButton
        shiny::isolate(reactiveGenerateAll(input = input))
    })
    
    output$plotData <- plotly::renderPlotly({ 
        reactiveResults <- reactiveRefresh()
        
        dataOrigin <- reactiveResults$dataOrigin
        dataNoise <- reactiveResults$trainSample[[1]]
        
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Y',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Fx,
                                 type = 'scatter', mode = 'lines',
                                 name = 'F(x)',
                                 line = list(color = 'rgba(38, 91, 181, 0.89)',
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
        dataNoise <- reactiveResults$trainSample[[1]]
        modelsResults <- reactiveResults$trainSample
        
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Function',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Fx,
                                 type = 'scatter', mode = 'lines',
                                 name = 'F(x)',
                                 line = list(color = 'rgba(38, 91, 181, 0.89)',
                                             width = 4)) %>%
            plotly::add_markers(data = dataNoise,
                                x = ~X, y = ~Y,
                                name = 'Train Data',
                                marker = list(size = 5,
                                              color = 'rgba(109, 237, 109, .9)',
                                              line = list(color = 'rgba(109, 109, 109, 1)',
                                                          width = 2)))
        
        for(i in 1:length(modelsResults)){
            p <- p %>%
                plotly::add_lines(data = dataNoise,
                                  x = ~X, y = modelsResults[[i]]$Ypred,
                                  name = paste('Flex.', i + input$modelFlexibility[1] - 1),
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
        modelsResults <- reactiveResults$testSample
       
        p <- plotly::plot_ly() %>%
            plotly::add_trace(   data = dataOrigin,
                                 x = ~X, y = ~Y,
                                 type = 'scatter', mode = 'lines',
                                 name = 'Y',
                                 line = list(color = 'rgba(235, 19, 19, 0.88)',
                                             width = 4)) 
            
        for(i in 1:length(modelsResults)){
            p <- p %>%
                plotly::add_lines(data = dataOrigin,
                                  x = ~X, y = modelsResults[[i]]$Y,
                                  name = paste('Flex.',
                                               i + input$modelFlexibility[1] - 1),
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
            plotly::add_trace(x = reactiveResults$modelFlexibility[1] : reactiveResults$modelFlexibility[2],
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
            plotly::add_trace(x = reactiveResults$modelFlexibility[1] : reactiveResults$modelFlexibility[2],
                              y = reactiveResults$meanBias,
                              name = 'Bias2',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 3)
                              ) %>%
            plotly::add_trace(x = reactiveResults$modelFlexibility[1] : reactiveResults$modelFlexibility[2],
                              y = reactiveResults$meanVar,
                              name = 'Variance',
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(size = 10,
                                            line = list(color = 'rgba(0, 0, 0, .8)',
                                                        width = 3)),
                              line = list(width = 3)
                              ) %>%
            plotly::add_trace(x = reactiveResults$modelFlexibility[1] : reactiveResults$modelFlexibility[2],
                              y = rep(reactiveResults$noiseVar,
                                  length(reactiveResults$meanVar)),
                              name = 'Noise Variance',
                              type = 'scatter',
                              mode = 'lines',
                              line = list(width = 1)
                              ) %>%
            plotly::layout(xaxis = list(title = "Flexibility"),
                           yaxis = list (title = "Error",
                                         rangemode = "tozero")
                           )
        
        p
    })
})
