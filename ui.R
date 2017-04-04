
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

ModelsOpts <- c('KNN', 'SVM', 'Polinomial Regression',
                'Neural Network', 'Gradient Boosting',
                'Radom Forest', 'Regression Tree', 'Spline')

NoiseDistributions <- c('Normal', 'Exponential', 'Log Normal',
                        'Uniform', 'Cauchy')

library(shiny)
library(shinythemes)

require('dplyr')
require('ggplot2')
require('plotly')

require('e1071')
require('FNN')
require('RSNNS')
require('xgboost')

source('./src/utils.R')
source('./src/getUserFun.R')
source('./src/getNoiseFunc.R')
source('./src/getModelFunc.R')
source('./src/generateData.R')
source('./src/reactiveGenerateAll.R')

shinyUI(fluidPage(theme = shinytheme("sandstone"),
                  
    sidebarPanel(
        shiny::textInput(inputId = "funInput",
                         label = "Input a function f(x):",
                         value = "(x - 2) * (2*x + 1) / (1 + x**2)",
                         placeholder = "(x ** 2) - 2x - 5",
                         width = '100%'),
        
        shiny::sliderInput(inputId = "range",
                           label = "Range:",
                           min = -20, max = 20,
                           value = c(-8, 12), 
                           step = 0.1, 
                           animate = TRUE, 
                           width = '100%', 
                           dragRange = TRUE),
        
        shiny::sliderInput(inputId = "numPoints", 
                           label = "Number of Points:",
                           min = 0, max = 500, 
                           value = 40, step = 1,
                           width = '100%', post = ' points',
                           animate = TRUE),
        
        # shiny::selectizeInput(inputId = 'noiseDistribution', 
        #                       label = 'Noise Distribution',
        #                       choices = NoiseDistributions,
        #                       selected = 'Normal', 
        #                       multiple = FALSE),
        
        shiny::sliderInput(inputId = "noiseRange", 
                           label = "Noise Range:",
                           min = 0, max = 5, 
                           value = 0.25, step = 0.05,
                           width = '100%', post = '',
                           animate = TRUE),
        
        shiny::selectizeInput(inputId = 'model', 
                              label = 'Model',
                              choices = ModelsOpts,
                              selected = 'KNN', 
                              multiple = FALSE),
        
        shiny::sliderInput(inputId = "modelFlexibility", 
                           label = "Model Felixibility:",
                           min = 1, max = 50, 
                           value = c(1, 5),
                           step = 1,
                           width = '100%', post = ' degrees',
                           animate = TRUE),
        
        shiny::sliderInput(inputId = "numTrainedModels", 
                           label = "Num. Trained Models:",
                           min = 1, max = 50, 
                           value = 3,
                           step = 1,
                           width = '100%', post = ' models',
                           animate = TRUE),
        #shiny::br(),
        
        actionButton(inputId = "refreshButton",
                     label = "Reload",
                     width = '100%',
                     icon = icon("refresh"))
        
        # shiny::hr(),
        # 
        # actionButton(inputId = "clearPrevPlotsButton",
        #              label = "Clear Previous Plots",
        #              width = '100%'),
        # 
        # 
        # shiny::br(),
        # shiny::br(),
        # 
        # actionButton(inputId = "generateDocButton",
        #              label = "Generate Document",
        #              width = '100%')
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Data", plotly::plotlyOutput("plotData")), 
            tabPanel("Models Fit Train", plotly::plotlyOutput("plotModelFitsTrain")), 
            tabPanel("Models Fit Test", plotly::plotlyOutput("plotModelFitsTest")), 
            tabPanel("Vies vs Variance", plotly::plotlyOutput("plotViesVsVariance"))
        )
        
    )
))