shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Meng Gao"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("bagEarth Model",
                        verbatimTextOutput(outputId = "bagEarthModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bagEarthPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "bagEarthPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bagEarthGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bagEarthGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bagEarthMetrics"),
                        hr(),
                        plotOutput(outputId = "bagEarthModelPlots"),
                        verbatimTextOutput(outputId = "bagEarthRecipe"),
                        verbatimTextOutput(outputId = "bagEarthModelSummary2")
               ),
               tabPanel("bagEarthGCV Model",
                        verbatimTextOutput(outputId = "bagEarthGCVModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bagEarthGCVPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "bagEarthGCVPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bagEarthGCVGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bagEarthGCVGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bagEarthGCVMetrics"),
                        hr(),
                        #plotOutput(outputId = "bagEarthGCVModelPlots"),
                        verbatimTextOutput(outputId = "bagEarthGCVRecipe"),
                        verbatimTextOutput(outputId = "bagEarthGCVModelSummary2")
               ),
               tabPanel("gcvEarth Model",
                        verbatimTextOutput(outputId = "gcvEarthModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gcvEarthPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "gcvEarthPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gcvEarthGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gcvEarthGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gcvEarthMetrics"),
                        hr(),
                        #plotOutput(outputId = "gcvEarthModelPlots"),
                        verbatimTextOutput(outputId = "gcvEarthRecipe"),
                        verbatimTextOutput(outputId = "gcvEarthModelSummary2")
               ),
               tabPanel("glmboost Model",
                        verbatimTextOutput(outputId = "glmboostModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "glmboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "glmboostPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "glmboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "glmboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "glmboostMetrics"),
                        hr(),
                        plotOutput(outputId = "glmboostModelPlots"),
                        verbatimTextOutput(outputId = "glmboostRecipe"),
                        verbatimTextOutput(outputId = "glmboostModelSummary2")
               ),
               tabPanel("Cubist Model",
                          verbatimTextOutput(outputId = "CubistModelSummary0"),
                          fluidRow(
                            column(width = 4, 
                                   selectizeInput(inputId = "CubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                   bsTooltip(id = "CubistPreprocess", 
                                             title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                            ),
                            column(width = 1, 
                                   actionButton(inputId = "CubistGo", label = "Train", icon = icon("play")),
                                   bsTooltip(id = "CubistGo", title = "This will train or retrain your model")
                            )
                          ),
                          hr(),
                          h3("Resampled performance:"),
                          tableOutput(outputId = "CubistMetrics"),
                          hr(),
                          plotOutput(outputId = "CubistModelPlots"),
                          #plotOutput(outputId = "CubistModelTree"),
                          verbatimTextOutput(outputId = "CubistRecipe"),
                          verbatimTextOutput(outputId = "CubistModelSummary2")
               ),
               tabPanel("BstLm Model",
                        verbatimTextOutput(outputId = "BstLmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "BstLmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale")),
                                 bsTooltip(id = "BstLmPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "BstLmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "BstLmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "BstLmMetrics"),
                        hr(),
                        plotOutput(outputId = "BstLmModelPlots"),
                        #plotOutput(outputId = "BstLmModelTree"),
                        verbatimTextOutput(outputId = "BstLmRecipe"),
                        verbatimTextOutput(outputId = "BstLmModelSummary2")
               ),
               tabPanel("xgbLinear Model",
                        verbatimTextOutput(outputId = "xgbLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "xgbLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "xgbLinearPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "xgbLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "xgbLinearGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "xgbLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "xgbLinearModelPlots"),
                        verbatimTextOutput(outputId = "xgbLinearRecipe"),
                        verbatimTextOutput(outputId = "xgbLinearModelSummary2")
               ),
               tabPanel("cforest Model",
                        verbatimTextOutput(outputId = "cforestModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "cforestPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "cforestPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "cforestGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "cforestGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "cforestMetrics"),
                        hr(),
                        plotOutput(outputId = "cforestModelPlots"),
                        verbatimTextOutput(outputId = "cforestRecipe"),
                        verbatimTextOutput(outputId = "cforestModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("widekernalPLS Model",
                        verbatimTextOutput(outputId = "widekernelplsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "widekernelplsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "widekernelplsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "widekernelplsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "widekernelplsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "widekernelplsMetrics"),
                        hr(),
                        plotOutput(outputId = "widekernelplsModelPlots"),
                        verbatimTextOutput(outputId = "widekernelplsRecipe"),
                        verbatimTextOutput(outputId = "widekernelplsModelSummary2")
               ),
               tabPanel("SimPLS Model",
                          verbatimTextOutput(outputId = "simplsModelSummary0"),
                          fluidRow(
                            column(width = 4, 
                                   selectizeInput(inputId = "simplsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                   bsTooltip(id = "simplsPreprocess", 
                                             title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                            ),
                            column(width = 1, 
                                   actionButton(inputId = "simplsGo", label = "Train", icon = icon("play")),
                                   bsTooltip(id = "simplsGo", title = "This will train or retrain your model")
                            )
                          ),
                          hr(),
                          h3("Resampled performance:"),
                          tableOutput(outputId = "simplsMetrics"),
                          hr(),
                          plotOutput(outputId = "simplsModelPlots"),
                          verbatimTextOutput(outputId = "simplsRecipe"),
                          verbatimTextOutput(outputId = "simplsModelSummary2")
               ),
               tabPanel("foba Model",
                        verbatimTextOutput(outputId = "fobaModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "fobaPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "fobaPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "fobaGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "fobaGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "fobaMetrics"),
                        hr(),
                        #plotOutput(outputId = "fobaModelPlots"),
                        verbatimTextOutput(outputId = "fobaRecipe"),
                        verbatimTextOutput(outputId = "fobaModelSummary2")
               ),
               tabPanel("gaussprPoly Model",
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                                 bsTooltip(id = "gaussprPolyPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprPolyModelPlots"),
                        verbatimTextOutput(outputId = "gaussprPolyRecipe"),
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary2")
               ),
               tabPanel("gaussprRadial Model",
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale")),
                                 bsTooltip(id = "gaussprRadialPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprRadialModelPlots"),
                        #plotOutput(outputId = "BstLmModelTree"),
                        verbatimTextOutput(outputId = "gaussprRadialRecipe"),
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary2")
               ),
               tabPanel("GaussprLinear Model",
                        verbatimTextOutput(outputId = "gaussprLinearModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale")),
                                 bsTooltip(id = "gaussprLinearPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprLinearGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprLinearGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprLinearMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprLinearModelPlots"),
                        #plotOutput(outputId = "BstLmModelTree"),
                        verbatimTextOutput(outputId = "gaussprLinearRecipe"),
                        verbatimTextOutput(outputId = "gaussprLinearModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               )
######################################################### maintenance point ####################################################
               
             )
             ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))

