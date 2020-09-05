shinyServer(function(input, output, session) {
  
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    models[[name]] <- readRDS(file = rdsfile)
  }

  ############################################################################## 
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
                 index = caret::createResample(y = y, times = n), savePredictions = "final")
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  ############################################################################## 
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  ############################################################ NULL ########################################################
  
  
  
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  ##############################################################################  
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ################################null##############################################  
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  
############################################################ Bagged MARS ########################################################
  
  
  
  
  ##############################################################################  
  getbagEarthRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bagEarthPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$bagEarthGo,
    {
      library(earth)
      method <- "bagEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbagEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$bagEarthSummary0 <- renderText({
    description("bagEarth")
  })

  ##############################################################################  
  output$bagEarthMetrics <- renderTable({
    req(models$bagEarth)
    models$bagEarth$results[ which.min(models$bagEarth$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$bagEarthModelPlots <- renderPlot({
    req(models$bagEarth)
    plot(models$bagEarth)
  })
  
  ############################################################################## 
  output$bagEarthRecipe <- renderPrint({
    req(models$bagEarth)
    models$bagEarth$recipe
  })  
  
  ############################################################################## 
  output$bagEarthModelSummary2 <- renderPrint({
    req(models$bagEarth)
    print(models$bagEarth)
  })
  ############################################################ bagEarthGCV ########################################################
  
  
  
  
  ##############################################################################  
  getbagEarthGCVRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$bagEarthGCVPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$bagEarthGCVGo,
    {
      library(earth)
      method <- "bagEarthGCV"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getbagEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$bagEarthGCVSummary0 <- renderText({
    description("bagEarthGCV")
  })
  
  ##############################################################################  
  output$bagEarthGCVMetrics <- renderTable({
    req(models$bagEarthGCV)
    models$bagEarthGCV$results[ which.min(models$bagEarthGCV$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$bagEarthGCVModelPlots <- renderPlot({
    req(models$bagEarthGCV)
    plot(models$bagEarthGCV)
  })
  
  ############################################################################## 
  output$bagEarthGCVRecipe <- renderPrint({
    req(models$bagEarthGCV)
    models$bagEarthGCV$recipe
  })  
  
  ############################################################################## 
  output$bagEarthGCVModelSummary2 <- renderPrint({
    req(models$bagEarthGCV)
    print(models$bagEarthGCV)
  })
  

  ############################################################ gcvEarth ########################################################
  
  
  
  
  ##############################################################################  
  getgcvEarthRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gcvEarthPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$gcvEarthGo,
    {
      library(earth)
      method <- "gcvEarth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgcvEarthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gcvEarthSummary0 <- renderText({
    description("gcvEarth")
  })
  
  ##############################################################################  
  output$gcvEarthMetrics <- renderTable({
    req(models$gcvEarth)
    models$gcvEarth$results[ which.min(models$gcvEarth$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gcvEarthModelPlots <- renderPlot({
    req(models$gcvEarth)
    plot(models$gcvEarth)
  })
  
  ############################################################################## 
  output$gcvEarthRecipe <- renderPrint({
    req(models$gcvEarth)
    models$gcvEarth$recipe
  })  
  
  ############################################################################## 
  output$gcvEarthModelSummary2 <- renderPrint({
    req(models$gcvEarth)
    print(models$gcvEarth)
  })

  
  ############################################################ glmboost ########################################################
  
  
  
  
  ##############################################################################  
  getglmboostRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$glmboostPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$glmboostGo,
    {
      library(mboost)
      library(plyr)
      method <- "glmboost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getglmboostRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$glmboostSummary0 <- renderText({
    description("glmboost")
  })
  
  ##############################################################################  
  output$glmboostMetrics <- renderTable({
    req(models$glmboost)
    models$glmboost$results[ which.min(models$glmboost$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$glmboostModelPlots <- renderPlot({
    req(models$glmboost)
    plot(models$glmboost)
  })
  
  ############################################################################## 
  output$glmboostRecipe <- renderPrint({
    req(models$glmboost)
    models$glmboost$recipe
  })  
  
  ############################################################################## 
  output$glmboostModelSummary2 <- renderPrint({
    req(models$glmboost)
    print(models$glmboost)
  })
  ############################################################ xgbLinear ########################################################
  
  
  
  
  ##############################################################################  
  getxgbLinearRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$xgbLinearPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$xgbLinearGo,
    {
      library(xgboost)
      method <- "xgbLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getxgbLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$xgbLinearSummary0 <- renderText({
    description("xgbLinear")
  })
  
  ##############################################################################  
  output$xgbLinearMetrics <- renderTable({
    req(models$xgbLinear)
    models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$xgbLinearModelPlots <- renderPlot({
    req(models$xgbLinear)
    plot(models$xgbLinear)
  })
  
  ############################################################################## 
  output$xgbLinearRecipe <- renderPrint({
    req(models$xgbLinear)
    models$xgbLinear$recipe
  })  
  
  ############################################################################## 
  output$xgbLinearModelSummary2 <- renderPrint({
    req(models$xgbLinear)
    print(models$xgbLinear)
  })
  
  ############################################################ cforest ########################################################
  
  
  
  
  ##############################################################################  
  getcforestRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$cforestPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$cforestGo,
    {
      library(party)
      method <- "cforest"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getcforestRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$cforestSummary0 <- renderText({
    description("cforest")
  })
  
  ##############################################################################  
  output$cforestMetrics <- renderTable({
    req(models$cforest)
    models$cforest$results[ which.min(models$cforest$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$cforestModelPlots <- renderPlot({
    req(models$cforest)
    plot(models$cforest)
  })
  
  ############################################################################## 
  output$cforestRecipe <- renderPrint({
    req(models$cforest)
    models$cforest$recipe
  })  
  
  ############################################################################## 
  output$cforestModelSummary2 <- renderPrint({
    req(models$cforest)
    print(models$cforest)
  })
  ############################################################foba########################################################
  
  
  
  
  ##############################################################################  
  getfobaRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$fobaPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$fobaGo,
    {
      library(foba)
      method <- "foba"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getfobaRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$fobaSummary0 <- renderText({
    description("foba")
  })
  
  ##############################################################################  
  output$fobaMetrics <- renderTable({
    req(models$foba)
    models$foba$results[ which.min(models$foba$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$fobaModelPlots <- renderPlot({
    req(models$foba)
    plot(models$foba)
  })
  
  ############################################################################## 
  output$fobaRecipe <- renderPrint({
    req(models$foba)
    models$foba$recipe
  })  
  
  ############################################################################## 
  output$fobaModelSummary2 <- renderPrint({
    req(models$foba)
    print(models$foba)
  })
  ############################################################gaussprPoly########################################################
  
  
  
  
  ##############################################################################  
  getgaussprPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprPolyPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$gaussprPolyGo,
    {
      library(kernlab)
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gaussprPolySummary0 <- renderText({
    description("gaussprPoly")
  })
  
  ##############################################################################  
  output$gaussprPolyMetrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gaussprPolyModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })
  
  ############################################################################## 
  output$gaussprPolyRecipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  ############################################################################## 
  output$gaussprPolyModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    print(models$gaussprPoly)
  })
  ############################################################ GLMNET ########################################################
  
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  ##############################################################################  
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  ############################################################################## 
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })
  
############################################################ PLS ########################################################
  
  
    
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ##############################################################################  
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  ############################################################################## 
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  ############################################################ WIDEKERNELPLS ########################################################
  
  
  
  
  ##############################################################################  
  getwidekernelplsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$widekernelplsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$widekernelplsGo,
    {
      library(pls)
      method <- "widekernelpls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getwidekernelplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$widekernelplsModelSummary0 <- renderText({
    description("widekernelpls")
  })
  
  ##############################################################################  
  output$widekernelplsMetrics <- renderTable({
    req(models$widekernelpls)
    models$widekernelpls$results[ which.min(models$widekernelpls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$widekernelplsModelPlots <- renderPlot({
    req(models$widekernelpls)
    plot(models$widekernelpls)
  })     
  
  ############################################################################## 
  output$widekernelplsRecipe <- renderPrint({
    req(models$widekernelpls)
    models$widekernelpls$recipe
  })  
  
  ############################################################################## 
  output$widekernelplsModelSummary2 <- renderPrint({
    req(models$widekernelpls)
    summary(models$widekernelpls$finalModel)
  })
  ############################################################ SIMPLS ########################################################
  ##############################################################################  
  getsimplsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$simplsPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$simplsGo,
    {
      library(pls)
      method <- "simpls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getsimplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], method)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$simplsModelSummary0 <- renderText({
    description("simpls")
  })
  
  ##############################################################################  
  output$simplsMetrics <- renderTable({
    req(models$simpls)
    models$simpls$results[ which.min(models$simpls$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$simplsModelPlots <- renderPlot({
    req(models$simpls)
    plot(models$simpls)
  })     
  
  ############################################################################## 
  output$simplsRecipe <- renderPrint({
    req(models$simpls)
    models$simpls$recipe
  })  
  
  ############################################################################## 
  output$simplsModelSummary2 <- renderPrint({
    req(models$simpls)
    summary(models$simpls$finalModel)
  })
  
############################################################ RPART ########################################################
  
  
    
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "rpart")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ##############################################################################  
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel)
  })     
  

  ############################################################ CUBIST ########################################################
  
  
  
  
  ##############################################################################  
  getCubistRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$CubistPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$CubistGo,
    {
      library(Cubist)
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getCubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "cubist")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$CubistModelSummary0 <- renderText({
    description("cubist")
  })
  
 
  ##############################################################################  
  output$CubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$CubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })
  
  ############################################################################## 
  output$CubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  ############################################################################## 
  output$CubistModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })

  
  
  
  ############################################################ BstLm ########################################################
  
  
  
  
  ##############################################################################  
  getBstLmRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$BstLmPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$BstLmGo,
    {
      library(bst)
      library(plyr)
      method <- "BstLm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getBstLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "BstLm")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$BstLmModelSummary0 <- renderText({
    description("BstLm")
  })
  
  ##############################################################################  
  output$BstLmMetrics <- renderTable({
    req(models$BstLm)
    models$BstLm$results[ which.min(models$BstLm$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$BstLmRecipe <- renderPrint({
    req(models$BstLm)
    models$BstLm$recipe
  })  
  
  ############################################################################## 
  output$BstLmModelPlots <- renderPlot({
    req(models$BstLm)
    plot(models$BstLm)
  })
  ############################################################################## 
  output$BstLmModelSummary2 <- renderPrint({
    req(models$BstLm)
    summary(models$BstLm$finalModel)
  })
  
  ############################################################ gaussprRadial ########################################################
  
  
  
  
  ##############################################################################  
  getgaussprRadialRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$gaussprRadialPreprocess)
  })
  
  ##############################################################################
  observeEvent(
    input$gaussprRadialGo,
    {
      library(kernlab)
      method <- "gaussprRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        models[[method]] <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(models[[method]], "gaussprRadial")
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  ############################################################################## 
  output$gaussprRadialrModelSummary0 <- renderText({
    description("gaussprRadialr")
  })
  
  ##############################################################################  
  output$gaussprRadialMetrics <- renderTable({
    req(models$gaussprRadial)
    models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  })
  
  ############################################################################## 
  output$gaussprRadialRecipe <- renderPrint({
    req(models$gaussprRadial)
    models$gaussprRadial$recipe
  })  
  
  ############################################################################## 
  output$gaussprRadialModelPlots <- renderPlot({
    req(models$gaussprRadial)
    plot(models$gaussprRadial)
  })
  ############################################################################## 
  output$gaussprRadialModelSummary2 <- renderPrint({
    req(models$gaussprRadial)
    summary(models$gaussprRadial$finalModel)
  })
  ############################################################################## 
 
######################################################### maintenance point ####################################################
  
          
  
  
  
  
  
  
  
#####################################################################################################################  
  
  
    
  
  
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)

    
})
