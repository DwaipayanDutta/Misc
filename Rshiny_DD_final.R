# Function to install and load required libraries
install_load <- function(packages) {
  # Install missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, 'Package'])]
  if(length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Load all packages
  invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))
}

# Function to build and train the logistic regression model
train_logreg_model <- function(formula, data) {
  logreg_model <- glm(formula, family = binomial(), data = data)
  summary(logreg_model)
  
  # Concordance and Discordance function
  fastConc <- function(model) {
    fitted <- data.frame(cbind(model$y, model$fitted.values))
    colnames(fitted) <- c('respvar', 'score')
    
    ones <- fitted[fitted[, 1] == 1, ]
    zeros <- fitted[fitted[, 1] == 0, ]
    
    pairs_tested <- nrow(ones) * nrow(zeros)
    conc <- sum(ones[,"score"] > zeros[,"score"])
    disc <- sum(ones[,"score"] < zeros[,"score"])
    
    concordance <- conc / pairs_tested
    discordance <- disc / pairs_tested
    ties_perc <- (1 - concordance - discordance)
    
    return(list("Concordance" = concordance, "Discordance" = discordance, "Tied" = ties_perc, "Pairs" = pairs_tested))
  }
  
  return(fastConc(logreg_model))
}

# Function to build and train SVM model
train_svm_model <- function(formula, data) {
  svm_model <- svm(formula, data = data)
  summary(svm_model)
}

# Function to build and train Neural Network model
train_nnet_model <- function(formula, data) {
  nnet_model <- nnet(formula, data = data, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
  summary(nnet_model)
}

# Shiny UI setup
ui <- navbarPage("Model Development by BPSA",
                 tabPanel("Data Import",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file", "Click and Upload your CSV", multiple = FALSE),
                              tags$hr(),
                              h5(helpText("Select the read.table parameters below")),
                              checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                              checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                              radioButtons(inputId = 'sep', label = 'Separator', 
                                           choices = c(Comma = ',', Semicolon = ';', Tab = '\t', Space = ''), selected = ',')
                            ),
                            mainPanel(uiOutput("tb1"))
                          )
                 ),
                 tabPanel("Model Development",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("model_select"),
                              uiOutput("var1_select"),
                              uiOutput("rest_var_select")
                            ),
                            mainPanel(
                              helpText("Your Selected Variables"),
                              verbatimTextOutput("other_val_show")
                            )
                          )
                 )
)

# Shiny Server setup
server <- function(input, output) {
  
  # Reactive data loading
  data <- reactive({
    file1 <- input$file
    if (is.null(file1)) return(NULL)
    read.table(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  })
  
  # Render data table in UI
  output$table <- renderTable({
    if (is.null(data())) return(NULL)
    data()
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  # Dynamic UI for model selection
  output$model_select <- renderUI({
    selectInput("modelselect", "Select Algorithm", choices = c("Logistic Regression" = "logreg", "SVM" = "svm", "Neural Net" = "nnet"))
  })
  
  # Dynamic UI for independent variable selection
  output$var1_select <- renderUI({
    selectInput("ind_var_select", "Select Independent Var", choices = as.list(names(data())), multiple = FALSE)
  })
  
  # Dynamic UI for selecting other variables
  output$rest_var_select <- renderUI({
    checkboxGroupInput("other_var_select", "Select other Var", choices = as.list(names(data())))
  })
  
  # Show model formula and results
  output$other_val_show <- renderPrint({
    f <- data()
    
    # Ensure the model formula is properly created
    if (is.null(f)) return(NULL)
    model_formula <- sprintf("%s ~ %s", input$ind_var_select, paste0(input$other_var_select, collapse = "+"))
    
    print(model_formula)
    
    # Train and display model based on selected algorithm
    if (input$modelselect == "logreg") {
      print(train_logreg_model(as.formula(model_formula), f))
    }
    if (input$modelselect == "svm") {
      train_svm_model(as.formula(model_formula), f)
    }
    if (input$modelselect == "nnet") {
      train_nnet_model(as.formula(model_formula), f)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
