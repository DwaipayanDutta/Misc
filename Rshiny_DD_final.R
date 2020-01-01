# Check for libraries
install_load <- function (x)  {   
  
  # convert arguments to vector
  packages <- c(x)
  
  # start loop to determine if each package is installed
  for(x in packages){
    
    # if package is installed locally, load
    if(x %in% rownames(installed.packages()))
      do.call('library', list(x))
    
    # if package is not installed locally, download, then load
    else {
      install.packages(x)
      do.call("library", list(x))
    }
  } 
}

x<- c("shiny","caret","nnet","e1071")
install_load(x)
library(shiny)

ui<-navbarPage("Model Developement by BPSA",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Click and Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               tabPanel("Model Development",
                        sidebarLayout(sidebarPanel(
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"))))
)
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$tb1 <- renderUI({
  tableOutput("table")
})


output$model_select<-renderUI({
  
 selectInput("modelselect","Select Algorithm ",choices = c("Logistic Regression"="logreg","SVM"="svm", "Nural Net"="nnet"))
    })

output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Independent Var", choices =as.list(names(data())),multiple = FALSE)
})
output$rest_var_select<-renderUI({
  checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(data())))
})
output$other_val_show<-renderPrint({
  input$other_var_select
  input$ind_var_select
  f<-data()
  
  
  
  if(input$modelselect == "logreg"){
  form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  print(form)
  logreg <-glm(as.formula(form),family=binomial(),data=f)
    print(summary(logreg))
    fastConc<-function(model){
      fitted<-data.frame(cbind(model$y,model$fitted.values))
      colnames(fitted)<-c('respvar','score')
      ones<-fitted[fitted[,1]==1,]
      # Subset only zeros
      zeros<-fitted[fitted[,1]==0,]
      # Initialise all the values
      pairs_tested<-nrow(ones)*nrow(zeros)
      conc<-0
      disc<-0
      # Get the values 
      for(i in 1:nrow(ones))
      {
        conc<-conc + sum(ones[i,"score"]>zeros[,"score"])
        disc<-disc + sum(ones[i,"score"]<zeros[,"score"])
      }
      # Calculate concordance, discordance and ties
      concordance<-conc/pairs_tested
      discordance<-disc/pairs_tested
      ties_perc<-(1-concordance-discordance)
      return(list("Concordance"=concordance,
                  "Discordance"=discordance,
                  "Tied"=ties_perc,
                  "Pairs"=pairs_tested))
    }
    
    fastConc(logreg) 
      }
  if(input$modelselect == "svm"){
  library("e1071")
  form1 <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  print(form1)
  svm <-svm(as.formula(form1),data=f)
  print(summary(svm))}

  library(nnet) 
  if(input$modelselect == "nnet"){
   form1 <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  print(form1)
  nnet <-nnet(as.formula(form1),data=f, size = 2, rang = 0.1,
              decay = 5e-4, maxit = 200)
  print(summary(nnet))
  
}
})

}
shinyApp(ui=ui,server=server)
