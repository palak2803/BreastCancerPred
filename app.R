library(shiny)
library(shinydashboard)
library(e1071)
library(randomForest)
library(class)

# Load the models
logisticModel <- readRDS("logisticModel.rds")
svmModel <- readRDS("svmModel.rds")
randomForestModel <- readRDS("randomForestModel.rds")
knnModel <- readRDS("knnModel.rds")
modelAccuracies <- readRDS("modelAccuracies.rds")

ui <- dashboardPage(
  dashboardHeader(
    title="Breast Cancer prediction", titleWidth = 650, 
    tags$li(class="dropdown",tags$a(href="https://www.youtube.com/playlist?list=PL6wLL_RojB5xNOhe2OTSd-DPkMLVY9DfB", icon("youtube"), "My Channel", target="_blank")),
    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-safe%C2%AE-5-agilist-csm%C2%AE-5720309" ,icon("linkedin"), "My Profile", target="_blank")),
    tags$li(class="dropdown",tags$a(href="https://github.com/aagarw30/R-Shiny-Dashboards/tree/main/USArrestDashboard", icon("github"), "Source Code", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")), 
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")), 
      menuItem("Prediction", tabName = "prediction", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Input Features", status = "primary", solidHeader = TRUE,
                    width = 12, collapsible = TRUE,
                    numericInput("radius_mean", "Radius Mean", value = 14),
                    numericInput("texture_mean", "Texture Mean", value = 20),
                    numericInput("perimeter_mean", "Perimeter Mean", value = 85),
                    numericInput("area_mean", "Area Mean", value = 550),
                    numericInput("smoothness_mean", "Smoothness Mean", value = 0.1),
                    numericInput("compactness_mean", "Compactness Mean", value = 0.1),
                    numericInput("concavity_mean", "Concavity Mean", value = 0.1),
                    numericInput("concave_points_mean", "Concave Points Mean", value = 0.05),
                    numericInput("symmetry_mean", "Symmetry Mean", value = 0.18),
                    numericInput("fractal_dimension_mean", "Fractal Dimension Mean", value = 0.06),
                    actionButton("predict", "Predict")
                )
              ),
              fluidRow(
                valueBoxOutput("logisticBox"),
                valueBoxOutput("svmBox"),
                valueBoxOutput("randomForestBox"),
                valueBoxOutput("knnBox")
              )
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    inputData <- data.frame(
      radius_mean = as.numeric(input$radius_mean),
      texture_mean = as.numeric(input$texture_mean),
      perimeter_mean = as.numeric(input$perimeter_mean),
      area_mean = as.numeric(input$area_mean),
      smoothness_mean = as.numeric(input$smoothness_mean),
      compactness_mean = as.numeric(input$compactness_mean),
      concavity_mean = as.numeric(input$concavity_mean),
      concave_points_mean = as.numeric(input$concave_points_mean),
      symmetry_mean = as.numeric(input$symmetry_mean),
      fractal_dimension_mean = as.numeric(input$fractal_dimension_mean)
    )
    
    # Make predictions
    logisticPred <- predict(logisticModel, newdata = inputData, type = "response")
    logisticPred <- ifelse(logisticPred > 0.5, "Malignant", "Benign")
    
    svmPred <- predict(svmModel, newdata = inputData, type = "response")
    svmPred <- ifelse(svmPred == 1, "Malignant", "Benign")
    
    randomForestPred <- predict(randomForestModel, newdata = inputData, type = "response")
    randomForestPred <- ifelse(randomForestPred == 1, "Malignant", "Benign")
    
    knnPred <- predict(knnModel, newdata = inputData, type = "class")
    knnPred <- ifelse(knnPred == 1, "Malignant", "Benign")
    
    # Update UI
    output$logisticBox <- renderValueBox({
      valueBox(
        "Logistic Regression",
        HTML(paste("<div style='font-size: 18px;'>Prediction: <b>", logisticPred, "</b>","<br>Accuracy: <b>", sprintf("%.2f%%", modelAccuracies["logistic"] * 100), "</b> </div>")),
        icon = icon("dna"),
        color = "green"
      )
    })
    output$svmBox <- renderValueBox({
      valueBox(
        "SVM",
        HTML(paste("<div style='font-size: 18px;'>Prediction: <b>", svmPred, "</b>","<br>Accuracy: <b>", sprintf("%.2f%%", modelAccuracies["svm"] * 100), "</b> </div>")),
        icon = icon("microscope"),
        color = "blue"
      )
    })
    output$randomForestBox <- renderValueBox({
      valueBox(
        "Random Forest",
        HTML(paste("<div style='font-size: 18px;'>Prediction: <b>", logisticPred, "</b>","<br>Accuracy: <b>", sprintf("%.2f%%", modelAccuracies["logistic"] * 100), "</b> </div>")),
        icon = icon("leaf"),
        color = "red"
      )
    })
    output$knnBox <- renderValueBox({
      valueBox(
        "KNN",
        HTML(paste("Prediction: <b>", knnPred, "</b>","<br>Accuracy: <b>", sprintf("%.2f%%", modelAccuracies["knn"] * 100), "</b> </div>")),
        icon = icon("users"),
        color = "yellow"
      )
    })
  })
}

shinyApp(ui, server)
