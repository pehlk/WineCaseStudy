library(XML) #xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(magrittr)
library(dplyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(plotly)
library(naniar)
library(stringr)
library(mapproj)
library(class)
library(caret)
library(e1071)

#RSHINY
# Load necessary libraries
library(shiny)
library(caret)

# Define the UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Wine Quality Prediction"),
  
  # Add custom CSS for styling the prediction text
  tags$style(HTML("
    .prediction-text {
      font-size: 30px;
      font-weight: bold;
      color: #FF5733;  /* Orange color */
      text-align: center;
      background-color: #F0F0F0;  /* Light gray background */
      padding: 20px;
      border-radius: 10px;
      border: 2px solid #FF5733;
    }
  ")),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Individual toggle for each input variable
      checkboxInput("toggle_fixed_acidity", "Enable Fixed Acidity", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_fixed_acidity == true",
        numericInput("fixed.acidity", "Fixed Acidity:", value = 7.2, min = 1, max = 20, step = 0.1)
      ),
      
      checkboxInput("toggle_volatile_acidity", "Enable Volatile Acidity", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_volatile_acidity == true",
        numericInput("volatile.acidity", "Volatile Acidity:", value = 0.35, min = 0, max = 1.5, step = 0.1)
      ),
      
      checkboxInput("toggle_citric_acid", "Enable Citric Acid", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_citric_acid == true",
        numericInput("citric.acid", "Citric Acid:", value = .32, min = 0, max = 1.5, step = 0.1)
      ),
      
      checkboxInput("toggle_residual_sugar", "Enable Residual Sugar", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_residual_sugar == true",
        numericInput("residual.sugar", "Residual Sugar:", value = 5.7, min = 0, max = 100, step = 0.1)
      ),
      
      checkboxInput("toggle_chlorides", "Enable Chlorides", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_chlorides == true",
        numericInput("chlorides", "Chlorides:", value = .06, min = 0, max = 0.6, step = 0.01)
      ),
      
      checkboxInput("toggle_free_sulfur_dioxide", "Enable Free Sulfur Dioxide", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_free_sulfur_dioxide == true",
        numericInput("free.sulfur.dioxide", "Free Sulfur Dioxide:", value = 30, min = 1, max = 150, step = 1)
      ),
      
      checkboxInput("toggle_total_sulfur_dioxide", "Enable Total Sulfur Dioxide", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_total_sulfur_dioxide == true",
        numericInput("total.sulfur.dioxide", "Total Sulfur Dioxide:", value = 115, min = 10, max = 350, step = 1)
      ),
      
      checkboxInput("toggle_density", "Enable Density", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_density == true",
        numericInput("density", "Density:", value = 0.99, min = 0.98, max = 1.1, step = 0.01)
      ),
      
      checkboxInput("toggle_pH", "Enable pH", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_pH == true",
        numericInput("pH", "pH:", value = 3.3, min = 2.5, max = 4.5, step = 0.1)
      ),
      
      checkboxInput("toggle_sulphates", "Enable Sulphates", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_sulphates == true",
        numericInput("sulphates", "Sulphates:", value = 0.52, min = 0, max = 2, step = 0.01)
      ),
      
      checkboxInput("toggle_alcohol", "Enable Alcohol", value = TRUE),
      conditionalPanel(
        condition = "input.toggle_alcohol == true",
        numericInput("alcohol", "Alcohol:", value = 10, min = 8, max = 20, step = 0.1)
      ),
      
      # Action button to trigger prediction
      actionButton("predict_btn", "Predict Wine Quality")
    ),
    
    mainPanel(
      h4("Predicted Quality of the Wine:"),
      # Styled predicted wine quality
      div(id = "prediction_display", class = "prediction-text", 
          verbatimTextOutput("prediction"))
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to make predictions when the button is clicked
  observeEvent(input$predict_btn, {
    # Create a data frame with user input values, checking each toggle
    input_data <- data.frame(
      fixed.acidity = if (input$toggle_fixed_acidity) input$fixed.acidity else NA,
      volatile.acidity = if (input$toggle_volatile_acidity) input$volatile.acidity else NA,
      citric.acid = if (input$toggle_citric_acid) input$citric.acid else NA,
      residual.sugar = if (input$toggle_residual_sugar) input$residual.sugar else NA,
      chlorides = if (input$toggle_chlorides) input$chlorides else NA,
      free.sulfur.dioxide = if (input$toggle_free_sulfur_dioxide) input$free.sulfur.dioxide else NA,
      total.sulfur.dioxide = if (input$toggle_total_sulfur_dioxide) input$total.sulfur.dioxide else NA,
      density = if (input$toggle_density) input$density else NA,
      pH = if (input$toggle_pH) input$pH else NA,
      sulphates = if (input$toggle_sulphates) input$sulphates else NA,
      alcohol = if (input$toggle_alcohol) input$alcohol else NA
    )
    
    # Check if any required variables are missing and handle accordingly
    if (any(is.na(input_data))) {
      output$prediction <- renderText({
        "Error: Please ensure all required input fields are filled."
      })
      return()  # Stop further execution if any field is missing
    }
    
    # Make the prediction using the trained model (using preds1 as per your setup)
    preds1 <- predict(model, newdata = input_data)  # Assuming this is how your model works
    
    # We store the prediction in preds2 as per your comment
    preds2 <- preds1
    
    # Calculate the overall mean prediction of the predicted values
    mean_pred <- mean(preds2, na.rm = TRUE)
    
    # Output the predicted quality (overall mean)
    output$prediction <- renderText({
      paste("Predicted Wine Quality (Mean): ", round(mean_pred, 2))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)