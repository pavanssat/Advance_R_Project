#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Here we created the ShinyApp to explore the Heart Disease Data by its attributes
# Loading of libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(GGally)

#load dataset
data_heart <- read.csv("processed.cleveland.data", stringsAsFactors = FALSE)
#creating vector for the column names
datacolumnnames <- c("Age",
                     "Sex",
                     "Chest.Pain.Type",
                     "Resting.Blood.Pressure",
                     "Serum.Cholesterol",
                     "Fasting.Blood.Sugar",
                     "Resting.ECG",
                     "Max.Heart.Rate.Achieved",
                     "Exercise.Induced.Angina",
                     "ST.Depression.Exercise",
                     "Peak.Exercise.ST.Segment",
                     "Num.Major.Vessels.Flouro",
                     "Thalassemia",
                     "Diagnosis.Heart.Disease")
#combining column names with the dataset
colnames(data_heart) <- datacolumnnames

#creating ShinyApp User interface
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis (EDA) of heart disease and its most and least correlated attributes"),
  #create sidebar of the application
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageInput", "Select Age Range", 30, 100, c(62, 65)), #applying filters for the age variables
      uiOutput("sexOutput"), #creating dropdown for the sex data
      uiOutput("rbpOutput"), #creating dropdown for the resting blood pressure data
      uiOutput("fbsOutput") #creating dropdown for the fasting blood sugar data

    ),
    #create the main content of the application
    mainPanel(
      plotOutput("plot_new"), #ploting the correlation matrix
      br(), br(),
      plotOutput("plot"), #plotting the histogram 
      br(), br(),
      tableOutput("results")
    )
  )
)
#creating ShinyApp server
server <- function(input, output) {
  #assigning sex data to the dropdown menu
  output$sexOutput <- renderUI({
    selectInput("sexInput", "Choose Sex (1 - Male , 0 - Female)",
                sort(unique(data_heart$Sex)),
                selected = "0")
  }) 
  #assigning resting blood pressure data to the dropdown menu
  output$rbpOutput <- renderUI({
    selectInput("rbpInput", "Select Resting Blood Pressure",
                sort(unique(data_heart$Resting.Blood.Pressure)),
                selected = "140")
  }) 
  
  #assigning fasting blood sugar data to the dropdown menu
  output$fbsOutput <- renderUI({
    selectInput("fbsInput", "Select Fasting Blood Sugar (0 - False, 1 - True)",
                sort(unique(data_heart$Fasting.Blood.Sugar)),
                selected = "0.00")
  }) 

  #filtering the data of age
  filtered <- reactive({
    if (is.null(input$ageInput)) {
      return(NULL)
    }    
    
    #creation of the table filtered by the the user choices attributes
    data_heart %>%
      filter(Age >= input$ageInput[1],
             Age <= input$ageInput[2],
             Sex == input$sexInput,
             Resting.Blood.Pressure == input$rbpInput,
             Fasting.Blood.Sugar == input$fbsInput
      )
  })
  #plotting of histogram based on the age filtered by the the user choices attributes
  output$plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Age)) +
      geom_histogram()
  })
  
  #plotting of correlation matrices based filtered data
  output$plot_new <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    data_heart %>% ggcorr(high = "#20a486ff",
                         low        = "#fde725ff",
                         label      = TRUE, 
                         hjust      = .75, 
                         size       = 5, 
                         label_size = 3,
                         nbreaks    = 5) +
      labs(title = "Heart Disease Data Correlation Matrix",
           subtitle = "Pairwise Obervations using Pearson Method")
  })
  
  #rendering the table
  output$results <- renderTable({
    filtered()
  })
}

# Running of the application 
shinyApp(ui = ui, server = server)
