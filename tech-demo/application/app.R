#### load packages ####
library(shiny)
library(tidyverse)
library(TDboost)

#### load model and data ####
load("~/Documents/stats-projects/ness-teacher-talk/models/tweediegbm.RData")
overallrate <- mean(df$tweedie_loss_cost) # typical loss cost amount

#### shiny application ####
library(shiny)

ui <- fluidPage(
  
  # App Name
  headerPanel("Auto Insurance Loss Cost Predictions"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Model Inputs
    sidebarPanel(
      
      # Vehicle body
      selectInput(inputId = "VehBody", 
                  label = "Vehicle Body:",
                  choices = c("bus","cabriolet","coupe","microvan","other microvan","sedan","sport utility vehicle","station wagon","van"), 
                  selected = "sedan"),
      
      # Vehicle Age
      sliderInput(inputId = "VehAge", 
                  label = "Vehicle Age:",
                  min = 0,
                  max = 8,
                  step = 1,
                  value = 0,
                  ticks = TRUE),
      
      # Vehicle Usage
      selectInput(inputId = "VehUsage", 
                  label = "Vehicle Usage:",
                  choices = c("Private","Private+trip to office","Professional","Professional run"), 
                  selected = "Private"),
      
      # Vehicle Max Speed
      selectInput(inputId = "VehMaxSpeed", 
                  label = "Vehicle Max Speed:",
                  choices = c("1-130 km/h", "130-140 km/h", "140-150 km/h", "150-160 km/h", "160-170 km/h", "170-180 km/h", "180-190 km/h", "190-200 km/h", "200-220 km/h", "220+ km/h"), 
                  selected = "1-130 km/h"),
      
      # Driver Gender
      selectInput(inputId = "Gender", 
                  label = "Driver Gender:",
                  choices = c("Female","Male"), 
                  selected = "Male"),
      
      # Driver Age
      sliderInput(inputId = "DrivAge", 
                  label = "Driver Age:",
                  min = 18,
                  max = 100,
                  step = 1,
                  value = 18,
                  ticks = TRUE),
      
      # License Age
      sliderInput(inputId = "LicAge", 
                  label = "Liscense Age (Mo):",
                  min = 0,
                  max = 940,
                  step = 12,
                  value = 0,
                  ticks = TRUE)
      # ,
      # actionButton("button", "Show")
      
    ),
    
    # Output: Pure Premium
    mainPanel(
      plotOutput(outputId = "plot", width = "100%", height = "650px")
    )
  )
)

server <- function(input, output, session) {
  # set up test data for predictions
  preddat <- reactive(
    {
      xpred <- data.frame(LicAge = as.integer(input$LicAge),
                          VehAge = as.integer(input$VehAge),
                          Gender = factor(input$Gender, levels = levels(df$Gender)),
                          VehUsage = factor(input$VehUsage, levels = levels(df$VehUsage)),
                          DrivAge = as.integer(input$DrivAge),
                          VehBody = factor(input$VehBody, levels = levels(df$VehBody)),
                          VehMaxSpeed = factor(input$VehMaxSpeed, levels = levels(df$VehMaxSpeed)))
      # model predictions
      modpred <- predict(tb, newdata = xpred, n.trees=2000)
      preddat <- data.frame(Group=c("Baseline","Prediction"), Value=c(overallrate,modpred))
      return(preddat)
    }
  )
  # make the plot
  output$plot <- renderPlot({
    ggplot(data=preddat(), aes(x=Group,y=Value)) + 
      geom_col(aes(fill=Group), color="black") +
      scale_fill_manual(values = c("Baseline" = "blue", "Prediction"="cyan")) +
      scale_y_continuous(breaks=seq(0, 5000, 200)) +
      geom_hline(aes(yintercept=overallrate), color="red", linetype=2, size=1) +
      labs(x="",y="Expected Loss Cost", title = "Expected Loss Cost: By-Risk Predictions vs. Reference") + 
      theme_light(base_size = 20) +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)