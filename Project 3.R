#--------------------------------------#
# SDS 313 Project 3 - Shaliha Guarnera #
#--------------------------------------#

library(shiny)
library(tidyverse)
library(dplyr)
library(rvest)
library(sas7bdat)
library(stringr)
library(lubridate)
library(reshape2)
outcomes <- read_csv('Austin_Animal_Center_Outcomes.csv')

#----------------------------------Data Preparation----------------------------------#

#Removing unwanted variables from the dataset
outcomes <- subset(outcomes, select = -c(2,3,7))

#Renaming the remaining variables
names(outcomes)[1:9] <- c('AnimalID', 'DateOfOutcome', 'DateOfBirth', 'OutcomeType', 'Species', 'Sex', 'AgeUponOutcome', 'Breed', 'Color')

#Parsing the 'DateofBirth' variable as a date
outcomes$DateOfBirth <- as.Date(outcomes$DateOfBirth, format="%m/%d/%Y")

#Splitting the 'DateofOutcome' variable to 'Month' and 'Year'
outcomedates <- colsplit(outcomes$DateOfOutcome, " ", names = c("Month", "Year"))
outcomes <- cbind(outcomes, outcomedates)

#Rearranging the variables
outcomes <- subset(outcomes, select = -c(2))
outcomes <- outcomes[, c(1,9,10,2,3,4,5,6,7,8)]

#Removing unwanted observations from the dataset
outcomes <- filter(outcomes, Year >= 2020)

#Parsing the 'Year' variable as a character
outcomes$Year <- as.character(outcomes$Year)

#Parsing the 'AgeUponOutcome' variable so that it is in years using a for loop
outcomes$AnimalAge <- numeric(nrow(outcomes))

for (i in seq_along(outcomes$AgeUponOutcome)){
  x <- outcomes$AgeUponOutcome[i]
  
  if (str_detect(x, 'year')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- x
  }
  else if (str_detect(x, 'years')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- x
  }
  else if (str_detect(x, 'month')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- (x/12)
  }
  else if (str_detect(x, 'months')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- (x/12)
  }
  else if (str_detect(x, 'week')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- (x/52)
  }
  else if (str_detect(x, 'weeks')){
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- (x/52)
  }
  else {
    x <- parse_number(x)
    outcomes$AnimalAge[i] <- (x/365.25)
  }
}
outcomes$AnimalAge <- round(outcomes$AnimalAge, 2)

#Making a new variable of the day an animal was born on
outcomes$DayOfBirth <- day(outcomes$DateOfBirth)

#Removing the old 'AgeUponOutcome' variable, and other unwanted variables
outcomes <- subset(outcomes, select = -c(1,8,9,10))
#Rearranging variables in the dataset
outcomes <- outcomes[, c(1,2,3,8,4,7,5,6)]

#Removing NAs and blank values (NULLs) from the dataset
outcomes <- na.omit(outcomes)

#------------------------------------------------------------------------------------#

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Austin Animal Center Outcomes Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Animal Age"=1, "Outcome Type"=2, "Month"=3, 'Sex of Animals'=4, 'Day of Birth'=5), 
                  selected = 1),
      
      # Radio buttons for display color
      radioButtons("color", label=h3("Select Color of Graph"),
                   choices = c("Pink"='lightpink', "Sea Green"='darkslategray3', "Light Purple"='lavender'), 
                   selected = "lightpink"),
      
      # Slider input for number of bins
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
      
      # Option to show mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      # Option to show sd
      checkboxInput("checkbox2", label="Display standard deviation", value=FALSE),
      
      # Option to show median
      checkboxInput("checkbox3", label="Display median", value=FALSE),
      
      # Option to show five-number summary
      checkboxInput("checkbox4", label="Display five-number summary", value=FALSE),
    ),
    
    # Show a plot of the generated distribution and the descriptive statistics
    mainPanel(
      img(src = "/Users/shali/Documents/dog-picture.jpg"),
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard deviation:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      p('Median:'),
      fluidRow(column(5, verbatimTextOutput("median"))),
      p('Five-Number Summary:'),
      fluidRow(column(5, verbatimTextOutput("fns"))),
    )
  )
)

# Define server logic required to draw a graph for the variable
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar == 1){
      hist(outcomes$AnimalAge, breaks=input$bins, main='Distribution of Animal Age', xlab='Animal Age Upon Outcome', col=input$color)
    }
    else if(input$selectvar == 2){
      barplot(table(outcomes$OutcomeType), main='Animal Outcome Types', xlab='Outcome Type', ylab='Frequency', col=input$color)
    }
    else if(input$selectvar == 3){
      barplot(table(outcomes$Month), main='Animal Outcomes Across Months', xlab='Outcome Type', ylab='Frequency', col=input$color)
    }
    else if(input$selectvar == 4){
      barplot(table(outcomes$Sex), main='Animal Outcomes Across Sex', xlab='Outcome Type', ylab='Frequency', col=input$color)
    }
    else if(input$selectvar == 5){
      hist(outcomes$DayOfBirth, breaks=input$bins, main='Distribution of Day of Birth', xlab='Day of Birth', ylab='Frequency', col=input$color)
    }
  })
  
  
  # Display mean if selected
  output$mean <- renderPrint({ 
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(outcomes$AnimalAge, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 5){
      mean(outcomes$DayofBirth, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE){print("This is not a numeric variable!")}
  })
  
  # Display sd if selected
  output$sd <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      sd(outcomes$AnimalAge, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 5){
      sd(outcomes$DayofBirth, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE){print("This is not a numeric variable!")}
  })
  
  # Display median if selected
  output$median <- renderPrint({ 
    if(input$checkbox3 == TRUE & input$selectvar == 1){
      median(outcomes$AnimalAge, na.rm=TRUE)}
    else if(input$checkbox3 == TRUE & input$selectvar == 5){
      median(outcomes$DayofBirth, na.rm=TRUE)}
    else if(input$checkbox3 == TRUE){print("This is not a numeric variable!")}
  })
  
  # Display five-number summary if selected
  output$fns <- renderPrint({ 
    if(input$checkbox3 == TRUE & input$selectvar == 1){
      fivenum(outcomes$AnimalAge, na.rm=TRUE)}
    else if(input$checkbox4 == TRUE & input$selectvar == 5){
      fivenum(outcomes$DayofBirth, na.rm=TRUE)}
    else if(input$checkbox4 == TRUE){print("This is not a numeric variable!")}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
