#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage( 
    titlePanel("Sentiment Analysis"), #Title
    textOutput("currentTime"),   #Here, I show a real time clock
    h4("Tweets:"),   #Sidebar title
    sidebarLayout(
        sidebarPanel(
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Plot Klasifikasi", plotOutput("distPlot")), # Plot
                        tabPanel("Hasil Analisis Sentimen", DT::dataTableOutput('hasil')), # Data hasil analisis
                        # tabPanel("Ringkasan Model", verbatimTextOutput("summary")), # output Regresi
                        tabPanel("Word Cloud", plotOutput("wordcloud")), # Word cloud
                        tabPanel("Data", DT::dataTableOutput('tbl')) # Data sebelum analisis
            )
            # plotOutput("distPlot"), #Here I will show the bars graph
            
            #  plotOutput("positive_wordcloud") #Cloud for positive words
            
            
        ))))
