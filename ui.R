#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
y = c(1,2,3,4,5)
# Define UI for application that draws a histogram
shinyUI(fluidPage( 
    titlePanel("Analisis Sentimen Tweet Corona Virus USA"), #Title
    textOutput("currentTime"),   #Here, I show a real time clock
    h4("Tweets:"),   #Sidebar title
    sidebarLayout(
        sidebarPanel(
            selectInput("topic", label = h3("Topik"),
                        choices = list("Topik" = y), selected = 1),
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Tweet Berdasarkan Topik", plotOutput("topic")), # Plot
                        tabPanel("Kata kunci", plotOutput('kata')), # Data hasil analisis
                        tabPanel("Plot Tweet", plotOutput("plot")), # BarPlot
                        tabPanel("Data Klasifikasi", DT::dataTableOutput('hasil')), # Data sebelum analisis
                        tabPanel("WordCloud", plotOutput('wordcloud')) # Word cloud
            )
            # plotOutput("distPlot"), #Here I will show the bars graph
            
            #  plotOutput("positive_wordcloud") #Cloud for positive words
            
            
        ))))
