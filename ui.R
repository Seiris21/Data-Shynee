library(shiny)

# Define UI 
shinyUI(fluidPage(
     titlePanel("My Shiny App"),
     sidebarLayout(
          sidebarPanel(
               h1("Options"),
               helpText("Select Intensity"),
               selectInput("yvalue",
                           label="Choose Intensity",
                           choices=c("Total.Intensity.DAPI","Mean.Intensity.DAPI","Mean.Intensity.Alexa.488","Mean.Intensity.Alexa.647","Mean.Intensity.Alexa.555"),
                           selected="Total.Intensity.DAPI"),
               p(br(),br(),br(),img(src="anime.png",height=113,width=125),"Testing")
          ),
          mainPanel(
               plotOutput("plot1")
          )
     )
))