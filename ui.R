library(shiny)

# Define UI 
shinyUI(fluidPage(
     titlePanel("My Shiny App"),
     sidebarLayout(
          sidebarPanel(
               h3("Options"),
               helpText("Select Y value"),
               selectInput("yvalue",
                           label="Choose Values for Y",
                           choices=c("Total.Intensity.DAPI","Mean.Intensity.DAPI","Mean.Intensity.Alexa.488","Mean.Intensity.Alexa.647","Mean.Intensity.Alexa.555"),
                           selected="Total.Intensity.DAPI"),
               #p(br()),
               helpText("Select X value"),
               selectInput("xvalue",
                           label="Choose Values for X",
                           choices=c("Rescan", "Scan","Time","Total.Intensity.DAPI","Mean.Intensity.DAPI","Mean.Intensity.Alexa.488","Mean.Intensity.Alexa.647","Mean.Intensity.Alexa.555"),
                           selected="Rescan"),
               helpText("Graph Type:"),
               radioButtons("graph_type",
                                  label="Select Graph type:",
                                  choices=list("Bar Plot"="bar","Scatter"="scatter"),
                                  selected="bar"),
               p(br(),br(),br(),img(src="anime.png",height=113,width=125),"Testing")
          ),
          mainPanel(
               plotOutput("plot1")
          )
     )
))