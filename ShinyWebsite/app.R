library(shiny)
library(bslib)
library(ggplot2)

# data upload
data_df <- read.csv("data.csv")

# the sidebars are just filler code so you can run it and see what the app looks like
# change the code to fit your data <3
ui <- page_navbar(
  title = "STAKS - STA304 Survey Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  
  # Saleena, Tanya
  nav_panel("RQ1: Confidence, International vs Domestic",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("var1", "Variable:", choices = c("mpg", "hp", "wt"))
                ),
              plotOutput("plot1")
              )
            ),
  
  # Krish, Alisyee
  nav_panel("RQ2: What Drives Confidence",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("var2", "Variable:", choices = c("mpg", "hp", "wt"))
                ),
              plotOutput("plot2")
              )
            ),
  
  # Solomon
  nav_panel("RQ3: Degree of Preparation",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("var3", "Variable:", choices = c("mpg", "hp", "wt"))
                ),
              plotOutput("plot3")
              )
            ),
  
  # Bonus Points!
  nav_panel("About",
            card(
              card_header("About the Survey & Methodology"),
              markdown("
              ## Background
              blah blah blah.. talk about our project... what are we exploring
              
              ## Sampling and Data Collection
              Our current population is this and our sample was that and we collected the data like this...
              are you still reading? hi hi hello
              
              ## Caveats and Limitations
              So we were limited by our limitations that limited us and this is some more filler...
              If you've read this far, I offer you a nice quote: 
                       'It's time to see what I can do, to test the limits and break through.' — Frozen
                      ")
              )
            )
  )

# I know the colors aren't the prettiest... change as you see fit <3
# Keep in mind we want this to be color-blind friendly (maybe add adjustable colors panel) :)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var1]])) + geom_histogram(bins = 10, fill = "violet", color = "white")
    })
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var2]])) + geom_histogram(bins = 10, fill = "aquamarine", color = "black")
    })
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var3]])) + geom_histogram(bins = 10, fill = "coral", color = "grey")
    })
  }

shinyApp(ui, server)