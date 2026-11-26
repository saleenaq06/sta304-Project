library(shiny)
library(bslib)
library(ggplot2)

# data upload
data_df <- read.csv("data.csv")

# -------------- Set up for RQ1 (Saleena, Tanya) --------------------
# note that Domestic = 0, International = 1
data_df$Status <- factor(data_df$Citizenship.Status, levels = c(0, 1), labels = c("Domestic", "International"))

# Job market confidence = average of the five confidence questions
data_df$Confidence <- rowMeans(data_df[, c("Necessary.Skills", "Abundant.Opportunities",
                                           "Job.Security", "Professional.Network",
                                           "Workforce.Preparedness")])

# Readable names for the dropdown
rq1_vars <- c("Overall job market confidence (average)" = "Confidence",
              "Has the skills employers want"           = "Necessary.Skills",
              "Sees abundant job opportunities"         = "Abundant.Opportunities",
              "Feels job security"                      = "Job.Security",
              "Has a strong professional network"       = "Professional.Network",
              "Feels prepared for the workforce"        = "Workforce.Preparedness")

# Colour-blind-friendly colour pairs (Domestic, International)
rq1_palettes <- list("Orange / Blue"  = c("#E69F00", "#0072B2"),
                     "Blue / Red"     = c("#005AB5", "#DC3220"),
                     "Greyscale"      = c("#D9D9D9", "#737373"))

# ----------------------- End of set up for RQ1 ----------------------------

# the sidebars are just filler code so you can run it and see what the app looks like
# change the code to fit your data <3
ui <- page_navbar(
  title = "STAKS - STA304 Survey Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  
  # Saleena, Tanya
  nav_panel("RQ1: Confidence, International vs Domestic",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("rq1_var", "Confidence measure:", choices = rq1_vars),
                sliderInput("rq1_ylim", "Y-axis range:", min = 1, max = 10, value = c(1, 7), step = 0.5),
                selectInput("rq1_pal", "Colours:", choices = names(rq1_palettes)),
                checkboxGroupInput("rq1_year", "Year of study:",
                                   choices = c("Third Year" = 3, "Fourth Year" = 4, "Fifth Year or Higher" = 5),
                                   selected = c(3, 4, 5))
                ),
              card(plotOutput("plot1")),
              card(card_header("What does this mean?"), uiOutput("rq1_text"))
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

  # ---------- TAB 1: RQ1 (Saleena, Tanya) ----------------
  # Data after the year-of-study filter
  rq1_data <- reactive({
    data_df[data_df$Year.of.Study %in% input$rq1_year, ]
  })

  output$plot1 <- renderPlot({
    d <- rq1_data()
    validate(need(nrow(d) > 0, "Select at least one year of study."))
    label <- names(rq1_vars)[rq1_vars == input$rq1_var]

    ggplot(d, aes(x = Status, y = .data[[input$rq1_var]], fill = Status)) +
      geom_boxplot(width = 0.5) +
      scale_fill_manual(values = rq1_palettes[[input$rq1_pal]]) +
      coord_cartesian(ylim = input$rq1_ylim) +
      labs(title = paste(label, "by Citizenship Status"),
           x = "Citizenship Status",
           y = "Score (1 = Strongly Agree, 7 = Strongly Disagree)",
           fill = "Citizenship Status") +
      theme_minimal(base_size = 15)
  })

  output$rq1_text <- renderUI({
    d <- rq1_data()
    dom  <- d[[input$rq1_var]][d$Status == "Domestic"]
    intl <- d[[input$rq1_var]][d$Status == "International"]
    if (length(dom) < 2 || length(intl) < 2) {
      return(p("Not enough students in this selection to compare the two groups."))
    }

    res <- t.test(intl, dom)   # Welch t-test, same as the EDA
    more_conf <- if (mean(dom) < mean(intl)) "Domestic" else "International"

    tagList(
      p(sprintf("Domestic students averaged %.2f and international students averaged %.2f
                 (lower scores mean more agreement, in other words, more confidence). On average, %s students
                 were more confident.", mean(dom), mean(intl), more_conf)),
      p(if (res$p.value < 0.05) {
        sprintf("This difference is statistically significant (Welch t-test, p = %.3f). Thus, it is
                 unlikely to be due to chance alone.", res$p.value)
      } else {
        sprintf("However, this difference is not statistically significant (Welch t-test, p = %.3f). Thus,
                 it could easily be due to chance, so we cannot say the two groups truly differ.",
                res$p.value)
      }),
      p(em(sprintf("Based on %d domestic and %d international students.", length(dom), length(intl))))
    )
  })

  # ---------- TAB 2: RQ2 (Krish, Alisyee) ----------------
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var2]])) + geom_histogram(bins = 10, fill = "aquamarine", color = "black")
    })


  # ---------- TAB 3: RQ3 (Solomon) ----------------
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var3]])) + geom_histogram(bins = 10, fill = "coral", color = "grey")
    })
  }

shinyApp(ui, server)