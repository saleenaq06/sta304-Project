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

# -------------- Set up for RQ3 (Solomon) --------------------
# Survey question: "How well do you feel your degree has prepared you for the workforce?"
# Coded 1 = Strongly prepared ... 7 = Strongly unprepared
rq3_levels <- c("Strongly prepared", "Moderately prepared", "Slightly prepared",
                "Neutral / Undecided", "Slightly unprepared",
                "Moderately unprepared", "Strongly unprepared")
data_df$Preparedness <- factor(data_df$Workforce.Preparedness, levels = 1:7, labels = rq3_levels)

# Same grouping as the EDA: strongly, moderately or slightly prepared (1 to 3) count as prepared,
# every other answer, including neutral, counts as not prepared
data_df$Prepared <- data_df$Workforce.Preparedness <= 3

# Dark blue vs light grey differ in brightness, so they stay distinct for colour-blind readers
rq3_colours <- c("Prepared" = "#2F5D8C", "Not prepared" = "#B0B7C3")

# ----------------------- End of set up for RQ3 ----------------------------

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
                checkboxGroupInput("rq3_status", "Citizenship status:",
                                   choices = c("Domestic", "International"),
                                   selected = c("Domestic", "International")),
                checkboxGroupInput("rq3_year", "Year of study:",
                                   choices = c("Third Year" = 3, "Fourth Year" = 4, "Fifth Year or Higher" = 5),
                                   selected = c(3, 4, 5))
                ),
              card(plotOutput("plot3")),
              card(card_header("What does this mean?"), uiOutput("rq3_text"))
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
  # Data after the citizenship and year-of-study filters
  rq3_data <- reactive({
    data_df[data_df$Status %in% input$rq3_status &
              data_df$Year.of.Study %in% input$rq3_year, ]
  })

  output$plot3 <- renderPlot({
    d <- rq3_data()
    validate(need(nrow(d) > 0, "Select at least one citizenship status and one year of study."))

    # One row per answer, keeping answers that nobody in this selection picked
    counts <- as.data.frame(table(Preparedness = d$Preparedness))
    counts$Group <- ifelse(as.integer(counts$Preparedness) <= 3, "Prepared", "Not prepared")
    counts$Label <- sprintf("%d (%.0f%%)", counts$Freq, 100 * counts$Freq / nrow(d))

    ggplot(counts, aes(x = Freq, y = Preparedness, fill = Group)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = Label), hjust = -0.1, size = 5) +
      scale_y_discrete(limits = rev(rq3_levels)) +   # puts "Strongly prepared" at the top
      scale_x_continuous(breaks = function(l) pretty(l)[pretty(l) %% 1 == 0],   # whole students only
                         expand = expansion(mult = c(0, 0.3))) +           # room for the labels
      scale_fill_manual(values = rq3_colours, breaks = names(rq3_colours)) +
      labs(title = "How Well Do Students Feel Their Degree\nPrepared Them for the Workforce?",
           subtitle = sprintf("Based on %d students", nrow(d)),
           x = "Number of Students",
           y = "Survey Response",
           fill = "Counted as") +
      theme_minimal(base_size = 15)
  })

  output$rq3_text <- renderUI({
    d <- rq3_data()
    n <- nrow(d)
    if (n == 0) return(p("Select at least one citizenship status and one year of study."))

    # One-sample z-test for a proportion, following the TA's feedback on the EDA
    # H0: p = 0.5 vs H1: p < 0.5 (fewer than half of students feel prepared)
    x     <- sum(d$Prepared)
    p_hat <- x / n
    z     <- (p_hat - 0.5) / sqrt(0.5 * 0.5 / n)
    p_val <- pnorm(z)   # lower tail, since H1 is p < 0.5
    p_txt <- if (p_val < 0.001) "p < 0.001" else sprintf("p = %.3f", p_val)

    tagList(
      p(strong(if (p_val < 0.05) {
        sprintf("%.0f%% feel prepared. This is enough evidence to say that fewer than half of students
                 feel prepared.", 100 * p_hat)
      } else {
        sprintf("%.0f%% feel prepared. There is not enough evidence that fewer than half of students
                 feel prepared.", 100 * p_hat)
      })),
      p(sprintf("%d of the %d students in this selection said their degree left them strongly, moderately
                 or slightly prepared for the workforce. Every other answer, including neutral, counts as
                 not prepared.", x, n)),
      p(if (p_val < 0.05) {
        sprintf("We tested whether fewer than half of students feel prepared (one-sample z-test, z = %.2f, %s).
                 The p-value is below 0.05, so a result this low would be unlikely if half of students
                 felt prepared.", z, p_txt)
      } else {
        sprintf("We tested whether fewer than half of students feel prepared (one-sample z-test, z = %.2f, %s).
                 The p-value is not below 0.05, so this selection does not give strong enough evidence that
                 fewer than half feel prepared. This test only checks for fewer than half, so it also does
                 not show that more than half feel prepared.", z, p_txt)
      }),
      if (n < 20) p(em(sprintf("Only %d students are in this selection. The z-test needs at least 20 to be
                                reliable, so treat this result with caution.", n)))
    )
  })
  }

shinyApp(ui, server)