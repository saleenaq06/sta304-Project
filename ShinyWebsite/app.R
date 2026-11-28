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

# -------------- Set up for RQ2 (Krish, Alisyee) --------------------
# Predictors, with readable names for the dropdown
rq2_predictors <- c("Completed internships" = "Completed.Internships",
                    "CGPA"                  = "CGPA",
                    "LinkedIn connections"  = "LinkedIn.Connections")

# LinkedIn in hundreds, so the regression coefficient reads "per 100 connections"
data_df$LinkedIn.100 <- data_df$LinkedIn.Connections / 100
rq2_model_names <- c(Completed.Internships = "Completed.Internships",
                     CGPA                  = "CGPA",
                     LinkedIn.Connections  = "LinkedIn.100")

# Axis settings per predictor. CGPA and LinkedIn were asked as ranges, so the ticks show
# the ranges from the codebook instead of the codes
# CGPA codes (codebook): 0 = Below 1.70, 1 = 1.70-2.19, ..., 5 = 3.7+
# LinkedIn codes (codebook): 50 = 0-100, 150 = 100-200, ..., 550 = 500+
rq2_axis <- list(
  Completed.Internships = list(label = "Completed internships", min = 0, max = 4, step = 1,
                               breaks = 0:4, labels = as.character(0:4)),
  CGPA                  = list(label = "CGPA range", min = 0, max = 5, step = 1,
                               breaks = 0:5,
                               labels = c("Below 1.70", "1.70-2.19", "2.20-2.69",
                                          "2.70-3.19", "3.20-3.69", "3.7+")),
  LinkedIn.Connections  = list(label = "LinkedIn connections", min = 50, max = 550, step = 100,
                               breaks = seq(50, 550, by = 100),
                               labels = c("0-100", "100-200", "200-300",
                                          "300-400", "400-500", "500+"))
)

# What a one-unit change means, for the plain-language reading
rq2_unit <- c(Completed.Internships = "each extra completed internship",
              CGPA                  = "each step up one CGPA range (e.g. from 2.70-3.19 to 3.20-3.69)",
              LinkedIn.Connections  = "every extra 100 LinkedIn connections")

# Point / trend-line colour pairs. The first three are colour-blind friendly (Okabe-Ito / greyscale)
rq2_palettes <- list("Blue / Vermilion"   = c("#0072B2", "#D55E00"),
                     "Green / Pink"       = c("#009E73", "#CC79A7"),
                     "Greyscale"          = c("#737373", "#000000"),
                     "Navy / Red (EDA)"   = c("#2F5D8C", "#B22222"))

# Names to use mid-sentence
rq2_lower <- c(Completed.Internships = "completed internships",
               CGPA                  = "CGPA",
               LinkedIn.Connections  = "LinkedIn connections")

# Small helper for p-values
fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))

# ----------------------- End of set up for RQ2 ----------------------------

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
                selectInput("rq2_x", "Variable to explore:", choices = rq2_predictors),
                selectInput("rq2_y", "Confidence measure:", choices = rq1_vars),
                conditionalPanel("input.rq2_x == 'LinkedIn.Connections'",
                                 checkboxInput("rq2_log", "Log scale for LinkedIn connections", value = FALSE)),
                uiOutput("rq2_xlim_ui"),
                sliderInput("rq2_ylim", "Y-axis range:", min = 1, max = 7, value = c(1, 7), step = 0.5),
                selectInput("rq2_pal", "Colours:", choices = names(rq2_palettes)),
                checkboxInput("rq2_se", "Show 95% confidence band around the trend line", value = TRUE),
                radioButtons("rq2_model", "Regression model:",
                             choices = c("All three variables together" = "multi",
                                         "Only the selected variable"   = "simple")),
                checkboxGroupInput("rq2_status", "Citizenship status:",
                                   choices = c("Domestic", "International"),
                                   selected = c("Domestic", "International")),
                checkboxGroupInput("rq2_year", "Year of study:",
                                   choices = c("Third Year" = 3, "Fourth Year" = 4, "Fifth Year or Higher" = 5),
                                   selected = c(3, 4, 5))
                ),
              card(plotOutput("plot2")),
              card(card_header("What does this mean?"), uiOutput("rq2_text")),
              card(card_header("Regression results"),
                   tableOutput("rq2_table"),
                   uiOutput("rq2_ftest"))
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
This project explores student confidence in the current job market. We surveyed Fall 2026 STA304H5 students to answer three questions:

1. RQ1: How does job market confidence differ between international and domestic students?
2. RQ2: How strongly do the number of internships/work experiences, GPA, and number of LinkedIn followers influence student confidence?
3. RQ3: What proportion of students believe their degree has prepared them for the workforce?

## Sampling and Data Collection
Target population: Fall 2026 STA304H5 students.

Ideal design: With no constraints, we would use stratified sampling by domestic/international status and year of study. We would get the class list (with permission) from the instructor, split students into strata, and draw a simple random sample within each stratum. Participants would be entered into a raffle for a $50 gift card of their choice, with a reminder email two weeks later offering double raffle entries to improve response rates.

Actual design: The data contains 61 student responses. We used convenience sampling. The survey was posted on the course Piazza page, and students were encouraged to participate. The survey was open for two weeks, with a reminder post on Piazza after one week.

## Statistical Methods
- Welch two-sample t-test (RQ1): compares mean confidence (Likert scale) between domestic and international students.
- One-sample Z-test for a proportion (RQ3): tests whether more than 50% of students feel prepared. Students who feel moderately or strongly prepared count as prepared; all other responses, including neutral, count as not prepared.
- Multiple linear regression (RQ2): predicts confidence from internship/work experience, GPA, and LinkedIn followers. T-tests assess each coefficient, and an F-test assesses the model as a whole.

## Caveats and Limitations
- Our respondents are a convenience sample, not a random sample, so results may not generalise to all students.
- Confidence and preparedness are measured on a Likert (ordinal) scale, so means and fitted lines should be read with caution.
- Responses are self-reported, so they may be affected by social desirability or inaccurate recall (e.g. GPA, LinkedIn followers).
- With a small sample, subgroup comparisons can be noisy, and the regression may be sensitive to outliers (e.g. a few students with very large LinkedIn followings).
- Because the study is observational, the regression shows association, not causation.

                      ")
              )
            )
  )

# I know the colors aren't the prettiest... change as you see fit <3
# Keep in mind we want this to be color-blind friendly (maybe add adjustable colors panel) :)
server <- function(input, output, session) {

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
  # Data after the citizenship and year-of-study filters
  rq2_data <- reactive({
    data_df[data_df$Status %in% input$rq2_status &
              data_df$Year.of.Study %in% input$rq2_year, ]
  })

  # The x-axis slider depends on which variable is picked
  output$rq2_xlim_ui <- renderUI({
    a <- rq2_axis[[input$rq2_x]]
    tagList(
      sliderInput("rq2_xlim", "X-axis range:", min = a$min, max = a$max,
                  value = c(a$min, a$max), step = a$step),
      # CGPA and LinkedIn were asked as ranges, so explain what the slider numbers stand for
      if (input$rq2_x != "Completed.Internships") {
        helpText(paste0("Slider values stand for these ranges: ",
                        paste(a$breaks, "=", a$labels, collapse = ", "), "."))
      }
    )
  })

  # The log scale only applies to LinkedIn connections
  rq2_use_log <- reactive({
    isTRUE(input$rq2_log) && input$rq2_x == "LinkedIn.Connections"
  })

  output$plot2 <- renderPlot({
    d <- rq2_data()
    x <- input$rq2_x
    y <- input$rq2_y
    d <- d[!is.na(d[[x]]) & !is.na(d[[y]]), ]
    validate(need(nrow(d) > 0, "Select at least one citizenship status and one year of study."))

    a <- rq2_axis[[x]]
    # Right after switching variables the old slider values can still be around, so fall back
    # to the full range if they don't fit the new variable
    xl <- input$rq2_xlim
    if (is.null(xl) || xl[1] < a$min || xl[2] > a$max) xl <- c(a$min, a$max)
    pal <- rq2_palettes[[input$rq2_pal]]
    y_label <- names(rq1_vars)[rq1_vars == y]

    # Answers are whole numbers, so jitter the points a little to show overlapping students
    jit_w <- if (rq2_use_log()) 0.02 else if (x == "LinkedIn.Connections") 12 else 0.1
    x_scale <- if (rq2_use_log()) scale_x_log10 else scale_x_continuous

    ggplot(d, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(position = position_jitter(width = jit_w, height = 0.12, seed = 304),
                 colour = pal[1], alpha = 0.75, size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = input$rq2_se,
                  colour = pal[2], fill = pal[2], alpha = 0.15, linewidth = 1.2) +
      # oob_keep zooms in without throwing points away, so the trend line uses every student
      x_scale(limits = xl, breaks = a$breaks, labels = a$labels, oob = scales::oob_keep) +
      scale_y_continuous(limits = input$rq2_ylim, breaks = 1:7, oob = scales::oob_keep) +
      labs(title = paste(names(rq2_predictors)[rq2_predictors == x], "vs", y_label),
           x = paste0(a$label, if (rq2_use_log()) " (log scale)"),
           y = "Score (1 = most confident, 7 = least confident)",
           caption = sprintf("Each dot is one student (n = %d). The line is the best-fitting straight line%s.",
                             nrow(d), if (rq2_use_log()) " on the log scale" else "")) +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = if (x == "Completed.Internships") 0 else 30, hjust = 0.5,
                                       vjust = if (x == "Completed.Internships") 0.5 else 0.8))
  })

  # Regression model, refitted every time the filters or settings change
  rq2_fit <- reactive({
    d <- rq2_data()
    preds <- if (input$rq2_model == "multi") unname(rq2_predictors) else input$rq2_x
    d <- d[complete.cases(d[, c(input$rq2_y, preds)]), ]

    validate(need(nrow(d) >= length(preds) + 5,
                  "Not enough students in this selection to fit the regression. Try widening the filters."))
    for (v in preds) {
      validate(need(length(unique(d[[v]])) > 1,
                    sprintf("Everyone in this selection gave the same answer for %s, so its effect can't be estimated.",
                            names(rq2_predictors)[rq2_predictors == v])))
    }

    f <- reformulate(rq2_model_names[preds], response = input$rq2_y)
    list(fit = lm(f, data = d), preds = preds, n = nrow(d))
  })

  output$rq2_table <- renderTable({
    m <- rq2_fit()
    co <- summary(m$fit)$coefficients
    # Readable term names, with beta-hat notation (these are estimates, not the true parameters)
    subs <- c("₀", "₁", "₂", "₃")
    terms <- c("Intercept", ifelse(m$preds == "LinkedIn.Connections", "LinkedIn connections (per 100)",
                                   names(rq2_predictors)[match(m$preds, rq2_predictors)]))
    data.frame(Term = paste0(terms, " (β̂", subs[seq_along(terms)], ")"),
               Estimate = sprintf("%.3f", co[, 1]),
               `Std. error` = sprintf("%.3f", co[, 2]),
               `t value` = sprintf("%.2f", co[, 3]),
               `p-value` = fmt_p(co[, 4]),
               check.names = FALSE)
  }, align = "lrrrr")

  output$rq2_ftest <- renderUI({
    m <- rq2_fit()
    s <- summary(m$fit)
    fs <- s$fstatistic
    p_f <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    p(em(sprintf("Overall F-test: F(%d, %d) = %.2f, p %s. R² = %.2f. Based on %d students with complete answers.",
                 fs[2], fs[3], fs[1], if (p_f < 0.001) "< 0.001" else paste("=", sprintf("%.3f", p_f)),
                 s$r.squared, m$n)))
  })

  output$rq2_text <- renderUI({
    m <- rq2_fit()
    s <- summary(m$fit)
    x <- input$rq2_x
    co <- s$coefficients[rq2_model_names[[x]], ]
    b <- co[1]
    p_b <- co[4]
    fs <- s$fstatistic
    p_f <- pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    y_label <- names(rq1_vars)[rq1_vars == input$rq2_y]
    others <- if (length(m$preds) > 1) {
      paste0(" for students with the same ",
             paste(rq2_lower[names(rq2_lower) != x], collapse = " and "))
    } else ""

    tagList(
      p(strong(sprintf("%s: %s is linked to a score %.2f points %s%s, which means %s confidence.",
                       names(rq2_predictors)[rq2_predictors == x],
                       rq2_unit[[x]], abs(b), if (b < 0) "lower" else "higher", others,
                       if (b < 0) "more" else "less"))),
      p(if (p_b < 0.05) {
        sprintf("This relationship is statistically significant (p %s), so it is unlikely to be due to chance alone.",
                if (p_b < 0.001) "< 0.001" else paste("=", sprintf("%.3f", p_b)))
      } else {
        sprintf("However, this relationship is not statistically significant (p = %.3f). It could easily be
                 due to chance, so we cannot say %s %s truly linked to confidence.",
                p_b, rq2_lower[[x]], if (x == "CGPA") "is" else "are")
      }),
      p(sprintf("Overall, %s %s predict students' \"%s\" scores better than simply using the average (F-test, p %s).
                 %s for about %.0f%% of the differences between students.",
                if (length(m$preds) > 1) "the three variables together" else rq2_lower[[x]],
                if (length(m$preds) == 1 && x == "CGPA") {
                  if (p_f < 0.05) "does" else "does not"
                } else {
                  if (p_f < 0.05) "do" else "do not"
                },
                y_label,
                if (p_f < 0.001) "< 0.001" else paste("=", sprintf("%.3f", p_f)),
                if (length(m$preds) > 1) "Together they account" else "It accounts",
                100 * s$r.squared)),
      p(em("Keep in mind: this shows an association, not cause and effect. CGPA and LinkedIn connections were
            asked as ranges, and students who don't use LinkedIn are left out of any model that includes it."))
    )
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