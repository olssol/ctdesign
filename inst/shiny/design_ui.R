##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

## vaccine efficacy
tab_abe <- function() {
    tabPanel(
        "Antibody Efficacy",
        wellPanel(
            h4("Statistical Background"),
            withMathJax(includeHTML("www/abe.html")))
    )
}

## incidence concept
tab_incidence <- function() {
    tabPanel(
        "Incidence Rate",
        wellPanel(
            h4("Statistical Background"),
            withMathJax(includeHTML("www/incidence.html")))
    )
}

## incidence concept
tab_multi <- function() {
    tabPanel(
        "Multiplicity",
        wellPanel(
            h4("Multiplicity Control Steps"),
            withMathJax(includeHTML("www/multi.html")))
    )
}

## design page
panel_par_1 <- function() {
    tabPanel("Design Parameters",
             wellPanel(
                 fluidRow(
                     column(4,
                            numericInput("inIpla",
                                         "Placebo Incidence Rate",
                                         value = 0.04,
                                         min = 0.005, max = 1,
                                         step = 0.005),
                            numericInput("inAe1", "AbE: C 1200mg",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01),
                            numericInput("inAe2", "AbE: C 600/300mg",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01),
                            numericInput("inAe3", "AbE: C* 1200mg",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01),
                            numericInput("inAe4", "AbE: C* 600/300mg",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01)
                            ),
                     column(4,
                            numericInput("inAenroll",
                                         "Annual Enroll Rate",
                                         value = 7500,
                                         min = 100, max = 20000,
                                         step = 100),
                            numericInput("inAdrop",
                                         "Annual Dropout Rate",
                                         value = 0.01, min = 0, max = 1,
                                         step = 0.01),
                            numericInput("inPeval",
                                         "Prop. of Evaluable Participants",
                                         value = 0.8, min = 0, max = 1,
                                         step = 0.01),
                            numericInput("inMaxfu",
                                         "Max Follow Up (in Months)",
                                         value = 12, min = 3, max = 24,
                                         step = 1)))
             ))
}

panel_par_2 <- function() {
    tabPanel("Technical Parameters",
             wellPanel(
                 fluidRow(
                     column(4,
                            textInput("inTarget",
                                      "Target Events (From, To, By)",
                                      value = "100, 140, 2"),
                            numericInput("inPnull", "Binomial P for H0",
                                         value = 0.5, min = 0, max = 1,
                                         step = 0.05),
                            numericInput("inNrep",
                                         "Number of Replications",
                                         value = 100,
                                         min = 100, max = 10000,
                                         step = 10)
                            ),
                     column(4,
                            numericInput("inMaxenroll",
                                         "Max Number of Enrollment",
                                         value = 15000,
                                         min = 1000, max = 50000,
                                         step = 1000),
                            sliderInput("inNcores",
                                        "Number of Cores",
                                        value = 5, min = 1,
                                        max = (parallel::detectCores() - 1),
                                        step = 1)
                            ))
             ))
}

tab_design <- function() {
    tabPanel("Design",
             fluidPage(
                 wellPanel(
                     h4("Settings"),
                     navlistPanel(
                         panel_par_1(),
                         panel_par_2(),
                         widths = c(3, 9))
                 ),
                 wellPanel(
                     h4("Simulation Results"),
                     wellPanel(
                         actionButton("btnSimu", "Conduct Simulation",
                                      styleclass = "info")),
                     navlistPanel(
                         tabPanel("Power",
                                  wellPanel(
                                      plotOutput("pltPower"),
                                      tableOutput("tblPower"))),
                         tabPanel("Events",
                                  wellPanel(
                                      plotOutput("pltEvent"),
                                      tableOutput("tblEvent"))),
                         tabPanel("Enrollment",
                                  wellPanel(
                                      plotOutput("pltEnroll"),
                                      tableOutput("tblEnroll"))),
                         widths = c(3, 9)
                     ))
                 )
             )
}

## calculator
calc_panel_1 <- function() {
    tabPanel(
        "Relative Risk vs. Antibody Efficacy",
        wellPanel(
            fluidRow(
                column(3,
                       h5("Input"),
                       numericInput("inRtrt",
                                    label = "Antibody Arm Risk Rate",
                                    value = 0.03,
                                    min=0, max=1,
                                    step = 0.01),
                       numericInput("inRand",
                                    label = "Randomization Ratio (Antibody vs. Placebo)",
                                    value = 1, min = 0,
                                    max = 5, step = 0.5)),
                column(6,
                       h5("Output"),
                       plotOutput("pltCurve")),
                column(3,
                       h5("Output"),
                       numericInput("inRctl",
                                    label = "Placebo Arm Risk Rate",
                                    value = 0.03,
                                    min=0, max=1, step = 0.01),
                       numericInput("inRR",
                                    "Relative Risk",
                                    value = 1),
                       numericInput("inPtrt",
                                    "Prob. Event in 1 Yr (Antibody)",
                                    value = (1 - exp(-0.03))),
                       numericInput("inPctl",
                                    "Prob. Event in 1 Yr (Placebo)",
                                    value = (1 - exp(-0.03))),
                       numericInput("inAbe",
                                    "Antibody Efficacy (AbE)",
                                    value = 0),
                       numericInput("inProb",
                                    "Probability of Interest (p)",
                                    value = 0.5))
            )))
}

calc_panel_2 <- function() {
    tabPanel("Incidence Rate vs Probability",
             fluidRow(
                 column(4,
                        wellPanel(
                            h4("To Probability of Events"),
                            numericInput("inClambda",
                                         "Incidence Rate (Hazard Rate)",
                                         value = 0.004,
                                         min = 0, max = 100, step = 0.1),
                            numericInput("inCmonth",
                                         "At Month",
                                         value = 12,
                                         min = 1, max = 12, step = 1),
                            actionButton("btnCal", "Convert"),
                            numericInput("inCprob",
                                         "Probability of Experiencing Event",
                                         value = 1 - exp(-0.004))
                        )),
                 column(4,
                        wellPanel(
                            h4("To Incidence Rate (Hazard Rate)"),
                            numericInput("inCmonth2",
                                         "At Month",
                                         value = 12,
                                         min = 1, max = 12, step = 1),
                            numericInput("inCprob2",
                                         "Probability of Experiencing Event",
                                         value = 1 - exp(-0.004)),
                            actionButton("btnCal2", "Convert"),
                            numericInput("inClambda2",
                                         "Incidence Rate (Hazard Rate)",
                                         value = 0.004,
                                         min = 0, max = 100, step = 0.1)
                        ))
             ))
}

tab_calc <- function() {
    tabPanel("Calculator",
             wellPanel(
                 h4("Calculators"),
                 navlistPanel(
                     calc_panel_1(),
                     calc_panel_2(),
                     widths = c(3, 9), well = TRUE)
             ))
}

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_abe(),
                tab_incidence(),
                tab_multi(),
                tab_design(),
                tab_calc()
                )
}

## -----------------------------------------------------------
##
##          SIMULATION
##
## -----------------------------------------------------------
get_simu <- reactive({
    if (is.null(input$btnSimu)) {
        return(NULL)
    }

    if (0 == input$btnSimu) {
        return(NULL)
    }

    isolate({
        lambda_placebo <- input$inIpla
        ve             <- c(input$inAe1, input$inAe2,
                            input$inAe3, input$inAe4)
        in_target      <- strsplit(input$inTarget, ",")[[1]]
        target_event   <- seq(as.numeric(in_target[1]),
                              as.numeric(in_target[2]),
                              by = as.numeric(in_target[3]))

        annual_enroll  <- input$inAenroll
        annual_dropout <- input$inAdrop
        p_evaluable    <- input$inPeval

        p_null         <- input$inPnull
        n_reps         <- input$inNrep
        n_cores        <- input$inNcores
        n_tot          <- input$inMaxenroll
        max_followup   <- input$inMaxfu / 12

        ##Create a Progress object
        progress <- shiny::Progress$new(session, min = 0, max=1)
        progress$set(message = "Simulating...", value = 0)
        ##Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        rst_simu <- rd_simu_all(n_reps         = n_reps,
                                lambda_placebo = lambda_placebo,
                                ve             = ve,
                                target_event   = target_event,
                                annual_enroll  = annual_enroll,
                                annual_dropout = annual_dropout,
                                p_evaluable    = p_evaluable,
                                p_null         = p_null,
                                n_cores        = n_cores,
                                n_tot          = n_tot,
                                max_followup   = max_followup,
                                update_progress=progress)
    })

    rd_summary(rst_simu, arms = c("C 1200mg", "C 600/300mg",
                                  "C* 1200mg", "C* 600/300mg",
                                  "Placebo"))
})
