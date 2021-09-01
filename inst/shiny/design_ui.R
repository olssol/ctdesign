##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

## vaccine efficacy
tab_abe <- function() {
    tabPanel("Antibody Efficacy",
             fluidPage(
                 fluidRow(
                     column(7, h4("Statistical Background"),
                            wellPanel(withMathJax(includeHTML("www/abe.html")))
                            ),
                     column(5, h4("Demonstration"),
                            wellPanel(
                                h5("Input"),
                                numericInput("inRtrt", label = "Antibody Arm Risk Rate",
                                             value = 0.03, min=0, max=1, step = 0.01),
                                numericInput("inRand",
                                             label = "Randomization Ratio (Antibody vs. Placebo)",
                                             value = 1, min = 0, max = 5, step = 0.5)),
                            wellPanel(
                                h5("Output"),
                                plotOutput("pltCurve")),
                            wellPanel(
                                h5("Single Output"),
                                numericInput("inRctl", label = "Placebo Arm Risk Rate",
                                             value = 0.03, min=0, max=1, step = 0.01),
                                numericInput("inRR","Relative Risk", value="1"),
                                numericInput("inAbe","Antibody Efficacy (AbE)", value="0"),
                                numericInput("inProb","Probability of Interest (p)", value="0.5")
                            ))
                 ))
             )
}

## incidence concept
tab_incidence <- function() {
    tabPanel("Incidence Rate",
             wellPanel(withMathJax(includeHTML("www/incidence.html")))
             )
}

## design page
panel_par <- function() {
    wellPanel(
        h4("Design Parameters"),
        fluidRow(
            column(4,
                   numericInput("inIpla", "Placebo Incidence Rate",
                                value = 0.04, min = 0.005, max = 1,
                                step = 0.005),
                   numericInput("inAe1", "AbE: C 1200mg",
                                value = 0.55, min = 0.1, max = 1,
                                step = 0.01),
                   numericInput("inAe2", "AbE: C 600/300mg",
                                value = 0.55, min = 0.1, max = 1,
                                step = 0.01),
                   numericInput("inAe3", "AbE: C* 1200mg",
                                value = 0.55, min = 0.1, max = 1,
                                step = 0.01),
                   numericInput("inAe4", "AbE: C* 600/300mg",
                                value = 0.55, min = 0.1, max = 1,
                                step = 0.01)
                   ),
            column(4,
                   textInput("inTarget",
                             "Target Events (From, To, By)",
                             value = "100, 140, 2"),
                   numericInput("inAenroll",
                                "Annual Enroll Rate",
                                value = 7500,
                                min = 100, max = 20000,
                                step = 100),
                   numericInput("inAdrop", "Annual Dropout Rate",
                                value = 0.01, min = 0, max = 1,
                                step = 0.01),
                   numericInput("inPeval",
                                "Prop. of Evaluable Participants",
                                value = 0.8, min = 0, max = 1,
                                step = 0.01)),
            column(4,
                   numericInput("inPnull", "Binomial P for H0",
                                value = 0.5, min = 0, max = 1,
                                step = 0.05),
                   numericInput("inNrep", "Number of Replications",
                                value = 100,
                                min = 100, max = 10000,
                                step = 10),
                   numericInput("inMax", "Max Number of Enrollment",
                                value = 15000, min = 1000, max = 50000,
                                step = 1000),
                   sliderInput("inNcores",
                               "Number of Cores",
                               value = 5, min = 1,
                               max = (parallel::detectCores() - 1),
                               step = 1)
                   )))
}

tab_design <- function() {
    tabPanel("Design",
             fluidPage(
                 panel_par(),
                 wellPanel(
                     h4("Simulation Results"),
                     actionButton("btnSimu", "Conduct Simulation",
                                  styleclass = "info"),
                     fluidRow(
                         column(4,
                                wellPanel(h6("Power"),
                                          plotOutput("pltPower"),
                                          tableOutput("tblPower")
                                          )
                                ),
                         column(4,
                                wellPanel(h6("Enrollment"),
                                          plotOutput("pltEnroll"),
                                          tableOutput("tblEnroll")
                                          )
                                ),
                         column(4,
                                wellPanel(h6("Events"),
                                          plotOutput("pltEvent"),
                                          tableOutput("tblEvent")
                                          )
                                )

                     )
                 ))
             )
}


##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_abe(),
                tab_incidence(),
                tab_design())
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
        n_tot          <- input$inMax

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
                                update_progress=progress)
    })

    rd_summary(rst_simu)
})
