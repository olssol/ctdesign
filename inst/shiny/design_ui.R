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
                                         "Placebo Incidence Rate at 1 Year",
                                         value = 0.04,
                                         min = 0.005, max = 1,
                                         step = 0.005),
                            numericInput("inAe1", "AbE: Gen-1",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01),
                            numericInput("inAe2", "AbE: Gen-2, High D",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01),
                            numericInput("inAe3", "AbE: Gen-2, Low D",
                                         value = 0.55,
                                         min = 0.1, max = 1,
                                         step = 0.01)
                            ),
                     column(4,
                            numericInput("inAenroll",
                                         "Annual Enroll Rate",
                                         value = 6000,
                                         min = 100, max = 15000,
                                         step = 100),
                            numericInput("inAdrop",
                                         "Annual Dropout Rate",
                                         value = 0.04, min = 0, max = 1,
                                         step = 0.01),
                            numericInput("inMaxfu",
                                         "Max Follow Up (in Months)",
                                         value = 6, min = 3, max = 24,
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
                                         step = 10),
                            numericInput("inPeval",
                                         "Prop. of Evaluable Participants",
                                         value = 1, min = 0, max = 1,
                                         step = 0.01)
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
                                        step = 1),
                            numericInput("inSeed",
                                        "Random Seed",
                                        value = 10000, min = 1, max = 1000000),
                            textInput("inSce", "Scenario", "Online Simulation")
                            ))
             ))
}

panel_results <- function() {
    wellPanel(
        h4("Simulation Results"),
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
}

tab_design <- function() {
    tabPanel("Simulation",
             wellPanel(
                 h4("Conduct Simulation"),
                 navlistPanel(
                     panel_par_1(),
                     panel_par_2(),
                     widths = c(3, 9)),
                 wellPanel(
                     actionButton("btnSimu", "Conduct Simulation",
                                  styleclass = "info"))
             ),
             wellPanel(h4("Upload Simulation Summary Results"),
                       fluidRow(
                           column(4,
                                  fileInput(inputId = 'inSimurst',
                                            label   = 'Choose File',
                                            accept  = '.Rdata')
                                  ))
             ))
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

tab_alldata <- function() {
    tabPanel("Result Data",
             wellPanel(
                 h4("All Results Data"),
                 DT::dataTableOutput("rstData")
             ))
}

tab_present <- function() {
    tabPanel("Result Presentation",
             wellPanel(
                 h4("Simulation Results"),
                 wellPanel(
                 fluidRow(
                     column(3,
                            selectInput(inputId = "inRstsce",
                                        "Select Simulation Scenario",
                                        choices = "",
                                        selected = ""),
                            selectInput(inputId = "inRstir",
                                        "Select IR at 1 Year for Placebo",
                                        choices = "",
                                        selected = ""),
                            selectInput(inputId = "inRstmulti",
                                        "Select Multiplicity Control",
                                        choices = "",
                                        multiple = TRUE,
                                        selected = ""),
                            selectInput(inputId = "inRsttype",
                                        "Select Results",
                                        choices = "",
                                        selected = "")
                            ),
                     column(9,
                            plotOutput("pltRst"),
                            tableOutput("tblRst"))
                 ))
             ))
}

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_abe(),
                tab_incidence(),
                ## tab_multi(),
                tab_calc(),
                tab_design(),
                tab_present(),
                tab_alldata()
                )
}

## -----------------------------------------------------------
##
##          SIMULATION
##
## -----------------------------------------------------------
observe({
    if (is.null(input$btnSimu)) {
        return(NULL)
    }

    if (0 == input$btnSimu) {
        return(NULL)
    }

    lst_hyp_tests   <- list("Hochberg"           = rd_rejection_s1,
                            "Holms"              = rd_rejection_s2,
                            "Hierarchical"       = rd_rejection_s3,
                            "Split-Holms"        = rd_rejection_s4,
                            "Split-Hochberg"     = rd_rejection_s5,
                            "Split-Hierarchical" = rd_rejection_s6)

    isolate({
        ir_placebo     <- input$inIpla
        ve             <- c("Gen-1"         = input$inAe1,
                            "Gen-2, High D" = input$inAe2,
                            "Gen-2, Low D"  = input$inAe3)

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
        seed           <- input$inSeed
        scenario       <- input$inSce

        ##Create a Progress object
        progress <- shiny::Progress$new(session, min = 0, max=1)
        progress$set(message = "Simulating...", value = 0)
        ##Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())

        rst_simu <- rd_simu_all(reps           = 1:n_reps,
                                n_cores        = n_cores,
                                ir_placebo_1yr = ir_placebo,
                                ve_trt         = ve,
                                target_event   = target_event,
                                annual_enroll  = annual_enroll,
                                annual_dropout = annual_dropout,
                                hyp_tests       = lst_hyp_tests,
                                p_evaluable    = p_evaluable,
                                p_null         = p_null,
                                n_tot          = n_tot,
                                max_followup   = max_followup,
                                update_progress=progress,
                                scenario       = scenario,
                                seed           = seed)

        userLog$data <- rbind(userLog$data,
                              rst_simu %>%
                              rd_combine_lst() %>%
                              rd_summary())
    })
})

## upload simulated results
observe({
    in_file <- input$inSimurst

    if (!is.null(in_file)) {
        ss  <- load(in_file$datapath)
        rst <- get(ss)
        isolate({
            userLog$data <- rbind(userLog$data, rst)
        })
    }
})

## -----------------------------------------------------------
##
##          PRESENTATION
##
## -----------------------------------------------------------
get_sce_ir <- reactive({
    dta <- userLog$data
    if (is.null(dta))
        return(NULL)

    list(sces  = unique(dta$Scenario),
         ir    = unique(dta$IR_Placebo_1Yr),
         multi = unique(dta$Multi[!is.na(dta$Multi)]),
         types = unique(dta$Type))
})

get_cur_data <- reactive({
    if (is.null(input$inRstsce) |
        is.null(input$inRstir) |
        is.null(userLog$data))

        return(NULL)

    userLog$data %>%
        dplyr::filter(Scenario       == input$inRstsce &
                      IR_Placebo_1Yr == input$inRstir)
})

get_present <- reactive({
    dta <- get_cur_data()
    if (is.null(dta)) {
        return(NULL)
    }

    rst <- rd_plot_summary(dta, type = input$inRsttype, multi = input$inRstmulti)
    rst
})
