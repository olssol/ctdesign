##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

## tabset for data uploading
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

tab_incidence <- function() {
    tabPanel("Incidence Rate",
             wellPanel(withMathJax(includeHTML("www/incidence.html")))
             )
}

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_abe(),
                tab_incidence())
}

get_all <- function(r1, r2, ratio) {
    rr   <- r1 / r2
    abe  <- 1 - rr
    p    <- rr * ratio / (1 + rr * ratio)

    cbind(r1, r2, ratio, rr, abe, p)
}
