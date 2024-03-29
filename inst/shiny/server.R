##    ----------------------------------------------------------------------
##    Copyright (C) 2015  Daniel O. Scharfstein and Chenguang Wang
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.

##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.

##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.
##    -----------------------------------------------------------------------

options(shiny.maxRequestSize = 200*1024^2)

require(ggplot2)

shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE);

    userLog          <- reactiveValues();
    userLog$data     <- NULL;


    ##--------------------------------------
    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
        tab_main()
    })

    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {
        stopApp()})

    ##--------------------------------------
    ##---------calculator-------------------
    ##--------------------------------------

    to_listen <- reactive({
        rst <-  c(input$inRtrt, input$inRctl, input$inRand)
        rst
    })

    observeEvent(to_listen(), {
        if (is.null(to_listen()))
            return(NULL)

        r1    <- to_listen()[1]
        r2    <- to_listen()[2]
        ratio <- to_listen()[3]

        rst   <- rd_rr(r1, r2, ratio)
        updateNumericInput(session, "inRR",   value = rst[4])
        updateNumericInput(session, "inAbe",  value = rst[5])
        updateNumericInput(session, "inProb", value = rst[6])
        updateNumericInput(session, "inPtrt", value = 1 - exp(-r1))
        updateNumericInput(session, "inPctl", value = 1 - exp(-r2))
    })


    output$pltCurve <- renderPlot({
        r1    <- input$inRtrt
        ratio <- input$inRand

        if (is.null(r1) | is.null(ratio))
            return(NULL)

        rst   <- rd_rr(r1, seq(r1, 1, by = 0.02), ratio)
        dta   <- rbind(data.frame(RR    = rst[, 4],
                                  Y     = rst[, 5],
                                  Label = "AbE"),
                       data.frame(RR    = rst[, 4],
                                  Y     = rst[, 6],
                                  Label = "p"))

        ggplot(data = dta, aes(x = RR, y = Y)) +
            geom_line(aes(group = Label, color = Label)) +
            labs(x = "Relative Risk", y = "Probability") +
            xlim(0 ,1) +
            ylim(0, 1) +
            theme_bw()
    })

    observeEvent(input$btnCal, {
        if (0 == input$btnCal)
            return(NULL)

        lambda <- input$inClambda
        year   <- input$inCmonth / 12
        prob   <- 1 - exp(-lambda * year)

        updateNumericInput(session, "inCprob", value = prob)
    })

    observeEvent(input$btnCal2, {
        if (0 == input$btnCal2)
            return(NULL)

        year   <- input$inCmonth2 / 12
        prob   <- input$inCprob2
        lambda <- - log(1 - prob) / year

        updateNumericInput(session, "inClambda2", value = lambda)
    })

    ##--------------------------------------
    ##---------simulation results-----------
    ##--------------------------------------
    output$rstData <- DT::renderDataTable({
        userLog$data
    },
    rownames = NULL,
    selection = "none",
    options = list(pageLength = 100))

    observe({
        lst <- get_sce_ir()
        if (is.null(lst))
            return(NULL)

        updateSelectInput(session, "inRstsce",   choices = lst$sce)
        updateSelectInput(session, "inRstir",    choices = lst$ir)
        updateSelectInput(session, "inRstmulti", choices = lst$multi)
        updateSelectInput(session, "inRsttype", choices = lst$types)

    })

    output$pltRst <- renderPlot({
        rst <- get_present()
        if (is.null(rst)) {
            return(NULL)
        }

        rst$plt
    })

    output$tblRst <- DT::renderDataTable({
        rst <- get_present()
        if (is.null(rst)) {
            return(NULL)
        }
        rst$data %>% data.frame()
    },
    rownames = NULL,
    selection = "none",
    options = list(pageLength=100))
})
