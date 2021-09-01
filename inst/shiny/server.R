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

require(ggplot2)

shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE);

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
    ##---------vaccine efficacy-------------
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
    })


    output$pltCurve <- renderPlot({
        r1    <- input$inRtrt
        ratio <- input$inRand

        if (is.null(r1) | is.null(ratio))
            return(NULL)

        rst   <- get_all(r1, seq(r1, 1, by = 0.02), ratio)
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
})
