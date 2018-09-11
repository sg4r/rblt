#-------------------------------------------------------------------------------
# rbio-logging-toolbox
# sebastien GEIGER IPHC CNRS
# le 29/06/2018
# Copyright (C) 2018 CNRS
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License
#
#-------------------------------------------------------------------------------


#' A LoggerUI reference class
#' @export LoggerUI
#' @exportClass LoggerUI
LoggerUI <-setRefClass("LoggerUI",
                       fields = list(loglst = "list",
                                     nbrow = "numeric"),
                       methods = list(
                         initialize = function(loglst) {
                           loglst<<-loglst
                           nbrow<<-12
                         },
                         gui = function() {
                           i=0
                           loggerchoice=list()
                           for(n in loglst) {
                             i=i+1
                             loggerchoice[[n$name]]=i
                           }
                           lnbrow=loglst[[1]]$nbrow
                           ui <- fluidPage(
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("logger",
                                             label = "Logger:",
                                             choices = loggerchoice),
                                 sliderInput("time", "RTCtick:",
                                             min = 1,
                                             max = lnbrow,
                                             value = c(min,max)),
                                 checkboxGroupInput("checkGroup", label = "Move",
                                                    choices = list("Time mark" = 1, "Swimming horizontally" = 2, "Swimming descent" = 3,
                                                                   "Swimming ascent"=4, "Breathing"=5, "Acceleration"=6),
                                                    selected = list(1,2))
                               ),
                               mainPanel(
                                 fluidRow(column(3, verbatimTextOutput("value")))
                               )  )
                           )
                           server <- function(input, output, session) {
                             observeEvent(input$logger, {
                               lmax=loglst[[as.numeric(input$logger)]]$nbrow
                               updateSliderInput(session, "time",min=1,max=lmax,value=c(1,lmax))
                             })
                             output$value <- renderPrint({ input$time })
                           }
                           shinyApp(ui = ui, server = server)
                         }
                       )
)
