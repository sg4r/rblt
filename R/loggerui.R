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

library(shiny)

#' A LoggerUI reference class
#' @export LoggerUI
#' @exportClass LoggerUI
LoggerUI <-setRefClass("LoggerUI",
                       fields = list(loglst = "LoggerList",
                                     nbrow = "numeric"),
                       methods = list(
                         initialize = function(loglst) {
                           loglst<<-loglst
                           nbrow<<-12
                         },
                         gui = function() {
                           i=0
                           loggerchoice=list()
                           for(n in loglst$.l) {
                             i=i+1
                             loggerchoice[[n$name]]=i
                           }
                           lnbrow=loglst$.l[[1]]$nbrow
                           lbechoices=loglst$.l[[1]]$behaviorchoices
                           lbeslct=loglst$.l[[1]]$behaviorselected
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
                                 actionButton("btzoom", "Zoom"),
                                 actionButton("btreset", "Reset"),
                                 checkboxGroupInput("checkGroup", label = "Behavior",
                                                    choices = lbechoices,
                                                    selected = lbeslct )
                               ),
                               mainPanel(
                                 fluidRow(column(3, verbatimTextOutput("value")))
                               )  )
                           )
                           server <- function(input, output, session) {
                             observeEvent(input$btzoom, {
                               lmin=input$time[1]
                               lmax=input$time[2]
                               updateSliderInput(session, "time",min=lmin,max=lmax,step = 1)
                             })
                             observeEvent(input$btreset, {
                               id=as.numeric(input$logger)
                               lmax=loglst$.l[[id]]$nbrow
                               updateSliderInput(session, "time",min=1,max=lmax,value = c(1,lmax),step = 1)
                             })
                             observeEvent(input$logger, {
                               id=as.numeric(input$logger)
                               lmax=loglst$.l[[id]]$nbrow
                               updateSliderInput(session, "time",min=1,max=lmax,value=c(1,lmax))
                               lbechoices=loglst$.l[[id]]$behaviorchoices
                               lbeslct=loglst$.l[[id]]$behaviorselected
                               updateCheckboxGroupInput(session, "checkGroup",choices =lbechoices, selected= lbeslct)
                             })
                             output$value <- renderPrint({ input$time })
                           }
                           shinyApp(ui = ui, server = server)
                         }
                       )
)
