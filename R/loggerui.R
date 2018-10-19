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

library(dygraphs)
library(xts)
library(shiny)


#' A LoggerUI reference class
#' @export LoggerUI
#' @exportClass LoggerUI
LoggerUI <-setRefClass(
  "LoggerUI",
   fields = list(loglst = "LoggerList",
                 id = "numeric",
                 ldatestart =  "POSIXct",
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
       id<<-1
       lnbrow=loglst$.l[[1]]$nbrow
       lbechoices=loglst$.l[[1]]$behaviorchoices
       lbeslct=loglst$.l[[1]]$behaviorselected
       lbecolor=loglst$.l[[1]]$becolor
       lbechnames=list()
       lbechvalues=list()
       ldatestart<<-loglst$.l[[1]]$datestart
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
                                #choices = lbechoices,
                                #selected = lbeslct
                                choiceNames = lbechnames,
                                choiceValues = lbechvalues)
           ),
           mainPanel(
             uiOutput("dygraph")
           )  )
       )
       server <- function(input, output, session) {
         observeEvent(input$btzoom, {
           lmin=input$time[1]
           lmax=input$time[2]
           updateSliderInput(session, "time",min=lmin,max=lmax,step = 1)
         })
         observeEvent(input$btreset, {
           id<<-as.numeric(input$logger)
           lmax=loglst$.l[[id]]$nbrow
           updateSliderInput(session, "time",min=1,max=lmax,value = c(1,lmax),step = 1)
         })
         observeEvent(input$logger, {
           id<<-as.numeric(input$logger)
           lmax=loglst$.l[[id]]$nbrow
           nbrow<<-lmax
           updateSliderInput(session, "time",min=1,max=lmax,value=c(1,lmax))
           lbechoices=loglst$.l[[id]]$behaviorchoices
           lbeslct=loglst$.l[[id]]$behaviorselected
           ldatestart<<-loglst$.l[[id]]$datestart
           lbecolor=loglst$.l[[id]]$becolor
           for(v in lbechoices) {
             tag=tags$span(names(lbechoices[v]),style =paste0("color :",substr(lbecolor[v],1,7),";"))
             lbechnames=c(lbechnames,list(tag))
             lbechvalues=c(lbechvalues,v)
           }
           updateCheckboxGroupInput(session, "checkGroup",choiceNames = lbechnames, choiceValues = lbechvalues,
                                    selected= lbeslct)
         })
         output$dygraph <- renderUI({
           fres=1000
           fmin=input$time[1]
           fmax=input$time[2]
           if ((fmax-fmin) < fres) {
             fpas=1
             fres=fmax-fmin
           } else {
             fpas=floor((fmax-fmin)/fres)
           }
           mi=seq(fmin,fmax,fpas)
           mi=mi[1:fres]
           fileh5=loglst$.l[[id]]$fileh5
           f=h5file(fileh5,"r")
           #m=ds[mi,]
           m=f["/data"][mi,1:12]
           h5close(f)
           datedeb=(ldatestart+fmin)
           datetimes <- seq.POSIXt(from=datedeb,(datedeb+fmax),fpas)
           datetimes=datetimes[1:fres]
           acc=cbind(m[,1],m[,2],m[,3])
           wacc=xts(acc, order.by = datetimes, tz="GMT" )
           dyacc=dygraphs::dygraph(wacc,main = "acc", group = "wac",height = 200) %>%
             dyOptions(labelsUTC = TRUE)
           lobs=loglst$.l[[id]]$beobslst
           for( ob in lobs ) {
             if (ob$code %in% input$checkGroup) {
               dyacc <- dyShading(dyacc, from = ob$from , to = ob$to, color = ob$color )
             }
           }
           wt=xts(m[,10], order.by = datetimes, tz="GMT" )
           dyt=dygraphs::dygraph(wt,main = "Temperature", group = "wac",height = 200)%>%
             dyOptions(labelsUTC = TRUE)
           wp=xts(m[,11], order.by = datetimes, tz="GMT" )
           dyp=dygraphs::dygraph(wp,main = "Pression", group = "wac",height = 200) %>%
             dyOptions(labelsUTC = TRUE)
           wl=xts(m[,12], order.by = datetimes, tz="GMT" )
           dyl=dygraphs::dygraph(wl,main = "Light intensity", group = "wac",height = 200)%>%
             dyOptions(labelsUTC = TRUE)
           dy_graph <- list(dyacc,dyt,dyp,dyl)
           tagList(dy_graph)
         })

       }
       shinyApp(ui = ui, server = server)
     }
   )
)

#' A LoggerWacuUI reference class
#' @export LoggerWacuUI
#' @exportClass LoggerWacuUI
LoggerWacuUI <-setRefClass(
  "LoggerWacuUI",
  fields = list(loglst = "LoggerList",
                id = "numeric",
                ldatestart =  "POSIXct",
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
      id<<-1
      lnbrow=loglst$.l[[1]]$nbrow
      lbechoices=loglst$.l[[1]]$behaviorchoices
      lbeslct=loglst$.l[[1]]$behaviorselected
      lbecolor=loglst$.l[[1]]$becolor
      lbechnames=list()
      lbechvalues=list()
      ldatestart<<-loglst$.l[[1]]$datestart
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
                               #choices = lbechoices,
                               #selected = lbeslct
                               choiceNames = lbechnames,
                               choiceValues = lbechvalues)
          ),
          mainPanel(
            uiOutput("dygraph")
          )  )
      )
      server <- function(input, output, session) {
        observeEvent(input$btzoom, {
          lmin=input$time[1]
          lmax=input$time[2]
          updateSliderInput(session, "time",min=lmin,max=lmax,step = 1)
        })
        observeEvent(input$btreset, {
          id<<-as.numeric(input$logger)
          lmax=loglst$.l[[id]]$nbrow
          updateSliderInput(session, "time",min=1,max=lmax,value = c(1,lmax),step = 1)
        })
        observeEvent(input$logger, {
          id<<-as.numeric(input$logger)
          lmax=loglst$.l[[id]]$nbrow
          nbrow<<-lmax
          updateSliderInput(session, "time",min=1,max=lmax,value=c(1,lmax))
          lbechoices=loglst$.l[[id]]$behaviorchoices
          lbeslct=loglst$.l[[id]]$behaviorselected
          ldatestart<<-loglst$.l[[id]]$datestart
          lbecolor=loglst$.l[[id]]$becolor
          for(v in lbechoices) {
            tag=tags$span(names(lbechoices[v]),style =paste0("color :",substr(lbecolor[v],1,7),";"))
            lbechnames=c(lbechnames,list(tag))
            lbechvalues=c(lbechvalues,v)
          }
          updateCheckboxGroupInput(session, "checkGroup",choiceNames = lbechnames, choiceValues = lbechvalues,
                                   selected= lbeslct)
        })
        output$dygraph <- renderUI({
          fres=1000
          fmin=input$time[1]
          fmax=input$time[2]
          if ((fmax-fmin) < fres) {
            fpas=1
            fres=fmax-fmin
          } else {
            fpas=floor((fmax-fmin)/fres)
          }
          mi=seq(fmin,fmax,fpas)
          mi=mi[1:fres]
          fileh5=loglst$.l[[id]]$fileh5
          f=h5file(fileh5,"r")
          #m=ds[mi,]
          m=f["/data"][mi,1:3]
          h5close(f)
          datedeb=(ldatestart+fmin)
          datetimes <- seq.POSIXt(from=datedeb,(datedeb+fmax),fpas)
          datetimes=datetimes[1:fres]
          #acc=cbind(m[,1],m[,2],m[,3])
          #wacc=xts(acc, order.by = datetimes, tz="GMT" )
          #dyacc=dygraphs::dygraph(wacc,main = "acc", group = "wac",height = 200) %>%
          #  dyOptions(labelsUTC = TRUE)
          #lobs=loglst$.l[[id]]$beobslst
          # for( ob in lobs ) {
          #   if (ob$code %in% input$checkGroup) {
          #     dyacc <- dyShading(dyacc, from = ob$from , to = ob$to, color = ob$color )
          #   }
          # }
          wt=xts(m[,1], order.by = datetimes, tz="GMT" )
          dyt=dygraphs::dygraph(wt,main = "Temperature", group = "wac",height = 200)%>%
            dyOptions(labelsUTC = TRUE)
          wp=xts(m[,2], order.by = datetimes, tz="GMT" )
          dyp=dygraphs::dygraph(wp,main = "Pression", group = "wac",height = 200) %>%
            dyOptions(labelsUTC = TRUE)
          wl=xts(m[,3], order.by = datetimes, tz="GMT" )
          dyl=dygraphs::dygraph(wl,main = "Light intensity", group = "wac",height = 200)%>%
            dyOptions(labelsUTC = TRUE)
#          dy_graph <- list(dyacc,dyt,dyp,dyl)
          dy_graph <- list(dyt,dyp,dyl)
          tagList(dy_graph)
        })

      }
      shinyApp(ui = ui, server = server)
    }
  )
)

