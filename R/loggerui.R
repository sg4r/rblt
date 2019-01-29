#-------------------------------------------------------------------------------
# Title: Bio-Logging Toolbox
# Author: Geiger Sebastien [aut, cre]
# Maintainer: Geiger Sebastien <sebastien.geiger@iphc.cnrs.fr>
# date: 29/06/2018
# License: GPL (>= 3)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License
#
#-------------------------------------------------------------------------------


#' A OldLoggerUI reference class
#' @import xts
#' @import dygraphs
#' @import shiny
OldLoggerUI<-setRefClass("OldLoggerUI",
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
          m=f["/data"][mi,]
          h5close(f)
          if (loglst$.l[[id]]$extmatrixenable) {
            me=as.matrix(loglst$.l[[id]]$extmatrix[mi,])
          }
          datedeb=(ldatestart+fmin)
          datetimes <- seq.POSIXt(from=datedeb,(datedeb+fmax),fpas)
          datetimes=datetimes[1:fres]

          mlst=loglst$.l[[id]]$metriclst
          dy_graph=list()
          #boucle creation graph
          for(dh in mlst$.l) {
            if (dh$enable==T) {
              cdeb=dh$colid
              cfin=cdeb
              if (dh$colnb>1) {
                cfin=cdeb+dh$colnb-1
              }
              cmax=ncol(m)
              if (loglst$.l[[id]]$extmatrixenable) {
                if (dh$srcin==F) {
                  cmax=ncol(me)
                }
              }
              if (cfin>cmax) {
                stop("ERROR: Metric index over ncol")
              }
              if (dh$srcin) {
                wt=xts(m[,cdeb:cfin], order.by = datetimes, tz="GMT" )
              } else {
                wt=xts(me[,cdeb:cfin], order.by = datetimes, tz="GMT" )
              }
              dyt=dygraphs::dygraph(wt,main = dh$name, group = "wac",height = 200)%>%
                dyOptions(labelsUTC = TRUE)
              if (dh$beobs==T) {
                #add obs
                lobs=loglst$.l[[id]]$beobslst
                for( ob in lobs ) {
                  if (ob$code %in% input$checkGroup) {
                    dyt <- dyShading(dyt, from = ob$from , to = ob$to, color = ob$color )
                  }
                }
              }
              dy_graph=list(dy_graph,dyt)
            }
          }
          tagList(dy_graph)
        })
      }
      shinyApp(ui = ui, server = server)
    }
  )
)


#' A LoggerUI reference class
#' @field loglst list of logger class
#' @field id id of curent loger view
#' @field ldatestart curent start date
#' @field nbrow courent row number
#' @export LoggerUI
#' @exportClass LoggerUI
#' @import xts
#' @import dygraphs
#' @import shiny
LoggerUI<-setRefClass("LoggerUI",
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
                          "plot logger list"
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
                                actionButton("btzg", "<<"),
                                actionButton("btzoom", "Zoom"),
                                actionButton("btreset", "Reset"),
                                actionButton("btzd", ">>"),
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
                              lmax=loglst$.l[[id]]$nbrow/loglst$.l[[id]]$accres
                              updateSliderInput(session, "time",min=1,max=lmax,value = c(1,lmax),step = 1)
                            })
                            observeEvent(input$btzg, {
                              tmin=input$time[1]
                              tmax=input$time[2]
                              tmil=(tmax-tmin)/2
                              if ((tmin-tmil)>0) {
                                tmin=tmin-tmil
                                tmax=tmin+2*tmil
                              }else{
                                tmin=1
                                tmax=2*tmil
                              }
                              updateSliderInput(session, "time",value = c(tmin,tmax),step = 1)
                            })
                            observeEvent(input$btzd, {
                              id<<-as.numeric(input$logger)
                              lmax=loglst$.l[[id]]$nbrow/loglst$.l[[id]]$accres
                              tmin=input$time[1]
                              tmax=input$time[2]
                              tmil=(tmax-tmin)/2
                              if ((tmax+tmil)<lmax) {
                                tmin=tmin+tmil
                                tmax=tmin+2*tmil
                              }else{
                                tmax=lmax
                                tmin=tmax-2*tmil
                              }
                              updateSliderInput(session, "time",value = c(tmin,tmax),step = 1)
                            })
                            observeEvent(input$logger, {
                              id<<-as.numeric(input$logger)
                              lmax=loglst$.l[[id]]$nbrow/loglst$.l[[id]]$accres
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
                              facc=loglst$.l[[id]]$accres
                              fdt=1
                              fmin=input$time[1]
                              fmax=input$time[2]
                              if ((fmax-fmin) < fres) {
                                fmin=fmin*facc
                                fmax=fmax*facc
                                if ((fmax-fmin) < fres) {
                                  fpas=1
                                  fres=fmax-fmin
                                }else {
                                  fpas=floor((fmax-fmin)/fres)
                                }
                                facc=1
                                fdt =1/loglst$.l[[id]]$accres
                              } else {
                                fpas=floor((fmax-fmin)/fres)
                              }
                              mi=seq(fmin,fmax,fpas)
                              mict=length(mi)
                              mi=mi[1:fres]*facc

                              fileh5=loglst$.l[[id]]$fileh5
                              f=h5file(fileh5,"r")
                              #m=ds[mi,]
                              m=f["/data"][mi,]
                              h5close(f)
                              if (loglst$.l[[id]]$extmatrixenable) {
                                me=as.matrix(loglst$.l[[id]]$extmatrix[mi,])
                              }
                              datedeb=(ldatestart+(fmin*fdt))
                              datetimes <- seq.POSIXt(from=datedeb,(datedeb+(fmax*fdt)),(fpas*fdt))
                              #cat(paste0("[",mict,":",length(datetimes),"] "))
                              datetimes=datetimes[1:fres]

                              mlst=loglst$.l[[id]]$metriclst
                              dy_graph=list()
                              #boucle creation graph
                              for(dh in mlst$.l) {
                                if (dh$enable==T) {
                                  cdeb=dh$colid
                                  cfin=cdeb
                                  if (dh$colnb>1) {
                                    cfin=cdeb+dh$colnb-1
                                  }
                                  cmax=ncol(m)
                                  if (loglst$.l[[id]]$extmatrixenable) {
                                    if (dh$srcin==F) {
                                      cmax=ncol(me)
                                    }
                                  }
                                  if (cfin>cmax) {
                                    stop("ERROR: Metric index over ncol")
                                  }
                                  if (dh$srcin) {
                                    wt=xts(m[,cdeb:cfin], order.by = datetimes, tz="GMT" )
                                  } else {
                                    wt=xts(me[,cdeb:cfin], order.by = datetimes, tz="GMT" )
                                  }
                                  dyt=dygraphs::dygraph(wt,main = dh$name, group = "wac",height = 200)%>%
                                    dyOptions(labelsUTC = TRUE)
                                  if (dh$beobs==T) {
                                    #add obs
                                    lobs=loglst$.l[[id]]$beobslst
                                    for( ob in lobs ) {
                                      if (ob$code %in% input$checkGroup) {
                                        dyt <- dyShading(dyt, from = ob$from , to = ob$to, color = ob$color )
                                      }
                                    }
                                  }
                                  dy_graph=list(dy_graph,dyt)
                                }
                              }
                              tagList(dy_graph)
                            })
                          }
                          shinyApp(ui = ui, server = server)
                        }
                      )
)
