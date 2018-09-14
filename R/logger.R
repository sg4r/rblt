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

library(h5)

#' A Logger reference class
#' @field filedata nom du fichier de donnée
#' @field filebehavior nom du chier des comportement
#' @export Logger
#' @exportClass Logger
#' @author sebastien geiger
Logger <- setRefClass("Logger",
                      fields = list(name = "character",
                                    fileh5 = "character",
                                    filebehavior = "character",
                                    datestart = "POSIXct",
                                    becolor = "character",
                                    beobslst = "list",
                                    behaviorchoices = "list",
                                    behaviorselected = "list" ),
                      methods = list(
                        initialize= function(fileh5 = "", filebehavior = "") {
                          if(!is.character(fileh5)){
                            stop("fileh5 file path")
                          }else if (!is.h5file(fileh5)){
                            stop("fileh5 is not h5 format")
                          } else {
                            fileh5<<-fileh5
                            name<<-basename(fileh5)
                            filebehavior<<-filebehavior
                            options(digits.secs = 3)
                            h5init()
                            behaviorinit()
                          }
                        },
                        draw = function() {
                          return(paste("t:Logger fd:",fileh5))
                        },
                        h5init = function() {
                          #get info from h5 file
                          datestart<<-as.POSIXct("2015-04-01", tz="GMT")
                          stop("default class ne doit pas etre executé")
                        },
                        behaviorinit= function() {
                          lchoices=list()
                          lslct=list()
                          becolor<<-""
                          lbeobslst=list()
                          if (nchar(filebehavior)>0) {
                            tryCatch({
                              dso=read.csv(filebehavior)
                              dsob=levels(dso$Behavior)
                              #get number of different Behavior
                              i=0
                              for(o in dsob) {
                                i=i+1
                                lchoices[[o]]=i
                                lslct=c(lslct,i)
                              }
                              becolor<<-rainbow(i)
                              #build Behavior obs list
                              for(i in 1:nrow(dso)) {
                                row=dso[i,]
                                lfrom=paste(datestart+row$Start..s.)
                                lto=paste(datestart+row$Stop..s.)
                                #verifier index Behavior< confnbcolor
                                lcode=as.numeric(row$Behavior)
                                lcolor=becolor[[lcode]]
                                lbeobslst[[i]]=list(from = lfrom , to = lto, color = lcolor, code=lcode)
                              }
                            }, error = function(c) {
                              c$message <- paste0(c$message, " (in ", filebehavior, ")")
                              stop(c)
                            })#try
                          }#if
                          behaviorchoices<<-lchoices
                          behaviorselected<<-lslct
                          beobslst<<-lbeobslst
                        }#function
                      )
)


#' A LoggerCats reference class
#' @export LoggerCats
#' @exportClass LoggerCats
LoggerCats <-setRefClass("LoggerCats",
                         contains = list("Logger"),
                         fields = list(nbrow = "numeric"),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "") {
                             callSuper(fileh5, filebehavior)
                           },
                           h5init = function() {
                             #cat("init version cats")
                             #get info from h5 file
                             f=h5file(fileh5,"r")
                             #list.attributes(f)
                             if (h5attr(f["/"], "logger")!="CATS") {
                               stop("h5 file not CATS structure")
                             }else {
                               dt=h5attr(f["/"], "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=openDataSet(f,"/data")
                               size=dset@dim
                               nbrow<<-size[1]
                             }
                             h5close(f)
                           },
                           draw = function() {
                             return(paste0("t:LoggerCats f:",name," s:",datestart))
                           }
                         )
)
#' A LoggerList reference class
#' @export LoggerList
#' @exportClass LoggerList
LoggerList <-setRefClass("LoggerList",
                         fields = list(.l ="list"),
                         methods = list(
                           initialize= function() {
                             .l<<-list()
                           },
                           add = function(node) {
                             .l<<-c(.l,node)
                           },
                           draw = function() {
                             rep=list()
                             for (i in l$.l) {rep=c(rep,i$draw())}
                             return(rep)
                           }
                         )
)
