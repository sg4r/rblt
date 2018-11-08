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

#' A datahead reference class
#' @export Datahead
#' @exportClass Datahead
Datahead <-setRefClass("Datahead",
                       fields = list(name = "character",
                                     colid = "numeric",
                                     colnb = "numeric",
                                     height ="numeric"),
                       methods = list(
                         initialize= function(name,colid,colnb,height=200) {
                           name<<-name
                           colid<<-colid
                           colnb<<-colnb
                           height<<-height
                         },
                         draw = function() {
                           rep=paste0("name:",name,",colid:",colid,",colnb:",colnb)
                           return(rep)
                         }
                       )
)


#' A DataheadList reference class
#' @export DataheadList
#' @exportClass DataheadList
DataheadList <-setRefClass("DataheadList",
                           fields = list(.l ="list"),
                           methods = list(
                             initialize= function() {
                               .l<<-list()
                             },
                             add = function(node) {
                               .l<<-c(.l,node)
                             },
                             getat = function(id) {
                               return(.l[id][[1]])
                             },
                             getsize = function() {
                               return(length(.l))
                             },
                             draw = function() {
                               rep=list()
                               for (i in .l) {rep=c(rep,i$draw())}
                               return(rep)
                             }
                           )
)


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
                                    dataheadlst ="DataheadList",
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
                            name<<-file_path_sans_ext(basename(fileh5))
                            filebehavior<<-filebehavior
                            options(digits.secs = 3)
                            h5init()
                            behaviorinit()
                            initdataheadlst()
                          }
                        },
                        draw = function() {
                          return(paste("t:Logger fd:",fileh5))
                        },
                        initdataheadlst = function() {
                          #definit les grandeurs par defaut
                          stop("default class ne doit pas etre executé")
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
                                lcolor=substr(becolor[[lcode]],1,7)
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
                             }else if (h5attr(f["/"], "version")!=getversion()){
                               stop("CATS h5 file not good version")
                             }else {
                               dt=h5attr(f["/"], "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=openDataSet(f,"/data")
                               size=dset@dim
                               nbrow<<-size[1]
                             }
                             h5close(f)
                           },
                           initdataheadlst = function() {
                             ldh=DataheadList$new()
                             ldh$add(Datahead("AAccelerometer",1,3))
                             ldh$add(Datahead("AGyroscope",4,3))
                             ldh$add(Datahead("AMagnetometer",7,3))
                             ldh$add(Datahead("ATemperature",10,1))
                             ldh$add(Datahead("APression",11,1))
                             ldh$add(Datahead("ALight intensity",12,1))
                             dataheadlst<<-ldh
                           },
                           draw = function() {
                             return(paste0("t:LoggerCats f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerAxytrek reference class
#' @export LoggerAxytrek
#' @exportClass LoggerAxytrek
LoggerAxytrek <-setRefClass("LoggerAxytrek",
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
                             if (h5attr(f["/"], "logger")!="AXYTREK") {
                               stop("h5 file not AXYTREK structure")
                             }else if (h5attr(f["/"], "version")!=getversion()){
                               stop("CATS h5 file not good version")
                             }else {
                               dt=h5attr(f["/"], "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=openDataSet(f,"/data")
                               size=dset@dim
                               nbrow<<-size[1]
                             }
                             h5close(f)
                           },
                           initdataheadlst = function() {
                             ldh=DataheadList$new()
                             ldh$add(Datahead("titre",1,1))
                             ldh$add(Datahead("Pres",3,1))
                             dataheadlst<<-ldh
                           },
                           draw = function() {
                             return(paste0("t:LoggerAxytrek f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerWacu reference class
#' @export LoggerWacu
#' @exportClass LoggerWacu
LoggerWacu <-setRefClass("LoggerWacu",
                         contains = list("Logger"),
                         fields = list(nbrow = "numeric"),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "") {
                             callSuper(fileh5, filebehavior)
                           },
                           h5init = function() {
                             cat("init version wacu")
                             #get info from h5 file
                             f=h5file(fileh5,"r")
                             #list.attributes(f)
                             if (h5attr(f["/"], "logger")!="WACU") {
                               stop("h5 file not WACU structure")
                             }else if (h5attr(f["/"], "version")!=getversion()){
                               stop("WACU h5 file not good version")
                             }else {
                               dt=h5attr(f["/"], "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=openDataSet(f,"/data")
                               size=dset@dim
                               nbrow<<-size[1]
                             }
                             h5close(f)
                           },
                           initdataheadlst = function() {
                             ldh=DataheadList$new()
                             ldh$add(Datahead("wTemperature",1,1))
                             ldh$add(Datahead("wPression",2,1))
                             ldh$add(Datahead("wLight intensity",3,1))
                             ldh$add(Datahead("wAccelerometer",3,3))
                             dataheadlst<<-ldh
                           },
                           draw = function() {
                             return(paste0("t:LoggerWacu f:",name," s:",datestart," r:",nbrow))
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
                             for (i in .l) {rep=c(rep,i$draw())}
                             return(rep)
                           }
                         )
)


