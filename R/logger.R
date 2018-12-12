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


#' A Metric reference class
#' @import methods
#' @export
#' @exportClass Metric
Metric <- setRefClass("Metric",
                       fields = list(name = "character",
                                     colid = "numeric",
                                     colnb = "numeric",
                                     enable = "logical",
                                     srcin = "logical",
                                     beobs = "logical",
                                     height ="numeric"),
                       methods = list(
                         initialize= function(name,colid,colnb,height=200,enable=TRUE,beobs=FALSE,srcin=TRUE) {
                           name<<-name
                           colid<<-colid
                           colnb<<-colnb
                           enable<<-enable
                           srcin<<-srcin
                           beobs<<-beobs
                           height<<-height
                         },
                         draw = function() {
                           rep=paste0("name:",name,",colid:",colid,",colnb:",colnb)
                           return(rep)
                         }
                       )
)


#' A MetricList reference class
#' @export
#' @exportClass MetricList
MetricList <- setRefClass("MetricList",
                           fields = list(.l ="list"),
                           methods = list(
                             initialize= function() {
                               .l<<-list()
                             },
                             add = function(node) {
                               .l<<-c(.l,node)
                             },
                             get = function() {
                               return(.l)
                             },
                             getat = function(id) {
                               return(.l[id][[1]])
                             },
                             getsize = function() {
                               return(length(.l))
                             },
                             slctset =function(v) {
                               if (length(v)!=length(.l)) {
                                 stop("metricList set size dif from internal size")
                               }else {
                                 i=1
                                 for(n in .l) {
                                   n$enable=v[i]
                                   i=i+1
                                 }
                               }
                             },
                             draw = function() {
                               rep=list()
                               for (i in .l) {rep=c(rep,i$draw())}
                               return(rep)
                             }
                           )
)


#' A Logger reference class
#' @field filedata nom du fichier de donnee
#' @field filebehavior nom du chier des comportement
#' @import h5
#' @import tools
#' @export Logger
#' @exportClass Logger
#' @author sebastien geiger
Logger <- setRefClass("Logger",
                      fields = list(name = "character",
                                    fileh5 = "character",
                                    filebehavior = "character",
                                    metriclst ="MetricList",
                                    extmatrix ="matrix",
                                    extmatrixenable = "logical",
                                    datestart = "POSIXct",
                                    nbrow = "numeric",
                                    nbcol = "numeric",
                                    accres = "numeric",
                                    becolor = "character",
                                    beobslst = "list",
                                    behaviorchoices = "list",
                                    behaviorselected = "list" ),
                      methods = list(
                        initialize= function(fileh5 = "", filebehavior = "", besep=",", metricshow=NULL ) {
                          if(!is.character(fileh5)){
                            stop("fileh5 file path")
                          }else if (!is.h5file(fileh5)){
                            stop("fileh5 is not h5 format")
                          } else {
                            fileh5<<-fileh5
                            name<<-file_path_sans_ext(basename(fileh5))
                            filebehavior<<-filebehavior
                            options(digits.secs = 3)
                            nbrow<<-0
                            nbcol<<-Inf
                            accres<<-1
                            h5init()
                            behaviorinit(besep)
                            initmetriclst()
                            if (is.null(metricshow)==F) {
                              metriclst$slctset(metricshow)
                            }
                            extmatrix<<-matrix()
                            extmatrixenable<<-FALSE
                          }
                        },
                        getdata= function() {
                          stop("getdata virtual function should not be called directly")
                        },
                        setextmatrix= function(m) {
                          if(!is.matrix(m)) {
                            stop("not matrix")
                          } else if (nrow(m)<nbcol) {
                            stop ("matrix not good size")
                          } else {
                            extmatrix<<-m
                            extmatrixenable<<-TRUE
                          }
                        },
                        draw = function() {
                          return(paste("t:Logger fd:",fileh5))
                        },
                        initmetriclst = function() {
                          #definit les grandeurs par defaut
                          stop("initmetriclst virtual function should not be called directly")
                        },
                        h5init = function() {
                          #get info from h5 file
                          datestart<<-as.POSIXct("2015-04-01", tz="GMT")
                          stop("h5init virtual function should not be called directly")
                        },
                        behaviorinit= function(besep) {
                          lchoices=list()
                          lslct=list()
                          becolor<<-""
                          lbeobslst=list()
                          if (nchar(filebehavior)>0) {
                            tryCatch({
                              dso=read.csv(filebehavior,sep = besep)
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
LoggerCats <- setRefClass("LoggerCats",
                         contains = list("Logger"),
                         fields = list(),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "",...) {
                             callSuper(fileh5, filebehavior,...)
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
                               nbcol<<-size[2]
                               accres<<-h5attr(f["/"], "accres")
                             }
                             h5close(f)
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=f["/data"][,]
                             h5close(f)
                             colnames(m)=c("a1","a2","a3","g1","g2","g3","m1","m2","m3","t","p","l")
                             return(m)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("Accelerometer",1,3,beobs=TRUE))
                             lm$add(Metric("Gyroscope",4,3))
                             lm$add(Metric("Magnetometer",7,3))
                             lm$add(Metric("Temperature",10,1))
                             lm$add(Metric("Pression",11,1))
                             lm$add(Metric("Light intensity",12,1))
                             metriclst<<-lm
                           },
                           draw = function() {
                             return(paste0("t:LoggerCats f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)


#' A LoggerCats reference class
#' @export
#' @exportClass LoggerCatsn
LoggerCatsn <- setRefClass("LoggerCatsn",
                          contains = list("Logger"),
                          fields = list(
                          ),
                          methods = list(
                            initialize = function(fileh5 = "", filebehavior = "",...) {
                              callSuper(fileh5, filebehavior,...)
                            },
                            h5init = function() {
                              #cat("init version cats")
                              #get info from h5 file
                              f=h5file(fileh5,"r")
                              #list.attributes(f)
                              if (h5attr(f["/"], "logger")!="CATSN") {
                                stop("h5 file not CATSN structure")
                              }else if (h5attr(f["/"], "version")!=getversion()){
                                stop("CATSN h5 file not good version")
                              }else {
                                dt=h5attr(f["/"], "datestart")
                                datestart<<-as.POSIXct(dt, tz="GMT")
                                dset=openDataSet(f,"/data")
                                size=dset@dim
                                nbrow<<-size[1]
                                nbcol<<-size[2]
                                accres<<-h5attr(f["/"], "accres")
                              }
                              h5close(f)
                            },
                            getdata= function() {
                              f=h5file(fileh5,"r")
                              m=f["/data"][,]
                              h5close(f)
                              colnames(m)=c("a1","a2","a3","g1","g2","g3","m1","m2","m3","t","p","l")
                              return(m)
                            },
                            initmetriclst = function() {
                              lm=MetricList$new()
                              lm$add(Metric("Accelerometer",1,3,beobs=TRUE))
                              #lm$add(Metric("Gyroscope",4,3))
                              #lm$add(Metric("Magnetometer",7,3))
                              lm$add(Metric("Temperature",4,1))
                              lm$add(Metric("Pression",5,1))
                              #lm$add(Metric("Light intensity",12,1))
                              metriclst<<-lm
                            },
                            draw = function() {
                              return(paste0("t:LoggerCats f:",name," s:",datestart," r:",nbrow))
                            }
                          )
)

#' A LoggerAxytrek reference class
#' @export
LoggerAxytrek <- setRefClass("LoggerAxytrek",
                         contains = list("Logger"),
                         fields = list(),
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
                               nbcol<<-size[2]
                               accres<<-h5attr(f["/"], "accres")
                             }
                             h5close(f)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("Accelerometer",1,3,beobs=TRUE))
                             lm$add(Metric("Pression",4,1))
                             lm$add(Metric("Temperature",5,1))
                             metriclst<<-lm
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=f["/data"][,]
                             h5close(f)
                             colnames(m)=c("a1","a2","a3","p","t")
                             return(m)
                           },
                           draw = function() {
                             return(paste0("t:LoggerAxytrek f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerWacu reference class
#' @export
LoggerWacu <- setRefClass("LoggerWacu",
                         contains = list("Logger"),
                         fields = list(),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "",...) {
                             callSuper(fileh5, filebehavior,...)
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
                               nbcol<<-size[2]
                               accres<<-h5attr(f["/"], "accres")
                             }
                             h5close(f)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("wTemperature",1,1))
                             lm$add(Metric("wPression",2,1))
                             lm$add(Metric("wLight intensity",3,1))
                             lm$add(Metric("wAccelerometer",3,3,beobs=TRUE))
                             metriclst<<-lm
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=f["/data"][,]
                             h5close(f)
                             colnames(m)=c("t","p","l","a1","a2","a3")
                             return(m)
                           },
                           draw = function() {
                             return(paste0("t:LoggerWacu f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerList reference class
#' @import methods
#' @export LoggerList
#' @exportClass LoggerList
LoggerList <- setRefClass("LoggerList",
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


