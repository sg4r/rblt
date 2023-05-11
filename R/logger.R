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


#' Metric reference class
#' @import methods
#' @field name title metric in chart
#' @field colid start column id
#' @field connb number of column for this metric
#' @export Metric
#' @exportClass Metric
Metric <- setRefClass("Metric",
                       fields = list(name = "character",
                                     colname ="character",
                                     colid = "numeric",
                                     colnb = "numeric",
                                     enable = "logical",
                                     srcin = "logical",
                                     beobs = "logical",
                                     height ="numeric"),
                       methods = list(
                         initialize= function(name,colname,colid,colnb,height=200,enable=TRUE,beobs=FALSE,srcin=TRUE) {
                           name<<-name
                           colname<<-colname
                           colid<<-colid
                           colnb<<-colnb
                           enable<<-enable
                           srcin<<-srcin
                           beobs<<-beobs
                           height<<-height
                         },
                         draw = function() {
                           "draw the objec value
                          \\subsection{Return Value}{returns a String object representing the value}"
                           rep=paste0("name:",name,",colid:",colid,",colnb:",colnb)
                           return(rep)
                         },
                         getmatrix = function(id) {
                           "get matrix of elements"
                           m=matrix(data= c(name,colname,id,colnb,enable,"TRUE",beobs,height),nrow = 1)
                           colnames(m)=c("name","colname","colid","colnb","enable","srcin","beobs","height")
                           return(m)
                         }
                       )
)


#' MetricList reference class
#' @export MetricList
#' @exportClass MetricList
MetricList <- setRefClass("MetricList",
                           fields = list(.l ="list"),
                           methods = list(
                             initialize= function() {
                               .l<<-list()
                             },
                             add = function(node) {
                               "add new node in the list."
                               .l<<-c(.l,node)
                             },
                             get = function() {
                               "get all node from the list.
                                \\subsection{Return Value}{returns a list of node}"
                               return(.l)
                             },
                             getat = function(id) {
                               "return element at id index.
                                \\subsection{Return Value}{returns the node @ id}"
                               return(.l[id][[1]])
                             },
                             getsize = function() {
                               "return lenght of element.
                                \\subsection{Return Value}{returns a non-negativ numeric}"
                               return(length(.l))
                             },
                             slctset =function(v) {
                               "enable or disable metric view
                               \\subsection{Parameters}{\\itemize{
                                 \\item{\\code{v} True or False vector}
                               }}"
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
                             getmatrix = function() {
                               "get matrix of elements"
                               rep=matrix()
                               repinit=FALSE
                               id=1
                               for(i in .l) {
                                 if(repinit) {
                                   if (i$enable) {
                                     rep=rbind(rep,i$getmatrix(id))
                                     id=id+i$colnb
                                   }
                                 }else{
                                   if (i$enable) {
                                     rep=i$getmatrix(id)
                                     id=id+i$colnb
                                     repinit=TRUE
                                   }
                                 }
                               }
                               return(rep)
                             },
                             getcolactive = function() {
                               "get matrix col enable"
                               rep=vector()
                               msize=0
                               for (i in .l) {
                                 if (i$srcin) {
                                   lcolnb=i$colnb-1
                                   msize=i$colid+lcolnb
                                 }
                                 if (i$enable) {
                                   if (i$srcin) {
                                     rep=c(rep,(i$colid):(i$colid+i$colnb-1))
                                   }
                                   else{
                                     rep=c(rep,(msize+i$colid):(msize+i$colid+i$colnb-1))
                                   }
                                 }#if
                               }#for
                               return(rep)
                             },
                             getcolnames = function() {
                               "get matrix col name"
                               rep=vector()
                               for (i in .l) {
                                 if (i$colnb>1) {
                                   for (j in 1:i$colnb) {
                                     rep=c(rep,paste0(i$colname,j))
                                   }
                                 }else {
                                   rep=c(rep,i$colname)
                                 }
                               }#for
                               return(rep)
                             },
                             draw = function() {
                          "draw the objec value
                          \\subsection{Return Value}{returns a list of String object representing the value}"
                               rep=vector()
                               for (i in .l) {rep=c(rep,i$draw())}
                               return(rep)
                             }
                           )
)

#const
VersionLCATS="0.2.3"
VersionLAxytrek="0.2.3"
VersionLWacu="0.2.3"
VersionLLul="0.2.3"
VersionLDATA="0.1.0"

#' A Logger reference class
#' @field name logger display name
#' @field fileh5 h5 data file name
#' @field filebehavior behavior file name
#' @field besep behavior field separator character
#' @field besaturation the ‘saturation’ value from 0 to 1
#' @field uizoomstart uizoomstart default value
#' @field uizoomend uizoomend default value
#' @import hdf5r
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
                                    rtctick = "numeric",
                                    version = "character",
                                    uizoomstart = "numeric",
                                    uizoomend = "numeric",
                                    becolor = "character",
                                    beobslst = "list",
                                    behaviorchoices = "list",
                                    behaviorselected = "list" ),
                      methods = list(
                        initialize= function(fileh5 = "", filebehavior = "", besep=",", besaturation=0.2, uizoomstart=0, uizoomend=0, metricshow=NULL ) {
                          if(!is.character(fileh5)){
                            stop("fileh5 file path")
                          }else if (!1){ #is.h5file(fileh5)
                            stop("fileh5 is not h5 format")
                          } else {
                            fileh5<<-fileh5
                            name<<-tools::file_path_sans_ext(basename(fileh5))
                            filebehavior<<-filebehavior
                            options(digits.secs = 3)
                            nbrow<<-0
                            nbcol<<-Inf
                            accres<<-1
                            rtctick<<-1
                            version<<-"0.2.3"
                            h5init()
                            if (uizoomstart>0) {
                              uizoomstart<<-uizoomstart
                            }else {
                              uizoomstart<<-1
                            }
                            if (uizoomend>0) {
                              uizoomend<<-uizoomend
                            }else {
                              uizoomend<<-nbrow
                            }
                            behaviorinit(besep, besaturation)
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
                          "set external matrix"
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
                          "draw the objec value
                          \\subsection{Return Value}{returns a String object representing the value}"
                          return(paste("t:Logger fd:",fileh5))
                        },
                        initmetriclst = function() {
                          "set metric list for this logger class"
                          #definit les grandeurs par defaut
                          stop("initmetriclst virtual function should not be called directly")
                        },
                        h5init = function() {
                          "verify if h5 is correct version"
                          #get info from h5 file
                          datestart<<-as.POSIXct("2015-04-01", tz="GMT")
                          stop("h5init virtual function should not be called directly")
                        },
                        saveasloggerdata = function(fileld) {
                          m=getdata()
                          if (extmatrixenable) {
                            m=cbind(m,extmatrix)
                          }
                          lcol=metriclst$getcolactive()
                          m=m[,lcol]
                          #ecriture du fichier H5
                          if(file.exists(fileld)) file.remove(fileld)
                          h5f <- h5file(name = fileld, mode = "a")
                          h5f["data"]=t(m)
                          h5attr(h5f, "logger")="LDATA"
                          h5attr(h5f, "version")=VersionLDATA
                          h5attr(h5f, "datestart")=as.character.Date(datestart)
                          h5attr(h5f, "accres")=accres
                          h5attr(h5f, "rtctick")=rtctick
                          h5attr(h5f, "filesrc")=fileh5
                          #metrics
                          lmt=metriclst$getmatrix()
                          h5f["metriclst"]=lmt
                          h5f["metriccol"]=colnames(lmt)
                          h5close(h5f)
                        },
                        behaviorinit= function(besep, besaturation) {
                          "init behavior list event"
                          lchoices=list()
                          lslct=list()
                          becolor<<-""
                          lbeobslst=list()
                          if (nchar(filebehavior)>0) {
                            tryCatch({
                              dso=read.csv(filebehavior,sep = besep)
                              dsob=levels(factor(dso$Behavior))
                              #get number of different Behavior
                              i=0
                              for(o in dsob) {
                                i=i+1
                                lchoices[[o]]=i
                                lslct=c(lslct,i)
                              }
                              becolor<<-rainbow(n=i)
                              becolorgr=rainbow(n=i,s=besaturation)
                              #build Behavior obs list
                              for(i in 1:nrow(dso)) {
                                row=dso[i,]
                                lfrom=paste(datestart+row$Start..s.)
                                lto=paste(datestart+row$Stop..s.)
                                #verifier index Behavior< confnbcolor
                                lcode=lchoices[[row$Behavior]]
                                lcolor=substr(becolorgr[[lcode]],1,7)
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
                             version<<-VersionLCATS
                           },
                           h5init = function() {
                             #cat("init version cats")
                             #get info from h5 file
                             f=h5file(fileh5,"r")
                             #list.attributes(f)
                             if (h5attr(f, "logger")!="CATS") {
                               stop("h5 file not CATS structure")
                             }else if (h5attr(f, "version")!=version){
                               stop("CATS h5 file not good version")
                             }else {
                               dt=h5attr(f, "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=f[["data"]]
                               size=dset$dims
                               nbrow<<-size[2]
                               nbcol<<-size[1]
                               accres<<-h5attr(f, "accres")
                               rtctick<<-1
                             }
                             f$close_all()
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=t(f["/data"][,])
                             h5close(f)
                             colnames(m)=c("a1","a2","a3","g1","g2","g3","m1","m2","m3","t","p","l")
                             return(m)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("Accelerometer","a",1,3,beobs=TRUE))
                             lm$add(Metric("Gyroscope","g",4,3))
                             lm$add(Metric("Magnetometer","m",7,3))
                             lm$add(Metric("Temperature","t",10,1))
                             lm$add(Metric("Pression","p",11,1))
                             lm$add(Metric("Light intensity","l",12,1))
                             metriclst<<-lm
                           },
                           draw = function() {
                             return(paste0("t:LoggerCats f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerAxytrek reference class
#' @export LoggerAxytrek
#' @exportClass LoggerAxytrek
LoggerAxytrek <- setRefClass("LoggerAxytrek",
                         contains = list("Logger"),
                         fields = list(),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "",...) {
                             callSuper(fileh5, filebehavior,...)
                             version<<-VersionLAxytrek
                           },
                           h5init = function() {
                             #cat("init version Axytrek)
                             #get info from h5 file
                             f=h5file(fileh5,"r")
                             #list.attributes(f)
                             if (h5attr(f, "logger")!="AXYTREK") {
                               stop("h5 file not AXYTREK structure")
                             }else if (h5attr(f, "version")!=version){
                               stop("Axytrek h5 file not good version")
                             }else {
                               dt=h5attr(f, "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=f[["data"]]
                               size=dset$dims
                               nbrow<<-size[2]
                               nbcol<<-size[1]
                               accres<<-h5attr(f, "accres")
                               rtctick<<-1
                             }
                             h5close(f)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("Accelerometer","a",1,3,beobs=TRUE))
                             lm$add(Metric("Pression","p",4,1))
                             lm$add(Metric("Temperature","t",5,1))
                             metriclst<<-lm
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=t(f[["data"]][,])
                             h5close(f)
                             colnames(m)=c("a1","a2","a3","p","t")
                             return(m)
                           },
                           draw = function() {
                             return(paste0("t:LoggerAxytrek f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)

#' A LoggerLul reference class
#' @export LoggerLul
#' @exportClass LoggerLul
LoggerLul <- setRefClass("LoggerLul",
                          contains = list("Logger"),
                          fields = list(),
                          methods = list(
                            initialize = function(fileh5 = "", filebehavior = "",...) {
                              callSuper(fileh5, filebehavior,...)
                              version<<-VersionLLul
                            },
                            h5init = function() {
                              #cat("init version Lul")
                              #get info from h5 file
                              f=h5file(fileh5,"r")
                              #list.attributes(f)
                              if (h5attr(f, "logger")!="LUL") {
                                stop("h5 file not Lul structure")
                              }else if (h5attr(f, "version")!=version){
                                stop("LUL h5 file not good version")
                              }else {
                                dt=h5attr(f, "datestart")
                                datestart<<-as.POSIXct(dt, tz="GMT")
                                dset=f[["data"]]
                                size=dset$dims
                                nbrow<<-size[2]
                                nbcol<<-size[1]
                                accres<<-h5attr(f, "accres")
                                rtctick<<-h5attr(f, "rtctick")
                              }
                              h5close(f)
                            },
                            initmetriclst = function() {
                              lm=MetricList$new()
                              lm$add(Metric("Temperature","t",1,1,beobs=TRUE))
                              lm$add(Metric("Pression","p",2,1))
                              lm$add(Metric("Light intensity","l",3,1))
                              metriclst<<-lm
                            },
                            getdata= function() {
                              f=h5file(fileh5,"r")
                              m=t(f[["data"]][,])
                              h5close(f)
                              colnames(m)=metriclst$getcolnames()
                              return(m)
                            },
                            draw = function() {
                              return(paste0("t:LoggerLul f:",name," s:",datestart," r:",nbrow))
                            }
                          )
)

#' A LoggerWacu reference class
#' @export LoggerWacu
#' @exportClass LoggerWacu
LoggerWacu <- setRefClass("LoggerWacu",
                         contains = list("Logger"),
                         fields = list(),
                         methods = list(
                           initialize = function(fileh5 = "", filebehavior = "",...) {
                             callSuper(fileh5, filebehavior,...)
                             version<<-VersionLWacu
                           },
                           h5init = function() {
                             #cat("init version wacu")
                             #get info from h5 file
                             f=h5file(fileh5,"r")
                             #list.attributes(f)
                             if (h5attr(f, "logger")!="WACU") {
                               stop("h5 file not WACU structure")
                             }else if (h5attr(f, "version")!=version){
                               stop("WACU h5 file not good version")
                             }else {
                               dt=h5attr(f, "datestart")
                               datestart<<-as.POSIXct(dt, tz="GMT")
                               dset=f[["data"]]
                               size=dset$dims
                               nbrow<<-size[2]
                               nbcol<<-size[1]
                               accres<<-h5attr(f, "accres")
                               rtctick<<-h5attr(f, "rtctick")
                             }
                             h5close(f)
                           },
                           initmetriclst = function() {
                             lm=MetricList$new()
                             lm$add(Metric("Temperature","t",1,1))
                             lm$add(Metric("Pression","p",2,1))
                             lm$add(Metric("Light intensity","l",3,1))
                             lm$add(Metric("Accelerometer","a",4,3,beobs=TRUE))
                             metriclst<<-lm
                           },
                           getdata= function() {
                             f=h5file(fileh5,"r")
                             m=t(f[["data"]][,])
                             h5close(f)
                             colnames(m)=c("t","p","l","a1","a2","a3")
                             return(m)
                           },
                           draw = function() {
                             return(paste0("t:LoggerWacu f:",name," s:",datestart," r:",nbrow))
                           }
                         )
)


#' A LoggerData reference class
#' @export LoggerData
#' @exportClass LoggerData
LoggerData <- setRefClass("LoggerData",
                          contains = list("Logger"),
                          fields = list(),
                          methods = list(
                            initialize = function(fileh5 = "", filebehavior = "",...) {
                              callSuper(fileh5, filebehavior,...)
                              version<<-VersionLDATA
                            },
                            h5init = function() {
                              cat("init version loggerdata")
                              #get info from h5 file
                              f=h5file(fileh5,"r")
                              #list.attributes(f)
                              if (h5attr(f, "logger")!="LDATA") {
                                stop("h5 file not LDATA structure")
                              }else if (h5attr(f, "version")!=VersionLDATA){
                                stop("LDATA h5 file not good version")
                              }else {
                                dt=h5attr(f, "datestart")
                                datestart<<-as.POSIXct(dt, tz="GMT")
                                dset=f[["data"]]
                                size=dset$dims
                                nbrow<<-size[2]
                                nbcol<<-size[1]
                                accres<<-h5attr(f, "accres")
                                rtctick<<-h5attr(f, "rtctick")
                                #metriclst
                                lm=MetricList$new()
                                lmt=f[["metriclst"]][,]
                                for(r in 1:nrow(lmt)) {
                                  rname=lmt[r,1]
                                  rcolname=lmt[r,2]
                                  rcolid=as.numeric(lmt[r,3])
                                  rcolnb=as.numeric(lmt[r,4])
                                  renable=as.logical(lmt[r,5])
                                  rsrcin=as.logical(lmt[r,6])
                                  rbeobs=as.logical(lmt[r,7])
                                  rheight=as.numeric(lmt[r,8])
                                  nm=Metric$new(name=rname,colname=rcolname,colid=rcolid,colnb=rcolnb,height=rheight,enable=renable,beobs=rbeobs,srcin=rsrcin)
                                  lm$add(nm)
                                }
                                metriclst<<-lm
                              }
                              h5close(f)
                            },
                            initmetriclst = function() {
                              #rien a faire, voir h5init
                            },
                            getdata= function() {
                              f=h5file(fileh5,"r")
                              m=t(f[["data"]][,])
                              h5close(f)
                              colnames(m)=metriclst$getcolnames()
                              return(m)
                            },
                            draw = function() {
                              return(paste0("t:LoggerData f:",name," s:",datestart," r:",nbrow))
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
                             "add new node in the list."
                             .l<<-c(.l,node)
                           },
                           draw = function() {
                             "draw the objec value
                          \\subsection{Return Value}{returns a list of String object representing the value}"
                             rep=list()
                             for (i in .l) {rep=c(rep,i$draw())}
                             return(rep)
                           }
                         )
)


