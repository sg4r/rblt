#' A Logger reference class
#' @field filedata nom du fichier de donnée
#' @field filebehavior nom du chier des comportement
#' @export Logger
#' @exportClass Logger
#' @author sebastien geiger
Logger <- setRefClass("Logger",
                      fields = list(name = "character",
                                    filedata = "character",
                                    filebehavior = "character",
                                    behaviorchoices = "list",
                                    behaviorselected = "list" ),
                      methods = list(
                        initialize= function(filedata = "", filebehavior = "") {
                          filedata<<-filedata
                          name<<-basename(filedata)
                          filebehavior<<-filebehavior
                          behaviorinit()
                        },
                        draw = function() {
                          print(paste("t:Logger fd:",filedata))
                        },
                        behaviorinit= function() {
                          lchoices=list()
                          lslct=list()
                          if (nchar(filebehavior)>0) {
                            tryCatch({
                              dso=read.csv(filebehavior)
                              dsob=levels(dso$Behavior)
                              i=0
                              for(o in dsob) {
                                i=i+1
                                lchoices[[o]]=i
                                lslct=c(lslct,i)
                              }
                            }, error = function(c) {
                              c$message <- paste0(c$message, " (in ", filebehavior, ")")
                              stop(c)
                            })#try
                          }#if
                          behaviorchoices<<-lchoices
                          behaviorselected<<-lslct
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
                           initialize = function(filedata = "", filebehavior = "",nbrow=10) {
                             callSuper(filedata, filebehavior)
                             nbrow<<-nbrow
                           },
                           draw = function() {
                             print(paste("t:LoggerCats fd",filedata))
                           }
                         )
)
