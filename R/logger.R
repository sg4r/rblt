#' A Logger reference class
#' @field filedata nom du fichier de donnée
#' @field filebehavior nom du chier des comportement
#' @export Logger
#' @exportClass Logger
#' @author sebastien geiger
Logger <- setRefClass("Logger",
                      fields = list(name = "character",
                                    filedata = "character",
                                    filebehavior = "character"),
                      methods = list(
                        initialize= function(filedata = "", filebehavior = "") {
                          filedata<<-filedata
                          name<<-filedata
                          filebehavior<<-filebehavior
                        },
                        draw = function() {
                          print(paste("t:Logger fd:",filedata))
                        }
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
