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

# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' A sayhello function
#' @export sayhello
sayhello <- function() {
  print("rblt:Hello, world!")
}

#' A getversion function
#' @export getversion
getversion = function() {
  print("rblt_version: 0.1.0")
}

#' A cats2h5 fonction for concert cats csv file to h5 file
#' @export cats2h5
cats2h5 = function(filecatscsv="",fileh5="") {
  if(!is.character(filecatscsv)){
    stop("filecatscsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filecatscsv))
    print(paste("out:",fileh5))
    lds=read.csv(file=filecatscsv,colClasses=c("Date..UTC."="character","Time..UTC."="character","Accelerometer.X..m.s²."="numeric"))
    ldscats=lds[seq(1,nrow(lds),50),c(1:2,5:7)]
    rm(lds)
    names(ldscats)=c("date","time","x","y","z")
    ldscats[,"id"]=as.POSIXct(paste(ldscats[,"date"],ldscats[,"time"]),format="%d.%m.%Y %H:%M:%OS",tz="GMT")
    datestart=ldscats[1,"id"]
    ldscats[,"tick"]=as.numeric(ldscats[,"id"]-datestart)
    nbrow=nrow(ldscats)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
    ldm=data.matrix(ldscats[,c("x","y","z","tick")])
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="CATS"
    h5attr(h5f, "version")="1.0"
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filecatscsv)
    h5close(h5f)
  }
}
