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
library("data.table")

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
  return("0.2.1")
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
    lds=read.csv(file=filecatscsv,check.names = F, stringsAsFactors=F)
    ldscats=lds[seq(1,nrow(lds),50),c(1:2,5:14,20,22)]
    rm(lds)
    names(ldscats)=c("date","time","ax","ay","az","gx","gy","gz","mx","my","mz","t","p","l")
    ldscats[,"id"]=as.POSIXct(paste(ldscats[,"date"],ldscats[,"time"]),format="%d.%m.%Y %H:%M:%OS",tz="GMT")
    datestart=ldscats[1,"id"]
    ldscats[,"tick"]=as.numeric(ldscats[,"id"]-datestart)
    nbrow=nrow(ldscats)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
    ldm=data.matrix(ldscats[,c("ax","ay","az","gx","gy","gz","mx","my","mz","t","p","l","tick")])
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="CATS"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filecatscsv)
    h5close(h5f)
  }
}

#' A democats2h5 fonction build demo cats h5 file
#' @export democats2h5
democats2h5 = function(fileh5="",nbrow=10000) {
  if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("out:",fileh5))
    xseq=seq(1,nbrow)
    ds=data.frame(xseq)
    #acc
    ds[,2]=nbrow-ds[,1]
    ds[,3]=nbrow/2
    #g
    ds[,4]=ds[,1]
    ds[,5]=ds[,2]
    ds[,6]=ds[,3]
    #m
    ds[,7]=ds[,1]
    ds[,8]=ds[,2]
    ds[,9]=ds[,3]
    #tpl
    ds[,10]=ds[,1]
    ds[,11]=ds[,2]
    ds[,12]=ds[,3]
    datestart=Sys.time()
    #ecriture du fichier H5
    ldm=data.matrix(ds)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="CATS"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")="democats2h5"
    h5close(h5f)
  }
}

democatsmkbe = function(fbe="",nbrow=10,nbseq=2) {
  if (!is.character(fbe)){
    stop("fbe must file path")
  }else {
    if(file.exists(fbe)) file.remove(fbe)
    s=seq.int(1,nbrow,length.out = nbseq)
    rep="Observation id,Observation date,Media file,Total length,FPS,Subject,Behavior,Modifiers,Behavior type,Start (s),Stop (s),Duration (s),Comment start,Comment stop"
    for(i in 1:(length(s)-1)) {
      deb=s[i]
      fin=s[i+1]
      dure=fin-deb
      ben=paste0("event-",i)
      rep=c(rep,paste(i,"1-1-1990,demo,100,20,sub,",ben,",STATE",deb,fin,dure,",",sep=","))
    }
    fileConn<-file(fbe)
    writeLines(rep,fileConn)
    close(fileConn)
  }
}

#' A wacu2h5 fonction for concert wacu csv file to h5 file
#' @export wacu2h5
wacu2h5 = function(filewacucsv="",fileh5="") {
  if(!is.character(filewacucsv)){
    stop("filewacucsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filewacucsv))
    print(paste("out:",fileh5))
    lds=read.csv(file=filewacucsv,check.names = F, colClasses=c("character","character","numeric","numeric","numeric"),skip = 24,header = F, sep="\t")
    ldswacu=lds[,c(1:5)]
    rm(lds)
    names(ldswacu)=c("date","time","x","y","z")
    strdatestart=paste(ldswacu[1,"date"],ldswacu[1,"time"])
    print(strdatestart)
    datestart=as.POSIXct(strdatestart,format="%d/%m/%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(ldswacu)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
    ldm=data.matrix(ldswacu[,c("x","y","z")])
    rm(ldswacu)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    #h5f["/data", "data", chunksize = c(4096,1), maxdimensions=c(nrow(ldm), ncol(ldm)), compression = 6]=ldm
    h5f["/data", "data"]=ldm
    h5attr(h5f, "logger")="WACU"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filewacucsv)
    h5close(h5f)
    rm(ldm)
  }
}

#' A wacu2h5dt fonction for concert wacu csv file to h5 file
#' @export wacu2h5dt
wacu2h5dt = function(filewacucsv="",fileh5="") {
  if(!is.character(filewacucsv)){
    stop("filewacucsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filewacucsv))
    print(paste("out:",fileh5))
    lds=fread(file=filewacucsv,skip = 24,header = F, sep="\t")
    names(lds)=c("date","time","x","y","z","v1")
    strdatestart=paste(lds[1,"date"],lds[1,"time"])
    print(strdatestart)
    datestart=as.POSIXct(strdatestart,format="%d/%m/%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(lds)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
    ldm=data.matrix(lds[,c("x","y","z")])
    rm(lds)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    #h5f["/data", chunksize = c(4096,1), maxdimensions=c(nrow(ldm), ncol(ldm)), compression = 6]=ldm
    h5f["/data"]=ldm
    h5attr(h5f, "logger")="WACU"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filewacucsv)
    h5attr(h5f, "rtctick")=1
    h5close(h5f)
    rm(ldm)
  }
}

#' A wacu2haccl function for insert wacu acc csv file to h5 file
#' @export wacu2haccl
wacu2haccl = function(filewacucsv= "", fileh5="", size=11274058, accfreq=25 ) {
# version rapide qui ne lit que les informations a la seconde
# pour préparer la ui et la démo
# je ferait pour trad;)
  m=matrix(0,size,3)
  if(!is.character(filewacucsv)){
    stop("filewacucsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filewacucsv))
    print(paste("out:",fileh5))
    con = file(filewacucsv, "r")
    nbline=510
    nblinetick=0
    nblineacc=1
    mid=1
    #file has lines
    line = readLines(con, n = 1)
    #head
    line = readLines(con, n = 1)
    while ( nbline ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }
      if (substr(line,1,1)==" ") {
        #read acc
        nblineacc=nblineacc+1
      }else {
        #est une ligne avec le temps
        #read time and acc
        nblineacc=1
        nblinetick=nblinetick+1
        val=strsplit(line,"\t")
        acc=c(as.numeric(val[[1]][5]),as.numeric(val[[1]][6]),as.numeric(val[[1]][7]))
        m[mid,]=acc
        mid=mid+1
      }
      print(paste0(nbline,":",nblinetick,"x",nblineacc,":",line))
      nbline=nbline-1
    }
    close(con)
  }
  h5f <- h5file(name = fileh5, mode = "a")
  l=list.datasets(h5f)
  if ("/acc" %in% l) {
    stop("ERROR : Dataset existing at location")
  }else {
    h5f["/acc"]=m
    h5attr(h5f, "acctype")=1
    h5attr(h5f, "accfreq")=accfreq
    h5attr(h5f, "accsize")=size
  }
  h5close(h5f)
}

# h5unlink(h5f,"acc1")
# h5f["acc1"]=ldm
# extendDataSet(h5f["acc1"],dims=c(288487438,3))
