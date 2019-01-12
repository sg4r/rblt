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

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' A getversion function
#' @export getversion
getversion = function() {
  return("0.2.3")
}

#' A sayhello function
#' @export sayhello
sayhello <- function() {
  print(paste0("rblt[",getversion(),"]:Hello ",Sys.info()[["user"]][1],"!"))
}

#' A cats2h5 function for convert csv file to h5 file
#' @import utils
#' @param filecsv  A input cats csv file.
#' @param accres input resolution
#' @param fileh5 A output h5 data file.
#' @export cats2h5
cats2h5 = function(filecsv="",accres=50, fileh5="" ) {
  if(!is.character(filecsv)){
    stop("filecsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filecsv))
    print(paste("out:",fileh5))
    ldsr=data.table::fread(file = filecsv, dec = ",", select=c(1:2,5:14,20,22), colClasses=list(character=1:2,numeric=5:14,20,22) )
    strdatestart=paste(ldsr[1,1],ldsr[1,2])
    datestart=as.POSIXct(strdatestart,format="%d.%m.%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(ldsr)
    print(paste("nbrow:",nbrow))
    #change default value
    ldm=data.matrix(ldsr[,c(3:14)])
    ldsr=ldm[,13]*(-1)
    ldm[,13]=ldsr
    #ecriture du fichier H5
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="CATS"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "accres")=accres
    h5attr(h5f, "filesrc")=basename(filecsv)
    h5close(h5f)
  }
}

#' A democats2h5 function build demo cats h5 file
#' @param fileh5 imput data h5 file
#' @param nbrow number of row
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
    h5attr(h5f, "accres")=1
    h5close(h5f)
  }
}

#' A democatsmkbe function for generate ramdom data
#' @param fbe  A outout be csv file.
#' @param nbrow input number of data rate in 1 seconde
#' @param nbseq input sequence lenght
#' @export democatsmkbe
#' @examples
#' democatsmkbe("democatsmkbe",nbrow=10,nbseq=2)
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
      rep=c(rep,paste(i,"1-1-1990,demo,100,20,sub",ben,"STATE,",deb,fin,dure,"0,0",sep=","))
    }
    fileConn<-file(fbe)
    writeLines(rep,fileConn)
    close(fileConn)
  }
}

#' A axytrek2h5 function for convert csv file to h5 file
#' @param filecsv  A input axytrek csv file.
#' @param accres input number of data rate in 1 seconde
#' @param fileh5 A output h5 data file.
#' @export axytrek2h5
axytrek2h5 = function(filecsv="", accres=25, fileh5="") {
  if(!is.character(filecsv)){
    stop("filecsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filecsv))
    print(paste("out:",fileh5))
    ldsr1=data.table::fread(file = filecsv,fill = TRUE, dec = ",", select=c(2:6), colClasses=list(character=2:3,numeric=4:6))
    ldsr2=data.table::fread(file = filecsv,fill = TRUE, dec = ".", select=c(8:9), colClasses=list(numeric=8:9))
    lds=cbind(ldsr1,ldsr2)
    rm(ldsr1,ldsr2)
    names(lds)=c("date","time","x","y","z","p","t")
    strdatestart=paste(lds[1,"date"],lds[1,"time"])
    print(strdatestart)
    datestart=as.POSIXct(strdatestart,format="%d/%m/%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(lds)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
    ldm=data.matrix(lds[,c(3:7)])
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="AXYTREK"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filecsv)
    h5attr(h5f, "accres")=accres
    h5close(h5f)
  }
}

#' A demoaxytrek2h5 function build demo cats h5 file
#' @param fileh5 input data H5 file
#' @param nbrow number of row
#' @export demoaxytrek2h5
#' @examples
#' demoaxytrek2h5("demoaxy.h5",nbrow=100)
demoaxytrek2h5 = function(fileh5="",nbrow=10000) {
  if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("out:",fileh5))
    xseq=seq(1,nbrow)
    ds=data.frame(xseq)
    #acc
    ds[,2]=nbrow-ds[,1]
    ds[,3]=nbrow/2
    #t,p
    ds[,4]=ds[,1]
    ds[,5]=ds[,2]
    datestart=Sys.time()
    #ecriture du fichier H5
    ldm=data.matrix(ds)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="AXYTREK"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")="demoaxytrek2h5"
    h5attr(h5f, "accres")=1
    h5close(h5f)
  }
}

#' A wacu2h5 function for concert wacu csv file to h5 file
#' @param filecsv  A input WACU csv file.
#' @param fileh5 A output h5 data file.
#' @export wacu2h5
wacu2h5 = function(filecsv="",fileh5="") {
  if(!is.character(filecsv)){
    stop("filecsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filecsv))
    print(paste("out:",fileh5))
    lds=data.table::fread(file=filecsv,skip = 24,header = F, sep="\t")
    names(lds)=c("date","time","t","p","l","v1")
    strdatestart=paste(lds[1,"date"],lds[1,"time"])
    print(strdatestart)
    datestart=as.POSIXct(strdatestart,format="%d/%m/%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(lds)
    print(paste("nbrow:",nbrow))
    #change default value
    ldm=lds[,"t"]/10
    lds[,"t"]=ldm
    ldm=lds[,"p"]*(-1)
    lds[,"p"]=ldm
    #ecriture du fichier H5
    ldm=data.matrix(lds[,c("t","p","l")])
    rm(lds)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    #h5f["/data", chunksize = c(4096,1), maxdimensions=c(nrow(ldm), ncol(ldm)), compression = 6]=ldm
    h5f["/tpl"]=ldm
    h5attr(h5f, "logger")="WACU"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filecsv)
    h5attr(h5f, "rtctick")=1
    h5attr(h5f, "accres")=1
    h5close(h5f)
    rm(ldm)
  }
}

#' A wacu2hacc function for insert wacu acc csv file to h5 file
#' @param filewacucsv  A input WACU csv file.
#' @param fileh5 A output h5 data file.
#' @param size the default data size
#' @param accfreq the default acc frequence
#' @export wacu2hacc
wacu2hacc = function(filewacucsv= "", fileh5="", size=11274058, accfreq=25 ) {
  # version rapide qui ne lit que les informations a la seconde
  # pour préparer la ui et la démo
  # evolution de la version 2, mais en utilisant wacu2csv en C++ pour reformater les data au format CSV
  if(!is.character(filewacucsv)){
    stop("filewacucsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filewacucsv))
    print(paste("out:",fileh5))
    #read wacu acc data
    macc=data.table::fread(file=filewacucsv,header = F, select=c(5,6,7))
    h5f=h5file(fileh5,"a")
    if (h5attr(h5f["/"], "logger")!="WACU") {
      stop("h5 file not WACU structure")
    }else if (h5attr(h5f["/"], "version")!=getversion()){
      stop("WACU h5 file not good version")
    }else {
      #ok all data
      mtpl=h5f["/tpl"][,1:3]
      m=cbind(mtpl,macc)
      m=as.matrix(m)
      #write new value in "/data
      h5f["/data"]=m
      h5close(h5f)
    }#end else if
  }
}


#' A demowacu2h5 function build demo cats h5 file
#' @param fileh5 A h5 data file.
#' @param nbrow number of row
#' @export demowacu2h5
demowacu2h5 = function(fileh5="",nbrow=10000) {
  if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("out:",fileh5))
    t=seq(1,60*10,1)
    x=t/100
    y=sin(t/10)+2
    z=tan(t/25)/40+3.5
    mx=matrix(x,ncol = 1)
    my=matrix(y,ncol = 1)
    mz=matrix(z,ncol = 1)
    m=data.frame(mx,my,mz,mx,my,mz)
    nbech=60*10
    nbloo=round(nbrow/nbech)
    w=m
    for (i in 1:nbloo) {
      w=rbind(w,m)
    }
    datestart=Sys.time()
    #ecriture du fichier H5
    ldm=data.matrix(w)
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="WACU"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")="demowacu2h5"
    h5attr(h5f, "accres")=1
    h5close(h5f)
  }
}
