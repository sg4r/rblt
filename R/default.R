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
  print(paste0("rblt[",getversion(),"]:Hello ",Sys.info()[["login"]][1],"!"))
}

#' A getversion function
#' @export getversion
getversion = function() {
  return("0.2.2")
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
    #inv pression
    invp=ldscats[,"p"]*(-1)
    ldscats[,"p"]=invp
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

#' A axytrek2h5 fonction for convert csv file to h5 file
#' @export axytrek2h5
axytrek2h5 = function(filecsv="",fileh5="") {
  if(!is.character(filecsv)){
    stop("filecsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filecsv))
    print(paste("out:",fileh5))
    ldsr1=fread(file = filecsv,fill = TRUE, dec = ",")
    ldsr2=fread(file = filecsv,fill = TRUE, dec = ".")
    ldsra=ldsr1[seq(1,nrow(ldsr1),15),c(2:6)]
    ldsrb=ldsr2[seq(1,nrow(ldsr2),15),c(8:9)]
    lds=cbind(ldsra,ldsrb)
    rm(ldsr1,ldsr2,ldsra,ldsrb)
    names(lds)=c("date","time","x","y","z","p","t")
    strdatestart=paste(lds[1,"date"],lds[1,"time"])
    print(strdatestart)
    datestart=as.POSIXct(strdatestart,format="%d/%m/%Y %H:%M:%OS",tz="GMT")
    nbrow=nrow(lds)
    print(paste("nbrow:",nbrow))
    #ecriture du fichier H5
#   ldm=data.matrix(lds[,c("x","y","z","p","t")])
    ldm=as.matrix(lds[,3:7])
    if(file.exists(fileh5)) file.remove(fileh5)
    h5f <- h5file(name = fileh5, mode = "a")
    h5f["data"]=ldm
    h5attr(h5f, "logger")="AXYTREK"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filecsv)
    h5close(h5f)
  }
}

#' A demoaxytrek2h5 fonction build demo cats h5 file
#' @export demoaxytrek2h5
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
    h5close(h5f)
  }
}


#' A wacu2h5old fonction for concert wacu csv file to h5 file
#' @export wacu2h5old
wacu2h5old = function(filewacucsv="",fileh5="") {
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
    h5f["/tpl"]=ldm
    h5attr(h5f, "logger")="WACU"
    h5attr(h5f, "version")=getversion()
    h5attr(h5f, "datestart")=as.character.Date(datestart)
    h5attr(h5f, "filesrc")=basename(filewacucsv)
    h5attr(h5f, "rtctick")=1
    h5close(h5f)
    rm(ldm)
  }
}

#' A wacu2hacc1 function for insert wacu acc csv file to h5 file
#' @export wacu2hacc1
wacu2hacc1 = function(filewacucsv= "", fileh5="", size=11274058, accfreq=25 ) {
# version rapide qui ne lit que les informations a la seconde
# pour préparer la ui et la démo
# je ferait pour trad;)
# readlines n'est pas rapide en ligne par ligne
# pas possible de lire le fichier de 9Go en ram
  library(svMisc)
  m=matrix(0,size,3)
  if(!is.character(filewacucsv)){
    stop("filewacucsv file path")
  }else if (!is.character(fileh5)){
    stop("fileh5 file path")
  }else {
    print(paste("in:",filewacucsv))
    print(paste("out:",fileh5))
    con = file(filewacucsv, "r")
    prmax=510000
    nbline=prmax*accfreq
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
      #print(paste0(nbline,":",nblinetick,"x",nblineacc,":",line))
      #affichage progression en pourcent
      progress(mid*100/prmax)
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
  #free local variable
  rm(m)
}
# system.time(wacu2hacc1(filewacucsv,w134))
# Timing stopped at: 4027 161.9 4025
# =>67.08333 minutes pour traiter 50% du fichier ACC de 9Go

# memo
# h5unlink(h5f,"acc1")
# h5f["acc1"]=ldm
# extendDataSet(h5f["acc1"],dims=c(288487438,3))


#' A wacu2hacc2 function for insert wacu acc csv file to h5 file
#' @export wacu2hacc2
wacu2hacc2 = function(filewacucsv= "", fileh5="", size=11274058, accfreq=25 ) {
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
    buffsize=10000000
    buffcpt=0
    buffcont=TRUE
    mid=1
    f=file(filewacucsv, "rb")
    while(buffcont) {
      buff=readChar(f, buffsize)
      buffcpt=buffcpt+1
      print(buffcpt)
      if (nchar(buff)<buffsize) {
        buffcont=FALSE
      }
      buffstr=strsplit(buff,"\r\n")
      for(l in buffstr[[1]]) {
        if (substr(l,1,1)==" ") {
          #read acc
          nblineacc=nblineacc+1
        }else {
          #est une ligne avec le temps
          #read time and acc
          nblineacc=1
          val=strsplit(l,"\t")
          acc=c(as.numeric(val[[1]][5]),as.numeric(val[[1]][6]),as.numeric(val[[1]][7]))
          m[mid,]=acc
          mid=mid+1
        }
      }#for
    }
    close(f)
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
  #free local variable
  rm(m)
}
# mesure du temps de traitement
# system.time(wacu2hacc2(filewacucsv,w134))
# user   system  elapsed
# 2719.274   15.107 2748.208
# => 45 minutes pour traiter le fichier ACC


# time python ./hwacug.py ./wacu134_TRDDU_cc.txt ./wacu134_TRDDU_cc_ACC.txt
# nbrow:11272094
# [[  184641.  1152739.        0.        0.        0.        0.]
#   [  184641.  1152739.        0.        0.        0.        0.]
#   [  184717.  1152739.        0.        0.        0.        0.]
#   [  184641.  1155506.        0.        0.        0.        0.]
#   [  184717.  1155506.        0.        0.        0.        0.]]
# [[  1.84641000e+05   1.15273900e+06   0.00000000e+00   1.60000000e+01
#     1.60000000e+01  -1.04700000e+03]
#   [  1.84641000e+05   1.15273900e+06   0.00000000e+00  -9.40000000e+01
#     2.66000000e+02  -1.00000000e+03]
#   [  1.84717000e+05   1.15273900e+06   0.00000000e+00  -7.80000000e+01
#     2.66000000e+02  -1.00000000e+03]
#   [  1.84641000e+05   1.15550600e+06   0.00000000e+00  -7.80000000e+01
#     2.66000000e+02  -1.00000000e+03]
#   [  1.84717000e+05   1.15550600e+06   0.00000000e+00  -9.40000000e+01
#     2.81000000e+02  -1.00000000e+03]]
# writing data...
#
# real	7m27,602s
# user	7m13,120s
# sys	0m7,509s
# version en python 7 minutes

#' A wacu2hacc function for insert wacu acc csv file to h5 file
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
    macc=fread(file=filewacucsv,header = F, select=c(5,6,7))
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


#' A demowacu2h5 fonction build demo cats h5 file
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
    h5close(h5f)
  }
}
