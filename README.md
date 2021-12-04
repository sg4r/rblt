# rblt
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/rblt)](https://CRAN.R-project.org/package=rblt)
[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/rblt)](https://CRAN.R-project.org/package=rblt)
[![CRAN total downloads](https://cranlogs.r-pkg.org/badges/grand-total/rblt)](https://CRAN.R-project.org/package=rblt)
<!-- badges: end -->

## Introduction
Le package rblt R-bio-logging-toolbox est une application R-shiny de visualisation des données accéléros des bio-loggers AXYTREK, CATS, et des bio-loggers LUL et WACU, fabriqués par le service MIBE de l’IPHC 

Il est possible d’associé à ces données la vision des comportements des animaux enregistrés depuis le logiciel BORIS http://www.boris.unito.it/ 

![rblt main screen](rblt.png)

## Keywords
Biologging Accelerometer, Gyroscope, Magnetometer, Temperature, Pressure, Light intensity, Data visualization, GPS, biologger, movement ecology, Animal Tracking
Trajectory Analysis


## Publications
Behavioural inference from signal processing using animal-borne multi-sensor loggers: a novel solution to extend the knowledge of sea turtle ecology  
Lorène Jeantet, Víctor Planas-Bielsa, Simon Benhamou,  Sebastien Geiger, Jordan Martin, Flora Siegwalt, Pierre Lelong, Julie Gresser, Denis Etienne, Gaëlle Hiélard, Alexandre Arqué, Sidney Régis, Nicolas Lecerf,  Cédric Frouin, Abdelwahab Benhalilou, Céline Murgale, Thomas Maillet, Lucas Andreani, Guilhem Campistron, Hélène Delvaux, Christelle Guyon, Sandrine Richard, Fabien Lefebvre, Nathalie Aubert, Caroline Habold, Yvon Le Maho, Damien Chevallier  
https://royalsocietypublishing.org/doi/10.1098/rsos.200139

Fully Convolutional Neural Network: A solution to infer animal behaviours from multi-sensor data  
Lorène Jeantet, Vincent Vigon, Sébastien Geiger, Damien Chevallier  
https://doi.org/10.1016/j.ecolmodel.2021.109555

# Installation
2 versions sont disponibles

## Version CRAN
Version stable en cours de soumission a CRAN
```
install.packages(rblt)
```
## Version de développement
Version de développement accessible depuis github.
```
install.packages("devtools")
devtools::install_github("sg4r/rblt")
```
# Démonstration
Visualisation des métrics depuis un Bio-loggers CATS, AXYTREK, LUL et WACU.
Les données sont simulées pour avoir un apercus des fonctions de visualisations.
```
library(rblt)
rblt::demo_gui()
````
# Utilisation
Créer un objet de la classe LoggerList qui va contenir la listes des fichiers de données a visualiser. Puis créer une vue avec l'object de la classe LoggerUI qui affichera les différentes données.
```
library(rblt)

cdemo10k="~/rtoolbox/democats-10k.h5"
l=LoggerList$new()
l$add(LoggerCats$new("~/rtoolbox/CC-07-48_14-02-2017_1.h5",filebehavior="~/rtoolbox/CC-07-48_14-02-2018.txt"))
l$add(LoggerCats$new("~/rtoolbox/CC-07-48_15-02-2017_1.h5"))
l$add(LoggerCats$new(cdemo10k))
lui=LoggerUI$new(l)
lui$gui()
````

## Utilisation avancée :
Différents exemples d'utilisation avancée

### Afficher la liste des metrics d'un logger
chaque type de Logger a des metrics par defaut. il est possible d'afficher la liste de metrics avec le commande metriclst draw
```
lg=LoggerCats$new(cdemo10k)
lg$metriclst$draw()
```
## Sélection des metrics à afficher par défaut
Par défaut tous les métrics du type de Bio-loggers sont affichés.
Il est possible de limiter les métrics afficher en définissant le vecteur 'metricshow' lors de l'initialisation d'un bio-logger
Utiliser T pour afficher le métric, et F pour le cacher
```
ll=LoggerList$new()
ll$add(LoggerCats$new("~/rtoolbox/democats2h5.h5",metricshow=c(T,F,T,F,F,F)))
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
```

### Changer la liste des metrics d'un logger
Il est possible de redéfinir l'ordre affichage des metrics, sous reserve d'avoir bien indiqué les bons parramétres.
Pour cela il est nécéssaire de redéfinir la liste de metrics a afficher et leur ordre. Lorsque le boolean 'beobs' est définit a 'TRUE' les comportements sont affichés pour ce metric.
```
lg=LoggerCats$new(cdemo10k,filebehavior=cdemo10kbe)
lg$metriclst$draw()

lm=MetricList$new()
lm$add(Metric("Gyroscope",4,3,beobs=TRUE))
lm$add(Metric("Magnetometer",7,3))
lm$add(Metric("Accelerometer",1,3,beobs=TRUE))
lg$metriclst=lm

ll=LoggerList$new()
ll$add(lg)
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
```
### Acces aux données
Utiliser getdata() pour avoir une copie de la matrice des données
```
lg=LoggerCats$new(cdemo10k)
lm=lg$getdata()
head(lm)
```

### Ajout d'un metric
réccuperer les données, puis réaliser divers calcul sur la matrice. Il est possible de visualiser les nouveaux metrics en rajoutant les resultat via la fonction 'Pour rajouter le résultat via 'setextmatrix', puis en ajoutant les informations concernant les metrics via la fonction 'metriclst$add'
Exemple pour calculer la moyenne mobile avec un LoggerCats
```
library(caTools)

lg=LoggerCats$new(cdemo10k)
lm=lg$getdata()
lt=lm[,"l"]
ltrm5=runmean(lt, 5)
ltrm10=runmean(lt, 10)
ltrm20=runmean(lt, 20)
extm=cbind(lt,ltrm5,ltrm10,ltrm20)
lg$setextmatrix(extm)
lg$metriclst$add(Metric(name="RunMeanLight",colid=1,colnb=4,srcin=FALSE))
#creation de la liste des logger a afficher
ll=LoggerList$new()
ll$add(lg)
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
```
### Enregistrement des metrics
A partir de la version 0.4, il est possible d'enregistrer une selection des metrics dans un fichier de donné. Les métrics calculés peuvent également être inclus dans la selection.
```
#utiliser l'exemple précédant pour calculer la moyenne mobile
metricshow=c(F,F,F,T,F,F,T)
lg$metriclst$slctset(metricshow)
ldata="~/rtoolbox/ldata.h5"
lg$saveasloggerdata(ldata)
```

### Lecture des metrics
Pour relire les donner enregistrer depuis la fonction 'saveasloggerdata' il est nécéssaire d'utiliser le Logger type 'LoggerData'. Le 'LoggerData' est une structure permettant de stocker plusieurs metrics dans un même fichier de donné.
```
ll=LoggerList$new()
ll$add(LoggerData$new(ldata))
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
```

# Conversion des données
Le package 'rblt' permet de générer les fichiers données depuis les fichiers brute. Dans la mesure où les différents bio-logger n’utilisent pas le même format de données, il est nécessaire de convertir les données au format utilisé par la librairie 'rblt'.

## Pour les bio-logger CATS :
Convertissez les résultats de vos données au format csv avec la fonction rblt::cats2h5

### Exemple :
```
filecatscsv="~/rtoolbox/20180216-004210-CC-07-48_15-02-2017_1.csv"
filecatsh5="~/rtoolbox/CC-07-48_15-02-2017_1.h5"
rblt::cats2h5(filecatscsv,50,filecatsh5)
[1] "in: ~/rtoolbox/20180216-004210-CC-07-48_15-02-2017_1.csv"
[1] "out: ~/rtoolbox/CC-07-48_15-02-2017_1.h5"
[1] "nbrow: 17099"
filecatscsv="~/rtoolbox/20180214-222647-CC-07-48_14-02-2017_1.csv"
filecatsh5="~/rtoolbox/CC-07-48_14-02-2017_1.h5"
rblt::cats2h5(filecatscsv,50,filecatsh5)
[1] "in: ~/rtoolbox/20180214-222647-CC-07-48_14-02-2017_1.csv"
[1] "out: ~/rtoolbox/CC-07-48_14-02-2017_1.h5"
[1] "nbrow: 5868"
```

## Pour les bio-logger AXYTREK :
Convertissez les résultats de vos données du format csv avec la fonction rblt::axytrek2h5

### Exemple:
```
atreks1="~/rtoolbox/axytrec-s1.h5"
rblt::axytrek2h5("~/rtoolbox/AXYTREK2_S1.csv",25,atreks1)
[1] "in: ~/rtoolbox/AXYTREK2_S1.csv"
[1] "out: ~/rtoolbox/axytrec-s1.h5"
[1] "nbrow: 670051"
atreks2="~/rtoolbox/axytrec-s2.h5"
rblt::axytrek2h5("~/rtoolbox/AXYTREK5_S1.csv",25,atreks2)
[1] "in: ~/rtoolbox/AXYTREK5_S1.csv"
[1] "out: ~/rtoolbox/axytrec-s2.h5"
[1] "nbrow: 2234282"
```

## Pour les bio-logger LUL :
Convertissez les résultats de vos données du format csv avec la fonction rblt::lul2h5

### Exemple :
```
# a completer
```

## Pour les bio-logger WACU :
Convertissez les résultats de vos données au format csv avec la fonction rblt::wacu2h5dt
Pour ajouter les informations accelero, il est nécéssaire d'utiliser l'utilitaire en C++ wacu2csv

### Exemple :
```
w134="~/rtoolbox/wacu134.h5"
wacu2h5("~/rtoolbox/wacu134_TRDDU_cc.txt",w134)
# voir wacu2csv pour la concersion des données accéléros en csv, puis
wacu2hacc("~/rtoolbox/wacu134_TRDDU_cc_ACC.csv",w134)
```
### Conversion des données
Lors de la conversion des données des fichiers CATS ou AXYTREK il est nécessaire d'indiquer pour le moment dans la variable 'accres' la fréquence d'échantillonnage utilisée lors de l'acquisition.
voir les fonction rblt::axytrek2h5 ou rblt::cats2h5
