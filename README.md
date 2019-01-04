# rblt
Le package rblt R-bio-logging-toolbox est une librairie de fonctions R de visualisation des données accéléros des bio-loggers AXYTREK, CATS, des  bio-loggers WACU, fabriqués par le service MIBE de l’IPHC http://www.iphc.cnrs.fr/-MIBE-.html 
![rblt Logo](rblt.png)
Il est possible d’associé à ces données la vision des comportements des animaux enregistrés depuis le logiciel BORIS http://www.boris.unito.it/ 

# Installation
2 versions sont disponibles

## Version CRAN
Version stable en cours de soumission a CRAN
```
install.packages(rblt)
```
## Version de développement
Version de développement direction depuis github.
```
install.packages("devtools")
devtools::install_github("sg4r/rblt")
```
# Démonstration
Visualisation des métrics depuis un Bio-loggers CATS, AXYTREK et WACU.
Les données sont simulées pour avoir un apercus des fonctions de visualisations.
```
library(rblt)

#Pour des Bio-loggers CATS
cdemo10k="~/rtoolbox/democats-10k.h5"
cdemo10kbe="~/rtoolbox/democats-10kbe.csv"
cdemo2600k="~/rtoolbox/democats-2600k.h5"
cdemo2600kbe="~/rtoolbox/democats-2600kbe.csv"
rblt::democats2h5(cdemo10k)
rblt::democats2h5(cdemo2600k,nbrow=2600000)
rblt::democatsmkbe(fbe = cdemo10kbe,nbrow = 10, nbseq = 20)
rblt::democatsmkbe(fbe = cdemo2600kbe,nbrow = 10, nbseq = 20)
#Pour des Bio-loggers AXYTREK
ademo="~/rtoolbox/demoaxytrek-10k.h5"
rblt::demoaxytrek2h5(ademo)
#Pour des Bio-loggers WACU
wdemo="~/rtoolbox/wacudemo-10k.h5"
rblt::demowacu2h5(wdemo)
#definition des bio-loggers a afficher
ll=LoggerList$new()
ll$add(LoggerCats$new(cdemo10k,filebehavior=cdemo10kbe))
ll$add(LoggerCats$new(cdemo2600k))
ll$add(LoggerCats$new(cdemo2600k, filebehavior=cdemo2600kbe ,besep="," ))
ll$add(LoggerAxytrek$new(ademo))
ll$add(LoggerWacu$new(wdemo))
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
````
# Conversion des données
Dans la mesure où les différents bio-logger n’utilisent pas le même format de données, il est nécessaire de convertir les données au format utilisé par la librairie rblt.

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

# Utilisation
Créer un objet de la classe LoggerList qui va contenir la listes des fichiers de données a visualiser. Puis créer une vue avec l'object de la classe LoggerUI qui affichera les différentes données.
```
library(rblt)

l=LoggerList$new()
l$add(LoggerCats$new("~/rtoolbox/CC-07-48_14-02-2017_1.h5",filebehavior="~/rtoolbox/CC-07-48_14-02-2018.txt"))
l$add(LoggerCats$new("~/rtoolbox/CC-07-48_15-02-2017_1.h5"))
l$add(LoggerCats$new("~/rtoolbox/democats-10k-pts.h5"))
lui=LoggerUI$new(l)
lui$gui()
````

## Utilisation avancée :
exemples d'utilisation avancée

## Selection des metrics à afficher par défaut
Par défaut tous les métrics du type de Bio-loggers sont affichés.
Il est possible de limiter les métrics afficher en définissant le vecteur metricshow lors de l'initialisation d'un bio-logger
Utiliser T pour afficher le métric, et F pour le cacher
```
ll=LoggerList$new()
ll$add(LoggerCats$new("~/rtoolbox/democats2h5.h5",metricshow=c(T,F,T,F,F,F)))
#affichage des informations
ui=LoggerUI$new(ll)
ui$gui()
```

### Acces aux données
Utiliser getdata() pour avoir une copie de la matrice des données
```
lg=LoggerCats$new(cdemo10k)
lm=lg$getdata()
lm
```

### Ajout d'un metric
réccuperer les données, puis réaliser divers calcul sur la matrice, puis rajouter le résultat via setextmatrix et metriclst add
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

### Afficher la liste des metrics d'un logger
chaque type de Logger a des metrics par defaut. il est possible de les afficher avec le commande metriclst draw
```
lg=LoggerCats$new(cdemo10k)
lg$metriclst$draw()
```

### Changer la liste des metrics d'un logger
Il est possible de redéfinir l'ordre affichage des metrics, sous reserve d'avoir de bien indiquer les bons parramétres
Il est possible de redefinir l'affichage des comportements pour chaque metric
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

### Conversion des données
Lors de la conversion des données des fichiers CATS ou AXYTREK il est nécessaire d'indiquer dans la variable accres la fréquence d'échantillonnage utilisée lors de l'acquisition.
voir les fonction rblt::axytrek2h5 ou rblt::cats2h5



