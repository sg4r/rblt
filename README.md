# rblt
Le package rblt R-bio-logging-toolbox est une librairie de fonctions R de visualisation des données accéléros des bio-loggers AXYTREK, CATS, des  bio-loggers WACU, fabriqués par le service MIBE de l’IPHC http://www.iphc.cnrs.fr/-MIBE-.html ![rblt Logo](rblt.png)
Il est possible d’associé à ces données la vision des comportements des animaux enregistrés depuis le logiciel BORIS http://www.boris.unito.it/ 
# Installation
Installation depuis R, saisir les instructions suivantes, qui installeront la librairie rblt et ses dépendances.
```
install.packages("devtools")
devtools::install_github("sebastien45/rblt")
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
# Conversion des données
Dans la mesure où les différents bio-logger n’utilisent pas le même format de données, il est nécessaire de convertir les données au format utilisé par la librairie rblt.

## Pour les bio-logger CATS :
Convertissez les résultats de vos données au format csv avec la fonction rblt::cats2h5

### Exemple :
```
filecatscsv="~/rtoolbox/20180216-004210-CC-07-48_15-02-2017_1.csv"
filecatsh5="~/rtoolbox/CC-07-48_15-02-2017_1.h5"
rblt::cats2h5(filecatscsv,filecatsh5)
[1] "in: ~/rtoolbox/20180216-004210-CC-07-48_15-02-2017_1.csv"
[1] "out: ~/rtoolbox/CC-07-48_15-02-2017_1.h5"
[1] "nbrow: 17099"
filecatscsv="~/rtoolbox/20180214-222647-CC-07-48_14-02-2017_1.csv"
filecatsh5="~/rtoolbox/CC-07-48_14-02-2017_1.h5"
rblt::cats2h5(filecatscsv,filecatsh5)
[1] "in: ~/rtoolbox/20180214-222647-CC-07-48_14-02-2017_1.csv"
[1] "out: ~/rtoolbox/CC-07-48_14-02-2017_1.h5"
[1] "nbrow: 5868"
```

## Pour les bio-logger AXYTREK :
Convertissez les résultats de vos données du format csv avec la fonction rblt::axytrek2h5

### Exemple:
```
atreks1="~/rtoolbox/axytrec-s1.h5"
rblt::axytrek2h5("~/rtoolbox/AXYTREK2_S1.csv",atreks1)
[1] "in: ~/rtoolbox/AXYTREK2_S1.csv"
[1] "out: ~/rtoolbox/axytrec-s1.h5"
[1] "nbrow: 670051"
atreks2="~/rtoolbox/axytrec-s2.h5"
rblt::axytrek2h5("~/rtoolbox/AXYTREK5_S1.csv",atreks2)
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
wacu2h5dt("~/rtoolbox/wacu134_TRDDU_cc.txt",w134)
# voir wacu2csv pour la concersion des données accéléros en csv, puis
wacu2hacc3("~/rtoolbox/wacu134_TRDDU_cc_ACC.csv",w134)
```
# Fichiers de démonstration
Il est possible de générer des fichiers de démonstrations de différentes tailles afin de tester l’affichage d’un ensemble plus ou moins important d’enregistrements de données.

## Exemple :
```
#Pour des Bio-loggers CATS
rblt::democats2h5("~/rtoolbox/democats-10k-pts.h5",nbrow=10000)
rblt::democats2h5("~/rtoolbox/democats-2600k-pts.h5",nbrow=2600000)
#Pour des Bio-loggers AXYTREK
#reste a faire
#Pour des Bio-loggers WACU
wdemo="~/rtoolbox/wacudemo-1k.h5"
rblt::demowacu2h5(wdemo)
```
