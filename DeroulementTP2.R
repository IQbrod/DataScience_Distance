## 1 Exploration des données ----
# a) Recuperation des données
data("USArrests")
summary(USArrests)

# b) Analyse des résultats
label<-attributes(USArrests)$row.names
label
# Recupère le noms des variables du jeu de données
boxplot(USArrests)
# Trace un diagramme avec min/max/moyenne etc...(boite à moustache) pour chaque attribut
plot(USArrests)
# Trace tous les graphiques possibles pour le jeu de donnée

# c) Interpretation des relations
# Murder, Assault => Dépendance linéaire forte positive
# UrbanPop, Murder => Indépendance
# Rape, Murder => Indépendance


## 2 Similaires et Differents ----
# a) Fonctionnement de dist
?dist
# Retourne une matrice de distance
D=dist(USArrests, method="euclidean", diag=FALSE,upper=FALSE,p=2)
is.matrix(D)
is.data.frame(D)
# D n'est ni une matrice ni un dataframe
attributes(D)
Dmat<-as.matrix(D)
# Dmat contient la distance de cahque élément avec les autres
Dmat[1:6,1:6]
Dmat<-print(round(as.matrix(D),digits=0))
# Arrondi les différences (données de la matrice)
Dmat[1:6,1:6]

# b) Difference D - DMat
min(D)
min(Dmat)
# min(D) = 2.29 et min(Dmat) = 0
# D ne calcule pas la différence entre deux memes éléments
# Dmat est la représentation matricielle des différences entres éléments
# Donc Dmat contient la différence d'un élément avec lui même : 0

# c) Extraction
# !! ATTENTION il faut enlever les arrondis pour que les commandes fonctionnent
Dmat<-as.matrix(D)
(1:2500)[Dmat == min(D)]
# Renvoie les identifiants des cellules qui contiennent le minimum entre 1 & 2500 de la matrice (de 1 à N)
col(Dmat)[Dmat == min(D)]
row(Dmat)[Dmat == min(D)]
# Récupère les colonnes/lignes auxquelle Dmat vaut le min(D)
# Hors comme la matrice est symétrique le couple [15,29] est le même que [29,15]

# d) Intégration dans fonction
minDiffDonnées <- function(d) {
  dd = dist(d, method="euclidean", diag=FALSE,upper=FALSE,p=2)
  dmat<-as.matrix(dd)
  label<-attributes(d)$row.names
  x<-col(dmat)[dmat == min(dd)]
  label[x]
}
maxDiffDonnées <- function(d) {
  dd = dist(d, method="euclidean", diag=FALSE,upper=FALSE,p=2)
  dmat<-as.matrix(dd)
  label<-attributes(d)$row.names
  x<-col(dmat)[dmat == max(dd)]
  label[x]
}
# La fonction renvoie le noms des éléments les plus similaires
minDiffDonnées(USArrests)
# Dans USArrests, c'est l'Iowa et le New Hampshire qui sont les moins différents en terme de criminalité
maxDiffDonnées(USArrests)
# Dans USArrests, c'est la Floride et le North Dakota qui sont les plus différents en terme de criminalité

## 3 Representation des distances ----
# a) Analyse de la commande cmdscale
Cord<-cmdscale(D)
Cord
# Représentation en 2 dimension des distances entre les données
label<-attributes(Cord)$dimnames[[1]]
plot(Cord,type="n")
text(Cord,label)
# Trace un plot avec les labels en guise de points sur le repère orthonormé

# b) Résultats
# Il est difficile d'observer le minimum mais on observe clairement le maximum

Cord3<-cmdscale(D, k=3)
label3<-attributes(Cord3)$dimnames[[1]]
# Représentation 3D des distances
plot(Cord3[,1],Cord3[,3],type="n")
text(Cord3[,1],Cord3[,3],label3,cex=0.7)
# De même, les similarités sont difficilement observables mais semblent représenter les données
# Toutefois un biais s'expliquent par la dimension non modélisée par le plot.


## 4 Analyse des preferences ----
# a) Application de dist
# Dans le cas de préférences dist n'est pas la bonne approche puisque les valeurs de 1 à 8
# représentant les groupes n'ont d'apport que le rang et non pas une valeur numérique.
# Le 1 n'est pas opposé au 8.

# b) Utilisation du rang (par order)
pref<-read.table("PrefMusique.txt")
rang<-t(apply(pref,1,order))
# Rang contient pour chaque étuidant et chaque groupe le poids de sa préférence.
# [1,1] = 5 -> L'étudiant n°1 préfère le groupe n°1 en 5° position
colnames(rang)<-c("U2","ABBA","Hendrix","Chaunoir","Zappa","Doors","Marley","Ferre")
# Associe les noms de groupe
DR<-dist(rang)
# Calcul de la distance puisque les données sont maintenant significatives, 1 est opposé à 8
attributes(DR)
boxplot(rang)

# c) Calcul des plus/moins différents
maxDiffDonnées <- function(d) {
  dd = dist(d, method="euclidean", diag=FALSE,upper=FALSE,p=2)
  dmat<-as.matrix(dd)
  col(dmat)[dmat == max(dd)]
}
minDiffDonnées <- function(d) {
  dd = dist(d, method="euclidean", diag=FALSE,upper=FALSE,p=2)
  dmat<-as.matrix(dd)
  col(dmat)[dmat == min(dd)]
}

maxDiffDonnées(rang)
minDiffDonnées(rang)

# d) Calcul coefficient Speerman