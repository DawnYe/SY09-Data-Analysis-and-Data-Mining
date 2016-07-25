# CRAN = Comprehensive R Archive Network

library(sm) # Chargement de sm

# Manipulation de nombres
print((8*5)/(2*2)) # 10
print(x<-4)
print(y<-5)
print(z<-x*y)
print(sin(pi*x))
print(ls()) # Environnement de travail : 
# les jeux de données et fonctions y étant définies

# Manipulation de vecteurs et de matrices
A<-matrix(1:9,nrow=3,byrow=T)
B<-matrix(c(5,3,7,4,6,3,1,6,3,2,8,5),nrow=4,byrow=T)
print(A)
print(B)

print(dim(A)) # Vecteurs contenant la taille de A
na<-dim(A)[1] # Nb de lignes de A
pa<-dim(A)[2] # Nb de colonnes de A
v<-1:3 # Vecteur contenant les valeurs de 1 à 3
# similaire à v<-c(1,2,3)
print(v)

C<-cbind(A,v) # Concaténation de matrices avec agrégation par colonnes
D<-rbind(A,B) # Concaténation de matrices avec agrégation par lignes
print(C)
print(D)


# Extraction des éléments, des lignes ou des colonnes d'une matrice :
print(dim(C))
print(dim(D))
print(D[3,3])
print(D[,2])
print(D[5,])
print(D[2:4,2:3])

print(diag(3)) # Matrice identité appartenant à M(3,3)
print(diag(v)) # Matrice de diagonale v
print(diag(A)) # Extraction de la diagonale de A

print(matrix(1,3,2)) # Création de matrice appartenant à M(3,2) 
print(t(A)) # Transposée de matrice
A[,2]<-v # Remplacement des éléménts d'une colonne par les éléments d'un vecteur
print(A) 

print(B%*%A) # Matrix product
print(A%*%A) # Matrix product 
print(A*A) # Element wise operation
print(A+A) # Element wise operation 

# L'opération %*% est une produit matricielle tandis 
# que l'opération * est un produit élement par élement 

E <- array(1,c(4,3,2))
print(E)
E[,,2] <- E[,,2]*2
print(E)

# install.packages("myLibrary") # Télécharge et installe des packages depuis
# un dossier du CRAN ou depuis une source locale
library(abind) # Chargement de abind
F1 <- matrix(1,4,3)
print(F1)
F2 <- matrix(2,4,3)
print(F2)
F <- abind(F1, F2, along=3) # On lie deux matrices sous la forme d'un tableau
print(F)

source("./mes-fonctions.R")
print(res1 <- somme(2, 3))
print(res2 <- produit(2, 3))


# Répertoire de travail
# getwd() # Retourne le répertoire courant
# setwd("/Users/myname/Documents/R/TP/TP0") # Change de répertoire courant

source("./prodtrans.R")
X <- matrix(1,3,3)
print(X)
print(prodtrans(X))

# Indicateurs numériques, graphiques de base
print(x<-c(2,4,3,7,1))
print(A<-matrix(c(1,2,5,3,0,9),nrow=3,byrow=T))
print(max(x))
print(max(A))
print(apply(A,1,max))
print(apply(A,2,max))
print(apply(E,3,sum))
print(mean(x))
print(mean(A))
print(apply(A,2,mean))
# apply retourne un vecteur, un tableur ou une liste de valeurs obtenues
# en appliquant une fonction à une dimension d'un tableau ou d'une matrice

print(var(x))
print(var(A))
print(sd(x))
print(sd(A))

# sd retourne l'ecart-type non biaisée des valeurs de l'élément envoyé en paramètre
# var retourne la matrice de variance-covariance non biaisée si l'élément envoyé en 
# paramètre est une matrice
# dans le cas contraire, var retourne la variance non biaisée des valeurs de l'élément 
# envoyé en paramètre

print(var(A))
print(cov.wt(A, method='unbiased'))
print(cov.wt(A, method='ML'))
# Avec cot.wt, on peut choisir entre calculer la variance biaisée ou calculer
# la variance non biaisée
# Avec var, seule la variance non biaisée est calculée

x<-c(2,2,2,1,3,4,1,1)
hist(x) # Affichage d'un histogramme
pause()
x<-c(1,2,3,4,5)
y<-c(1,4,9,16,25)
plot(x,y) # Fonction générique pour l'affichage d'objets R
pause()
plot(x,y,pch=22) # 'pch' pour 'plotting character': '22' pour 'carré rempli' 
pause()
plot(x,y,pch=19,col='blue') # 'pch' pour 'plotting character': '19' pour 'cercle' 
pause()
plot(x,y,type='l',col='blue') # 'type' pour le type d'affichage ('l' pour ligne)
pause()

# Centrage de A
print(A)
source("./centre.R")
print(centre(A))
print(apply(centre(A), 2, mean))

print(data(iris)) # Chargement du jeu de données iris

print(class(iris)) # Classe de l'objet envoyé en paramètre
# Il s'agit d'un data.frame
print(names(iris))
print(iris[,1]) # Colonne 1
print(iris$Sepal.Length) # Colonne ayant pour nom Sepal.Length
print(class(iris[,1])) # Classe de la colonne 1
print(class(iris$Species)) # Classe de la colonne ayant pour nom Species

print(summary(iris)) # Donne certaines infos sur le jeu de données
print(apply(iris[,1:4],2,mean))
print(cor(iris[,1:4]))
print(cor(iris[,1:4]),digits=3)
plot(iris)
pause()
boxplot(iris) # Diagramme en boite
pause()

def.par <- par(no.readonly=T) # On récupère les paramètres graphiques initiaux
par(mfrow=c(2,2)) # On veut pouvoir afficher des graphiques sur 2 lignes et sur 2
# colonnes
for(i in 1:4){
	hist(iris[,i]) # On crée un histogramme pour chaque colonne du dataset
}
par(def.par) # On rétablit les paramètres initiaux

pie(summary(iris$Species)) # Affiche un diagramme circulaire sur les espèces
pause()
barplot(summary(iris$Species)) # Affiche un digaramme en bâtons sur les espèces
pause()

x11() # Démarre un gestionnaire de périphériques graphiques
plot(iris[,1:4], col=c("red","green","blue")[iris$Species]) # Les couleurs dépendent de l'espèce
pause()
x11() # Démarre un gestionnaire de périphériques graphiques
pairs(iris[,1:4], main="Iris de Fisher", pch=21, bg=c("red","green3","blue")[iris$Species]) 
# Une matrice de nuage de points est produite
# '21' pour cercle rempli et les couleurs dépendent des espèces
pause()

attach(iris) # La variable est attachée au PATH de recherche
# Histogrammes avec les espèces
print(inter<-seq(min(Petal.Length),max(Petal.Length),by=(max(Petal.Length)-min(Petal.Length))/10))
# Création d'un vecteur commencant par la plus petite longueur de pétale et se terminant par la plus grande
# Le pas est de (1/10 * max-min) -> le vecteur contient donc 11 valeurs
print(h1<-hist(plot=F,Petal.Length[Species=='setosa'], breaks=inter))
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.
print(h2<-hist(plot=F,Petal.Length[Species=='versicolor'], breaks=inter))
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.
print(h3<-hist(plot=F,Petal.Length[Species=='virginica'], breaks=inter))
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.
print(rbind(h1$counts,h2$counts,h3$counts))
barplot(rbind(h1$counts,h2$counts,h3$counts),space=0,
legend=levels(Species), main="LoPe", col=c('blue','red','yellow'))
# Affichage du diagramme en batons
pause()
# Graphique sur un fichier Postscript
postscript('exemple.eps', horizontal=F, width=12/2.5, height=12/2.5)
# Génération d'un fichier postscript
pause()
pairs(iris[1:4],main="Les Iris", pch=21,bg=c("red","green3","blue")[Species])
# Une matrice de nuage de points est produite
pause()
dev.off() # Rend inactif le gestionnaire de périphériques graphiques actuellement actif
detach(iris) # La variable est detachée du PATH de recherche

source('histFactor.R')
nbObs <- 10000
notes <- rnorm(nbObs, 15, 1)
genre <- rbinom(nbObs, 1, 0.5)
genre[genre==0] <- "Homme"
genre[genre==1] <- "Femme"
genre <- factor(genre)
histFactor(notes, genre) # Affichage du diagramme en batons
pause()

# Chargement du jeu de données contenu dans le fichier median-sy02-p2014.csv 
# et suppression des lignes correspondant aux absents 
notes <- read.csv("median-sy02-p2014.csv", header=F, na.strings=c("NA","ABS"))
names(notes) <- c("branche","note")
notes <- notes[-which(is.na(notes$note)),]

# Conversion de l'information de branche en chaîne de caractère, agrégation de l’information de branche,
# et reconversion en variable qualitative
notes$branche <- as.character(notes$branche)
notes$branche <- substr(notes$branche,1,2)
notes$branche <- as.factor(notes$branche)

histFactor(notes$note, notes$branche) # Affichage du diagramme en batons
pause()

gi = notes[which(notes$branche == "GI"),]$note
tc = notes[which(notes$branche == "TC"),]$note
mt = notes[which(notes$branche == "MT"),]$note
gu = notes[which(notes$branche == "GU"),]$note
gs = notes[which(notes$branche == "GS"),]$note
gp = notes[which(notes$branche == "GP"),]$note
gm = notes[which(notes$branche == "GM"),]$note
gb = notes[which(notes$branche == "GB"),]$note
boxplot(gi, tc, mt, gu, gs, gp, gm, gb, names=c('GI', 'TC', 'MASTER', 'GU', 'GS', 'GP', 'GM', 'GB'))
# Affiche de diagrammes en boite
pause()
