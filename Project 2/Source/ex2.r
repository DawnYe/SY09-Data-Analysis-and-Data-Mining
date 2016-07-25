# setwd('C:/Users/Murdow/Desktop/tp2')
# library(devtools)
# install_github("vqv/ggbiplot")

library(sm) # Chargement de sm
library(ggbiplot) # Chargement de ggbiplot
library(MASS) # Librairie MASS pour la fonction Shepard

# Utilisation de hclust(dist(matrice)) pour calculer la distance euclidienne

######################## Question 1 : Mutations / Classification hierarchique ascendante #################

# Lecture donnees crabs via fichier csv
# Remarque : Déjà en présence d'une matrice de distance

mut <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)

# attributes(mut)

# mutClust <- hclust(mut, method="ward.D2") #  Classification hierarchique ascendante

# Attention au chemin de l'image

# Classification hiérarchique ascendante des données mutation
	 
# pdf(file = "./images/wardd1_mut.pdf")
# plot(hclust((mut * mut), method = "ward.D"), 
	# main = "CHA méthode Ward.D",
	# hang = -1,
	# xlab = "",
	# ylab = "",
	# sub = "")
# dev.off();

pdf(file = "./images/Mutations_cluster_dendrogramme_ascendante.pdf")
plot(hclust((mut), method = "ward.D2"), 
	main = "Classification hierarchique ascendante \ndes donnees de Mutations (cluster dendrogramme) \nutilisation du critère de Ward (ward.d2)",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pdf(file = "./images/complete_mut.pdf")
plot(hclust(mut, method = "complete"), 
	main = "CHA données mutations méthode Complete",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pdf(file = "./images/centroid_mut.pdf")
plot(hclust(mut, method = "centroid"), 
	main = "CHA méthode Centroid",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pdf(file = "./images/median_mut.pdf")
plot(hclust(mut, method = "median"), 
	main = "CHA données mutations méthode Median",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pdf(file = "./images/mcquitty_mut.pdf")
plot(hclust(mut, method = "mcquitty"), 
	main = "CHA données mutations méthode McQuitty",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pdf(file = "./images/single_mut.pdf")
plot(hclust(mut, method = "single"), 
	main = "CHA données mutations méthode Single",
	hang = -1,
	xlab = "",
	ylab = "",
	sub = "")
dev.off();

pause()

# Retrouve bien les 3 caracteristique eloignes de l'AFTD sur une branche differente de l'arbre

######################## Question 2 : Iris / Classification hierarchique ascendante #################

data(iris)
library(MASS)

irisData <- iris[,-c(5)] # On retire la colonne espece
irisDist <- dist(irisData) # Calcul des distances associees
irisClust <- hclust(irisDist, method="ward.D2") #  Classification hierarchique ascendante

# Attention au chemin de l'image

pdf("./images/Iris_cluster_dendrogramme_ascendante.pdf")
plot(irisClust, 
	main = "Classification hierarchique ascendante \ndes donnees de Iris (cluster dendrogramme)",
	hang = -1,
	sub = "",
	xlab = "", 
	ylab = "")
rect.hclust(irisClust, 3, border = c("firebrick", "darkcyan", "blue2"))
dev.off()

pause()

# Autre méthode
# plot(agnes(iris.x, metric = "euclidian", method = "ward"))

# Même couleur pour les group que lors de l'analyse dans l'exercice 1

######################## Question 3 : Iris / Classification hierarchique descendante #################

# Importer bibliotheque cluster
library(cluster)
# ?diana : DIvisive ANAlysis Clustering

png("./images/Iris_cluster_dendrogramme_descendante.png")
plot(diana(irisData, metric = "euclidian"), 
	main = "Classification hierarchique descendante\n des donnees de Iris (cluster dendrogramme)",
	hang = -1,
	sub = "",
	xlab = "", 
	ylab = "")
dev.off();

# Peu de différence avec la classification hiérarchique ascendante

# Ascendante :  elle part d'une situation où tous les individus sont seuls dans une classe, puis sont rassemblés en classes de plus en plus grandes
# Le qualificatif "hiérarchique" vient du fait qu'elle produit une hiérarchie H, l'ensemble des classes à toutes les étapes de l'algorithme
# Initialement, chaque individu forme une classe, soit n classes.
# À chaque étape, on fusionne deux classes, réduisant ainsi le nombre de classes.
# Les deux classes choisies pour être fusionnées sont celles qui sont les plus "proches", en d'autres termes, celles dont la dissimilarité entre elles est minimale, cette valeur de dissimilarité est appelée indice d'agrégation.
# Un dendrogramme est la représentation graphique d'une classification ascendante hiérarchique

# Classification ascendante hiérarchique : Au départ, chaque individu est dans un groupe distinct. A chaque étape, deux groupes sont rassemblés en un seul. il faut un critère d’agrégation.
# Classification descendante hiérarchique : Au départ tous les individus sont dans le même groupe. A chaque étape, un groupe est séparé en deux. Il faut un critère de séparation.

# Ward : le regroupement choisi est celui qui maximise la variance inter-groupes ; Variance totale = Variance intra-groupes + variance inter-groupes