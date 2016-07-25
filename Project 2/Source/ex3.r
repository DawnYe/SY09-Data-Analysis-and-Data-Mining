library(sm) # Chargement de sm
library(ggbiplot) # Chargement de ggbiplot
library(MASS) # Librairie MASS pour la fonction Shepard


sSOfDistancesBetweenMeanAndEachIndvidual <- function(x){
	sum(scale(x, scale = FALSE)^2)
}


######################## Iris #################


######################## Question 1 #################

data(iris) # Appel donnees Iris

# head(iris)
# summary(iris)
# ?iris

irisData <- iris[,-c(5)] # On retire la colonne espece pour faire 
# tourner la méthode des centres mobiles

m = dim(irisData)[1]

pdf("./images/ex3/iris_2_clusters.pdf")
cl <- kmeans(irisData, 2, iter.max = 1000)
plot(irisData, col = cl$cluster, main="Méthode des centres mobiles sur Iris \n 2 clusters")
dev.off()

pause()

pdf("./images/ex3/iris_3_clusters.pdf")
cl <- kmeans(irisData, 3, iter.max = 1000)
plot(irisData, col = cl$cluster, main="Méthode des centres mobiles sur Iris \n 3 clusters")
dev.off()

pause()

pdf("./images/ex3/iris_4_clusters.pdf")
cl <- kmeans(irisData, 4, iter.max = 1000)
plot(irisData, col = cl$cluster, main="Méthode des centres mobiles sur Iris \n 4 clusters")
dev.off()

pause()

######################## Question 2 #################

print("Méthode des centres mobiles sur Iris avec 3 clusters")
for(i in 1:10){
	print(paste("Essai ", i, ":"))
	cl <- kmeans(irisData, 3, iter.max = 1000)
	plot(irisData, col = cl$cluster, main="Méthode des centres mobiles sur Iris \n 3 clusters")
	print(cl$tot.withinss/m)
	pause()
}

# Optimisation locale -> Dépend des points initiaux sélectionnés.
# Les résultats sont différents


######################## Question 3 #################

dataMatrix <- matrix(1, nrow = 100, ncol = 9)

for (j in 2:10) {
	for (i in 1:100) {
		cl <- kmeans(irisData, j, iter.max = 1000)
		dataMatrix[i, j-1] = cl$tot.withinss/m
	}
}

# 2 ou 3 clusters
minimumTotalWithinSS <- apply(dataMatrix, 2, min)

print(minimumTotalWithinSS)

pdf("./images/ex3/iris_variation_inertie_min.pdf")
plot(minimumTotalWithinSS, type="b", main="Variation d'inertie minimale en fonction de K")
dev.off()

pause()

######################## Question 4 #################

print(minimumTotalWithinSS)

# Calcul de la somme des inerties des classes par rapport à leurs centres (à une constante près)
withinSS <- sapply(split(irisData, iris[,5]), sSOfDistancesBetweenMeanAndEachIndvidual)
tot.withinss <- sum(withinSS)
print(tot.withinss/m) # Magnifique ! Tout colle ! 
# Pour trois clusters, la somme des inerties des classes par rapport à leurs centres obtenue
# via la méthode des centres mobiles est inférieure à celle obtenue avec la partition réelle.
# Logique, puisque la méthode des centres mobiles vise à trouver une partition minimisant 
# le critère mentionné ci-dessus (le critère d'inertie intra-classe).

pause()

# Details below
# withinSS <- vector(,3) # Vector of within-cluster sum of squares, one component per
# # cluster. 
# for (i in 1:3) { # We iterate for each cluster
# 	specieData <- irisData[which(as.numeric(iris[,5])==i),] # We get the data of the current
# 	# cluster.
# 	specieDataMean <- apply(specieData, 2, mean) # We get the mean of the current cluster.
# 	m <- dim(specieData)[1]	# We get the size of the cluster.
# 	ones <- matrix(1, nrow=m, ncol=1) # To calculate a matrix with the mean of the cluster.
# 	specieDataMean <- ones %*% t(as.matrix(specieDataMean))
# 	withinSS[i] <- sum((specieData - specieDataMean)^2)
# }
# totalWithinSS <- (sum(withinSS))/m # Total within-cluster sum of squares divided by the number of 
# individuals

# Ici on voit bien les quelques différences
print(as.numeric(iris[, 5]))
print(cl$cluster)

pause()

######################## Crabs #################


######################## Question 1 #################

crabs2 <- read.csv("./subject/crabs2.csv", header=T)
group <- interaction(crabs2$sex, crabs2$sp, sep = "/")
crabsData <- crabs2[, -c(5:6)] # On retire deux colonnes pour faire 
# tourner la méthode des centres mobiles
m <- dim(crabsData)[1]

print("Méthode des centres mobiles sur Crabs avec 2 clusters")
for(i in 1:10){
	print(paste("Essai ", i, ":"))
	cl <- kmeans(crabsData, 2, iter.max = 1000)
	plot(crabsData, col = cl$cluster, main="Méthode des centres mobiles sur Crabs \n 2 clusters")
	print(cl$tot.withinss/m)
	pause()
}

# Optimisation locale -> Dépend des points initiaux sélectionnés.
# Les résultats sont différents
# C'est magnifique, en fonction des points initiaux, la partition se fait
# soit au niveau de l'espèce, soit au niveau du sexe.
# On constate également que le critère d'inertie intra-classe est plus faible
# lorsque la partition se fait selon l'espèce que lorsqu'elle se fait selon 
# le sexe.


######################## Question 2 #################

pdf("./images/ex3/crabs_4_clusters.pdf")
cl <- kmeans(crabsData, 4, iter.max = 1000)
plot(crabsData, col = cl$cluster, main="Méthode des centres mobiles sur Crabs \n 4 clusters")
dev.off()

pause()

# A première vue, la partion obtenue avec la méthode des centres mobiles
# est proche de la partion réelle.

# Calcul de la somme des inerties des classes par rapport à leurs centres (à une constante près)
withinSS <- sapply(split(crabsData, group), sSOfDistancesBetweenMeanAndEachIndvidual)
tot.withinss <- sum(withinSS)

print(tot.withinss/m) # Magnifique ! Tout colle !
# Pour quatre clusters, la somme des inerties des classes par rapport à leurs centres obtenue
# via la méthode des centres mobiles est inférieure à celle obtenue avec la partition réelle.
# Logique, puisque la méthode des centres mobiles vise à trouver une partition minimisant 
# le critère mentionné ci-dessus (le critère d'inertie intra-classe). 

print(cl$tot.withinss/m) # Cela semble se confirmer

pause()

# Ici on voit bien les quelques différences
print(as.numeric(group))
print(cl$cluster)

pause()


######################## Mutations #################


######################## Question 1 et 2 #################

# Lecture des donnees mutations2 via un fichier csv
mut <- read.csv("./subject/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)

resMut <- cmdscale(mut, k=5, eig=T, x.ret=T) # AFTD avec ajout d'une constante

m <- dim(resMut$points)[1]

pdf("./images/ex3/mutations_dim_5_1.pdf")
plot(as.data.frame(resMut$points), main="Les individus du jeu de données Mutations \navec ajout d'une constante \n dans un espace de dimension 5")
dev.off()

pause()

print("Méthode des centres mobiles sur Mutations avec ajout d'une constante avec 3 clusters")
for(i in 1:10){
	print(paste("Essai ", i, ":"))
	cl <- kmeans(resMut$points, 3, iter.max = 1000)
	plot(resMut$points[,1], resMut$points[,2], pch= 4, col = cl$cluster)
	text(resMut$points[,1], resMut$points[,2], rownames(resMut$points), cex=0.5)
	print(cl$tot.withinss/m)
	pause()
}

# On voit clairement que le résultat de la partition est moins stable que celui des deux précédents
# jeux de données.
# Cela est probablement lié au fait que la matrice de dissimilarités utilisée n'est pas une matrice de 
# distances euclidiennes.
# Dans l'exercice 1, cette constatation nous a fortement incités à transformer la dissimilarité initiale
# en une distance en lui ajoutant une constante.
# On sait que cette pratique ne donne pas toujours de très bons résultats. Ainsi, cette transformation 
# explique peut-être cette "faible" stabilité.

resMut <- cmdscale(mut, k=5, eig=T) # AFTD sans ajout d'une constante

pdf("./images/ex3/mutations_dim_5_2.pdf")
plot(as.data.frame(resMut$points), main="Les individus du jeu de données Mutations \nsans ajout d'une constante \n dans un espace de dimension 5")
dev.off()

pause()

print("Méthode des centres mobiles sur Mutations sans ajout d'une constante avec 3 clusters")
for(i in 1:10){
	print(paste("Essai ", i, ":"))
	cl <- kmeans(resMut$points, 3, iter.max = 1000)
	plot(resMut$points[,1], resMut$points[,2], pch= 4, col = cl$cluster)
	text(resMut$points[,1], resMut$points[,2], rownames(resMut$points), cex=0.5)
	print(cl$tot.withinss/m)
	pause()
}

# Il ne semble pas y avoir d'évolution drastique de la stabilité (sans ajout d'une constante).
# En plus, vu qu'il y a des valeurs propres négatives, la position des individus dans l'espace à cinq 
# dimensions est peut-être plus mauvaise qu'avec la méthode précédente (bien que les valeurs propres 
# associées aux axes factoriels de ces cinq dimensions ne sont pas négatives).

# Finalement, la "faible" stabilité est probablement uniquement liée au fait que l'on ne travaille pas 
# avec une matrice de distances euclidiennes.