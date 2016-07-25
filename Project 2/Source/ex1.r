library(sm) # Chargement de sm
library(ggbiplot) # Chargement de ggbiplot
library(MASS) # Librairie MASS pour la fonction Shepard


drawScreePlot <- function(princompResult, title, cex=1, round=2){
	eigenValues <- princompResult^2
	eigenValuesSum <- sum(eigenValues)
	n <- length(eigenValues)
	xLab <- as.vector(interaction('U',1:n, sep=""))
	ones <- matrix(1, nrow=1, ncol=n)
	eigenValuesSum <- eigenValuesSum %*% ones
	screePlotData <- (eigenValues / eigenValuesSum) * 100
	brplt <- barplot(screePlotData, main=title, names.arg=xLab, ylim=c(0, 100), ylab= "Pourcentage d'inertie expliqué par chaque axe")
	text(x=brplt, y=screePlotData+3, labels=as.character(round(screePlotData, round)), xpd=TRUE, cex=cex)
}

drawCumulativeScreePlot <- function(princompResult, title, cex=1, round=2){
	eigenValues <- princompResult^2
	eigenValuesSum <- sum(eigenValues)
	cumulativeEigenValues <- cumsum(eigenValues)
	n <- length(eigenValues)
	xLab <- as.vector(interaction('E',1:n, sep=""))
	ones <- matrix(1, nrow=1, ncol=n)
	eigenValuesSum <- eigenValuesSum %*% ones
	screePlotData <- (cumulativeEigenValues / eigenValuesSum) * 100
	brplt <- barplot(screePlotData, main=title, names.arg=xLab, ylim=c(0, 100), ylab= "Pourcentage d'inertie expliqué par chaque chaque sous espace principal")
	text(x=brplt, y=screePlotData+3, labels=as.character(round(screePlotData, round)), xpd=TRUE, cex=cex)
}


######################## Question 1 : Iris #################

data(iris) # Chargement des donnees Iris
# head(iris)
# summary(iris)
# ?iris

irisData <- iris[,-c(5)] # On retire la colonne espece pour faire l'ACP

# Caracterisation des Iris
labelsNames <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
pdf("./images/ex1/Iris_selon_espece.pdf")
plot(irisData, main="Caracteristiques morphologiques\n des iris selon l'espece", 
	pch=21, 
	col=c("firebrick", "darkcyan", "blue2")[iris[,5]],
	labels=labelsNames	
	)
legend("center", 200,legend=c("setosa", "versicolor", "virginica"), col = c("firebrick", "darkcyan", "blue2"), 
	pch = 21, inset=0)
dev.off()

pause()

resIris <- princomp(irisData) # Calcul de l'ACP
summary(resIris)

# Pourcentage d'inertie expliquée par chaque axe
pdf("./images/ex1/Iris_batons_valeurs_propres.pdf")
drawScreePlot(resIris$sdev, "Pourcentage d'inertie expliqué par chaque axe (Iris)")
dev.off()

pause()

# Pourcentage d'inertie expliquée par les sous-espaces principaux
pdf("./images/ex1/Iris_batons_valeurs_propres_cumul.pdf")
drawCumulativeScreePlot(resIris$sdev, "Pourcentage d'inertie expliqué par les sous-espaces principaux (Iris)")
dev.off()

pause()

# Representation de l'ancienne base et des individus dans le plan 
# forme a partir de U1 et U2 sans tenir compte de l'espèce.
pdf("./images/ex1/Iris_ex_base_et_individus_u1_u2_sans_dist.pdf")
g <- ggbiplot(resIris)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
print(g)
dev.off()

pause()

# Representation de l'ancienne base et des individus dans le plan 
# forme a partir de U1 et U2 en tenant compte de l'espèce.
pdf("./images/ex1/Iris_ex_base_et_individus_u1_u2_avec_dist.pdf")
g <- ggbiplot(resIris, groups=iris[,5])
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
print(g)
dev.off()

pause()


######################## Question 2 : Crabs #################

# Lecture donnees crabs via fichier csv
crabs2 <- read.csv("./subject/crabs2.csv", header=T)
# head(crabs2)
# summary(crabs2)
group <- interaction(crabs2$sex, crabs2$sp, sep = "/")

crabsData <- crabs2[,-c(5:6)] # Retire colonne espece pour faire l'ACP

# Couleur et sexe
labelsNames2 <- c("FL2", "RW2", "CL2", "BD2")
pdf("./images/ex1/Crabs_selon_sexe_et_espece.pdf")
plot(crabsData, main="Caracteristiques morphologiques\n des crabes selon le sexe et l'espece", 
	pch=21, 
	col=c("darkorchid", "firebrick", "darkcyan", "darkolivegreen")[group],
	labels=labelsNames2	
	)
legend("center", 200, legend=c("M/B", "M/O", "F/B", "F/O"), 
	col = c("firebrick", "darkolivegreen", "darkorchid", "darkcyan"), 
	pch = 21, inset=0)
dev.off()

pause()

resCrabs <- princomp(crabsData) # Calcul de l'ACP
summary(resCrabs)

# Pourcentage d'inertie expliquée par chaque axe
pdf("./images/ex1/Crabs_batons_valeurs_propres.pdf")
drawScreePlot(resCrabs$sdev, "Pourcentage d'inertie expliqué par chaque axe (Crabs)")
dev.off()

pause()

# Pourcentage d'inertie expliquée par les sous-espaces principaux
pdf("./images/ex1/Crabs_batons_valeurs_propre_cumul.pdf")
drawCumulativeScreePlot(resCrabs$sdev, "Pourcentage d'inertie expliqué par les sous-espaces principaux (Crabs)")
dev.off()

pause()

# Representation de l'ancienne base et des individus dans le plan 
# forme a partir de U1 et U2 sans tenir compte de l'espèce.
pdf("./images/ex1/Crabs_ex_base_et_individus_u1_u2_sans_dist.pdf")
g <- ggbiplot(resCrabs)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
print(g)
dev.off()

pause()

# Representation de l'ancienne base et des individus dans le plan 
# forme a partir de U1 et U2 en tenant compte du sexe et de l'espèce
pdf("./images/ex1/Crabs_ex_base_et_individus_u1_u2_avec_dist.pdf")
g <- ggbiplot(resCrabs, groups=group)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
print(g)
dev.off()

pause()


######################## Question 3 : Mutations #################

# Lecture des donnees mutations2 via un fichier csv
mut <- read.csv("./subject/mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
attributes(mut)

resMut <- cmdscale(mut, k=2, eig=T, x.ret=T) 
resMut$eig
pdf("./images/ex1/mutations_test_1.pdf")
x <- resMut$points[,1]
y <- resMut$points[,2] 
plot(x, y, pch=4, type="n", xlab="U1", ylab="U2", main="Représentation euclienne des données en 2 variables")
text(x, y, rownames(resMut$points), cex=0.5)
dev.off()

pause()

# Valeur propre negative trop grande et importante : 
# on utilise la transformation de la dissimilarite en distance malgre que 
# les resultats ne sont pas toujours tres bon 
# car nous ne pouvons pas exploite l'autre matrice de distance

resMut2 <- cmdscale(mut, k=2, eig=T, x.ret=T, add=T)
resMut2$eig
pdf("./images/ex1/mutations_test_2.pdf")
plot(resMut2$points, pch=4, type="n", xlab="U1", ylab="U2", main="Représentation euclienne des données en 2 variables")
text(resMut2$points[,1], resMut2$points[,2], rownames(resMut2$points))
dev.off()

pause()

# Pourcentage d'inertie expliquée par chaque axe
pdf("./images/ex1/mutations_test_2_pourc_iner.pdf")
drawScreePlot(resMut2$eig, "Pourcentage d'inertie expliqué par chaque axe (Mutations)", cex=0.4, round=1)
dev.off()

pause()

# Pourcentage d'inertie expliquée par les sous-espaces principaux
pdf("./images/ex1/mutations_test_2_pourc_iner_cumul.pdf")
drawCumulativeScreePlot(resMut2$eig, "Pourcentage d'inertie expliqué par les sous-espaces principaux (Mutations)", cex=0.4, round=1)
dev.off()

pause()

resMut2Sh <- Shepard(mut, resMut2$points, p = 2)
pdf("./images/ex1/mutations_test_2_shepard.pdf")
plot(resMut2Sh, pch = ".", cex=3, xlab="Dissimilarite", ylab="Distance")
abline(0, 1, col="firebrick4")
dev.off()

pause()

resMut3 <- isoMDS(mut)
resMut3$eig
pdf("./images/ex1/mutations_test_3.pdf")
plot(resMut3$points, type="n", xlab="U1", ylab="U2", main="Représentation euclienne des données en 2 variables")
text(resMut3$points[,1], resMut3$points[,2], rownames(resMut3$points), cex=0.1)
dev.off()

pause()

resMut3Sh <- Shepard(mut, resMut3$points)
pdf("./images/ex1/mutations_test_3_shepard.pdf")
plot(resMut3Sh, pch = ".", cex=3, xlab="Dissimilarite", ylab="Distance")
abline(0, 1, col="firebrick4")
dev.off()


resMut3Sh <- Shepard(mut, resMut3$points)
pdf("./images/ex1/mutations_test_3_shepard.pdf")
plot(resMut3Sh, pch = ".", cex=3, xlab="Dissimilarite", ylab="Distance")
abline(0, 1, col="firebrick4")
dev.off()

pause()

resMut4 <- cmdscale(mut, k=5, eig=T, x.ret=T, add=T)

resMut4Sh <- Shepard(mut, resMut4$points, p = 2)
pdf("./images/ex1/mutations_test_4_shepard.pdf")
plot(resMut4Sh, pch = ".", cex=3, xlab="Dissimilarite", ylab="Distance")
abline(0, 1, col="firebrick4")
dev.off()

pause()