histFactor <-function(variableQuantitative, variableQualitative){
	# variableQuantitative: un vecteur de valeurs 
	# variableQualitative: un facteur de chaine de caractères
	minV <- min(variableQuantitative)
	maxV <- max(variableQuantitative)	
	inter <-seq(minV, maxV,by=((maxV-minV)/10))
	# Création d'un vecteur commencant par la plus petite valeur et se terminant par la plus grande
	# Le pas est de (1/10 * max-min) -> le vecteur contient donc 11 valeurs
	counts <- c()
	l <- list()
	i <- 0
	for(modalite in levels(variableQualitative)){
		i <- i+1
		l[[i]] <- hist(plot=F, variableQuantitative[variableQualitative==modalite], breaks=inter)
		# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
		# est retournée.
		# Ensuite, cette dernière est ajoutée à une liste
		counts <- c(counts, l[[i]]$counts)
		# Enfin, on crée un vecteur contenant le nombre de valeurs dans chaque bin (pour chaque modalité). 
	}
	data <- matrix(counts, nrow=length(l), byrow=T)
	barplot(data, space=0,
	legend=levels(variableQualitative), main="Diagramme en boîte")
}