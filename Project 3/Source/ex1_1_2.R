source('./fonctions-tp3/errorRate.R');
source('./fonctions-tp3/distXY.R')

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv){
	errorRates <- rep(0, length(nppv)); # It will contain the error rate for each potential K
	i <- 0;
	for(k in nppv){ # Foreach potential k
		i <- i+1;
		# print(k); # Test
		pval <- kppv.val(Xapp, zapp, k, Xval);
		# print(pval); # Test
		# print(zval); # Test
		errorRates[i] <- errorRate(pval, zval);	
		# print("+++++++++++++++"); # Test
	}
	# print(class(errorRates)); # Test
	# print(errorRates); # Test
	minErrorRate <- min(errorRates);
	kOptIndex <- max( which( errorRates == minErrorRate ) );
	# kOptIndex <- which.min(errorRates);
	# print(kOptIndex); # Test
	nppv[kOptIndex];
}

kppv.val <- function(Xapp, zapp, K, Xtst){
	Xtst <- as.matrix(Xtst); # Cast to a matrix
	# print(Xtst); # Test
	ntst <- dim(Xtst)[1]; # Size of the test set
	ptst <- rep(1, ntst); # It will contain the predictions
	# print(zapp); # Test
	# Computation of the distances between each individual of the test set
	# and each individual of the learning set	
	distXtstXapp <- distXY(Xtst, Xapp);
	# print(distXtstXapp); # Test
	for(i in 1:ntst){ # Foreach individual in the test set
		# We sort the distances between our individual and every individual of the learning set
		sortResult <- sort( distXtstXapp[i, ], index.return = TRUE );
		# print(sortResult); # Test
		# print(sortResult$ix[1:K]); # Test
		classes <- zapp[ sortResult$ix[1:K] ] # Classes of the K-nearest neighbours
		# print(class(classes)); # Test
		# print(classes); # Test
		tableClasses <- table( factor( classes, levels=c(1, 2) ) ) # To know how much each class is represented
		# print(tableClasses); # Test
		if(tableClasses[1] >= tableClasses[2]){
			ptst[i] <- 1;
		}else{
			ptst[i] <- 2;
		}
	}
	# print(ptst); # Test
	ptst;
}