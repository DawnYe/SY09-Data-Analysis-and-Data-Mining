source('./fonctions-tp3/distXY.R');

ceuc.app <- function(Xapp, zapp){
	napp <- dim(Xapp)[1]; # Size of the learning set
	mu1 <- apply( Xapp[which( zapp == 1 ), ], 2, mean ); # Mean of the first class individuals
	mu2 <- apply( Xapp[which( zapp == 2 ), ], 2, mean ); # Mean of the second class individuals
	Mu <- rbind(mu1, mu2);
}

ceuc.val <- function(Mu, Xtst){
	Xtst <- as.matrix(Xtst); # Cast to a matrix
	distXtstMu <- distXY(Xtst, Mu); # Computation of the distances between each individual and each mean
	# print(class(distXtstMu)); # Test
	# print(distXtstMu); # Test
	ptst <- apply(distXtstMu, 1, which.min); # Computation of the index of the nearest mean for each individual
	# print(class(ptst)); # Test
	# ptst;
}