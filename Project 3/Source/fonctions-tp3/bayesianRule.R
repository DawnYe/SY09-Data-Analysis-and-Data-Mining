bayesianRule <- function(X, Mu1, Mu2, Sigma1, Sigma2, pi1, pi2){
	X <- as.matrix(X); # Cast to a matrix
	nAll <- dim(X)[1];
	X1 <- X[,1];
	X2 <- X[,2];
	mu1_1 <- Mu1[1];
	mu1_2 <- Mu1[2];
	mu2_1 <- Mu2[1];
	mu2_2 <- Mu2[2];
	sigma1_11 <- Sigma1[1, 1];
	sigma1_22 <- Sigma1[2, 2];
	sigma2_11 <- Sigma2[1, 1];
	sigma2_22 <- Sigma2[2, 2];
	predictions <- rep(0, 15);
	for(i in 1:nAll){
		if( ( dnorm(X1[i], mu1_1, sqrt(sigma1_11)) * dnorm(X2[i], mu1_2, sqrt(sigma1_22)) ) / ( dnorm(X1[i], mu2_1, sqrt(sigma2_11)) * dnorm(X2[i], mu2_2, sqrt(sigma2_22)) ) > (pi2 / pi1) ){
			predictions[i] <- 1;
		}else{
			predictions[i] <- 2
		}
	}
	predictions;
}