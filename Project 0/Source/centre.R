# Cette fonction centre une matrice X en colonne
centre <- function(X){
	numberOfLines <- dim(X)[1]
	X <- X - matrix(rep(apply(X, 2, mean), numberOfLines), nrow=numberOfLines, byrow=T)
}