plotContourLine <- function(C, mu, sigma, legendValues, X, fileName){
	pdf(file=fileName);
	colors <- c("darkorchid", "firebrick", "darkcyan", "darkolivegreen")
	t <- seq(0, 2*pi,length=100)
	sigma1Square <- sigma[1, 1];
	sigma2Square <- sigma[2, 2];
	j <- 0;
	n <- 0;
	plot(X, col = 'burlywood4');
	for(c in C){		
		csteSquare <- -2 * log(c * 2 * pi * sqrt(sigma1Square)* sqrt(sigma2Square));
		x <- mu[1] + sqrt(sigma1Square) * sqrt(csteSquare) * cos(t);
		y <- mu[2] + sqrt(sigma2Square) * sqrt(csteSquare) * sin(t);
		n <- (j %% 4 + 1);
		lines(x, y, type='l', col=colors[n], add=TRUE);
		j <- j + 1;
	}
	legend("bottomright", 200,legend=legendValues, col =c('burlywood4', colors), 
	pch = 21, inset=0)
	dev.off();
}