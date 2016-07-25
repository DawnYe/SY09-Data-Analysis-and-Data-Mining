library(sm); # Loading of sm

source('./ex1_1_1.R');
source('./ex1_1_2.R');
source('./fonctions-tp3/errorRate.R');
source('./fonctions-tp3/separ1.R');
source('./fonctions-tp3/separ2.R');
source('./fonctions-tp3/getData.R');
source('./fonctions-tp3/estimationOfMusAndSigmas.R');
source('./fonctions-tp3/front.ceuc2.R');
source('./fonctions-tp3/bayesianRule.R');
source('./fonctions-tp3/plotContourLine.R');

# Data
XAll <- NULL;
zAll <- NULL;
Mu1 <- c(0, 2);
Mu2 <- c(0, -1);
Sigma1 <- diag(c(1, 1));
Sigma2 <- diag(c(1, 1));
pi1 <- 0.5;
pi2 <- 0.5;

############################### Question 2 ###############################
plotContourLine(c(0.00001,0.0001, 0.001, 0.003), c(0, -5), diag(c(5, 5)), c('c=0.00001', 'c=0.0001', 'c=0.001', 'c=0.003'), "images/ex2/ellipses.pdf");
pause()

############################### Question 3 and 4 ###############################
filesNames <- c("donnees-tp3/Synth1-40.txt", "donnees-tp3/Synth1-100.txt", "donnees-tp3/Synth1-500.txt", "donnees-tp3/Synth1-1000.txt");
errorRates <- c();
j <- 0;
imageName <- NULL;
for(fileName in filesNames){
	j <- j +1;
	data <- getData(fileName);
	X <- data[[1]];
	z <- data[[2]];
	predictions <- bayesianRule(X, Mu1, Mu2, Sigma1, Sigma2, pi1, pi2);

	# Decision boundary
	imageName <- paste ("images/ex2/synth_1_decision_boudary_", j, ".pdf", sep="");
	pdf(file = imageName);
	front.ceuc2(X, Mu1, Mu2, Sigma1, Sigma2, pi1, pi2, z)
	dev.off();	
	errorRates[j] <- errorRate(predictions, z); # This computation is stupid
}
# print(errorRates);

############################### Question 5 ###############################
bayesianError <- pnorm(-sqrt(mahalanobis(Mu1, Mu2, Sigma1, inverted = FALSE))/2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE);
print('Erreur de Bayes : ')
print(bayesianError);

############################### Question 3 and 4 (bonus) ###############################
# Data
Mu1 <- c(0, 3);
Mu2 <- c(0, -5);
Sigma1 <- diag(c(1, 1));
Sigma2 <- diag(c(5, 5));
pi1 <- 0.5;
pi2 <- 0.5;

fileName <- "donnees-tp3/Synth2-1000.txt";
data <- getData(fileName);
X <- data[[1]];
z <- data[[2]];
predictions <- bayesianRule(X, Mu1, Mu2, Sigma1, Sigma2, pi1, pi2);
imageName <- "images/ex2/synth_2_decision_boudary.pdf";
pdf(file = imageName);
front.ceuc2(X, Mu1, Mu2, Sigma1, Sigma2, pi1, pi2, z)
dev.off();
# print(errorRate(predictions, z)); # This computation is stupid