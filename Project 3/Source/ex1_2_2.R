 source('./ex1_1_1.R');
source('./ex1_1_2.R');
source('./fonctions-tp3/errorRate.R');
source('./fonctions-tp3/separ1.R');
source('./fonctions-tp3/separ2.R');
source('.//fonctions-tp3/getData.R');
source('.//fonctions-tp3/estimationOfMusAndSigmas.R');

############################### Question 1 et 2 (classifieur euclidien) ###############################

alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
fileName <- c("donnees-tp3/Synth2-1000.txt");
N <- 20;
XAll <- NULL;
zAll <- NULL;
# Exploitation fichier fileName
data <- getData(fileName);
X <- data[[1]];
XAll <- rbind(XAll, X);
z <- data[[2]];
zAll <- c(zAll, z);
data <- getData(fileName);
X <- data[[1]];
XAll <- rbind(XAll, X);
z <- data[[2]];
zAll <- c(zAll, z);
results <- estimationOfMusAndSigmas(X, z);	
tmpList <- NULL
errorRates <- c()
errorRatesApp <- c()
errorRatesTst <- c()
for(j in 1:N){
	donn.sep <- separ1(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	MuApp <- ceuc.app(Xapp, zapp);
	papp <- ceuc.val(MuApp, Xapp);
	ptst <- ceuc.val(MuApp, Xtst);
	errorRatesApp[j] <- errorRate(papp, zapp);
	errorRatesTst[j] <- errorRate(ptst, ztst);
}
cILengthDivByTwoApp <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesApp))/sqrt(N);
cILengthDivByTwoTst <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesTst))/sqrt(N);	
tmpList$errorRatesApp <- errorRatesApp;
tmpList$errorRatesTst <- errorRatesTst;
errorMeanApp <- mean(errorRatesApp);
errorMeanTst <- mean(errorRatesTst);
tmpList$estimatorErrorRateApp <- errorMeanApp;
tmpList$estimatorErrorRateTst <- errorMeanTst;
tmpList$cILengthApp <- cILengthDivByTwoApp + cILengthDivByTwoApp;
tmpList$cILeftBoundApp <- errorMeanApp - cILengthDivByTwoApp;
tmpList$cIRightBoundApp <- errorMeanApp + cILengthDivByTwoApp;
tmpList$cILengthTst <- cILengthDivByTwoTst + cILengthDivByTwoTst;
tmpList$cILeftBoundTst <- errorMeanTst - cILengthDivByTwoTst;
tmpList$cIRightBoundTst <- errorMeanTst + cILengthDivByTwoTst;
results <- c(results, tmpList)
print(results); 

############################### Question 2 (K plus proches voisins) ###############################

results <- list();
data <- getData(fileName);
X <- data[[1]];
z <- data[[2]];
errorRates <- c()
errorRatesApp <- c()
errorRatesTst <- c()
for(j in 1:N){
	donn.sep <- separ2(X, z);
	Xapp <- donn.sep$Xapp;
	zapp <- donn.sep$zapp;
	Xval <- donn.sep$Xval;
	zval <- donn.sep$zval;
	Xtst <- donn.sep$Xtst;
	ztst <- donn.sep$ztst;
	kOpt <- kppv.tune(Xapp, zapp, Xval, zval, 1:20);
	papp <- kppv.val(Xapp, zapp, kOpt, Xapp);
	ptst <- kppv.val(Xapp, zapp, kOpt, Xtst);
	errorRatesApp[j] <- errorRate(papp, zapp);
	errorRatesTst[j] <- errorRate(ptst, ztst);
}
cILengthDivByTwoApp <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesApp))/sqrt(N);
cILengthDivByTwoTst <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesTst))/sqrt(N);	
results$errorRatesApp <- errorRatesApp;
results$errorRatesTst <- errorRatesTst;
errorMeanApp <- mean(errorRatesApp);
errorMeanTst <- mean(errorRatesTst);
results$estimatorErrorRateApp <- errorMeanApp;
results$cILengthApp <- cILengthDivByTwoApp + cILengthDivByTwoApp;
results$cILeftBoundApp <- errorMeanApp - cILengthDivByTwoApp;
results$cIRightBoundApp <- errorMeanApp + cILengthDivByTwoApp;
results$estimatorErrorRateTst <- errorMeanTst;
results$cILengthTst <- cILengthDivByTwoTst + cILengthDivByTwoTst;
results$cILeftBoundTst <- errorMeanTst - cILengthDivByTwoTst;
results$cIRightBoundTst <- errorMeanTst + cILengthDivByTwoTst;
print(results); 
