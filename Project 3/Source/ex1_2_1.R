source('./ex1_1_1.R');
source('./ex1_1_2.R');
source('./fonctions-tp3/errorRate.R');
source('./fonctions-tp3/separ1.R');
source('./fonctions-tp3/separ2.R');
source('.//fonctions-tp3/getData.R');
source('.//fonctions-tp3/estimationOfMusAndSigmas.R');

############################### Question 1 et 2 ###############################
alpha <- 0.05;
alphaDivBy2 <- alpha/2;
oneMinusAlphaDivBy2 <- 1 - alphaDivBy2
filesNames <- c("donnees-tp3/Synth1-40.txt", "donnees-tp3/Synth1-100.txt", "donnees-tp3/Synth1-500.txt", "donnees-tp3/Synth1-1000.txt");
results <- list();
i <- 1;
N <- 20;
XAll <- NULL;
zAll <- NULL;
for(fileName in filesNames){
	data <- getData(fileName);
	X <- data[[1]];
	XAll <- rbind(XAll, X);
	z <- data[[2]];
	zAll <- c(zAll, z);
	results[[i]] <- estimationOfMusAndSigmas(X, z);	
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
		# print( summary(MuApp) ); # Test
		# print(MuApp); # Test
		papp <- ceuc.val(MuApp, Xapp);
		ptst <- ceuc.val(MuApp, Xtst);
		# print(ptst); # Test
		# print(ztst); # Test
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
	results[[i]] <- c(results[[i]], tmpList)
	i <- i + 1;
}
print(results); # Test


############################### Question 3 ###############################
# print(XAll); # Test
# print(zAll); # Test
donn.sep <- separ1(XAll, zAll);
Xapp <- donn.sep$Xapp;
zapp <- donn.sep$zapp;
Xtst <- donn.sep$Xtst;
ztst <- donn.sep$ztst;
kOpt <- kppv.tune(Xapp, zapp, Xapp, zapp, 1:10);
print(kOpt); # It must be one... 


############################### Question 4 ###############################
results <- list();
i <- 1;
for(fileName in filesNames){
	results[[i]] <- list();
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
		# print(kOpt); # Test
		papp <- kppv.val(Xapp, zapp, kOpt, Xapp);
		ptst <- kppv.val(Xapp, zapp, kOpt, Xtst);

		# print(ptst); # Test
		# print(ztst); # Test
		
		errorRatesApp[j] <- errorRate(papp, zapp);
		errorRatesTst[j] <- errorRate(ptst, ztst);
	}
	cILengthDivByTwoApp <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesApp))/sqrt(N);
	cILengthDivByTwoTst <- (qnorm(oneMinusAlphaDivBy2)* sd(errorRatesTst))/sqrt(N);	
	results[[i]]$errorRatesApp <- errorRatesApp;
	results[[i]]$errorRatesTst <- errorRatesTst;
	errorMeanApp <- mean(errorRatesApp);
	errorMeanTst <- mean(errorRatesTst);
	results[[i]]$estimatorErrorRateApp <- errorMeanApp;
	results[[i]]$cILengthApp <- cILengthDivByTwoApp + cILengthDivByTwoApp;
	results[[i]]$cILeftBoundApp <- errorMeanApp - cILengthDivByTwoApp;
	results[[i]]$cIRightBoundApp <- errorMeanApp + cILengthDivByTwoApp;
	results[[i]]$estimatorErrorRateTst <- errorMeanTst;
	results[[i]]$cILengthTst <- cILengthDivByTwoTst + cILengthDivByTwoTst;
	results[[i]]$cILeftBoundTst <- errorMeanTst - cILengthDivByTwoTst;
	results[[i]]$cIRightBoundTst <- errorMeanTst + cILengthDivByTwoTst;
	i <- i + 1;
}
print(results); # Test