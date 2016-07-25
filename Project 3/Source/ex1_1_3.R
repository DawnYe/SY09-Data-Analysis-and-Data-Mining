library(sm); # Loading of sm

source('./ex1_1_1.R');
source('./ex1_1_2.R');
source('./fonctions-tp3/errorRate.R');
source('./fonctions-tp3/front.ceuc.R');
source('./fonctions-tp3/front.kppv.R');

data <- read.table("donnees-tp3/Synth1-40.txt", header=F);

print('*** Classifieur euclidien ***');
X <- data[, 1:2];
z <- data[, 3];
Xapp <- X[c(1:15, 24:35),];
zapp <- z[c(1:15, 24:35)];
Xtst <- X[c(16:20, 36:40),];
ztst <- z[c(16:20, 36:40)];
MuApp <- ceuc.app(Xapp, zapp);
# print( summary(MuApp) ); # Test
print(MuApp);
ptst <- ceuc.val(MuApp, Xtst);
print(ptst); # Test
print(ztst); # Test
print(errorRate(ptst, ztst));

pause()

# Decision boundary
pdf(file = "images/ex1/test_classifieur_euclidien.pdf");
front.ceuc(MuApp, as.matrix(Xapp), as.matrix(zapp));
dev.off();

pause()

print('*** K plus proches voisins ***');
Xapp <- X[c(1:15, 24:26), ];
Xtst <- X[c(16:20, 36:40), ];
zapp <-z[c(1:15, 24:26)];
ztst <- z[c(16:20, 36:40)];
Xval <- X[c(27:35), ];
zval <- z[c(27:35)];
kOpt <- kppv.tune(Xapp, zapp, Xval, zval, 1:18);
print(kOpt); # Test
ptst <- kppv.val(Xapp, zapp, kOpt, Xtst);
print(ptst); # Test
print(ztst); # Test
print(errorRate(ptst, ztst));

pause()

# Decision boundary
pdf(file = "images/ex1/test_k_plus_proches_voisins.pdf");
front.kppv(Xapp, zapp, kOpt, Xapp, zapp)
dev.off();