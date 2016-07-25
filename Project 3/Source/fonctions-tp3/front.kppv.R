source('./ex1_1_2.R');

front.kppv <- function(Xapp, zapp, K, Xaff, zaff)
{
    discretisation=150
    deltaX <- (max(Xapp[,1]) -min(X[,1]))/discretisation
    deltaY <- (max(Xapp[,2]) -min(X[,2]))/discretisation
    minX <- min(Xapp[,1])-deltaX
    maxX <- max(Xapp[,1])+deltaX
    minY <- min(Xapp[,2])-deltaY
    maxY <- max(Xapp[,2])+deltaY
  
    # grille d'affichage 
    grilleX <- seq(from=minX,to=maxX,by=deltaX)
    naffX <- length(grilleX)
    grilleY <- seq(from=minY,to=maxY,by=deltaY)
    naffY <- length(grilleY)
    grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
    # calcul des valeurs de la fonction 
    valf <- kppv.val(Xapp, zapp, K, grille)
    plot(Xaff, col=c("darkorchid","firebrick","darkcyan","darkolivegreen","orange")[zaff], asp=1)
    contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
