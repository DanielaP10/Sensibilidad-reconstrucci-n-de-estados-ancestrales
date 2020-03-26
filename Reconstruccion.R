## INTALACION DE PAQUETES
install.packages("phytools")
install.packages("phangorn")

## LLAMADO DE LIBRERIAS
library(phytools)
library(phangorn)
library(abind)

### ARBOL
tree <- read.tree('tree_Baker_et_al.2016.nex')
tree <- drop.tip(tree,tip = c(1,2))
windows()
plotTree(tree,fsize=0.8,ftype="i")

### MATRIZ DE CARACTERES - DIETAS
## Dietas como diferentes  caracteres binarios

dieta_csv <- read.table("dietas_Baker2.csv", sep = ",", text = T,header = F)

# Insectivoro
dietaI <- matrix(dieta_csv$V2[-1])
names(dietaI) <- dieta_csv$V1[-1]
matrizI <- as.factor(dietaI)

# Carnivoro
dietaC <- matrix(dieta_csv$V3[-1])
names(dietaC) <- dieta_csv$V1[-1]
matrizC <- as.factor(dietaC)

# Hematofago
dietaH <- matrix(dieta_csv$V4[-1])
names(dietaH) <- dieta_csv$V1[-1]
matrizH <- as.factor(dietaH)

# Frugivoro
dietaF <- matrix(dieta_csv$V5[-1])
names(dietaF) <- dieta_csv$V1[-1]
matrizF <- as.factor(dietaF)

# Nectarivoro
dietaN <- matrix(dieta_csv$V6[-1])
names(dietaN) <- dieta_csv$V1[-1]
matrizN <- as.factor(dietaN)

## Dieta como un caracter multiestado
dietaCM_csv <- read.table("dietas_Baker.csv", sep = ",", text = T,header = F)

dietaCM <- matrix(dietaCM_csv$V2[-1])
names(dietaCM) <- dietaCM_csv$V1[-1]
matrizCM <- as.factor(dietaCM)

### RECONSTRUCCION

## Dietas como diferentes caracteres binarios
# Insectivoro
di.anc.I <- ace(dietaI,tree,type = "discrete",model = "ER")
di.anc.I$lik.anc
round(di.anc.I$lik.anc,3)

color <- c("cadetblue1","brown1","cyan4","limegreen","purple1","seagreen4", "darkolivegreen2","darkorange")

colsI<-setNames(color[1:length(unique(matrizI))],sort(unique(matrizI)))

plotTree(tree,fsize=0.5,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.I$lik.anc,piecol=colsI,cex=0.5)
add.simmap.legend(colors=colsI,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)

# Carnivoria
di.anc.C <- ace(dietaC,tree,type = "discrete",model = "ER")
di.anc.C$lik.anc
round(di.anc.C$lik.anc,3)

colsC<-setNames(color[1:length(unique(matrizC))],sort(unique(matrizC)))

plotTree(tree,fsize=0.5,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.C$lik.anc,piecol=colsC,cex=0.5)
add.simmap.legend(colors=colsC,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)

# Hematofagia
di.anc.H <- ace(dietaH,tree,type = "discrete",model = "ER")
di.anc.H$lik.anc
round(di.anc.H$lik.anc,3)

colsH<-setNames(color[1:length(unique(matrizH))],sort(unique(matrizH)))

plotTree(tree,fsize=0.5,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.H$lik.anc,piecol=colsH,cex=0.5)
add.simmap.legend(colors=colsH,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)

# Frugivoria
di.anc.F <- ace(dietaF,tree,type = "discrete",model = "ER")
di.anc.F$lik.anc
round(di.anc.F$lik.anc,3)

colsF<-setNames(color[1:length(unique(matrizF))],sort(unique(matrizF)))

plotTree(tree,fsize=0.5,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.F$lik.anc,piecol=colsF,cex=0.5)
add.simmap.legend(colors=colsF,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)
# Nectavoria
di.anc.N <- ace(dietaN,tree,type = "discrete",model = "ER")
di.anc.N$lik.anc
round(di.anc.N$lik.anc,3)

colsN<-setNames(color[1:length(unique(matrizN))],sort(unique(matrizN)))

plotTree(tree,fsize=0.5,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.N$lik.anc,piecol=colsN,cex=0.5)
add.simmap.legend(colors=colsN,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)

## Dietas como un caracter multiestado
modelo <- matrix(c(0,3,4,1,4,3,0,4,2,4,4,4,0,4,4,2,4,4,0,2,2,4,4,2,0),nrow = 5,ncol = 5,byrow = T)

di.anc.CM <- ace(dietaCM,tree,type = "discrete",model = "ER")
di.anc.CM$lik.anc
round(di.anc.CM$lik.anc,3)

color <- c("cadetblue1","brown1","cyan4","limegreen","purple1","seagreen4", "darkolivegreen2","darkorange")

colsCM<-setNames(color[1:length(unique(matrizCM))],sort(unique(matrizCM)))

plotTree(tree,fsize=0.5,ftype="i")

nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=di.anc.CM$lik.anc,piecol=colsCM,cex=0.5)
add.simmap.legend(colors=colsCM,prompt=FALSE,x=0.2*par()$usr[1],
                  y=50,fsize=0.8)


### Exportacion de marginales de los estados ancestrales
#marginales <- di.anc.I$lik.anc
#write.csv(marginales,"Marginales-I.csv")
#MI <- read.csv("Marginales-I.csv")