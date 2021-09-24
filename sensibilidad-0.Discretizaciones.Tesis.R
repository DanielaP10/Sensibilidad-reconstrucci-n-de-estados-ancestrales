#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")
#source("reconstrucciones_Mk_DR.Tesis.R")
#source("reconstrucciones_Parsimonia.Tesis.R")

############################################################ DISCRETIZACIÓN DE RESULTADOS. 

# Dado que las reconstrucciones de Máxima verosimilitud arrojan probabilidades de estado, se discretizaran de forma binaria estas probabilidades para las comparaciones de cada reconstrucción. En el caso de la codificación Multiestado se le asignará a los nodos el estado presente/ausente (0,1) si la dieta fue recuperada o no.


# Abreviaciones de las codificaciones: 
# - Mb: matriz base
# - Multi: matriz multiestado
# - B1: matriz binaria 1
# - B2: matriz binaria 2
# - B3: matriz binaria 3


## TASAS IGUALES

# MATRIZ MULTIESTADO ----

mpMulti <- data.frame("Car"=datos.reaMkERMulti[,1],"Fru"=datos.reaMkERMulti[,2],"Hem"=datos.reaMkERMulti[,3],"Ins"=datos.reaMkERMulti[,4],"Nec"=datos.reaMkERMulti[,5],"Estado"=NA,"Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) #En la multiestado están 0-ins, 1-hem, 2-car, 3-fru y 4-nec

### Carnivoria
for (i in 1:length(mpMulti$Ins)) {
  if (max(mpMulti[i,1],mpMulti[i,2],mpMulti[i,3],mpMulti[i,4],mpMulti[i,5])==mpMulti[i,1]) {
    mpMulti[i,6]=0
  }
}

### Frugivoria
for (i in 1:length(mpMulti$Ins)) {
  if (max(mpMulti[i,1],mpMulti[i,2],mpMulti[i,3],mpMulti[i,4],mpMulti[i,5])==mpMulti[i,2]) {
    mpMulti[i,6]=1
  }
}

### Hematofagia
for (i in 1:length(mpMulti$Ins)) {
  if (max(mpMulti[i,1],mpMulti[i,2],mpMulti[i,3],mpMulti[i,4],mpMulti[i,5])==mpMulti[i,3]) {
    mpMulti[i,6]=2
  }
}

### Insectivoria
for (i in 1:length(mpMulti$Ins)) {
  if (max(mpMulti[i,1],mpMulti[i,2],mpMulti[i,3],mpMulti[i,4],mpMulti[i,5])==mpMulti[i,4]) {
    mpMulti[i,6]=3
  }
}

### Nectarivoria
for (i in 1:length(mpMulti$Ins)) {
  if (max(mpMulti[i,1],mpMulti[i,2],mpMulti[i,3],mpMulti[i,4],mpMulti[i,5])==mpMulti[i,5]) {
    mpMulti[i,6]=4
  }
}


## Binarización Carnivoria
for (i in 1:length(mpMulti$Ins)) {
  if (mpMulti[i,6]==0) {
    mpMulti[i,7]=1
  }
}

## Binarización Frugivoria
for (i in 1:length(mpMulti$Ins)) {
  if (mpMulti[i,6]==1) {
    mpMulti[i,8]=1
  }
}

## Binarización Hematofagia
for (i in 1:length(mpMulti$Ins)) {
  if (mpMulti[i,6]==2) {
    mpMulti[i,9]=1
  }
}

## Binarización Isectivoria
for (i in 1:length(mpMulti$Ins)) {
  if (mpMulti[i,6]==3) {
    mpMulti[i,10]=1
  }
}

## Binarización Nectarivoria
for (i in 1:length(mpMulti$Ins)) {
  if (mpMulti[i,6]==4) {
    mpMulti[i,11]=1
  }
}


# MATRIZ BASE ####
mpBase <- data.frame ("Car"=rep(0,length(datos.reaMkErMb.car[,1])),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
#### Ausente. Cada estado es una columna del archivo datos...
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.car[i,c(1,2,3)])==datos.reaMkErMb.car[i,1]) {
    mpBase[i,1]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.car[i,c(1,2,3)])==datos.reaMkErMb.car[i,2]) {
    mpBase[i,1]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.car[i,c(1,2,3)])==datos.reaMkErMb.car[i,3]) {
    mpBase[i,1]=2
  }
}


### Frugivoria
#### Ausente
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.fru[i,c(1,2,3,4)])==datos.reaMkErMb.fru[i,1]) {
    mpBase[i,2]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.fru[i,c(1,2,3,4)])==datos.reaMkErMb.fru[i,2]) {
    mpBase[i,2]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.fru[i,c(1,2,3,4)])==datos.reaMkErMb.fru[i,3]) {
    mpBase[i,2]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkErMb.car[,1])) {
  if (max(datos.reaMkErMb.fru[i,c(1,2,3,4)])==datos.reaMkErMb.fru[i,4]) {
    mpBase[i,2]=3
  }
}


### Hematofagia 
#### Ausente
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.hem[i,c(1,2,3)])==datos.reaMkErMb.hem[i,1]) {
    mpBase[i,3]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.hem[i,c(1,2,3)])==datos.reaMkErMb.hem[i,2]) {
    mpBase[i,3]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.hem[i,c(1,2,3)])==datos.reaMkErMb.hem[i,3]) {
    mpBase[i,3]=2
  }
}


### Insectivoria
#### Ausente
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.ins[i,c(1,2,3,4)])==datos.reaMkErMb.ins[i,1]) {
    mpBase[i,4]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.ins[i,c(1,2,3,4)])==datos.reaMkErMb.ins[i,2]) {
    mpBase[i,4]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.ins[i,c(1,2,3,4)])==datos.reaMkErMb.ins[i,3]) {
    mpBase[i,4]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.ins[i,c(1,2,3,4)])==datos.reaMkErMb.ins[i,4]) {
    mpBase[i,4]=3
  }
}



### Nectarivoria
#### Ausente
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.nec[i,c(1,2,3,4)])==datos.reaMkErMb.nec[i,1]) {
    mpBase[i,5]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.nec[i,c(1,2,3,4)])==datos.reaMkErMb.nec[i,2]) {
    mpBase[i,5]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.nec[i,c(1,2,3,4)])==datos.reaMkErMb.nec[i,3]) {
    mpBase[i,5]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkErMb.hem[,1])) {
  if (max(datos.reaMkErMb.nec[i,c(1,2,3,4)])==datos.reaMkErMb.nec[i,4]) {
    mpBase[i,5]=3
  }
}


### Ya discretizadas las probabilidades se organizarán como binarios los estados para cada caracter. Las dietas contarán con tres columnas respectivamente organizadas como las matrices binarias: columna 1, EPC, se compondrá de los estados estrictos, predominantes y complementarios; asi sucesivamente con las demás codificaciones binarias

mpBaseBin <- data.frame("Car.EPC"=rep(0,length(mpBase$Car)),"Car.P"=0,"Car.E"=0,"Fru.EPC"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.EPC"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.EPC"=0,"Nec.EP"=0,"Nec.E"=0)

#### Carnivoria
##### Predominante y Complementario - B1
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,1]==1) {
    mpBaseBin[i,1]=1
  }
  else if (mpBase[i,1]==2) {
    mpBaseBin[i,1]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,1]==2) {
    mpBaseBin[i,2]=1
  }
}

#### Frugivoria
##### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,2]==1) {
    mpBaseBin[i,4]=1
  }
  else if (mpBase[i,2]==2) {
    mpBaseBin[i,4]=1
  }
  else if (mpBase[i,2]==3) {
    mpBaseBin[i,4]=1
  }
}

##### Predominante y Estricto - B2
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,2]==2) {
    mpBaseBin[i,5]=1
  }
  else if (mpBase[i,2]==3) {
    mpBaseBin[i,5]=1
  }
}

##### Estricto - B3
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,2]==3) {
    mpBaseBin[i,6]=1
  }
}

#### Hematofagia
##### Predominante y Complementario - B1
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,3]==1) {
    mpBaseBin[i,7]=1
  }
  else if (mpBase[i,3]==2) {
    mpBaseBin[i,7]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,3]==2) {
    mpBaseBin[i,8]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,4]==1) {
    mpBaseBin[i,10]=1
  }
  else if (mpBase[i,4]==2) {
    mpBaseBin[i,10]=1
  }
  else if (mpBase[i,4]==3) {
    mpBaseBin[i,10]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,4]==1) {
    mpBaseBin[i,11]=1
  }
  else if (mpBase[i,4]==2) {
    mpBaseBin[i,11]=1
  }
}

##### Estricto
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,4]==2) {
    mpBaseBin[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,5]==1) {
    mpBaseBin[i,13]=1
  }
  else if (mpBase[i,5]==2) {
    mpBaseBin[i,13]=1
  }
  else if (mpBase[i,5]==3) {
    mpBaseBin[i,13]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,5]==1) {
    mpBaseBin[i,14]=1
  }
  else if (mpBase[i,5]==2) {
    mpBaseBin[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBase$Car)) {
  if (mpBase[i,5]==2) {
    mpBaseBin[i,15]=1
  }
}

# Matriz Binaria 1 ####
car1 <- datos.reaMkERB1.car
long <- length(car1[,2])

mp1 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 
#maxProbBakerMkERB1 #Car-Fru-Hem-Ins-Nec

for (i in 1:long) {
  if (car1[i,1]>car1[i,2]) {
    mp1[i,1] <- 0
  }
  else if (car1[i,1]<car1[i,2]) {
    mp1[i,1] <- 1
  }
}


fru1 <- datos.reaMkERB1.fru

for (i in 1:long) {
  if (fru1[i,1]>fru1[i,2]) {
    mp1[i,2] <- 0
  }
  else if (fru1[i,1]<fru1[i,2]) {
    mp1[i,2] <- 1
  }
}


hem1 <- datos.reaMkERB1.hem

for (i in 1:long) {
  if (hem1[i,1]>hem1[i,2]) {
    mp1[i,3] <- 0
  }
  else if (hem1[i,1]<hem1[i,2]) {
    mp1[i,3] <- 1
  }
}


ins1 <- datos.reaMkERB1.ins

for (i in 1:long) {
  if (ins1[i,1]>ins1[i,2]) {
    mp1[i,4] <- 0
  }
  else if (ins1[i,1]<ins1[i,2]) {
    mp1[i,4] <- 1
  }
}


nec1 <- datos.reaMkERB1.nec

for (i in 1:long) {
  if (nec1[i,1]>nec1[i,2]) {
    mp1[i,5] <- 0
  }
  else if (nec1[i,1]<nec1[i,2]) {
    mp1[i,5] <- 1
  }
}



# Matriz Binaria 2 ####
car2 <- datos.reaMkERB2.car

mp2 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 

for (i in 1:long) {
  if (car2[i,1]>car2[i,2]) {
    mp2[i,1] <- 0
  }
  else if (car2[i,1]<car2[i,2]) {
    mp2[i,1] <- 1
  }
}


fru2 <- datos.reaMkERB2.fru

for (i in 1:long) {
  if (fru2[i,1]>fru2[i,2]) {
    mp2[i,2] <- 0
  }
  else if (fru2[i,1]<fru2[i,2]) {
    mp2[i,2] <- 1
  }
}


hem2 <- datos.reaMkERB2.hem

for (i in 1:long) {
  if (hem2[i,1]>hem2[i,2]) {
    mp2[i,3] <- 0
  }
  else if (hem2[i,1]<hem2[i,2]) {
    mp2[i,3] <- 1
  }
}


ins2 <- datos.reaMkERB2.ins

for (i in 1:long) {
  if (ins2[i,1]>ins2[i,2]) {
    mp2[i,4] <- 0
  }
  else if (ins2[i,1]<ins2[i,2]) {
    mp2[i,4] <- 1
  }
}


nec2 <- datos.reaMkERB2.nec

for (i in 1:long) {
  if (nec2[i,1]>nec2[i,2]) {
    mp2[i,5] <- 0
  }
  else if (nec2[i,1]<nec2[i,2]) {
    mp2[i,5] <- 1
  }
}



# Matriz Binaria 3 ####
# Para esta configuración no se reconstruyó carnivoria porque no habian reportes estrictos para las especies.
fru3 <- datos.reaMkERB3.fru
long <- length(fru3[,2])

mp3 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 


for (i in 1:long) {
  if (fru3[i,1]>fru3[i,2]) {
    mp3[i,2] <- 0
  }
  else if (fru3[i,1]<fru3[i,2]) {
    mp3[i,2] <- 1
  }
}


hem3 <- datos.reaMkERB3.hem

for (i in 1:long) {
  if (hem3[i,1]>hem3[i,2]) {
    mp3[i,3] <- 0
  }
  else if (hem3[i,1]<hem3[i,2]) {
    mp3[i,3] <- 1
  }
}


ins3 <- datos.reaMkERB3.ins

for (i in 1:long) {
  if (ins3[i,1]>ins3[i,2]) {
    mp3[i,4] <- 0
  }
  else if (ins3[i,1]<ins3[i,2]) {
    mp3[i,4] <- 1
  }
}


nec3 <- datos.reaMkERB3.nec

for (i in 1:long) {
  if (nec3[i,1]>nec3[i,2]) {
    mp3[i,5] <- 0
  }
  else if (nec3[i,1]<nec3[i,2]) {
    mp3[i,5] <- 1
  }
}



## Suma de la cantidad de nodos que recuperaron 1 en columnas y una columna final con el valor más alto, para luego sacar el conteo de nodos relativo. #####

NodosTotales <- data.frame("B1Car"=sum(mp1$Car),"B2Car"=sum(mp2$Car),"B3Car"=sum(mp3$Car),"TotalCar"=0,"B1Fru"=sum(mp1$Fru),"B2Fru"=sum(mp2$Fru),"B3Fru"=sum(mp3$Fru),"TotalFru"=0,"B1Hem"=sum(mp1$Hem),"B2Hem"=sum(mp2$Hem),"B3Hem"=sum(mp3$Hem),"TotalHem"=0,"B1Ins"=sum(mp1$Ins),"B2Ins"=sum(mp2$Ins),"B3Ins"=sum(mp3$Ins),"TotalIns"=0,"B1Nec"=sum(mp1$Nec),"B2Nec"=sum(mp2$Nec),"B3Nec"=sum(mp3$Nec),"TotalNec"=0) #Car-Fru-Hem-Ins-Nec

NodosTotales$TotalCar <- max(NodosTotales$B1Car,NodosTotales$B2Car,NodosTotales$B3Car)
NodosTotales$TotalFru <- max(NodosTotales$B1Fru,NodosTotales$B2Fru,NodosTotales$B3Fru)
NodosTotales$TotalHem <- max(NodosTotales$B1Hem,NodosTotales$B2Hem,NodosTotales$B3Hem)
NodosTotales$TotalIns <- max(NodosTotales$B1Ins,NodosTotales$B2Ins,NodosTotales$B3Ins)
NodosTotales$TotalNec <- max(NodosTotales$B1Nec,NodosTotales$B2Nec,NodosTotales$B3Nec)

##----


# Parsimonia. Este método arroja estados discretos, pero se organizarán como binarios las codificaciones Base y Multiestado.

## Matriz base ----
binarizacionBasePars <- data.frame("Car.CP"=rep(0,length(mpBase$Car)),"Car.P"=0,"Car.E"=0,"Fru.ECP"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.ECP"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.ECP"=0,"Nec.EP"=0,"Nec.E"=0)

### Carnivoria
#### Predominante y Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]>=1) {
    binarizacionBasePars[i,1]=1
  }
}

#### Predominante y Estricto :v
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]>=2) {
    binarizacionBasePars[i,2]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]>=3) {
    binarizacionBasePars[i,3]=1
  }
}

### Frugivoria
#### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]>=1) {
    binarizacionBasePars[i,4]=1
  }
}

#### Predominante y Estricto - B2
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]>=2) {
    binarizacionBasePars[i,5]=1
  }
}

#### Estricto - B3
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]>=3) {
    binarizacionBasePars[i,6]=1
  }
}

### Hematofagia
#### Predominante y Complementario - B1
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]>=1) {
    binarizacionBasePars[i,7]=1
  }
}

#### Predominante - B2
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]>=2) {
    binarizacionBasePars[i,8]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]>=3) {
    binarizacionBasePars[i,9]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]>=1) {
    binarizacionBasePars[i,10]=1
  }
}

#### Estricto y predominante
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]>=2) {
    binarizacionBasePars[i,11]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]>=3) {
    binarizacionBasePars[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Nec[i,1]>=1) {
    binarizacionBasePars[i,13]=1
  }
}

#### Estricto y predominante
for (i in 1:length(mpBase$Car)) {
  if  (estadosMbPars.Nec[i,1]>=2) {
    binarizacionBasePars[i,14]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if  (estadosMbPars.Nec[i,1]>=3) {
    binarizacionBasePars[i,15]=1
  }
}





# Multiestado  ----
binarizacionMultiPars <- data.frame("Carnivoria"=rep(0,length(binarizacionBasePars$Car.CP)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) 


## Carnivoria
for (i in 1:length(mpMulti$Ins)) {
  if (estadosMultiPars[i,1]==0) {
    binarizacionMultiPars[i,1]=1
  }
}

## Frugivoria
for (i in 1:length(mpMulti$Ins)) {
  if (estadosMultiPars[i,1]==1) {
    binarizacionMultiPars[i,2]=1
  }
}

## Hematofagia
for (i in 1:length(mpMulti$Ins)) {
  if (estadosMultiPars[i,1]==2) {
    binarizacionMultiPars[i,3]=1
  }
}

## Isectivoria
for (i in 1:length(mpMulti$Ins)) {
  if (estadosMultiPars[i,1]==3) {
    binarizacionMultiPars[i,4]=1
  }
}

## Nectarivoria
for (i in 1:length(mpMulti$Ins)) {
  if (estadosMultiPars[i,1]==4) {
    binarizacionMultiPars[i,5]=1
  }
}

#----




############## TRANSFORMACIÓN ASIMÉTRICA 1 ###############

# Máxima verosimilitud

## Matriz base ----

mpBaseAsim1 <- data.frame ("Car"=rep(0,length(datos.reaMkDrMb.car[,1])),"Fru"=0,"Hem"=NA,"Ins"=0,"Nec"=0)

### Carnivoria
#### Ausente. Cada estado es una columna del archivo datos.
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car[i,c(1,2,3)])==datos.reaMkDrMb.car[i,1]) {
    mpBaseAsim1[i,1]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car[i,c(1,2,3)])==datos.reaMkDrMb.car[i,2]) {
    mpBaseAsim1[i,1]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car[i,c(1,2,3)])==datos.reaMkDrMb.car[i,3]) {
    mpBaseAsim1[i,1]=2
  }
}


### Frugivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru[i,c(1,2,3,4)])==datos.reaMkDrMb.fru[i,1]) {
    mpBaseAsim1[i,2]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru[i,c(1,2,3,4)])==datos.reaMkDrMb.fru[i,2]) {
    mpBaseAsim1[i,2]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru[i,c(1,2,3,4)])==datos.reaMkDrMb.fru[i,3]) {
    mpBaseAsim1[i,2]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru[i,c(1,2,3,4)])==datos.reaMkDrMb.fru[i,4]) {
    mpBaseAsim1[i,2]=3
  }
}


### Hematofagia 
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem[i,c(1,2,3)])==datos.reaMkDrMb.hem[i,1]) {
    mpBaseAsim1[i,3]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem[i,c(1,2,3)])==datos.reaMkDrMb.hem[i,2]) {
    mpBaseAsim1[i,3]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem[i,c(1,2,3)])==datos.reaMkDrMb.hem[i,3]) {
    mpBaseAsim1[i,3]=2
  }
}


### Insectivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins[i,c(1,2,3,4)])==datos.reaMkDrMb.ins[i,1]) {
    mpBaseAsim1[i,4]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins[i,c(1,2,3,4)])==datos.reaMkDrMb.ins[i,2]) {
    mpBaseAsim1[i,4]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins[i,c(1,2,3,4)])==datos.reaMkDrMb.ins[i,3]) {
    mpBaseAsim1[i,4]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins[i,c(1,2,3,4)])==datos.reaMkDrMb.ins[i,4]) {
    mpBaseAsim1[i,4]=3
  }
}



### Nectarivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec[i,c(1,2,3,4)])==datos.reaMkDrMb.nec[i,1]) {
    mpBaseAsim1[i,5]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec[i,c(1,2,3,4)])==datos.reaMkDrMb.nec[i,2]) {
    mpBaseAsim1[i,5]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec[i,c(1,2,3,4)])==datos.reaMkDrMb.nec[i,3]) {
    mpBaseAsim1[i,5]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec[i,c(1,2,3,4)])==datos.reaMkDrMb.nec[i,4]) {
    mpBaseAsim1[i,5]=3
  }
}


### Binarización - Matriz base. ####

mpBaseBinAsim1 <- data.frame("Car.CP"=rep(0,length(mpBaseAsim1$Car)),"Car.P"=0,"Car.E"=0,"Fru.ECP"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.ECP"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.ECP"=0,"Nec.EP"=0,"Nec.E"=0)

#### Carnivoria
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,1]==1) {
    mpBaseBinAsim1[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==2) {
    mpBaseBinAsim1[i,1]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,1]==2) {
    mpBaseBinAsim1[i,2]=1
  }
}

#### Frugivoria
##### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==1) {
    mpBaseBinAsim1[i,4]=1
  }
  else if (mpBaseAsim1[i,2]==2) {
    mpBaseBinAsim1[i,4]=1
  }
  else if (mpBaseAsim1[i,2]==3) {
    mpBaseBinAsim1[i,4]=1
  }
}

##### Predominante y Estricto - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==2) {
    mpBaseBinAsim1[i,5]=1
  }
  else if (mpBaseAsim1[i,2]==3) {
    mpBaseBinAsim1[i,5]=1
  }
}

##### Estricto - B3
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==3) {
    mpBaseBinAsim1[i,6]=1
  }
}

#### Hematofagia
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,3]==1) {
    mpBaseBinAsim1[i,7]=1
  }
  else if (mpBaseAsim1[i,3]==2) {
    mpBaseBinAsim1[i,7]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,3]==2) {
    mpBaseBinAsim1[i,8]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==1) {
    mpBaseBinAsim1[i,10]=1
  }
  else if (mpBaseAsim1[i,4]==2) {
    mpBaseBinAsim1[i,10]=1
  }
  else if (mpBaseAsim1[i,4]==3) {
    mpBaseBinAsim1[i,10]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==1) {
    mpBaseBinAsim1[i,11]=1
  }
  else if (mpBaseAsim1[i,4]==2) {
    mpBaseBinAsim1[i,11]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==3) {
    mpBaseBinAsim1[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==1) {
    mpBaseBinAsim1[i,13]=1
  }
  else if (mpBaseAsim1[i,5]==2) {
    mpBaseBinAsim1[i,13]=1
  }
  else if (mpBaseAsim1[i,5]==3) {
    mpBaseBinAsim1[i,13]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==1) {
    mpBaseBinAsim1[i,14]=1
  }
  else if (mpBaseAsim1[i,5]==2) {
    mpBaseBinAsim1[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==3) {
    mpBaseBinAsim1[i,15]=1
  }
}


## Matriz multiestado ####

mpMultiAsim1 <- data.frame("Car"=datos.reaMkDRMulti[,1],"Fru"=datos.reaMkDRMulti[,2],"Hem"=datos.reaMkDRMulti[,3],"Ins"=datos.reaMkDRMulti[,4],"Nec"=datos.reaMkDRMulti[,5],"Estado"=NA,"Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) 


### Carnivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim1[i,1],mpMultiAsim1[i,2],mpMultiAsim1[i,3],mpMultiAsim1[i,4],mpMultiAsim1[i,5])==mpMultiAsim1[i,1]) {
    mpMultiAsim1[i,6]=0
  }
}

### Frugivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim1[i,1],mpMultiAsim1[i,2],mpMultiAsim1[i,3],mpMultiAsim1[i,4],mpMultiAsim1[i,5])==mpMultiAsim1[i,2]) {
    mpMultiAsim1[i,6]=1
  }
}

### Hematofagia
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim1[i,1],mpMultiAsim1[i,2],mpMultiAsim1[i,3],mpMultiAsim1[i,4],mpMultiAsim1[i,5])==mpMultiAsim1[i,3]) {
    mpMultiAsim1[i,6]=2
  }
}

### Insectivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim1[i,1],mpMultiAsim1[i,2],mpMultiAsim1[i,3],mpMultiAsim1[i,4],mpMultiAsim1[i,5])==mpMultiAsim1[i,4]) {
    mpMultiAsim1[i,6]=3
  }
}

### Nectarivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim1[i,1],mpMultiAsim1[i,2],mpMultiAsim1[i,3],mpMultiAsim1[i,4],mpMultiAsim1[i,5])==mpMultiAsim1[i,5]) {
    mpMultiAsim1[i,6]=4
  }
}


## Binarización Carnivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (mpMultiAsim1[i,6]==0) {
    mpMultiAsim1[i,7]=1
  }
}

## Binarización Frugivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (mpMultiAsim1[i,6]==1) {
    mpMultiAsim1[i,8]=1
  }
}

## Binarización Hematofagia
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (mpMultiAsim1[i,6]==2) {
    mpMultiAsim1[i,9]=1
  }
}

## Binarización Isectivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (mpMultiAsim1[i,6]==3) {
    mpMultiAsim1[i,10]=1
  }
}

## Binarización Nectarivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (mpMultiAsim1[i,6]==4) {
    mpMultiAsim1[i,11]=1
  }
}


# MATRICES BINARIAS 

## Matriz binaria 1 ####
car1Asim1 <- datos.reaMkDRB1.car
long <- length(car1Asim1[,2])

mp1Asim1 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 


for (i in 1:long) {
  if (car1Asim1[i,1]>car1Asim1[i,2]) {
    mp1Asim1[i,1] <- 0
  }
  else if (car1Asim1[i,1]<car1Asim1[i,2]) {
    mp1Asim1[i,1] <- 1
  }
}


fru1Asim1 <- datos.reaMkDRB1.fru

for (i in 1:long) {
  if (fru1Asim1[i,1]>fru1Asim1[i,2]) {
    mp1Asim1[i,2] <- 0
  }
  else if (fru1Asim1[i,1]<fru1Asim1[i,2]) {
    mp1Asim1[i,2] <- 1
  }
}


hem1Asim1 <- datos.reaMkDRB1.hem

for (i in 1:long) {
  if (hem1Asim1[i,1]>hem1Asim1[i,2]) {
    mp1Asim1[i,3] <- 0
  }
  else if (hem1Asim1[i,1]<hem1Asim1[i,2]) {
    mp1Asim1[i,3] <- 1
  }
}


ins1Asim1 <- datos.reaMkDRB1.ins

for (i in 1:long) {
  if (ins1Asim1[i,1]>ins1Asim1[i,2]) {
    mp1Asim1[i,4] <- 0
  }
  else if (ins1Asim1[i,1]<ins1Asim1[i,2]) {
    mp1Asim1[i,4] <- 1
  }
}


nec1Asim1 <- datos.reaMkDRB1.nec

for (i in 1:long) {
  if (nec1Asim1[i,1]>nec1Asim1[i,2]) {
    mp1Asim1[i,5] <- 0
  }
  else if (nec1Asim1[i,1]<nec1Asim1[i,2]) {
    mp1Asim1[i,5] <- 1
  }
}



## Matriz binaria 2 ####
car2Asim1 <- datos.reaMkDRB2.car

mp2Asim1 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long))

for (i in 1:long) {
  if (car2Asim1[i,1]>car2Asim1[i,2]) {
    mp2Asim1[i,1] <- 0
  }
  else if (car2Asim1[i,1]<car2Asim1[i,2]) {
    mp2Asim1[i,1] <- 1
  }
}


fru2Asim1 <- datos.reaMkDRB2.fru

for (i in 1:long) {
  if (fru2Asim1[i,1]>fru2Asim1[i,2]) {
    mp2Asim1[i,2] <- 0
  }
  else if (fru2Asim1[i,1]<fru2Asim1[i,2]) {
    mp2Asim1[i,2] <- 1
  }
}


hem2Asim1 <- datos.reaMkDRB2.hem

for (i in 1:long) {
  if (hem2Asim1[i,1]>hem2Asim1[i,2]) {
    mp2Asim1[i,3] <- 0
  }
  else if (hem2Asim1[i,1]<hem2Asim1[i,2]) {
    mp2Asim1[i,3] <- 1
  }
}


ins2Asim1 <- datos.reaMkDRB2.ins

for (i in 1:long) {
  if (ins2Asim1[i,1]>ins2Asim1[i,2]) {
    mp2Asim1[i,4] <- 0
  }
  else if (ins2Asim1[i,1]<ins2Asim1[i,2]) {
    mp2Asim1[i,4] <- 1
  }
}


nec2Asim1 <- datos.reaMkDRB2.nec

for (i in 1:long) {
  if (nec2Asim1[i,1]>nec2Asim1[i,2]) {
    mp2Asim1[i,5] <- 0
  }
  else if (nec2Asim1[i,1]<nec2Asim1[i,2]) {
    mp2Asim1[i,5] <- 1
  }
}



## Matriz binaria 3 ####
fru3Asim1 <- datos.reaMkDRB3.fru
long <- length(fru3Asim1[,2])

mp3Asim1 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 


for (i in 1:long) {
  if (fru3Asim1[i,1]>fru3Asim1[i,2]) {
    mp3Asim1[i,2] <- 0
  }
  else if (fru3Asim1[i,1]<fru3Asim1[i,2]) {
    mp3Asim1[i,2] <- 1
  }
}


hem3Asim1 <- datos.reaMkDRB3.hem

for (i in 1:long) {
  if (hem3Asim1[i,1]>hem3Asim1[i,2]) {
    mp3Asim1[i,3] <- 0
  }
  else if (hem3Asim1[i,1]<hem3Asim1[i,2]) {
    mp3Asim1[i,3] <- 1
  }
}


ins3Asim1 <- datos.reaMkDRB3.ins

for (i in 1:long) {
  if (ins3Asim1[i,1]>ins3Asim1[i,2]) {
    mp3Asim1[i,4] <- 0
  }
  else if (ins3Asim1[i,1]<ins3Asim1[i,2]) {
    mp3Asim1[i,4] <- 1
  }
}


nec3Asim1 <- datos.reaMkDRB3.nec

for (i in 1:long) {
  if (nec3Asim1[i,1]>nec3Asim1[i,2]) {
    mp3Asim1[i,5] <- 0
  }
  else if (nec3Asim1[i,1]<nec3Asim1[i,2]) {
    mp3Asim1[i,5] <- 1
  }
}

## 
NodosTotalesAsim1 <- data.frame("B1Car"=sum(mp1Asim1$Car),"B2Car"=sum(mp2Asim1$Car),"B3Car"=sum(mp3Asim1$Car),"B1Fru"=sum(mp1Asim1$Fru),"B2Fru"=sum(mp2Asim1$Fru),"B3Fru"=sum(mp3Asim1$Fru),"B1Hem"=sum(mp1Asim1$Hem),"B2Hem"=sum(mp2Asim1$Hem),"B3Hem"=sum(mp3Asim1$Hem),"B1Ins"=sum(mp1Asim1$Ins),"B2Ins"=sum(mp2Asim1$Ins),"B3Ins"=sum(mp3Asim1$Ins),"B1Nec"=sum(mp1Asim1$Nec),"B2Nec"=sum(mp2Asim1$Nec),"B3Nec"=sum(mp3Asim1$Nec)) #Car-Fru-Hem-Ins-Nec


## Organización de las matrices binarias. Data frame con la suma de estados presente en cada carácter (dieta) y la suma total de estos. #####

NodosTotalesAsim1 <- data.frame("B1Car"=sum(mp1Asim1$Car),"B2Car"=sum(mp2Asim1$Car),"B3Car"=sum(mp3Asim1$Car),"B1Fru"=sum(mp1Asim1$Fru),"B2Fru"=sum(mp2Asim1$Fru),"B3Fru"=sum(mp3Asim1$Fru),"B1Hem"=sum(mp1Asim1$Hem),"B2Hem"=sum(mp2Asim1$Hem),"B3Hem"=sum(mp3Asim1$Hem),"B1Ins"=sum(mp1Asim1$Ins),"B2Ins"=sum(mp2Asim1$Ins),"B3Ins"=sum(mp3Asim1$Ins),"B1Nec"=sum(mp1Asim1$Nec),"B2Nec"=sum(mp2Asim1$Nec),"B3Nec"=sum(mp3Asim1$Nec)) #Car-Fru-Hem-Ins-Nec
#----

# Parsimonia ----
## Matriz base ----
### Binarización Base pars
binarizacionBaseParsAsim1 <- data.frame("Car.CP"=rep(0,length(mpBaseAsim1$Car)),"Car.P"=0,"Car.E"=0,"Fru.ECP"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.ECP"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.ECP"=0,"Nec.EP"=0,"Nec.E"=0)

#### Carnivoria
##### Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]>=1) {
    binarizacionBaseParsAsim1[i,1]=1
  }
}

##### Predominante y Estricto :v
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]>=2) {
    binarizacionBaseParsAsim1[i,2]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]>=3) {
    binarizacionBaseParsAsim1[i,3]=1
  }
}

#### Frugivoria
##### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]>=1) {
    binarizacionBaseParsAsim1[i,4]=1
  }
}

##### Predominante y Estricto - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]>=2) {
    binarizacionBaseParsAsim1[i,5]=1
  }
}

##### Estricto - B3
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]>=3) {
    binarizacionBaseParsAsim1[i,6]=1
  }
}

#### Hematofagia
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]>=1) {
    binarizacionBaseParsAsim1[i,7]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]>=2) {
    binarizacionBaseParsAsim1[i,8]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]>=3) {
    binarizacionBaseParsAsim1[i,9]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]>=1) {
    binarizacionBaseParsAsim1[i,10]=1
  }
}

##### Estricto y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]>=2) {
    binarizacionBaseParsAsim1[i,11]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]>=3) {
    binarizacionBaseParsAsim1[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.NecAsim1[i,1]>=1) {
    binarizacionBaseParsAsim1[i,13]=1
  }
}

##### Estricto y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if  (estadosMbPars.NecAsim1[i,1]>=2) {
    binarizacionBaseParsAsim1[i,14]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if  (estadosMbPars.NecAsim1[i,1]>=3) {
    binarizacionBaseParsAsim1[i,15]=1
  }
}





# Binarización Multiestado Pars  ----
binarizacionMultiParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mp1Asim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) 


## Binarización Carnivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (estadosMultiParsAsim1[i,1]==0) {
    binarizacionMultiParsAsim1[i,1]=1
  }
}

## Binarización Frugivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (estadosMultiParsAsim1[i,1]==1) {
    binarizacionMultiParsAsim1[i,2]=1
  }
}

## Binarización Hematofagia
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (estadosMultiParsAsim1[i,1]==2) {
    binarizacionMultiParsAsim1[i,3]=1
  }
}

## Binarización Isectivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (estadosMultiParsAsim1[i,1]==3) {
    binarizacionMultiParsAsim1[i,4]=1
  }
}

## Binarización Nectarivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (estadosMultiParsAsim1[i,1]==4) {
    binarizacionMultiParsAsim1[i,5]=1
  }
}
#----


############## TRANSFORMACIÓN ASIMÉTRICA 2 ###############


# Máxima verosimilitud
## Matriz base ----

mpBaseAsim2 <- data.frame ("Car"=rep(0,length(datos.reaMkDrMb.car[,1])),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
#### Ausente. Cada estado es una columna del archivo datos.
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car2[i,c(1,2,3)])==datos.reaMkDrMb.car2[i,1]) {
    mpBaseAsim2[i,1]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car2[i,c(1,2,3)])==datos.reaMkDrMb.car2[i,2]) {
    mpBaseAsim2[i,1]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.car2[i,c(1,2,3)])==datos.reaMkDrMb.car2[i,3]) {
    mpBaseAsim2[i,1]=2
  }
}


### Frugivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru2[i,c(1,2,3,4)])==datos.reaMkDrMb.fru2[i,1]) {
    mpBaseAsim2[i,2]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru2[i,c(1,2,3,4)])==datos.reaMkDrMb.fru2[i,2]) {
    mpBaseAsim2[i,2]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru2[i,c(1,2,3,4)])==datos.reaMkDrMb.fru2[i,3]) {
    mpBaseAsim2[i,2]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.car[,1])) {
  if (max(datos.reaMkDrMb.fru2[i,c(1,2,3,4)])==datos.reaMkDrMb.fru2[i,4]) {
    mpBaseAsim2[i,2]=3
  }
}


### Hematofagia 
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem2[i,c(1,2,3)])==datos.reaMkDrMb.hem2[i,1]) {
    mpBaseAsim2[i,3]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem2[i,c(1,2,3)])==datos.reaMkDrMb.hem2[i,2]) {
    mpBaseAsim2[i,3]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.hem2[i,c(1,2,3)])==datos.reaMkDrMb.hem2[i,3]) {
    mpBaseAsim2[i,3]=2
  }
}


### Insectivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins2[i,c(1,2,3,4)])==datos.reaMkDrMb.ins2[i,1]) {
    mpBaseAsim2[i,4]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins2[i,c(1,2,3,4)])==datos.reaMkDrMb.ins2[i,2]) {
    mpBaseAsim2[i,4]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins2[i,c(1,2,3,4)])==datos.reaMkDrMb.ins2[i,3]) {
    mpBaseAsim2[i,4]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.ins2[i,c(1,2,3,4)])==datos.reaMkDrMb.ins2[i,4]) {
    mpBaseAsim2[i,4]=3
  }
}



### Nectarivoria
#### Ausente
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec2[i,c(1,2,3,4)])==datos.reaMkDrMb.nec2[i,1]) {
    mpBaseAsim2[i,5]=0
  }
}

#### Complementario
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec2[i,c(1,2,3,4)])==datos.reaMkDrMb.nec2[i,2]) {
    mpBaseAsim2[i,5]=1
  }
}

#### Predominante
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec2[i,c(1,2,3,4)])==datos.reaMkDrMb.nec2[i,3]) {
    mpBaseAsim2[i,5]=2
  }
}

#### Estricto
for (i in 1:length(datos.reaMkDrMb.hem[,1])) {
  if (max(datos.reaMkDrMb.nec2[i,c(1,2,3,4)])==datos.reaMkDrMb.nec2[i,4]) {
    mpBaseAsim2[i,5]=3
  }
}



## Mb - binaria. ####

mpBaseBinAsim2 <- data.frame("Car.CP"=rep(0,length(mpBaseAsim1$Car)),"Car.P"=0,"Car.E"=0,"Fru.ECP"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.ECP"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.ECP"=0,"Nec.EP"=0,"Nec.E"=0)

#### Carnivoria
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,1]==1) {
    mpBaseBinAsim2[i,1]=1
  }
  else if (mpBaseAsim2[i,1]==2) {
    mpBaseBinAsim2[i,1]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,1]==2) {
    mpBaseBinAsim2[i,2]=1
  }
}

#### Frugivoria
##### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,2]==1) {
    mpBaseBinAsim2[i,4]=1
  }
  else if (mpBaseAsim2[i,2]==2) {
    mpBaseBinAsim2[i,4]=1
  }
  else if (mpBaseAsim2[i,2]==3) {
    mpBaseBinAsim2[i,4]=1
  }
}

##### Predominante y Estricto - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,2]==2) {
    mpBaseBinAsim2[i,5]=1
  }
  else if (mpBaseAsim2[i,2]==3) {
    mpBaseBinAsim2[i,5]=1
  }
}

##### Estricto - B3
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,2]==3) {
    mpBaseBinAsim2[i,6]=1
  }
}

#### Hematofagia
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,3]==1) {
    mpBaseBinAsim2[i,7]=1
  }
  else if (mpBaseAsim2[i,3]==2) {
    mpBaseBinAsim2[i,7]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,3]==2) {
    mpBaseBinAsim2[i,8]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,4]==1) {
    mpBaseBinAsim2[i,10]=1
  }
  else if (mpBaseAsim2[i,4]==2) {
    mpBaseBinAsim2[i,10]=1
  }
  else if (mpBaseAsim2[i,4]==3) {
    mpBaseBinAsim2[i,10]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,4]==1) {
    mpBaseBinAsim2[i,11]=1
  }
  else if (mpBaseAsim2[i,4]==2) {
    mpBaseBinAsim2[i,11]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,4]==3) {
    mpBaseBinAsim2[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,5]==1) {
    mpBaseBinAsim2[i,13]=1
  }
  else if (mpBaseAsim2[i,5]==2) {
    mpBaseBinAsim2[i,13]=1
  }
  else if (mpBaseAsim2[i,5]==3) {
    mpBaseBinAsim2[i,13]=1
  }
}

##### Complementario y predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,5]==1) {
    mpBaseBinAsim2[i,14]=1
  }
  else if (mpBaseAsim2[i,5]==2) {
    mpBaseBinAsim2[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,5]==3) {
    mpBaseBinAsim2[i,15]=1
  }
}


## Matriz multiestado ####

mpMultiAsim2 <- data.frame("Car"=datos.reaMkDRMulti2[,1],"Fru"=datos.reaMkDRMulti2[,2],"Hem"=datos.reaMkDRMulti2[,3],"Ins"=datos.reaMkDRMulti2[,4],"Nec"=datos.reaMkDRMulti2[,5],"Estado"=NA,"Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) 


### Carnivoria
for (i in 1:length(mpMultiAsim1$Ins)) {
  if (max(mpMultiAsim2[i,1],mpMultiAsim2[i,2],mpMultiAsim2[i,3],mpMultiAsim2[i,4],mpMultiAsim2[i,5])==mpMultiAsim2[i,1]) {
    mpMultiAsim2[i,6]=0
  }
}

### Frugivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (max(mpMultiAsim2[i,1],mpMultiAsim2[i,2],mpMultiAsim2[i,3],mpMultiAsim2[i,4],mpMultiAsim2[i,5])==mpMultiAsim2[i,2]) {
    mpMultiAsim2[i,6]=1
  }
}

### Hematofagia
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (max(mpMultiAsim2[i,1],mpMultiAsim2[i,2],mpMultiAsim2[i,3],mpMultiAsim2[i,4],mpMultiAsim2[i,5])==mpMultiAsim2[i,3]) {
    mpMultiAsim2[i,6]=2
  }
}

### Insectivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (max(mpMultiAsim2[i,1],mpMultiAsim2[i,2],mpMultiAsim2[i,3],mpMultiAsim2[i,4],mpMultiAsim2[i,5])==mpMultiAsim2[i,4]) {
    mpMultiAsim2[i,6]=3
  }
}

### Nectarivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (max(mpMultiAsim2[i,1],mpMultiAsim2[i,2],mpMultiAsim2[i,3],mpMultiAsim2[i,4],mpMultiAsim2[i,5])==mpMultiAsim2[i,5]) {
    mpMultiAsim2[i,6]=4
  }
}


## Binarización Carnivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (mpMultiAsim2[i,6]==0) {
    mpMultiAsim2[i,7]=1
  }
}

## Binarización Frugivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (mpMultiAsim2[i,6]==1) {
    mpMultiAsim2[i,8]=1
  }
}

## Binarización Hematofagia
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (mpMultiAsim2[i,6]==2) {
    mpMultiAsim2[i,9]=1
  }
}

## Binarización Isectivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (mpMultiAsim2[i,6]==3) {
    mpMultiAsim2[i,10]=1
  }
}

## Binarización Nectarivoria
for (i in 1:length(mpMultiAsim2$Ins)) {
  if (mpMultiAsim2[i,6]==4) {
    mpMultiAsim2[i,11]=1
  }
}


## Matriz binaria 1 ####
car1Asim2 <- datos.reaMkDRB1.car2
long <- length(car1Asim2[,2])

mp1Asim2 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 


for (i in 1:long) {
  if (car1Asim2[i,1]>car1Asim2[i,2]) {
    mp1Asim2[i,1] <- 0
  }
  else if (car1Asim2[i,1]<car1Asim2[i,2]) {
    mp1Asim2[i,1] <- 1
  }
}


fru1Asim2 <- datos.reaMkDRB1.fru2

for (i in 1:long) {
  if (fru1Asim2[i,1]>fru1Asim2[i,2]) {
    mp1Asim2[i,2] <- 0
  }
  else if (fru1Asim2[i,1]<fru1Asim2[i,2]) {
    mp1Asim2[i,2] <- 1
  }
}


hem1Asim2 <- datos.reaMkDRB1.hem2

for (i in 1:long) {
  if (hem1Asim2[i,1]>hem1Asim2[i,2]) {
    mp1Asim2[i,3] <- 0
  }
  else if (hem1Asim2[i,1]<hem1Asim2[i,2]) {
    mp1Asim2[i,3] <- 1
  }
}


ins1Asim2 <- datos.reaMkDRB1.ins2

for (i in 1:long) {
  if (ins1Asim2[i,1]>ins1Asim2[i,2]) {
    mp1Asim2[i,4] <- 0
  }
  else if (ins1Asim2[i,1]<ins1Asim2[i,2]) {
    mp1Asim2[i,4] <- 1
  }
}


nec1Asim2 <- datos.reaMkDRB1.nec2

for (i in 1:long) {
  if (nec1Asim2[i,1]>nec1Asim2[i,2]) {
    mp1Asim2[i,5] <- 0
  }
  else if (nec1Asim2[i,1]<nec1Asim2[i,2]) {
    mp1Asim2[i,5] <- 1
  }
}


## Matriz binaria 2 ####
car2Asim2 <- datos.reaMkDRB2.car2
long <- length(car2Asim2[,2])

mp2Asim2 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 


for (i in 1:long) {
  if (car2Asim2[i,1]>car2Asim2[i,2]) {
    mp2Asim2[i,1] <- 0
  }
  else if (car2Asim2[i,1]<car2Asim2[i,2]) {
    mp2Asim2[i,1] <- 1
  }
}


fru2Asim2 <- datos.reaMkDRB2.fru2

for (i in 1:long) {
  if (fru2Asim2[i,1]>fru2Asim2[i,2]) {
    mp2Asim2[i,2] <- 0
  }
  else if (fru2Asim2[i,1]<fru2Asim2[i,2]) {
    mp2Asim2[i,2] <- 1
  }
}


hem2Asim2 <- datos.reaMkDRB2.hem2

for (i in 1:long) {
  if (hem2Asim2[i,1]>hem2Asim2[i,2]) {
    mp2Asim2[i,3] <- 0
  }
  else if (hem2Asim2[i,1]<hem2Asim2[i,2]) {
    mp2Asim2[i,3] <- 1
  }
}


ins2Asim2 <- datos.reaMkDRB2.ins2

for (i in 1:long) {
  if (ins2Asim2[i,1]>ins2Asim2[i,2]) {
    mp2Asim2[i,4] <- 0
  }
  else if (ins2Asim2[i,1]<ins2Asim2[i,2]) {
    mp2Asim2[i,4] <- 1
  }
}


nec2Asim2 <- datos.reaMkDRB2.nec2

for (i in 1:long) {
  if (nec2Asim2[i,1]>nec2Asim2[i,2]) {
    mp2Asim2[i,5] <- 0
  }
  else if (nec2Asim2[i,1]<nec2Asim2[i,2]) {
    mp2Asim2[i,5] <- 1
  }
}


# Matriz binaria 3 ----
fru3Asim2 <- datos.reaMkDRB3.fru2
long <- length(fru3Asim2[,2])

mp3Asim2 <- data.frame("Car"=rep(0,long),"Fru"=rep(0,long),"Hem"=rep(0,long),"Ins"=rep(0,long),"Nec"=rep(0,long)) 



for (i in 1:long) {
  if (fru3Asim2[i,1]>fru3Asim2[i,2]) {
    mp3Asim2[i,2] <- 0
  }
  else if (fru3Asim2[i,1]<fru3Asim2[i,2]) {
    mp3Asim2[i,2] <- 1
  }
}


hem3Asim2 <- datos.reaMkDRB3.hem2

for (i in 1:long) {
  if (hem3Asim2[i,1]>hem3Asim2[i,2]) {
    mp3Asim2[i,3] <- 0
  }
  else if (hem3Asim2[i,1]<hem3Asim2[i,2]) {
    mp3Asim2[i,3] <- 1
  }
}


ins3Asim2 <- datos.reaMkDRB3.ins2

for (i in 1:long) {
  if (ins3Asim2[i,1]>ins3Asim2[i,2]) {
    mp3Asim2[i,4] <- 0
  }
  else if (ins3Asim2[i,1]<ins3Asim2[i,2]) {
    mp3Asim2[i,4] <- 1
  }
}


nec3Asim2 <- datos.reaMkDRB3.nec2

for (i in 1:long) {
  if (nec3Asim2[i,1]>nec3Asim2[i,2]) {
    mp3Asim2[i,5] <- 0
  }
  else if (nec3Asim2[i,1]<nec3Asim2[i,2]) {
    mp3Asim2[i,5] <- 1
  }
}

## Organización de las matrices binarias. Data frame con la suma de estados presente en cada carácter (dieta) y la suma total de estos. #####

NodosTotalesAsim2 <- data.frame("B1Car"=sum(mp1Asim2$Car),"B2Car"=sum(mp2Asim2$Car),"B3Car"=sum(mp3Asim2$Car),"B1Fru"=sum(mp1Asim2$Fru),"B2Fru"=sum(mp2Asim2$Fru),"B3Fru"=sum(mp3Asim2$Fru),"B1Hem"=sum(mp1Asim2$Hem),"B2Hem"=sum(mp2Asim2$Hem),"B3Hem"=sum(mp3Asim2$Hem),"B1Ins"=sum(mp1Asim2$Ins),"B2Ins"=sum(mp2Asim2$Ins),"B3Ins"=sum(mp3Asim2$Ins),"B1Nec"=sum(mp1Asim2$Nec),"B2Nec"=sum(mp2Asim2$Nec),"B3Nec"=sum(mp3Asim2$Nec)) #Car-Fru-Hem-Ins-Nec

#----

# Parsimonia 

## Matriz base ----
### Binarización Base pars
binarizacionBaseParsAsim2 <- data.frame("Car.CP"=rep(0,length(mp1Asim1$Car)),"Car.P"=0,"Car.E"=0,"Fru.ECP"=0,"Fru.EP"=0,"Fru.E"=0,"Hem.CP"=0,"Hem.P"=0,"Hem.E"=0,"Ins.ECP"=0,"Ins.EP"=0,"Ins.E"=0,"Nec.ECP"=0,"Nec.EP"=0,"Nec.E"=0)

#### Carnivoria
##### Predominante y Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]>=1) {
    binarizacionBaseParsAsim2[i,1]=1
  }
}

##### Predominante y Estricto 
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]>=2) {
    binarizacionBaseParsAsim2[i,2]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]>=3) {
    binarizacionBaseParsAsim2[i,3]=1
  }
}

#### Frugivoria
##### Estricto, Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]>=1) {
    binarizacionBaseParsAsim2[i,4]=1
  }
}

##### Predominante y Estricto - B2
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]>=2) {
    binarizacionBaseParsAsim2[i,5]=1
  }
}

##### Estricto - B3
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]>=3) {
    binarizacionBaseParsAsim2[i,6]=1
  }
}

#### Hematofagia
##### Predominante y Complementario - B1
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]>=1) {
    binarizacionBaseParsAsim2[i,7]=1
  }
}

##### Predominante - B2
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]>=2) {
    binarizacionBaseParsAsim2[i,8]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]>=3) {
    binarizacionBaseParsAsim2[i,9]=1
  }
}

### Insectivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]>=1) {
    binarizacionBaseParsAsim2[i,10]=1
  }
}

##### Estricto y predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]>=2) {
    binarizacionBaseParsAsim2[i,11]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]>=3) {
    binarizacionBaseParsAsim2[i,12]=1
  }
}

### Nectarivoria
#### Estricto, Predominante y Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.NecAsim2[i,1]>=1) {
    binarizacionBaseParsAsim2[i,13]=1
  }
}

##### Estricto y predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if  (estadosMbPars.NecAsim2[i,1]>=2) {
    binarizacionBaseParsAsim2[i,14]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if  (estadosMbPars.NecAsim2[i,1]>=3) {
    binarizacionBaseParsAsim2[i,15]=1
  }
}





# Binarización Multiestado Pars  ----
binarizacionMultiParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mp1Asim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) 


## Binarización Carnivoria
for (i in 1:length(mp1Asim1$Ins)) {
  if (estadosMultiParsAsim2[i,1]==0) {
    binarizacionMultiParsAsim2[i,1]=1
  }
}

## Binarización Frugivoria
for (i in 1:length(mp1Asim1$Ins)) {
  if (estadosMultiParsAsim2[i,1]==1) {
    binarizacionMultiParsAsim2[i,2]=1
  }
}

## Binarización Hematofagia
for (i in 1:length(mp1Asim1$Ins)) {
  if (estadosMultiParsAsim2[i,1]==2) {
    binarizacionMultiParsAsim2[i,3]=1
  }
}

## Binarización Isectivoria
for (i in 1:length(mp1Asim1$Ins)) {
  if (estadosMultiParsAsim2[i,1]==3) {
    binarizacionMultiParsAsim2[i,4]=1
  }
}

## Binarización Nectarivoria
for (i in 1:length(mp1Asim1$Ins)) {
  if (estadosMultiParsAsim2[i,1]==4) {
    binarizacionMultiParsAsim2[i,5]=1
  }
}

