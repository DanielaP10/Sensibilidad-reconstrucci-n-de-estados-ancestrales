#source("sensibilidad-0.Discretizaciones y tales.Tesis.R")


########################################################### COMPARACIONES TRANSFORMACIÓN SIMÉTRICA 

# Puesto que Mb cuenta con cuatro estados distintos dependiendo de la frecuencia de la dieta y estos están organizados por columnas, se usará la columna EPC (estados estricto, predominante y complementario) para las comparaciones con Multi. Para las matrices binarias dependerá de la columna correspondiente: B1, EPC; B2, PC (estados predominante y complementario); B3, E (estado estricto). 


############################################################ Comparaciones Máxima verosimilitud 

## B1 vs B2 ----
compB1vsB2 <- data.frame("B1Car-B2Car"=rep(0,length(mp1$Car)),"B1Fru-B2Fru"=0,"B1Hem-B2Hem"=0,"B1Ins-B2Ins"=0,"B1Nec-B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,1]==1&mp2[i,1]==1) {
    compB1vsB2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,2]==1&mp2[i,2]==1) {
    compB1vsB2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1[i,3]==1&mp2[i,3]==1) {
    compB1vsB2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,4]==1&mp2[i,4]==1) {
    compB1vsB2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,5]==1&mp2[i,5]==1) {
    compB1vsB2[i,5]=1
  }
}



## B1 vs B3 ----
compB1vsB3 <- data.frame("B1Car-B3Car"=rep(0,length(mp1$Car)),"B1Fru-B3Fru"=0,"B1Hem-B3Hem"=0,"B1Ins-B3Ins"=0,"B1Nec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,1]==1&mp3[i,1]==1) {
    compB1vsB3[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,2]==1&mp3[i,2]==1) {
    compB1vsB3[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1[i,3]==1&mp3[i,3]==1) {
    compB1vsB3[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,4]==1&mp3[i,4]==1) {
    compB1vsB3[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1[i,5]==1&mp3[i,5]==1) {
    compB1vsB3[i,5]=1
  }
}


## B2 vs B3 ----
compB2vsB3 <- data.frame("B2Car-B3Car"=rep(0,length(mp1$Car)),"B2Fru-B3Fru"=0,"B2Hem-B3Hem"=0,"B2Ins-B3Ins"=0,"B2Nec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp2[i,1]==1&mp3[i,1]==1) {
    compB2vsB3[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp2[i,2]==1&mp3[i,2]==1) {
    compB2vsB3[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp2[i,3]==1&mp3[i,3]==1) {
    compB2vsB3[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp2[i,4]==1&mp3[i,4]==1) {
    compB2vsB3[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp2[i,5]==1&mp3[i,5]==1) {
    compB2vsB3[i,5]=1
  }
}






## Multi vs B1 ####
compMultivsB1 <- data.frame("MultiCar-B1Car"=rep(0,length(mp1$Car)),"MultiFru-B1Fru"=0,"MultiHem-B1Hem"=0,"MultiIns-B1Ins"=0,"MultiNec-B1Nec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,7]==1&mp1[i,1]==1) {
    compMultivsB1[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,8]==1&mp1[i,2]==1) {
    compMultivsB1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,9]==1&mp1[i,3]==1) {
    compMultivsB1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,10]==1&mp1[i,4]==1) {
    compMultivsB1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,11]==1&mp1[i,5]==1) {
    compMultivsB1[i,5]=1
  }
}



## Multi vs B2 ####
compMultivsB2 <- data.frame("Carnivoria"=rep(0,length(mp1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,7]==1&mp2[i,1]==1) {
    compMultivsB2[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,8]==1&mp2[i,2]==1) {
    compMultivsB2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,9]==1&mp2[i,3]==1) {
    compMultivsB2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,10]==1&mp2[i,4]==1) {
    compMultivsB2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,11]==1&mp2[i,5]==1) {
    compMultivsB2[i,5]=1
  }
}



## Multi vs B3 ####
compMultivsB3 <- data.frame("Carnivoria"=rep(0,length(mp1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,7]==1&mp3[i,1]==1) {
    compMultivsB3[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,8]==1&mp3[i,2]==1) {
    compMultivsB3[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,9]==1&mp3[i,3]==1) {
    compMultivsB3[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,10]==1&mp3[i,4]==1) {
    compMultivsB3[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMulti[i,11]==1&mp3[i,5]==1) {
    compMultivsB3[i,5]=1
  }
}






  

  


## Mb vs B1 ####
compMbvsB1 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1[i,1]==1&mpBaseBin[i,1]==1) {
    compMbvsB1[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1[i,2]==1&mpBaseBin[i,4]==1) {
    compMbvsB1[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp1[i,3]==1&mpBaseBin[i,7]==1) {
    compMbvsB1[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1[i,4]==1&mpBaseBin[i,10]==1) {
    compMbvsB1[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1[i,5]==1&mpBaseBin[i,13]==1) {
    compMbvsB1[i,5]=1
  }
}


## Mb vs B2 ####
compMbvsB2 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2[i,1]==1&mpBaseBin[i,2]==1) {
    compMbvsB2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2[i,2]==1&mpBaseBin[i,5]==1) {
    compMbvsB2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp2[i,3]==1&mpBaseBin[i,8]==1) {
    compMbvsB2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2[i,4]==1&mpBaseBin[i,11]==1) {
    compMbvsB2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2[i,5]==1&mpBaseBin[i,14]==1) {
    compMbvsB2[i,5]=1
  }
}



## Mb vs B3 ####
compMbvsB3 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3[i,1]==1&mpBaseBin[i,3]==1) {
    compMbvsB3[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3[i,2]==1&mpBaseBin[i,6]==1) {
    compMbvsB3[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp3[i,3]==1&mpBaseBin[i,9]==1) {
    compMbvsB3[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3[i,4]==1&mpBaseBin[i,12]==1) {
    compMbvsB3[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3[i,5]==1&mpBaseBin[i,15]==1) {
    compMbvsB3[i,5]=1
  }
}

## Mb vs Multi ####
compMbvsMulti <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMulti[i,7]==1&mpBaseBin[i,1]==1) {
    compMbvsMulti[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMulti[i,8]==1&mpBaseBin[i,4]==1) {
    compMbvsMulti[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mpMulti[i,9]==1&mpBaseBin[i,7]==1) {
    compMbvsMulti[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMulti[i,10]==1&mpBaseBin[i,10]==1) {
    compMbvsMulti[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMulti[i,11]==1&mpBaseBin[i,13]==1) {
    compMbvsMulti[i,5]=1
  }
}



#----




# Comparaciones parsimonia 

## Mb vs Multi ----
compMbParsvsMultiPars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,1]==1&binarizacionMultiPars[i,1]==1)   { compMbParsvsMultiPars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,4]==1&binarizacionMultiPars[i,2]==1)   { compMbParsvsMultiPars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,7]==1&binarizacionMultiPars[i,3]==1)   { compMbParsvsMultiPars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,10]==1&binarizacionMultiPars[i,4]==1)   { compMbParsvsMultiPars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,13]==1&binarizacionMultiPars[i,5]==1)   { compMbParsvsMultiPars[i,5]=1
  }
}

## Mb vs B1  ----
compMbParsvsB1Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,1]==1&estadosB1Pars.Car[i,1]==1)   { compMbParsvsB1Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,4]==1&estadosB1Pars.Fru[i,1]==1)   { compMbParsvsB1Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,7]==1&estadosB1Pars.Hem[i,1]==1)   { compMbParsvsB1Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,10]==1&estadosB1Pars.Ins[i,1]==1)   { compMbParsvsB1Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,13]==1&estadosB1Pars.Nec[i,1]==1)   { compMbParsvsB1Pars[i,5]=1
  }
}

## Mb vs B2 ----
compMbParsvsB2Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,2]==1&estadosB2Pars.Car[i,1]==1)   { compMbParsvsB2Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,5]==1&estadosB2Pars.Fru[i,1]==1)   { compMbParsvsB2Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,8]==1&estadosB2Pars.Hem[i,1]==1)   { compMbParsvsB2Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,11]==1&estadosB2Pars.Ins[i,1]==1)   { compMbParsvsB2Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,14]==1&estadosB2Pars.Nec[i,1]==1)   { compMbParsvsB2Pars[i,5]=1
  }
}


## Mb vs B3 ----
compMbParsvsB3Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,3]==1&estadosB3Pars.Car[i,1]==1)   { compMbParsvsB3Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,6]==1&estadosB3Pars.Fru[i,1]==1)   { compMbParsvsB3Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,9]==1&estadosB3Pars.Hem[i,1]==1)   { compMbParsvsB3Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,12]==1&estadosB3Pars.Ins[i,1]==1)   { compMbParsvsB3Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionBasePars[i,15]==1&estadosB3Pars.Nec[i,1]==1)   { compMbParsvsB3Pars[i,5]=1
  }
}



## Multi vs B1 ----
compMultiParsvsB1Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,1]==1&estadosB1Pars.Car[i,1]==1)   { compMultiParsvsB1Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,2]==1&estadosB1Pars.Fru[i,1]==1)   { compMultiParsvsB1Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,3]==1&estadosB1Pars.Hem[i,1]==1)   { compMultiParsvsB1Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,4]==1&estadosB1Pars.Ins[i,1]==1)   { compMultiParsvsB1Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,5]==1&estadosB1Pars.Nec[i,1]==1)   { compMultiParsvsB1Pars[i,5]=1
  }
}



## Multi vs B2 ----
compMultiParsvsB2Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,1]==1&estadosB2Pars.Car[i,1]==1)   { compMultiParsvsB2Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,2]==1&estadosB2Pars.Fru[i,1]==1)   { compMultiParsvsB2Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,3]==1&estadosB2Pars.Hem[i,1]==1)   { compMultiParsvsB2Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,4]==1&estadosB2Pars.Ins[i,1]==1)   { compMultiParsvsB2Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,5]==1&estadosB2Pars.Nec[i,1]==1)   { compMultiParsvsB2Pars[i,5]=1
  }
}



## Multi vs B3 ----
compMultiParsvsB3Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,1]==1&estadosB3Pars.Car[i,1]==1)   { compMultiParsvsB3Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,2]==1&estadosB3Pars.Fru[i,1]==1)   { compMultiParsvsB3Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,3]==1&estadosB3Pars.Hem[i,1]==1)   { compMultiParsvsB3Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,4]==1&estadosB3Pars.Ins[i,1]==1)   { compMultiParsvsB3Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(binarizacionMultiPars[i,5]==1&estadosB3Pars.Nec[i,1]==1)   { compMultiParsvsB3Pars[i,5]=1
  }
}



## B1 vs B2 ----
compB1ParsvsB2Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Car[i,1]==1&estadosB2Pars.Car[i,1]==1)   { compB1ParsvsB2Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Fru[i,1]==1&estadosB2Pars.Fru[i,1]==1)   { compB1ParsvsB2Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Hem[i,1]==1&estadosB2Pars.Hem[i,1]==1)   { compB1ParsvsB2Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Ins[i,1]==1&estadosB2Pars.Ins[i,1]==1)   { compB1ParsvsB2Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Nec[i,1]==1&estadosB2Pars.Nec[i,1]==1)   { compB1ParsvsB2Pars[i,5]=1
  }
}



## B1 vs B3 ----
compB1ParsvsB3Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Car[i,1]==1&estadosB3Pars.Car[i,1]==1)   { compB1ParsvsB3Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Fru[i,1]==1&estadosB3Pars.Fru[i,1]==1)   { compB1ParsvsB3Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Hem[i,1]==1&estadosB3Pars.Hem[i,1]==1)   { compB1ParsvsB3Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Ins[i,1]==1&estadosB3Pars.Ins[i,1]==1)   { compB1ParsvsB3Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(estadosB1Pars.Nec[i,1]==1&estadosB3Pars.Nec[i,1]==1)   { compB1ParsvsB3Pars[i,5]=1
  }
}





## B2 vs B3 ----
compB2ParsvsB3Pars <- data.frame("Carnivoria"=rep(0,length(mpBase$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBase$Car)) {
  if(estadosB2Pars.Car[i,1]==1&estadosB3Pars.Car[i,1]==1)   { compB2ParsvsB3Pars[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBase$Car)) {
  if(estadosB2Pars.Fru[i,1]==1&estadosB3Pars.Fru[i,1]==1)   { compB2ParsvsB3Pars[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBase$Car)) {
  if(estadosB2Pars.Hem[i,1]==1&estadosB3Pars.Hem[i,1]==1)   { compB2ParsvsB3Pars[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBase$Car)) {
  if(estadosB2Pars.Ins[i,1]==1&estadosB3Pars.Ins[i,1]==1)   { compB2ParsvsB3Pars[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBase$Car)) {
  if(estadosB2Pars.Nec[i,1]==1&estadosB3Pars.Nec[i,1]==1)   { compB2ParsvsB3Pars[i,5]=1
  }
}

#-----




###########################################################         COMPARACIONES TRANSFORMACIÓN ASIMÉTRICA 1 ----


# Máxima verosimilitud 

## B1 vs B2 ----
compB1vsB2Asim1 <- data.frame("B1Car-B2Car"=rep(0,length(mp1$Car)),"B1Fru-B2Fru"=0,"B1Hem-B2Hem"=0,"B1Ins-B2Ins"=0,"B1Nec-B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,1]==1&mp2Asim1[i,1]==1) {
    compB1vsB2Asim1[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,2]==1&mp2Asim1[i,2]==1) {
    compB1vsB2Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,3]==1&mp2Asim1[i,3]==1) {
    compB1vsB2Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,4]==1&mp2Asim1[i,4]==1) {
    compB1vsB2Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,5]==1&mp2Asim1[i,5]==1) {
    compB1vsB2Asim1[i,5]=1
  }
}



## B1 vs B3 ----
compB1vsB3Asim1 <- data.frame("B1Car-B3Car"=rep(0,length(mp1$Car)),"B1Fru-B3Fru"=0,"B1Hem-B3Hem"=0,"B1Ins-B3Ins"=0,"B1Nec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,1]==1&mp3Asim1[i,1]==1) {
    compB1vsB3Asim1[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,2]==1&mp3Asim1[i,2]==1) {
    compB1vsB3Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,3]==1&mp3Asim1[i,3]==1) {
    compB1vsB3Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,4]==1&mp3Asim1[i,4]==1) {
    compB1vsB3Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim1[i,5]==1&mp3Asim1[i,5]==1) {
    compB1vsB3Asim1[i,5]=1
  }
}


## B2 vs B3 ----
compB2vsB3Asim1 <- data.frame("B2Car-B3Car"=rep(0,length(mp1$Car)),"B2Fru-B3Fru"=0,"B2Hem-B3Hem"=0,"B2Ins-B3Ins"=0,"B2Nec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim1[i,1]==1&mp3Asim1[i,1]==1) {
    compB2vsB3Asim1[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim1[i,2]==1&mp3Asim1[i,2]==1) {
    compB2vsB3Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp2Asim1[i,3]==1&mp3Asim1[i,3]==1) {
    compB2vsB3Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim1[i,4]==1&mp3Asim1[i,4]==1) {
    compB2vsB3Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim1[i,5]==1&mp3Asim1[i,5]==1) {
    compB2vsB3Asim1[i,5]=1
  }
}






## Multi vs B1 ####
compMultivsB1Asim1 <- data.frame("MultiCar-B1Car"=rep(0,length(mp1$Car)),"MultiFru-B1Fru"=0,"MultiHem-B1Hem"=0,"MultiIns-B1Ins"=0,"MultiNec-B1Nec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,7]==1&mp1Asim1[i,1]==1) {
    compMultivsB1Asim1[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,8]==1&mp1Asim1[i,2]==1) {
    compMultivsB1Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,9]==1&mp1Asim1[i,3]==1) {
    compMultivsB1Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,10]==1&mp1Asim1[i,4]==1) {
    compMultivsB1Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,11]==1&mp1Asim1[i,5]==1) {
    compMultivsB1Asim1[i,5]=1
  }
}



## Multi vs B2 ####
compMultivsB2Asim1 <- data.frame("Carnivoria"=rep(0,length(mp1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,7]==1&mp2Asim1[i,1]==1) {
    compMultivsB2Asim1[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,8]==1&mp2Asim1[i,2]==1) {
    compMultivsB2Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,9]==1&mp2Asim1[i,3]==1) {
    compMultivsB2Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,10]==1&mp2Asim1[i,4]==1) {
    compMultivsB2Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,11]==1&mp2Asim1[i,5]==1) {
    compMultivsB2Asim1[i,5]=1
  }
}



## Multi vs B3 ####
compMultivsB3Asim1 <- data.frame("Carnivoria"=rep(0,length(mp1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,7]==1&mp3Asim1[i,1]==1) {
    compMultivsB3Asim1[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,8]==1&mp3Asim1[i,2]==1) {
    compMultivsB3Asim1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,9]==1&mp3Asim1[i,3]==1) {
    compMultivsB3Asim1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,10]==1&mp3Asim1[i,4]==1) {
    compMultivsB3Asim1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim1[i,11]==1&mp3Asim1[i,5]==1) {
    compMultivsB3Asim1[i,5]=1
  }
}











## Mb vs B1 ####
compMbvsB1Asim1 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim1[i,1]==1&mpBaseBinAsim1[i,1]==1) {
    compMbvsB1Asim1[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim1[i,2]==1&mpBaseBinAsim1[i,4]==1) {
    compMbvsB1Asim1[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim1[i,3]==1&mpBaseBinAsim1[i,7]==1) {
    compMbvsB1Asim1[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim1[i,4]==1&mpBaseBinAsim1[i,10]==1) {
    compMbvsB1Asim1[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim1[i,5]==1&mpBaseBinAsim1[i,13]==1) {
    compMbvsB1Asim1[i,5]=1
  }
}


## Mb vs B2 ####
compMbvsB2Asim1 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim1[i,1]==1&mpBaseBinAsim1[i,2]==1) {
    compMbvsB2Asim1[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim1[i,2]==1&mpBaseBinAsim1[i,5]==1) {
    compMbvsB2Asim1[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim1[i,3]==1&mpBaseBinAsim1[i,8]==1) {
    compMbvsB2Asim1[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim1[i,4]==1&mpBaseBinAsim1[i,11]==1) {
    compMbvsB2Asim1[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim1[i,5]==1&mpBaseBinAsim1[i,14]==1) {
    compMbvsB2Asim1[i,5]=1
  }
}



## Mb vs B3 ####
compMbvsB3Asim1 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim1[i,1]==1&mpBaseBinAsim1[i,3]==1) {
    compMbvsB3Asim1[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim1[i,2]==1&mpBaseBinAsim1[i,6]==1) {
    compMbvsB3Asim1[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim1[i,3]==1&mpBaseBinAsim1[i,9]==1) {
    compMbvsB3Asim1[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim1[i,4]==1&mpBaseBinAsim1[i,12]==1) {
    compMbvsB3Asim1[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim1[i,5]==1&mpBaseBinAsim1[i,15]==1) {
    compMbvsB3Asim1[i,5]=1
  }
}

## Mb vs Multi ####
compMbvsMultiAsim1 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim1[i,7]==1&mpBaseBinAsim1[i,1]==1) {
    compMbvsMultiAsim1[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim1[i,8]==1&mpBaseBinAsim1[i,4]==1) {
    compMbvsMultiAsim1[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim1[i,9]==1&mpBaseBinAsim1[i,7]==1) {
    compMbvsMultiAsim1[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim1[i,10]==1&mpBaseBinAsim1[i,10]==1) {
    compMbvsMultiAsim1[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim1[i,11]==1&mpBaseBinAsim1[i,13]==1) {
    compMbvsMultiAsim1[i,5]=1
  }
}

#----



# Parsimonia

## Multi vs Mb ----
compMbParsvsMultiParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,1]==1&binarizacionMultiParsAsim1[i,1]==1)   { compMbParsvsMultiParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,4]==1&binarizacionMultiParsAsim1[i,2]==1)   { compMbParsvsMultiParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,7]==1&binarizacionMultiParsAsim1[i,3]==1)   { compMbParsvsMultiParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,10]==1&binarizacionMultiParsAsim1[i,4]==1)   { compMbParsvsMultiParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,13]==1&binarizacionMultiParsAsim1[i,5]==1)   { compMbParsvsMultiParsAsim1[i,5]=1
  }
}

## Mb vs B1  ----
compMbParsvsB1ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,1]==1&estadosB1Pars.CarAsim1[i,1]==1)   
  { compMbParsvsB1ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,4]==1&estadosB1Pars.FruAsim1[i,1]==1)   
  { compMbParsvsB1ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBasePars[i,7]==1&estadosB1Pars.HemAsim1[i,1]==1)   { compMbParsvsB1ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,10]==1&estadosB1Pars.InsAsim1[i,1]==1)   
  { compMbParsvsB1ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,13]==1&estadosB1Pars.NecAsim1[i,1]==1)   
  { compMbParsvsB1ParsAsim1[i,5]=1
  }
}

## Mb vs B2 ----
compMbParsvsB2ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,2]==1&estadosB2Pars.CarAsim1[i,1]==1)   
  { compMbParsvsB2ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,5]==1&estadosB2Pars.FruAsim1[i,1]==1)  
  { compMbParsvsB2ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,8]==1&estadosB2Pars.HemAsim1[i,1]==1)   
  { compMbParsvsB2ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,11]==1&estadosB2Pars.InsAsim1[i,1]==1) 
  { compMbParsvsB2ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,14]==1&estadosB2Pars.NecAsim1[i,1]==1) 
  { compMbParsvsB2ParsAsim1[i,5]=1
  }
}


## Mb vs B3 ----
compMbParsvsB3ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,3]==1&estadosB3Pars.CarAsim1[i,1]==1)  
  { compMbParsvsB3ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,6]==1&estadosB3Pars.FruAsim1[i,1]==1)   
  { compMbParsvsB3ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,9]==1&estadosB3Pars.HemAsim1[i,1]==1)   
  { compMbParsvsB3ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,12]==1&estadosB3Pars.InsAsim1[i,1]==1)   
  { compMbParsvsB3ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionBaseParsAsim1[i,15]==1&estadosB3Pars.NecAsim1[i,1]==1)   
  { compMbParsvsB3ParsAsim1[i,5]=1
  }
}



## Multi vs B1 ----
compMultiParsvsB1ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,1]==1&estadosB1Pars.CarAsim1[i,1]==1)   
  { compMultiParsvsB1ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,2]==1&estadosB1Pars.FruAsim1[i,1]==1)   
  { compMultiParsvsB1ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,3]==1&estadosB1Pars.HemAsim1[i,1]==1)   
    { compMultiParsvsB1ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,4]==1&estadosB1Pars.InsAsim1[i,1]==1)  
  { compMultiParsvsB1ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,5]==1&estadosB1Pars.NecAsim1[i,1]==1)  
  { compMultiParsvsB1ParsAsim1[i,5]=1
  }
}



## Multi vs B2 ----
compMultiParsvsB2ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,1]==1&estadosB2Pars.CarAsim1[i,1]==1)  
  { compMultiParsvsB2ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,2]==1&estadosB2Pars.FruAsim1[i,1]==1)   
  { compMultiParsvsB2ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,3]==1&estadosB2Pars.HemAsim1[i,1]==1)  
  { compMultiParsvsB2ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,4]==1&estadosB2Pars.InsAsim1[i,1]==1) 
  { compMultiParsvsB2ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,5]==1&estadosB2Pars.NecAsim1[i,1]==1)   
  { compMultiParsvsB2ParsAsim1[i,5]=1
  }
}



## Multi vs B3 ----
compMultiParsvsB3ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,1]==1&estadosB3Pars.CarAsim1[i,1]==1)  
  { compMultiParsvsB3ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,2]==1&estadosB3Pars.FruAsim1[i,1]==1)  
  { compMultiParsvsB3ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,3]==1&estadosB3Pars.HemAsim1[i,1]==1)  
  { compMultiParsvsB3ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,4]==1&estadosB3Pars.InsAsim1[i,1]==1)  
  { compMultiParsvsB3ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(binarizacionMultiParsAsim1[i,5]==1&estadosB3Pars.NecAsim1[i,1]==1)  
  { compMultiParsvsB3ParsAsim1[i,5]=1
  }
}



## B1 vs B2 ----
compB1ParsvsB2ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.CarAsim1[i,1]==1&estadosB2Pars.CarAsim1[i,1]==1)  
  { compB1ParsvsB2ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.FruAsim1[i,1]==1&estadosB2Pars.FruAsim1[i,1]==1)   
  { compB1ParsvsB2ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.HemAsim1[i,1]==1&estadosB2Pars.HemAsim1[i,1]==1)  
  { compB1ParsvsB2ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.InsAsim1[i,1]==1&estadosB2Pars.InsAsim1[i,1]==1)   
  { compB1ParsvsB2ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.NecAsim1[i,1]==1&estadosB2Pars.NecAsim1[i,1]==1)   { compB1ParsvsB2ParsAsim1[i,5]=1
  }
}



## B1 vs B3 ----
compB1ParsvsB3ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.CarAsim1[i,1]==1&estadosB3Pars.CarAsim1[i,1]==1)   
  { compB1ParsvsB3ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.FruAsim1[i,1]==1&estadosB3Pars.FruAsim1[i,1]==1)  
  { compB1ParsvsB3ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.HemAsim1[i,1]==1&estadosB3Pars.HemAsim1[i,1]==1)  
  { compB1ParsvsB3ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.InsAsim1[i,1]==1&estadosB3Pars.InsAsim1[i,1]==1)  
  { compB1ParsvsB3ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB1Pars.NecAsim1[i,1]==1&estadosB3Pars.NecAsim1[i,1]==1) 
  { compB1ParsvsB3ParsAsim1[i,5]=1
  }
}





## B2 vs B3 ----
compB2ParsvsB3ParsAsim1 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim1$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB2Pars.CarAsim1[i,1]==1&estadosB3Pars.CarAsim1[i,1]==1)  
  { compB2ParsvsB3ParsAsim1[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB2Pars.FruAsim1[i,1]==1&estadosB3Pars.FruAsim1[i,1]==1)  
  { compB2ParsvsB3ParsAsim1[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB2Pars.HemAsim1[i,1]==1&estadosB3Pars.HemAsim1[i,1]==1)  
  { compB2ParsvsB3ParsAsim1[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB2Pars.InsAsim1[i,1]==1&estadosB3Pars.InsAsim1[i,1]==1) 
  { compB2ParsvsB3ParsAsim1[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim1$Car)) {
  if(estadosB2Pars.NecAsim1[i,1]==1&estadosB3Pars.NecAsim1[i,1]==1)   
  { compB2ParsvsB3ParsAsim1[i,5]=1
  }
}

#----


###########################################################         COMPARACIONES TRANSFORMACIÓN ASIMÉTRICA 2


# Máxima verosimilitud
## B1 vs B2 ----
compB1vsB2Asim2 <- data.frame("B1Car-B2Car"=rep(0,length(mp1$Car)),"B1Fru-B2Fru"=0,"B1Hem-B2Hem"=0,"B1Ins-B2Ins"=0,"B1Nec-B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,1]==1&mp2Asim2[i,1]==1) {
    compB1vsB2Asim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,2]==1&mp2Asim2[i,2]==1) {
    compB1vsB2Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,3]==1&mp2Asim2[i,3]==1) {
    compB1vsB2Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,4]==1&mp2Asim2[i,4]==1) {
    compB1vsB2Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,5]==1&mp2Asim2[i,5]==1) {
    compB1vsB2Asim2[i,5]=1
  }
}

## B2 vs B3 ----
compB2vsB3Asim2 <- data.frame("B2Car-B3Car"=rep(0,length(mp1$Car)),"B2Fru-B3Fru"=0,"B2Hem-B3Hem"=0,"B2Ins-B3Ins"=0,"B2Nec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim2[i,1]==1&mp3Asim2[i,1]==1) {
    compB2vsB3Asim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim2[i,2]==1&mp3Asim2[i,2]==1) {
    compB2vsB3Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp2Asim2[i,3]==1&mp3Asim2[i,3]==1) {
    compB2vsB3Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim2[i,4]==1&mp3Asim2[i,4]==1) {
    compB2vsB3Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp2Asim2[i,5]==1&mp3Asim2[i,5]==1) {
    compB2vsB3Asim2[i,5]=1
  }
}


## B1 vs B3 ----
compB1vsB3Asim2 <- data.frame("B1Car-B2Car"=rep(0,length(mp1$Car)),"B1Fru-B2Fru"=0,"B1Hem-B2Hem"=0,"B1Ins-B2Ins"=0,"B1Nec-B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,1]==1&mp3Asim2[i,1]==1) {
    compB1vsB3Asim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,2]==1&mp3Asim2[i,2]==1) {
    compB1vsB3Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,3]==1&mp3Asim2[i,3]==1) {
    compB1vsB3Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,4]==1&mp3Asim2[i,4]==1) {
    compB1vsB3Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mp1Asim2[i,5]==1&mp3Asim2[i,5]==1) {
    compB1vsB3Asim2[i,5]=1
  }
}

## Multi vs B1 ####
compMultivsB1Asim2 <- data.frame("MultiCar-B1Car"=rep(0,length(mp1$Car)),"MultiFru-B1Fru"=0,"MultiHem-B1Hem"=0,"MultiIns-B1Ins"=0,"MultiNec-B1Nec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim2[i,7]==1&mp1Asim2[i,1]==1) {
    compMultivsB1Asim2[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim2[i,8]==1&mp1Asim2[i,2]==1) {
    compMultivsB1Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim2[i,9]==1&mp1Asim2[i,3]==1) {
    compMultivsB1Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim2[i,10]==1&mp1Asim2[i,4]==1) {
    compMultivsB1Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (mpMultiAsim2[i,11]==1&mp1Asim2[i,5]==1) {
    compMultivsB1Asim2[i,5]=1
  }
}


## Multi vs B2 ####
compMultivsB2Asim2 <- data.frame("MultiCar-B2Car"=rep(0,length(mp2$Car)),"MultiFru-B2Fru"=0,"MultiHem-B2Hem"=0,"MultiIns-B2Ins"=0,"MultiNec-B2Nec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp2$Car)) {
  if (mpMultiAsim2[i,7]==1&mp2Asim2[i,1]==1) {
    compMultivsB2Asim2[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp2$Car)) {
  if (mpMultiAsim2[i,8]==1&mp2Asim2[i,2]==1) {
    compMultivsB2Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp2$Car)) {
  if (mpMultiAsim2[i,9]==1&mp2Asim2[i,3]==1) {
    compMultivsB2Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp2$Car)) {
  if (mpMultiAsim2[i,10]==1&mp2Asim2[i,4]==1) {
    compMultivsB2Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp2$Car)) {
  if (mpMultiAsim2[i,11]==1&mp2Asim2[i,5]==1) {
    compMultivsB2Asim2[i,5]=1
  }
}



## Multi vs B3 ####
compMultivsB3Asim2 <- data.frame("MultiCar-B3Car"=rep(0,length(mp3$Car)),"MultiFru-B3Fru"=0,"MultiHem-B3Hem"=0,"MultiIns-B3Ins"=0,"MultiNec-B3Nec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp3$Car)) {
  if (mpMultiAsim2[i,7]==1&mp3Asim2[i,1]==1) {
    compMultivsB3Asim2[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp3$Car)) {
  if (mpMultiAsim2[i,8]==1&mp3Asim2[i,2]==1) {
    compMultivsB3Asim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp3$Car)) {
  if (mpMultiAsim2[i,9]==1&mp3Asim2[i,3]==1) {
    compMultivsB3Asim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp3$Car)) {
  if (mpMultiAsim2[i,10]==1&mp3Asim2[i,4]==1) {
    compMultivsB3Asim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp3$Car)) {
  if (mpMultiAsim2[i,11]==1&mp3Asim2[i,5]==1) {
    compMultivsB3Asim2[i,5]=1
  }
}


## Mb vs B1 ####
compMbvsB1Asim2 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim2[i,1]==1&mpBaseBinAsim2[i,1]==1) {
    compMbvsB1Asim2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim2[i,2]==1&mpBaseBinAsim2[i,4]==1) {
    compMbvsB1Asim2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim2[i,3]==1&mpBaseBinAsim2[i,7]==1) {
    compMbvsB1Asim2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim2[i,4]==1&mpBaseBinAsim2[i,10]==1) {
    compMbvsB1Asim2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp1Asim2[i,5]==1&mpBaseBinAsim2[i,13]==1) {
    compMbvsB1Asim2[i,5]=1
  }
}

## Mb vs B2 ####
compMbvsB2Asim2 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim2[i,1]==1&mpBaseBinAsim2[i,2]==1) {
    compMbvsB2Asim2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim2[i,2]==1&mpBaseBinAsim2[i,5]==1) {
    compMbvsB2Asim2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim2[i,3]==1&mpBaseBinAsim2[i,8]==1) {
    compMbvsB2Asim2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim2[i,4]==1&mpBaseBinAsim2[i,11]==1) {
    compMbvsB2Asim2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp2Asim2[i,5]==1&mpBaseBinAsim2[i,14]==1) {
    compMbvsB2Asim2[i,5]=1
  }
}

## Mb vs B3 ####
compMbvsB3Asim2 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim2[i,1]==1&mpBaseBinAsim2[i,3]==1) {
    compMbvsB3Asim2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim2[i,2]==1&mpBaseBinAsim2[i,6]==1) {
    compMbvsB3Asim2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim2[i,3]==1&mpBaseBinAsim2[i,9]==1) {
    compMbvsB3Asim2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim2[i,4]==1&mpBaseBinAsim2[i,12]==1) {
    compMbvsB3Asim2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mp3Asim2[i,5]==1&mpBaseBinAsim2[i,15]==1) {
    compMbvsB3Asim2[i,5]=1
  }
}

## Mb vs Multi ####
compMbvsMultiAsim2 <- data.frame("Car"=rep(0,length(mpBase$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim2[i,7]==1&mpBaseBinAsim2[i,1]==1) {
    compMbvsMultiAsim2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim2[i,8]==1&mpBaseBinAsim2[i,4]==1) {
    compMbvsMultiAsim2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim2[i,9]==1&mpBaseBinAsim2[i,7]==1) {
    compMbvsMultiAsim2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim2[i,10]==1&mpBaseBinAsim2[i,10]==1) {
    compMbvsMultiAsim2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (mpMultiAsim2[i,11]==1&mpBaseBinAsim2[i,13]==1) {
    compMbvsMultiAsim2[i,5]=1
  }
}


#----



# Parsimonia

## Mb vs Multi ----
compMbParsvsMultiParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,1]==1&binarizacionMultiParsAsim2[i,1]==1)   { compMbParsvsMultiParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,4]==1&binarizacionMultiParsAsim2[i,2]==1)   { compMbParsvsMultiParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,7]==1&binarizacionMultiParsAsim2[i,3]==1)   { compMbParsvsMultiParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,10]==1&binarizacionMultiParsAsim2[i,4]==1)   { compMbParsvsMultiParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,13]==1&binarizacionMultiParsAsim2[i,5]==1)   { compMbParsvsMultiParsAsim2[i,5]=1
  }
}

## Mb vs B1  ----
compMbParsvsB1ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,1]==1&estadosB1Pars.CarAsim2[i,1]==1)   
  { compMbParsvsB1ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,4]==1&estadosB1Pars.FruAsim2[i,1]==1)   
  { compMbParsvsB1ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,7]==1&estadosB1Pars.HemAsim2[i,1]==1)   { compMbParsvsB1ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,10]==1&estadosB1Pars.InsAsim2[i,1]==1)   
  { compMbParsvsB1ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,13]==1&estadosB1Pars.NecAsim2[i,1]==1)   
  { compMbParsvsB1ParsAsim2[i,5]=1
  }
}

## Mb vs B2 ----
compMbParsvsB2ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,2]==1&estadosB2Pars.CarAsim2[i,1]==1)   
  { compMbParsvsB2ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,5]==1&estadosB2Pars.FruAsim2[i,1]==1)  
  { compMbParsvsB2ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,8]==1&estadosB2Pars.HemAsim2[i,1]==1)   
  { compMbParsvsB2ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,11]==1&estadosB2Pars.InsAsim2[i,1]==1) 
  { compMbParsvsB2ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,14]==1&estadosB2Pars.NecAsim2[i,1]==1) 
  { compMbParsvsB2ParsAsim2[i,5]=1
  }
}


## Mb vs B3 ----
compMbParsvsB3ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,3]==1&estadosB3Pars.CarAsim2[i,1]==1)  
  { compMbParsvsB3ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,6]==1&estadosB3Pars.FruAsim2[i,1]==1)   
  { compMbParsvsB3ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,9]==1&estadosB3Pars.HemAsim2[i,1]==1)   
  { compMbParsvsB3ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,12]==1&estadosB3Pars.InsAsim2[i,1]==1)   
  { compMbParsvsB3ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionBaseParsAsim2[i,15]==1&estadosB3Pars.NecAsim2[i,1]==1)   
  { compMbParsvsB3ParsAsim2[i,5]=1
  }
}



## Multi vs B1 ----
compMultiParsvsB1ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,1]==1&estadosB1Pars.CarAsim2[i,1]==1)   
  { compMultiParsvsB1ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,2]==1&estadosB1Pars.FruAsim2[i,1]==1)   
  { compMultiParsvsB1ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,3]==1&estadosB1Pars.HemAsim2[i,1]==1)   
    { compMultiParsvsB1ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,4]==1&estadosB1Pars.InsAsim2[i,1]==1)  
  { compMultiParsvsB1ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,5]==1&estadosB1Pars.NecAsim2[i,1]==1)  
  { compMultiParsvsB1ParsAsim2[i,5]=1
  }
}



## Multi vs B2 ----
compMultiParsvsB2ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,1]==1&estadosB2Pars.CarAsim2[i,1]==1)  
  { compMultiParsvsB2ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,2]==1&estadosB2Pars.FruAsim2[i,1]==1)   
  { compMultiParsvsB2ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,3]==1&estadosB2Pars.HemAsim2[i,1]==1)  
  { compMultiParsvsB2ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,4]==1&estadosB2Pars.InsAsim2[i,1]==1) 
  { compMultiParsvsB2ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,5]==1&estadosB2Pars.NecAsim2[i,1]==1)   
  { compMultiParsvsB2ParsAsim2[i,5]=1
  }
}



## Multi vs B3 ----
compMultiParsvsB3ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1)  
  { compMultiParsvsB3ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,2]==1&estadosB3Pars.FruAsim2[i,1]==1)  
  { compMultiParsvsB3ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,3]==1&estadosB3Pars.HemAsim2[i,1]==1)  
  { compMultiParsvsB3ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,4]==1&estadosB3Pars.InsAsim2[i,1]==1)  
  { compMultiParsvsB3ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(binarizacionMultiParsAsim2[i,5]==1&estadosB3Pars.NecAsim2[i,1]==1)  
  { compMultiParsvsB3ParsAsim2[i,5]=1
  }
}



## B1 vs B2 ----
compB1ParsvsB2ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.CarAsim2[i,1]==1&estadosB2Pars.CarAsim2[i,1]==1)  
  { compB1ParsvsB2ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.FruAsim2[i,1]==1&estadosB2Pars.FruAsim2[i,1]==1)   
  { compB1ParsvsB2ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.HemAsim2[i,1]==1&estadosB2Pars.HemAsim2[i,1]==1)  
  { compB1ParsvsB2ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.InsAsim2[i,1]==1&estadosB2Pars.InsAsim2[i,1]==1)   
  { compB1ParsvsB2ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.NecAsim2[i,1]==1&estadosB2Pars.NecAsim2[i,1]==1)   { compB1ParsvsB2ParsAsim2[i,5]=1
  }
}



## B1 vs B3 ----
compB1ParsvsB3ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.CarAsim2[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1)   
  { compB1ParsvsB3ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.FruAsim2[i,1]==1&estadosB3Pars.FruAsim2[i,1]==1)  
  { compB1ParsvsB3ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.HemAsim2[i,1]==1&estadosB3Pars.HemAsim2[i,1]==1)  
  { compB1ParsvsB3ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.InsAsim2[i,1]==1&estadosB3Pars.InsAsim2[i,1]==1)  
  { compB1ParsvsB3ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB1Pars.NecAsim2[i,1]==1&estadosB3Pars.NecAsim2[i,1]==1) 
  { compB1ParsvsB3ParsAsim2[i,5]=1
  }
}





## B2 vs B3 ----
compB2ParsvsB3ParsAsim2 <- data.frame("Carnivoria"=rep(0,length(mpBaseAsim2$Car)),"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

### Carni
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB2Pars.CarAsim2[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1)  
  { compB2ParsvsB3ParsAsim2[i,1]=1
  }
}

### Frugi
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB2Pars.FruAsim2[i,1]==1&estadosB3Pars.FruAsim2[i,1]==1)  
  { compB2ParsvsB3ParsAsim2[i,2]=1
  }
}

### Hema
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB2Pars.HemAsim2[i,1]==1&estadosB3Pars.HemAsim2[i,1]==1)  
  { compB2ParsvsB3ParsAsim2[i,3]=1
  }
}

### Inse
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB2Pars.InsAsim2[i,1]==1&estadosB3Pars.InsAsim2[i,1]==1) 
  { compB2ParsvsB3ParsAsim2[i,4]=1
  }
}

### Nect
for (i in 1:length(mpBaseAsim2$Car)) {
  if(estadosB2Pars.NecAsim2[i,1]==1&estadosB3Pars.NecAsim2[i,1]==1)   
  { compB2ParsvsB3ParsAsim2[i,5]=1
  }
}






