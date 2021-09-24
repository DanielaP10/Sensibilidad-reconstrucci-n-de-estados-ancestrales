#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")
#source("reconstrucciones_Mk_DR.Tesis.R")
#source("reconstrucciones_Parsimonia.Tesis.R")
#source("sensibilidad-0.Discretizaciones y tales.Tesis.R")

# Comparaciones de las reconstrucciones dados los métodos

###############################################################    TRANSFOMRACIÓN SIMÉTRICA

# Máxima verosimilitud vs Parsimonia ----

## Multi ----
compMkvsParsMultiER <- data.frame("Carnivoria"=rep(0,length(mpMulti$Estado)),"Frugivoria"=0,"Hematofagia"=0, "Insectivoria"=0,"Nectarivoria"=0)

#### Carnivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpMulti[i,7]==1&binarizacionMultiPars[i,1]==1) {
    compMkvsParsMultiER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpMulti[i,8]==1&binarizacionMultiPars[i,2]==1) {
    compMkvsParsMultiER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpMulti$Car)) {
  if (mpMulti[i,9]==1&binarizacionMultiPars[i,3]==1) {
    compMkvsParsMultiER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpMulti[i,10]==1&binarizacionMultiPars[i,4]==1) {
    compMkvsParsMultiER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpMulti[i,11]==1&binarizacionMultiPars[i,5]==1) {
    compMkvsParsMultiER[i,5]=1
  }
}

## Mb ----
compMkvsParsMbER <- data.frame("Carnivoria"=rep(0,length(mpMulti$Estado)),"Frugivoria"=0,"Hematofagia"=0, "Insectivoria"=0,"Nectarivoria"=0)

#### Carnivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpBase[i,1]==1&estadosMbPars.Car[i,1]==1) {
    compMkvsParsMbER[i,1]=1 }
  else if (mpBase[i,1]==2&estadosMbPars.Car[i,1]==2) {
    compMkvsParsMbER[i,1]=1
  }
  else if (mpBase[i,1]==3&estadosMbPars.Car[i,1]==3) {
    compMkvsParsMbER[i,1]=1
  }
  
}

#### Frugivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpBase[i,2]==1&estadosMbPars.Fru[i,1]==1) {
    compMkvsParsMbER[i,2]=1 }  
  else if (mpBase[i,2]==2&estadosMbPars.Fru[i,1]==2) {
    compMkvsParsMbER[i,2]=1
  }
  else if (mpBase[i,2]==3&estadosMbPars.Fru[i,1]==3) {
    compMkvsParsMbER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpMulti$Car)) {
  if (mpBase[i,3]==1&estadosMbPars.Hem[i,1]==1) {
    compMkvsParsMbER[i,3]=1 }
  else if (mpBase[i,3]==2&estadosMbPars.Hem[i,1]==2) {
    compMkvsParsMbER[i,3]=1
  }
  else if (mpBase[i,3]==3&estadosMbPars.Hem[i,1]==3) {
    compMkvsParsMbER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpBase[i,4]==1&estadosMbPars.Ins[i,1]==1) {
    compMkvsParsMbER[i,4]=1 }
  else if (mpBase[i,4]==2&estadosMbPars.Ins[i,1]==2) {
    compMkvsParsMbER[i,4]=1
  }
  else if (mpBase[i,4]==3&estadosMbPars.Ins[i,1]==3) {
    compMkvsParsMbER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpMulti$Car)) {
  if (mpBase[i,5]==1&estadosMbPars.Nec[i,1]==1) {
    compMkvsParsMbER[i,5]=1 }
  else if (mpBase[i,5]==2&estadosMbPars.Nec[i,1]==2) {
    compMkvsParsMbER[i,5]=1
  }
  else if (mpBase[i,5]==3&estadosMbPars.Nec[i,1]==3) {
    compMkvsParsMbER[i,5]=1
  }
}


## B1 #####
compMkvsParsB1ER <- data.frame("Carnivoria"=rep(0,length(mpMulti$Estado)),"Frugivoria"=0,"Hematofagia"=0, "Insectivoria"=0,"Nectarivoria"=0)
#### Carnivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp1[i,1]==1&estadosB1Pars.Car[i,1]==1) {
    compMkvsParsB1ER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp1[i,2]==1&estadosB1Pars.Fru[i,1]==1) {
    compMkvsParsB1ER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpMulti$Car)) {
  if (mp1[i,3]==1&estadosB1Pars.Hem[i,1]==1) {
    compMkvsParsB1ER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp1[i,4]==1&estadosB1Pars.Ins[i,1]==1) {
    compMkvsParsB1ER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp1[i,5]==1&estadosB1Pars.Nec[i,1]==1) {
    compMkvsParsB1ER[i,5]=1
  }
}


## B2 #####
compMkvsParsB2ER <- data.frame("Carnivoria"=rep(0,length(mpMulti$Estado)),"Frugivoria"=0,"Hematofagia"=0, "Insectivoria"=0,"Nectarivoria"=0)

#### Carnivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp2[i,1]==1&estadosB2Pars.Car[i,1]==1) {
    compMkvsParsB2ER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp2[i,2]==1&estadosB2Pars.Fru[i,1]==1) {
    compMkvsParsB2ER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpMulti$Car)) {
  if (mp2[i,3]==1&estadosB2Pars.Hem[i,1]==1) {
    compMkvsParsB2ER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp2[i,4]==1&estadosB2Pars.Ins[i,1]==1) {
    compMkvsParsB2ER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp2[i,5]==1&estadosB2Pars.Nec[i,1]==1) {
    compMkvsParsB2ER[i,5]=1
  }
}





## B3 ####
compMkvsParsB3ER <- data.frame("Carnivoria"=rep(0,length(mpMulti$Estado)),"Frugivoria"=0,"Hematofagia"=0, "Insectivoria"=0,"Nectarivoria"=0)
#### Carnivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp3[i,1]==1&estadosB3Pars.Car[i,1]==1) {
    compMkvsParsB3ER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp3[i,2]==1&estadosB3Pars.Fru[i,1]==1) {
    compMkvsParsB3ER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpMulti$Car)) {
  if (mp3[i,3]==1&estadosB3Pars.Hem[i,1]==1) {
    compMkvsParsB3ER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp3[i,4]==1&estadosB3Pars.Ins[i,1]==1) {
    compMkvsParsB3ER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpMulti$Car)) {
  if (mp3[i,5]==1&estadosB3Pars.Nec[i,1]==1) {
    compMkvsParsB3ER[i,5]=1
  }
}


#----


###############################################################    TRANSFOMRACIÓN ASIMÉTRICA 1

# Máxima verosimilitud vs Parsimonia

# B1.DR1----
compParsVsMv.DR1.B1 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,1]==1&estadosB1Pars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.B1[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,2]==1&estadosB1Pars.FruAsim1[i,1]==1) {
    compParsVsMv.DR1.B1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,3]==1&estadosB1Pars.HemAsim1[i,1]==1) {
    compParsVsMv.DR1.B1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,4]==1&estadosB1Pars.InsAsim1[i,1]==1) {
    compParsVsMv.DR1.B1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,5]==1&estadosB1Pars.NecAsim1[i,1]==1) {
    compParsVsMv.DR1.B1[i,5]=1
  }
}


# B2.DR1----
compParsVsMv.DR1.B2 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,1]==1&estadosB2Pars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.B2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,2]==1&estadosB2Pars.FruAsim1[i,1]==1) {
    compParsVsMv.DR1.B2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,3]==1&estadosB2Pars.HemAsim1[i,1]==1) {
    compParsVsMv.DR1.B2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,4]==1&estadosB2Pars.InsAsim1[i,1]==1) {
    compParsVsMv.DR1.B2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,5]==1&estadosB2Pars.NecAsim1[i,1]==1) {
    compParsVsMv.DR1.B2[i,5]=1
  }
}


# B3.DR1----
compParsVsMv.DR1.B3 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,1]==1&estadosB3Pars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.B3[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,2]==1&estadosB3Pars.FruAsim1[i,1]==1) {
    compParsVsMv.DR1.B3[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,3]==1&estadosB3Pars.HemAsim1[i,1]==1) {
    compParsVsMv.DR1.B3[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,4]==1&estadosB3Pars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.B3[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,5]==1&estadosB3Pars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.B3[i,5]=1
  }
}


# Multi.DR1 ----
compParsVsMv.DR1.Multi <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,7]==1&binarizacionMultiParsAsim1[i,1]==1) {
    compParsVsMv.DR1.Multi[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,8]==1&binarizacionMultiParsAsim1[i,2]==1) {
    compParsVsMv.DR1.Multi[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,9]==1&binarizacionMultiParsAsim1[i,3]==1) {
    compParsVsMv.DR1.Multi[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,10]==1&binarizacionMultiParsAsim1[i,4]==1) {
    compParsVsMv.DR1.Multi[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,11]==1&binarizacionMultiParsAsim1[i,5]==1) {
    compParsVsMv.DR1.Multi[i,5]=1
  }
}


# MatrizBase.DR1 ----
compParsVsMv.DR1.Mb <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,1]==1&estadosMbPars.CarAsim1[i,1]==1) {
    compParsVsMv.DR1.Mb[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==2&estadosMbPars.CarAsim1[i,1]==2) {
    compParsVsMv.DR1.Mb[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==3&estadosMbPars.CarAsim1[i,1]==3) {
    compParsVsMv.DR1.Mb[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==1&estadosMbPars.FruAsim1[i,1]==1) {
    compParsVsMv.DR1.Mb[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==2&estadosMbPars.FruAsim1[i,1]==2) {
    compParsVsMv.DR1.Mb[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==3&estadosMbPars.FruAsim1[i,1]==3) {
    compParsVsMv.DR1.Mb[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,3]==1&estadosMbPars.HemAsim1[i,1]==1) {
    compParsVsMv.DR1.Mb[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==2&estadosMbPars.HemAsim1[i,1]==2) {
    compParsVsMv.DR1.Mb[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==3&estadosMbPars.HemAsim1[i,1]==3) {
    compParsVsMv.DR1.Mb[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==1&estadosMbPars.InsAsim1[i,1]==1) {
    compParsVsMv.DR1.Mb[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==2&estadosMbPars.InsAsim1[i,1]==2) {
    compParsVsMv.DR1.Mb[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==3&estadosMbPars.InsAsim1[i,1]==3) {
    compParsVsMv.DR1.Mb[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==1&estadosMbPars.NecAsim1[i,1]==1) {
    compParsVsMv.DR1.Mb[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==2&estadosMbPars.NecAsim1[i,1]==2) {
    compParsVsMv.DR1.Mb[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==3&estadosMbPars.NecAsim1[i,1]==3) {
    compParsVsMv.DR1.Mb[i,5]=1
  }
}

#----


###############################################################    TRANSFOMRACIÓN ASIMÉTRICA 2

# Máxima verosimilitud vs Parsimonia

## B1 ----
compParsVsMv.DR2.B1 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp1Asim2[i,1]==1&estadosB1Pars.CarAsim2[i,1]==1) {
    compParsVsMv.DR2.B1[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp1Asim2[i,2]==1&estadosB1Pars.FruAsim2[i,1]==1) {
    compParsVsMv.DR2.B1[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim2$Car)) {
  if (mp1Asim2[i,3]==1&estadosB1Pars.HemAsim2[i,1]==1) {
    compParsVsMv.DR2.B1[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp1Asim2[i,4]==1&estadosB1Pars.InsAsim2[i,1]==1) {
    compParsVsMv.DR2.B1[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp1Asim2[i,5]==1&estadosB1Pars.NecAsim2[i,1]==1) {
    compParsVsMv.DR2.B1[i,5]=1
  }
}


## B2 ----
compParsVsMv.DR2.B2 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp2Asim2[i,1]==1&estadosB2Pars.CarAsim2[i,1]==1) {
    compParsVsMv.DR2.B2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp2Asim2[i,2]==1&estadosB2Pars.FruAsim2[i,1]==1) {
    compParsVsMv.DR2.B2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim2$Car)) {
  if (mp2Asim2[i,3]==1&estadosB2Pars.HemAsim2[i,1]==1) {
    compParsVsMv.DR2.B2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp2Asim2[i,4]==1&estadosB2Pars.InsAsim2[i,1]==1) {
    compParsVsMv.DR2.B2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp2Asim2[i,5]==1&estadosB2Pars.NecAsim2[i,1]==1) {
    compParsVsMv.DR2.B2[i,5]=1
  }
}


## B3 ----
compParsVsMv.DR2.B3 <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp3Asim2[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1) {
    compParsVsMv.DR2.B3[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp3Asim2[i,2]==1&estadosB3Pars.FruAsim2[i,1]==1) {
    compParsVsMv.DR2.B3[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim2$Car)) {
  if (mp3Asim2[i,3]==1&estadosB3Pars.HemAsim2[i,1]==1) {
    compParsVsMv.DR2.B3[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp3Asim2[i,4]==1&estadosB3Pars.InsAsim2[i,1]==1) {
    compParsVsMv.DR2.B3[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mp3Asim2[i,5]==1&estadosB3Pars.NecAsim2[i,1]==1) {
    compParsVsMv.DR2.B3[i,5]=1
  }
}
## Multi ----
compParsVsMv.DR2.Multi <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0)

### Carnivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mpMultiAsim2[i,7]==1&binarizacionMultiParsAsim2[i,1]==1) {
    compParsVsMv.DR2.Multi[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mpMultiAsim2[i,8]==1&binarizacionMultiParsAsim2[i,2]==1) {
    compParsVsMv.DR2.Multi[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim2$Car)) {
  if (mpMultiAsim2[i,9]==1&binarizacionMultiParsAsim2[i,3]==1) {
    compParsVsMv.DR2.Multi[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mpMultiAsim2[i,10]==1&binarizacionMultiParsAsim2[i,4]==1) {
    compParsVsMv.DR2.Multi[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim2$Car)) {
  if (mpMultiAsim2[i,11]==1&binarizacionMultiParsAsim2[i,5]==1) {
    compParsVsMv.DR2.Multi[i,5]=1
  }
}

## MatrizBase.DR2 ----
compParsVsMv.DR2.Mb <- data.frame("Car"=rep(0,length(mp1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim2$Car)) {
  if (mpBaseAsim2[i,1]==1&estadosMbPars.CarAsim2[i,1]==1) {
    compParsVsMv.DR2.Mb[i,1]=1
  }
  else if (mpBaseAsim2[i,1]==2&estadosMbPars.CarAsim2[i,1]==2) {
    compParsVsMv.DR2.Mb[i,1]=1
  }
  else if (mpBaseAsim2[i,1]==3&estadosMbPars.CarAsim2[i,1]==3) {
    compParsVsMv.DR2.Mb[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim2$Car)) {
  if (mpBaseAsim2[i,2]==1&estadosMbPars.FruAsim2[i,1]==1) {
    compParsVsMv.DR2.Mb[i,2]=1
  }
  else if (mpBaseAsim2[i,2]==2&estadosMbPars.FruAsim2[i,1]==2) {
    compParsVsMv.DR2.Mb[i,2]=1
  }
  else if (mpBaseAsim2[i,2]==3&estadosMbPars.FruAsim2[i,1]==3) {
    compParsVsMv.DR2.Mb[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim2$Car)) {
  if (mpBaseAsim2[i,3]==1&estadosMbPars.HemAsim2[i,1]==1) {
    compParsVsMv.DR2.Mb[i,3]=1
  }
  else if (mpBaseAsim2[i,3]==2&estadosMbPars.HemAsim2[i,1]==2) {
    compParsVsMv.DR2.Mb[i,3]=1
  }
  else if (mpBaseAsim2[i,3]==3&estadosMbPars.HemAsim2[i,1]==3) {
    compParsVsMv.DR2.Mb[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim2$Car)) {
  if (mpBaseAsim2[i,4]==1&estadosMbPars.InsAsim2[i,1]==1) {
    compParsVsMv.DR2.Mb[i,4]=1
  }
  else if (mpBaseAsim2[i,4]==2&estadosMbPars.InsAsim2[i,1]==2) {
    compParsVsMv.DR2.Mb[i,4]=1
  }
  else if (mpBaseAsim2[i,4]==3&estadosMbPars.InsAsim2[i,1]==3) {
    compParsVsMv.DR2.Mb[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim2$Car)) {
  if (mpBaseAsim2[i,5]==1&estadosMbPars.NecAsim2[i,1]==1) {
    compParsVsMv.DR2.Mb[i,5]=1
  }
  else if (mpBaseAsim2[i,5]==2&estadosMbPars.NecAsim2[i,1]==2) {
    compParsVsMv.DR2.Mb[i,5]=1
  }
  else if (mpBaseAsim2[i,5]==3&estadosMbPars.NecAsim2[i,1]==3) {
    compParsVsMv.DR2.Mb[i,5]=1
  }
}
#----