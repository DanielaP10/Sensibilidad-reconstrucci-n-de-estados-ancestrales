#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")
#source("reconstrucciones_Mk_DR.Tesis.R")
#source("reconstrucciones_Parsimonia.Tesis.R")
#source("sensibilidad-0.Discretizaciones y tales.Tesis.R")


############################################################ COMPARACIONES ENTRE TRANSFORMACIONES

# Máxima verosimilitud

# Comparaciones entre transformaciones - DR1vsDR2 ----
## Matriz binaria 1 ----
compB1Asim1vsAsim2 <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,1]==1&mp1Asim2[i,1]==1) {
    compB1Asim1vsAsim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,2]==1&mp1Asim2[i,2]==1) {
    compB1Asim1vsAsim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,3]==1&mp1Asim2[i,3]==1) {
    compB1Asim1vsAsim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,4]==1&mp1Asim2[i,4]==1) {
    compB1Asim1vsAsim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,5]==1&mp1Asim2[i,5]==1) {
    compB1Asim1vsAsim2[i,5]=1
  }
}



## Matriz binaria 2 ----
compB2Asim1vsAsim2 <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,1]==1&mp2Asim2[i,1]==1) {
    compB2Asim1vsAsim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,2]==1&mp2Asim2[i,2]==1) {
    compB2Asim1vsAsim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,3]==1&mp2Asim2[i,3]==1) {
    compB2Asim1vsAsim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,4]==1&mp2Asim2[i,4]==1) {
    compB2Asim1vsAsim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,5]==1&mp2Asim2[i,5]==1) {
    compB2Asim1vsAsim2[i,5]=1
  }
}


## Matriz binaria 3 ----
compB3Asim1vsAsim2 <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,1]==1&mp3Asim2[i,1]==1) {
    compB3Asim1vsAsim2[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,2]==1&mp3Asim2[i,2]==1) {
    compB3Asim1vsAsim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,3]==1&mp3Asim2[i,3]==1) {
    compB3Asim1vsAsim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,4]==1&mp3Asim2[i,4]==1) {
    compB3Asim1vsAsim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,5]==1&mp3Asim2[i,5]==1) {
    compB3Asim1vsAsim2[i,5]=1
  }
}






## Matriz multiestado####
compMultiAsim1vsAsim2 <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,7]==1&mpMultiAsim2[i,7]==1) {
    compMultiAsim1vsAsim2[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,8]==1&mpMultiAsim2[i,8]==1) {
    compMultiAsim1vsAsim2[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,9]==1&mpMultiAsim2[i,9]==1) {
    compMultiAsim1vsAsim2[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,10]==1&mpMultiAsim2[i,10]==1) {
    compMultiAsim1vsAsim2[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,11]==1&mpMultiAsim2[i,11]==1) {
    compMultiAsim1vsAsim2[i,5]=1
  }
}


## Matriz base ####
compMbAsim1vsAsim2 <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,1]==1&mpBaseAsim2[i,1]==1) {
    compMbAsim1vsAsim2[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==2&mpBaseAsim2[i,1]==2) {
    compMbAsim1vsAsim2[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==3&mpBaseAsim2[i,1]==3) {
    compMbAsim1vsAsim2[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==1&mpBaseAsim2[i,2]==1) {
    compMbAsim1vsAsim2[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==2&mpBaseAsim2[i,2]==2) {
    compMbAsim1vsAsim2[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==3&mpBaseAsim2[i,2]==3) {
    compMbAsim1vsAsim2[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,3]==1&mpBaseAsim2[i,3]==1) {
    compMbAsim1vsAsim2[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==2&mpBaseAsim2[i,3]==2) {
    compMbAsim1vsAsim2[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==3&mpBaseAsim2[i,3]==3) {
    compMbAsim1vsAsim2[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==1&mpBaseAsim2[i,4]==1) {
    compMbAsim1vsAsim2[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==2&mpBaseAsim2[i,4]==2) {
    compMbAsim1vsAsim2[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==3&mpBaseAsim2[i,4]==3) {
    compMbAsim1vsAsim2[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==1&mpBaseAsim2[i,5]==1) {
    compMbAsim1vsAsim2[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==2&mpBaseAsim2[i,5]==2) {
    compMbAsim1vsAsim2[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==3&mpBaseAsim2[i,5]==3) {
    compMbAsim1vsAsim2[i,5]=1
  }
}

# Comparaciones entre transformaciones - DR1vsER----
## Matriz binaria 1 ----
compB1Asim1vsER <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,1]==1&mp1[i,1]==1) {
    compB1Asim1vsER[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,2]==1&mp1[i,2]==1) {
    compB1Asim1vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,3]==1&mp1[i,3]==1) {
    compB1Asim1vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,4]==1&mp1[i,4]==1) {
    compB1Asim1vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim1[i,5]==1&mp1[i,5]==1) {
    compB1Asim1vsER[i,5]=1
  }
}



## Matriz binaria 2 ----
compB2Asim1vsER <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,1]==1&mp2[i,1]==1) {
    compB2Asim1vsER[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,2]==1&mp2[i,2]==1) {
    compB2Asim1vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,3]==1&mp2[i,3]==1) {
    compB2Asim1vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,4]==1&mp2[i,4]==1) {
    compB2Asim1vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim1[i,5]==1&mp2[i,5]==1) {
    compB2Asim1vsER[i,5]=1
  }
}


## Matriz binaria 3 ----
compB3Asim1vsER <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,1]==1&mp3[i,1]==1) {
    compB3Asim1vsER[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,2]==1&mp3[i,2]==1) {
    compB3Asim1vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,3]==1&mp3[i,1]==1) {
    compB3Asim1vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,4]==1&mp3[i,4]==1) {
    compB3Asim1vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim1[i,5]==1&mp3[i,5]==1) {
    compB3Asim1vsER[i,5]=1
  }
}






## Matriz multiestado####
compMultiAsim1vsER <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,7]==1&mpMulti[i,7]==1) {
    compMultiAsim1vsER[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,8]==1&mpMulti[i,8]==1) {
    compMultiAsim1vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,9]==1&mpMulti[i,9]==1) {
    compMultiAsim1vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,10]==1&mpMulti[i,10]==1) {
    compMultiAsim1vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim1[i,11]==1&mpMulti[i,11]==1) {
    compMultiAsim1vsER[i,5]=1
  }
}


## Matriz base ####
compMbAsim1vsER <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,1]==1&mpBase[i,1]==1) {
    compMbAsim1vsER[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==2&mpBase[i,1]==2) {
    compMbAsim1vsER[i,1]=1
  }
  else if (mpBaseAsim1[i,1]==3&mpBase[i,1]==3) {
    compMbAsim1vsER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,2]==1&mpBase[i,2]==1) {
    compMbAsim1vsER[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==2&mpBase[i,2]==2) {
    compMbAsim1vsER[i,2]=1
  }
  else if (mpBaseAsim1[i,2]==3&mpBase[i,2]==3) {
    compMbAsim1vsER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,3]==1&mpBase[i,3]==1) {
    compMbAsim1vsER[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==2&mpBase[i,3]==2) {
    compMbAsim1vsER[i,3]=1
  }
  else if (mpBaseAsim1[i,3]==3&mpBase[i,3]==3) {
    compMbAsim1vsER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,4]==1&mpBase[i,4]==1) {
    compMbAsim1vsER[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==2&mpBase[i,4]==2) {
    compMbAsim1vsER[i,4]=1
  }
  else if (mpBaseAsim1[i,4]==3&mpBase[i,4]==3) {
    compMbAsim1vsER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim1[i,5]==1&mpBase[i,5]==1) {
    compMbAsim1vsER[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==2&mpBase[i,5]==2) {
    compMbAsim1vsER[i,5]=1
  }
  else if (mpBaseAsim1[i,5]==3&mpBase[i,5]==3) {
    compMbAsim1vsER[i,5]=1
  }
}

# Comparaciones entre transformaciones - DR2vsER ----
## Matriz binaria 1 ----
compB1Asim2vsER <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim2[i,1]==1&mp1[i,1]==1) {
    compB1Asim2vsER[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim2[i,2]==1&mp1[i,2]==1) {
    compB1Asim2vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim2[i,3]==1&mp1[i,3]==1) {
    compB1Asim2vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim2[i,4]==1&mp1[i,4]==1) {
    compB1Asim2vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp1Asim2[i,5]==1&mp1[i,5]==1) {
    compB1Asim2vsER[i,5]=1
  }
}



## Matriz binaria 2 ----
compB2Asim2vsER <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim2[i,1]==1&mp2[i,1]==1) {
    compB2Asim2vsER[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim2[i,2]==1&mp2[i,2]==1) {
    compB2Asim2vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim2[i,3]==1&mp2[i,3]==1) {
    compB2Asim2vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim2[i,4]==1&mp2[i,4]==1) {
    compB2Asim2vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp2Asim2[i,5]==1&mp2[i,5]==1) {
    compB2Asim2vsER[i,5]=1
  }
}


## Matriz binaria 3 ----
compB3Asim2vsER <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim2[i,1]==1&mp3[i,1]==1) {
    compB3Asim2vsER[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim2[i,2]==1&mp3[i,2]==1) {
    compB3Asim2vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim2[i,3]==1&mp3[i,3]==1) {
    compB3Asim2vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim2[i,4]==1&mp3[i,4]==1) {
    compB3Asim2vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mp3Asim2[i,5]==1&mp3[i,5]==1) {
    compB3Asim2vsER[i,5]=1
  }
}






## Matriz multiestado####
compMultiAsim2vsER <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim2[i,7]==1&mpMulti[i,7]==1) {
    compMultiAsim2vsER[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim2[i,8]==1&mpMulti[i,8]==1) {
    compMultiAsim2vsER[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim2[i,9]==1&mpMulti[i,9]==1) {
    compMultiAsim2vsER[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim2[i,10]==1&mpMulti[i,10]==1) {
    compMultiAsim2vsER[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (mpMultiAsim2[i,11]==1&mpMulti[i,11]==1) {
    compMultiAsim2vsER[i,5]=1
  }
}


## Matriz base ####
compMbAsim2vsER <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,1]==1&mpBase[i,1]==1) {
    compMbAsim2vsER[i,1]=1
  }
  else if (mpBaseAsim2[i,1]==2&mpBase[i,1]==2) {
    compMbAsim2vsER[i,1]=1
  }
  else if (mpBaseAsim2[i,1]==3&mpBase[i,1]==3) {
    compMbAsim2vsER[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,2]==1&mpBase[i,2]==1) {
    compMbAsim2vsER[i,2]=1
  }
  else if (mpBaseAsim2[i,2]==2&mpBase[i,2]==2) {
    compMbAsim2vsER[i,2]=1
  }
  else if (mpBaseAsim2[i,2]==3&mpBase[i,2]==3) {
    compMbAsim2vsER[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,3]==1&mpBase[i,3]==1) {
    compMbAsim2vsER[i,3]=1
  }
  else if (mpBaseAsim2[i,3]==2&mpBase[i,3]==2) {
    compMbAsim2vsER[i,3]=1
  }
  else if (mpBaseAsim2[i,3]==3&mpBase[i,3]==3) {
    compMbAsim2vsER[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,4]==1&mpBase[i,4]==1) {
    compMbAsim2vsER[i,4]=1
  }
  else if (mpBaseAsim2[i,4]==2&mpBase[i,4]==2) {
    compMbAsim2vsER[i,4]=1
  }
  else if (mpBaseAsim2[i,4]==3&mpBase[i,4]==3) {
    compMbAsim2vsER[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (mpBaseAsim2[i,5]==1&mpBase[i,5]==1) {
    compMbAsim2vsER[i,5]=1
  }
  else if (mpBaseAsim2[i,5]==2&mpBase[i,5]==2) {
    compMbAsim2vsER[i,5]=1
  }
  else if (mpBaseAsim2[i,5]==3&mpBase[i,5]==3) {
    compMbAsim2vsER[i,5]=1
  }
}









#----


# Parsimonia 

# Comparaciones entre transformaciones - DR1vsDR2----
# Matriz binaria 1 ----
compB1Asim1vsAsim2Pars <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.CarAsim1[i,1]==1&estadosB1Pars.CarAsim2[i,1]==1) {
    compB1Asim1vsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.FruAsim1[i,1]==1&estadosB1Pars.FruAsim2[i,1]==1) {
    compB1Asim1vsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.HemAsim1[i,1]==1&estadosB1Pars.HemAsim2[i,1]==1) {
    compB1Asim1vsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.InsAsim1[i,1]==1&estadosB1Pars.InsAsim2[i,1]==1) {
    compB1Asim1vsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.NecAsim1[i,1]==1&estadosB1Pars.NecAsim2[i,1]==1) {
    compB1Asim1vsAsim2Pars[i,5]=1
  }
}


# Matriz binaria 2----
compB2Asim1vsAsim2Pars <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.CarAsim1[i,1]==1&estadosB2Pars.CarAsim2[i,1]==1) {
    compB2Asim1vsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.FruAsim1[i,1]==1&estadosB2Pars.FruAsim2[i,1]==1) {
    compB2Asim1vsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.HemAsim1[i,1]==1&estadosB2Pars.HemAsim2[i,1]==1) {
    compB2Asim1vsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.InsAsim1[i,1]==1&estadosB2Pars.InsAsim2[i,1]==1) {
    compB2Asim1vsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.NecAsim1[i,1]==1&estadosB2Pars.NecAsim2[i,1]==1) {
    compB2Asim1vsAsim2Pars[i,5]=1
  }
}


# Matriz binaria 3 ----
compB3Asim1vsAsim2Pars <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.CarAsim1[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1) {
    compB3Asim1vsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.FruAsim1[i,1]==1&estadosB3Pars.FruAsim2[i,1]==1) {
    compB3Asim1vsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.HemAsim1[i,1]==1&estadosB3Pars.HemAsim2[i,1]==1) {
    compB3Asim1vsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.InsAsim1[i,1]==1&estadosB3Pars.InsAsim2[i,1]==1) {
    compB3Asim1vsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.NecAsim1[i,1]==1&estadosB3Pars.NecAsim2[i,1]==1) {
    compB3Asim1vsAsim2Pars[i,5]=1
  }
}


# Matriz multiestado ----
compMultiAsim1vsAsim2Pars <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,1]==1&binarizacionMultiParsAsim2[i,1]==1) {
    compMultiAsim1vsAsim2Pars[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,2]==1&binarizacionMultiParsAsim2[i,2]==1) {
    compMultiAsim1vsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,3]==1&binarizacionMultiParsAsim2[i,3]==1) {
    compMultiAsim1vsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,4]==1&binarizacionMultiParsAsim2[i,4]==1) {
    compMultiAsim1vsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,5]==1&binarizacionMultiParsAsim2[i,5]==1) {
    compMultiAsim1vsAsim2Pars[i,5]=1
  }
}


# Matriz base ----
compMbAsim1vsAsim2Pars <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]==1&estadosMbPars.CarAsim2[i,1]==1) {
    compMbAsim1vsAsim2Pars[i,1]=1
  }
  else if (estadosMbPars.CarAsim1[i,1]==2&estadosMbPars.CarAsim2[i,1]==2) {
    compMbAsim1vsAsim2Pars[i,1]=1
  }
  else if (estadosMbPars.CarAsim1[i,1]==3&estadosMbPars.CarAsim2[i,1]==3) {
    compMbAsim1vsAsim2Pars[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==1&estadosMbPars.FruAsim2[i,1]==1) {
    compMbAsim1vsAsim2Pars[i,2]=1
  }
  else if (estadosMbPars.FruAsim1[i,1]==2&estadosMbPars.FruAsim2[i,1]==2) {
    compMbAsim1vsAsim2Pars[i,2]=1
  }
  else if (estadosMbPars.FruAsim1[i,1]==3&estadosMbPars.FruAsim2[i,1]==3) {
    compMbAsim1vsAsim2Pars[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==1&estadosMbPars.HemAsim2[i,1]==1) {
    compMbAsim1vsAsim2Pars[i,3]=1
  }
  else if (estadosMbPars.HemAsim1[i,1]==2&estadosMbPars.HemAsim2[i,1]==2) {
    compMbAsim1vsAsim2Pars[i,3]=1
  }
  else if (estadosMbPars.HemAsim1[i,1]==3&estadosMbPars.HemAsim2[i,1]==3) {
    compMbAsim1vsAsim2Pars[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==1&estadosMbPars.InsAsim2[i,1]==1) {
    compMbAsim1vsAsim2Pars[i,4]=1
  }
  else if (estadosMbPars.InsAsim1[i,1]==2&estadosMbPars.InsAsim2[i,1]==2) {
    compMbAsim1vsAsim2Pars[i,4]=1
  }
  else if (estadosMbPars.InsAsim1[i,1]==3&estadosMbPars.InsAsim2[i,1]==3) {
    compMbAsim1vsAsim2Pars[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.NecAsim1[i,1]==1&estadosMbPars.NecAsim2[i,1]==1) {
    compMbAsim1vsAsim2Pars[i,5]=1
  }
  else if (estadosMbPars.NecAsim1[i,1]==2&estadosMbPars.NecAsim2[i,1]==2) {
    compMbAsim1vsAsim2Pars[i,5]=1
  }
  else if (estadosMbPars.NecAsim1[i,1]==3&estadosMbPars.NecAsim2[i,1]==3) {
    compMbAsim1vsAsim2Pars[i,5]=1
  }
}



# Comparaciones entre transformaciones - DR1vsER
# Comparaciones entre transformaciones - DR1vsER----
# Matriz binaria 1 ----
compB1Asim1vsERPars <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.CarAsim1[i,1]==1&estadosB1Pars.Car[i,1]==1) {
    compB1Asim1vsERPars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.FruAsim1[i,1]==1&estadosB1Pars.Fru[i,1]==1) {
    compB1Asim1vsERPars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.HemAsim1[i,1]==1&estadosB1Pars.Hem[i,1]==1) {
    compB1Asim1vsERPars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.InsAsim1[i,1]==1&estadosB1Pars.Ins[i,1]==1) {
    compB1Asim1vsERPars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB1Pars.NecAsim1[i,1]==1&estadosB1Pars.Nec[i,1]==1) {
    compB1Asim1vsERPars[i,5]=1
  }
}


# Matriz binaria 2 ----
compB2Asim1vsERPars <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.CarAsim1[i,1]==1&estadosB2Pars.Car[i,1]==1) {
    compB2Asim1vsERPars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.FruAsim1[i,1]==1&estadosB2Pars.Fru[i,1]==1) {
    compB2Asim1vsERPars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.HemAsim1[i,1]==1&estadosB2Pars.Hem[i,1]==1) {
    compB2Asim1vsERPars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.InsAsim1[i,1]==1&estadosB2Pars.Ins[i,1]==1) {
    compB2Asim1vsERPars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB2Pars.NecAsim1[i,1]==1&estadosB2Pars.Nec[i,1]==1) {
    compB2Asim1vsERPars[i,5]=1
  }
}


# Matriz binaria 3 ----
compB3Asim1vsERPars <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0) #Car-Fru-Hem-Ins-Nec

### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.CarAsim1[i,1]==1&estadosB3Pars.Car[i,1]==1) {
    compB3Asim1vsERPars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.FruAsim1[i,1]==1&estadosB3Pars.Fru[i,1]==1) {
    compB3Asim1vsERPars[i,2]=1
  }
  
}


### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.HemAsim1[i,1]==1&estadosB3Pars.Hem[i,1]==1) {
    compB3Asim1vsERPars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.InsAsim1[i,1]==1&estadosB3Pars.Ins[i,1]==1) {
    compB3Asim1vsERPars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (estadosB3Pars.NecAsim1[i,1]==1&estadosB3Pars.Nec[i,1]==1) {
    compB3Asim1vsERPars[i,5]=1
  }
}

# Matriz multiestado ----
compMultiAsim1vsERPars <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,1]==1&binarizacionMultiPars[i,1]==1) {
    compMultiAsim1vsERPars[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,2]==1&binarizacionMultiPars[i,2]==1) {
    compMultiAsim1vsERPars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,3]==1&binarizacionMultiPars[i,3]==1) {
    compMultiAsim1vsERPars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,4]==1&binarizacionMultiPars[i,4]==1) {
    compMultiAsim1vsERPars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiParsAsim1[i,5]==1&binarizacionMultiPars[i,5]==1) {
    compMultiAsim1vsERPars[i,5]=1
  }
}

# Matriz base ----
compMbAsim1vsERPars <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]==1&estadosMbPars.Car[i,1]==1) {
    compMbAsim1vsERPars[i,1]=1
  }
  else if (estadosMbPars.CarAsim1[i,1]==2&estadosMbPars.Car[i,1]==2) {
    compMbAsim1vsERPars[i,1]=1
  }
  else if (estadosMbPars.CarAsim1[i,1]==3&estadosMbPars.Car[i,1]==3) {
    compMbAsim1vsERPars[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==1&estadosMbPars.Fru[i,1]==1) {
    compMbAsim1vsERPars[i,2]=1
  }
  else if (estadosMbPars.FruAsim1[i,1]==2&estadosMbPars.Fru[i,1]==2) {
    compMbAsim1vsERPars[i,2]=1
  }
  else if (estadosMbPars.FruAsim1[i,1]==3&estadosMbPars.Fru[i,1]==3) {
    compMbAsim1vsERPars[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==1&estadosMbPars.Hem[i,1]==1) {
    compMbAsim1vsERPars[i,3]=1
  }
  else if (estadosMbPars.HemAsim1[i,1]==2&estadosMbPars.Hem[i,1]==2) {
    compMbAsim1vsERPars[i,3]=1
  }
  else if (estadosMbPars.HemAsim1[i,1]==3&estadosMbPars.Hem[i,1]==3) {
    compMbAsim1vsERPars[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==1&estadosMbPars.Ins[i,1]==1) {
    compMbAsim1vsERPars[i,4]=1
  }
  else if (estadosMbPars.InsAsim1[i,1]==2&estadosMbPars.Ins[i,1]==2) {
    compMbAsim1vsERPars[i,4]=1
  }
  else if (estadosMbPars.InsAsim1[i,1]==3&estadosMbPars.Ins[i,1]==3) {
    compMbAsim1vsERPars[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.NecAsim1[i,1]==1&estadosMbPars.Nec[i,1]==1) {
    compMbAsim1vsERPars[i,5]=1
  }
  else if (estadosMbPars.NecAsim1[i,1]==2&estadosMbPars.Nec[i,1]==2) {
    compMbAsim1vsERPars[i,5]=1
  }
  else if (estadosMbPars.NecAsim1[i,1]==3&estadosMbPars.Nec[i,1]==3) {
    compMbAsim1vsERPars[i,5]=1
  }
}

# Comparaciones entre transformaciones - DR2vsER----
# Matriz binaria 1 ----
compERB1vsAsim2Pars <- data.frame("B1Car"=rep(0,length(mp1Asim1$Car)),"B1Fru"=0,"B1Hem"=0,"B1Ins"=0,"B1Nec"=0)

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB1Pars.Car[i,1]==1&estadosB1Pars.CarAsim2[i,1]==1) {
    compERB1vsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB1Pars.Fru[i,1]==1&estadosB1Pars.FruAsim2[i,1]==1) {
    compERB1vsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (estadosB1Pars.Hem[i,1]==1&estadosB1Pars.HemAsim2[i,1]==1) {
    compERB1vsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB1Pars.Ins[i,1]==1&estadosB1Pars.InsAsim2[i,1]==1) {
    compERB1vsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB1Pars.Nec[i,1]==1&estadosB1Pars.NecAsim2[i,1]==1) {
    compERB1vsAsim2Pars[i,5]=1
  }
}


# Matriz binaria 2----
compB2ERvsAsim2Pars <- data.frame("B2Car"=rep(0,length(mp1Asim1$Car)),"B2Fru"=0,"B2Hem"=0,"B2Ins"=0,"B2Nec"=0)

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB2Pars.Car[i,1]==1&estadosB2Pars.CarAsim2[i,1]==1) {
    compB2ERvsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB2Pars.Fru[i,1]==1&estadosB2Pars.FruAsim2[i,1]==1) {
    compB2ERvsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (estadosB2Pars.Hem[i,1]==1&estadosB2Pars.HemAsim2[i,1]==1) {
    compB2ERvsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB2Pars.Ins[i,1]==1&estadosB2Pars.InsAsim2[i,1]==1) {
    compB2ERvsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB2Pars.Nec[i,1]==1&estadosB2Pars.NecAsim2[i,1]==1) {
    compB2ERvsAsim2Pars[i,5]=1
  }
}


# Matriz binaria 3 ----
compB3ERvsAsim2Pars <- data.frame("B3Car"=rep(0,length(mp1Asim1$Car)),"B3Fru"=0,"B3Hem"=0,"B3Ins"=0,"B3Nec"=0)

### Carnivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB3Pars.Car[i,1]==1&estadosB3Pars.CarAsim2[i,1]==1) {
    compB3ERvsAsim2Pars[i,1]=1
  }
  
}


### Frugivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB3Pars.Fru[i,1]==1&estadosB3Pars.FruAsim2[i,1]==1) {
    compB3ERvsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1$Car)) {
  if (estadosB3Pars.Hem[i,1]==1&estadosB3Pars.HemAsim2[i,1]==1) {
    compB3ERvsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB3Pars.Ins[i,1]==1&estadosB3Pars.InsAsim2[i,1]==1) {
    compB3ERvsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1$Car)) {
  if (estadosB3Pars.Nec[i,1]==1&estadosB3Pars.NecAsim2[i,1]==1) {
    compB3ERvsAsim2Pars[i,5]=1
  }
}


# Matriz multiestado ----
compMultiERvsAsim2Pars <- data.frame("MultiCar"=rep(0,length(mp1Asim1$Car)),"MultiFru"=0,"MultiHem"=0,"MultiIns"=0,"MultiNec"=0) #Car-Fru-Hem-Ins-Nec


### Carnivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiPars[i,1]==1&binarizacionMultiParsAsim2[i,1]==1) {
    compMultiERvsAsim2Pars[i,1]=1
  }
  
}

### Frugivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiPars[i,2]==1&binarizacionMultiParsAsim2[i,2]==1) {
    compMultiERvsAsim2Pars[i,2]=1
  }
  
}

### Hematofagia
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiPars[i,3]==1&binarizacionMultiParsAsim2[i,3]==1) {
    compMultiERvsAsim2Pars[i,3]=1
  }
  
}

### Insectivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiPars[i,4]==1&binarizacionMultiParsAsim2[i,4]==1) {
    compMultiERvsAsim2Pars[i,4]=1
  }
  
}

### Nectarivoria
for (i in 1:length(mp1Asim1$Car)) {
  if (binarizacionMultiPars[i,5]==1&binarizacionMultiParsAsim2[i,5]==1) {
    compMultiERvsAsim2Pars[i,5]=1
  }
}

# Matriz base ----
compMbERvsAsim2Pars <- data.frame("Car"=rep(0,length(mpBaseAsim1$Car)),"Fru"=0,"Hem"=0,"Ins"=0,"Nec"=0) 

#### Carnivoria
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]==1&estadosMbPars.CarAsim2[i,1]==1) {
    compMbERvsAsim2Pars[i,1]=1
  }
  else if (estadosMbPars.Car[i,1]==2&estadosMbPars.CarAsim2[i,1]==2) {
    compMbERvsAsim2Pars[i,1]=1
  }
  else if (estadosMbPars.Car[i,1]==3&estadosMbPars.CarAsim2[i,1]==3) {
    compMbERvsAsim2Pars[i,1]=1
  }
}

#### Frugivoria
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]==1&estadosMbPars.FruAsim2[i,1]==1) {
    compMbERvsAsim2Pars[i,2]=1
  }
  else if (estadosMbPars.Fru[i,1]==2&estadosMbPars.FruAsim2[i,1]==2) {
    compMbERvsAsim2Pars[i,2]=1
  }
  else if (estadosMbPars.Fru[i,1]==3&estadosMbPars.FruAsim2[i,1]==3) {
    compMbERvsAsim2Pars[i,2]=1
  }
}

#### Hematofagia
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]==1&estadosMbPars.HemAsim2[i,1]==1) {
    compMbERvsAsim2Pars[i,3]=1
  }
  else if (estadosMbPars.Hem[i,1]==2&estadosMbPars.HemAsim2[i,1]==2) {
    compMbERvsAsim2Pars[i,3]=1
  }
  else if (estadosMbPars.Hem[i,1]==3&estadosMbPars.HemAsim2[i,1]==3) {
    compMbERvsAsim2Pars[i,3]=1
  }
}

#### Insectivoria
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]==1&estadosMbPars.InsAsim2[i,1]==1) {
    compMbERvsAsim2Pars[i,4]=1
  }
  else if (estadosMbPars.Ins[i,1]==2&estadosMbPars.InsAsim2[i,1]==2) {
    compMbERvsAsim2Pars[i,4]=1
  }
  else if (estadosMbPars.Ins[i,1]==3&estadosMbPars.InsAsim2[i,1]==3) {
    compMbERvsAsim2Pars[i,4]=1
  }
}

#### Nectarivoria
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Nec[i,1]==1&estadosMbPars.NecAsim2[i,1]==1) {
    compMbERvsAsim2Pars[i,5]=1
  }
  else if (estadosMbPars.Nec[i,1]==2&estadosMbPars.NecAsim2[i,1]==2) {
    compMbERvsAsim2Pars[i,5]=1
  }
  else if (estadosMbPars.Nec[i,1]==3&estadosMbPars.NecAsim2[i,1]==3) {
    compMbERvsAsim2Pars[i,5]=1
  }
}




#----


