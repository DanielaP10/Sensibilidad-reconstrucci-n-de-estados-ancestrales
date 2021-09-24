#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")
#source("reconstrucciones_Mk_DR.Tesis.R")

############################################################
#          RECONSTRUCCIONES CON PARSIMONIA - ER

### Conversión de los estados como números enteros, ya que así es como los debe tomar la función de reconsturcción de estados ancestrales con parsimonia
multiPars  <- as.integer(matrizMultiestadoPars$Dieta)

### Estimación ancestral con parsimonia
rea.MultiEKPars<- asr_max_parsimony(tree = aceTree,tip_states = multiPars,transition_costs = "all_equal",Nstates = 5)

### Estados ancestrales dados los valores de probabilidad
resultados <- max.col(rea.MultiEKPars$ancestral_likelihoods)

estadosMultiPars <- replace(resultados,resultados==1,0)
estadosMultiPars <- replace(estadosMultiPars,resultados==2,1)
estadosMultiPars <- replace(estadosMultiPars,estadosMultiPars==3,2)
estadosMultiPars <- replace(estadosMultiPars,estadosMultiPars==4,3)
estadosMultiPars <- replace(estadosMultiPars,estadosMultiPars==5,4)
estadosMultiPars <- as.data.frame(estadosMultiPars)


### Conversión de los estados como números enteros
basePars.Car <- as.integer(matrizBasePars$Carnivoria)
basePars.Fru <- as.integer(matrizBasePars$Frugivoria)
basePars.Hem <- as.integer(matrizBasePars$Hematofagia)
basePars.Ins <- as.integer(matrizBasePars$Insectivoria)
basePars.Nec <- as.integer(matrizBasePars$Nectarivoria)


### Estimación ancestral con parsimonia
rea.BaseEKPars.Car<- asr_max_parsimony(aceTree, basePars.Car,transition_costs = "all_equal",Nstates =4)
rea.BaseEKPars.Fru<- asr_max_parsimony(aceTree, basePars.Fru,transition_costs = "all_equal",Nstates =4)
rea.BaseEKPars.Hem<- asr_max_parsimony(aceTree, basePars.Hem,transition_costs = "all_equal",Nstates =4)
rea.BaseEKPars.Ins<- asr_max_parsimony(aceTree, basePars.Ins,transition_costs = "all_equal",Nstates =4)
rea.BaseEKPars.Nec<- asr_max_parsimony(aceTree, basePars.Nec,transition_costs = "all_equal",Nstates =4)

#### Carnivoria
resultados.basePars.Car <- max.col(rea.BaseEKPars.Car$ancestral_likelihoods)

estadosMbPars.Car <- replace(resultados.basePars.Car ,resultados.basePars.Car==1,0)
estadosMbPars.Car <- replace(estadosMbPars.Car,estadosMbPars.Car==2,1)
estadosMbPars.Car <- replace(estadosMbPars.Car,estadosMbPars.Car==3,2)
estadosMbPars.Car <- replace(estadosMbPars.Car,estadosMbPars.Car==4,3)
estadosMbPars.Car <- as.data.frame(estadosMbPars.Car)

#### Frugivoria
resultados.basePars.Fru <- max.col(rea.BaseEKPars.Fru$ancestral_likelihoods)

estadosMbPars.Fru <- replace(resultados.basePars.Fru,resultados.basePars.Fru==1,0)
estadosMbPars.Fru <- replace(estadosMbPars.Fru,estadosMbPars.Fru==2,1)
estadosMbPars.Fru <- replace(estadosMbPars.Fru,estadosMbPars.Fru==3,2)
estadosMbPars.Fru <- replace(estadosMbPars.Fru,estadosMbPars.Fru==4,3)
estadosMbPars.Fru <- as.data.frame(estadosMbPars.Fru)

#### Hematofagia
resultados.basePars.Hem <- max.col(rea.BaseEKPars.Hem$ancestral_likelihoods)

estadosMbPars.Hem <- replace(resultados.basePars.Hem,resultados.basePars.Hem==1,0)
estadosMbPars.Hem <- replace(estadosMbPars.Hem,estadosMbPars.Hem==2,1)
estadosMbPars.Hem <- replace(estadosMbPars.Hem,estadosMbPars.Hem==3,2)
estadosMbPars.Hem <- replace(estadosMbPars.Hem,estadosMbPars.Hem==4,3)
estadosMbPars.Hem <- as.data.frame(estadosMbPars.Hem)

#### Insectivoria
resultados.basePars.Ins <- max.col(rea.BaseEKPars.Ins$ancestral_likelihoods)

estadosMbPars.Ins <- replace(resultados.basePars.Ins,resultados.basePars.Ins==1,0)
estadosMbPars.Ins <- replace(estadosMbPars.Ins,estadosMbPars.Ins==2,1)
estadosMbPars.Ins <- replace(estadosMbPars.Ins,estadosMbPars.Ins==3,2)
estadosMbPars.Ins <- replace(estadosMbPars.Ins,estadosMbPars.Ins==4,3)
estadosMbPars.Ins <- as.data.frame(estadosMbPars.Ins)

#### Nectarivoria
resultados.basePars.Nec <- max.col(rea.BaseEKPars.Nec$ancestral_likelihoods)

estadosMbPars.Nec <- replace(resultados.basePars.Nec,resultados.basePars.Nec==1,0)
estadosMbPars.Nec <- replace(estadosMbPars.Nec,estadosMbPars.Nec==2,1)
estadosMbPars.Nec <- replace(estadosMbPars.Nec,estadosMbPars.Nec==3,2)
estadosMbPars.Nec <- replace(estadosMbPars.Nec,estadosMbPars.Nec==4,3)
estadosMbPars.Nec <- as.data.frame(estadosMbPars.Nec)




# Matriz binaria 1----

## Conversión de los estados como números enteros
B1Pars.Car <- as.integer(matrizBinaria1Pars$Carnivoria)
B1Pars.Fru <- as.integer(matrizBinaria1Pars$Frugivoria)
B1Pars.Hem <- as.integer(matrizBinaria1Pars$Hematofagia)
B1Pars.Ins <- as.integer(matrizBinaria1Pars$Insectivoria)
B1Pars.Nec <- as.integer(matrizBinaria1Pars$Nectarivoria)

## Reconstrucción
rea.B1EKPars.Car<- asr_max_parsimony(aceTree, B1Pars.Car,transition_costs = "all_equal",Nstates =2)
rea.B1EKPars.Fru<- asr_max_parsimony(aceTree, B1Pars.Fru,transition_costs = "all_equal",Nstates =2)
rea.B1EKPars.Hem<- asr_max_parsimony(aceTree, B1Pars.Hem,transition_costs = "all_equal",Nstates =2)
rea.B1EKPars.Ins<- asr_max_parsimony(aceTree, B1Pars.Ins,transition_costs = "all_equal",Nstates =2)
rea.B1EKPars.Nec<- asr_max_parsimony(aceTree, B1Pars.Nec,transition_costs = "all_equal",Nstates =2)

### Carnivoria
resultados.B1Pars.Car <- max.col(rea.B1EKPars.Car $ancestral_likelihoods)

estadosB1Pars.Car <- replace(resultados.B1Pars.Car ,resultados.B1Pars.Car==1,0)
estadosB1Pars.Car <- replace(estadosB1Pars.Car,estadosB1Pars.Car==2,1)
estadosB1Pars.Car <- as.data.frame(estadosB1Pars.Car)


### Frugivoria
resultados.B1Pars.Fru <- max.col(rea.B1EKPars.Fru$ancestral_likelihoods)

estadosB1Pars.Fru <- replace(resultados.B1Pars.Fru ,resultados.B1Pars.Fru==1,0)
estadosB1Pars.Fru <- replace(estadosB1Pars.Fru,estadosB1Pars.Fru==2,1)
estadosB1Pars.Fru <- as.data.frame(estadosB1Pars.Fru)


### Hematofagia
resultados.B1Pars.Hem <- max.col(rea.B1EKPars.Hem$ancestral_likelihoods)

estadosB1Pars.Hem <- replace(resultados.B1Pars.Hem ,resultados.B1Pars.Hem==1,0)
estadosB1Pars.Hem <- replace(estadosB1Pars.Hem,estadosB1Pars.Hem==2,1)
estadosB1Pars.Hem <- as.data.frame(estadosB1Pars.Hem)


### Insectivoria
resultados.B1Pars.Ins <- max.col(rea.B1EKPars.Ins$ancestral_likelihoods)

estadosB1Pars.Ins <- replace(resultados.B1Pars.Ins ,resultados.B1Pars.Ins==1,0)
estadosB1Pars.Ins <- replace(estadosB1Pars.Ins,estadosB1Pars.Ins==2,1)
estadosB1Pars.Ins <- as.data.frame(estadosB1Pars.Ins)


### Nectarivoria
resultados.B1Pars.Nec <- max.col(rea.B1EKPars.Nec$ancestral_likelihoods)

estadosB1Pars.Nec <- replace(resultados.B1Pars.Nec ,resultados.B1Pars.Nec==1,0)
estadosB1Pars.Nec <- replace(estadosB1Pars.Nec,estadosB1Pars.Nec==2,1)
estadosB1Pars.Nec <- as.data.frame(estadosB1Pars.Nec)




### MATRIZ BINARIA 2----

## Conversión de los estados como números enteros
B2Pars.Car <- as.integer(matrizBinaria2Pars$Carnivoria)
B2Pars.Fru <- as.integer(matrizBinaria2Pars$Frugivoria)
B2Pars.Hem <- as.integer(matrizBinaria2Pars$Hematofagia)
B2Pars.Ins <- as.integer(matrizBinaria2Pars$Insectivoria)
B2Pars.Nec <- as.integer(matrizBinaria2Pars$Nectarivoria)

## Reconstrucción
rea.B2EKPars.Car<- asr_max_parsimony(aceTree, B2Pars.Car,transition_costs = "all_equal",Nstates =2)
rea.B2EKPars.Fru<- asr_max_parsimony(aceTree, B2Pars.Fru,transition_costs = "all_equal",Nstates =2)
rea.B2EKPars.Hem<- asr_max_parsimony(aceTree, B2Pars.Hem,transition_costs = "all_equal",Nstates =2)
rea.B2EKPars.Ins<- asr_max_parsimony(aceTree, B2Pars.Ins,transition_costs = "all_equal",Nstates =2)
rea.B2EKPars.Nec<- asr_max_parsimony(aceTree, B2Pars.Nec,transition_costs = "all_equal",Nstates =2)

### Estados ancestrales dados los valores de probabilidad que arroja la función

#### Carnivoria
resultados.B2Pars.Car <- max.col(rea.B2EKPars.Car$ancestral_likelihoods)

estadosB2Pars.Car <- replace(resultados.B2Pars.Car ,resultados.B2Pars.Car==1,0)
estadosB2Pars.Car <- replace(estadosB2Pars.Car,estadosB2Pars.Car==2,1)
estadosB2Pars.Car <- as.data.frame(estadosB2Pars.Car)



#### Frugivoria
resultados.B2Pars.Fru <- max.col(rea.B2EKPars.Fru$ancestral_likelihoods)

estadosB2Pars.Fru <- replace(resultados.B2Pars.Fru ,resultados.B2Pars.Fru==1,0)
estadosB2Pars.Fru <- replace(estadosB2Pars.Fru,estadosB2Pars.Fru==2,1)
estadosB2Pars.Fru <- as.data.frame(estadosB2Pars.Fru)


#### Hematofagia
resultados.B2Pars.Hem <- max.col(rea.B2EKPars.Hem$ancestral_likelihoods)

estadosB2Pars.Hem <- replace(resultados.B2Pars.Hem ,resultados.B2Pars.Hem==1,0)
estadosB2Pars.Hem <- replace(estadosB2Pars.Hem,estadosB2Pars.Hem==2,1)
estadosB2Pars.Hem <- as.data.frame(estadosB2Pars.Hem)


#### Insectivoria
resultados.B2Pars.Ins <- max.col(rea.B2EKPars.Ins$ancestral_likelihoods)

estadosB2Pars.Ins <- replace(resultados.B2Pars.Ins ,resultados.B2Pars.Ins==1,0)
estadosB2Pars.Ins <- replace(estadosB2Pars.Ins,estadosB2Pars.Ins==2,1)
estadosB2Pars.Ins <- as.data.frame(estadosB2Pars.Ins)


#### Nectarivoria
resultados.B2Pars.Nec <- max.col(rea.B2EKPars.Nec$ancestral_likelihoods)

estadosB2Pars.Nec <- replace(resultados.B2Pars.Nec ,resultados.B2Pars.Nec==1,0)
estadosB2Pars.Nec <- replace(estadosB2Pars.Nec,estadosB2Pars.Nec==2,1)
estadosB2Pars.Nec <- as.data.frame(estadosB2Pars.Nec)



## Matris binaria 3----

## Conversión de los estados como números enteros
B3Pars.Car <- as.integer(matrizBinaria3Pars$Carnivoria)
B3Pars.Fru <- as.integer(matrizBinaria3Pars$Frugivoria)
B3Pars.Hem <- as.integer(matrizBinaria3Pars$Hematofagia)
B3Pars.Ins <- as.integer(matrizBinaria3Pars$Insectivoria)
B3Pars.Nec <- as.integer(matrizBinaria3Pars$Nectarivoria)

## Reconstrucción
rea.B3EKPars.Car<- asr_max_parsimony(aceTree, B3Pars.Car,transition_costs = "all_equal",Nstates =2)
rea.B3EKPars.Fru<- asr_max_parsimony(aceTree, B3Pars.Fru,transition_costs = "all_equal",Nstates =2)
rea.B3EKPars.Hem<- asr_max_parsimony(aceTree, B3Pars.Hem,transition_costs = "all_equal",Nstates =2)
rea.B3EKPars.Ins<- asr_max_parsimony(aceTree, B3Pars.Ins,transition_costs = "all_equal",Nstates =2)
rea.B3EKPars.Nec<- asr_max_parsimony(aceTree, B3Pars.Nec,transition_costs = "all_equal",Nstates =2)

### Estados ancestrales dados los valores de probabilidad
#### Carnivoria
resultados.B3Pars.Car <- max.col(rea.B3EKPars.Car$ancestral_likelihoods)

estadosB3Pars.Car <- replace(resultados.B3Pars.Car ,resultados.B3Pars.Car==1,0)
estadosB3Pars.Car <- replace(estadosB3Pars.Car,estadosB3Pars.Car==2,1)
estadosB3Pars.Car <- as.data.frame(estadosB3Pars.Car)


#### Frugivoria
resultados.B3Pars.Fru <- max.col(rea.B3EKPars.Fru$ancestral_likelihoods)

estadosB3Pars.Fru <- replace(resultados.B3Pars.Fru ,resultados.B3Pars.Fru==1,0)
estadosB3Pars.Fru <- replace(estadosB3Pars.Fru,estadosB3Pars.Fru==2,1)
estadosB3Pars.Fru <- as.data.frame(estadosB3Pars.Fru)


#### Hematofagia
resultados.B3Pars.Hem <- max.col(rea.B3EKPars.Hem$ancestral_likelihoods)

estadosB3Pars.Hem <- replace(resultados.B3Pars.Hem ,resultados.B3Pars.Hem==1,0)
estadosB3Pars.Hem <- replace(estadosB3Pars.Hem,estadosB3Pars.Hem==2,1)
estadosB3Pars.Hem <- as.data.frame(estadosB3Pars.Hem)


#### Insectivoria
resultados.B3Pars.Ins <- max.col(rea.B3EKPars.Ins$ancestral_likelihoods)

estadosB3Pars.Ins <- replace(resultados.B3Pars.Ins ,resultados.B3Pars.Ins==1,0)
estadosB3Pars.Ins <- replace(estadosB3Pars.Ins,estadosB3Pars.Ins==2,1)
estadosB3Pars.Ins <- as.data.frame(estadosB3Pars.Ins)


#### Nectarivoria
resultados.B3Pars.Nec <- max.col(rea.B3EKPars.Nec$ancestral_likelihoods)

estadosB3Pars.Nec <- replace(resultados.B3Pars.Nec ,resultados.B3Pars.Nec==1,0)
estadosB3Pars.Nec <- replace(estadosB3Pars.Nec,estadosB3Pars.Nec==2,1)
estadosB3Pars.Nec <- as.data.frame(estadosB3Pars.Nec)


##########################################################
##                 TASAS DESIGUALES

# ASIMÉTRICA 1

## Multiestado ----
multAsim1 <- matrix(c(0,2.9,2.9,2.9,2.9,1,0,2.9,2.9,2.9,1,1,0,2.9,2.9,1,1,1,0,2.9,1,1,1,1,0),nrow = 5)

multAsim2 <- matrix(c(0,1,1,1,1,2.9,0,1,1,1,2.9,2.9,0,1,1,2.9,2.9,2.9,0,1,2.9,2.9,2.9,2.9,0),nrow = 5)

rea.MultiParsAsim1 <- asr_max_parsimony(tree = aceTree,tip_states = multiPars,transition_costs = multAsim1,Nstates = 5)

### Estados ancestrales dados los valores de probabilidad
resultadosAsim1 <- max.col(rea.MultiParsAsim1$ancestral_likelihoods)

estadosMultiParsAsim1 <- replace(resultadosAsim1,resultadosAsim1==1,0)
estadosMultiParsAsim1 <- replace(estadosMultiParsAsim1,estadosMultiParsAsim1==2,1)
estadosMultiParsAsim1 <- replace(estadosMultiParsAsim1,estadosMultiParsAsim1==3,2)
estadosMultiParsAsim1 <- replace(estadosMultiParsAsim1,estadosMultiParsAsim1==4,3)
estadosMultiParsAsim1 <- replace(estadosMultiParsAsim1,estadosMultiParsAsim1==5,4)
estadosMultiParsAsim1 <- as.data.frame(estadosMultiParsAsim1)


## Matrix base ----
baseAsim1 <- matrix(c(0,2.9,2.9,2.9,1,0,2.9,2.9,1,1,0,2.9,1,1,1,0),nrow = 4)

baseAsim2 <- matrix(c(0,1,1,1,2.9,0,1,1,2.9,2.9,0,1,2.9,2.9,2.9,0),nrow = 4)

rea.BaseEKPars.CarAsim1 <- asr_max_parsimony(aceTree, basePars.Car,transition_costs = baseAsim1,Nstates =4)
rea.BaseEKPars.FruAsim1 <- asr_max_parsimony(aceTree, basePars.Fru,transition_costs = baseAsim1,Nstates =4)
rea.BaseEKPars.HemAsim1 <- asr_max_parsimony(aceTree, basePars.Hem,transition_costs = baseAsim1,Nstates =4)
rea.BaseEKPars.InsAsim1 <- asr_max_parsimony(aceTree, basePars.Ins,transition_costs = baseAsim1,Nstates =4)
rea.BaseEKPars.NecAsim1 <- asr_max_parsimony(aceTree, basePars.Nec,transition_costs = baseAsim1,Nstates =4)

### Carnivoria
resultados.basePars.CarAsim1 <- max.col(rea.BaseEKPars.CarAsim1$ancestral_likelihoods)

estadosMbPars.CarAsim1 <- replace(resultados.basePars.CarAsim1,resultados.basePars.CarAsim1==1,0)
estadosMbPars.CarAsim1 <- replace(estadosMbPars.CarAsim1,estadosMbPars.CarAsim1==2,1)
estadosMbPars.CarAsim1 <- replace(estadosMbPars.CarAsim1,estadosMbPars.CarAsim1==3,2)
estadosMbPars.CarAsim1 <- replace(estadosMbPars.CarAsim1,estadosMbPars.CarAsim1==4,3)
estadosMbPars.CarAsim1 <- as.data.frame(estadosMbPars.CarAsim1)

### Frugivoria
resultados.basePars.FruAsim1 <- max.col(rea.BaseEKPars.FruAsim1$ancestral_likelihoods)

estadosMbPars.FruAsim1 <- replace(resultados.basePars.FruAsim1,resultados.basePars.FruAsim1==1,0)
estadosMbPars.FruAsim1 <- replace(estadosMbPars.FruAsim1,estadosMbPars.FruAsim1==2,1)
estadosMbPars.FruAsim1 <- replace(estadosMbPars.FruAsim1,estadosMbPars.FruAsim1==3,2)
estadosMbPars.FruAsim1 <- replace(estadosMbPars.FruAsim1,estadosMbPars.FruAsim1==4,3)
estadosMbPars.FruAsim1 <- as.data.frame(estadosMbPars.FruAsim1)

### Hematofagia
resultados.basePars.HemAsim1 <- max.col(rea.BaseEKPars.HemAsim1$ancestral_likelihoods)

estadosMbPars.HemAsim1 <- replace(resultados.basePars.HemAsim1,resultados.basePars.HemAsim1==1,0)
estadosMbPars.HemAsim1 <- replace(estadosMbPars.HemAsim1,estadosMbPars.HemAsim1==2,1)
estadosMbPars.HemAsim1 <- replace(estadosMbPars.HemAsim1,estadosMbPars.HemAsim1==3,2)
estadosMbPars.HemAsim1 <- replace(estadosMbPars.HemAsim1,estadosMbPars.HemAsim1==4,3)
estadosMbPars.HemAsim1 <- as.data.frame(estadosMbPars.HemAsim1)

### Insectivoria
resultados.basePars.InsAsim1 <- max.col(rea.BaseEKPars.InsAsim1$ancestral_likelihoods)

estadosMbPars.InsAsim1 <- replace(resultados.basePars.InsAsim1,resultados.basePars.InsAsim1==1,0)
estadosMbPars.InsAsim1 <- replace(estadosMbPars.InsAsim1,estadosMbPars.InsAsim1==2,1)
estadosMbPars.InsAsim1 <- replace(estadosMbPars.InsAsim1,estadosMbPars.InsAsim1==3,2)
estadosMbPars.InsAsim1 <- replace(estadosMbPars.InsAsim1,estadosMbPars.InsAsim1==4,3)
estadosMbPars.InsAsim1 <- as.data.frame(estadosMbPars.InsAsim1)

### Nectarivoria
resultados.basePars.NecAsim1 <- max.col(rea.BaseEKPars.NecAsim1$ancestral_likelihoods)

estadosMbPars.NecAsim1 <- replace(resultados.basePars.NecAsim1,resultados.basePars.NecAsim1==1,0)
estadosMbPars.NecAsim1 <- replace(estadosMbPars.NecAsim1,estadosMbPars.NecAsim1==2,1)
estadosMbPars.NecAsim1 <- replace(estadosMbPars.NecAsim1,estadosMbPars.NecAsim1==3,2)
estadosMbPars.NecAsim1 <- replace(estadosMbPars.NecAsim1,estadosMbPars.NecAsim1==4,3)
estadosMbPars.NecAsim1 <- as.data.frame(estadosMbPars.NecAsim1)


## Matriz bianria 1 ----
Asim2 <- matrix(c(0,1,2.9,0),nrow = 2)
Asim1 <- matrix(c(0,2.9,1,0),nrow = 2)

rea.B1EKPars.CarAsim1<- asr_max_parsimony(aceTree, B1Pars.Car,transition_costs = Asim1,Nstates =2)
rea.B1EKPars.FruAsim1<- asr_max_parsimony(aceTree, B1Pars.Fru,transition_costs = Asim1,Nstates =2)
rea.B1EKPars.HemAsim1<- asr_max_parsimony(aceTree, B1Pars.Hem,transition_costs = Asim1,Nstates =2)
rea.B1EKPars.InsAsim1<- asr_max_parsimony(aceTree, B1Pars.Ins,transition_costs = Asim1,Nstates =2)
rea.B1EKPars.NecAsim1<- asr_max_parsimony(aceTree, B1Pars.Nec,transition_costs = Asim1,Nstates =2)

### Carnivoria
resultados.B1Pars.CarAsim1 <- max.col(rea.B1EKPars.CarAsim1$ancestral_likelihoods)

estadosB1Pars.CarAsim1 <- replace(resultados.B1Pars.CarAsim1,resultados.B1Pars.CarAsim1==1,0)
estadosB1Pars.CarAsim1 <- replace(estadosB1Pars.CarAsim1,estadosB1Pars.CarAsim1==2,1)
estadosB1Pars.CarAsim1 <- as.data.frame(estadosB1Pars.CarAsim1)


### Frugivoria
resultados.B1Pars.FruAsim1 <- max.col(rea.B1EKPars.FruAsim1$ancestral_likelihoods)

estadosB1Pars.FruAsim1 <- replace(resultados.B1Pars.FruAsim1,resultados.B1Pars.FruAsim1==1,0)
estadosB1Pars.FruAsim1 <- replace(estadosB1Pars.FruAsim1,estadosB1Pars.FruAsim1==2,1)
estadosB1Pars.FruAsim1 <- as.data.frame(estadosB1Pars.FruAsim1)


### Hematofagia
resultados.B1Pars.HemAsim1 <- max.col(rea.B1EKPars.HemAsim1$ancestral_likelihoods)

estadosB1Pars.HemAsim1 <- replace(resultados.B1Pars.HemAsim1,resultados.B1Pars.HemAsim1==1,0)
estadosB1Pars.HemAsim1 <- replace(estadosB1Pars.HemAsim1,estadosB1Pars.HemAsim1==2,1)
estadosB1Pars.HemAsim1 <- as.data.frame(estadosB1Pars.HemAsim1)


### Insectivoria
resultados.B1Pars.InsAsim1 <- max.col(rea.B1EKPars.InsAsim1$ancestral_likelihoods)

estadosB1Pars.InsAsim1 <- replace(resultados.B1Pars.InsAsim1,resultados.B1Pars.InsAsim1==1,0)
estadosB1Pars.InsAsim1 <- replace(estadosB1Pars.InsAsim1,estadosB1Pars.InsAsim1==2,1)
estadosB1Pars.InsAsim1 <- as.data.frame(estadosB1Pars.InsAsim1)


### Nectarivoria
resultados.B1Pars.NecAsim1 <- max.col(rea.B1EKPars.NecAsim1$ancestral_likelihoods)

estadosB1Pars.NecAsim1 <- replace(resultados.B1Pars.NecAsim1,resultados.B1Pars.NecAsim1==1,0)
estadosB1Pars.NecAsim1 <- replace(estadosB1Pars.NecAsim1,estadosB1Pars.NecAsim1==2,1)
estadosB1Pars.NecAsim1 <- as.data.frame(estadosB1Pars.NecAsim1)


## Matriz bianria 2 ----

rea.B2EKPars.CarAsim1<- asr_max_parsimony(aceTree, B2Pars.Car,transition_costs = Asim1,Nstates =2)
rea.B2EKPars.FruAsim1<- asr_max_parsimony(aceTree, B2Pars.Fru,transition_costs = Asim1,Nstates =2)
rea.B2EKPars.HemAsim1<- asr_max_parsimony(aceTree, B2Pars.Hem,transition_costs = Asim1,Nstates =2)
rea.B2EKPars.InsAsim1<- asr_max_parsimony(aceTree, B2Pars.Ins,transition_costs = Asim1,Nstates =2)
rea.B2EKPars.NecAsim1<- asr_max_parsimony(aceTree, B2Pars.Nec,transition_costs = Asim1,Nstates =2)

### Carnivoria
resultados.B2Pars.CarAsim1 <- max.col(rea.B2EKPars.CarAsim1$ancestral_likelihoods)

estadosB2Pars.CarAsim1 <- replace(resultados.B2Pars.CarAsim1,resultados.B2Pars.CarAsim1==1,0)
estadosB2Pars.CarAsim1 <- replace(estadosB2Pars.CarAsim1,estadosB2Pars.CarAsim1==2,1)
estadosB2Pars.CarAsim1 <- as.data.frame(estadosB2Pars.CarAsim1)


### Frugivoria
resultados.B2Pars.FruAsim1 <- max.col(rea.B2EKPars.FruAsim1$ancestral_likelihoods)

estadosB2Pars.FruAsim1 <- replace(resultados.B2Pars.FruAsim1,resultados.B2Pars.FruAsim1==1,0)
estadosB2Pars.FruAsim1 <- replace(estadosB2Pars.FruAsim1,estadosB2Pars.FruAsim1==2,1)
estadosB2Pars.FruAsim1 <- as.data.frame(estadosB2Pars.FruAsim1)


### Hematofagia
resultados.B2Pars.HemAsim1 <- max.col(rea.B2EKPars.HemAsim1$ancestral_likelihoods)

estadosB2Pars.HemAsim1 <- replace(resultados.B2Pars.HemAsim1,resultados.B2Pars.HemAsim1==1,0)
estadosB2Pars.HemAsim1 <- replace(estadosB2Pars.HemAsim1,estadosB2Pars.HemAsim1==2,1)
estadosB2Pars.HemAsim1 <- as.data.frame(estadosB2Pars.HemAsim1)


### Insectivoria
resultados.B2Pars.InsAsim1 <- max.col(rea.B2EKPars.InsAsim1$ancestral_likelihoods)

estadosB2Pars.InsAsim1 <- replace(resultados.B2Pars.InsAsim1,resultados.B2Pars.InsAsim1==1,0)
estadosB2Pars.InsAsim1 <- replace(estadosB2Pars.InsAsim1,estadosB2Pars.InsAsim1==2,1)
estadosB2Pars.InsAsim1 <- as.data.frame(estadosB2Pars.InsAsim1)


### Nectarivoria
resultados.B2Pars.NecAsim1 <- max.col(rea.B2EKPars.NecAsim1$ancestral_likelihoods)

estadosB2Pars.NecAsim1 <- replace(resultados.B2Pars.NecAsim1,resultados.B2Pars.NecAsim1==1,0)
estadosB2Pars.NecAsim1 <- replace(estadosB2Pars.NecAsim1,estadosB2Pars.NecAsim1==2,1)
estadosB2Pars.NecAsim1 <- as.data.frame(estadosB2Pars.NecAsim1)


## Matriz bianria 3 ----

rea.B3EKPars.CarAsim1<- asr_max_parsimony(aceTree, B3Pars.Car,transition_costs = Asim1,Nstates =2)
rea.B3EKPars.FruAsim1<- asr_max_parsimony(aceTree, B3Pars.Fru,transition_costs = Asim1,Nstates =2)
rea.B3EKPars.HemAsim1<- asr_max_parsimony(aceTree, B3Pars.Hem,transition_costs = Asim1,Nstates =2)
rea.B3EKPars.InsAsim1<- asr_max_parsimony(aceTree, B3Pars.Ins,transition_costs = Asim1,Nstates =2)
rea.B3EKPars.NecAsim1<- asr_max_parsimony(aceTree, B3Pars.Nec,transition_costs = Asim1,Nstates =2)

### Carnivoria
resultados.B3Pars.CarAsim1 <- max.col(rea.B3EKPars.CarAsim1$ancestral_likelihoods)

estadosB3Pars.CarAsim1 <- replace(resultados.B3Pars.CarAsim1,resultados.B3Pars.CarAsim1==1,0)
estadosB3Pars.CarAsim1 <- replace(estadosB3Pars.CarAsim1,estadosB3Pars.CarAsim1==2,1)
estadosB3Pars.CarAsim1 <- as.data.frame(estadosB3Pars.CarAsim1)


### Frugivoria
resultados.B3Pars.FruAsim1 <- max.col(rea.B3EKPars.FruAsim1$ancestral_likelihoods)

estadosB3Pars.FruAsim1 <- replace(resultados.B3Pars.FruAsim1,resultados.B3Pars.FruAsim1==1,0)
estadosB3Pars.FruAsim1 <- replace(estadosB3Pars.FruAsim1,estadosB3Pars.FruAsim1==2,1)
estadosB3Pars.FruAsim1 <- as.data.frame(estadosB3Pars.FruAsim1)


### Hematofagia
resultados.B3Pars.HemAsim1 <- max.col(rea.B3EKPars.HemAsim1$ancestral_likelihoods)

estadosB3Pars.HemAsim1 <- replace(resultados.B3Pars.HemAsim1,resultados.B3Pars.HemAsim1==1,0)
estadosB3Pars.HemAsim1 <- replace(estadosB3Pars.HemAsim1,estadosB3Pars.HemAsim1==2,1)
estadosB3Pars.HemAsim1 <- as.data.frame(estadosB3Pars.HemAsim1)


### Insectivoria
resultados.B3Pars.InsAsim1 <- max.col(rea.B3EKPars.InsAsim1$ancestral_likelihoods)

estadosB3Pars.InsAsim1 <- replace(resultados.B3Pars.InsAsim1,resultados.B3Pars.InsAsim1==1,0)
estadosB3Pars.InsAsim1 <- replace(estadosB3Pars.InsAsim1,estadosB3Pars.InsAsim1==2,1)
estadosB3Pars.InsAsim1 <- as.data.frame(estadosB3Pars.InsAsim1)


### Nectarivoria
resultados.B3Pars.NecAsim1 <- max.col(rea.B3EKPars.NecAsim1$ancestral_likelihoods)

estadosB3Pars.NecAsim1 <- replace(resultados.B3Pars.NecAsim1,resultados.B3Pars.NecAsim1==1,0)
estadosB3Pars.NecAsim1 <- replace(estadosB3Pars.NecAsim1,estadosB3Pars.NecAsim1==2,1)
estadosB3Pars.NecAsim1 <- as.data.frame(estadosB3Pars.NecAsim1)



#----


# ASIMÉTRICA 2

## Matriz multiestado ----
rea.MultiEKParsAsim2 <- asr_max_parsimony(tree = aceTree,tip_states = multiPars,transition_costs = multAsim2,Nstates = 5)

### Estados ancestrales dados los valores de probabilidad
resultadosAsim2 <- max.col(rea.MultiEKParsAsim2$ancestral_likelihoods)

estadosMultiParsAsim2 <- replace(resultadosAsim2,resultadosAsim2==1,0)
estadosMultiParsAsim2 <- replace(estadosMultiParsAsim2,estadosMultiParsAsim2==2,1)
estadosMultiParsAsim2 <- replace(estadosMultiParsAsim2,estadosMultiParsAsim2==3,2)
estadosMultiParsAsim2 <- replace(estadosMultiParsAsim2,estadosMultiParsAsim2==4,3)
estadosMultiParsAsim2 <- replace(estadosMultiParsAsim2,estadosMultiParsAsim2==5,4)
estadosMultiParsAsim2 <- as.data.frame(estadosMultiParsAsim2)



## Matriz base ----
rea.BaseEKPars.CarAsim2 <- asr_max_parsimony(aceTree, basePars.Car,transition_costs = baseAsim2,Nstates =4)
rea.BaseEKPars.FruAsim2 <- asr_max_parsimony(aceTree, basePars.Fru,transition_costs = baseAsim2,Nstates =4)
rea.BaseEKPars.HemAsim2 <- asr_max_parsimony(aceTree, basePars.Hem,transition_costs = baseAsim2,Nstates =4)
rea.BaseEKPars.InsAsim2 <- asr_max_parsimony(aceTree, basePars.Ins,transition_costs = baseAsim2,Nstates =4)
rea.BaseEKPars.NecAsim2 <- asr_max_parsimony(aceTree, basePars.Nec,transition_costs = baseAsim2,Nstates =4)

### Carnivoria
resultados.basePars.CarAsim2 <- max.col(rea.BaseEKPars.CarAsim2$ancestral_likelihoods)

estadosMbPars.CarAsim2 <- replace(resultados.basePars.CarAsim2,resultados.basePars.CarAsim2==1,0)
estadosMbPars.CarAsim2 <- replace(estadosMbPars.CarAsim2,estadosMbPars.CarAsim2==2,1)
estadosMbPars.CarAsim2 <- replace(estadosMbPars.CarAsim2,estadosMbPars.CarAsim2==3,2)
estadosMbPars.CarAsim2 <- replace(estadosMbPars.CarAsim2,estadosMbPars.CarAsim2==4,3)
estadosMbPars.CarAsim2 <- as.data.frame(estadosMbPars.CarAsim2)


### Frugivoria
resultados.basePars.FruAsim2 <- max.col(rea.BaseEKPars.FruAsim2$ancestral_likelihoods)

estadosMbPars.FruAsim2 <- replace(resultados.basePars.FruAsim2,resultados.basePars.FruAsim2==1,0)
estadosMbPars.FruAsim2 <- replace(estadosMbPars.FruAsim2,estadosMbPars.FruAsim2==2,1)
estadosMbPars.FruAsim2 <- replace(estadosMbPars.FruAsim2,estadosMbPars.FruAsim2==3,2)
estadosMbPars.FruAsim2 <- replace(estadosMbPars.FruAsim2,estadosMbPars.FruAsim2==4,3)
estadosMbPars.FruAsim2 <- as.data.frame(estadosMbPars.FruAsim2)

### Hematofagia
resultados.basePars.HemAsim2 <- max.col(rea.BaseEKPars.HemAsim2$ancestral_likelihoods)

estadosMbPars.HemAsim2 <- replace(resultados.basePars.HemAsim2,resultados.basePars.HemAsim2==1,0)
estadosMbPars.HemAsim2 <- replace(estadosMbPars.HemAsim2,estadosMbPars.HemAsim2==2,1)
estadosMbPars.HemAsim2 <- replace(estadosMbPars.HemAsim2,estadosMbPars.HemAsim2==3,2)
estadosMbPars.HemAsim2 <- replace(estadosMbPars.HemAsim2,estadosMbPars.HemAsim2==4,3)
estadosMbPars.HemAsim2 <- as.data.frame(estadosMbPars.HemAsim2)

### Insectivoria
resultados.basePars.InsAsim2 <- max.col(rea.BaseEKPars.InsAsim2$ancestral_likelihoods)

estadosMbPars.InsAsim2 <- replace(resultados.basePars.InsAsim2,resultados.basePars.InsAsim2==1,0)
estadosMbPars.InsAsim2 <- replace(estadosMbPars.InsAsim2,estadosMbPars.InsAsim2==2,1)
estadosMbPars.InsAsim2 <- replace(estadosMbPars.InsAsim2,estadosMbPars.InsAsim2==3,2)
estadosMbPars.InsAsim2 <- replace(estadosMbPars.InsAsim2,estadosMbPars.InsAsim2==4,3)
estadosMbPars.InsAsim2 <- as.data.frame(estadosMbPars.InsAsim2)

### Nectarivoria
resultados.basePars.NecAsim2 <- max.col(rea.BaseEKPars.NecAsim2$ancestral_likelihoods)

estadosMbPars.NecAsim2 <- replace(resultados.basePars.NecAsim2,resultados.basePars.NecAsim2==1,0)
estadosMbPars.NecAsim2 <- replace(estadosMbPars.NecAsim2,estadosMbPars.NecAsim2==2,1)
estadosMbPars.NecAsim2 <- replace(estadosMbPars.NecAsim2,estadosMbPars.NecAsim2==3,2)
estadosMbPars.NecAsim2 <- replace(estadosMbPars.NecAsim2,estadosMbPars.NecAsim2==4,3)
estadosMbPars.NecAsim2 <- as.data.frame(estadosMbPars.NecAsim2)


## Matriz binaria 1 ----

rea.B1EKPars.CarAsim2<- asr_max_parsimony(aceTree, B1Pars.Car,transition_costs = Asim2,Nstates =2)
rea.B1EKPars.FruAsim2<- asr_max_parsimony(aceTree, B1Pars.Fru,transition_costs = Asim2,Nstates =2)
rea.B1EKPars.HemAsim2<- asr_max_parsimony(aceTree, B1Pars.Hem,transition_costs = Asim2,Nstates =2)
rea.B1EKPars.InsAsim2<- asr_max_parsimony(aceTree, B1Pars.Ins,transition_costs = Asim2,Nstates =2)
rea.B1EKPars.NecAsim2<- asr_max_parsimony(aceTree, B1Pars.Nec,transition_costs = Asim2,Nstates =2)

### Carnivoria
resultados.B1Pars.CarAsim2 <- max.col(rea.B1EKPars.CarAsim2$ancestral_likelihoods)

estadosB1Pars.CarAsim2 <- replace(resultados.B1Pars.CarAsim2,resultados.B1Pars.CarAsim2==1,0)
estadosB1Pars.CarAsim2 <- replace(estadosB1Pars.CarAsim2,estadosB1Pars.CarAsim2==2,1)
estadosB1Pars.CarAsim2 <- as.data.frame(estadosB1Pars.CarAsim2)


### Frugivoria
resultados.B1Pars.FruAsim2 <- max.col(rea.B1EKPars.FruAsim2$ancestral_likelihoods)

estadosB1Pars.FruAsim2 <- replace(resultados.B1Pars.FruAsim2,resultados.B1Pars.FruAsim2==1,0)
estadosB1Pars.FruAsim2 <- replace(estadosB1Pars.FruAsim2,estadosB1Pars.FruAsim2==2,1)
estadosB1Pars.FruAsim2 <- as.data.frame(estadosB1Pars.FruAsim2)


### Hematofagia
resultados.B1Pars.HemAsim2 <- max.col(rea.B1EKPars.HemAsim2$ancestral_likelihoods)

estadosB1Pars.HemAsim2 <- replace(resultados.B1Pars.HemAsim2,resultados.B1Pars.HemAsim2==1,0)
estadosB1Pars.HemAsim2 <- replace(estadosB1Pars.HemAsim2,estadosB1Pars.HemAsim2==2,1)
estadosB1Pars.HemAsim2 <- as.data.frame(estadosB1Pars.HemAsim2)


### Insectivoria
resultados.B1Pars.InsAsim2 <- max.col(rea.B1EKPars.InsAsim2$ancestral_likelihoods)

estadosB1Pars.InsAsim2 <- replace(resultados.B1Pars.InsAsim2,resultados.B1Pars.InsAsim2==1,0)
estadosB1Pars.InsAsim2 <- replace(estadosB1Pars.InsAsim2,estadosB1Pars.InsAsim2==2,1)
estadosB1Pars.InsAsim2 <- as.data.frame(estadosB1Pars.InsAsim2)


### Nectarivoria
resultados.B1Pars.NecAsim2 <- max.col(rea.B1EKPars.NecAsim2$ancestral_likelihoods)

estadosB1Pars.NecAsim2 <- replace(resultados.B1Pars.NecAsim2,resultados.B1Pars.NecAsim2==1,0)
estadosB1Pars.NecAsim2 <- replace(estadosB1Pars.NecAsim2,estadosB1Pars.NecAsim2==2,1)
estadosB1Pars.NecAsim2 <- as.data.frame(estadosB1Pars.NecAsim2)


## Matriz binaria 2 ----

rea.B2EKPars.CarAsim2<- asr_max_parsimony(aceTree, B2Pars.Car,transition_costs = Asim2,Nstates =2)
rea.B2EKPars.FruAsim2<- asr_max_parsimony(aceTree, B2Pars.Fru,transition_costs = Asim2,Nstates =2)
rea.B2EKPars.HemAsim2<- asr_max_parsimony(aceTree, B2Pars.Hem,transition_costs = Asim2,Nstates =2)
rea.B2EKPars.InsAsim2<- asr_max_parsimony(aceTree, B2Pars.Ins,transition_costs = Asim2,Nstates =2)
rea.B2EKPars.NecAsim2<- asr_max_parsimony(aceTree, B2Pars.Nec,transition_costs = Asim2,Nstates =2)

### Carnivoria
resultados.B2Pars.CarAsim2 <- max.col(rea.B2EKPars.CarAsim2$ancestral_likelihoods)

estadosB2Pars.CarAsim2 <- replace(resultados.B2Pars.CarAsim2,resultados.B2Pars.CarAsim2==1,0)
estadosB2Pars.CarAsim2 <- replace(estadosB2Pars.CarAsim2,estadosB2Pars.CarAsim2==2,1)
estadosB2Pars.CarAsim2 <- as.data.frame(estadosB2Pars.CarAsim2)


### Frugivoria
resultados.B2Pars.FruAsim2 <- max.col(rea.B2EKPars.FruAsim2$ancestral_likelihoods)

estadosB2Pars.FruAsim2 <- replace(resultados.B2Pars.FruAsim2,resultados.B2Pars.FruAsim2==1,0)
estadosB2Pars.FruAsim2 <- replace(estadosB2Pars.FruAsim2,estadosB2Pars.FruAsim2==2,1)
estadosB2Pars.FruAsim2 <- as.data.frame(estadosB2Pars.FruAsim2)


### Hematofagia
resultados.B2Pars.HemAsim2 <- max.col(rea.B2EKPars.HemAsim2$ancestral_likelihoods)

estadosB2Pars.HemAsim2 <- replace(resultados.B2Pars.HemAsim2,resultados.B2Pars.HemAsim2==1,0)
estadosB2Pars.HemAsim2 <- replace(estadosB2Pars.HemAsim2,estadosB2Pars.HemAsim2==2,1)
estadosB2Pars.HemAsim2 <- as.data.frame(estadosB2Pars.HemAsim2)


### Insectivoria
resultados.B2Pars.InsAsim2 <- max.col(rea.B2EKPars.InsAsim2$ancestral_likelihoods)

estadosB2Pars.InsAsim2 <- replace(resultados.B2Pars.InsAsim2,resultados.B2Pars.InsAsim2==1,0)
estadosB2Pars.InsAsim2 <- replace(estadosB2Pars.InsAsim2,estadosB2Pars.InsAsim2==2,1)
estadosB2Pars.InsAsim2 <- as.data.frame(estadosB2Pars.InsAsim2)


### Nectarivoria
resultados.B2Pars.NecAsim2 <- max.col(rea.B2EKPars.NecAsim2$ancestral_likelihoods)

estadosB2Pars.NecAsim2 <- replace(resultados.B2Pars.NecAsim2,resultados.B2Pars.NecAsim2==1,0)
estadosB2Pars.NecAsim2 <- replace(estadosB2Pars.NecAsim2,estadosB2Pars.NecAsim2==2,1)
estadosB2Pars.NecAsim2 <- as.data.frame(estadosB2Pars.NecAsim2)


## Matriz binaria 3----

rea.B3EKPars.CarAsim2<- asr_max_parsimony(aceTree, B3Pars.Car,transition_costs = Asim2,Nstates =2)
rea.B3EKPars.FruAsim2<- asr_max_parsimony(aceTree, B3Pars.Fru,transition_costs = Asim2,Nstates =2)
rea.B3EKPars.HemAsim2<- asr_max_parsimony(aceTree, B3Pars.Hem,transition_costs = Asim2,Nstates =2)
rea.B3EKPars.InsAsim2<- asr_max_parsimony(aceTree, B3Pars.Ins,transition_costs = Asim2,Nstates =2)
rea.B3EKPars.NecAsim2<- asr_max_parsimony(aceTree, B3Pars.Nec,transition_costs = Asim2,Nstates =2)

### Carnivoria
resultados.B3Pars.CarAsim2 <- max.col(rea.B3EKPars.CarAsim2$ancestral_likelihoods)

estadosB3Pars.CarAsim2 <- replace(resultados.B3Pars.CarAsim2,resultados.B3Pars.CarAsim2==1,0)
estadosB3Pars.CarAsim2 <- replace(estadosB3Pars.CarAsim2,estadosB3Pars.CarAsim2==2,1)
estadosB3Pars.CarAsim2 <- as.data.frame(estadosB3Pars.CarAsim2)


### Frugivoria
resultados.B3Pars.FruAsim2 <- max.col(rea.B3EKPars.FruAsim2$ancestral_likelihoods)

estadosB3Pars.FruAsim2 <- replace(resultados.B3Pars.FruAsim2,resultados.B3Pars.FruAsim2==1,0)
estadosB3Pars.FruAsim2 <- replace(estadosB3Pars.FruAsim2,estadosB3Pars.FruAsim2==2,1)
estadosB3Pars.FruAsim2 <- as.data.frame(estadosB3Pars.FruAsim2)


### Hematofagia
resultados.B3Pars.HemAsim2 <- max.col(rea.B3EKPars.HemAsim2$ancestral_likelihoods)

estadosB3Pars.HemAsim2 <- replace(resultados.B3Pars.HemAsim2,resultados.B3Pars.HemAsim2==1,0)
estadosB3Pars.HemAsim2 <- replace(estadosB3Pars.HemAsim2,estadosB3Pars.HemAsim2==2,1)
estadosB3Pars.HemAsim2 <- as.data.frame(estadosB3Pars.HemAsim2)


### Insectivoria
resultados.B3Pars.InsAsim2 <- max.col(rea.B3EKPars.InsAsim2$ancestral_likelihoods)

estadosB3Pars.InsAsim2 <- replace(resultados.B3Pars.InsAsim2,resultados.B3Pars.InsAsim2==1,0)
estadosB3Pars.InsAsim2 <- replace(estadosB3Pars.InsAsim2,estadosB3Pars.InsAsim2==2,1)
estadosB3Pars.InsAsim2 <- as.data.frame(estadosB3Pars.InsAsim2)


### Nectarivoria
resultados.B3Pars.NecAsim2 <- max.col(rea.B3EKPars.NecAsim2$ancestral_likelihoods)

estadosB3Pars.NecAsim2 <- replace(resultados.B3Pars.NecAsim2,resultados.B3Pars.NecAsim2==1,0)
estadosB3Pars.NecAsim2 <- replace(estadosB3Pars.NecAsim2,estadosB3Pars.NecAsim2==2,1)
estadosB3Pars.NecAsim2 <- as.data.frame(estadosB3Pars.NecAsim2)

