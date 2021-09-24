#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")

######################################################### RECONSTRUCCIONES Máxima verosimilitud TASAS DESIGUALES

# Multiestado 
multAsim1 <- matrix(c(0,2.9,2.9,2.9,2.9,1,0,2.9,2.9,2.9,1,1,0,2.9,2.9,1,1,1,0,2.9,1,1,1,1,0),nrow = 5)

multAsim2 <- matrix(c(0,1,1,1,1,2.9,0,1,1,1,2.9,2.9,0,1,1,2.9,2.9,2.9,0,1,2.9,2.9,2.9,2.9,0),nrow = 5)

## Asimétrica 1

reaMkDRMulti <- ace(multiestadoFactor,aceTree,type = "discrete",model = multAsim1)
datos.reaMkDRMulti <- reaMkDRMulti$lik.anc

## Asimétrica 2

reaMkDRMulti2 <- ace(multiestadoFactor,aceTree,type = "discrete",model = multAsim2)
datos.reaMkDRMulti2 <- reaMkDRMulti2$lik.anc



## MatrizBinaria1
asim2 <- matrix(c(0,1,2.9,0),nrow = 2)
asim1 <- matrix(c(0,2.9,1,0),nrow = 2)

## Asimétrica 1
reaMkDRB1.ins <- ace(matrizBinaria1.ins,aceTree,type = "discrete",model = asim1)
reaMkDRB1.car <- ace(matrizBinaria1.car,aceTree,type = "discrete",model = asim1)
reaMkDRB1.hem <- ace(matrizBinaria1.hem,aceTree,type = "discrete",model = asim1)
reaMkDRB1.fru <- ace(matrizBinaria1.fru,aceTree,type = "discrete",model = asim1)
reaMkDRB1.nec <- ace(matrizBinaria1.nec,aceTree,type = "discrete",model = asim1)

datos.reaMkDRB1.ins <- reaMkDRB1.ins$lik.anc
datos.reaMkDRB1.car <- reaMkDRB1.car$lik.anc
datos.reaMkDRB1.hem <- reaMkDRB1.hem$lik.anc
datos.reaMkDRB1.fru <- reaMkDRB1.fru$lik.anc
datos.reaMkDRB1.nec <- reaMkDRB1.nec$lik.anc


## Asimétrica 2
reaMkDRB1.ins2 <- ace(matrizBinaria1.ins,aceTree,type = "discrete",model = asim2)
reaMkDRB1.car2 <- ace(matrizBinaria1.car,aceTree,type = "discrete",model = asim2)
reaMkDRB1.hem2 <- ace(matrizBinaria1.hem,aceTree,type = "discrete",model = asim2)
reaMkDRB1.fru2 <- ace(matrizBinaria1.fru,aceTree,type = "discrete",model = asim2)
reaMkDRB1.nec2 <- ace(matrizBinaria1.nec,aceTree,type = "discrete",model = asim2)

datos.reaMkDRB1.ins2 <- reaMkDRB1.ins2$lik.anc
datos.reaMkDRB1.car2 <- reaMkDRB1.car2$lik.anc
datos.reaMkDRB1.hem2 <- reaMkDRB1.hem2$lik.anc
datos.reaMkDRB1.fru2 <- reaMkDRB1.fru2$lik.anc
datos.reaMkDRB1.nec2 <- reaMkDRB1.nec2$lik.anc



# Matriz binaria 2 

## Asimétrica 1
reaMkDRB2.ins <- ace(matrizBinaria2.ins,aceTree,type = "discrete",model = asim1)
reaMkDRB2.car <- ace(matrizBinaria2.car,aceTree,type = "discrete",model = asim1)
reaMkDRB2.hem <- ace(matrizBinaria2.hem,aceTree,type = "discrete",model = asim1)
reaMkDRB2.fru <- ace(matrizBinaria2.fru,aceTree,type = "discrete",model = asim1)
reaMkDRB2.nec <- ace(matrizBinaria2.nec,aceTree,type = "discrete",model = asim1)

datos.reaMkDRB2.ins <- reaMkDRB2.ins$lik.anc
datos.reaMkDRB2.car <- reaMkDRB2.car$lik.anc
datos.reaMkDRB2.hem <- reaMkDRB2.hem$lik.anc
datos.reaMkDRB2.fru <- reaMkDRB2.fru$lik.anc
datos.reaMkDRB2.nec <- reaMkDRB2.nec$lik.anc



## Asimétrica 2
reaMkDRB2.ins2 <- ace(matrizBinaria2.ins,aceTree,type = "discrete",model = asim2)
reaMkDRB2.car2 <- ace(matrizBinaria2.car,aceTree,type = "discrete",model = asim2)
reaMkDRB2.hem2 <- ace(matrizBinaria2.hem,aceTree,type = "discrete",model = asim2)
reaMkDRB2.fru2 <- ace(matrizBinaria2.fru,aceTree,type = "discrete",model = asim2)
reaMkDRB2.nec2 <- ace(matrizBinaria2.nec,aceTree,type = "discrete",model = asim2)

datos.reaMkDRB2.ins2 <- reaMkDRB2.ins2$lik.anc
datos.reaMkDRB2.car2 <- reaMkDRB2.car2$lik.anc
datos.reaMkDRB2.hem2 <- reaMkDRB2.hem2$lik.anc
datos.reaMkDRB2.fru2 <- reaMkDRB2.fru2$lik.anc
datos.reaMkDRB2.nec2 <- reaMkDRB2.nec2$lik.anc


# Matriz binaria 3 

## Asimétrica 1. Carnivoria no se reconstruye porque no hay carnivoros estrictos en mis datos.
reaMkDRB3.ins <- ace(matrizBinaria3.ins,aceTree,type = "discrete",model = asim1)
#reaMkDRB3.car <- ace(matrizBinaria3.car,aceTree,type = "discrete",model = asim1)
reaMkDRB3.hem <- ace(matrizBinaria3.hem,aceTree,type = "discrete",model = asim1)
reaMkDRB3.fru <- ace(matrizBinaria3.fru,aceTree,type = "discrete",model = asim1)
reaMkDRB3.nec <- ace(matrizBinaria3.nec,aceTree,type = "discrete",model = asim1)

datos.reaMkDRB3.ins <- reaMkDRB3.ins$lik.anc
#datos.reaMkDRB3.car <- reaMkDRB3.car$lik.anc
datos.reaMkDRB3.hem <- reaMkDRB3.hem$lik.anc
datos.reaMkDRB3.fru <- reaMkDRB3.fru$lik.anc
datos.reaMkDRB3.nec <- reaMkDRB3.nec$lik.anc



## Asimétrica 2
reaMkDRB3.ins2 <- ace(matrizBinaria3.ins,aceTree,type = "discrete",model = asim2)
#reaMkDRB3.car2 <- ace(matrizBinaria3.car,aceTree,type = "discrete",model = asim1)
reaMkDRB3.hem2 <- ace(matrizBinaria3.hem,aceTree,type = "discrete",model = asim2)
reaMkDRB3.fru2 <- ace(matrizBinaria3.fru,aceTree,type = "discrete",model = asim2)
reaMkDRB3.nec2 <- ace(matrizBinaria3.nec,aceTree,type = "discrete",model = asim2)

datos.reaMkDRB3.ins2 <- reaMkDRB3.ins2$lik.anc
#datos.reaMkDRB3.car2 <- reaMkDRB3.car2$lik.anc
datos.reaMkDRB3.hem2 <- reaMkDRB3.hem2$lik.anc
datos.reaMkDRB3.fru2 <- reaMkDRB3.fru2$lik.anc
datos.reaMkDRB3.nec2 <- reaMkDRB3.nec2$lik.anc


## Matriz base 
baseAsim1 <- matrix(c(0,2.9,2.9,2.9,1,0,2.9,2.9,1,1,0,2.9,1,1,1,0),nrow = 4)
baseAsimHem1 <- matrix(c(0,2.9,2.9,1,0,2.9,1,1,0),nrow = 3)

baseAsim2 <- matrix(c(0,1,1,1,2.9,0,1,1,2.9,2.9,0,1,2.9,2.9,2.9,0),nrow = 4)
baseAsimHem2 <- matrix(c(0,2.9,2.9,1,0,2.9,1,1,0),nrow = 3)


## Asimétrica 1
reaMkDrMb.ins <- ace(matrizBase.ins,aceTree,type = "discrete",model = baseAsim1)
reaMkDrMb.car <- ace(matrizBase.car,aceTree,type = "discrete",model = baseAsimHem1)
reaMkDrMb.hem <- ace(matrizBase.hem,aceTree,type = "discrete",model = baseAsimHem1)
reaMkDrMb.fru <- ace(matrizBase.fru,aceTree,type = "discrete",model = baseAsim1)
reaMkDrMb.nec <- ace(matrizBase.nec,aceTree,type = "discrete",model = baseAsim1)

datos.reaMkDrMb.ins <- reaMkDrMb.ins$lik.anc
datos.reaMkDrMb.car <- reaMkDrMb.car$lik.anc
datos.reaMkDrMb.hem <- reaMkDrMb.hem$lik.anc
datos.reaMkDrMb.fru <- reaMkDrMb.fru$lik.anc
datos.reaMkDrMb.nec <- reaMkDrMb.nec$lik.anc



## Asimétrica 2
reaMkDrMb.ins2 <- ace(matrizBase.ins,aceTree,type = "discrete",model = baseAsim2)
reaMkDrMb.car2 <- ace(matrizBase.car,aceTree,type = "discrete",model = baseAsimHem2)
reaMkDrMb.hem2 <- ace(matrizBase.hem,aceTree,type = "discrete",model = baseAsimHem2)
reaMkDrMb.fru2 <- ace(matrizBase.fru,aceTree,type = "discrete",model = baseAsim2)
reaMkDrMb.nec2 <- ace(matrizBase.nec,aceTree,type = "discrete",model = baseAsim2)

datos.reaMkDrMb.ins2 <- reaMkDrMb.ins2$lik.anc
datos.reaMkDrMb.car2 <- reaMkDrMb.car2$lik.anc
datos.reaMkDrMb.hem2 <- reaMkDrMb.hem2$lik.anc
datos.reaMkDrMb.fru2 <- reaMkDrMb.fru2$lik.anc
datos.reaMkDrMb.nec2 <- reaMkDrMb.nec2$lik.anc
