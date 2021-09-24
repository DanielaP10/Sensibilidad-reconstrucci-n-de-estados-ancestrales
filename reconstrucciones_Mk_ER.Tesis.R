#source("Script1_ArbolesYMatrices.Tesis.R")

##########################################################

#            RECONSTRUCCIONES TASAS IGUALES

## Multiestado

multiestadoFactor <- setNames(matrizMultiestado$Dieta,matrizMultiestado$Especie) 

reaMkERMulti <- ace(multiestadoFactor,aceTree,type = "discrete",model = "ER")

datos.reaMkERMulti <- reaMkERMulti$lik.anc


## MatrizBinaria1

### La matrizBinaria1 será dividida en cinco reconstrucciones de estado ancestral, una por cada dieta

matrizBinaria1.car <- setNames(matrizBinaria1$Carnivoria,matrizBinaria1$Especie)
matrizBinaria1.fru <- setNames(matrizBinaria1$Frugivoria,matrizBinaria1$Especie)
matrizBinaria1.hem <- setNames(matrizBinaria1$Hematofagia,matrizBinaria1$Especie)
matrizBinaria1.ins <- setNames(matrizBinaria1$Insectivoria,matrizBinaria1$Especie)
matrizBinaria1.nec <- setNames(matrizBinaria1$Nectarivoria,matrizBinaria1$Especie)

### Debido a que la reconstrucción solo se puede realizar de a un caracter, el estado ancestral de cada dieta será calculado
reaMkERB1.ins <- ace(matrizBinaria1.ins,aceTree,type = "discrete",model = "ER")
reaMkERB1.car <- ace(matrizBinaria1.car,aceTree,type = "discrete",model = "ER")
reaMkERB1.hem <- ace(matrizBinaria1.hem,aceTree,type = "discrete",model = "ER")
reaMkERB1.fru <- ace(matrizBinaria1.fru,aceTree,type = "discrete",model = "ER")
reaMkERB1.nec <- ace(matrizBinaria1.nec,aceTree,type = "discrete",model = "ER")

datos.reaMkERB1.ins <- reaMkERB1.ins$lik.anc
datos.reaMkERB1.car <- reaMkERB1.car$lik.anc
datos.reaMkERB1.hem <- reaMkERB1.hem$lik.anc
datos.reaMkERB1.fru <- reaMkERB1.fru$lik.anc
datos.reaMkERB1.nec <- reaMkERB1.nec$lik.anc



###########################################################

## Reconstrucción matrizBinaria 2 

### 
matrizBinaria2.car <- setNames(matrizBinaria2$Carnivoria,matrizBinaria2$Especie)
matrizBinaria2.fru <- setNames(matrizBinaria2$Frugivoria,matrizBinaria2$Especie)
matrizBinaria2.hem <- setNames(matrizBinaria2$Hematofagia,matrizBinaria2$Especie)
matrizBinaria2.ins <- setNames(matrizBinaria2$Insectivoria,matrizBinaria2$Especie)
matrizBinaria2.nec <- setNames(matrizBinaria2$Nectarivoria,matrizBinaria2$Especie)

## Reconstrucción
reaMkERB2.ins <- ace(matrizBinaria2.ins,aceTree,type = "discrete",model = "ER")
reaMkERB2.car <- ace(matrizBinaria2.car,aceTree,type = "discrete",model = "ER")
reaMkERB2.hem <- ace(matrizBinaria2.hem,aceTree,type = "discrete",model = "ER")
reaMkERB2.fru <- ace(matrizBinaria2.fru,aceTree,type = "discrete",model = "ER")
reaMkERB2.nec <- ace(matrizBinaria2.nec,aceTree,type = "discrete",model = "ER")

datos.reaMkERB2.ins <- reaMkERB2.ins$lik.anc
datos.reaMkERB2.car <- reaMkERB2.car$lik.anc
datos.reaMkERB2.hem <- reaMkERB2.hem$lik.anc
datos.reaMkERB2.fru <- reaMkERB2.fru$lik.anc
datos.reaMkERB2.nec <- reaMkERB2.nec$lik.anc

View(datos.reaMkERB2.car)

################################################################# 
# Reconstrucción matrizBinaria 3 

## 
matrizBinaria3.car <- setNames(matrizBinaria3$Carnivoria,matrizBinaria3$Especie)
matrizBinaria3.fru <- setNames(matrizBinaria3$Frugivoria,matrizBinaria3$Especie)
matrizBinaria3.hem <- setNames(matrizBinaria3$Hematofagia,matrizBinaria3$Especie)
matrizBinaria3.ins <- setNames(matrizBinaria3$Insectivoria,matrizBinaria3$Especie)
matrizBinaria3.nec <- setNames(matrizBinaria3$Nectarivoria,matrizBinaria3$Especie)

## Reconstrucción. Carnivoria no se reconstruye porque no hay carnivoros estrictos en mis datos.
reaMkERB3.ins <- ace(matrizBinaria3.ins,aceTree,type = "discrete",model = "ER")
#reaMkERB3.car <- ace(matrizBinaria3.car,aceTree,type = "discrete",model = "ER")
reaMkERB3.hem <- ace(matrizBinaria3.hem,aceTree,type = "discrete",model = "ER")
reaMkERB3.fru <- ace(matrizBinaria3.fru,aceTree,type = "discrete",model = "ER")
reaMkERB3.nec <- ace(matrizBinaria3.nec,aceTree,type = "discrete",model = "ER")

datos.reaMkERB3.ins <- reaMkERB3.ins$lik.anc
#datos.reaMkERB3.car <- reaMkERB3.car$lik.anc
datos.reaMkERB3.hem <- reaMkERB3.hem$lik.anc
datos.reaMkERB3.fru <- reaMkERB3.fru$lik.anc
datos.reaMkERB3.nec <- reaMkERB3.nec$lik.anc



##########################################################

## Reconstrucciones Matriz base

matrizBase.car <- setNames(matrizBase$Carnivoria,matrizBase$Especie)
matrizBase.fru <- setNames(matrizBase$Frugivoria,matrizBase$Especie)
matrizBase.hem <- setNames(matrizBase$Hematofagia,matrizBase$Especie)
matrizBase.ins <- setNames(matrizBase$Insectivoria,matrizBase$Especie)
matrizBase.nec <- setNames(matrizBase$Nectarivoria,matrizBase$Especie)

## Reconstrucción
reaMkErMb.ins <- ace(matrizBase.ins,aceTree,type = "discrete",model = "ER")
reaMkErMb.car <- ace(matrizBase.car,aceTree,type = "discrete",model = "ER")
reaMkErMb.hem <- ace(matrizBase.hem,aceTree,type = "discrete",model = "ER")
reaMkErMb.fru <- ace(matrizBase.fru,aceTree,type = "discrete",model = "ER")
reaMkErMb.nec <- ace(matrizBase.nec,aceTree,type = "discrete",model = "ER")

datos.reaMkErMb.ins <- reaMkErMb.ins$lik.anc
datos.reaMkErMb.car <- reaMkErMb.car$lik.anc
datos.reaMkErMb.hem <- reaMkErMb.hem$lik.anc
datos.reaMkErMb.fru <- reaMkErMb.fru$lik.anc
datos.reaMkErMb.nec <- reaMkErMb.nec$lik.anc
