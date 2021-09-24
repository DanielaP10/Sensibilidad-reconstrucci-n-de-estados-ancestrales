
library(readxl)
library(dplyr)
library(ape)
library(ggplot2)
library(ggtree)
library(phytools)
library(tidyr)
library(castor)
library(phangorn)
library(reshape2)


###########################################################
#Este código tiene como fin generar las tres matrices de codificación: matriz base (Mb), matriz multiestado y las matrices binarias 1, 2 y 3 (B1, B2 y B3, respectivamente) derivadas de la tabla de dietas, la cual contiene los reportes de dieta obtenidos para todas las especies junto con su fuente de literatura.


###########################################################

setwd("C:/Users/Papra/Documents/Trabajo de Grado/Piloto1/")

# Llamada de la Tabla de dietas
tablaDietasTotal <- read_xlsx('C:/Users/Papra/Documents/Trabajo de Grado/Dietas/Tabla_dietas.xlsx',guess_max = 10000) ###guess_max es para indicar el número máximo de filas de datos que se utilizarán para adivinar tipos de columnas (según R)


# En la columna "Reportes" de tablaDietasTotal, dividiremos los nombres de las especies y los reportes en dos columnas distintas: Especie y Reporte, los cuales están originalmente separados por un guión "-". Para esto se creará un objeto llamado tmp1, que después será el data frame tmp2. Seguido de esto, crearemos un objeto llamado tmp3 que contenga únicamente las columnas de los trabajos usados con la información de los reportes, para unirlo con tmp1 y crear tablaDietas2
tmp1 <- strsplit(tablaDietasTotal$Reportes,"-")

## Data frame con los nombres de las especies y de los reportes
temp2 <- as.data.frame(matrix(unlist(tmp1),ncol= 2,byrow = T))

names(temp2) <- c("Especie","Reporte")

## Data frame de la información de cada trabajo revisado
temp3 <- as.data.frame(tablaDietasTotal[,2:length(tablaDietasTotal[1,])])

## Unión de las filas con los nombres de las especies y los reportes, con las columnas de lo reportado por cada trabajo 
temp4 <- cbind(temp2,temp3)


# En la Tabla de dietas original hay reportes de fuentes alimenticias como materia vegetal no identificada, polen, nectar-polen, y el número de reportes por tipo de evidencia, es necesario agrupar únicamente los reportes de las dietas Insectivora, Carnivora, Hematofaga, Frugivora y Nectarivora
tablaDietasResumida <- temp4[!temp4$Reporte %in% c('obs','matVeg','contEst','heces','isótopos','nect','poli'),]

tablaDietasResumida[is.na(tablaDietasResumida)]=0

rownames(tablaDietasResumida) <- 1:nrow(tablaDietasResumida)


# Crearemos el objeto llamado matrizIncidencias, el cual es la organización de los reportes de números de individuos a incidencias como binarios
matrizIncidencias <- tablaDietasResumida

## Cambio de reportes continuos a binarios.
for (columnas in 3:length(tablaDietasResumida[1,])) {
  for (filas in 1:length(tablaDietasResumida[,1])) {
    if (matrizIncidencias[filas,columnas]>0) {
      matrizIncidencias[filas,columnas] <- 1
    }
  }
}

# Para hacer la Matriz base, crearemos una tabla preliminar, el dataframe matrizBasePrem, con las mismas especies y nombres de matrizIncidencias, más 4 columnas adicionles: Conteo, Sumatoria, Frecuencia y Estado. Los valores de la columna Conteo hacen referencia a la cantidad de reportes de dieta para cada especie; los valores de Sumatoria son la suma total de reportes de dieta por especie; la columna de Frecuencia contará con los valores de frecuencia de reporte de dietas (Conteo) dada la cantidad de reportes totales para la especie (Sumatoria); y por último, la columna Estado tendrá los estados de cada dieta siguiendo la regla mencionada en el escrito del trabajo.
matrizBasePrem <- data.frame('Especie'= tablaDietasResumida$Especie,'Reporte'= tablaDietasResumida$Reporte, 'Conteo' = 0,'Sumatoria'=0,'Frecuencia'=0,'Estado'=0)


## Columna Conteo
for (i in 1:length(tablaDietasResumida$Especie)) {
  matrizBasePrem[i,3] <- sum(matrizIncidencias[i,3:length(tablaDietasResumida[1,])],na.rm = T)
}


## Columna Sumatoria
suma <- matrizBasePrem %>% group_by(Especie) %>% summarize(Sumatoria=sum(Conteo)) # Ctrl + Shift + M = %>% 

matrizBasePrem$Sumatoria=rep(suma$Sumatoria,each=5)


## Columna Frecuencia
matrizBasePrem$Frecuencia <- round(matrizBasePrem$Conteo/matrizBasePrem$Sumatoria,digits=3)


## Columna Estado. Ya teniendo la frecuencia de reporte de las dietas, les asignaremos un estado: Ausente (0), Complementaria (1), Predominante (2) o Estricta (3); a cada las especies. 

frecuencia1 <- 0.05
frecuencia2 <- 0.5
frecuencia3 <- 0.95

for (i in 1:length(matrizBasePrem$Especie)) { 
  if (matrizBasePrem[i,5]<frecuencia1) {
    matrizBasePrem[i,6]=0
  } 
  else if (matrizBasePrem[i,5]==frecuencia1){
    matrizBasePrem[i,6]=0
  }
  else if (matrizBasePrem[i,5]>frecuencia1&matrizBasePrem[i,5]<frecuencia2){
    matrizBasePrem[i,6]=1
  }
  else if (matrizBasePrem[i,5]==frecuencia2){
    matrizBasePrem[i,6]=1
  }
  else if (matrizBasePrem[i,5]>frecuencia2&matrizBasePrem[i,5]<frecuencia3){
    matrizBasePrem[i,6]=2
  }
  else if (matrizBasePrem[i,5]==frecuencia3){
    matrizBasePrem[i,6]=3
  }
  else if (matrizBasePrem[i,5]>frecuencia3){
    matrizBasePrem[i,6]=3
  }
} 


# Matriz base. Ya que contamos con los estados de dieta en cada especie, construiremos un dataframe cuyas filas serán las especies, las columnas las dietas y los estados los valores que conpongan el contenido dentro de estas.
matrizBasePrem2 <- data.frame('Especie'=matrizBasePrem$Especie,'Dieta'=matrizBasePrem$Reporte,'Estados'=matrizBasePrem$Estado)

# Como los reportes de dieta están organizados como filas para cada especie, traspondremos la matrizBasePrem2 de tal forma que hayan 6 columnas, una con los nombres de las especies y las 5 siguientes con las dietas. Este nuevo dataframe, matrizBase, se compondrá de los estados de dieta por especie.
matrizBase1 <- matrix(matrizBasePrem2[,3],nrow = length(matrizBasePrem2$Especie)/5,ncol = 5,byrow = T)

## Las dietas van en orden alfabético: carnivoria, frugivoria, hematofagia, insectivoria y nectarivoria. Como en la tabla de dietas están con otro orden acá pues lo acomodo alfebéticamente.
matrizBase <- data.frame('Especie'=unique(matrizBasePrem2$Especie),"Carnivoria"=matrizBase1[,3],"Frugivoria"=matrizBase1[,4],"Hematofagia"=matrizBase1[,2],'Insectivoria'=matrizBase1[,1],"Nectarivoria"=matrizBase1[,5])

matrizBaseTotal <- matrizBase

# Matriz multiestado. Asignaremos las dietas predominantes (2) o estrictas (3) obtenidas en la matrizBase para cada especie. Es decir, las filas serán las especies y habrán dos columnas, una con los nombres de las especies, llamada Especie, y la segunda, Dieta, con los valores de estado que serian las dietas: Carnivoria (0), Frugivoria (1),  Hematofagia (2), Insectivoria (3) y Nectarivoria (4).

matrizMultiestado <- data.frame('Especie'= unique(matrizBase$Especie),'Dieta'=NA)

freq <- matrix(matrizBasePrem$Frecuencia,nrow = length(matrizBasePrem2$Especie)/5,ncol = 5,byrow = T)

for (i in 1:length(matrizBase$Especie)) {
  if (max(freq[i,1:5])==freq[i,1]) {
    matrizMultiestado[i,2]=3
  }
  else if (max(freq[i,1:5])==freq[i,2]) {
    matrizMultiestado[i,2]=2
  }
  else if (max(freq[i,1:5])==freq[i,3]) {
    matrizMultiestado[i,2]=0
  }
  else if (max(freq[i,1:5])==freq[i,4]) {
    matrizMultiestado[i,2]=1
  }
  else if (max(freq[i,1:5])==freq[i,5]) {
    matrizMultiestado[i,2]=4
  }
}

matrizMultiestadoTotal <- matrizMultiestado

# Matriz binaria 1. Para construir esta matriz consideraremos las dietas complementarias (1), predominantes (2) y estrictas (3) de la matrizBase, dietas que serán codificadas como presentes (1). Las dietas ausentes (0) en la matrizBase se mantendrán con esa misma codificación en la matrizBinaria1.

matrizBinaria1 <- data.frame('Especie'=matrizBase$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBase$Especie)) {
  for (colums in 2:6) {
    if (matrizBase[rows,colums]==0) {
      matrizBinaria1[rows,colums]=0
    }
    else if (matrizBase[rows,colums]>0) {
      matrizBinaria1[rows,colums]=1
    }
  }
}

matrizBinaria1Total <- matrizBinaria1

# Matriz binaria 2. Similar a la matrizBinaria1, esta matriz codifica las dietas predominantes y estrictas como presentes en la especie, y las complementarias y asuentes como ausentes.

matrizBinaria2 <- data.frame('Especie'=matrizBase$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBase$Especie)) {
  for (colums in 2:6) {
    if (matrizBase[rows,colums]<1) {
      matrizBinaria2[rows,colums]=0
    }
    else if (matrizBase[rows,colums]>1) {
      matrizBinaria2[rows,colums]=1
    }
  }
}

matrizBinaria2Total <- matrizBinaria2

# Matriz binaria 3. A diferencia de las matrices matrizBinaria1 y matrizBinaria2, esta matriz solo codificará como presente las dietas estrictas de matrizBase, mientras que las ausentes, complementarias y predominantes se codificarán como ausentes.

matrizBinaria3 <- data.frame('Especie'=matrizBase$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBase$Especie)) {
  for (colums in 2:6) {
    if (matrizBase[rows,colums]<2) {
      matrizBinaria3[rows,colums]=0
    }
    else if (matrizBase[rows,colums]>2) {
      matrizBinaria3[rows,colums]=1
    }
  }
}

matrizBinaria3Total <- matrizBinaria3






############################################################

# Recorte del árbol de Upham et al. (2019). Se seguirá la taxonimia de Mammal Diversity Database.

treePhyllostomidae <- read.tree("Arbol_mammals/aceTree.tree",tree.names = T)

## Poda del árbol con respecto a las especies con reportes de dieta.

noComunes <- treePhyllostomidae$tip.label[!treePhyllostomidae$tip.label %in% matrizMultiestado$Especie]

aceTree <- drop.tip(treePhyllostomidae,tip=noComunes)

matrizMultiestado <- matrizMultiestadoTotal %>% filter(matrizMultiestadoTotal$Especie%in%aceTree$tip.label)

############################################################# ORGANIZACIÓN MATRICES - MÁXIMA VEROSIMILITUD

## Matriz binaria 1

matrizBinaria1 <- matrizBinaria1Total %>% filter(matrizBinaria1Total$Especie%in%aceTree$tip.label)


## Matriz binaria 2

matrizBinaria2 <- matrizBinaria2Total %>% filter(matrizBinaria2Total$Especie%in%aceTree$tip.label)


## Matriz binaria 3

matrizBinaria3 <- matrizBinaria3Total %>% filter(matrizBinaria3Total$Especie%in%aceTree$tip.label)


## Matriz base

matrizBase <- matrizBaseTotal %>% filter(matrizBaseTotal$Especie%in%aceTree$tip.label)


############################################################# ORGANIZACIÓN MATRICES - PARSIMONIA

# Como la función usada para reconstruir estados ancestrales con parsimonia asigna los estados en orden con las especies en el árbol, es necesario construir de nuevo las matrices, desde frecuencia de reporte de dietas. 

## En Excel se organizaron las frecuencias de reporte de dieta de las especies, según el árbol,  para creación de las demás codificaciones.


## Frecuencia de reporte (objeto freq) con los nombres organizados.
freqRep <- as.data.frame(read.csv("freqReports.csv",sep=";"))

## Matriz multiestado -----
# Las codificaciones no pueden tener estado 0, entonces empieza en 1: Carnivoria, 1; frugivoria, 2; hematofagia, 3; insectivoria, 4, y nectarioria, 5
matrizMultiestadoPars <- data.frame('Especie'= aceTree$tip.label,'Dieta'=0)

for (i in 1:length(freqRep$Especie)) {
  if (max(freqRep[i,2:6])==freqRep[i,4]) {
    matrizMultiestadoPars[i,2]=1
  }
  else if (max(freqRep[i,2:6])==freqRep[i,5]) {
    matrizMultiestadoPars[i,2]=2
  }
  else if (max(freqRep[i,2:6])==freqRep[i,3]) {
    matrizMultiestadoPars[i,2]=3
  }
  else if (max(freqRep[i,2:6])==freqRep[i,2]) {
    matrizMultiestadoPars[i,2]=4
  }
  else if (max(freqRep[i,2:6])==freqRep[i,6]) {
    matrizMultiestadoPars[i,2]=5
  }
}


## Matriz base ---- 
#Los estados se asignarán similar a la matriz multiestado, pero con los estados correspondientes a esta codificación: ausente, 1; complementario, 2; predominante, 3, y estricto, 4

matrizBasePars <- data.frame("Especie"=matrizMultiestadoPars$Especie,"Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,"Insectivoria"=0,"Nectarivoria"=0)

frecuencia1 <- 0.05
frecuencia2 <- 0.5
frecuencia3 <- 0.95

## Carnivoria
for (i in 1:length(freqRep$Especie)) { 
  if (freqRep[i,4]<=frecuencia1) {
    matrizBasePars[i,2]=1
  } 
  else if (freqRep[i,4]>frecuencia1&freqRep[i,4]<=frecuencia2){
    matrizBasePars[i,2]=2
  }
  else if (freqRep[i,4]>frecuencia2&freqRep[i,4]<frecuencia3){
    matrizBasePars[i,2]=3
  }
  else if (freqRep[i,4]>=frecuencia3){
    matrizBasePars[i,2]=4
  }
} 


## Frugivoria
for (i in 1:length(freqRep$Especie)) { 
  if (freqRep[i,5]<=frecuencia1) {
    matrizBasePars[i,3]=1
  } 
  else if (freqRep[i,5]>frecuencia1&freqRep[i,5]<=frecuencia2){
    matrizBasePars[i,3]=2
  }
  else if (freqRep[i,5]>frecuencia2&freqRep[i,5]<frecuencia3){
    matrizBasePars[i,3]=3
  }
  else if (freqRep[i,5]>=frecuencia3){
    matrizBasePars[i,3]=4
  }
}


## Hematofagia
for (i in 1:length(freqRep$Especie)) { 
  if (freqRep[i,3]<=frecuencia1) {
    matrizBasePars[i,4]=1
  } 
  else if (freqRep[i,3]>frecuencia1&freqRep[i,3]<=frecuencia2){
    matrizBasePars[i,4]=2
  }
  else if (freqRep[i,3]>frecuencia2&freqRep[i,3]<frecuencia3){
    matrizBasePars[i,4]=3
  }
  else if (freqRep[i,3]>=frecuencia3){
    matrizBasePars[i,4]=4
  }
}


## Insectivoria
for (i in 1:length(freqRep$Especie)) { 
  if (freqRep[i,2]<=frecuencia1) {
    matrizBasePars[i,5]=1
  } 
  else if (freqRep[i,2]>frecuencia1&freqRep[i,2]<=frecuencia2){
    matrizBasePars[i,5]=2
  }
  else if (freqRep[i,2]>frecuencia2&freqRep[i,2]<frecuencia3){
    matrizBasePars[i,5]=3
  }
  else if (freqRep[i,2]>=frecuencia3){
    matrizBasePars[i,5]=4
  }
}


## Nectarivoria
for (i in 1:length(freqRep$Especie)) { 
  if (freqRep[i,6]<=frecuencia1) {
    matrizBasePars[i,6]=1
  } 
  else if (freqRep[i,6]>frecuencia1&freqRep[i,6]<=frecuencia2){
    matrizBasePars[i,6]=2
  }
  else if (freqRep[i,6]>frecuencia2&freqRep[i,6]<frecuencia3){
    matrizBasePars[i,6]=3
  }
  else if (freqRep[i,6]>=frecuencia3){
    matrizBasePars[i,6]=4
  }
}
## Matriz bianria 1----
matrizBinaria1Pars <- data.frame('Especie'=matrizBasePars$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBasePars$Especie)) {
  for (colums in 2:6) {
    if (matrizBasePars[rows,colums]>1) {
      matrizBinaria1Pars[rows,colums]=2
    }
    else if (matrizBasePars[rows,colums]==1) {
      matrizBinaria1Pars[rows,colums]=1
    }
  }
}
## Matriz bianria 2----
matrizBinaria2Pars <- data.frame('Especie'=matrizBasePars$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBasePars$Especie)) {
  for (colums in 2:6) {
    if (matrizBasePars[rows,colums]<3) {
      matrizBinaria2Pars[rows,colums]=1
    }
    else if (matrizBasePars[rows,colums]>2) {
      matrizBinaria2Pars[rows,colums]=2
    }
  }
}
## Matris binaria 3----
matrizBinaria3Pars <- data.frame('Especie'=matrizBasePars$Especie, "Carnivoria"=0,"Frugivoria"=0,"Hematofagia"=0,'Insectivoria'=0, "Nectarivoria"=0)

for (rows in 1:length(matrizBasePars$Especie)) {
  for (colums in 2:6) {
    if (matrizBasePars[rows,colums]<=3) {
      matrizBinaria3Pars[rows,colums]=1
    }
    else if (matrizBasePars[rows,colums]>3) {
      matrizBinaria3Pars[rows,colums]=2
    }
  }
}