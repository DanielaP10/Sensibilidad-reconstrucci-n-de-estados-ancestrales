###########################################################
#Este código tiene como fin crear tres matrices: Matriz base (Mb), Matriz multiestado (Mm), Matriz binaria 1, 2 y 3 (B1, B2 y B3, respectivamente) derivadas de la Tabla de dietas cruda, la cual contiene los reportes de dieta obtenidos para todas las especies junto con su fuente en la literatura.

#Estos reportes de dieta están dados por la cantidad de individuos utilizados en los trabajos revisados. Siguiendo esto, se construirán tres tablas complementarias distintas, basadas en la Tabla de dietas cruda; con estas tablas se crearán las matrices Mb, Mm, B1, B2 y B3, es decir, habrán tres réplicas de cada matriz.

#Matriz base - Mb: esta matriz cuenta con 5 caracteres: Insectivoría, Carnivoría, Hematofagia, Frugivoría y Nectarivoría; y 4 estados: Ausente (0), Complementario (1), Predominante (2) y Estricto (3). La asignación de estados dependerá de la frecuencia de reporte de la dieta por especie, es decir, si una especie presenta una frecuencia de reporte de alguna dieta (carácter) en menos de 0.05, se considera que esta dieta está ausente (0) en la epecie; si la frecuencia está entre 0.05 y 0.5 se considera que la dieta es complementaria (1) en la especie; por otro lado, si la frecuencia es mayor a 0.5 y menor a 0.95, la dieta se asume como predominante (2) en la especie; y si esta frecuencia es mayor o igual a 0.95, asignamos la especie como estricta (3) a dicho hábito alimenticio. De esta matriz derivarán las demás matrices.

#Matriz multiestado - Mm: esta matriz cuenta con un solo caracter, dieta, y 5 estados: Insectivoría (0), Carnivoría (1), Hematofagia (2), Frugivoría (3) y Nectarivoría (4). Para construir esta matriz se asignará a cada especie la dieta (estado) que en Mb sea complementaria o estricta. En caso de que presente solo dietas complementarias, en Mb, se tendrá en cuenta la dieta con mayor frecuencia de reporte para la especie.

#Matrices binarias: estas matrices tendrán 5 carácteres: Insectivoría, Carnivoría, Hematofagia, Frugivoría y Nectarivoría, y los estados serán ausente (0) y presente (0). Las matrices serán contruidas bajo la misma norma, pero distinto grado de estrictez.La matriz binaria 1 (B1) considerará las dietas complementarias, predominantes y estrictas, en Mb, de la especie como presentes (1). Para la matriz binaria 2 (B2) las dietas predominantes y estrictas, en Mb, serán asignadas como presentes en las especies, pero no las complementarias que serán ausentes. Por último, la matriz B3 será construida únicamente con las dietas estrictas, de Mb, como presentes en las especies, y las dietas complementarias y predominantes se considerarán ausentes. Para todas las matrices binarias, las dietas ausentes en Mb serán tomadas como ausentes también.
###########################################################

library(readxl)
library(dplyr)

setwd('C:/Users/Papra/Documents/Trabajo de Grado/Dietas')

# Llamado de la Tabla de dietas cruda
dietas <- read_xlsx('Tabla_dietas.xlsx',guess_max = 10000) #guess_max es para indicar el número máximo de filas de datos que se utilizarán para adivinar tipos de columnas (según R)


## Lista de los elementos de la columna "Reportes" que presenten palabras sepradas por un guión "-".
tmp1 <- strsplit(dietas$Reportes,"-")

## Data frame con los nombres de las especies y sus reportes.
datos <- as.data.frame(matrix(unlist(tmp1),ncol= 2,byrow = T))

names(datos) <- c("Especie","Reporte")

head(datos)

## Data frame de la información de cada trabajo revisado.
conteos <- as.data.frame(dietas[,2:length(dietas[1,])])

## Data frame que une los nombres de las especies y los reportes con la información de cada reporte por trabajo revisado.
tablaDietas <- cbind(datos,conteos)

## Dataframe con únicamente los reportes de dieta. Ya que en la Tabla de dietas cruda hay reportes de fuentes alimenticias como materia vegetal no identificada, polen y nectar-polen, y el número de reportes por tipo de evidencia, es necesario agrupar únicamente los reportes de las dietas Insectivoría, Carnivoría, Hematofagia, Frugivoría y Nectarivoría.
Mb <- tablaDietas[!tablaDietas$Reporte %in% c('obs','matVeg','contEst','heces','isótopos','nect','poli'),]

## Ya que no todas las especies cuentan con reportes, eliminamos las que no tienen aún.
Mb <- Mb[-which(is.na(Mb$'1')),]

## Para mejor manejo de los datos cambiamos los valores de NA como 0
Mb[is.na(Mb)]=0

## Organizamos los valores de las filas del data frame par que sean continuos
rownames(Mb) <- 1:nrow(Mb)

Mb_2 <- Mb

# Tabla complementaria 1: individuos. En esta tabla se mantendran los valores de individuos reportados para luego construir las demás matrices.



# Tabla complementaria 2: incidencias. Esta tabla considerará el número de individuos de los reportes como binarios, entonces las celdas con números de individuos serán cambiadas por 1, y las que tengan 0 o NA serán 0.

## Paso de reportes continuos a binarios
for (i in 3:length(Mb[1,])) {
  for (t in 1:length(Mb[,1])) {
    if (Mb_2[t,i]>0) {
      Mb_2[t,i]=1
    }
  }
}


## Matriz base - Mb. Este objeto, Mb_final, es un data frame con las mismas especies de Mb, los nombres de los reportes y 4 columnas vacias, Conteo, Sumatoria, Frecuencia y Estado, que serán llenadas más adelante.
Mb_final <- data.frame('Especie'= Mb$Especie,'Reporte'= Mb$Reporte, 'Conteo' = rep(0,length(Mb$Especie)),'Sumatoria'=rep(0,length(Mb$Especie)),'Frecuencia'=rep(0,length(Mb$Especie)),'Estado'=rep(0,length(Mb$Especie)))

## Número de reportes por dieta extraidos de Mb
for (i in 1:length(Mb$Especie)) {
  Mb_final[i,3]=sum(Mb_2[i,3:length(Mb[1,])],na.rm = T)
}

## Número de reportes totales por especie extraidos de Mb, para llenar la columna Sumatoria de Mb_final
sumatoria <- Mb_final %>% group_by(Especie) %>% summarize(Sumatoria=sum(Conteo)) # Ctrl + Shift + M = %>% 

## Ya que cada especie cuenta con 5 filas, en la sumatoria ubico el valor de esta para cada fila de la especie.
Mb_final$Sumatoria=rep(sumatoria$Sumatoria,each=5)

## Frecuencia de reporte de cada dieta. Ya que la Matriz base (Mb) es construida con base a la frecuenca de reportes, en este paso lo calculo para cada reporte de dieta.
for (i in 1:length(Mb_final$Especie)) {
  Mb_final[i,5] <-  round(Mb_final[i,3]/Mb_final[i,4],digits=3)
}

## Asignación de estados a las dietas considerando la regla mencionada al inicio del script.
for (i in 1:length(Mb_final$Especie)) { 
  if (Mb_final[i,5]<0.05) {
    Mb_final[i,6]=0
  } 
  else if (Mb_final[i,5]==0.05){
    Mb_final[i,6]=0
  }
  else if (Mb_final[i,5]>0.05&Mb_final[i,5]<0.5){
    Mb_final[i,6]=1
  }
  else if (Mb_final[i,5]==0.5){
    Mb_final[i,6]=1
  }
  else if (Mb_final[i,5]>0.5&Mb_final[i,5]<0.95){
    Mb_final[i,6]=2
  }
  else if (Mb_final[i,5]>0.95){
    Mb_final[i,6]=3
  }
  else if (Mb_final[i,5]==0.95){
    Mb_final[i,6]=3
  }
} 


## Mb_estados. En este data frame organizamos los estados de las dietas para cada especie según lo obtenido de las freuencias.
Mb_estados <- data.frame('Especie'=Mb_final$Especie,'Dieta'=Mb_final$Reporte,'Estados'=Mb_final$Estado)


## M1. En este paso es necesario asignar las dietas predominantes o estrictas para cada especie.
function(x){
  if (Mb_estados==0) {
    
  }
}





M1 <- data.frame('Especie'= unique(Mb_final$Especie),'Dieta'=rep(NA,length(unique(Mb_final$Especie,))))

#for (i in seq(1,length(Mb_final$Especie),5)) {
#  for (t in 1:length(unique(Mb_final$Especie,))) {
#    if (Mb_final[i,6]==2) {
#      M1[t,2] = 1
#    }
#    else if (Mb_final[i,6]==3) {
#      M1[t,2]=1
#    }
#    else if (Mb_final[i,6]==0) {
#      M1[t,2]=0
#    }
#    else if (Mb_final[i,6]==1) {
#      M1[t,2]=0
#    }
#  }
#  }






# Tabla complementaria 3: frcuencias. Para esta tabla se considerarán las frecuencias de los números de indiviuos reportados.





