#source("Script1_ArbolesYMatrices.Tesis.R")
#source("reconstrucciones_Mk_ER.Tesis.R")
#source("reconstrucciones_Mk_DR.Tesis.R")
#source("reconstrucciones_Parsimonia.Tesis.R")
#source("sensibilidad-0.Discretizaciones y tales.Tesis.R")
#source("sensibilidad-1.Codificaciones.Tesis.R")
#source("sensibilidad-2.Metodos.Tesis.R")
#source("sensibilidad-3.Transformaciones.Tesis.R")


############################################################ Gráficas y exportaciones 

## Codificaciones 

# Máxima verosimilitud - ER
## Recopilación de las comparaciones y cálculo de número de nodos relativos ####

ComparacionesMvER <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                  ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B2Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                  ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                  ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                  ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosCompartidos"=0,"NodosTotales"=0)

ComparacionesMvER$NodosCompartidos <- c(sum(as.numeric(compB1vsB2$B1Car.B2Car)),sum(as.numeric(compB2vsB3$B2Car.B3Car)),sum(as.numeric(compB1vsB3$B1Car.B3Car)),sum(as.numeric(compMultivsB1$MultiCar.B1Car)),sum(compMultivsB2$Carnivoria),sum(compMultivsB3$Carnivoria),sum(as.numeric(compMbvsB1$Car)), sum(compMbvsB2$Car), sum(compMbvsB3$Car),sum(compMbvsMulti$Car)
                                        ,sum(as.numeric(compB1vsB2$B1Fru.B2Fru)),sum(as.numeric(compB2vsB3$B2Fru.B3Fru)),sum(as.numeric(compB1vsB3$B1Fru.B3Fru)),sum(as.numeric(compMultivsB1$MultiFru.B1Fru)),sum(compMultivsB2$Frugivoria),sum(compMultivsB3$Frugivoria),sum(as.numeric(compMbvsB1$Fru)), sum(compMbvsB2$Fru), sum(compMbvsB3$Fru),sum(compMbvsMulti$Fru)
                                        , sum(as.numeric(compB1vsB2$B1Hem.B2Hem)),sum(as.numeric(compB2vsB3$B2Hem.B3Hem)),sum(as.numeric(compB1vsB3$B1Hem.B3Hem)),sum(as.numeric(compMultivsB1$MultiHem.B1Hem)),sum(compMultivsB2$Hematofagia),sum(compMultivsB3$Hematofagia),sum(as.numeric(compMbvsB1$Hem)), sum(compMbvsB2$Hem), sum(compMbvsB3$Hem),sum(compMbvsMulti$Hem)
                                        , sum(as.numeric(compB1vsB2$B1Ins.B2Ins)),sum(as.numeric(compB2vsB3$B2Ins.B3Ins)),sum(as.numeric(compB1vsB3$B1Ins.B3Ins)),sum(as.numeric(compMultivsB1$MultiIns.B1Ins)),sum(compMultivsB2$Insectivoria),sum(compMultivsB3$Insectivoria),sum(as.numeric(compMbvsB1$Ins)), sum(compMbvsB2$Ins), sum(compMbvsB3$Ins),sum(compMbvsMulti$Ins)
                                        ,sum(as.numeric(compB1vsB2$B1Nec.B2Nec)),sum(as.numeric(compB2vsB3$B2Nec.B3Nec)),sum(as.numeric(compB1vsB3$B1Nec.B3Nec)),sum(as.numeric(compMultivsB1$MultiNec.B1Nec)),sum(compMultivsB2$Nectarivoria),sum(compMultivsB3$Nectarivoria),sum(as.numeric(compMbvsB1$Nec)), sum(compMbvsB2$Nec), sum(compMbvsB3$Nec),sum(compMbvsMulti$Nec))



ComparacionesMvER$NodosTotales <- c(max(NodosTotales$B1Car,NodosTotales$B2Car), max(NodosTotales$B2Car,NodosTotales$B3Car), max(NodosTotales$B1Car,NodosTotales$B3Car), max(NodosTotales$B1Car,sum(mpMulti$Carnivoria)), max(NodosTotales$B2Car,sum(mpMulti$Carnivoria)), max(NodosTotales$B3Car,sum(mpMulti$Carnivoria)), max(NodosTotales$B1Car,sum(mpBaseBin$Car.EPC)),max(NodosTotales$B2Car,sum(mpBaseBin$Car.P)), max(NodosTotales$B3Car,sum(mpBaseBin$Car.E)),max(sum(mpMulti$Carnivoria),sum(mpBaseBin$Car.EPC))
                                    ,max(NodosTotales$B1Fru,NodosTotales$B2Fru),max(NodosTotales$B2Fru,NodosTotales$B3Fru),max(NodosTotales$B1Fru,NodosTotales$B3Fru),max(NodosTotales$B1Fru,sum(mpMulti$Frugivoria)),max(NodosTotales$B2Fru,sum(mpMulti$Frugivoria)),max(NodosTotales$B3Fru,sum(mpMulti$Frugivoria)),max(NodosTotales$B1Fru,sum(mpBaseBin$Fru.EPC)), max(NodosTotales$B2Fru,sum(mpBaseBin$Fru.EP)), max(NodosTotales$B3Fru,sum(mpBaseBin$Fru.E)),max(sum(mpMulti$Frugivoria),sum(mpBaseBin$Fru.EPC))
                                    ,max(NodosTotales$B1Hem,NodosTotales$B2Hem),max(NodosTotales$B2Hem,NodosTotales$B3Hem),max(NodosTotales$B1Hem,NodosTotales$B3Hem),max(NodosTotales$B1Hem,sum(mpMulti$Hematofagia)),max(NodosTotales$B2Hem,sum(mpMulti$Hematofagia)),max(NodosTotales$B3Hem,sum(mpMulti$Hematofagia)),max(NodosTotales$B1Hem,sum(mpBaseBin$Hem.CP)), max(NodosTotales$B2Hem,sum(mpBaseBin$Hem.P)), max(NodosTotales$B3Hem,sum(mpBaseBin$Hem.E)),max(sum(mpMulti$Hematofagia),sum(mpBaseBin$Hem.CP))
                                    ,max(NodosTotales$B1Ins,NodosTotales$B2Ins),max(NodosTotales$B2Ins,NodosTotales$B3Ins),max(NodosTotales$B1Ins,NodosTotales$B3Ins),max(NodosTotales$B1Ins,sum(mpMulti$Insectivoria)),max(NodosTotales$B2Ins,sum(mpMulti$Insectivoria)),max(NodosTotales$B3Ins,sum(mpMulti$Insectivoria)),max(NodosTotales$B1Ins,sum(mpBaseBin$Ins.EPC)), max(NodosTotales$B2Ins,sum(mpBaseBin$Ins.EP)), max(NodosTotales$B3Ins,sum(mpBaseBin$Ins.E)),max(sum(mpMulti$Insectivoria),sum(mpBaseBin$Ins.EPC))
                                    ,max(NodosTotales$B1Nec,NodosTotales$B2Nec),max(NodosTotales$B2Nec,NodosTotales$B3Nec),max(NodosTotales$B1Nec,NodosTotales$B3Nec),max(NodosTotales$B1Nec,sum(mpMulti$Nectarivoria)),max(NodosTotales$B2Nec,sum(mpMulti$Nectarivoria)),max(NodosTotales$B3Nec,sum(mpMulti$Nectarivoria)),max(NodosTotales$B1Nec,sum(mpBaseBin$Nec.EPC)), max(NodosTotales$B2Nec,sum(mpBaseBin$Nec.EP)), max(NodosTotales$B3Nec,sum(mpBaseBin$Nec.E)),max(sum(mpMulti$Nectarivoria),sum(mpBaseBin$Nec.EPC)))

ComparacionesMvER$Nodos = ComparacionesMvER$NodosCompartidos/ComparacionesMvER$NodosTotales

ComparacionesMvER[is.na(ComparacionesMvER)] <- 0

ComparacionesMvER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))

ComparacionesMvER$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)

exportar <- data.frame("Comparaciones"=ComparacionesMvER$Matrices,"Dietas"=ComparacionesMvER$Dieta,"Número-nodos-relativos"=ComparacionesMvER$Nodos)

write.csv(exportar,file="ComparacionesMvER.csv")

### Gráfica de puntos ----
pdf(file="comparacionesMvER.pdf",width = 11)
ggplot(data=ComparacionesMvER, aes(x=Matrices,Nodos, fill=Dieta)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
   scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Sensibilidad codificaciones - MV") +
  ylab("Nodos relativos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
 
  title = element_text(size = 17), 
  legend.title = element_text(size = 17),
  legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
  panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
  panel.grid.minor.y = element_blank()
)
dev.off()



#### Multiestado ----
probMulti <- data.frame("Carnivoria"=mpMulti$Car,"Frugivoria"=mpMulti$Fru,"Hematofagia"=mpMulti$Hem,"Insectivoria"=mpMulti$Ins,"Nectarivoria"=mpMulti$Nec)

number <- 1:length(mpMulti$Car)

probMulti$Nodo <- number

probMulti <- as.data.frame(melt(probMulti,id.vars ="Nodo" ))

pdf(file='prob.multi.pdf')

ggplot(data=probMulti, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
#+
 # theme(axis.text = element_blank(),                         axis.ticks = element_blank(),
  #panel.grid  = element_blank())

dev.off()


#### Matriz base ----
##### Carnivoria
probMbCar <- data.frame("Ausente"=datos.reaMkErMb.car[,1],"Complementario"=datos.reaMkErMb.car[,2],"Predominante"=datos.reaMkErMb.car[,3])
probMbCar$Nodo <- number

probMbCar <- as.data.frame(melt(probMbCar,id.vars ="Nodo" ))

pdf(file='probMbCar.bar.pdf')
ggplot(data=probMbCar, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probMbFru <- data.frame("Ausente"=datos.reaMkErMb.fru[,1],"Complementario"=datos.reaMkErMb.fru[,2],"Predominante"=datos.reaMkErMb.fru[,3],"Estricto"=datos.reaMkErMb.fru[,4])
probMbFru$Nodo <- number

probMbFru <- as.data.frame(melt(probMbFru,id.vars ="Nodo" ))

pdf(file='probMbFru.bar.pdf')
ggplot(data=probMbFru, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHem <- data.frame("Ausente"=datos.reaMkErMb.hem[,1],"Complementario"=datos.reaMkErMb.hem[,2],"Predominante"=datos.reaMkErMb.hem[,3])
probMbHem$Nodo <- number

probMbHem <- as.data.frame(melt(probMbHem,id.vars ="Nodo" ))

pdf(file='probMbHem.bar.pdf')
ggplot(data=probMbHem, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick","firebrick3"))
dev.off()


##### Insectivoria
probMbIns <- data.frame("Ausente"=datos.reaMkErMb.ins[,1],"Complementario"=datos.reaMkErMb.ins[,2],"Predominante"=datos.reaMkErMb.ins[,3],"Estricto"=datos.reaMkErMb.ins[,4])
probMbIns$Nodo <- number

probMbIns <- as.data.frame(melt(probMbIns,id.vars ="Nodo" ))

pdf(file='probMbIns.bar.pdf')
ggplot(data=probMbIns, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNec <- data.frame("Ausente"=datos.reaMkErMb.nec[,1],"Complementario"=datos.reaMkErMb.nec[,2],"Predominante"=datos.reaMkErMb.nec[,3],"Estricto"=datos.reaMkErMb.nec[,4])
probMbNec$Nodo <- number

probMbNec <- as.data.frame(melt(probMbNec,id.vars ="Nodo" ))

pdf(file='probMbNec.bar.pdf')
ggplot(data=probMbNec, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



#### Matriz binaria 1 ----
##### Carnivoria
probB1Carw <- data.frame("Ausente"=datos.reaMkERB1.car[,1],"Presente"=datos.reaMkERB1.car[,2])
probB1Car$Nodo <- number

probB1Car <- as.data.frame(melt(probB1Car,id.vars ="Nodo" ))

pdf(file='probB1Car.bar.pdf')
ggplot(data=probB1Car, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB1Fru <- data.frame("Ausente"=datos.reaMkERB1.fru[,1],"Presente"=datos.reaMkERB1.fru[,2])
probB1Fru$Nodo <- number

probB1Fru <- as.data.frame(melt(probB1Fru,id.vars ="Nodo" ))

pdf(file='probB1Fru.bar.pdf')
ggplot(data=probB1Fru, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB1Hem <- data.frame("Ausente"=datos.reaMkERB1.hem[,1],"Presente"=datos.reaMkERB1.hem[,2])
probB1Hem$Nodo <- number

probB1Hem <- as.data.frame(melt(probB1Hem,id.vars ="Nodo" ))

pdf(file='probB1Hem.bar.pdf')
ggplot(data=probB1Hem, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB1Ins <- data.frame("Ausente"=datos.reaMkERB1.ins[,1],"Presente"=datos.reaMkERB1.ins[,2])
probB1Ins$Nodo <- number

probB1Ins <- as.data.frame(melt(probB1Ins,id.vars ="Nodo" ))

pdf(file='probB1Ins.bar.pdf')
ggplot(data=probB1Ins, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB1Nec <- data.frame("Ausente"=datos.reaMkERB1.nec[,1],"Presente"=datos.reaMkERB1.nec[,2])
probB1Nec$Nodo <- number

probB1Nec <- as.data.frame(melt(probB1Nec,id.vars ="Nodo" ))

pdf(file='probB1Nec.bar.pdf')
ggplot(data=probB1Nec, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()




#### Matriz binaria 2 ----
##### Carnivoria
probB2Car <- data.frame("Ausente"=datos.reaMkERB2.car[,1],"Presente"=datos.reaMkERB2.car[,2])
probB2Car$Nodo <- number

probB2Car <- as.data.frame(melt(probB2Car,id.vars ="Nodo" ))

pdf(file='probB2Car.bar.pdf')
ggplot(data=probB2Car, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB2Fru <- data.frame("Ausente"=datos.reaMkERB2.fru[,1],"Presente"=datos.reaMkERB2.fru[,2])
probB2Fru$Nodo <- number

probB2Fru <- as.data.frame(melt(probB2Fru,id.vars ="Nodo" ))

pdf(file='probB2Fru.bar.pdf')
ggplot(data=probB2Fru, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB2Hem <- data.frame("Ausente"=datos.reaMkERB2.hem[,1],"Presente"=datos.reaMkERB2.hem[,2])
probB2Hem$Nodo <- number

probB2Hem <- as.data.frame(melt(probB2Hem,id.vars ="Nodo" ))

pdf(file='probB2Hem.bar.pdf')
ggplot(data=probB2Hem, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB2Ins <- data.frame("Ausente"=datos.reaMkERB2.ins[,1],"Presente"=datos.reaMkERB2.ins[,2])
probB2Ins$Nodo <- number

probB2Ins <- as.data.frame(melt(probB2Ins,id.vars ="Nodo" ))

pdf(file='probB2Ins.bar.pdf')
ggplot(data=probB2Ins, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB2Nec <- data.frame("Ausente"=datos.reaMkERB2.nec[,1],"Presente"=datos.reaMkERB2.nec[,2])
probB2Nec$Nodo <- number

probB2Nec <- as.data.frame(melt(probB2Nec,id.vars ="Nodo" ))

pdf(file='probB2Nec.bar.pdf')
ggplot(data=probB2Nec, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()



#### Matriz binaria 3 ----
##### Carnivoria. No hayyyyy
#probB3Car <- data.frame("Ausente"=datos.reaMkERB3.car[,1],"Presente"=datos.reaMkERB3.car[,2])
probB3Car$Nodo <- number

probB3Car <- as.data.frame(melt(probB3Car,id.vars ="Nodo" ))

#pdf(file='probB3Car.bar.pdf')
#ggplot(data=probB3Car, aes(x=Nodo,value, fill=variable)) +
geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))

#dev.off()

##### Frugivoria
probB3Fru <- data.frame("Ausente"=datos.reaMkERB3.fru[,1],"Presente"=datos.reaMkERB3.fru[,2])
probB3Fru$Nodo <- number

probB3Fru <- as.data.frame(melt(probB3Fru,id.vars ="Nodo" ))

pdf(file='probB3Fru.bar.pdf')
ggplot(data=probB3Fru, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()


##### Hematofagia
probB3Hem <- data.frame("Ausente"=datos.reaMkERB3.hem[,1],"Presente"=datos.reaMkERB3.hem[,2])
probB3Hem$Nodo <- number

probB3Hem <- as.data.frame(melt(probB3Hem,id.vars ="Nodo" ))

pdf(file='probB3Hem.bar.pdf')
ggplot(data=probB3Hem, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))

dev.off()

##### Insectivoria
probB3Ins <- data.frame("Ausente"=datos.reaMkERB3.ins[,1],"Presente"=datos.reaMkERB3.ins[,2])
probB3Ins$Nodo <- number

probB3Ins <- as.data.frame(melt(probB3Ins,id.vars ="Nodo" ))

pdf(file='probB3Ins.bar.pdf')
ggplot(data=probB3Ins, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))

dev.off()

##### Nectarivoria
probB3Nec <- data.frame("Ausente"=datos.reaMkERB3.nec[,1],"Presente"=datos.reaMkERB3.nec[,2])
probB3Nec$Nodo <- number

probB3Nec <- as.data.frame(melt(probB3Nec,id.vars ="Nodo" ))

pdf(file='probB3Nec.bar.pdf')
ggplot(data=probB3Nec, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))

dev.off()


# Máximas probabilidades ----
mpGrafica2 <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0,"NodosTotales"=44,"NodosRelativos"=0)



mpGrafica2$Suma <- c(sum(mpMulti$Carnivoria),sum(mpMulti$Frugivoria),sum(mpMulti$Hematofagia),sum(mpMulti$Insectivoria),sum(mpMulti$Nectarivoria),sum(mpBase$Car==0),sum(mpBase$Car==1),sum(mpBase$Car==2),sum(mpBase$Car==3),sum(mpBase$Fru==0),sum(mpBase$Fru==1),sum(mpBase$Fru==2),sum(mpBase$Fru==3),sum(mpBase$Hem==0),sum(mpBase$Hem==1),sum(mpBase$Hem==2),sum(mpBase$Hem==3),sum(mpBase$Ins==0),sum(mpBase$Ins==1),sum(mpBase$Ins==2),sum(mpBase$Ins==3),sum(mpBase$Nec==0),sum(mpBase$Nec==1),sum(mpBase$Nec==2),sum(mpBase$Nec==3),sum(mp1$Car==0),sum(mp1$Car==1),sum(mp1$Fru==0),sum(mp1$Fru==1),sum(mp1$Hem==0),sum(mp1$Hem==1),sum(mp1$Ins==0),sum(mp1$Ins==1),sum(mp1$Nec==0),sum(mp1$Nec==1),sum(mp2$Car==0),sum(mp2$Car==1),sum(mp2$Fru==0),sum(mp2$Fru==1),sum(mp2$Hem==0),sum(mp2$Hem==1),sum(mp2$Ins==0),sum(mp2$Ins==1),sum(mp2$Nec==0),sum(mp2$Nec==1),sum(mp3$Car==0),sum(mp3$Car==1),sum(mp3$Fru==0),sum(mp3$Fru==1),sum(mp3$Hem==0),sum(mp3$Hem==1),sum(mp3$Ins==0),sum(mp3$Ins==1),sum(mp3$Nec==0),sum(mp3$Nec==1))

mpGrafica2$NodosRelativos <- mpGrafica2$Suma/mpGrafica2$NodosTotales

estadosMvER <- data.frame("Codificación"=mpGrafica2$Reconstrucción,"Estado"=mpGrafica2$Estado,"Suma"=mpGrafica2$Suma)

write.csv(estadosMvER,file="estadosMvER.csv")


#----



# Máxima verosimilitud - DR1

# Recopilación de las comparaciones y cálculo de número de nodos relativos - Mv DR1 ----
ComparacionesMvAsim1 <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                     ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B2Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                     ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                     ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                     ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosCompartidos"=0,"NodosTotales"=0)

ComparacionesMvAsim1$NodosCompartidos <- c(sum(as.numeric(compB1vsB2Asim1$B1Car.B2Car)),sum(as.numeric(compB2vsB3Asim1$B2Car.B3Car)),sum(as.numeric(compB1vsB3Asim1$B1Car.B3Car)),sum(as.numeric(compMultivsB1Asim1$MultiCar.B1Car)),sum(compMultivsB2Asim1$Carnivoria),sum(compMultivsB3Asim1$Carnivoria),sum(as.numeric(compMbvsB1Asim1$Car)), sum(compMbvsB2Asim1$Car), sum(compMbvsB3Asim1$Car),sum(compMbvsMultiAsim1$Car)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim1$B1Fru.B2Fru)), sum(as.numeric(compB2vsB3Asim1$B2Fru.B3Fru)),sum(as.numeric(compB1vsB3Asim1$B1Fru.B3Fru)),sum(as.numeric(compMultivsB1Asim1$MultiFru.B1Fru)),sum(compMultivsB2Asim1$Frugivoria),sum(compMultivsB3Asim1$Frugivoria),sum(as.numeric(compMbvsB1Asim1$Fru)), sum(compMbvsB2Asim1$Fru), sum(compMbvsB3Asim1$Fru),sum(compMbvsMultiAsim1$Fru)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim1$B1Hem.B2Hem)),sum(as.numeric(compB2vsB3Asim1$B2Hem.B3Hem)),sum(as.numeric(compB1vsB3Asim1$B1Hem.B3Hem)),sum(as.numeric(compMultivsB1Asim1$MultiHem.B1Hem)),sum(compMultivsB2Asim1$Hematofagia),sum(compMultivsB3Asim1$Hematofagia),sum(as.numeric(compMbvsB1Asim1$Hem)), sum(compMbvsB2Asim1$Hem), sum(compMbvsB3Asim1$Hem),sum(compMbvsMultiAsim1$Hem)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim1$B1Ins.B2Ins)),sum(as.numeric(compB2vsB3Asim1$B2Ins.B3Ins)),sum(as.numeric(compB1vsB3Asim1$B1Ins.B3Ins)),sum(as.numeric(compMultivsB1Asim1$MultiIns.B1Ins)),sum(compMultivsB2Asim1$Insectivoria),sum(compMultivsB3Asim1$Insectivoria),sum(as.numeric(compMbvsB1Asim1$Ins)), sum(compMbvsB2Asim1$Ins), sum(compMbvsB3Asim1$Ins),sum(compMbvsMultiAsim1$Ins)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim1$B1Nec.B2Nec)),sum(as.numeric(compB2vsB3Asim1$B2Nec.B3Nec)),sum(as.numeric(compB1vsB3Asim1$B1Nec.B3Nec)),sum(as.numeric(compMultivsB1Asim1$MultiNec.B1Nec)),sum(compMultivsB2Asim1$Nectarivoria),sum(compMultivsB3Asim1$Nectarivoria),sum(as.numeric(compMbvsB1Asim1$Nec)), sum(compMbvsB2Asim1$Nec), sum(compMbvsB3Asim1$Nec),sum(compMbvsMultiAsim1$Nec))



ComparacionesMvAsim1$NodosTotales <- c(max(NodosTotalesAsim1$B1Car,NodosTotalesAsim1$B2Car), max(NodosTotalesAsim1$B2Car,NodosTotalesAsim1$B3Car), max(NodosTotalesAsim1$B1Car,NodosTotalesAsim1$B3Car), max(NodosTotalesAsim1$B1Car,sum(mpMultiAsim1$Carnivoria)),max(NodosTotalesAsim1$B2Car,sum(mpMultiAsim1$Carnivoria)), max(NodosTotalesAsim1$B3Car,sum(mpMultiAsim1$Carnivoria)),max(NodosTotalesAsim1$B1Car,sum(mpBaseBinAsim1$Car.CP)),max(NodosTotalesAsim1$B2Car,sum(mpBaseBinAsim1$Car.P)),max(NodosTotalesAsim1$B3Car,sum(mpBaseBinAsim1$Car.E)),max(sum(mpMultiAsim1$Carnivoria),sum(mpBaseBinAsim1$Car.CP))
                                     
                                     ,max(NodosTotalesAsim1$B1Fru,NodosTotalesAsim1$B2Fru),max(NodosTotalesAsim1$B2Fru,NodosTotalesAsim1$B3Fru),max(NodosTotalesAsim1$B1Fru,NodosTotalesAsim1$B3Fru),max(NodosTotalesAsim1$B1Fru,sum(mpMultiAsim1$Frugivoria)),max(NodosTotalesAsim1$B2Fru,sum(mpMultiAsim1$Frugivoria)),max(NodosTotalesAsim1$B3Fru,sum(mpMultiAsim1$Frugivoria)),max(NodosTotalesAsim1$B1Fru,sum(mpBaseBinAsim1$Fru.ECP)), max(NodosTotalesAsim1$B2Fru,sum(mpBaseBinAsim1$Fru.EP)), max(NodosTotalesAsim1$B3Fru,sum(mpBaseBinAsim1$Fru.E)),max(sum(mpMultiAsim1$Frugivoria),sum(mpBaseBinAsim1$Fru.ECP))
                                     
                                     ,max(NodosTotalesAsim1$B1Hem,NodosTotalesAsim1$B2Hem),max(NodosTotalesAsim1$B2Hem,NodosTotalesAsim1$B3Hem),max(NodosTotalesAsim1$B1Hem,NodosTotalesAsim1$B3Hem),max(NodosTotalesAsim1$B1Hem,sum(mpMultiAsim1$Hematofagia)),max(NodosTotalesAsim1$B2Hem,sum(mpMultiAsim1$Hematofagia)),max(NodosTotalesAsim1$B3Hem,sum(mpMultiAsim1$Hematofagia)),max(NodosTotalesAsim1$B1Hem,sum(mpBaseBinAsim1$Hem.CP)), max(NodosTotalesAsim1$B2Hem,sum(mpBaseBinAsim1$Hem.P)), max(NodosTotalesAsim1$B3Hem,sum(mpBaseBinAsim1$Hem.E)),max(sum(mpMultiAsim1$Hematofagia),sum(mpBaseBinAsim1$Hem.CP))
                                     
                                     ,max(NodosTotalesAsim1$B1Ins,NodosTotalesAsim1$B2Ins),max(NodosTotalesAsim1$B2Ins,NodosTotalesAsim1$B3Ins),max(NodosTotalesAsim1$B1Ins,NodosTotalesAsim1$B3Ins),max(NodosTotalesAsim1$B1Ins,sum(mpMultiAsim1$Insectivoria)),max(NodosTotalesAsim1$B2Ins,sum(mpMultiAsim1$Insectivoria)),max(NodosTotalesAsim1$B3Ins,sum(mpMultiAsim1$Insectivoria)),max(NodosTotalesAsim1$B1Ins,sum(mpBaseBinAsim1$Ins.ECP)), max(NodosTotalesAsim1$B2Ins,sum(mpBaseBinAsim1$Ins.EP)), max(NodosTotalesAsim1$B3Ins,sum(mpBaseBinAsim1$Ins.E)),max(sum(mpMultiAsim1$Insectivoria),sum(mpBaseBinAsim1$Ins.ECP))
                                     
                                     ,max(NodosTotalesAsim1$B1Nec,NodosTotalesAsim1$B2Nec),max(NodosTotalesAsim1$B2Nec,NodosTotalesAsim1$B3Nec),max(NodosTotalesAsim1$B1Nec,NodosTotalesAsim1$B3Nec),max(NodosTotalesAsim1$B1Nec,sum(mpMultiAsim1$Nectarivoria)),max(NodosTotalesAsim1$B2Nec,sum(mpMultiAsim1$Nectarivoria)),max(NodosTotalesAsim1$B3Nec,sum(mpMultiAsim1$Nectarivoria)),max(NodosTotalesAsim1$B1Nec,sum(mpBaseBinAsim1$Nec.ECP)), max(NodosTotalesAsim1$B2Nec,sum(mpBaseBinAsim1$Nec.EP)), max(NodosTotalesAsim1$B3Nec,sum(mpBaseBinAsim1$Nec.E)),max(sum(mpMultiAsim1$Nectarivoria),sum(mpBaseBinAsim1$Nec.ECP)))


ComparacionesMvAsim1$Nodos = ComparacionesMvAsim1$NodosCompartidos/ComparacionesMvAsim1$NodosTotales

ComparacionesMvAsim1[is.na(ComparacionesMvAsim1)] <- 0

ComparacionesMvAsim1$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))

ComparacionesMvAsim1$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)

exportt1 <- data.frame("Comparaciones"=ComparacionesMvAsim1$Matrices,"Dietas"=ComparacionesMvAsim1$Dieta,"Nnr"=ComparacionesMvAsim1$Nodos)

write.csv(exportt1,file="ComparacionesMvAsim1.csv")

### Gráfica de puntos ----
pdf(file="comparacionesMvAsim2vsER.pdf",width = 11)
ggplot(data=ComparacionesAsim2vsER, aes(x=Matrices,Nodos, fill=Dieta)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Comparaciones MV - Asimétrica 1") +
  ylab("Número relativo de nodos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
        
        title = element_text(size = 17), 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
          panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
          panel.grid.minor.y = element_blank()
        )
dev.off()



#### Multiestado ----
probMultiAsim1 <- data.frame("Carnivoria"=mpMultiAsim1$Car,"Frugivoria"=mpMultiAsim1$Fru,"Hematofagia"=mpMultiAsim1$Hem,"Insectivoria"=mpMultiAsim1$Ins,"Nectarivoria"=mpMultiAsim1$Nec)

number <- 1:length(mpMultiAsim1$Car)

probMultiAsim1$Nodo <- number

probMultiAsim1 <- as.data.frame(melt(probMultiAsim1,id.vars ="Nodo" ))

pdf(file='prob.multiAsim1.pdf')

ggplot(data=probMultiAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
#+
# theme(axis.text = element_blank(),                         axis.ticks = element_blank(),
#panel.grid  = element_blank())

dev.off()


#### Matriz base ----
##### Carnivoria
probMbCarAsim1 <- data.frame("Ausente"=datos.reaMkDrMb.car[,1],"Complementario"=datos.reaMkDrMb.car[,2],"Predominante"=datos.reaMkDrMb.car[,3])
probMbCarAsim1$Nodo <- number

probMbCarAsim1 <- as.data.frame(melt(probMbCarAsim1,id.vars ="Nodo" ))

pdf(file='probMbCarAsim1.bar.pdf')
ggplot(data=probMbCarAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probMbFruAsim1 <- data.frame("Ausente"=datos.reaMkDrMb.fru[,1],"Complementario"=datos.reaMkDrMb.fru[,2],"Predominante"=datos.reaMkDrMb.fru[,3],"Estricto"=datos.reaMkDrMb.fru[,4])
probMbFruAsim1$Nodo <- number

probMbFruAsim1 <- as.data.frame(melt(probMbFruAsim1,id.vars ="Nodo" ))

pdf(file='probMbFruAsim1.bar.pdf')
ggplot(data=probMbFruAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHemAsim1 <- data.frame("Ausente"=datos.reaMkDrMb.hem[,1],"Complementario"=datos.reaMkDrMb.hem[,2],"Predominante"=datos.reaMkDrMb.hem[,3])
probMbHemAsim1$Nodo <- number

probMbHemAsim1 <- as.data.frame(melt(probMbHemAsim1,id.vars ="Nodo" ))

pdf(file='probMbHemAsim1.bar.pdf')
ggplot(data=probMbHemAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick","firebrick3"))
dev.off()


##### Insectivoria
probMbInsAsim1 <- data.frame("Ausente"=datos.reaMkDrMb.ins[,1],"Complementario"=datos.reaMkDrMb.ins[,2],"Predominante"=datos.reaMkDrMb.ins[,3],"Estricto"=datos.reaMkDrMb.ins[,4])
probMbInsAsim1$Nodo <- number

probMbInsAsim1 <- as.data.frame(melt(probMbInsAsim1,id.vars ="Nodo" ))

pdf(file='probMbInsAsim1.bar.pdf')
ggplot(data=probMbInsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNecAsim1 <- data.frame("Ausente"=datos.reaMkDrMb.nec[,1],"Complementario"=datos.reaMkDrMb.nec[,2],"Predominante"=datos.reaMkDrMb.nec[,3],"Estricto"=datos.reaMkDrMb.nec[,4])
probMbNecAsim1$Nodo <- number

probMbNecAsim1 <- as.data.frame(melt(probMbNecAsim1,id.vars ="Nodo" ))

pdf(file='probMbNecAsim1.bar.pdf')
ggplot(data=probMbNecAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



#### Matriz binaria 1 ----
##### Carnivoria
probB1CarAsim1 <- data.frame("Ausente"=datos.reaMkDRB1.car[,1],"Presente"=datos.reaMkDRB1.car[,2])
probB1CarAsim1$Nodo <- number

probB1CarAsim1 <- as.data.frame(melt(probB1CarAsim1,id.vars ="Nodo" ))

pdf(file='probB1CarAsim1.bar.pdf')
ggplot(data=probB1CarAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB1FruAsim1 <- data.frame("Ausente"=datos.reaMkDRB1.fru[,1],"Presente"=datos.reaMkDRB1.fru[,2])
probB1FruAsim1$Nodo <- number

probB1FruAsim1 <- as.data.frame(melt(probB1FruAsim1,id.vars ="Nodo" ))

pdf(file='probB1FruAsim1.bar.pdf')
ggplot(data=probB1FruAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB1HemAsim1 <- data.frame("Ausente"=datos.reaMkDRB1.hem[,1],"Presente"=datos.reaMkDRB1.hem[,2])
probB1HemAsim1$Nodo <- number

probB1HemAsim1 <- as.data.frame(melt(probB1HemAsim1,id.vars ="Nodo" ))

pdf(file='probB1HemAsim1.bar.pdf')
ggplot(data=probB1HemAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB1InsAsim1 <- data.frame("Ausente"=datos.reaMkDRB1.ins[,1],"Presente"=datos.reaMkDRB1.ins[,2])
probB1InsAsim1$Nodo <- number

probB1InsAsim1 <- as.data.frame(melt(probB1InsAsim1,id.vars ="Nodo" ))

pdf(file='probB1InsAsim1.bar.pdf')
ggplot(data=probB1InsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB1NecAsim1 <- data.frame("Ausente"=datos.reaMkDRB1.nec[,1],"Presente"=datos.reaMkDRB1.nec[,2])
probB1NecAsim1$Nodo <- number

probB1NecAsim1 <- as.data.frame(melt(probB1NecAsim1,id.vars ="Nodo" ))

pdf(file='probB1NecAsim1.bar.pdf')
ggplot(data=probB1NecAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()




#### Matriz binaria 2 ----
##### Carnivoria
probB2CarAsim1 <- data.frame("Ausente"=datos.reaMkDRB2.car[,1],"Presente"=datos.reaMkDRB2.car[,2])
probB2CarAsim1$Nodo <- number

probB2CarAsim1 <- as.data.frame(melt(probB2CarAsim1,id.vars ="Nodo" ))

pdf(file='probB2CarAsim1.bar.pdf')
ggplot(data=probB2CarAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB2FruAsim1 <- data.frame("Ausente"=datos.reaMkDRB2.fru[,1],"Presente"=datos.reaMkDRB2.fru[,2])
probB2FruAsim1$Nodo <- number

probB2FruAsim1 <- as.data.frame(melt(probB2FruAsim1,id.vars ="Nodo" ))

pdf(file='probB2FruAsim1.bar.pdf')
ggplot(data=probB2FruAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB2HemAsim1 <- data.frame("Ausente"=datos.reaMkDRB2.hem[,1],"Presente"=datos.reaMkDRB2.hem[,2])
probB2HemAsim1$Nodo <- number

probB2HemAsim1 <- as.data.frame(melt(probB2HemAsim1,id.vars ="Nodo" ))

pdf(file='probB2HemAsim1.bar.pdf')
ggplot(data=probB2HemAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB2InsAsim1 <- data.frame("Ausente"=datos.reaMkDRB2.ins[,1],"Presente"=datos.reaMkDRB2.ins[,2])
probB2InsAsim1$Nodo <- number

probB2InsAsim1 <- as.data.frame(melt(probB2InsAsim1,id.vars ="Nodo" ))

pdf(file='probB2InsAsim1.bar.pdf')
ggplot(data=probB2InsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB2NecAsim1 <- data.frame("Ausente"=datos.reaMkDRB2.nec[,1],"Presente"=datos.reaMkDRB2.nec[,2])
probB2NecAsim1$Nodo <- number

probB2NecAsim1 <- as.data.frame(melt(probB2NecAsim1,id.vars ="Nodo" ))

pdf(file='probB2NecAsim1.bar.pdf')
ggplot(data=probB2NecAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()



#### Matriz binaria 3 ----
##### Carnivoria. 
probB3CarAsim1$Nodo <- number

probB3CarAsim1 <- as.data.frame(melt(probB3CarAsim1,id.vars ="Nodo" ))

#pdf(file='probB3Car.bar.pdf')
#ggplot(data=probB3Car, aes(x=Nodo,value, fill=variable)) +
geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))

#dev.off()

##### Frugivoria
probB3FruAsim1 <- data.frame("Ausente"=datos.reaMkDRB3.fru[,1],"Presente"=datos.reaMkDRB3.fru[,2])
probB3FruAsim1$Nodo <- number

probB3FruAsim1 <- as.data.frame(melt(probB3FruAsim1,id.vars ="Nodo" ))

pdf(file='probB3FruAsim1.bar.pdf')
ggplot(data=probB3FruAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()


##### Hematofagia
probB3HemAsim1 <- data.frame("Ausente"=datos.reaMkDRB3.hem[,1],"Presente"=datos.reaMkDRB3.hem[,2])
probB3HemAsim1$Nodo <- number

probB3HemAsim1 <- as.data.frame(melt(probB3HemAsim1,id.vars ="Nodo" ))

pdf(file='probB3HemAsim1.bar.pdf')
ggplot(data=probB3HemAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))

dev.off()

##### Insectivoria
probB3InsAsim1 <- data.frame("Ausente"=datos.reaMkDRB3.ins[,1],"Presente"=datos.reaMkDRB3.ins[,2])
probB3InsAsim1$Nodo <- number

probB3InsAsim1 <- as.data.frame(melt(probB3InsAsim1,id.vars ="Nodo" ))

pdf(file='probB3InsAsim1.bar.pdf')
ggplot(data=probB3InsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))

dev.off()

##### Nectarivoria
probB3NecAsim1 <- data.frame("Ausente"=datos.reaMkDRB3.nec[,1],"Presente"=datos.reaMkDRB3.nec[,2])
probB3NecAsim1$Nodo <- number

probB3NecAsim1 <- as.data.frame(melt(probB3NecAsim1,id.vars ="Nodo" ))

pdf(file='probB3NecAsim1.bar.pdf')
ggplot(data=probB3NecAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))

dev.off()

# Máximas probabilidades ----
mpGrafica2Asim1 <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0,"NodosTotales"=44,"NodosRelativos"=0)



mpGrafica2Asim1$Suma <- c(sum(mpMultiAsim1$Carnivoria),sum(mpMultiAsim1$Frugivoria),sum(mpMultiAsim1$Hematofagia),sum(mpMultiAsim1$Insectivoria),sum(mpMultiAsim1$Nectarivoria),sum(mpBaseAsim1$Car==0),sum(mpBaseAsim1$Car==1),sum(mpBaseAsim1$Car==2),sum(mpBaseAsim1$Car==3),sum(mpBaseAsim1$Fru==0),sum(mpBaseAsim1$Fru==1),sum(mpBaseAsim1$Fru==2),sum(mpBaseAsim1$Fru==3),sum(mpBaseAsim1$Hem==0),sum(mpBaseAsim1$Hem==1),sum(mpBaseAsim1$Hem==2),sum(mpBaseAsim1$Hem==3),sum(mpBaseAsim1$Ins==0),sum(mpBaseAsim1$Ins==1),sum(mpBaseAsim1$Ins==2),sum(mpBaseAsim1$Ins==3),sum(mpBaseAsim1$Nec==0),sum(mpBaseAsim1$Nec==1),sum(mpBaseAsim1$Nec==2),sum(mpBaseAsim1$Nec==3),sum(mp1Asim1$Car==0),sum(mp1Asim1$Car==1),sum(mp1Asim1$Fru==0),sum(mp1Asim1$Fru==1),sum(mp1Asim1$Hem==0),sum(mp1Asim1$Hem==1),sum(mp1Asim1$Ins==0),sum(mp1Asim1$Ins==1),sum(mp1Asim1$Nec==0),sum(mp1Asim1$Nec==1),sum(mp2Asim1$Car==0),sum(mp2Asim1$Car==1),sum(mp2Asim1$Fru==0),sum(mp2Asim1$Fru==1),sum(mp2Asim1$Hem==0),sum(mp2Asim1$Hem==1),sum(mp2Asim1$Ins==0),sum(mp2Asim1$Ins==1),sum(mp2Asim1$Nec==0),sum(mp2Asim1$Nec==1),sum(mp3Asim1$Car==0),sum(mp3Asim1$Car==1),sum(mp3Asim1$Fru==0),sum(mp3Asim1$Fru==1),sum(mp3Asim1$Hem==0),sum(mp3Asim1$Hem==1),sum(mp3Asim1$Ins==0),sum(mp3Asim1$Ins==1),sum(mp3Asim1$Nec==0),sum(mp3Asim1$Nec==1))

mpGrafica2Asim1$NodosRelativos <- mpGrafica2Asim1$Suma/mpGrafica2Asim1$NodosTotales

write.csv(mpGrafica2Asim1,file="estadosMvAsim1.csv")

#----


# Máxima verosimilitud - DR2

## Recopilación de las comparaciones y cálculo de número de nodos relativos - Mv DR2 ####

ComparacionesAsim2 <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                   ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B2Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                   ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                   ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                   ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosCompartidos"=0,"NodosTotales"=0)

ComparacionesAsim2$NodosCompartidos <- c(sum(as.numeric(compB1vsB2Asim2$B1Car.B2Car)),sum(as.numeric(compB2vsB3Asim2$B2Car.B3Car)),sum(as.numeric(compB1vsB3Asim2$B1Car.B2Car)),sum(as.numeric(compMultivsB1Asim2$MultiCar.B1Car)),sum(compMultivsB2Asim2$MultiCar.B2Car),sum(compMultivsB3Asim2$MultiCar.B3Car),sum(as.numeric(compMbvsB1Asim2$Car)), sum(compMbvsB2Asim2$Car), sum(compMbvsB3Asim2$Car),sum(compMbvsMultiAsim2$Car)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim2$B1Fru.B2Fru)), sum(as.numeric(compB2vsB3Asim2$B2Fru.B3Fru)),sum(as.numeric(compB1vsB3Asim2$B1Fru.B2Fru)),sum(as.numeric(compMultivsB1Asim2$MultiFru.B1Fru)),sum(compMultivsB2Asim2$MultiFru.B2Fru),sum(compMultivsB3Asim2$MultiFru.B3Fru),sum(as.numeric(compMbvsB1Asim2$Fru)), sum(compMbvsB2Asim2$Fru), sum(compMbvsB3Asim2$Fru),sum(compMbvsMultiAsim2$Fru)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim2$B1Hem.B2Hem)),sum(as.numeric(compB2vsB3Asim2$B2Hem.B3Hem)),sum(as.numeric(compB1vsB3Asim2$B1Hem.B2Hem)),sum(as.numeric(compMultivsB1Asim2$MultiHem.B1Hem)),sum(compMultivsB2Asim2$MultiHem.B2Hem),sum(compMultivsB3Asim2$MultiHem.B3Hem),sum(as.numeric(compMbvsB1Asim2$Hem)), sum(compMbvsB2Asim2$Hem), sum(compMbvsB3Asim2$Hem),sum(compMbvsMultiAsim2$Hem)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim2$B1Ins.B2Ins)),sum(as.numeric(compB2vsB3Asim2$B2Ins.B3Ins)),sum(as.numeric(compB1vsB3Asim2$B1Ins.B2Ins)),sum(as.numeric(compMultivsB1Asim2$MultiIns.B1Ins)),sum(compMultivsB2Asim2$MultiIns.B2Ins),sum(compMultivsB3Asim2$MultiIns.B3Ins),sum(as.numeric(compMbvsB1Asim2$Ins)), sum(compMbvsB2Asim2$Ins), sum(compMbvsB3Asim2$Ins),sum(compMbvsMultiAsim2$Ins)
                                         
                                         ,sum(as.numeric(compB1vsB2Asim2$B1Nec.B2Nec)),sum(as.numeric(compB2vsB3Asim2$B2Nec.B3Nec)),sum(as.numeric(compB1vsB3Asim2$B1Nec.B2Nec)),sum(as.numeric(compMultivsB1Asim2$MultiNec.B1Nec)),sum(compMultivsB2Asim2$MultiNec.B2Nec),sum(compMultivsB3Asim2$MultiNec.B3Nec),sum(as.numeric(compMbvsB1Asim2$Nec)), sum(compMbvsB2Asim2$Nec), sum(compMbvsB3Asim2$Nec),sum(compMbvsMultiAsim2$Nec))






ComparacionesAsim2$NodosTotales <- c(max(NodosTotalesAsim2$B1Car,NodosTotalesAsim2$B2Car), max(NodosTotalesAsim2$B2Car,NodosTotalesAsim2$B3Car), max(NodosTotalesAsim2$B1Car,NodosTotalesAsim2$B3Car), max(NodosTotalesAsim2$B1Car,sum(mpMultiAsim2$Carnivoria)),max(NodosTotalesAsim2$B2Car,sum(mpMultiAsim2$Carnivoria)), max(NodosTotalesAsim2$B3Car,sum(mpMultiAsim2$Carnivoria)),max(NodosTotalesAsim2$B1Car,sum(mpBaseBinAsim2$Car.CP)),max(NodosTotalesAsim2$B2Car,sum(mpBaseBinAsim2$Car.P)),max(NodosTotalesAsim2$B3Car,sum(mpBaseBinAsim2$Car.E)),max(sum(mpMultiAsim2$Carnivoria),sum(mpBaseBinAsim2$Car.CP))
                                     
                                     ,max(NodosTotalesAsim2$B1Fru,NodosTotalesAsim2$B2Fru),max(NodosTotalesAsim2$B2Fru,NodosTotalesAsim2$B3Fru),max(NodosTotalesAsim2$B1Fru,NodosTotalesAsim2$B3Fru),max(NodosTotalesAsim2$B1Fru,sum(mpMultiAsim2$Frugivoria)),max(NodosTotalesAsim2$B2Fru,sum(mpMultiAsim2$Frugivoria)),max(NodosTotalesAsim2$B3Fru,sum(mpMultiAsim2$Frugivoria)),max(NodosTotalesAsim2$B1Fru,sum(mpBaseBinAsim2$Fru.ECP)), max(NodosTotalesAsim2$B2Fru,sum(mpBaseBinAsim2$Fru.EP)), max(NodosTotalesAsim2$B3Fru,sum(mpBaseBinAsim2$Fru.E)),max(sum(mpMultiAsim2$Frugivoria),sum(mpBaseBinAsim2$Fru.E))
                                     
                                     ,max(NodosTotalesAsim2$B1Hem,NodosTotalesAsim2$B2Hem),max(NodosTotalesAsim2$B2Hem,NodosTotalesAsim2$B3Hem),max(NodosTotalesAsim2$B1Hem,NodosTotalesAsim2$B3Hem),max(NodosTotalesAsim2$B1Hem,sum(mpMultiAsim2$Hematofagia)),max(NodosTotalesAsim2$B2Hem,sum(mpMultiAsim2$Hematofagia)),max(NodosTotalesAsim2$B3Hem,sum(mpMultiAsim2$Hematofagia)),max(NodosTotalesAsim2$B1Hem,sum(mpBaseBinAsim2$Hem.CP)), max(NodosTotalesAsim2$B2Hem,sum(mpBaseBinAsim2$Hem.P)), max(NodosTotalesAsim2$B3Hem,sum(mpBaseBinAsim2$Hem.E)),max(sum(mpMultiAsim2$Hematofagia),sum(mpBaseBinAsim2$Hem.CP))
                                     
                                     ,max(NodosTotalesAsim2$B1Ins,NodosTotalesAsim2$B2Ins),max(NodosTotalesAsim2$B2Ins,NodosTotalesAsim2$B3Ins),max(NodosTotalesAsim2$B1Ins,NodosTotalesAsim2$B3Ins),max(NodosTotalesAsim2$B1Ins,sum(mpMultiAsim2$Insectivoria)),max(NodosTotalesAsim2$B2Ins,sum(mpMultiAsim2$Insectivoria)),max(NodosTotalesAsim2$B3Ins,sum(mpMultiAsim2$Insectivoria)),max(NodosTotalesAsim2$B1Ins,sum(mpBaseBinAsim2$Ins.ECP)), max(NodosTotalesAsim2$B2Ins,sum(mpBaseBinAsim2$Ins.EP)), max(NodosTotalesAsim2$B3Ins,sum(mpBaseBinAsim2$Ins.E)),max(sum(mpMultiAsim2$Insectivoria),sum(mpBaseBinAsim2$Ins.ECP))
                                     
                                     ,max(NodosTotalesAsim2$B1Nec,NodosTotalesAsim2$B2Nec),max(NodosTotalesAsim2$B2Nec,NodosTotalesAsim2$B3Nec),max(NodosTotalesAsim2$B1Nec,NodosTotalesAsim2$B3Nec),max(NodosTotalesAsim2$B1Nec,sum(mpMultiAsim2$Nectarivoria)),max(NodosTotalesAsim2$B2Nec,sum(mpMultiAsim2$Nectarivoria)),max(NodosTotalesAsim2$B3Nec,sum(mpMultiAsim2$Nectarivoria)),max(NodosTotalesAsim2$B1Nec,sum(mpBaseBinAsim2$Nec.ECP)), max(NodosTotalesAsim2$B2Nec,sum(mpBaseBinAsim2$Nec.EP)), max(NodosTotalesAsim2$B3Nec,sum(mpBaseBinAsim2$Nec.E)),max(sum(mpMultiAsim2$Nectarivoria),sum(mpBaseBinAsim2$Nec.ECP)))

ComparacionesAsim2$Nodos = ComparacionesAsim2$NodosCompartidos/ComparacionesAsim2$NodosTotales

ComparacionesAsim2[is.na(ComparacionesAsim2)] <- 0

ComparacionesAsim2$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))

ComparacionesAsim2$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)

exportation <- data.frame("Comparaciones"=ComparacionesAsim2$Matrices,"Dietas"=ComparacionesAsim2$Dieta,"Nnr"=ComparacionesAsim2$Nodos)

write.csv(exportation,file="ComparacionesMvAsim2.csv")


## Gráficas ----

### Gráfica de puntos ----
pdf(file="comparacionesMvAsim2.pdf",width = 11)
ggplot(data=ComparacionesAsim2vsER, aes(x=Matrices,Nodos, fill=Dieta)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Comparaciones MV - Asimétrica 2") +
  ylab("Número relativo de nodos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
        
        title = element_text(size = 17), 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
          panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
          panel.grid.minor.y = element_blank()
        )
dev.off()




#### Multiestado ----
probMultiAsim2 <- data.frame("Carnivoria"=mpMultiAsim2$Car,"Frugivoria"=mpMultiAsim2$Fru,"Hematofagia"=mpMultiAsim2$Hem,"Insectivoria"=mpMultiAsim2$Ins,"Nectarivoria"=mpMultiAsim2$Nec)

number <- 1:length(mpMultiAsim2$Car)

probMultiAsim2$Nodo <- number

probMultiAsim2 <- as.data.frame(melt(probMultiAsim2,id.vars ="Nodo" ))

pdf(file='prob.multiAsim2.pdf')

ggplot(data=probMultiAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
#+
# theme(axis.text = element_blank(),                         axis.ticks = element_blank(),
#panel.grid  = element_blank())

dev.off()


#### Matriz base ----
##### Carnivoria
probMbCarAsim2 <- data.frame("Ausente"=datos.reaMkDrMb.car2[,1],"Complementario"=datos.reaMkDrMb.car2[,2],"Predominante"=datos.reaMkDrMb.car2[,3])
probMbCarAsim2$Nodo <- number

probMbCarAsim2 <- as.data.frame(melt(probMbCarAsim2,id.vars ="Nodo" ))

pdf(file='probMbCarAsim2.bar.pdf')
ggplot(data=probMbCarAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probMbFruAsim2 <- data.frame("Ausente"=datos.reaMkDrMb.fru2[,1],"Complementario"=datos.reaMkDrMb.fru2[,2],"Predominante"=datos.reaMkDrMb.fru2[,3],"Estricto"=datos.reaMkDrMb.fru2[,4])
probMbFruAsim2$Nodo <- number

probMbFruAsim2 <- as.data.frame(melt(probMbFruAsim2,id.vars ="Nodo" ))

pdf(file='probMbFruAsim2.bar.pdf')
ggplot(data=probMbFruAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHemAsim2 <- data.frame("Ausente"=datos.reaMkDrMb.hem2[,1],"Complementario"=datos.reaMkDrMb.hem2[,2],"Predominante"=datos.reaMkDrMb.hem2[,3])
probMbHemAsim2$Nodo <- number

probMbHemAsim2 <- as.data.frame(melt(probMbHemAsim2,id.vars ="Nodo" ))

pdf(file='probMbHemAsim2.bar.pdf')
ggplot(data=probMbHemAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick","firebrick3"))
dev.off()


##### Insectivoria
probMbInsAsim2 <- data.frame("Ausente"=datos.reaMkDrMb.ins2[,1],"Complementario"=datos.reaMkDrMb.ins2[,2],"Predominante"=datos.reaMkDrMb.ins2[,3],"Estricto"=datos.reaMkDrMb.ins2[,4])
probMbInsAsim2$Nodo <- number

probMbInsAsim2 <- as.data.frame(melt(probMbInsAsim2,id.vars ="Nodo" ))

pdf(file='probMbInsAsim2.bar.pdf')
ggplot(data=probMbInsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNecAsim2 <- data.frame("Ausente"=datos.reaMkDrMb.nec2[,1],"Complementario"=datos.reaMkDrMb.nec2[,2],"Predominante"=datos.reaMkDrMb.nec2[,3],"Estricto"=datos.reaMkDrMb.nec2[,4])
probMbNecAsim2$Nodo <- number

probMbNecAsim2 <- as.data.frame(melt(probMbNecAsim2,id.vars ="Nodo" ))

pdf(file='probMbNecAsim2.bar.pdf')
ggplot(data=probMbNecAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



#### Matriz binaria 1 ----
##### Carnivoria
probB1CarAsim2 <- data.frame("Ausente"=datos.reaMkDRB1.car2[,1],"Presente"=datos.reaMkDRB1.car2[,2])
probB1CarAsim2$Nodo <- number

probB1CarAsim2 <- as.data.frame(melt(probB1CarAsim2,id.vars ="Nodo" ))

pdf(file='probB1CarAsim2.bar.pdf')
ggplot(data=probB1CarAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB1FruAsim2 <- data.frame("Ausente"=datos.reaMkDRB1.fru2[,1],"Presente"=datos.reaMkDRB1.fru2[,2])
probB1FruAsim2$Nodo <- number

probB1FruAsim2 <- as.data.frame(melt(probB1FruAsim2,id.vars ="Nodo" ))

pdf(file='probB1FruAsim2.bar.pdf')
ggplot(data=probB1FruAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB1HemAsim2 <- data.frame("Ausente"=datos.reaMkDRB1.hem2[,1],"Presente"=datos.reaMkDRB1.hem2[,2])
probB1HemAsim2$Nodo <- number

probB1HemAsim2 <- as.data.frame(melt(probB1HemAsim2,id.vars ="Nodo" ))

pdf(file='probB1HemAsim2.bar.pdf')
ggplot(data=probB1HemAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB1InsAsim2 <- data.frame("Ausente"=datos.reaMkDRB1.ins2[,1],"Presente"=datos.reaMkDRB1.ins2[,2])
probB1InsAsim2$Nodo <- number

probB1InsAsim2 <- as.data.frame(melt(probB1InsAsim2,id.vars ="Nodo" ))

pdf(file='probB1InsAsim2.bar.pdf')
ggplot(data=probB1InsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB1NecAsim2 <- data.frame("Ausente"=datos.reaMkDRB1.nec2[,1],"Presente"=datos.reaMkDRB1.nec2[,2])
probB1NecAsim2$Nodo <- number

probB1NecAsim2 <- as.data.frame(melt(probB1NecAsim2,id.vars ="Nodo" ))

pdf(file='probB1NecAsim2.bar.pdf')
ggplot(data=probB1NecAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()




#### Matriz binaria 2 ----
##### Carnivoria
probB2CarAsim2 <- data.frame("Ausente"=datos.reaMkDRB2.car2[,1],"Presente"=datos.reaMkDRB2.car2[,2])
probB2CarAsim2$Nodo <- number

probB2CarAsim2 <- as.data.frame(melt(probB2CarAsim2,id.vars ="Nodo" ))

pdf(file='probB2CarAsim2.bar.pdf')
ggplot(data=probB2CarAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))
dev.off()

##### Frugivoria
probB2FruAsim2 <- data.frame("Ausente"=datos.reaMkDRB2.fru2[,1],"Presente"=datos.reaMkDRB2.fru2[,2])
probB2FruAsim2$Nodo <- number

probB2FruAsim2 <- as.data.frame(melt(probB2FruAsim2,id.vars ="Nodo" ))

pdf(file='probB2FruAsim2.bar.pdf')
ggplot(data=probB2FruAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()

##### Hematofagia
probB2HemAsim2 <- data.frame("Ausente"=datos.reaMkDRB2.hem2[,1],"Presente"=datos.reaMkDRB2.hem2[,2])
probB2HemAsim2$Nodo <- number

probB2HemAsim2 <- as.data.frame(melt(probB2HemAsim2,id.vars ="Nodo" ))

pdf(file='probB2HemAsim2.bar.pdf')
ggplot(data=probB2HemAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))
dev.off()

##### Insectivoria
probB2InsAsim2 <- data.frame("Ausente"=datos.reaMkDRB2.ins2[,1],"Presente"=datos.reaMkDRB2.ins2[,2])
probB2InsAsim2$Nodo <- number

probB2InsAsim2 <- as.data.frame(melt(probB2InsAsim2,id.vars ="Nodo" ))

pdf(file='probB2InsAsim2.bar.pdf')
ggplot(data=probB2InsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))
dev.off()

##### Nectarivoria
probB2NecAsim2 <- data.frame("Ausente"=datos.reaMkDRB2.nec2[,1],"Presente"=datos.reaMkDRB2.nec2[,2])
probB2NecAsim2$Nodo <- number

probB2NecAsim2 <- as.data.frame(melt(probB2NecAsim2,id.vars ="Nodo" ))

pdf(file='probB2NecAsim2.bar.pdf')
ggplot(data=probB2NecAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))
dev.off()



#### Matriz binaria 3 ----
##### Carnivoria
probB3CarAsim2$Nodo <- number

probB3CarAsim2 <- as.data.frame(melt(probB3CarAsim2,id.vars ="Nodo" ))

#pdf(file='probB3Car.bar.pdf')
#ggplot(data=probB3Car, aes(x=Nodo,value, fill=variable)) +
geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2"))

#dev.off()

##### Frugivoria
probB3FruAsim2 <- data.frame("Ausente"=datos.reaMkDRB3.fru2[,1],"Presente"=datos.reaMkDRB3.fru2[,2])
probB3FruAsim2$Nodo <- number

probB3FruAsim2 <- as.data.frame(melt(probB3FruAsim2,id.vars ="Nodo" ))

pdf(file='probB3FruAsim2.bar.pdf')
ggplot(data=probB3FruAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","palegreen1"))
dev.off()


##### Hematofagia
probB3HemAsim2 <- data.frame("Ausente"=datos.reaMkDRB3.hem2[,1],"Presente"=datos.reaMkDRB3.hem2[,2])
probB3HemAsim2$Nodo <- number

probB3HemAsim2 <- as.data.frame(melt(probB3HemAsim2,id.vars ="Nodo" ))

pdf(file='probB3HemAsim2.bar.pdf')
ggplot(data=probB3HemAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1"))

dev.off()

##### Insectivoria
probB3InsAsim2 <- data.frame("Ausente"=datos.reaMkDRB3.ins2[,1],"Presente"=datos.reaMkDRB3.ins2[,2])
probB3InsAsim2$Nodo <- number

probB3InsAsim2 <- as.data.frame(melt(probB3InsAsim2,id.vars ="Nodo" ))

pdf(file='probB3InsAsim2.bar.pdf')
ggplot(data=probB3InsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1"))

dev.off()

##### Nectarivoria
probB3NecAsim2 <- data.frame("Ausente"=datos.reaMkDRB3.nec2[,1],"Presente"=datos.reaMkDRB3.nec2[,2])
probB3NecAsim2$Nodo <- number

probB3NecAsim2 <- as.data.frame(melt(probB3NecAsim2,id.vars ="Nodo" ))

pdf(file='probB3NecAsim2.bar.pdf')
ggplot(data=probB3NecAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1"))

dev.off()


# Máximas probabilidades ----
mpGrafica2Asim2 <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0,"NodosTotales"=44,"NodosRelativos"=0)



mpGrafica2Asim2$Suma <- c(sum(mpMultiAsim2$Carnivoria),sum(mpMultiAsim2$Frugivoria),sum(mpMultiAsim2$Hematofagia),sum(mpMultiAsim2$Insectivoria),sum(mpMultiAsim2$Nectarivoria),sum(mpBaseAsim2$Car==0),sum(mpBaseAsim2$Car==1),sum(mpBaseAsim2$Car==2),sum(mpBaseAsim2$Car==3),sum(mpBaseAsim2$Fru==0),sum(mpBaseAsim2$Fru==1),sum(mpBaseAsim2$Fru==2),sum(mpBaseAsim2$Fru==3),sum(mpBaseAsim2$Hem==0),sum(mpBaseAsim2$Hem==1),sum(mpBaseAsim2$Hem==2),sum(mpBaseAsim2$Hem==3),sum(mpBaseAsim2$Ins==0),sum(mpBaseAsim2$Ins==1),sum(mpBaseAsim2$Ins==2),sum(mpBaseAsim2$Ins==3),sum(mpBaseAsim2$Nec==0),sum(mpBaseAsim2$Nec==1),sum(mpBaseAsim2$Nec==2),sum(mpBaseAsim2$Nec==3),sum(mp1Asim2$Car==0),sum(mp1Asim2$Car==1),sum(mp1Asim2$Fru==0),sum(mp1Asim2$Fru==1),sum(mp1Asim2$Hem==0),sum(mp1Asim2$Hem==1),sum(mp1Asim2$Ins==0),sum(mp1Asim2$Ins==1),sum(mp1Asim2$Nec==0),sum(mp1Asim2$Nec==1),sum(mp2Asim2$Car==0),sum(mp2Asim2$Car==1),sum(mp2Asim2$Fru==0),sum(mp2Asim2$Fru==1),sum(mp2Asim2$Hem==0),sum(mp2Asim2$Hem==1),sum(mp2Asim2$Ins==0),sum(mp2Asim2$Ins==1),sum(mp2Asim2$Nec==0),sum(mp2Asim2$Nec==1),sum(mp3Asim2$Car==0),sum(mp3Asim2$Car==1),sum(mp3Asim2$Fru==0),sum(mp3Asim2$Fru==1),sum(mp3Asim2$Hem==0),sum(mp3Asim2$Hem==1),sum(mp3Asim2$Ins==0),sum(mp3Asim2$Ins==1),sum(mp3Asim2$Nec==0),sum(mp3Asim2$Nec==1))

mpGrafica2Asim2$NodosRelativos <- mpGrafica2Asim2$Suma/mpGrafica2Asim2$NodosTotales

write.csv(mpGrafica2Asim2,file="estadosMvAsim2.csv")

#----


# Parsimonia - ER

### Recopilación de las comparaciones y cálculo de número de nodos relativos - Parsimonia ER ----
ComparacionesPars <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                  ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B3Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                  ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                  ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                  ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosComunes"=0)

ComparacionesPars$NodosComunes <- c(sum(compB1ParsvsB2Pars$Carnivoria), sum(compB2ParsvsB3Pars$Carnivoria), sum(compB1ParsvsB3Pars$Carnivoria), sum(compMultiParsvsB1Pars$Carnivoria), sum(compMultiParsvsB2Pars$Carnivoria), sum(compMultiParsvsB3Pars$Carnivoria), sum(compMbParsvsB1Pars$Carnivoria), sum(compMbParsvsB2Pars$Carnivoria), sum(compMbParsvsB3Pars$Carnivoria), sum(compMbParsvsMultiPars$Carnivoria)
                                    , sum(compB1ParsvsB2Pars$Frugivoria), sum(compB2ParsvsB3Pars$Frugivoria), sum(compB1ParsvsB3Pars$Frugivoria), sum(compMultiParsvsB1Pars$Frugivoria), sum(compMultiParsvsB2Pars$Frugivoria), sum(compMultiParsvsB3Pars$Frugivoria), sum(compMbParsvsB1Pars$Frugivoria), sum(compMbParsvsB2Pars$Frugivoria), sum(compMbParsvsB3Pars$Frugivoria), sum(compMbParsvsMultiPars$Frugivoria)
                                    , sum(compB1ParsvsB2Pars$Hematofagia), sum(compB2ParsvsB3Pars$Hematofagia), sum(compB1ParsvsB3Pars$Hematofagia), sum(compMultiParsvsB1Pars$Hematofagia), sum(compMultiParsvsB2Pars$Hematofagia), sum(compMultiParsvsB3Pars$Hematofagia), sum(compMbParsvsB1Pars$Hematofagia), sum(compMbParsvsB2Pars$Hematofagia), sum(compMbParsvsB3Pars$Hematofagia), sum(compMbParsvsMultiPars$Hematofagia)
                                    , sum(compB1ParsvsB2Pars$Insectivoria), sum(compB2ParsvsB3Pars$Insectivoria), sum(compB1ParsvsB3Pars$Insectivoria), sum(compMultiParsvsB1Pars$Insectivoria), sum(compMultiParsvsB2Pars$Insectivoria), sum(compMultiParsvsB3Pars$Insectivoria), sum(compMbParsvsB1Pars$Insectivoria), sum(compMbParsvsB2Pars$Insectivoria), sum(compMbParsvsB3Pars$Insectivoria), sum(compMbParsvsMultiPars$Insectivoria)
                                    , sum(compB1ParsvsB2Pars$Nectarivoria), sum(compB2ParsvsB3Pars$Nectarivoria), sum(compB1ParsvsB3Pars$Nectarivoria), sum(compMultiParsvsB1Pars$Nectarivoria), sum(compMultiParsvsB2Pars$Nectarivoria), sum(compMultiParsvsB3Pars$Nectarivoria), sum(compMbParsvsB1Pars$Nectarivoria), sum(compMbParsvsB2Pars$Nectarivoria), sum(compMbParsvsB3Pars$Nectarivoria), sum(compMbParsvsMultiPars$Nectarivoria))


ComparacionesPars$NodosTotales <- c(max(sum(estadosB1Pars.Car),sum(estadosB2Pars.Car)),max(sum(estadosB2Pars.Ins),sum(estadosB3Pars.Ins)),max(sum(estadosB1Pars.Car),sum(estadosB3Pars.Car)),max(sum(binarizacionMultiPars$Carnivoria),sum(estadosB1Pars.Car)),max(sum(binarizacionMultiPars$Carnivoria),sum(estadosB2Pars.Car)),max(sum(binarizacionMultiPars$Carnivoria),sum(estadosB3Pars.Car)),max(sum(binarizacionBasePars$Car.CP),sum(estadosB1Pars.Car)),max(sum(binarizacionBasePars$Car.P),sum(estadosB2Pars.Car)), max(sum(binarizacionBasePars$Car.E),sum(estadosB3Pars.Car)), max(sum(binarizacionBasePars$Car.CP,binarizacionMultiPars$Carnivoria))
                                    ,max(sum(estadosB1Pars.Fru),sum(estadosB2Pars.Fru)),max(sum(estadosB2Pars.Fru),sum(estadosB3Pars.Fru)),max(sum(estadosB1Pars.Fru),sum(estadosB3Pars.Fru)),max(sum(binarizacionMultiPars$Frugivoria),sum(estadosB1Pars.Fru)),max(sum(binarizacionMultiPars$Frugivoria),sum(estadosB2Pars.Fru)), max(sum(binarizacionMultiPars$Frugivoria), sum(estadosB3Pars.Fru)),max(sum(binarizacionBasePars$Fru.ECP), sum(estadosB1Pars.Fru)),max(sum(binarizacionBasePars$Fru.EP), sum(estadosB2Pars.Fru)) , max(sum(binarizacionBasePars$Fru.E),sum(estadosB3Pars.Fru)),max(sum(binarizacionBasePars$Fru.ECP),sum(binarizacionMultiPars$Frugivoria))
                                    ,max(sum(estadosB1Pars.Hem),sum(estadosB2Pars.Hem)),max(sum(estadosB2Pars.Hem),sum(estadosB3Pars.Hem)),max(sum(estadosB1Pars.Hem),sum(estadosB3Pars.Hem)),max(sum(binarizacionMultiPars$Hematofagia),sum(estadosB1Pars.Hem)),max(sum(binarizacionMultiPars$Hematofagia),sum(estadosB2Pars.Hem)),max(sum(binarizacionMultiPars$Hematofagia),sum(estadosB3Pars.Hem)),max(sum(binarizacionBasePars$Hem.CP),sum(estadosB1Pars.Hem)),max(sum(binarizacionBasePars$Hem.P),sum(estadosB2Pars.Hem)) , max(sum(binarizacionBasePars$Hem.E),sum(estadosB3Pars.Hem)),max(sum(binarizacionBasePars$Hem.CP), sum(binarizacionMultiPars$Hematofagia))
                                    ,max(sum(estadosB1Pars.Ins),sum(estadosB2Pars.Ins)),max(sum(estadosB2Pars.Ins),sum(estadosB3Pars.Ins)),max(sum(estadosB1Pars.Ins),sum(estadosB3Pars.Ins)),max(sum(binarizacionMultiPars$Insectivoria),sum(estadosB1Pars.Ins)),max(sum(binarizacionMultiPars$Insectivoria),sum(estadosB2Pars.Ins)),max(sum(binarizacionMultiPars$Insectivoria),sum(estadosB3Pars.Ins)),max(sum(binarizacionBasePars$Ins.ECP),sum(estadosB1Pars.Ins)),max(sum(binarizacionBasePars$Ins.EP),sum(estadosB2Pars.Ins)), max(sum(binarizacionBasePars$Ins.E),sum(estadosB3Pars.Ins)),max(sum(binarizacionBasePars$Ins.ECP), sum(binarizacionMultiPars$Insectivoria))
                                    ,max(sum(estadosB1Pars.Nec),sum(estadosB2Pars.Nec)),max(sum(estadosB2Pars.Nec),sum(estadosB3Pars.Nec)),max(sum(estadosB1Pars.Nec),sum(estadosB3Pars.Nec)),max(sum(binarizacionMultiPars$Nectarivoria),sum(estadosB1Pars.Nec)),max(sum(binarizacionMultiPars$Nectarivoria),sum(estadosB2Pars.Nec)),max(sum(binarizacionMultiPars$Nectarivoria),sum(estadosB3Pars.Nec)),max(sum(binarizacionBasePars$Nec.ECP),sum(estadosB1Pars.Nec)),max(sum(binarizacionBasePars$Nec.EP),sum(estadosB2Pars.Nec)), max(sum(binarizacionBasePars$Nec.E),sum(estadosB3Pars.Nec)), max(sum(binarizacionBasePars$Nec.ECP), sum(binarizacionMultiPars$Nectarivoria)))


ComparacionesPars$NodosRelativos <- ComparacionesPars$NodosComunes/ComparacionesPars$NodosTotales

ComparacionesPars[is.na(ComparacionesPars)] <- 0

ComparacionesPars$Dietas <- c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))


ComparacionesPars$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)

exportacion <- data.frame("Comparaciones"=ComparacionesPars$Matrices,"Dietas"=ComparacionesPars$Dietas,"Número-nodos-relativos"=ComparacionesPars$NodosRelativos)

write.csv(exportacion,file="ComparacionesParsER.csv")


## Gráficas----
pdf(file="comparacionesParsER.pdf",width = 11)
ggplot(data=ComparacionesPars, aes(x=Matrices,NodosRelativos, fill=Dietas)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
  scale_fill_manual(values=c("mediumorchid2","palegreen2","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Sensibilidad codificaciones - Parsimonia") +
  ylab("Nodos relativos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
        
        title = element_text(size = 17), 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
          panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
          panel.grid.minor.y = element_blank()
        )
dev.off()




# Gráfica 1
## Matriz multiestado----
probMulti.pars <- binarizacionMultiPars
probMulti.pars$Nodo <- number

probMulti.pars <- as.data.frame(melt(probMulti.pars,id.vars ="Nodo" ))

pdf(file='prob.multi.pars.pdf')
ggplot(data=probMulti.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
dev.off()



## Matriz base----
probMb.pars <- data.frame("Car.A"=0,"Car.C"=rep(0,length(mpBase$Car)),"Car.P"=0,"Car.E"=0,"Fru.A"=0,"Fru.C"=0,"Fru.P"=0,"Fru.E"=0,"Hem.A"=0,"Hem.C"=0,"Hem.P"=0,"Hem.E"=0,"Ins.A"=0,"Ins.C"=0,"Ins.P"=0,"Ins.E"=0,"Nec.A"=0,"Nec.C"=0,"Nec.P"=0,"Nec.E"=0)

### Carnivoria
#### Ausente
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]==0) {
    probMb.pars[i,1]=1
  }
}

#### Complementario 
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Car[i,1]==1) {
    probMb.pars[i,2]=1
  }
}

### Frugivoria
#### Ausente
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]==0) {
    probMb.pars[i,5]=1
  }
}

#### Complementario 
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]==1) {
    probMb.pars[i,6]=1
  }
}

#### Predominante
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]==2) {
    probMb.pars[i,7]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Fru[i,1]==3) {
    probMb.pars[i,8]=1
  }
}

### Hematofagia
#### Ausente
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]==0) {
    probMb.pars[i,9]=1
  }
}

#### Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]==1) {
    probMb.pars[i,10]=1
  }
}

#### Predominante
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]==2) {
    probMb.pars[i,11]=1
  }
}

#### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Hem[i,1]==3) {
    probMb.pars[i,12]=1
  }
}

### Insectivoria
#### Ausente
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]==0) {
    probMb.pars[i,13]=1
  }
}

#### Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]==1) {
    probMb.pars[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]==2) {
    probMb.pars[i,15]=1
  }
}

##### Estricto
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Ins[i,1]==3) {
    probMb.pars[i,16]=1
  }
}

### Nectarivoria
#### Ausente
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Nec[i,1]==0) {
    probMb.pars[i,17]=1
  }
}

#### Complementario
for (i in 1:length(mpBase$Car)) {
  if (estadosMbPars.Nec[i,1]==1) {
    probMb.pars[i,18]=1
  }
}

##### Predominante
for (i in 1:length(mpBase$Car)) {
  if  (estadosMbPars.Nec[i,1]==2) {
    probMb.pars[i,19]=1
  }
}

##### Estricto
for (i in 1:length(mpBase$Car)) {
  if  (estadosMbPars.Nec[i,1]==3) {
    probMb.pars[i,20]=1
  }
}




### Carnivoria. No fue reconstruida
probMbCar.pars <- data.frame("Ausente"=probMb.pars[,1],"Complementario"=probMb.pars[,2],"Predominante"=probMb.pars[,3])
probMbCar.pars$Nodo <- number

probMbCar.pars <- as.data.frame(melt(probMbCar.pars,id.vars ="Nodo" ))

pdf(file='probMbCar.bar.pars.pdf')
ggplot(data=probMbCar.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray86","mediumorchid2","mediumorchid4","green"))
dev.off()


##### Frugivoria
probMbFru.pars <- data.frame("Ausente"=probMb.pars[,5],"Complementario"=probMb.pars[,6],"Predominante"=probMb.pars[,7],"Estricto"=probMb.pars[,8])
probMbFru.pars$Nodo <- number

probMbFru.pars <- as.data.frame(melt(probMbFru.pars,id.vars ="Nodo" ))

pdf(file='probMbFru.bar.pars.pdf')
ggplot(data=probMbFru.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHem.pars <- data.frame("Ausente"=probMb.pars[,9],"Complementario"=probMb.pars[,10],"Predominante"=probMb.pars[,11],"Estricto"=probMb.pars[,12])
probMbHem.pars$Nodo <- number

probMbHem.pars <- as.data.frame(melt(probMbHem.pars,id.vars ="Nodo" ))

pdf(file='probMbHem.bar.pars.pdf')
ggplot(data=probMbHem.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probMbIns.pars <- data.frame("Ausente"=probMb.pars[,13],"Complementario"=probMb.pars[,14],"Predominante"=probMb.pars[,15],"Estricto"=probMb.pars[,16])
probMbIns.pars$Nodo <- number

probMbIns.pars <- as.data.frame(melt(probMbIns.pars,id.vars ="Nodo" ))

pdf(file='probMbIns.bar.pars.pdf')
ggplot(data=probMbIns.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNec.pars <- data.frame("Ausente"=probMb.pars[,17],"Complementario"=probMb.pars[,18],"Predominante"=probMb.pars[,19],"Estricto"=probMb.pars[,20])
probMbNec.pars$Nodo <- number

probMbNec.pars <- as.data.frame(melt(probMbNec.pars,id.vars ="Nodo" ))

pdf(file='probMbNec.bar.pars.pdf')
ggplot(data=probMbNec.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 1----
### Carnivoria
probB1Car.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB1Pars.Car[i,1]==0) {
    probB1Car.pars[i,1]=1
  }
  else if (estadosB1Pars.Car[i,1]==1) {
    probB1Car.pars[i,2]=1
  }
}

probB1Car.pars$Nodo <- number

probB1Car.pars <- as.data.frame(melt(probB1Car.pars,id.vars ="Nodo" ))

pdf(file='probB1Car.bar.pars.pdf')
ggplot(data=probB1Car.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB1Fru.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB1Pars.Fru[i,1]==0) {
    probB1Fru.pars[i,1]=1
  }
  else if (estadosB1Pars.Fru[i,1]==1) {
    probB1Fru.pars[i,2]=1
  }
}

probB1Fru.pars$Nodo <- number

probB1Fru.pars <- as.data.frame(melt(probB1Fru.pars,id.vars ="Nodo" ))

pdf(file='probB1Fru.bar.pars.pdf')
ggplot(data=probB1Fru.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB1Hem.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB1Pars.Hem[i,1]==0) {
    probB1Hem.pars[i,1]=1
  }
  else if (estadosB1Pars.Hem[i,1]==1) {
    probB1Hem.pars[i,2]=1
  }
}

probB1Hem.pars$Nodo <- number

probB1Hem.pars <- as.data.frame(melt(probB1Hem.pars,id.vars ="Nodo" ))

pdf(file='probB1Hem.bar.pars.pdf')
ggplot(data=probB1Hem.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB1Ins.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB1Pars.Ins[i,1]==0) {
    probB1Ins.pars[i,1]=1
  }
  else if (estadosB1Pars.Ins[i,1]==1) {
    probB1Ins.pars[i,2]=1
  }
}

probB1Ins.pars$Nodo <- number

probB1Ins.pars <- as.data.frame(melt(probB1Ins.pars,id.vars ="Nodo" ))

pdf(file='probB1Ins.bar.pars.pdf')
ggplot(data=probB1Ins.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB1Nec.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB1Pars.Nec[i,1]==0) {
    probB1Nec.pars[i,1]=1
  }
  else if (estadosB1Pars.Nec[i,1]==1) {
    probB1Nec.pars[i,2]=1
  }
}

probB1Nec.pars$Nodo <- number

probB1Nec.pars <- as.data.frame(melt(probB1Nec.pars,id.vars ="Nodo" ))

pdf(file='probB1Nec.bar.pars.pdf')
ggplot(data=probB1Nec.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()





## Matriz binaria 2----
### Carnivoria
probB2Car.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB2Pars.Car[i,1]==0) {
    probB2Car.pars[i,1]=1
  }
  else if (estadosB2Pars.Car[i,1]==1) {
    probB2Car.pars[i,2]=1
  }
}

probB2Car.pars$Nodo <- number

probB2Car.pars <- as.data.frame(melt(probB2Car.pars,id.vars ="Nodo" ))

pdf(file='probB2Car.bar.pars.pdf')
ggplot(data=probB2Car.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB2Fru.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB2Pars.Car$estadosB2Pars.Car)) {
  if (estadosB2Pars.Fru[i,1]==0) {
    probB2Fru.pars[i,1]=1
  }
  else if (estadosB2Pars.Fru[i,1]==1) {
    probB2Fru.pars[i,2]=1
  }
}

probB2Fru.pars$Nodo <- number

probB2Fru.pars <- as.data.frame(melt(probB2Fru.pars,id.vars ="Nodo" ))

pdf(file='probB2Fru.bar.pars.pdf')
ggplot(data=probB2Fru.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB2Hem.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB2Pars.Hem[i,1]==0) {
    probB2Hem.pars[i,1]=1
  }
  else if (estadosB2Pars.Hem[i,1]==1) {
    probB2Hem.pars[i,2]=1
  }
}

probB2Hem.pars$Nodo <- number

probB2Hem.pars <- as.data.frame(melt(probB2Hem.pars,id.vars ="Nodo" ))

pdf(file='probB2Hem.bar.pars.pdf')
ggplot(data=probB2Hem.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB2Ins.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB2Pars.Ins[i,1]==0) {
    probB2Ins.pars[i,1]=1
  }
  else if (estadosB2Pars.Ins[i,1]==1) {
    probB2Ins.pars[i,2]=1
  }
}

probB2Ins.pars$Nodo <- number

probB2Ins.pars <- as.data.frame(melt(probB2Ins.pars,id.vars ="Nodo" ))

pdf(file='probB2Ins.bar.pars.pdf')
ggplot(data=probB2Ins.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB2Nec.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB2Pars.Nec[i,1]==0) {
    probB2Nec.pars[i,1]=1
  }
  else if (estadosB2Pars.Nec[i,1]==1) {
    probB2Nec.pars[i,2]=1
  }
}

probB2Nec.pars$Nodo <- number

probB2Nec.pars <- as.data.frame(melt(probB2Nec.pars,id.vars ="Nodo" ))

pdf(file='probB2Nec.bar.pars.pdf')
ggplot(data=probB2Nec.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 3----
### Carnivoria
probB3Car.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB3Pars.Car[i,1]==0) {
    probB3Car.pars[i,1]=1
  }
  else if (estadosB3Pars.Car[i,1]==1) {
    probB3Car.pars[i,2]=1
  }
}

probB3Car.pars$Nodo <- number

probB3Car.pars <- as.data.frame(melt(probB3Car.pars,id.vars ="Nodo" ))

pdf(file='probB3Car.bar.pars.pdf')
ggplot(data=probB3Car.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB3Fru.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB2Pars.Car$estadosB2Pars.Car)) {
  if (estadosB3Pars.Fru[i,1]==0) {
    probB3Fru.pars[i,1]=1
  }
  else if (estadosB3Pars.Fru[i,1]==1) {
    probB3Fru.pars[i,2]=1
  }
}

probB3Fru.pars$Nodo <- number

probB3Fru.pars <- as.data.frame(melt(probB3Fru.pars,id.vars ="Nodo" ))

pdf(file='probB3Fru.bar.pars.pdf')
ggplot(data=probB3Fru.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB3Hem.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB3Pars.Hem[i,1]==0) {
    probB3Hem.pars[i,1]=1
  }
  else if (estadosB3Pars.Hem[i,1]==1) {
    probB3Hem.pars[i,2]=1
  }
}

probB3Hem.pars$Nodo <- number

probB3Hem.pars <- as.data.frame(melt(probB3Hem.pars,id.vars ="Nodo" ))

pdf(file='probB3Hem.bar.pars.pdf')
ggplot(data=probB3Hem.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB3Ins.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB3Pars.Ins[i,1]==0) {
    probB3Ins.pars[i,1]=1
  }
  else if (estadosB3Pars.Ins[i,1]==1) {
    probB3Ins.pars[i,2]=1
  }
}

probB3Ins.pars$Nodo <- number

probB3Ins.pars <- as.data.frame(melt(probB3Ins.pars,id.vars ="Nodo" ))

pdf(file='probB3Ins.bar.pars.pdf')
ggplot(data=probB3Ins.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB3Nec.pars <- data.frame("Ausente"=rep(0,length(estadosB1Pars.Car$estadosB1Pars.Car)),"Presente"=0)

for (i in 1:length(estadosB1Pars.Car$estadosB1Pars.Car)) {
  if (estadosB3Pars.Nec[i,1]==0) {
    probB3Nec.pars[i,1]=1
  }
  else if (estadosB3Pars.Nec[i,1]==1) {
    probB3Nec.pars[i,2]=1
  }
}

probB3Nec.pars$Nodo <- number

probB3Nec.pars <- as.data.frame(melt(probB3Nec.pars,id.vars ="Nodo" ))

pdf(file='probB3Nec.bar.pars.pdf')
ggplot(data=probB3Nec.pars, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()
















# Máximas probabilidades----
mpPars <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0)

filas <- length(probB1NecAsim1$Nodo)/2
filas2 <- length(probB1NecAsim1$Nodo)

mpPars$Suma <- c(sum(binarizacionMultiPars$Carnivoria),sum(binarizacionMultiPars$Frugivoria),sum(binarizacionMultiPars$Hematofagia),sum(binarizacionMultiPars$Insectivoria),sum(binarizacionMultiPars$Nectarivoria)
                 ,sum(probMb.pars$Car.A),sum(probMb.pars$Car.C),sum(probMb.pars$Car.P),sum(probMb.pars$Car.E),sum(probMb.pars$Fru.A),sum(probMb.pars$Fru.C),sum(probMb.pars$Fru.P),sum(probMb.pars$Fru.E),sum(probMb.pars$Hem.A),sum(probMb.pars$Hem.C),sum(probMb.pars$Hem.P),sum(probMb.pars$Hem.E),sum(probMb.pars$Ins.A),sum(probMb.pars$Ins.C),sum(probMb.pars$Ins.P),sum(probMb.pars$Ins.E),sum(probMb.pars$Nec.A),sum(probMb.pars$Nec.C),sum(probMb.pars$Nec.P),sum(probMb.pars$Nec.E)
                 ,sum(probB1Car.pars[1:filas,3]),sum(probB1Car.pars[(filas+1):filas2,3]),sum(probB1Fru.pars[1:filas,3]),sum(probB1Fru.pars[(filas+1):filas2,3]),sum(probB1Hem.pars[1:filas,3]),sum(probB1Hem.pars[(filas+1):filas2,3]),sum(probB1Ins.pars[1:filas,3]),sum(probB1Ins.pars[(filas+1):filas2,3]),sum(probB1Nec.pars[1:filas,3]),sum(probB1Nec.pars[(filas+1):filas2,3])
                 ,sum(probB2Car.pars[1:filas,3]),sum(probB2Car.pars[(filas+1):filas2,3]),sum(probB2Fru.pars[1:filas,3]),sum(probB2Fru.pars[(filas+1):filas2,3]),sum(probB2Hem.pars[1:filas,3]),sum(probB2Hem.pars[(filas+1):filas2,3]),sum(probB2Ins.pars[1:filas,3]),sum(probB2Ins.pars[(filas+1):filas2,3]),sum(probB2Nec.pars[1:filas,3]),sum(probB2Nec.pars[(filas+1):filas2,3])
                 ,sum(probB3Car.pars[1:filas,3]),sum(probB3Car.pars[(filas+1):filas2,3]),sum(probB3Fru.pars[1:filas,3]),sum(probB3Fru.pars[(filas+1):filas2,3]),sum(probB3Hem.pars[1:filas,3]),sum(probB3Hem.pars[(filas+1):filas2,3]),sum(probB3Ins.pars[1:filas,3]),sum(probB3Ins.pars[(filas+1):filas2,3]),sum(probB3Nec.pars[1:filas,3]),sum(probB3Nec.pars[(filas+1):filas2,3]))

write.csv(mpPars,file="estadosParsER.csv")

probMulti.pars

#----

# Parsimoia - DR1

## Recopilación de las comparaciones y cálculo de número de nodos relativos - Parsimonia DR1 ----
ComparacionesParsAsim1 <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                       ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B3Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                       ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                       ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                       ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosComunes"=0)

ComparacionesParsAsim1$NodosComunes <- c(sum(compB1ParsvsB2ParsAsim1$Carnivoria), sum(compB2ParsvsB3ParsAsim1$Carnivoria), sum(compB1ParsvsB3ParsAsim1$Carnivoria), sum(compMultiParsvsB1ParsAsim1$Carnivoria), sum(compMultiParsvsB2ParsAsim1$Carnivoria), sum(compMultiParsvsB3ParsAsim1$Carnivoria), sum(compMbParsvsB1ParsAsim1$Carnivoria), sum(compMbParsvsB2ParsAsim1$Carnivoria), sum(compMbParsvsB3ParsAsim1$Carnivoria), sum(compMbParsvsMultiParsAsim1$Carnivoria)
                                         , sum(compB1ParsvsB2ParsAsim1$Frugivoria), sum(compB2ParsvsB3ParsAsim1$Frugivoria), sum(compB1ParsvsB3ParsAsim1$Frugivoria), sum(compMultiParsvsB1ParsAsim1$Frugivoria), sum(compMultiParsvsB2ParsAsim1$Frugivoria), sum(compMultiParsvsB3ParsAsim1$Frugivoria), sum(compMbParsvsB1ParsAsim1$Frugivoria), sum(compMbParsvsB2ParsAsim1$Frugivoria), sum(compMbParsvsB3ParsAsim1$Frugivoria), sum(compMbParsvsMultiParsAsim1$Frugivoria)
                                         , sum(compB1ParsvsB2ParsAsim1$Hematofagia), sum(compB2ParsvsB3ParsAsim1$Hematofagia), sum(compB1ParsvsB3ParsAsim1$Hematofagia), sum(compMultiParsvsB1ParsAsim1$Hematofagia), sum(compMultiParsvsB2ParsAsim1$Hematofagia), sum(compMultiParsvsB3ParsAsim1$Hematofagia), sum(compMbParsvsB1ParsAsim1$Hematofagia), sum(compMbParsvsB2ParsAsim1$Hematofagia), sum(compMbParsvsB3ParsAsim1$Hematofagia), sum(compMbParsvsMultiParsAsim1$Hematofagia)
                                         , sum(compB1ParsvsB2ParsAsim1$Insectivoria), sum(compB2ParsvsB3ParsAsim1$Insectivoria), sum(compB1ParsvsB3ParsAsim1$Insectivoria), sum(compMultiParsvsB1ParsAsim1$Insectivoria), sum(compMultiParsvsB2ParsAsim1$Insectivoria), sum(compMultiParsvsB3ParsAsim1$Insectivoria), sum(compMbParsvsB1ParsAsim1$Insectivoria), sum(compMbParsvsB2ParsAsim1$Insectivoria), sum(compMbParsvsB3ParsAsim1$Insectivoria), sum(compMbParsvsMultiParsAsim1$Insectivoria)
                                         , sum(compB1ParsvsB2ParsAsim1$Nectarivoria), sum(compB2ParsvsB3ParsAsim1$Nectarivoria), sum(compB1ParsvsB3ParsAsim1$Nectarivoria), sum(compMultiParsvsB1ParsAsim1$Nectarivoria), sum(compMultiParsvsB2ParsAsim1$Nectarivoria), sum(compMultiParsvsB3ParsAsim1$Nectarivoria), sum(compMbParsvsB1ParsAsim1$Nectarivoria), sum(compMbParsvsB2ParsAsim1$Nectarivoria), sum(compMbParsvsB3ParsAsim1$Nectarivoria), sum(compMbParsvsMultiParsAsim1$Nectarivoria))

ComparacionesParsAsim1$NodosTotales <- c(max(sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1),sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)),max(sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1),sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1)),max(sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1),sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1)),max(sum(binarizacionMultiParsAsim1$Carnivoria),sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),max(sum(binarizacionMultiParsAsim1$Carnivoria),sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)),max(sum(binarizacionMultiParsAsim1$Carnivoria),sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1)),max(sum(binarizacionBaseParsAsim1$Car.CP),sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),max(sum(binarizacionBaseParsAsim1$Car.P),sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)), max(sum(binarizacionBaseParsAsim1$Car.E),sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1)), max(sum(binarizacionBaseParsAsim1$Car.CP,binarizacionMultiParsAsim1$Carnivoria))
                                         ,max(sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1),sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1)),max(sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1),sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1)),max(sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1),sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1)),max(sum(binarizacionMultiParsAsim1$Frugivoria),sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1)),max(sum(binarizacionMultiParsAsim1$Frugivoria),sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1)), max(sum(binarizacionMultiParsAsim1$Frugivoria), sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1)),max(sum(binarizacionBaseParsAsim1$Fru.ECP), sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1)),max(sum(binarizacionBaseParsAsim1$Fru.EP), sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1)) , max(sum(binarizacionBaseParsAsim1$Fru.E),sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1)),max(sum(binarizacionBaseParsAsim1$Fru.ECP),sum(binarizacionMultiParsAsim1$Frugivoria))
                                         ,max(sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1),sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1)),max(sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1),sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1)),max(sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1),sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1)),max(sum(binarizacionMultiParsAsim1$Hematofagia),sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1)),max(sum(binarizacionMultiParsAsim1$Hematofagia),sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1)),max(sum(binarizacionMultiParsAsim1$Hematofagia),sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1)),max(sum(binarizacionBaseParsAsim1$Hem.CP),sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1)),max(sum(binarizacionBaseParsAsim1$Hem.P),sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1)) , max(sum(binarizacionBaseParsAsim1$Hem.E),sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1)),max(sum(binarizacionBaseParsAsim1$Hem.CP), sum(binarizacionMultiParsAsim1$Hematofagia))
                                         ,max(sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1),sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1)),max(sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1),sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1)),max(sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1),sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1)),max(sum(binarizacionMultiParsAsim1$Insectivoria),sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1)),max(sum(binarizacionMultiParsAsim1$Insectivoria),sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1)),max(sum(binarizacionMultiParsAsim1$Insectivoria),sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1)),max(sum(binarizacionBaseParsAsim1$Ins.ECP),sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1)),max(sum(binarizacionBaseParsAsim1$Ins.EP),sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1)), max(sum(binarizacionBaseParsAsim1$Ins.E),sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1)),max(sum(binarizacionBaseParsAsim1$Ins.ECP), sum(binarizacionMultiParsAsim1$Insectivoria))
                                         ,max(sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1),sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1)),max(sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1),sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1)),max(sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1),sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1)),max(sum(binarizacionMultiParsAsim1$Nectarivoria),sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1)),max(sum(binarizacionMultiParsAsim1$Nectarivoria),sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1)),max(sum(binarizacionMultiParsAsim1$Nectarivoria),sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1)),max(sum(binarizacionBaseParsAsim1$Nec.ECP),sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1)),max(sum(binarizacionBaseParsAsim1$Nec.EP),sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1)), max(sum(binarizacionBaseParsAsim1$Nec.E),sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1)), max(sum(binarizacionBaseParsAsim1$Nec.ECP), sum(binarizacionMultiParsAsim1$Nectarivoria)))

ComparacionesParsAsim1$NodosRelativos <- ComparacionesParsAsim1$NodosComunes/ComparacionesParsAsim1$NodosTotales

ComparacionesParsAsim1[is.na(ComparacionesParsAsim1)] <- 0

ComparacionesParsAsim1$Dietas <- c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))

ComparacionesParsAsim1$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)


compExportar <- data.frame("Matrices"=ComparacionesParsAsim1$Matrices,"Dietas"=ComparacionesParsAsim1$Dietas,"Número-nodos-relativos"=ComparacionesParsAsim1$NodosRelativos)

write.csv(compExportar,file="ComparacionesParsAsim1.csv")

## Gráficas----
pdf(file="comparacionesParsAsim1.pdf",width = 11)
ggplot(data=ComparacionesParsAsim1, aes(x=Matrices,NodosRelativos, fill=Dietas)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
  scale_fill_manual(values=c("mediumorchid2","palegreen2","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Sensibilidad codificaciones - Parsimonia") +
  ylab("Nodos relativos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
        
        title = element_text(size = 17), 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
          panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
          panel.grid.minor.y = element_blank()
        )
dev.off()




# Gráfica adicional 1
## Matriz multiestado----
probMulti.parsAsim1 <- binarizacionMultiParsAsim1
probMulti.parsAsim1$Nodo <- number

probMulti.parsAsim1 <- as.data.frame(melt(probMulti.parsAsim1,id.vars ="Nodo" ))

pdf(file='prob.multi.parsAsim1.pdf')
ggplot(data=probMulti.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
dev.off()



## Matriz base ----
probMb.parsAsim1 <- data.frame("Car.A"=0,"Car.C"=rep(0,length(mpBaseAsim1$Car)),"Car.P"=0,"Car.E"=0,"Fru.A"=0,"Fru.C"=0,"Fru.P"=0,"Fru.E"=0,"Hem.A"=0,"Hem.C"=0,"Hem.P"=0,"Hem.E"=0,"Ins.A"=0,"Ins.C"=0,"Ins.P"=0,"Ins.E"=0,"Nec.A"=0,"Nec.C"=0,"Nec.P"=0,"Nec.E"=0)

### Carnivoria
#### Ausente
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]==0) {
    probMb.parsAsim1[i,1]=1
  }
}

#### Complementario 
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.CarAsim1[i,1]==1) {
    probMb.parsAsim1[i,2]=1
  }
}

### Frugivoria
#### Ausente
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==0) {
    probMb.parsAsim1[i,5]=1
  }
}

#### Complementario 
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==1) {
    probMb.parsAsim1[i,6]=1
  }
}

#### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==2) {
    probMb.parsAsim1[i,7]=1
  }
}

#### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.FruAsim1[i,1]==3) {
    probMb.parsAsim1[i,8]=1
  }
}

### Hematofagia
#### Ausente
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==0) {
    probMb.parsAsim1[i,9]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==1) {
    probMb.parsAsim1[i,10]=1
  }
}

#### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==2) {
    probMb.parsAsim1[i,11]=1
  }
}

#### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.HemAsim1[i,1]==3) {
    probMb.parsAsim1[i,12]=1
  }
}

### Insectivoria
#### Ausente
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==0) {
    probMb.parsAsim1[i,13]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==1) {
    probMb.parsAsim1[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==2) {
    probMb.parsAsim1[i,15]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.InsAsim1[i,1]==3) {
    probMb.parsAsim1[i,16]=1
  }
}

### Nectarivoria
#### Ausente
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.NecAsim1[i,1]==0) {
    probMb.parsAsim1[i,17]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim1$Car)) {
  if (estadosMbPars.NecAsim1[i,1]==1) {
    probMb.parsAsim1[i,18]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim1$Car)) {
  if  (estadosMbPars.NecAsim1[i,1]==2) {
    probMb.parsAsim1[i,19]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim1$Car)) {
  if  (estadosMbPars.NecAsim1[i,1]==3) {
    probMb.parsAsim1[i,20]=1
  }
}




### Carnivoria. De todos modos es 0 :v
probMbCar.parsAsim1 <- data.frame("Ausente"=probMb.parsAsim1[,1],"Complementario"=probMb.parsAsim1[,2],"Predominante"=probMb.parsAsim1[,3])
probMbCar.parsAsim1$Nodo <- number

probMbCar.parsAsim1 <- as.data.frame(melt(probMbCar.parsAsim1,id.vars ="Nodo" ))

pdf(file='probMbCar.bar.parsAsim1.pdf')
ggplot(data=probMbCar.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray86","mediumorchid2","mediumorchid4","green"))
dev.off()


##### Frugivoria
probMbFru.parsAsim1 <- data.frame("Ausente"=probMb.parsAsim1[,5],"Complementario"=probMb.parsAsim1[,6],"Predominante"=probMb.parsAsim1[,7],"Estricto"=probMb.parsAsim1[,8])
probMbFru.parsAsim1$Nodo <- number

probMbFru.parsAsim1 <- as.data.frame(melt(probMbFru.parsAsim1,id.vars ="Nodo" ))

pdf(file='probMbFru.bar.parsAsim1.pdf')
ggplot(data=probMbFru.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHem.parsAsim1 <- data.frame("Ausente"=probMb.parsAsim1[,9],"Complementario"=probMb.parsAsim1[,10],"Predominante"=probMb.parsAsim1[,11],"Estricto"=probMb.parsAsim1[,12])
probMbHem.parsAsim1$Nodo <- number

probMbHem.parsAsim1 <- as.data.frame(melt(probMbHem.parsAsim1,id.vars ="Nodo" ))

pdf(file='probMbHem.bar.parsAsim1.pdf')
ggplot(data=probMbHem.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probMbIns.parsAsim1 <- data.frame("Ausente"=probMb.parsAsim1[,13],"Complementario"=probMb.parsAsim1[,14],"Predominante"=probMb.parsAsim1[,15],"Estricto"=probMb.parsAsim1[,16])
probMbIns.parsAsim1$Nodo <- number

probMbIns.parsAsim1 <- as.data.frame(melt(probMbIns.parsAsim1,id.vars ="Nodo" ))

pdf(file='probMbIns.bar.parsAsim1.pdf')
ggplot(data=probMbIns.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNec.parsAsim1 <- data.frame("Ausente"=probMb.parsAsim1[,17],"Complementario"=probMb.parsAsim1[,18],"Predominante"=probMb.parsAsim1[,19],"Estricto"=probMb.parsAsim1[,20])
probMbNec.parsAsim1$Nodo <- number

probMbNec.parsAsim1 <- as.data.frame(melt(probMbNec.parsAsim1,id.vars ="Nodo" ))

pdf(file='probMbNec.bar.parsAsim1.pdf')
ggplot(data=probMbNec.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 1 ----
### Carnivoria
probB1Car.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB1Pars.CarAsim1[i,1]==0) {
    probB1Car.parsAsim1[i,1]=1
  }
  else if (estadosB1Pars.CarAsim1[i,1]==1) {
    probB1Car.parsAsim1[i,2]=1
  }
}

probB1Car.parsAsim1$Nodo <- number

probB1Car.parsAsim1 <- as.data.frame(melt(probB1Car.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB1Car.bar.parsAsim1.pdf')
ggplot(data=probB1Car.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB1Fru.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB1Pars.FruAsim1[i,1]==0) {
    probB1Fru.parsAsim1[i,1]=1
  }
  else if (estadosB1Pars.FruAsim1[i,1]==1) {
    probB1Fru.parsAsim1[i,2]=1
  }
}

probB1Fru.parsAsim1$Nodo <- number

probB1Fru.parsAsim1 <- as.data.frame(melt(probB1Fru.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB1Fru.bar.parsAsim1.pdf')
ggplot(data=probB1Fru.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB1Hem.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB1Pars.HemAsim1[i,1]==0) {
    probB1Hem.parsAsim1[i,1]=1
  }
  else if (estadosB1Pars.HemAsim1[i,1]==1) {
    probB1Hem.parsAsim1[i,2]=1
  }
}

probB1Hem.parsAsim1$Nodo <- number

probB1Hem.parsAsim1 <- as.data.frame(melt(probB1Hem.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB1Hem.bar.parsAsim1.pdf')
ggplot(data=probB1Hem.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB1Ins.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB1Pars.InsAsim1[i,1]==0) {
    probB1Ins.parsAsim1[i,1]=1
  }
  else if (estadosB1Pars.InsAsim1[i,1]==1) {
    probB1Ins.parsAsim1[i,2]=1
  }
}

probB1Ins.parsAsim1$Nodo <- number

probB1Ins.parsAsim1 <- as.data.frame(melt(probB1Ins.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB1Ins.bar.parsAsim1.pdf')
ggplot(data=probB1Ins.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB1Nec.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB1Pars.NecAsim1[i,1]==0) {
    probB1Nec.parsAsim1[i,1]=1
  }
  else if (estadosB1Pars.NecAsim1[i,1]==1) {
    probB1Nec.parsAsim1[i,2]=1
  }
}

probB1Nec.parsAsim1$Nodo <- number

probB1Nec.parsAsim1 <- as.data.frame(melt(probB1Nec.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB1Nec.bar.parsAsim1.pdf')
ggplot(data=probB1Nec.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



## Matriz binaria 2----
### Carnivoria
probB2Car.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB2Pars.CarAsim1[i,1]==0) {
    probB2Car.parsAsim1[i,1]=1
  }
  else if (estadosB2Pars.CarAsim1[i,1]==1) {
    probB2Car.parsAsim1[i,2]=1
  }
}

probB2Car.parsAsim1$Nodo <- number

probB2Car.parsAsim1 <- as.data.frame(melt(probB2Car.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB2Car.bar.parsAsim1.pdf')
ggplot(data=probB2Car.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB2Fru.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)) {
  if (estadosB2Pars.FruAsim1[i,1]==0) {
    probB2Fru.parsAsim1[i,1]=1
  }
  else if (estadosB2Pars.FruAsim1[i,1]==1) {
    probB2Fru.parsAsim1[i,2]=1
  }
}

probB2Fru.parsAsim1$Nodo <- number

probB2Fru.parsAsim1 <- as.data.frame(melt(probB2Fru.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB2Fru.bar.parsAsim1.pdf')
ggplot(data=probB2Fru.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB2Hem.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB2Pars.HemAsim1[i,1]==0) {
    probB2Hem.parsAsim1[i,1]=1
  }
  else if (estadosB2Pars.HemAsim1[i,1]==1) {
    probB2Hem.parsAsim1[i,2]=1
  }
}

probB2Hem.parsAsim1$Nodo <- number

probB2Hem.parsAsim1 <- as.data.frame(melt(probB2Hem.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB2Hem.bar.parsAsim1.pdf')
ggplot(data=probB2Hem.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB2Ins.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB2Pars.InsAsim1[i,1]==0) {
    probB2Ins.parsAsim1[i,1]=1
  }
  else if (estadosB2Pars.InsAsim1[i,1]==1) {
    probB2Ins.parsAsim1[i,2]=1
  }
}

probB2Ins.parsAsim1$Nodo <- number

probB2Ins.parsAsim1 <- as.data.frame(melt(probB2Ins.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB2Ins.bar.parsAsim1.pdf')
ggplot(data=probB2Ins.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB2Nec.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB2Pars.NecAsim1[i,1]==0) {
    probB2Nec.parsAsim1[i,1]=1
  }
  else if (estadosB2Pars.NecAsim1[i,1]==1) {
    probB2Nec.parsAsim1[i,2]=1
  }
}

probB2Nec.parsAsim1$Nodo <- number

probB2Nec.parsAsim1 <- as.data.frame(melt(probB2Nec.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB2Nec.bar.parsAsim1.pdf')
ggplot(data=probB2Nec.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 3 ----
### Carnivoria
probB3Car.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB3Pars.CarAsim1[i,1]==0) {
    probB3Car.parsAsim1[i,1]=1
  }
  else if (estadosB3Pars.CarAsim1[i,1]==1) {
    probB3Car.parsAsim1[i,2]=1
  }
}

probB3Car.parsAsim1$Nodo <- number

probB3Car.parsAsim1 <- as.data.frame(melt(probB3Car.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB3Car.bar.parsAsim1.pdf')
ggplot(data=probB3Car.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB3Fru.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)) {
  if (estadosB3Pars.FruAsim1[i,1]==0) {
    probB3Fru.parsAsim1[i,1]=1
  }
  else if (estadosB3Pars.FruAsim1[i,1]==1) {
    probB3Fru.parsAsim1[i,2]=1
  }
}

probB3Fru.parsAsim1$Nodo <- number

probB3Fru.parsAsim1 <- as.data.frame(melt(probB3Fru.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB3Fru.bar.parsAsim1.pdf')
ggplot(data=probB3Fru.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB3Hem.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB3Pars.HemAsim1[i,1]==0) {
    probB3Hem.parsAsim1[i,1]=1
  }
  else if (estadosB3Pars.HemAsim1[i,1]==1) {
    probB3Hem.parsAsim1[i,2]=1
  }
}

probB3Hem.parsAsim1$Nodo <- number

probB3Hem.parsAsim1 <- as.data.frame(melt(probB3Hem.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB3Hem.bar.parsAsim1.pdf')
ggplot(data=probB3Hem.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB3Ins.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB3Pars.InsAsim1[i,1]==0) {
    probB3Ins.parsAsim1[i,1]=1
  }
  else if (estadosB3Pars.InsAsim1[i,1]==1) {
    probB3Ins.parsAsim1[i,2]=1
  }
}

probB3Ins.parsAsim1$Nodo <- number

probB3Ins.parsAsim1 <- as.data.frame(melt(probB3Ins.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB3Ins.bar.parsAsim1.pdf')
ggplot(data=probB3Ins.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB3Nec.parsAsim1 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)) {
  if (estadosB3Pars.NecAsim1[i,1]==0) {
    probB3Nec.parsAsim1[i,1]=1
  }
  else if (estadosB3Pars.NecAsim1[i,1]==1) {
    probB3Nec.parsAsim1[i,2]=1
  }
}

probB3Nec.parsAsim1$Nodo <- number

probB3Nec.parsAsim1 <- as.data.frame(melt(probB3Nec.parsAsim1,id.vars ="Nodo" ))

pdf(file='probB3Nec.bar.parsAsim1.pdf')
ggplot(data=probB3Nec.parsAsim1, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



## Máximas probabilidades----
mpGrafica2ParsAsim1 <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0)

filas <- length(probB1Car.parsAsim1$Nodo)/2
filas2 <- length(probB1Car.parsAsim1$Nodo)

mpGrafica2ParsAsim1$Suma <- c(sum(binarizacionMultiParsAsim1$Carnivoria),sum(binarizacionMultiParsAsim1$Frugivoria),sum(binarizacionMultiParsAsim1$Hematofagia),sum(binarizacionMultiParsAsim1$Insectivoria),sum(binarizacionMultiParsAsim1$Nectarivoria)
                              ,sum(probMb.parsAsim1$Car.A),sum(probMb.parsAsim1$Car.C),sum(probMb.parsAsim1$Car.P),sum(probMb.parsAsim1$Car.E),sum(probMb.parsAsim1$Fru.A),sum(probMb.parsAsim1$Fru.C),sum(probMb.parsAsim1$Fru.P),sum(probMb.parsAsim1$Fru.E),sum(probMb.parsAsim1$Hem.A),sum(probMb.parsAsim1$Hem.C),sum(probMb.parsAsim1$Hem.P),sum(probMb.parsAsim1$Hem.E),sum(probMb.parsAsim1$Ins.A),sum(probMb.parsAsim1$Ins.C),sum(probMb.parsAsim1$Ins.P),sum(probMb.parsAsim1$Ins.E),sum(probMb.parsAsim1$Nec.A),sum(probMb.parsAsim1$Nec.C),sum(probMb.parsAsim1$Nec.P),sum(probMb.parsAsim1$Nec.E)
                              
                              ,sum(probB1Car.parsAsim1[1:filas,3]),sum(probB1Car.parsAsim1[(filas+1):filas2,3]),sum(probB1Fru.parsAsim1[1:filas,3]),sum(probB1Fru.parsAsim1[(filas+1):filas2,3]),sum(probB1Hem.parsAsim1[1:filas,3]),sum(probB1Hem.parsAsim1[(filas+1):filas2,3]),sum(probB1Ins.parsAsim1[1:filas,3]),sum(probB1Ins.parsAsim1[(filas+1):filas2,3]),sum(probB1Nec.parsAsim1[1:filas,3]),sum(probB1Nec.parsAsim1[(filas+1):filas2,3])
                              
                              ,sum(probB2Car.parsAsim1[1:filas,3]),sum(probB2Car.parsAsim1[(filas+1):filas2,3]),sum(probB2Fru.parsAsim1[1:filas,3]),sum(probB2Fru.parsAsim1[(filas+1):filas2,3]),sum(probB2Hem.parsAsim1[1:filas,3]),sum(probB2Hem.parsAsim1[(filas+1):filas2,3]),sum(probB2Ins.parsAsim1[1:filas,3]),sum(probB2Ins.parsAsim1[(filas+1):filas2,3]),sum(probB2Nec.parsAsim1[1:filas,3]),sum(probB2Nec.parsAsim1[(filas+1):filas2,3])
                              
                              ,sum(probB3Car.parsAsim1[1:filas,3]),sum(probB3Car.parsAsim1[(filas+1):filas2,3]),sum(probB3Fru.parsAsim1[1:filas,3]),sum(probB3Fru.parsAsim1[(filas+1):filas2,3]),sum(probB3Hem.parsAsim1[1:filas,3]),sum(probB3Hem.parsAsim1[(filas+1):filas2,3]),sum(probB3Ins.parsAsim1[1:filas,3]),sum(probB3Ins.parsAsim1[(filas+1):filas2,3]),sum(probB3Nec.parsAsim1[1:filas,3]),sum(probB3Nec.parsAsim1[(filas+1):filas2,3]))

write.csv(mpGrafica2ParsAsim1,file="estadosParsAsim1.csv")

#----


# Parsimoia - DR2

## Recopilación de las comparaciones y cálculo de número de nodos relativos - Parsimonia DR2 ----
ComparacionesParsAsim2 <- data.frame("Comparaciones"=c("B1Car-B2Car","B2Car-B3Car","B1Car-B3Car","MultiCar-B1Car","MultiCar-B2Car","MultiCar-B3Car","MbCar-B1Car","MbCar-B2Car","MbCar-B3Car","MbCar-MultiCar"
                                                       ,"B1Fru-B2Fru","B2Fru-B3Fru","B1Fru-B3Fru","MultiFru-B1Fru","MultiFru-B2Fru","MultiFru-B3Fru","MbFru-B1Fru","MbFru-B2Fru","MbFru-B3Fru","MbFru-MultiFru"
                                                       ,"B1Hem-B2Hem","B2Hem-B3Hem","B1Hem-B3Hem","MultiHem-B1Hem","MultiHem-B2Hem","MultiHem-B3Hem","MbHem-B1Hem","MbHem-B2Hem","MbHem-B3Hem","MbHem-MultiHem"
                                                       ,"B1Ins-B2Ins","B2Ins-B3Ins","B1Ins-B3Ins","MultiIns-B1Ins","MultiIns-B2Ins","MultiIns-B3Ins","MbIns-B1Ins","MbIns-B2Ins","MbIns-B3Ins","MbIns-MultiIns"
                                                       ,"B1Nec-B2Nec","B2Nec-B3Nec","B1Nec-B3Nec","MultiNec-B1Nec","MultiNec-B2Nec","MultiNec-B3Nec","MbNec-B1Nec","MbNec-B2Nec","MbNec-B3Nec","MbNec-MultiNec"), "NodosComunes"=0)

ComparacionesParsAsim2$NodosComunes <- c(sum(compB1ParsvsB2ParsAsim2$Carnivoria), sum(compB2ParsvsB3ParsAsim2$Carnivoria), sum(compB1ParsvsB3ParsAsim2$Carnivoria), sum(compMultiParsvsB1ParsAsim2$Carnivoria), sum(compMultiParsvsB2ParsAsim2$Carnivoria), sum(compMultiParsvsB3ParsAsim2$Carnivoria), sum(compMbParsvsB1ParsAsim2$Carnivoria), sum(compMbParsvsB2ParsAsim2$Carnivoria), sum(compMbParsvsB3ParsAsim2$Carnivoria), sum(compMbParsvsMultiParsAsim2$Carnivoria)
                                         , sum(compB1ParsvsB2ParsAsim2$Frugivoria), sum(compB2ParsvsB3ParsAsim2$Frugivoria), sum(compB1ParsvsB3ParsAsim2$Frugivoria), sum(compMultiParsvsB1ParsAsim2$Frugivoria), sum(compMultiParsvsB2ParsAsim2$Frugivoria), sum(compMultiParsvsB3ParsAsim2$Frugivoria), sum(compMbParsvsB1ParsAsim2$Frugivoria), sum(compMbParsvsB2ParsAsim2$Frugivoria), sum(compMbParsvsB3ParsAsim2$Frugivoria), sum(compMbParsvsMultiParsAsim2$Frugivoria)
                                         , sum(compB1ParsvsB2ParsAsim2$Hematofagia), sum(compB2ParsvsB3ParsAsim2$Hematofagia), sum(compB1ParsvsB3ParsAsim2$Hematofagia), sum(compMultiParsvsB1ParsAsim2$Hematofagia), sum(compMultiParsvsB2ParsAsim2$Hematofagia), sum(compMultiParsvsB3ParsAsim2$Hematofagia), sum(compMbParsvsB1ParsAsim2$Hematofagia), sum(compMbParsvsB2ParsAsim2$Hematofagia), sum(compMbParsvsB3ParsAsim2$Hematofagia), sum(compMbParsvsMultiParsAsim2$Hematofagia)
                                         , sum(compB1ParsvsB2ParsAsim2$Insectivoria), sum(compB2ParsvsB3ParsAsim2$Insectivoria), sum(compB1ParsvsB3ParsAsim2$Insectivoria), sum(compMultiParsvsB1ParsAsim2$Insectivoria), sum(compMultiParsvsB2ParsAsim2$Insectivoria), sum(compMultiParsvsB3ParsAsim2$Insectivoria), sum(compMbParsvsB1ParsAsim2$Insectivoria), sum(compMbParsvsB2ParsAsim2$Insectivoria), sum(compMbParsvsB3ParsAsim2$Insectivoria), sum(compMbParsvsMultiParsAsim2$Insectivoria)
                                         , sum(compB1ParsvsB2ParsAsim2$Nectarivoria), sum(compB2ParsvsB3ParsAsim2$Nectarivoria), sum(compB1ParsvsB3ParsAsim2$Nectarivoria), sum(compMultiParsvsB1ParsAsim2$Nectarivoria), sum(compMultiParsvsB2ParsAsim2$Nectarivoria), sum(compMultiParsvsB3ParsAsim2$Nectarivoria), sum(compMbParsvsB1ParsAsim2$Nectarivoria), sum(compMbParsvsB2ParsAsim2$Nectarivoria), sum(compMbParsvsB3ParsAsim2$Nectarivoria), sum(compMbParsvsMultiParsAsim2$Nectarivoria))

ComparacionesParsAsim2$NodosTotales <- c(max(sum(estadosB1Pars.CarAsim2),sum(estadosB2Pars.CarAsim2)),max(sum(estadosB2Pars.CarAsim2),sum(estadosB3Pars.CarAsim2)),max(sum(estadosB1Pars.CarAsim2),sum(estadosB3Pars.CarAsim2)),max(sum(binarizacionMultiParsAsim2$Carnivoria),sum(estadosB1Pars.CarAsim2)),max(sum(binarizacionMultiParsAsim2$Carnivoria),sum(estadosB2Pars.CarAsim2)),max(sum(binarizacionMultiParsAsim2$Carnivoria),sum(estadosB3Pars.CarAsim2)),max(sum(binarizacionBaseParsAsim2$Car.CP),sum(estadosB1Pars.CarAsim2)),max(sum(binarizacionBaseParsAsim2$Car.P),sum(estadosB2Pars.CarAsim2)), max(sum(binarizacionBaseParsAsim2$Car.E),sum(estadosB3Pars.CarAsim2)), max(sum(binarizacionBaseParsAsim2$Car.CP,binarizacionMultiParsAsim2$Carnivoria))
                                         ,max(sum(estadosB1Pars.FruAsim2),sum(estadosB2Pars.FruAsim2)),max(sum(estadosB2Pars.FruAsim2),sum(estadosB3Pars.FruAsim2)),max(sum(estadosB1Pars.FruAsim2),sum(estadosB3Pars.FruAsim2)),max(sum(binarizacionMultiParsAsim2$Frugivoria),sum(estadosB1Pars.FruAsim2)),max(sum(binarizacionMultiParsAsim2$Frugivoria),sum(estadosB2Pars.FruAsim2)), max(sum(binarizacionMultiParsAsim2$Frugivoria), sum(estadosB3Pars.FruAsim2)),max(sum(binarizacionBaseParsAsim2$Fru.ECP), sum(estadosB1Pars.FruAsim2)),max(sum(binarizacionBaseParsAsim2$Fru.EP), sum(estadosB2Pars.FruAsim2)) , max(sum(binarizacionBaseParsAsim2$Fru.E),sum(estadosB3Pars.FruAsim2)),max(sum(binarizacionBaseParsAsim2$Fru.ECP),sum(binarizacionMultiParsAsim2$Frugivoria))
                                         ,max(sum(estadosB1Pars.HemAsim2),sum(estadosB2Pars.HemAsim2)),max(sum(estadosB2Pars.HemAsim2),sum(estadosB3Pars.HemAsim2)),max(sum(estadosB1Pars.HemAsim2),sum(estadosB3Pars.HemAsim2)),max(sum(binarizacionMultiParsAsim2$Hematofagia),sum(estadosB1Pars.HemAsim2)),max(sum(binarizacionMultiParsAsim2$Hematofagia),sum(estadosB2Pars.HemAsim2)),max(sum(binarizacionMultiParsAsim2$Hematofagia),sum(estadosB3Pars.HemAsim2)),max(sum(binarizacionBaseParsAsim2$Hem.CP),sum(estadosB1Pars.HemAsim2)),max(sum(binarizacionBaseParsAsim2$Hem.P),sum(estadosB2Pars.HemAsim2)) , max(sum(binarizacionBaseParsAsim2$Hem.E),sum(estadosB3Pars.HemAsim2)),max(sum(binarizacionBaseParsAsim2$Hem.CP), sum(binarizacionMultiParsAsim2$Hematofagia))
                                         ,max(sum(estadosB1Pars.InsAsim2),sum(estadosB2Pars.InsAsim2)),max(sum(estadosB2Pars.InsAsim2),sum(estadosB3Pars.InsAsim2)),max(sum(estadosB1Pars.InsAsim2),sum(estadosB3Pars.InsAsim2)),max(sum(binarizacionMultiParsAsim2$Insectivoria),sum(estadosB1Pars.InsAsim2)),max(sum(binarizacionMultiParsAsim2$Insectivoria),sum(estadosB2Pars.InsAsim2)),max(sum(binarizacionMultiParsAsim2$Insectivoria),sum(estadosB3Pars.InsAsim2)),max(sum(binarizacionBaseParsAsim2$Ins.ECP),sum(estadosB1Pars.InsAsim2)),max(sum(binarizacionBaseParsAsim2$Ins.EP),sum(estadosB2Pars.InsAsim2)), max(sum(binarizacionBaseParsAsim2$Ins.E),sum(estadosB3Pars.InsAsim2)),max(sum(binarizacionBaseParsAsim2$Ins.ECP), sum(binarizacionMultiParsAsim2$Insectivoria))
                                         ,max(sum(estadosB1Pars.NecAsim2),sum(estadosB2Pars.NecAsim2)),max(sum(estadosB2Pars.NecAsim2),sum(estadosB3Pars.NecAsim2)),max(sum(estadosB1Pars.NecAsim2),sum(estadosB3Pars.NecAsim2)),max(sum(binarizacionMultiParsAsim2$Nectarivoria),sum(estadosB1Pars.NecAsim2)),max(sum(binarizacionMultiParsAsim2$Nectarivoria),sum(estadosB2Pars.NecAsim2)),max(sum(binarizacionMultiParsAsim2$Nectarivoria),sum(estadosB3Pars.NecAsim2)),max(sum(binarizacionBaseParsAsim2$Nec.ECP),sum(estadosB1Pars.NecAsim2)),max(sum(binarizacionBaseParsAsim2$Nec.EP),sum(estadosB2Pars.NecAsim2)), max(sum(binarizacionBaseParsAsim2$Nec.E),sum(estadosB3Pars.NecAsim2)), max(sum(binarizacionBaseParsAsim2$Nec.ECP), sum(binarizacionMultiParsAsim2$Nectarivoria)))

ComparacionesParsAsim2$NodosRelativos <- ComparacionesParsAsim2$NodosComunes/ComparacionesParsAsim2$NodosTotales

ComparacionesParsAsim2[is.na(ComparacionesParsAsim2)] <- 0

ComparacionesParsAsim2$Dietas <- c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),each=10))

ComparacionesParsAsim2$Matrices <- rep(c("B1vsB2","B2vsB3","B1vsB3","MultivsB1","MultivsB2","MultivsB3","MbvsB1","MbvsB2","MbvsB3","MbvsMulti"),5)


compExportar <- data.frame("Matrices"=ComparacionesParsAsim2$Matrices,"Dietas"=ComparacionesParsAsim2$Dietas,"Número-nodos-relativos"=ComparacionesParsAsim2$NodosRelativos)

write.csv(compExportar,file="ComparacionesParsAsim2.csv")

## Gráficas----
pdf(file="comparacionesParsAsim2.pdf",width = 11)
ggplot(data=ComparacionesParsAsim2, aes(x=Matrices,NodosRelativos, fill=Dietas)) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 1) +
  scale_fill_manual(values=c("mediumorchid2","palegreen2","firebrick1","steelblue1","goldenrod1")) +
  ggtitle("Sensibilidad codificaciones - Parsimonia") +
  ylab("Nodos relativos")+
  theme(axis.text=element_text(size=12),                     axis.title=element_text(size=14),
        
        title = element_text(size = 17), 
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 13))+theme(panel.background = element_rect(fill = "gray94")) + theme(
          panel.grid.major.y = element_line(colour = "gray48", linetype = "dotted"),
          panel.grid.minor.y = element_blank()
        )
dev.off()




# Gráfica adicional 1
## Matriz multiestado----
probMulti.parsAsim2 <- binarizacionMultiParsAsim2
probMulti.parsAsim2$Nodo <- number

probMulti.parsAsim2 <- as.data.frame(melt(probMulti.parsAsim2,id.vars ="Nodo" ))

pdf(file='prob.multi.parsAsim2.pdf')
ggplot(data=probMulti.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  #coord_polar("y", start=0) + facet_wrap(~ Nodo)+
  scale_fill_manual(values=c("mediumorchid2","palegreen1","firebrick1","steelblue1","goldenrod1"))
dev.off()



## Matriz base ----
probMb.parsAsim2 <- data.frame("Car.A"=0,"Car.C"=rep(0,length(mpBaseAsim2$Car)),"Car.P"=0,"Car.E"=0,"Fru.A"=0,"Fru.C"=0,"Fru.P"=0,"Fru.E"=0,"Hem.A"=0,"Hem.C"=0,"Hem.P"=0,"Hem.E"=0,"Ins.A"=0,"Ins.C"=0,"Ins.P"=0,"Ins.E"=0,"Nec.A"=0,"Nec.C"=0,"Nec.P"=0,"Nec.E"=0)

### Carnivoria
#### Ausente
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]==0) {
    probMb.parsAsim2[i,1]=1
  }
}

#### Complementario 
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]==1) {
    probMb.parsAsim2[i,2]=1
  }
}

#### Predominante 
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.CarAsim2[i,1]==2) {
    probMb.parsAsim2[i,3]=1
  }
}

### Frugivoria
#### Ausente
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]==0) {
    probMb.parsAsim2[i,5]=1
  }
}

#### Complementario 
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]==1) {
    probMb.parsAsim2[i,6]=1
  }
}

#### Predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]==2) {
    probMb.parsAsim2[i,7]=1
  }
}

#### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.FruAsim2[i,1]==3) {
    probMb.parsAsim2[i,8]=1
  }
}

### Hematofagia
#### Ausente
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]==0) {
    probMb.parsAsim2[i,9]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]==1) {
    probMb.parsAsim2[i,10]=1
  }
}

#### Predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]==2) {
    probMb.parsAsim2[i,11]=1
  }
}

#### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.HemAsim2[i,1]==3) {
    probMb.parsAsim2[i,12]=1
  }
}

### Insectivoria
#### Ausente
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]==0) {
    probMb.parsAsim2[i,13]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]==1) {
    probMb.parsAsim2[i,14]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]==2) {
    probMb.parsAsim2[i,15]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.InsAsim2[i,1]==3) {
    probMb.parsAsim2[i,16]=1
  }
}

### Nectarivoria
#### Ausente
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.NecAsim2[i,1]==0) {
    probMb.parsAsim2[i,17]=1
  }
}

#### Complementario
for (i in 1:length(mpBaseAsim2$Car)) {
  if (estadosMbPars.NecAsim2[i,1]==1) {
    probMb.parsAsim2[i,18]=1
  }
}

##### Predominante
for (i in 1:length(mpBaseAsim2$Car)) {
  if  (estadosMbPars.NecAsim2[i,1]==2) {
    probMb.parsAsim2[i,19]=1
  }
}

##### Estricto
for (i in 1:length(mpBaseAsim2$Car)) {
  if  (estadosMbPars.NecAsim2[i,1]==3) {
    probMb.parsAsim2[i,20]=1
  }
}




### Carnivoria. De todos modos es 0 :v
probMbCar.parsAsim2 <- data.frame("Ausente"=probMb.parsAsim2[,1],"Complementario"=probMb.parsAsim2[,2],"Predominante"=probMb.parsAsim2[,3])
probMbCar.parsAsim2$Nodo <- number

probMbCar.parsAsim2 <- as.data.frame(melt(probMbCar.parsAsim2,id.vars ="Nodo" ))

pdf(file='probMbCar.bar.parsAsim2.pdf')
ggplot(data=probMbCar.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray86","mediumorchid2","mediumorchid4","green"))
dev.off()


##### Frugivoria
probMbFru.parsAsim2 <- data.frame("Ausente"=probMb.parsAsim2[,5],"Complementario"=probMb.parsAsim2[,6],"Predominante"=probMb.parsAsim2[,7],"Estricto"=probMb.parsAsim2[,8])
probMbFru.parsAsim2$Nodo <- number

probMbFru.parsAsim2 <- as.data.frame(melt(probMbFru.parsAsim2,id.vars ="Nodo" ))

pdf(file='probMbFru.bar.parsAsim2.pdf')
ggplot(data=probMbFru.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probMbHem.parsAsim2 <- data.frame("Ausente"=probMb.parsAsim2[,9],"Complementario"=probMb.parsAsim2[,10],"Predominante"=probMb.parsAsim2[,11],"Estricto"=probMb.parsAsim2[,12])
probMbHem.parsAsim2$Nodo <- number

probMbHem.parsAsim2 <- as.data.frame(melt(probMbHem.parsAsim2,id.vars ="Nodo" ))

pdf(file='probMbHem.bar.parsAsim2.pdf')
ggplot(data=probMbHem.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()

asim2
##### Insectivoria
probMbIns.parsAsim2 <- data.frame("Ausente"=probMb.parsAsim2[,13],"Complementario"=probMb.parsAsim2[,14],"Predominante"=probMb.parsAsim2[,15],"Estricto"=probMb.parsAsim2[,16])
probMbIns.parsAsim2$Nodo <- number

probMbIns.parsAsim2 <- as.data.frame(melt(probMbIns.parsAsim2,id.vars ="Nodo" ))

pdf(file='probMbIns.bar.parsAsim2.pdf')
ggplot(data=probMbIns.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probMbNec.parsAsim2 <- data.frame("Ausente"=probMb.parsAsim2[,17],"Complementario"=probMb.parsAsim2[,18],"Predominante"=probMb.parsAsim2[,19],"Estricto"=probMb.parsAsim2[,20])
probMbNec.parsAsim2$Nodo <- number

probMbNec.parsAsim2 <- as.data.frame(melt(probMbNec.parsAsim2,id.vars ="Nodo" ))

pdf(file='probMbNec.bar.parsAsim2.pdf')
ggplot(data=probMbNec.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 1 ----
### Carnivoria
probB1Car.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB1Pars.CarAsim2[i,1]==0) {
    probB1Car.parsAsim2[i,1]=1
  }
  else if (estadosB1Pars.CarAsim2[i,1]==1) {
    probB1Car.parsAsim2[i,2]=1
  }
}

probB1Car.parsAsim2$Nodo <- number

probB1Car.parsAsim2 <- as.data.frame(melt(probB1Car.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB1Car.bar.parsAsim2.pdf')
ggplot(data=probB1Car.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB1Fru.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB1Pars.FruAsim2[i,1]==0) {
    probB1Fru.parsAsim2[i,1]=1
  }
  else if (estadosB1Pars.FruAsim2[i,1]==1) {
    probB1Fru.parsAsim2[i,2]=1
  }
}

probB1Fru.parsAsim2$Nodo <- number

probB1Fru.parsAsim2 <- as.data.frame(melt(probB1Fru.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB1Fru.bar.parsAsim2.pdf')
ggplot(data=probB1Fru.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB1Hem.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB1Pars.HemAsim2[i,1]==0) {
    probB1Hem.parsAsim2[i,1]=1
  }
  else if (estadosB1Pars.HemAsim2[i,1]==1) {
    probB1Hem.parsAsim2[i,2]=1
  }
}

probB1Hem.parsAsim2$Nodo <- number

probB1Hem.parsAsim2 <- as.data.frame(melt(probB1Hem.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB1Hem.bar.parsAsim2.pdf')
ggplot(data=probB1Hem.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB1Ins.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB1Pars.InsAsim2[i,1]==0) {
    probB1Ins.parsAsim2[i,1]=1
  }
  else if (estadosB1Pars.InsAsim2[i,1]==1) {
    probB1Ins.parsAsim2[i,2]=1
  }
}

probB1Ins.parsAsim2$Nodo <- number

probB1Ins.parsAsim2 <- as.data.frame(melt(probB1Ins.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB1Ins.bar.parsAsim2.pdf')
ggplot(data=probB1Ins.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB1Nec.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB1Pars.NecAsim2[i,1]==0) {
    probB1Nec.parsAsim2[i,1]=1
  }
  else if (estadosB1Pars.NecAsim2[i,1]==1) {
    probB1Nec.parsAsim2[i,2]=1
  }
}

probB1Nec.parsAsim2$Nodo <- number

probB1Nec.parsAsim2 <- as.data.frame(melt(probB1Nec.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB1Nec.bar.parsAsim2.pdf')
ggplot(data=probB1Nec.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()


## Matriz binaria 2----
### Carnivoria
probB2Car.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB2Pars.CarAsim2[i,1]==0) {
    probB2Car.parsAsim2[i,1]=1
  }
  else if (estadosB2Pars.CarAsim2[i,1]==1) {
    probB2Car.parsAsim2[i,2]=1
  }
}

probB2Car.parsAsim2$Nodo <- number

probB2Car.parsAsim2 <- as.data.frame(melt(probB2Car.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB2Car.bar.parsAsim2.pdf')
ggplot(data=probB2Car.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB2Fru.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB2Pars.CarAsim2$estadosB2Pars.CarAsim2)) {
  if (estadosB2Pars.FruAsim2[i,1]==0) {
    probB2Fru.parsAsim2[i,1]=1
  }
  else if (estadosB2Pars.FruAsim2[i,1]==1) {
    probB2Fru.parsAsim2[i,2]=1
  }
}

probB2Fru.parsAsim2$Nodo <- number

probB2Fru.parsAsim2 <- as.data.frame(melt(probB2Fru.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB2Fru.bar.parsAsim2.pdf')
ggplot(data=probB2Fru.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB2Hem.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB2Pars.HemAsim2[i,1]==0) {
    probB2Hem.parsAsim2[i,1]=1
  }
  else if (estadosB2Pars.HemAsim2[i,1]==1) {
    probB2Hem.parsAsim2[i,2]=1
  }
}

probB2Hem.parsAsim2$Nodo <- number

probB2Hem.parsAsim2 <- as.data.frame(melt(probB2Hem.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB2Hem.bar.parsAsim2.pdf')
ggplot(data=probB2Hem.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB2Ins.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB2Pars.InsAsim2[i,1]==0) {
    probB2Ins.parsAsim2[i,1]=1
  }
  else if (estadosB2Pars.InsAsim2[i,1]==1) {
    probB2Ins.parsAsim2[i,2]=1
  }
}

probB2Ins.parsAsim2$Nodo <- number

probB2Ins.parsAsim2 <- as.data.frame(melt(probB2Ins.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB2Ins.bar.parsAsim2.pdf')
ggplot(data=probB2Ins.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB2Nec.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB2Pars.NecAsim2[i,1]==0) {
    probB2Nec.parsAsim2[i,1]=1
  }
  else if (estadosB2Pars.NecAsim2[i,1]==1) {
    probB2Nec.parsAsim2[i,2]=1
  }
}

probB2Nec.parsAsim2$Nodo <- number

probB2Nec.parsAsim2 <- as.data.frame(melt(probB2Nec.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB2Nec.bar.parsAsim2.pdf')
ggplot(data=probB2Nec.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()




## Matriz binaria 3----
### Carnivoria
probB3Car.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB3Pars.CarAsim2[i,1]==0) {
    probB3Car.parsAsim2[i,1]=1
  }
  else if (estadosB3Pars.CarAsim2[i,1]==1) {
    probB3Car.parsAsim2[i,2]=1
  }
}

probB3Car.parsAsim2$Nodo <- number

probB3Car.parsAsim2 <- as.data.frame(melt(probB3Car.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB3Car.bar.parsAsim2.pdf')
ggplot(data=probB3Car.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","mediumorchid2","mediumorchid4"))
dev.off()


##### Frugivoria
probB3Fru.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB2Pars.CarAsim2$estadosB2Pars.CarAsim2)) {
  if (estadosB3Pars.FruAsim2[i,1]==0) {
    probB3Fru.parsAsim2[i,1]=1
  }
  else if (estadosB3Pars.FruAsim2[i,1]==1) {
    probB3Fru.parsAsim2[i,2]=1
  }
}

probB3Fru.parsAsim2$Nodo <- number

probB3Fru.parsAsim2 <- as.data.frame(melt(probB3Fru.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB3Fru.bar.parsAsim2.pdf')
ggplot(data=probB3Fru.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values=c("gray90","palegreen","palegreen3","palegreen4"))
dev.off()


##### Hematofagia
probB3Hem.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB3Pars.HemAsim2[i,1]==0) {
    probB3Hem.parsAsim2[i,1]=1
  }
  else if (estadosB3Pars.HemAsim2[i,1]==1) {
    probB3Hem.parsAsim2[i,2]=1
  }
}

probB3Hem.parsAsim2$Nodo <- number

probB3Hem.parsAsim2 <- as.data.frame(melt(probB3Hem.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB3Hem.bar.parsAsim2.pdf')
ggplot(data=probB3Hem.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","firebrick1","firebrick3","firebrick4"))
dev.off()


##### Insectivoria
probB3Ins.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB3Pars.InsAsim2[i,1]==0) {
    probB3Ins.parsAsim2[i,1]=1
  }
  else if (estadosB3Pars.InsAsim2[i,1]==1) {
    probB3Ins.parsAsim2[i,2]=1
  }
}

probB3Ins.parsAsim2$Nodo <- number

probB3Ins.parsAsim2 <- as.data.frame(melt(probB3Ins.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB3Ins.bar.parsAsim2.pdf')
ggplot(data=probB3Ins.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","steelblue1","steelblue3","steelblue4"))
dev.off()


##### Nectarivoria
probB3Nec.parsAsim2 <- data.frame("Ausente"=rep(0,length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),"Presente"=0)

for (i in 1:length(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)) {
  if (estadosB3Pars.NecAsim2[i,1]==0) {
    probB3Nec.parsAsim2[i,1]=1
  }
  else if (estadosB3Pars.NecAsim2[i,1]==1) {
    probB3Nec.parsAsim2[i,2]=1
  }
}

probB3Nec.parsAsim2$Nodo <- number

probB3Nec.parsAsim2 <- as.data.frame(melt(probB3Nec.parsAsim2,id.vars ="Nodo" ))

pdf(file='probB3Nec.bar.parsAsim2.pdf')
ggplot(data=probB3Nec.parsAsim2, aes(x=Nodo,value, fill=variable)) +
  geom_bar(stat="identity", width=1)+
  scale_fill_manual(values=c("gray90","goldenrod1","goldenrod3","goldenrod4"))
dev.off()



# Máximas probabilidades----
mpGrafica2ParsAsim2 <- data.frame("Reconstrucción"=c(rep("Multi",5),rep(c("BaseCar","BaseFru","BaseHem","BaseIns","BaseNec") ,each=4),"B1Car","B1Car","B1Fru","B1Fru","B1Hem","B1Hem","B1Ins","B1Ins","B1Nec","B1Nec","B2Car","B2Car","B2Fur","B2Fur","B2Hem","B2Hem","B2Ins","B2Ins","B2Nec","B2Nec","B3Car","B3Car","B3Fur","B3Fur","B3Hem","B3Hem","B3Ins","B3Ins","B3Nec","B3Nec"),"Estado"=c("Car","Fru","Hem","Ins","Nec",rep(c("Aus","Comp","Pred","Est"),5),rep(c("Aus","Pres"),15)),"Suma"=0)

filas <- length(probB1Car.parsAsim1$Nodo)/2
filas2 <- length(probB1Car.parsAsim1$Nodo)

mpGrafica2ParsAsim2$Suma <- c(sum(binarizacionMultiParsAsim2$Carnivoria),sum(binarizacionMultiParsAsim2$Frugivoria),sum(binarizacionMultiParsAsim2$Hematofagia),sum(binarizacionMultiParsAsim2$Insectivoria),sum(binarizacionMultiParsAsim2$Nectarivoria)
                              ,sum(probMb.parsAsim2$Car.A),sum(probMb.parsAsim2$Car.C),sum(probMb.parsAsim2$Car.P),sum(probMb.parsAsim2$Car.E),sum(probMb.parsAsim2$Fru.A),sum(probMb.parsAsim2$Fru.C),sum(probMb.parsAsim2$Fru.P),sum(probMb.parsAsim2$Fru.E),sum(probMb.parsAsim2$Hem.A),sum(probMb.parsAsim2$Hem.C),sum(probMb.parsAsim2$Hem.P),sum(probMb.parsAsim2$Hem.E),sum(probMb.parsAsim2$Ins.A),sum(probMb.parsAsim2$Ins.C),sum(probMb.parsAsim2$Ins.P),sum(probMb.parsAsim2$Ins.E),sum(probMb.parsAsim2$Nec.A),sum(probMb.parsAsim2$Nec.C),sum(probMb.parsAsim2$Nec.P),sum(probMb.parsAsim2$Nec.E)
                              
                              ,sum(probB1Car.parsAsim2[1:filas,3]),sum(probB1Car.parsAsim2[(filas+1):filas2,3]),sum(probB1Fru.parsAsim2[1:filas,3]),sum(probB1Fru.parsAsim2[(filas+1):filas2,3]),sum(probB1Hem.parsAsim2[1:filas,3]),sum(probB1Hem.parsAsim2[(filas+1):filas2,3]),sum(probB1Ins.parsAsim2[1:filas,3]),sum(probB1Ins.parsAsim2[(filas+1):filas2,3]),sum(probB1Nec.parsAsim2[1:filas,3]),sum(probB1Nec.parsAsim2[(filas+1):filas2,3])
                              
                              ,sum(probB2Car.parsAsim2[1:filas,3]),sum(probB2Car.parsAsim2[(filas+1):filas2,3]),sum(probB2Fru.parsAsim2[1:filas,3]),sum(probB2Fru.parsAsim2[(filas+1):filas2,3]),sum(probB2Hem.parsAsim2[1:filas,3]),sum(probB2Hem.parsAsim2[(filas+1):filas2,3]),sum(probB2Ins.parsAsim2[1:filas,3]),sum(probB2Ins.parsAsim2[(filas+1):filas2,3]),sum(probB2Nec.parsAsim2[1:filas,3]),sum(probB2Nec.parsAsim2[(filas+1):filas2,3])
                              
                              ,sum(probB3Car.parsAsim2[1:filas,3]),sum(probB3Car.parsAsim2[(filas+1):filas2,3]),sum(probB3Fru.parsAsim2[1:filas,3]),sum(probB3Fru.parsAsim2[(filas+1):filas2,3]),sum(probB3Hem.parsAsim2[1:filas,3]),sum(probB3Hem.parsAsim2[(filas+1):filas2,3]),sum(probB3Ins.parsAsim2[1:filas,3]),sum(probB3Ins.parsAsim2[(filas+1):filas2,3]),sum(probB3Nec.parsAsim2[1:filas,3]),sum(probB3Nec.parsAsim2[(filas+1):filas2,3]))

write.csv(mpGrafica2ParsAsim2,file="estadosParsAsim2.csv")

#----


############################################################             COMPARACIÓN PARS VS MV 

# ER 

#Recopilación de las comparaciones y cálculo de número de nodos relativos - Pars vs Mv ER ----
ComparacionesParsVsMvER <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))

ComparacionesParsVsMvER$NodosCompartidos <- c(sum(compMkvsParsB1ER$Carnivoria),sum(compMkvsParsB1ER$Frugivoria),sum(compMkvsParsB1ER$Hematofagia),sum(compMkvsParsB1ER$Insectivoria),sum(compMkvsParsB1ER$Nectarivoria),sum(compMkvsParsB2ER$Carnivoria),sum(compMkvsParsB2ER$Frugivoria),sum(compMkvsParsB2ER$Hematofagia),sum(compMkvsParsB2ER$Insectivoria),sum(compMkvsParsB2ER$Nectarivoria),sum(compMkvsParsB3ER$Carnivoria),sum(compMkvsParsB3ER$Frugivoria),sum(compMkvsParsB3ER$Hematofagia),sum(compMkvsParsB3ER$Insectivoria),sum(compMkvsParsB3ER$Nectarivoria),sum(compMkvsParsMbER$Carnivoria),sum(compMkvsParsMbER$Frugivoria),sum(compMkvsParsMbER$Hematofagia),sum(compMkvsParsMbER$Insectivoria),sum(compMkvsParsMbER$Nectarivoria),sum(compMkvsParsMultiER$Carnivoria),sum(compMkvsParsMultiER$Frugivoria),sum(compMkvsParsMultiER$Hematofagia),sum(compMkvsParsMultiER$Insectivoria),sum(compMkvsParsMultiER$Nectarivoria))


ComparacionesParsVsMvER$NodosTotales <- c(max(sum(mp1$Car),sum(estadosB1Pars.Car$estadosB1Pars.Car)),max(sum(mp1$Fru),sum(estadosB1Pars.Fru$estadosB1Pars.Fru)),max(sum(mp1$Hem),sum(estadosB1Pars.Hem$estadosB1Pars.Hem)),max(sum(mp1$Ins),sum(estadosB1Pars.Ins$estadosB1Pars.Ins)),max(sum(mp1$Nec),sum(estadosB1Pars.Nec$estadosB1Pars.Nec)),
                                          max(sum(mp2$Car),sum(estadosB2Pars.Car$estadosB2Pars.Car)),max(sum(mp2$Fru),sum(estadosB2Pars.Fru$estadosB2Pars.Fru)),max(sum(mp2$Hem),sum(estadosB2Pars.Hem$estadosB2Pars.Hem)),max(sum(mp2$Ins),sum(estadosB2Pars.Ins$estadosB2Pars.Ins)),max(sum(mp2$Nec),sum(estadosB2Pars.Nec$estadosB2Pars.Nec)),
                                          max(sum(mp3$Car),sum(estadosB3Pars.Car$estadosB3Pars.Car)),max(sum(mp3$Fru),sum(estadosB3Pars.Fru$estadosB3Pars.Fru)),max(sum(mp3$Hem),sum(estadosB3Pars.Hem$estadosB3Pars.Hem)),max(sum(mp3$Ins),sum(estadosB3Pars.Ins$estadosB3Pars.Ins)),max(sum(mp3$Nec),sum(estadosB3Pars.Nec$estadosB3Pars.Nec)),
                                          max(sum(mpBaseBin$Car.EPC),sum(binarizacionBasePars$Car.CP)),max(sum(mpBaseBin$Fru.ECP),sum(binarizacionBasePars$Fru.ECP)),max(sum(mpBaseBin$Hem.CP),sum(binarizacionBasePars$Hem.CP)),max(sum(mpBaseBin$Ins.ECP),sum(binarizacionBasePars$Ins.ECP)),max(sum(mpBaseBin$Nec.ECP),sum(binarizacionBasePars$Nec.ECP)),
                                          max(sum(mpMulti$Carnivoria),sum(binarizacionMultiPars$Carnivoria)),max(sum(mpMulti$Frugivoria),sum(binarizacionMultiPars$Frugivoria)),max(sum(mpMulti$Hematofagia),sum(binarizacionMultiPars$Hematofagia)),max(sum(mpMulti$Insectivoria),sum(binarizacionMultiPars$Insectivoria)),max(sum(mpMulti$Nectarivoria),sum(binarizacionMultiPars$Nectarivoria)))

ComparacionesParsVsMvER$Nodos = ComparacionesParsVsMvER$NodosCompartidos/ComparacionesParsVsMvER$NodosTotales

ComparacionesParsVsMvER[is.na(ComparacionesParsVsMvER)] <- 0

ComparacionesParsVsMvER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesParsVsMvER$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expowrtq <- data.frame("Comparaciones"=ComparacionesParsVsMvER$Matrices,"Dietas"=ComparacionesParsVsMvER$Dieta,"Número-nodos-relativos"=ComparacionesParsVsMvER$Nodos)

write.csv(expowrtq,file = "ComparacionesParsVsMvER.csv")
#----

# DR1

# Recopilación de las comparaciones y cálculo de número de nodos relativos - Pars vs Mv DR1----
ComparacionesAsim1ParsVsMv <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))


ComparacionesAsim1ParsVsMv$NodosCompartidos <- c(sum(compParsVsMv.DR1.B1$Car),sum(compParsVsMv.DR1.B1$Fru),sum(compParsVsMv.DR1.B1$Hem),sum(compParsVsMv.DR1.B1$Ins),sum(compParsVsMv.DR1.B1$Nec),sum(compParsVsMv.DR1.B2$Car),sum(compParsVsMv.DR1.B2$Fru),sum(compParsVsMv.DR1.B2$Hem),sum(compParsVsMv.DR1.B2$Ins),sum(compParsVsMv.DR1.B2$Nec),sum(compParsVsMv.DR1.B3$Car),sum(compParsVsMv.DR1.B3$Fru),sum(compParsVsMv.DR1.B3$Hem),sum(compParsVsMv.DR1.B3$Ins),sum(compParsVsMv.DR1.B3$Nec),sum(compParsVsMv.DR1.Mb$Car),sum(compParsVsMv.DR1.Mb$Fru),sum(compParsVsMv.DR1.Mb$Hem),sum(compParsVsMv.DR1.Mb$Ins),sum(compParsVsMv.DR1.Mb$Nec),sum(compParsVsMv.DR1.Multi$Car),sum(compParsVsMv.DR1.Multi$Fru),sum(compParsVsMv.DR1.Multi$Hem),sum(compParsVsMv.DR1.Multi$Ins),sum(compParsVsMv.DR1.Multi$Nec))


ComparacionesAsim1ParsVsMv$NodosTotales <- c(max(sum(mp1Asim1$Car),sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1)),max(sum(mp1Asim1$Fru),sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1)),max(sum(mp1Asim1$Hem),sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1)),max(sum(mp1Asim1$Ins),sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1)),max(sum(mp1Asim1$Nec),sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1)),
                                             max(sum(mp2Asim1$Car),sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1)),max(sum(mp2Asim1$Fru),sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1)),max(sum(mp2Asim1$Hem),sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1)),max(sum(mp2Asim1$Ins),sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1)),max(sum(mp2Asim1$Nec),sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1)),
                                             max(sum(mp3Asim1$Car),sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1)),max(sum(mp3Asim1$Fru),sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1)),max(sum(mp3Asim1$Hem),sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1)),max(sum(mp3Asim1$Ins),sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1)),max(sum(mp3Asim1$Nec),sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1)),
                                             max(sum(mpBaseBinAsim1$Car.CP),sum(binarizacionBaseParsAsim1$Car.CP)),max(sum(mpBaseBinAsim1$Fru.ECP),sum(binarizacionBaseParsAsim1$Fru.ECP)),max(sum(mpBaseBinAsim1$Hem.CP),sum(binarizacionBaseParsAsim1$Hem.CP)),max(sum(mpBaseBinAsim1$Ins.ECP),sum(binarizacionBaseParsAsim1$Ins.ECP)),max(sum(mpBaseBinAsim1$Nec.ECP),sum(binarizacionBaseParsAsim1$Nec.ECP)),
                                             max(sum(mpMultiAsim1$Carnivoria),sum(binarizacionMultiParsAsim1$Carnivoria)),max(sum(mpMultiAsim1$Frugivoria),sum(binarizacionMultiParsAsim1$Frugivoria)),max(sum(mpMultiAsim1$Hematofagia),sum(binarizacionMultiParsAsim1$Hematofagia)),max(sum(mpMultiAsim1$Insectivoria),sum(binarizacionMultiParsAsim1$Insectivoria)),max(sum(mpMultiAsim1$Nectarivoria),sum(binarizacionMultiParsAsim1$Nectarivoria)))


ComparacionesAsim1ParsVsMv$Nodos = ComparacionesAsim1ParsVsMv$NodosCompartidos/ComparacionesAsim1ParsVsMv$NodosTotales

ComparacionesAsim1ParsVsMv[is.na(ComparacionesAsim1ParsVsMv)] <- 0

ComparacionesAsim1ParsVsMv$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesAsim1ParsVsMv$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expowrt <- data.frame("Comparaciones"=ComparacionesAsim1ParsVsMv$Matrices,"Dietas"=ComparacionesAsim1ParsVsMv$Dieta,"Número-nodos-relativos"=ComparacionesAsim1ParsVsMv$Nodos)

write.csv(expowrt,file = "ComparacionesAsim1ParsVsMv.csv")

#----

# DR2

# Recopilación de las comparaciones y cálculo de número de nodos relativos - Pars vs Mv DR2----
ComparacionesAsim2ParsVsMv <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))


ComparacionesAsim2ParsVsMv$NodosCompartidos <- c(sum(compParsVsMv.DR2.B1$Car),sum(compParsVsMv.DR2.B1$Fru),sum(compParsVsMv.DR2.B1$Hem),sum(compParsVsMv.DR2.B1$Ins),sum(compParsVsMv.DR2.B1$Nec),sum(compParsVsMv.DR2.B2$Car),sum(compParsVsMv.DR2.B2$Fru),sum(compParsVsMv.DR2.B2$Hem),sum(compParsVsMv.DR2.B2$Ins),sum(compParsVsMv.DR2.B2$Nec),sum(compParsVsMv.DR2.B3$Car),sum(compParsVsMv.DR2.B3$Fru),sum(compParsVsMv.DR2.B3$Hem),sum(compParsVsMv.DR2.B3$Ins),sum(compParsVsMv.DR2.B3$Nec),sum(compParsVsMv.DR2.Mb$Car),sum(compParsVsMv.DR2.Mb$Fru),sum(compParsVsMv.DR2.Mb$Hem),sum(compParsVsMv.DR2.Mb$Ins),sum(compParsVsMv.DR2.Mb$Nec),sum(compParsVsMv.DR2.Multi$Car),sum(compParsVsMv.DR2.Multi$Fru),sum(compParsVsMv.DR2.Multi$Hem),sum(compParsVsMv.DR2.Multi$Ins),sum(compParsVsMv.DR2.Multi$Nec))


ComparacionesAsim2ParsVsMv$NodosTotales <- c(max(sum(mp1Asim2$Car),sum(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),max(sum(mp1Asim2$Fru),sum(estadosB1Pars.FruAsim2$estadosB1Pars.FruAsim2)),max(sum(mp1Asim2$Hem),sum(estadosB1Pars.HemAsim2$estadosB1Pars.HemAsim2)),max(sum(mp1Asim2$Ins),sum(estadosB1Pars.InsAsim2$estadosB1Pars.InsAsim2)),max(sum(mp1Asim2$Nec),sum(estadosB1Pars.NecAsim2$estadosB1Pars.NecAsim2)),
                                             max(sum(mp2Asim2$Car),sum(estadosB2Pars.CarAsim2$estadosB2Pars.CarAsim2)),max(sum(mp2Asim2$Fru),sum(estadosB2Pars.FruAsim2$estadosB2Pars.FruAsim2)),max(sum(mp2Asim2$Hem),sum(estadosB2Pars.HemAsim2$estadosB2Pars.HemAsim2)),max(sum(mp2Asim2$Ins),sum(estadosB2Pars.InsAsim2$estadosB2Pars.InsAsim2)),max(sum(mp2Asim2$Nec),sum(estadosB2Pars.NecAsim2$estadosB2Pars.NecAsim2)),
                                             max(sum(mp3Asim2$Car),sum(estadosB3Pars.CarAsim2$estadosB3Pars.CarAsim2)),max(sum(mp3Asim2$Fru),sum(estadosB3Pars.FruAsim2$estadosB3Pars.FruAsim2)),max(sum(mp3Asim2$Hem),sum(estadosB3Pars.HemAsim2$estadosB3Pars.HemAsim2)),max(sum(mp3Asim2$Ins),sum(estadosB3Pars.InsAsim2$estadosB3Pars.InsAsim2)),max(sum(mp3Asim2$Nec),sum(estadosB3Pars.NecAsim2$estadosB3Pars.NecAsim2)),
                                             max(sum(mpBaseBinAsim2$Car.CP),sum(binarizacionBaseParsAsim2$Car.CP)),max(sum(mpBaseBinAsim2$Fru.ECP),sum(binarizacionBaseParsAsim2$Fru.ECP)),max(sum(mpBaseBinAsim2$Hem.CP),sum(binarizacionBaseParsAsim2$Hem.CP)),max(sum(mpBaseBinAsim2$Ins.ECP),sum(binarizacionBaseParsAsim2$Ins.ECP)),max(sum(mpBaseBinAsim2$Nec.ECP),sum(binarizacionBaseParsAsim2$Nec.ECP)),
                                             max(sum(mpMultiAsim2$Carnivoria),sum(binarizacionMultiParsAsim2$Carnivoria)),max(sum(mpMultiAsim2$Frugivoria),sum(binarizacionMultiParsAsim2$Frugivoria)),max(sum(mpMultiAsim2$Hematofagia),sum(binarizacionMultiParsAsim2$Hematofagia)),max(sum(mpMultiAsim2$Insectivoria),sum(binarizacionMultiParsAsim2$Insectivoria)),max(sum(mpMultiAsim2$Nectarivoria),sum(binarizacionMultiParsAsim2$Nectarivoria)))


ComparacionesAsim2ParsVsMv$Nodos = ComparacionesAsim2ParsVsMv$NodosCompartidos/ComparacionesAsim2ParsVsMv$NodosTotales

ComparacionesAsim2ParsVsMv[is.na(ComparacionesAsim2ParsVsMv)] <- 0

ComparacionesAsim2ParsVsMv$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesAsim2ParsVsMv$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expowrti <- data.frame("Comparaciones"=ComparacionesAsim2ParsVsMv$Matrices,"Dietas"=ComparacionesAsim2ParsVsMv$Dieta,"Número-nodos-relativos"=ComparacionesAsim2ParsVsMv$Nodos)

write.csv(expowrti,file = "ComparacionesAsim2ParsVsMv.csv")
#----


############################################################            COMPARACIONES TRANSFORMACIONES


# Máxima verosimilitud

## Comparaciones DR1 vs DR2 ####

ComparacionesAsim1vsAsim2 <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))


ComparacionesAsim1vsAsim2$NodosCompartidos <- c(sum(compB1Asim1vsAsim2$B1Car),sum(compB1Asim1vsAsim2$B1Fru),sum(compB1Asim1vsAsim2$B1Hem),sum(compB1Asim1vsAsim2$B1Ins),sum(compB1Asim1vsAsim2$B1Nec),sum(compB2Asim1vsAsim2$B2Car),sum(compB2Asim1vsAsim2$B2Fru),sum(compB2Asim1vsAsim2$B2Hem),sum(compB2Asim1vsAsim2$B2Ins),sum(compB2Asim1vsAsim2$B2Nec),sum(compB3Asim1vsAsim2$B3Car),sum(compB3Asim1vsAsim2$B3Fru),sum(compB3Asim1vsAsim2$B3Hem),sum(compB3Asim1vsAsim2$B3Ins),sum(compB3Asim1vsAsim2$B3Nec),sum(compMbAsim1vsAsim2$Car),sum(compMbAsim1vsAsim2$Fru),sum(compMbAsim1vsAsim2$Hem),sum(compMbAsim1vsAsim2$Ins),sum(compMbAsim1vsAsim2$Nec),sum(compMultiAsim1vsAsim2$MultiCar),sum(compMultiAsim1vsAsim2$MultiFru),sum(compMultiAsim1vsAsim2$MultiHem),sum(compMultiAsim1vsAsim2$MultiIns),sum(compMultiAsim1vsAsim2$MultiNec))


ComparacionesAsim1vsAsim2$NodosTotales <- c(max(sum(mp1Asim1$Car),sum(mp1Asim2$Car)),max(sum(mp1Asim1$Fru),sum(mp1Asim2$Fru)),max(sum(mp1Asim1$Hem),sum(mp1Asim2$Hem)),max(sum(mp1Asim1$Ins),sum(mp1Asim2$Ins)),max(sum(mp1Asim1$Nec),sum(mp1Asim2$Nec)),
                                            max(sum(mp2Asim1$Car),sum(mp2Asim2$Car)),max(sum(mp2Asim1$Fru),sum(mp2Asim2$Fru)),max(sum(mp2Asim1$Hem),sum(mp2Asim2$Hem)),max(sum(mp2Asim1$Ins),sum(mp2Asim2$Ins)),max(sum(mp2Asim1$Nec),sum(mp2Asim2$Nec)),
                                            max(sum(mp3Asim1$Car),sum(mp3Asim2$Car)),max(sum(mp3Asim1$Fru),sum(mp3Asim2$Fru)),max(sum(mp3Asim1$Hem),sum(mp3Asim2$Hem)),max(sum(mp3Asim1$Ins),sum(mp3Asim2$Ins)),max(sum(mp3Asim1$Nec),sum(mp3Asim2$Nec)),
                                            max(sum(mpBaseBinAsim1$Car.CP),sum(mpBaseBinAsim2$Car.CP)),max(sum(mpBaseBinAsim1$Fru.ECP),sum(mpBaseBinAsim2$Fru.ECP)),max(sum(mpBaseBinAsim1$Hem.CP),sum(mpBaseBinAsim2$Hem.CP)),max(sum(mpBaseBinAsim1$Ins.ECP),sum(mpBaseBinAsim2$Ins.ECP)),max(sum(mpBaseBinAsim1$Nec.ECP),sum(mpBaseBinAsim2$Car.CP)),
                                            max(sum(mpMultiAsim1$Carnivoria),sum(mpMultiAsim2$Carnivoria)),max(sum(mpMultiAsim1$Frugivoria),sum(mpMultiAsim2$Frugivoria)),max(sum(mpMultiAsim1$Hematofagia),sum(mpMultiAsim2$Hematofagia)),max(sum(mpMultiAsim1$Insectivoria),sum(mpMultiAsim2$Insectivoria)),max(sum(mpMultiAsim1$Nectarivoria),sum(mpMultiAsim2$Nectarivoria)))


ComparacionesAsim1vsAsim2$Nodos = ComparacionesAsim1vsAsim2$NodosCompartidos/ComparacionesAsim1vsAsim2$NodosTotales

ComparacionesAsim1vsAsim2[is.na(ComparacionesAsim1vsAsim2)] <- 0

ComparacionesAsim1vsAsim2$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesAsim1vsAsim2$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoort <- data.frame("Comparaciones"=ComparacionesAsim1vsAsim2$Matrices,"Dietas"=ComparacionesAsim1vsAsim2$Dieta,"Número-nodos-relativos"=ComparacionesAsim1vsAsim2$Nodos)

write.csv(expoort,file = "ComparacionesMvAsim1vsAsim2.csv")



## Comparaciones DR1 vs ER ####

ComparacionesAsim1vsER <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))


ComparacionesAsim1vsER$NodosCompartidos <- c(sum(compB1Asim1vsER$B1Car),sum(compB1Asim1vsER$B1Fru),sum(compB1Asim1vsER$B1Hem),sum(compB1Asim1vsER$B1Ins),sum(compB1Asim1vsER$B1Nec),sum(compB2Asim1vsER$B2Car),sum(compB2Asim1vsER$B2Fru),sum(compB2Asim1vsER$B2Hem),sum(compB2Asim1vsER$B2Ins),sum(compB2Asim1vsER$B2Nec),sum(compB3Asim1vsER$B3Car),sum(compB3Asim1vsER$B3Fru),sum(compB3Asim1vsER$B3Hem),sum(compB3Asim1vsER$B3Ins),sum(compB3Asim1vsER$B3Nec),sum(compMbAsim1vsER$Car),sum(compMbAsim1vsER$Fru),sum(compMbAsim1vsER$Hem),sum(compMbAsim1vsER$Ins),sum(compMbAsim1vsER$Nec),sum(compMultiAsim1vsER$MultiCar),sum(compMultiAsim1vsER$MultiFru),sum(compMultiAsim1vsER$MultiHem),sum(compMultiAsim1vsER$MultiIns),sum(compMultiAsim1vsER$MultiNec))


ComparacionesAsim1vsER$NodosTotales <- c(max(sum(mp1Asim1$Car),sum(mp1$Car)),max(sum(mp1Asim1$Fru),sum(mp1$Fru)),max(sum(mp1Asim1$Hem),sum(mp1$Hem)),max(sum(mp1Asim1$Ins),sum(mp1$Ins)),max(sum(mp1Asim1$Nec),sum(mp1$Nec)),
                                         max(sum(mp2Asim1$Car),sum(mp2$Car)),max(sum(mp2Asim1$Fru),sum(mp2$Fru)),max(sum(mp2Asim1$Hem),sum(mp2$Hem)),max(sum(mp2Asim1$Ins),sum(mp2$Ins)),max(sum(mp2Asim1$Nec),sum(mp2$Nec)),
                                         max(sum(mp3Asim1$Car),sum(mp3$Car)),max(sum(mp3Asim1$Fru),sum(mp3$Fru)),max(sum(mp3Asim1$Hem),sum(mp3$Hem)),max(sum(mp3Asim1$Ins),sum(mp3$Ins)),max(sum(mp3Asim1$Nec),sum(mp3$Nec)),
                                         max(sum(mpBaseBinAsim1$Car.CP),sum(mpBaseBin$Car.EPC)),max(sum(mpBaseBinAsim1$Fru.ECP),sum(mpBaseBin$Fru.ECP)),max(sum(mpBaseBinAsim1$Hem.CP),sum(mpBaseBin$Hem.CP)),max(sum(mpBaseBinAsim1$Ins.ECP),sum(mpBaseBin$Ins.ECP)),max(sum(mpBaseBinAsim1$Nec.ECP),sum(mpBaseBin$Car.CP)),
                                         max(sum(mpMultiAsim1$Carnivoria),sum(mpMulti$Carnivoria)),max(sum(mpMultiAsim1$Frugivoria),sum(mpMulti$Frugivoria)),max(sum(mpMultiAsim1$Hematofagia),sum(mpMulti$Hematofagia)),max(sum(mpMultiAsim1$Insectivoria),sum(mpMulti$Insectivoria)),max(sum(mpMultiAsim1$Nectarivoria),sum(mpMulti$Nectarivoria)))


ComparacionesAsim1vsER$Nodos = ComparacionesAsim1vsER$NodosCompartidos/ComparacionesAsim1vsER$NodosTotales

ComparacionesAsim1vsER[is.na(ComparacionesAsim1vsER)] <- 0

ComparacionesAsim1vsER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesAsim1vsER$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoortt <- data.frame("Comparaciones"=ComparacionesAsim1vsER$Matrices,"Dietas"=ComparacionesAsim1vsER$Dieta,"Número-nodos-relativos"=ComparacionesAsim1vsER$Nodos)

write.csv(expoortt,file = "ComparacionesMvAsim1vsER.csv")


## Comparaciones DR2 vs ER----

ComparacionesAsim2vsER <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))


ComparacionesAsim2vsER$NodosCompartidos <- c(sum(compB1Asim2vsER$B1Car),sum(compB1Asim2vsER$B1Fru),sum(compB1Asim2vsER$B1Hem),sum(compB1Asim2vsER$B1Ins),sum(compB1Asim2vsER$B1Nec),sum(compB2Asim2vsER$B2Car),sum(compB2Asim2vsER$B2Fru),sum(compB2Asim2vsER$B2Hem),sum(compB2Asim2vsER$B2Ins),sum(compB2Asim2vsER$B2Nec),sum(compB3Asim2vsER$B3Car),sum(compB3Asim2vsER$B3Fru),sum(compB3Asim2vsER$B3Hem),sum(compB3Asim2vsER$B3Ins),sum(compB3Asim2vsER$B3Nec),sum(compMbAsim1vsER$Car),sum(compMbAsim1vsER$Fru),sum(compMbAsim1vsER$Hem),sum(compMbAsim1vsER$Ins),sum(compMbAsim1vsER$Nec),sum(compMultiAsim1vsER$MultiCar),sum(compMultiAsim1vsER$MultiFru),sum(compMultiAsim1vsER$MultiHem),sum(compMultiAsim1vsER$MultiIns),sum(compMultiAsim1vsER$MultiNec))


ComparacionesAsim2vsER$NodosTotales <- c(max(sum(mp1Asim2$Car),sum(mp1$Car)),max(sum(mp1Asim2$Fru),sum(mp1$Fru)),max(sum(mp1Asim2$Hem),sum(mp1$Hem)),max(sum(mp1Asim2$Ins),sum(mp1$Ins)),max(sum(mp1Asim2$Nec),sum(mp1$Nec)),
                                         max(sum(mp2Asim2$Car),sum(mp2$Car)),max(sum(mp2Asim2$Fru),sum(mp2$Fru)),max(sum(mp2Asim2$Hem),sum(mp2$Hem)),max(sum(mp2Asim2$Ins),sum(mp2$Ins)),max(sum(mp2Asim2$Nec),sum(mp2$Nec)),
                                         max(sum(mp3Asim2$Car),sum(mp3$Car)),max(sum(mp3Asim2$Fru),sum(mp3$Fru)),max(sum(mp3Asim2$Hem),sum(mp3$Hem)),max(sum(mp3Asim2$Ins),sum(mp3$Ins)),max(sum(mp3Asim2$Nec),sum(mp3$Nec)),
                                         max(sum(mpBaseBinAsim2$Car.CP),sum(mpBaseBin$Car.EPC)),max(sum(mpBaseBinAsim2$Fru.ECP),sum(mpBaseBin$Fru.ECP)),max(sum(mpBaseBinAsim2$Hem.CP),sum(mpBaseBin$Hem.CP)),max(sum(mpBaseBinAsim2$Ins.ECP),sum(mpBaseBin$Ins.ECP)),max(sum(mpBaseBinAsim2$Nec.ECP),sum(mpBaseBin$Car.CP)),
                                         max(sum(mpMultiAsim2$Carnivoria),sum(mpMulti$Carnivoria)),max(sum(mpMultiAsim2$Frugivoria),sum(mpMulti$Frugivoria)),max(sum(mpMultiAsim2$Hematofagia),sum(mpMulti$Hematofagia)),max(sum(mpMultiAsim2$Insectivoria),sum(mpMulti$Insectivoria)),max(sum(mpMultiAsim2$Nectarivoria),sum(mpMulti$Nectarivoria)))


ComparacionesAsim2vsER$Nodos = ComparacionesAsim2vsER$NodosCompartidos/ComparacionesAsim2vsER$NodosTotales

ComparacionesAsim2vsER[is.na(ComparacionesAsim2vsER)] <- 0

ComparacionesAsim2vsER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesAsim2vsER$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoorte <- data.frame("Comparaciones"=ComparacionesAsim2vsER$Matrices,"Dietas"=ComparacionesAsim2vsER$Dieta,"Número-nodos-relativos"=ComparacionesAsim2vsER$Nodos)

write.csv(expoorte,file = "ComparacionesMvAsim2vsER.csv")


#----

# Parsimonia

## Comparaciones DR1 vs DR2 ----
ComparacionesParsAsim1vsAsim2 <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))

compB2Asim1vsAsim2Pars
ComparacionesParsAsim1vsAsim2$NodosCompartidos <- c(sum(compB1Asim1vsAsim2Pars$B1Car),sum(compB1Asim1vsAsim2Pars$B1Fru),sum(compB1Asim1vsAsim2Pars$B1Hem),sum(compB1Asim1vsAsim2Pars$B1Ins),sum(compB1Asim1vsAsim2Pars$B1Nec),sum(compB2Asim1vsAsim2Pars$B2Car),sum(compB2Asim1vsAsim2Pars$B2Fru),sum(compB2Asim1vsAsim2Pars$B2Hem),sum(compB2Asim1vsAsim2Pars$B2Ins),sum(compB2Asim1vsAsim2Pars$B2Nec),sum(compB3Asim1vsAsim2Pars$B3Car),sum(compB3Asim1vsAsim2Pars$B3Fru),sum(compB3Asim1vsAsim2Pars$B3Hem),sum(compB3Asim1vsAsim2Pars$B3Ins),sum(compB3Asim1vsAsim2Pars$B3Nec),sum(compMbAsim1vsAsim2Pars$Car),sum(compMbAsim1vsAsim2Pars$Fru),sum(compMbAsim1vsAsim2Pars$Hem),sum(compMbAsim1vsAsim2Pars$Ins),sum(compMbAsim1vsAsim2Pars$Nec),sum(compMultiAsim1vsAsim2Pars$MultiCar),sum(compMultiAsim1vsAsim2Pars$MultiFru),sum(compMultiAsim1vsAsim2Pars$MultiHem),sum(compMultiAsim1vsAsim2Pars$MultiIns),sum(compMultiAsim1vsAsim2Pars$MultiNec))


ComparacionesParsAsim1vsAsim2$NodosTotales <- c(max(sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1),sum(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2)),max(sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1),sum(estadosB1Pars.FruAsim2$estadosB1Pars.FruAsim2)),max(sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1),sum(estadosB1Pars.HemAsim2$estadosB1Pars.HemAsim2)),max(sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1),sum(estadosB1Pars.InsAsim2$estadosB1Pars.InsAsim2)),max(sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1),sum(estadosB1Pars.NecAsim2$estadosB1Pars.NecAsim2)),max(sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1),sum(estadosB2Pars.CarAsim2$estadosB2Pars.CarAsim2)),max(sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1),sum(estadosB2Pars.FruAsim2$estadosB2Pars.FruAsim2)),max(sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1),sum(estadosB2Pars.HemAsim2$estadosB2Pars.HemAsim2)),max(sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1),sum(estadosB2Pars.InsAsim2$estadosB2Pars.InsAsim2)),max(sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1),sum(estadosB2Pars.NecAsim2$estadosB2Pars.NecAsim2)),max(sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1),sum(estadosB3Pars.CarAsim2$estadosB3Pars.CarAsim2)),max(sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1),sum(estadosB3Pars.FruAsim2$estadosB3Pars.FruAsim2)),max(sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1),sum(estadosB3Pars.HemAsim2$estadosB3Pars.HemAsim2)),max(sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1),sum(estadosB3Pars.InsAsim2$estadosB3Pars.InsAsim2)),max(sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1),sum(estadosB3Pars.NecAsim2$estadosB3Pars.NecAsim2)),max(sum(binarizacionBaseParsAsim1$Car.CP),sum(binarizacionBaseParsAsim2$Car.CP)),max(sum(binarizacionBaseParsAsim1$Fru.ECP),sum(binarizacionBaseParsAsim2$Fru.ECP)),max(sum(binarizacionBaseParsAsim1$Hem.CP),sum(binarizacionBaseParsAsim2$Hem.CP)),max(sum(binarizacionBaseParsAsim1$Ins.ECP),sum(binarizacionBaseParsAsim2$Ins.ECP)),max(sum(binarizacionBaseParsAsim1$Nec.ECP),sum(binarizacionBaseParsAsim2$Nec.ECP)),max(sum(binarizacionMultiParsAsim1$Carnivoria),sum(binarizacionMultiParsAsim2$Carnivoria)),max(sum(binarizacionMultiParsAsim1$Frugivoria),sum(binarizacionMultiParsAsim2$Frugivoria)),max(sum(binarizacionMultiParsAsim1$Hematofagia),sum(binarizacionMultiParsAsim2$Hematofagia)),max(sum(binarizacionMultiParsAsim1$Insectivoria),sum(binarizacionMultiParsAsim2$Insectivoria)),max(sum(binarizacionMultiParsAsim1$Nectarivoria),sum(binarizacionMultiParsAsim2$Nectarivoria)))


ComparacionesParsAsim1vsAsim2$Nodos = ComparacionesParsAsim1vsAsim2$NodosCompartidos/ComparacionesParsAsim1vsAsim2$NodosTotales

ComparacionesParsAsim1vsAsim2[is.na(ComparacionesParsAsim1vsAsim2)] <- 0

ComparacionesParsAsim1vsAsim2$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesParsAsim1vsAsim2$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoortaa <- data.frame("Comparaciones"=ComparacionesParsAsim1vsAsim2$Matrices,"Dietas"=ComparacionesParsAsim1vsAsim2$Dieta,"Número-nodos-relativos"=ComparacionesParsAsim1vsAsim2$Nodos)

write.csv(expoortaa,file = "ComparacionesParsAsim1vsAsim2.csv")

## Comparaciones DR1 vs ER ----

ComparacionesParsAsim1vsER <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))
compB3Asim1vsERPars
ComparacionesParsAsim1vsER$NodosCompartidos <- c(sum(compB1Asim1vsERPars$B1Car),sum(compB1Asim1vsERPars$B1Fru),sum(compB1Asim1vsERPars$B1Hem),sum(compB1Asim1vsERPars$B1Ins),sum(compB1Asim1vsERPars$B1Nec),sum(compB2Asim1vsERPars$B2Car),sum(compB2Asim1vsERPars$B2Fru),sum(compB2Asim1vsERPars$B2Hem),sum(compB2Asim1vsERPars$B2Ins),sum(compB2Asim1vsERPars$B2Nec),sum(compB3Asim1vsERPars$B3Car),sum(compB3Asim1vsERPars$B3Fru),sum(compB3Asim1vsERPars$B3Hem),sum(compB3Asim1vsERPars$B3Ins),sum(compB3Asim1vsERPars$B3Nec),sum(compMbAsim1vsERPars$Car),sum(compMbAsim1vsERPars$Fru),sum(compMbAsim1vsERPars$Hem),sum(compMbAsim1vsERPars$Ins),sum(compMbAsim1vsERPars$Nec),sum(compMultiAsim1vsERPars$MultiCar),sum(compMultiAsim1vsERPars$MultiFru),sum(compMultiAsim1vsERPars$MultiHem),sum(compMultiAsim1vsERPars$MultiIns),sum(compMultiAsim1vsERPars$MultiNec))


ComparacionesParsAsim1vsER$NodosTotales <- c(max(sum(estadosB1Pars.CarAsim1$estadosB1Pars.CarAsim1),sum(estadosB1Pars.Car$estadosB1Pars.Car)),max(sum(estadosB1Pars.FruAsim1$estadosB1Pars.FruAsim1),sum(estadosB1Pars.Fru$estadosB1Pars.Fru)),max(sum(estadosB1Pars.HemAsim1$estadosB1Pars.HemAsim1),sum(estadosB1Pars.Hem$estadosB1Pars.Hem)),max(sum(estadosB1Pars.InsAsim1$estadosB1Pars.InsAsim1),sum(estadosB1Pars.Ins$estadosB1Pars.Ins)),max(sum(estadosB1Pars.NecAsim1$estadosB1Pars.NecAsim1),sum(estadosB1Pars.Nec$estadosB1Pars.Nec)),max(sum(estadosB2Pars.CarAsim1$estadosB2Pars.CarAsim1),sum(estadosB2Pars.Car$estadosB2Pars.Car)),max(sum(estadosB2Pars.FruAsim1$estadosB2Pars.FruAsim1),sum(estadosB2Pars.Fru$estadosB2Pars.Fru)),max(sum(estadosB2Pars.HemAsim1$estadosB2Pars.HemAsim1),sum(estadosB2Pars.Hem$estadosB2Pars.Hem)),max(sum(estadosB2Pars.InsAsim1$estadosB2Pars.InsAsim1),sum(estadosB2Pars.Ins$estadosB2Pars.Ins)),max(sum(estadosB2Pars.NecAsim1$estadosB2Pars.NecAsim1),sum(estadosB2Pars.Nec$estadosB2Pars.Nec)),max(sum(estadosB3Pars.CarAsim1$estadosB3Pars.CarAsim1),sum(estadosB3Pars.Car$estadosB3Pars.Car)),max(sum(estadosB3Pars.FruAsim1$estadosB3Pars.FruAsim1),sum(estadosB3Pars.Fru$estadosB3Pars.Fru)),max(sum(estadosB3Pars.HemAsim1$estadosB3Pars.HemAsim1),sum(estadosB3Pars.Hem$estadosB3Pars.Hem)),max(sum(estadosB3Pars.InsAsim1$estadosB3Pars.InsAsim1),sum(estadosB3Pars.Ins$estadosB3Pars.Ins)),max(sum(estadosB3Pars.NecAsim1$estadosB3Pars.NecAsim1),sum(estadosB3Pars.Nec$estadosB3Pars.Nec)),max(sum(binarizacionBaseParsAsim1$Car.CP),sum(binarizacionBasePars$Car.CP)),max(sum(binarizacionBaseParsAsim1$Fru.ECP),sum(binarizacionBasePars$Fru.ECP)),max(sum(binarizacionBaseParsAsim1$Hem.CP),sum(binarizacionBasePars$Hem.CP)),max(sum(binarizacionBaseParsAsim1$Ins.ECP),sum(binarizacionBasePars$Ins.ECP)),max(sum(binarizacionBaseParsAsim1$Nec.ECP),sum(binarizacionBasePars$Nec.ECP)),max(sum(binarizacionMultiParsAsim1$Carnivoria),sum(binarizacionMultiPars$Carnivoria)),max(sum(binarizacionMultiParsAsim1$Frugivoria),sum(binarizacionMultiPars$Frugivoria)),max(sum(binarizacionMultiParsAsim1$Hematofagia),sum(binarizacionMultiPars$Hematofagia)),max(sum(binarizacionMultiParsAsim1$Insectivoria),sum(binarizacionMultiPars$Insectivoria)),max(sum(binarizacionMultiParsAsim1$Nectarivoria),sum(binarizacionMultiPars$Nectarivoria)))


ComparacionesParsAsim1vsER$Nodos = ComparacionesParsAsim1vsER$NodosCompartidos/ComparacionesParsAsim1vsER$NodosTotales

ComparacionesParsAsim1vsER[is.na(ComparacionesParsAsim1vsER)] <- 0

ComparacionesParsAsim1vsER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesParsAsim1vsER$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoorto <- data.frame("Comparaciones"=ComparacionesParsAsim1vsER$Matrices,"Dietas"=ComparacionesParsAsim1vsER$Dieta,"Número-nodos-relativos"=ComparacionesParsAsim1vsER$Nodos)

write.csv(expoorto,file = "ComparacionesParsAsim1vsER.csv")


## Comparaciones DR2 vs ER ----

ComparacionesParsAsim2vsER <- data.frame("Comparaciones"=c("B1.Car","B1.Fru","B1.Hem","B1.Ins","B1.Nec","B2.Car","B2.Fru","B2.Hem","B2.Ins","B2.Nec","B3.Car","B3.Fru","B3.Hem","B3.Ins","B3.Nec","Mb.Car","Mb.Fru","Mb.Hem","Mb.Ins","Mb.Nec","Multi.Car","Multi.Fru","Multi.Hem","Multi.Ins","Multi.Nec"))

ComparacionesParsAsim2vsER$NodosCompartidos <- c(sum(compERB1vsAsim2Pars$B1Car),sum(compERB1vsAsim2Pars$B1Fru),sum(compERB1vsAsim2Pars$B1Hem),sum(compERB1vsAsim2Pars$B1Ins),sum(compERB1vsAsim2Pars$B1Nec),sum(compB2ERvsAsim2Pars$B2Car),sum(compB2ERvsAsim2Pars$B2Fru),sum(compB2ERvsAsim2Pars$B2Hem),sum(compB2ERvsAsim2Pars$B2Ins),sum(compB2ERvsAsim2Pars$B2Nec),sum(compB3ERvsAsim2Pars$B3Car),sum(compB3ERvsAsim2Pars$B3Fru),sum(compB3ERvsAsim2Pars$B3Hem),sum(compB3ERvsAsim2Pars$B3Ins),sum(compB3ERvsAsim2Pars$B3Nec),sum(compMbERvsAsim2Pars$Car),sum(compMbERvsAsim2Pars$Fru),sum(compMbERvsAsim2Pars$Hem),sum(compMbERvsAsim2Pars$Ins),sum(compMbERvsAsim2Pars$Nec),sum(compMultiERvsAsim2Pars$MultiCar),sum(compMultiERvsAsim2Pars$MultiFru),sum(compMultiERvsAsim2Pars$MultiHem),sum(compMultiERvsAsim2Pars$MultiIns),sum(compMultiERvsAsim2Pars$MultiNec))


ComparacionesParsAsim2vsER$NodosTotales <- c(max(sum(estadosB1Pars.CarAsim2$estadosB1Pars.CarAsim2),sum(estadosB1Pars.Car$estadosB1Pars.Car)),max(sum(estadosB1Pars.FruAsim2$estadosB1Pars.FruAsim2),sum(estadosB1Pars.Fru$estadosB1Pars.Fru)),max(sum(estadosB1Pars.HemAsim2$estadosB1Pars.HemAsim2),sum(estadosB1Pars.Hem$estadosB1Pars.Hem)),max(sum(estadosB1Pars.InsAsim2$estadosB1Pars.InsAsim2),sum(estadosB1Pars.Ins$estadosB1Pars.Ins)),max(sum(estadosB1Pars.NecAsim2$estadosB1Pars.NecAsim2),sum(estadosB1Pars.Nec$estadosB1Pars.Nec)),max(sum(estadosB2Pars.CarAsim2$estadosB2Pars.CarAsim2),sum(estadosB2Pars.Car$estadosB2Pars.Car)),max(sum(estadosB2Pars.FruAsim2$estadosB2Pars.FruAsim2),sum(estadosB2Pars.Fru$estadosB2Pars.Fru)),max(sum(estadosB2Pars.HemAsim2$estadosB2Pars.HemAsim2),sum(estadosB2Pars.Hem$estadosB2Pars.Hem)),max(sum(estadosB2Pars.InsAsim2$estadosB2Pars.InsAsim2),sum(estadosB2Pars.Ins$estadosB2Pars.Ins)),max(sum(estadosB2Pars.NecAsim2$estadosB2Pars.NecAsim2),sum(estadosB2Pars.Nec$estadosB2Pars.Nec)),max(sum(estadosB3Pars.CarAsim2$estadosB3Pars.CarAsim2),sum(estadosB3Pars.Car$estadosB3Pars.Car)),max(sum(estadosB3Pars.FruAsim2$estadosB3Pars.FruAsim2),sum(estadosB3Pars.Fru$estadosB3Pars.Fru)),max(sum(estadosB3Pars.HemAsim2$estadosB3Pars.HemAsim2),sum(estadosB3Pars.Hem$estadosB3Pars.Hem)),max(sum(estadosB3Pars.InsAsim2$estadosB3Pars.InsAsim2),sum(estadosB3Pars.Ins$estadosB3Pars.Ins)),max(sum(estadosB3Pars.NecAsim2$estadosB3Pars.NecAsim2),sum(estadosB3Pars.Nec$estadosB3Pars.Nec)),max(sum(binarizacionBaseParsAsim2$Car.CP),sum(binarizacionBasePars$Car.CP)),max(sum(binarizacionBaseParsAsim2$Fru.ECP),sum(binarizacionBasePars$Fru.ECP)),max(sum(binarizacionBaseParsAsim2$Hem.CP),sum(binarizacionBasePars$Hem.CP)),max(sum(binarizacionBaseParsAsim2$Ins.ECP),sum(binarizacionBasePars$Ins.ECP)),max(sum(binarizacionBaseParsAsim2$Nec.ECP),sum(binarizacionBasePars$Nec.ECP)),max(sum(binarizacionMultiParsAsim2$Carnivoria),sum(binarizacionMultiPars$Carnivoria)),max(sum(binarizacionMultiParsAsim2$Frugivoria),sum(binarizacionMultiPars$Frugivoria)),max(sum(binarizacionMultiParsAsim2$Hematofagia),sum(binarizacionMultiPars$Hematofagia)),max(sum(binarizacionMultiParsAsim2$Insectivoria),sum(binarizacionMultiPars$Insectivoria)),max(sum(binarizacionMultiParsAsim2$Nectarivoria),sum(binarizacionMultiPars$Nectarivoria)))


ComparacionesParsAsim2vsER$Nodos = ComparacionesParsAsim2vsER$NodosCompartidos/ComparacionesParsAsim2vsER$NodosTotales

ComparacionesParsAsim2vsER[is.na(ComparacionesParsAsim2vsER)] <- 0

ComparacionesParsAsim2vsER$Dieta = c(rep(c("Carnivoria","Frugivoria","Hematofagia","Insectivoria","Nectarivoria"),5))

ComparacionesParsAsim2vsER$Matrices <- rep(c("B1","B2","B3","Mb","Multi"),each=5)


expoortos <- data.frame("Comparaciones"=ComparacionesParsAsim2vsER$Matrices,"Dietas"=ComparacionesParsAsim2vsER$Dieta,"Número-nodos-relativos"=ComparacionesParsAsim2vsER$Nodos)

write.csv(expoortos,file = "ComparacionesParsAsim2vsER.csv")
#----

##########################################################
# Figuras comparaciones ----
library(ggcorrplot)

## Primera columna: Pars ER, Mv ER y Pars vs Mv ER ----

### Carnivoria

carER <- read.csv("COL1.Car.csv",sep = ";")

colnames(carER) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(carER) <- carER$Matrices
carER$Matrices <- NULL

pdf(file="Col1Car.pdf",)
ggcorrplot(carER,outline.color = "gray75") +
    scale_fill_gradient2(mid = "gray96", high = "mediumorchid4",breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Frugivoria
fruER <- read.csv("COL1.Fru.csv",sep = ";")

colnames(fruER) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(fruER) <- fruER$Matrices
fruER$Matrices <- NULL

pdf(file="Col1Fru.pdf")
ggcorrplot(fruER,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "palegreen4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Hematofagia
hemER <- read.csv("COL1.Hem.csv",sep = ";")

colnames(hemER) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(hemER) <- hemER$Matrices
hemER$Matrices <- NULL

pdf(file="Col1Hem.pdf")
ggcorrplot(hemER,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "firebrick4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Insectivoria
insER <- read.csv("COL1.Ins.csv",sep = ";")

colnames(insER) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(insER) <- insER$Matrices
insER$Matrices <- NULL

pdf(file="Col1Ins.pdf")
ggcorrplot(insER,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "steelblue4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Nectarivoria
necER <- read.csv("COL1.Nec.csv",sep = ";")

colnames(necER) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(necER) <- necER$Matrices
necER$Matrices <- NULL

pdf(file="Col1Nec.pdf")
ggcorrplot(necER,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "goldenrod3", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()



## Segunda columna: Pars Asim1, Mv Asim1 y Pars vs Mv Asim1----

### Carnivoria

carDR1 <- read.csv("COL2.Car.csv",sep = ";")

colnames(carDR1) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(carDR1) <- carDR1$Matrices
carDR1$Matrices <- NULL

pdf(file="Col2Car.pdf",)
ggcorrplot(carDR1,outline.color = "gray75") +
  scale_fill_gradient2(mid = "gray96", high = "mediumorchid4",breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Frugivoria
fruDR1 <- read.csv("COL2.Fru.csv",sep = ";")

colnames(fruDR1) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(fruDR1) <- fruDR1$Matrices
fruDR1$Matrices <- NULL

pdf(file="Col2Fru.pdf")
ggcorrplot(fruDR1,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "palegreen4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Hematofagia
hemDR1 <- read.csv("COL2.Hem.csv",sep = ";")

colnames(hemDR1) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(hemDR1) <- hemDR1$Matrices
hemDR1$Matrices <- NULL

pdf(file="Col2Hem.pdf")
ggcorrplot(hemDR1,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "firebrick4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Insectivoria
insDR1 <- read.csv("COL2.Ins.csv",sep = ";")

colnames(insDR1) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(insDR1) <- insDR1$Matrices
insDR1$Matrices <- NULL

pdf(file="Col2Ins.pdf")
ggcorrplot(insDR1,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "steelblue4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Nectarivoria
necDR1 <- read.csv("COL2.Nec.csv",sep = ";")

colnames(necDR1) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(necDR1) <- necDR1$Matrices
necDR1$Matrices <- NULL

pdf(file="Col2Nec.pdf")
ggcorrplot(necDR1,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "goldenrod3", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()


## Tercera columna: Pars Asim2, Mv Asim2 y Pars vs Mv Asim2----

### Carnivoria

carDR2 <- read.csv("COL3.Car.csv",sep = ";")

colnames(carDR2) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(carDR2) <- carDR2$Matrices
carDR2$Matrices <- NULL

pdf(file="Col3Car.pdf",)
ggcorrplot(carDR2,outline.color = "gray75") +
  scale_fill_gradient2(mid = "gray96", high = "mediumorchid4",breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Frugivoria
fruDR2 <- read.csv("COL3.Fru.csv",sep = ";")

colnames(fruDR2) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(fruDR2) <- fruDR2$Matrices
fruDR2$Matrices <- NULL

pdf(file="Col3Fru.pdf")
ggcorrplot(fruDR2,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "palegreen4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Hematofagia
hemDR2 <- read.csv("COL3.Hem.csv",sep = ";")

colnames(hemDR2) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(hemDR2) <- hemDR2$Matrices
hemDR2$Matrices <- NULL

pdf(file="Col3Hem.pdf")
ggcorrplot(hemDR2,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "firebrick4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Insectivoria
insDR2 <- read.csv("COL3.Ins.csv",sep = ";")

colnames(insDR2) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(insDR2) <- insDR2$Matrices
insDR2$Matrices <- NULL

pdf(file="Col3Ins.pdf")
ggcorrplot(insDR2,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "steelblue4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Nectarivoria
necDR2 <- read.csv("COL3.Nec.csv",sep = ";")

colnames(necDR2) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(necDR2) <- necDR2$Matrices
necDR2$Matrices <- NULL

pdf(file="Col3Nec.pdf")
ggcorrplot(necDR2,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "goldenrod3", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()



## Cuarta columna: Asim1 vs Asim2, Asim1 vs ER y Asim2 vs ER en Pars y Mv ----

### Carnivoria

car <- read.csv("COL4.Car.csv",sep = ";")

colnames(car) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(car) <- car$Matrices
car$Matrices <- NULL

pdf(file="Col4Car.pdf",)
ggcorrplot(car,outline.color = "gray75") +
  scale_fill_gradient2(mid = "gray96", high = "mediumorchid4",breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Frugivoria
fru <- read.csv("COL4.Fru.csv",sep = ";")

colnames(fru) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(fru) <- fru$Matrices
fru$Matrices <- NULL

pdf(file="Col4Fru.pdf")
ggcorrplot(fru,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "palegreen4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Hematofagia
hem <- read.csv("COL4.Hem.csv",sep = ";")

colnames(hem) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(hem) <- hem$Matrices
hem$Matrices <- NULL

pdf(file="Col4Hem.pdf")
ggcorrplot(hem,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "firebrick4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Insectivoria
ins <- read.csv("COL4.Ins.csv",sep = ";")

colnames(ins) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(ins) <- ins$Matrices
ins$Matrices <- NULL

pdf(file="Col4Ins.pdf")
ggcorrplot(ins,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "steelblue4", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

### Nectarivoria
nec <- read.csv("COL4.Nec.csv",sep = ";")

colnames(nec) <- c("Matrices","Mb","Multi","B1","B2","B3")
rownames(nec) <- nec$Matrices
nec$Matrices <- NULL

pdf(file="Col4Nec.pdf")
ggcorrplot(nec,outline.color = "gray75")+
  scale_fill_gradient2(mid = "gray96", high = "goldenrod3", breaks=c(0,0.5, 1), limit=c(0, 1))
dev.off()

#----

