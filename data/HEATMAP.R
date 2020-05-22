library(readxl)
Notas <- read_excel("Notas Asignatura.xlsx")
Notas = Notas[Notas$NmDef!=0,]

#OBTENCION MUNICIPIOS ASIGNATURAS
Ciudad = strsplit(Notas$Centro,"-")
Poblacion = c()
for(i in 1:length(Ciudad)){
        Poblacion = c(Poblacion,Ciudad[[i]][2])
        print(i)
}
Poblacion = as.data.frame(Poblacion)
Poblacion$Poblacion=as.character(Poblacion$Poblacion)
for(i in 1:dim(Poblacion)[1]){
        Poblacion$Poblacion[i]=trimws(Poblacion$Poblacion[i])
        print(i)
}
table(Poblacion$Poblacion)
which(Poblacion$Poblacion=="BENETUSER")
for(i in which(Poblacion$Poblacion=="BENETUSER")){
        Poblacion$Poblacion[i]="BENETUSSER"
        print(i)
}
which(Poblacion$Poblacion=="C.E.U.")
for(i in which(Poblacion$Poblacion=="C.E.U.")){
        Poblacion$Poblacion[i]="MONCADA"
        print(i)
}
which(Poblacion$Poblacion=="ALBORAIA")
for(i in which(Poblacion$Poblacion=="ALBORAIA")){
        Poblacion$Poblacion[i]="ALBORAYA"
        print(i)
}
which(Poblacion$Poblacion=="ANTONIO CAÑUELO")
for(i in which(Poblacion$Poblacion=="ANTONIO CAÑUELO")){
        Poblacion$Poblacion[i]="VALENCIA"
        print(i)
}
which(Poblacion$Poblacion=="Catarroja")
for(i in which(Poblacion$Poblacion=="Catarroja")){
        Poblacion$Poblacion[i]="CATARROJA"
        print(i)
}
which(Poblacion$Poblacion=="Cullera")
for(i in which(Poblacion$Poblacion=="Cullera")){
        Poblacion$Poblacion[i]="CULLERA"
        print(i)
}
which(Poblacion$Poblacion=="DOMINICOS")
for(i in which(Poblacion$Poblacion=="DOMINICOS")){
        Poblacion$Poblacion[i]="VALENCIA"
        print(i)
}
which(Poblacion$Poblacion=="ESCLAVAS")
for(i in which(Poblacion$Poblacion=="ESCLAVAS")){
        Poblacion$Poblacion[i]="VALENCIA"
        print(i)
}
which(Poblacion$Poblacion=="LA ELIANA")
for(i in which(Poblacion$Poblacion=="LA ELIANA")){
        Poblacion$Poblacion[i]="L'ELIANA"
        print(i)
}
which(Poblacion$Poblacion=="Mislata")
for(i in which(Poblacion$Poblacion=="Mislata")){
        Poblacion$Poblacion[i]="MISLATA"
        print(i)
}
which(Poblacion$Poblacion=="Oliva")
for(i in which(Poblacion$Poblacion=="Oliva")){
        Poblacion$Poblacion[i]="OLIVA"
        print(i)
}
which(Poblacion$Poblacion=="PUZOL")
for(i in which(Poblacion$Poblacion=="PUZOL")){
        Poblacion$Poblacion[i]="PUÇOL"
        print(i)
}
which(Poblacion$Poblacion=="TAVERNES DE VALLDIGNA")
for(i in which(Poblacion$Poblacion=="TAVERNES DE VALLDIGNA")){
        Poblacion$Poblacion[i]="TAVERNES VALLDIGNA"
        print(i)
}
which(Poblacion$Poblacion=="Turís")
for(i in which(Poblacion$Poblacion=="Turís")){
        Poblacion$Poblacion[i]="TURÍS"
        print(i)
}
save(Poblacion,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/Poblacion.rda")
load("Poblacion.rda")
Notas=cbind(Notas,Poblacion)

#OBTENCION HEATMAP
colores_categorias=c('#FFFFFF','#FFEBBE','#FFA77F','#E64C00','#731800')
Notas$Materia = as.factor(Notas$Materia)
ASIG=c("Biología","Castellano: Lengua y Literatura II","Ciencias de la Tierra y Medioambientales","Dibujo Técnico II",
       "Economía de la Empresa","Física","Geografía","Historia de España","Historia de la Filosofía","Historia del Arte",
       "Inglés","Latín II","Literatura Universal","Matemáticas aplicadas a las Ciencias Sociales","Matemáticas II",
       "Química","Valenciano: Lengua y Literatura II")
CURSO=c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
MUN=c("Ademuz","Alaquas","Albaida","Albal","Alberic","Alboraya","Alcasser","Alfafar","Alginet","Almussafes",
      "Ayora","Benaguasil","Benetusser","Benifaio","Benimamet","Betera","Bocairent","Buñol","Burjassot","Campolivar",
      "Carcer","Carlet","Castello de Rugat","Catadau","Catarroja","Cullera","El Puig","El Saler","Godella","Guadassuar",
      "Jalance","L'Alcudia","L'Eliana","L'Olleria","La Cañada-Paterna","La Pobla de Vallbona","Lliria",
      "Massamagrell","Massanassa","Mislata","Moncada","Montecañada-Paterna","Oliva","Ontinyent","Paterna",
      "Picassent","Pobla de Farnals","Puzol","Rafelbunyol","Requena","Sedavi","Silla","Sueca","Tavernes Blanques",
      "Tavernes de Valldigna","Turís","Utiel","Valencia","Vilamarxant","Villanueva de Castellón","Villar del Arzobispo")

FINAL1 = c()
for(i in 1:length(ASIG)){#Table funciona mejor que Unique o Levels
        NotaMAT = Notas[Notas$Materia==ASIG[i],]
        Mark = aggregate(NotaMAT$NmDef,list(NotaMAT$Curso),mean)
        FINAL1 = cbind(FINAL1,Mark$x)
}
FINAL1=as.data.frame(FINAL1)
DATOSESP1=c()
for (i in 1:dim(FINAL1)[2]){
  FINALESP1=mean(FINAL1[,i])
  FINALESP1=rep(FINALESP1,length(CURSO))
  DATOSESP1=cbind(DATOSESP1,FINALESP1)
}
DATOSESP1=as.data.frame(DATOSESP1)
valor=c()
for (j in c(1:ncol(FINAL1))){
  valor=c(valor,rev(as.numeric(FINAL1[,j])))
}
valor_se=c()
for (j in c(1:ncol(DATOSESP1))){
  valor_se=c(valor_se,rev(as.numeric(DATOSESP1[,j])))
}
valor_rec=rep(NA,length(valor))
for (j in c(1:length(valor))){
    if (valor[j]>=valor_se[j]+0.5){valor_rec[j]=5}
    else if (valor[j]>=valor_se[j]+0.2 & valor[j]<valor_se[j]+0.5){valor_rec[j]=4}
    else if (valor[j]>=valor_se[j]-0.2 & valor[j]<valor_se[j]+0.2){valor_rec[j]=3}
    else if (valor[j]>=valor_se[j]-0.5 & valor[j]<valor_se[j]-0.2){valor_rec[j]=2}
    else if (valor[j]<=valor_se[j]-0.5){valor_rec[j]=1}
}
CURSO=rev(CURSO)
CURSO=as.character(CURSO)
ASIG=as.character(ASIG)
df1=expand.grid(CURSO,ASIG)
print(dim(df1))
print(length(valor))
df1=cbind(df1,valor_rec)
colnames(df1)=c("CURSO","ASIGNATURA","VALOR")
save(df1,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/df1.rda")
G=ggplot(data=df1,aes(x=ASIGNATURA,y=CURSO,fill=factor(VALOR)))+
  geom_tile(colour="black",
            alpha = 0.7) +
  scale_fill_manual(
    values=rev(colores_categorias),
    breaks=c("1","2","3","4","5"),
    labels=c("MAL AÑO","AÑO REGULAR","AÑO ESTANDARD","BUEN AÑO",
             "GRAN AÑO"))+
  xlab("ASIGNATURA")+
  ylab("CURSO")+
  guides(fill=guide_legend(title="CALIDAD"))+
  scale_x_discrete(position = "top")+
  theme(axis.text.y = element_text(size=10))+
  theme(axis.text.x = element_text(size=10,angle = 90))

FINAL2 = c()
for(i in 1:length(ASIG)){#Table funciona mejor que Unique o Levels
        NotaMAT = Notas[Notas$Materia==ASIG[i],]        
        Mark = aggregate(NotaMAT$NmDef,list(NotaMAT$Poblacion),mean)
        FINAL2 = cbind(FINAL2,Mark$x)
}
FINAL2=as.data.frame(FINAL2)
DATOSESP2=c()
for (i in 1:dim(FINAL2)[2]){
  FINALESP2=mean(FINAL2[,i])
  FINALESP2=rep(FINALESP2,length(MUN))
  DATOSESP2=cbind(DATOSESP2,FINALESP2)
}
DATOSESP2=as.data.frame(DATOSESP2)
valor=c()
for (j in c(1:ncol(FINAL2))){
  valor=c(valor,rev(as.numeric(FINAL2[,j])))
}
valor_se=c()
for (j in c(1:ncol(DATOSESP2))){
  valor_se=c(valor_se,rev(as.numeric(DATOSESP2[,j])))
}
valor_rec=rep(NA,length(valor))
for (j in c(1:length(valor))){
  if (valor[j]>=valor_se[j]+0.5){valor_rec[j]=5}
  else if (valor[j]>=valor_se[j]+0.2 & valor[j]<valor_se[j]+0.5){valor_rec[j]=4}
  else if (valor[j]>=valor_se[j]-0.2 & valor[j]<valor_se[j]+0.2){valor_rec[j]=3}
  else if (valor[j]>=valor_se[j]-0.5 & valor[j]<valor_se[j]-0.2){valor_rec[j]=2}
  else if (valor[j]<=valor_se[j]-0.5){valor_rec[j]=1}
}
MUN=rev(MUN)
MUN=as.character(MUN)
ASIG=as.character(ASIG)
df2=expand.grid(MUN,ASIG)
print(dim(df2))
print(length(valor))
df2=cbind(df2,valor_rec)
colnames(df2)=c("MUNICIPIO","ASIGNATURA","VALOR")
save(df2,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/df2.rda")
G2=ggplot(data=df2,aes(x=ASIGNATURA,y=MUNICIPIO,fill=factor(VALOR)))+
  geom_tile(colour="black",
            alpha = 0.7) +
  scale_fill_manual(
    values=rev(colores_categorias),
    breaks=c("1","2","3","4","5"),
    labels=c("MAL MUNICIPIO","MUNICIPIO REGULAR","MUNICIPIO ESTANDARD","BUEN MUNICIPIO",
             "GRAN MUNICIPIO"))+
  xlab("ASIGNATURA")+
  ylab("MUNICIPIO")+
  guides(fill=guide_legend(title="CALIDAD"))+
  scale_x_discrete(position = "top")+
  theme(axis.text.y = element_text(size=8))+
  theme(axis.text.x = element_text(size=10,angle=90))
