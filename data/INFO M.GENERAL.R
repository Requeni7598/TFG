library(readxl) #Libreria para trabajar con documentos EXCEL
Notas <- read_excel("Notas.xlsx") #Archivo principal
Notas = Notas[Notas$NotaPAU!=0,]
#Eliminamos todas las observaciones con NotaPAU = 0. Se trata de datos faltantes
attach(Notas)
library(sp)

#DEFINIMOS LAS COORDENADAS DE LOS MUNICIPIOS DE NUESTRA MUESTRA
ADEMUZ <- cbind(-1.286,40.063)
ALAQUAS <- cbind(-0.461,39.457)
ALBAIDA <- cbind(-0.518,38.838)
ALBAL <- cbind(-0.415,39.395)
ALBERIC <- cbind(-0.520,39.116)
ALBORAYA <- cbind(-0.349,39.501)
ALCASSER <- cbind(-0.444,39.367)
ALFAFAR <- cbind(-0.389,39.420)
ALGINET <- cbind(-0.471,39.265)
ALMUSSAFES <- cbind(-0.413,39.292)
AYORA <- cbind(-1.057,39.059)
BENAGUASIL <- cbind(-0.588,39.594)
BENETUSSER <- cbind(-0.397,39.422)
BENIFAIO <- cbind(-0.426,39.285)
BENIMAMET <- cbind(-0.421,39.502)
BETERA <- cbind(-0.459,39.589)
BOCAIRENT <- cbind(-0.611,38.766)
BUÑOL <- cbind(-0.791,39.418)
BURJASSOT <- cbind(-0.411,39.509)
CAMPOLIVAR <- cbind(-0.431,39.531)
CARCER <- cbind(-0.567,39.070)
CARLET <- cbind(-0.520,39.225)
CASTELLO_DE_RUGAT <- cbind(-0.381,38.874)
CATADAU <- cbind(-0.571,39.274)
CATARROJA <- cbind(-0.402,39.403)
CULLERA <- cbind(-0.254,39.164)
EL_PUIG <- cbind(-0.306,39.588)
EL_SALER <- cbind(-0.332,39.383)
GODELLA <- cbind(-0.410,39.518)
GUADASSUAR <- cbind(-0.476,39.185)
JALANCE <- cbind(-1.076,39.191)
LALCUDIA <- cbind(-0.504,39.194)
LELIANA <- cbind(-0.530,39.565)
LOLLERIA <- cbind(-0.553,38.914)
LA_CAÑADA_PATERNA <- cbind(-0.486,39.527)
LA_POBLA_DE_FARNALS <- cbind(-0.326,39.577)
LA_POBLA_DE_VALLBONA <- cbind(-0.553,39.591)
LLIRIA <- cbind(-0.595,39.625)
MASSAMAGRELL <- cbind(-0.330,39.570)
MASSANASSA <- cbind(-0.400,39.411)
MISLATA <- cbind(-0.417,39.475)
MONCADA <- cbind(-0.394,39.545)
MONTECAÑADA_PATERNA <- cbind(-0.467,39.536)
OLIVA <- cbind(-0.121,38.920)
ONTINYENT <- cbind(-0.609,38.821)
PATERNA <- cbind(-0.442,39.503)
PICASSENT <- cbind(-0.461,39.363)
PUZOL <- cbind(-0.302,39.617)
RAFELBUNYOL <- cbind(-0.332,39.588)
REQUENA <- cbind(-1.102,39.488)
S._ANTONIO_BENAGEBER <- cbind(-0.499,39.561)
SEDAVI <- cbind(-0.383,39.424)
SILLA <- cbind(-0.411,39.363)
SUECA <- cbind(-0.311,39.202)
TAVERNES_BLANQUES <- cbind(-0.362,39.506)
TAVERNES_DE_VALLDIGNA <- cbind(-0.267,39.071)
TURIS <- cbind(-0.710,39.389)
UTIEL <- cbind(-1.205,39.568)
VALENCIA <- cbind(-0.376,39.469)
VILAMARXANT <- cbind(-0.622,39.568)
VILLANUEVA_DE_CASTELLON <- cbind(-0.512,39.078)
VILLAR_DEL_ARZOBISPO <- cbind(-0.827,39.734)

#CREAMOS UN VECTOR CON TODAS LAS COORDENADAS ALMACENADAS
points <- rbind(ADEMUZ,ALAQUAS,ALBAIDA,ALBAL,ALBERIC,ALBORAYA,ALCASSER,ALFAFAR,ALGINET,ALMUSSAFES,AYORA,BENAGUASIL,BENETUSSER,
                BENIFAIO,BENIMAMET,BETERA,BOCAIRENT,BUÑOL,BURJASSOT,CAMPOLIVAR,CARCER,CARLET,CASTELLO_DE_RUGAT,CATADAU, CATARROJA,
                CULLERA,EL_PUIG,EL_SALER,GODELLA,GUADASSUAR,JALANCE,LALCUDIA,LOLLERIA,LA_CAÑADA_PATERNA,LELIANA,LA_POBLA_DE_FARNALS,
                LA_POBLA_DE_VALLBONA,LLIRIA,MASSAMAGRELL,MASSANASSA,MISLATA,MONCADA,MONTECAÑADA_PATERNA,OLIVA,ONTINYENT,PATERNA,
                PICASSENT,PUZOL,RAFELBUNYOL,REQUENA,S._ANTONIO_BENAGEBER,SEDAVI,SILLA,SUECA,TAVERNES_BLANQUES,TAVERNES_DE_VALLDIGNA,
                TURIS,UTIEL,VALENCIA,VILAMARXANT,VILLANUEVA_DE_CASTELLON,VILLAR_DEL_ARZOBISPO)

#EMPEZAMOS A CONSTRUIR LA INFO(DATA) DE NUESTRO SPATIAL POINT

#INFORMACION GENERAL
data1G <- as.data.frame(rbind("ADEMUZ","ALAQUAS","ALBAIDA","ALBAL","ALBERIC","ALBORAYA","ALCASSER","ALFAFAR","ALGINET","ALMUSSAFES","AYORA","BENAGUASIL","BENETUSSER",
                              "BENIFAIO","BENIMAMET","BETERA","BOCAIRENT","BUÑOL","BURJASSOT","CAMPOLIVAR","CARCER","CARLET","CASTELLO_DE_RUGAT","CATADAU","CATARROJA",
                              "CULLERA","EL_PUIG","EL_SALER","GODELLA","GUADASSUAR","JALANCE","L'ALCUDIA","L'OLLERIA","LA_CAÑADA-PATERNA","LA_ELIANA","LA_POBLA_DE_FARNALS",
                              "LA_POBLA_DE_VALLBONA","LLIRIA","MASSAMAGRELL","MASSANASSA","MISLATA","MONCADA","MONTECAÑADA-PATERNA","OLIVA","ONTINYENT","PATERNA",
                              "PICASSENT","PUZOL","RAFELBUNYOL","REQUENA","S._ANTONIO_BENAGEBER","SEDAVI","SILLA","SUECA","TAVERNES_BLANQUES","TAVERNES_DE_VALLDIGNA",
                              "TURÍS","UTIEL","VALENCIA","VILAMARXANT","VILLANUEVA_CASTELLON","VILLAR_DEL_ARZOBISPO"))
colnames(data1G)=c("Municipio") #NOMBRE MUNICIPIOS

data2G <- c()
for (i in 1:length(unique(Notas$Municipio))){
  data2G <- as.data.frame(cbind(data2G,table(Notas$Municipio)[i]))
}
data2G <- as.data.frame(t(data2G))
colnames(data2G)=c("Estudiantes") #ESTUDIANTES POR MUNICIPIO

info3G = aggregate(Notas$NotaPAU, list(Notas$Municipio), mean) #NOTA MEDIA POR MUNICIPIO
data3G <- c()
for (i in 1:length(unique(Notas$Municipio))){
  data3G <- as.data.frame(cbind(data3G,info3G$x[i]))
}
data3G <- as.data.frame(t(data3G))
colnames(data3G)=c("NotaMedia")

info4G = aggregate(Notas$NotaPAU, list(Notas$Municipio,Notas$Sexo), mean) #NOTA MEDIA POR MUNICIPIO Y SEXO
info4G = info4G[info4G$Group.2=="H",] #NOTA MEDIA HOMBRES
data4G <- c()
for (i in 1:length(unique(Notas$Municipio))){
  data4G <- as.data.frame(cbind(data4G,info4G$x[i]))
}
data4G <- as.data.frame(t(data4G))
colnames(data4G)=c("NotaMediaH")

info5G = aggregate(Notas$NotaPAU, list(Notas$Municipio,Notas$Sexo), mean) #NOTA MEDIA POR MUNICIPIO Y SEXO
info5G = info5G[info5G$Group.2=="M",] #NOTA MEDIA MUJERES
data5G <- c()
for (i in 1:length(unique(Notas$Municipio))){
  data5G <- as.data.frame(cbind(data5G,info5G$x[i]))
}
data5G <- as.data.frame(t(data5G))
colnames(data5G)=c("NotaMediaM")

data = cbind(data1G,data2G,data3G,data4G,data5G) #DATA CON LAS CINCO COLUMNAS
pointsGM <- SpatialPointsDataFrame(coords = points, data=data) #SPATIAL POINT

save(pointsGM,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/CursoG.rda")

#AHORA REALIZAMOS EL MISMO PROCEDIMIENTO PARA CADA CURSO ACADEMICO
#EL PROCEDIMIENTO ES EL MISMO. SOLO DEBEMOS TENER EN CUENTA LOS MUNICIPIOS FALTANTES

CURSOS <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")

for (j in 1:length(CURSOS)){
  CURSOS[j]
  NotasA = Notas[Notas$Curso==CURSOS[j],]
  MUNICIPIOSA = unique(NotasA$Municipio)
  MUNICIPIOSA = as.data.frame(MUNICIPIOSA)
  MUNICIPIOSA = na.omit(MUNICIPIOSA)
  MUNICIPIOSA$MUNICIPIOSA = as.character(MUNICIPIOSA$MUNICIPIOSA)
  MUNICIPIOSA = MUNICIPIOSA[order(MUNICIPIOSA$MUNICIPIOSA),] #ORDEN ALFABETICO
  MUNICIPIOSA = as.data.frame(MUNICIPIOSA)
  MUNICIPIOSA$MUNICIPIOSA = as.character(MUNICIPIOSA$MUNICIPIOSA)
  #Repetimos lo mismo varias veces obligados por cambios en la estructura que se producen al ejecutar ciertas lineas
  
  #A PARTIR DE AQUI EL PROCEDIMIENTO ES SIMILAR AL ANTERIOR. REPETIMOS EL MISMO BUCLE PARA CADA CURSO DE INTERES
  COORDENADAS <- c()
  for(i in 1:dim(MUNICIPIOSA)[1]){
    DATOS=which(data1G==MUNICIPIOSA[i,1])
    COORDENADAS=rbind(COORDENADAS,points[DATOS,])
  }
  colnames(MUNICIPIOSA)=c("Municipio")
  data2 <- c()
  for(i in 1:dim(MUNICIPIOSA)[1]){
    data2 <- as.data.frame(cbind(data2,table(NotasA$Municipio)[i]))
  }
  data2 <- as.data.frame(t(data2))
  colnames(data2)=c("Estudiantes")
  
  info3 = aggregate(NotasA$NotaPAU, list(NotasA$Municipio), mean)
  data3 <- c()
  for(i in 1:dim(MUNICIPIOSA)[1]){
    data3 <- as.data.frame(cbind(data3,info3$x[i]))
  }
  data3 <- as.data.frame(t(data3))
  colnames(data3)=c("NotaMedia")
  
  info4 = aggregate(NotasA$NotaPAU, list(NotasA$Municipio,NotasA$Sexo), mean)
  info4 = info4[info4$Group.2=="H",]
  data4 <- c()
  for(i in 1:dim(MUNICIPIOSA)[1]){
    data4 <- as.data.frame(cbind(data4,info4$x[i]))
  }
  data4 <- as.data.frame(t(data4))
  colnames(data4)=c("NotaMediaH")
  
  info5 = aggregate(NotasA$NotaPAU, list(NotasA$Municipio,NotasA$Sexo), mean)
  info5 = info5[info5$Group.2=="M",]
  data5 <- c()
  for(i in 1:dim(MUNICIPIOSA)[1]){
    data5 <- as.data.frame(cbind(data5,info5$x[i]))
  }
  data5 <- as.data.frame(t(data5))
  colnames(data5)=c("NotaMediaM")
  
  data = cbind(MUNICIPIOSA,data2,data3,data4,data5)
  pointsGM <- SpatialPointsDataFrame(coords = COORDENADAS, data=data)
  
  save(pointsGM,file=paste0("/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/Curso",j,".rda"))
}