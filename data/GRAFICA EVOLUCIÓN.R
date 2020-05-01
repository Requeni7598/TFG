#GENERAR PUNTOS PARA GRAFICAS EVOLUTIVAS

library(readxl) #Libreria para trabajar con documentos EXCEL
Notas <- read_excel("Notas.xlsx") #Archivo principal
Notas = Notas[Notas$NotaPAU!=0,]
#Eliminamos todas las observaciones con NotaPAU = 0. Se trata de datos faltantes
attach(Notas)

A = as.data.frame(aggregate(NotaPAU,by=list(Curso),summary)) #Sumario nota media por curso
MatrizA <- cbind(A$Group.1,A$x)
colnames(MatrizA) = c("Año","Minimo","Cuartil1","Mediana","Media","Cuartil3","Maximo")
B = as.data.frame(aggregate(NotaPAU,by=list(Curso,Municipio),summary)) #Sumario nota media por curso y municipio
MatrizB <- cbind(B$Group.1,B$Group.2,B$x)
colnames(MatrizB) = c("Año","Munucipio","Minimo","Cuartil1","Mediana","Media","Cuartil3","Maximo")
C = as.data.frame(aggregate(NotaPAU,by=list(Curso,Conv),summary)) #Sumario nota media por curso y convocatoria
MatrizC <- cbind(C$Group.1,C$Group.2,C$x)
colnames(MatrizC) = c("Año","Convocatoria","Minimo","Cuartil1","Mediana","Media","Cuartil3","Maximo")
D = as.data.frame(aggregate(NotaPAU,by=list(Curso,ModalPAU),summary)) #Sumario para nota media por curso y modalidad
MatrizD <- cbind(D$Group.1,D$Group.2,D$x)
colnames(MatrizD) = c("Año","Modalidad","Minimo","Cuartil1","Mediana","Media","Cuartil3","Maximo")
E = as.data.frame(aggregate(NotaPAU,by=list(Curso,Ensenyanza),summary)) #Sumario para nota media por curso y centro
MatrizE <- cbind(E$Group.1,E$Group.2,E$x)
colnames(MatrizE) = c("Año","Tipo Centro","Minimo","Cuartil1","Mediana","Media","Cuartil3","Maximo")

save(MatrizA,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/MatrizA.rda")
save(MatrizB,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/MatrizB.rda")
save(MatrizC,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/MatrizC.rda")
save(MatrizD,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/MatrizD.rda")
save(MatrizE,file="/Users/requelui/Desktop/UNIVERSIDAD/TFG/R STUDIO/data/MatrizE.rda")






