library(readxl)
Notas <- read_excel("Notas.xlsx")
Notas = Notas[Notas$NotaPAU!=0,]
attach(Notas)

colores <- c("Media"=heat.colors(1),"Minimo"=rainbow(1),"Cuartil1"=topo.colors(1),"Mediana"=terrain.colors(1),
             "Cuartil3"="blue","Maximo"=cm.colors(1))

#CARGAMOS INFORMACION PARA NUESTROS MAPA MUNICIPAL (AUTOMATIZADO) #MODIFICAR NOTAMEDIAH GENERAL Y VALENCIA(59)
load("data/InstitutoG.rda")
InstitutoG = pointsGI
InstitutoG$NotaMediaH=as.numeric(paste(InstitutoG$NotaMediaH)) #CAMBIO TIPO FACTOR. SOLO PARA GENERAL Y VALENCIA
load("data/Instituto1.rda")
Instituto1 = pointsGI
load("data/Instituto2.rda")
Instituto2 = pointsGI
load("data/Instituto3.rda")
Instituto3 = pointsGI
load("data/Instituto4.rda")
Instituto4 = pointsGI
load("data/Instituto5.rda")
Instituto5 = pointsGI
load("data/Instituto6.rda")
Instituto6 = pointsGI
load("data/Instituto7.rda")
Instituto7 = pointsGI
load("data/Instituto8.rda")
Instituto8 = pointsGI
load("data/Instituto9.rda")
Instituto9 = pointsGI
load("data/Instituto10.rda")
Instituto10 = pointsGI
load("data/Instituto11.rda")
Instituto11 = pointsGI
load("data/Instituto12.rda")
Instituto12 = pointsGI
load("data/Instituto13.rda")
Instituto13 = pointsGI
load("data/Instituto14.rda")
Instituto14 = pointsGI
load("data/Instituto15.rda")
Instituto15 = pointsGI
load("data/Instituto16.rda")
Instituto16 = pointsGI
load("data/Instituto17.rda")
Instituto17 = pointsGI
load("data/Instituto18.rda")
Instituto18 = pointsGI
load("data/Instituto19.rda")
Instituto19 = pointsGI
load("data/Instituto20.rda")
Instituto20 = pointsGI
load("data/Instituto21.rda")
Instituto21 = pointsGI
load("data/Instituto22.rda")
Instituto22 = pointsGI
load("data/Instituto23.rda")
Instituto23 = pointsGI
load("data/Instituto24.rda")
Instituto24 = pointsGI
load("data/Instituto25.rda")
Instituto25 = pointsGI
load("data/Instituto26.rda")
Instituto26 = pointsGI
load("data/Instituto27.rda")
Instituto27 = pointsGI
load("data/Instituto28.rda")
Instituto28 = pointsGI
load("data/Instituto29.rda")
Instituto29 = pointsGI
load("data/Instituto30.rda")
Instituto30 = pointsGI
load("data/Instituto31.rda")
Instituto31 = pointsGI
load("data/Instituto32.rda")
Instituto32 = pointsGI
load("data/Instituto33.rda")
Instituto33 = pointsGI
load("data/Instituto34.rda")
Instituto34 = pointsGI
load("data/Instituto35.rda")
Instituto35 = pointsGI
load("data/Instituto36.rda")
Instituto36 = pointsGI
load("data/Instituto37.rda")
Instituto37 = pointsGI
load("data/Instituto38.rda")
Instituto38 = pointsGI
load("data/Instituto39.rda")
Instituto39 = pointsGI
load("data/Instituto40.rda")
Instituto40 = pointsGI
load("data/Instituto41.rda")
Instituto41 = pointsGI
load("data/Instituto42.rda")
Instituto42 = pointsGI
load("data/Instituto43.rda")
Instituto43 = pointsGI
load("data/Instituto44.rda")
Instituto44 = pointsGI
load("data/Instituto45.rda")
Instituto45 = pointsGI
load("data/Instituto46.rda")
Instituto46 = pointsGI
load("data/Instituto47.rda")
Instituto47 = pointsGI
load("data/Instituto48.rda")
Instituto48 = pointsGI
load("data/Instituto49.rda")
Instituto49 = pointsGI
load("data/Instituto50.rda")
Instituto50 = pointsGI
load("data/Instituto51.rda")
Instituto51 = pointsGI
load("data/Instituto52.rda")
Instituto52 = pointsGI
load("data/Instituto53.rda")
Instituto53 = pointsGI
load("data/Instituto54.rda")
Instituto54 = pointsGI
load("data/Instituto55.rda")
Instituto55 = pointsGI
load("data/Instituto56.rda")
Instituto56 = pointsGI
load("data/Instituto57.rda")
Instituto57 = pointsGI
load("data/Instituto58.rda")
Instituto58 = pointsGI
load("data/Instituto59.rda")
Instituto59 = pointsGI
Instituto59$NotaMediaH=as.numeric(paste(Instituto59$NotaMediaH)) #CAMBIO TIPO FACTOR. SOLO PARA GENERAL Y VALENCIA
load("data/Instituto60.rda")
Instituto60 = pointsGI
load("data/Instituto61.rda")
Instituto61 = pointsGI
load("data/Instituto62.rda")
Instituto62 = pointsGI

#CARGAMOS TODA LA INFORMACION PARA MAPA GENERAL
load("data/CursoG.rda")
CursoG = pointsGM
load("data/Curso1.rda")
Curso1 = pointsGM
load("data/Curso2.rda")
Curso2 = pointsGM
load("data/Curso3.rda")
Curso3 = pointsGM
load("data/Curso4.rda")
Curso4 = pointsGM
load("data/Curso5.rda")
Curso5 = pointsGM
load("data/Curso6.rda")
Curso6 = pointsGM
load("data/Curso7.rda")
Curso7 = pointsGM
load("data/Curso8.rda")
Curso8 = pointsGM
load("data/Curso9.rda")
Curso9 = pointsGM
load("data/Curso10.rda")
Curso10 = pointsGM

#CARGAMOS INFORMACION PARA GRAFICAS EVOLUTIVAS
load("data/MatrizA.rda")
MatrizA=as.data.frame(MatrizA)
load("data/MatrizB.rda")
MatrizB=as.data.frame(MatrizB)
load("data/MatrizC.rda")
MatrizC=as.data.frame(MatrizC)
load("data/MatrizD.rda")
MatrizD=as.data.frame(MatrizD)
load("data/MatrizE.rda")
MatrizE=as.data.frame(MatrizE)

zoom_mapa_general=c()
center_mapa_general=c()
zoom_mapa_municipal=c()
center_mapa_municipal=c()
center_mapa = c()
zoom_mapa = c()

shinyServer <- function(input, output, session){

  observeEvent(input$Variable, {
    if(input$Variable=="Variable_Curso"){
      nombre_variable=gsub("Variable_","",input$Variable)
      observeEvent(input$Nivel1, {
        nombre_nivel=gsub("Nivel_","",input$Nivel1)
        #SELECCION UNILATERAL
        datos=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos=datos[!is.na(datos)]
        datos=datos[datos>0]
        new=as.data.frame(datos)
        #SELECCION AMBOS SEXOS
        datos1=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos2=Notas$Sexo[Notas[,nombre_variable]==nombre_nivel]
        data=cbind(datos1,datos2)
        data=na.omit(data)
        newdata=as.data.frame(data)
        newdata$datos1=as.numeric(paste(newdata$datos1))
        colnames(newdata)=c("Nota","Sexo")
        mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
        MatrizA$Año=as.factor(paste(MatrizA$Año))
        output$hist1_1 <- renderPlot({
          ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                     geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist1_2 <- renderPlot({
          ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist1_3 <- renderPlot({
          ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                     geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist1_4 <- renderPlot({
          ggplot() + geom_point(aes(x=Año,y=Media),data = MatrizA,shape=21,fill=rainbow(1),size=3) + 
                        geom_line(aes(x=Año,y=Media,color="Media",group=1),data = MatrizA,lwd=1) + 
                     geom_point(aes(x=Año,y=Minimo),data = MatrizA,shape=21,fill=heat.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Minimo,color="Minimo",group=1),data = MatrizA,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil1),data = MatrizA,shape=21,fill=terrain.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil1,color="Cuartil1",group=1),data = MatrizA,lwd=1) + 
                     geom_point(aes(x=Año,y=Mediana),data = MatrizA,shape=21,fill=topo.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Mediana,color="Mediana",group=1),data = MatrizA,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil3),data = MatrizA,shape=21,fill=cm.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil3,color="Cuartil3",group=1),data = MatrizA,lwd=1) +  
                     geom_point(aes(x=Año,y=Maximo),data = MatrizA,shape=21,fill="blue",size=3) + 
                        geom_line(aes(x=Año,y=Maximo,color="Maximo",group=1),data = MatrizA,lwd=1) + 
                     labs(x="Año", y="Nota") +
                     scale_colour_manual("", 
                                        breaks = c("Media","Minimo","Cuartil1","Mediana","Cuartil3","Maximo"),
                                        values = c(heat.colors(1),rainbow(1),topo.colors(1),terrain.colors(1),"blue",cm.colors(1))) +
                     theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                     ggtitle("Evolucion Temporal")
        })
        output$hist1_5 <- renderPlot({
          ggplot(aes(x=Año,y=Media),data = MatrizA) + geom_point(shape=21,color="black",fill=cm.colors(1),size=6) + 
                                                      geom_line(color=heat.colors(1),lwd=2,group=1) +
                                                      theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                                                      labs(x="Año", y="Nota") + 
                                                      ggtitle("Evolucion Temporal Nota Media")
        })
        output$stats1 <- renderPrint({
          summary(datos)
        })
        output$Box1 <- renderInfoBox({
          infoBox(
           "Número de estudiantes", length(datos), icon = icon("list"),
           color = "purple", fill = TRUE
         ) 
        })
        output$BoxBox1 <- renderInfoBox({
          infoBox(
            "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
            color = "yellow", fill = TRUE
          ) 
        })
      })
    }
    if(input$Variable=="Variable_Municipio"){
      nombre_variable=gsub("Variable_","",input$Variable)
      observeEvent(input$Nivel2, {
        nombre_nivel=gsub("Nivel_","",input$Nivel2)
        #SELECCION UNILATERAL
        datos=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos=datos[!is.na(datos)]
        datos=datos[datos>0]
        new=as.data.frame(datos)
        #SELECCION AMBOS SEXOS
        datos1=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos2=Notas$Sexo[Notas[,nombre_variable]==nombre_nivel]
        data=cbind(datos1,datos2)
        data=na.omit(data)
        newdata=as.data.frame(data)
        newdata$datos1=as.numeric(paste(newdata$datos1))
        colnames(newdata)=c("Nota","Sexo")
        mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
        #PARA GRAFICAS EVOLUTIVAS
        Matriz1=MatrizB[MatrizB$Munucipio==nombre_nivel,]
        Matriz1$Minimo=as.numeric(paste(Matriz1$Minimo))
        Matriz1$Cuartil1=as.numeric(paste(Matriz1$Cuartil1))
        Matriz1$Mediana=as.numeric(paste(Matriz1$Mediana))
        Matriz1$Media=as.numeric(paste(Matriz1$Media))
        Matriz1$Cuartil3=as.numeric(paste(Matriz1$Cuartil3))
        Matriz1$Maximo=as.numeric(paste(Matriz1$Maximo))
        output$hist2_1 <- renderPlot({
          ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                     geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist2_2 <- renderPlot({
          ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist2_3 <- renderPlot({
          ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                     geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist2_4 <- renderPlot({
          ggplot() + geom_point(aes(x=Año,y=Media),data = Matriz1,shape=21,fill=rainbow(1),size=3) + 
                        geom_line(aes(x=Año,y=Media,color="Media",group=1),data = Matriz1,lwd=1) + 
                     geom_point(aes(x=Año,y=Minimo),data = Matriz1,shape=21,fill=heat.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Minimo,color="Minimo",group=1),data = Matriz1,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil1),data = Matriz1,shape=21,fill=terrain.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil1,color="Cuartil1",group=1),data = Matriz1,lwd=1) + 
                     geom_point(aes(x=Año,y=Mediana),data = Matriz1,shape=21,fill=topo.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Mediana,color="Mediana",group=1),data = Matriz1,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil3),data = Matriz1,shape=21,fill=cm.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil3,color="Cuartil3",group=1),data = Matriz1,lwd=1) +  
                     geom_point(aes(x=Año,y=Maximo),data = Matriz1,shape=21,fill="blue",size=3) + 
                        geom_line(aes(x=Año,y=Maximo,color="Maximo",group=1),data = Matriz1,lwd=1) + 
                     labs(x="Año", y="Nota") +
                     scale_colour_manual("", 
                                        breaks = c("Media","Minimo","Cuartil1","Mediana","Cuartil3","Maximo"),
                                        values = c(heat.colors(1),rainbow(1),topo.colors(1),terrain.colors(1),"blue",cm.colors(1))) +
                     theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                     ggtitle("Evolucion Temporal")
        })
        output$hist2_5 <- renderPlot({
          ggplot(aes(x=Año,y=Media),data = Matriz1) + geom_point(shape=21,color="black",fill=cm.colors(1),size=6) + 
                                                      geom_line(color=heat.colors(1),lwd=2,group=1) +
                                                      theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                                                      labs(x="Año", y="Nota") + 
                                                      ggtitle("Evolucion Temporal Nota Media")
        })
        output$stats2 <- renderPrint({
          summary(datos)  
        })
        output$Box2 <- renderInfoBox({
          infoBox(
            "Número de estudiantes", length(datos), icon = icon("list"),
            color = "purple", fill = TRUE
          ) 
        })
        output$BoxBox2 <- renderInfoBox({
          infoBox(
            "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
            color = "yellow", fill = TRUE
          ) 
        })
      })
    }
    if(input$Variable=="Variable_Conv"){
      nombre_variable=gsub("Variable_","",input$Variable)
      observeEvent(input$Nivel3, {
        nombre_nivel=gsub("Nivel_","",input$Nivel3)
        #SELECCION UNILATERAL
        datos=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos=datos[!is.na(datos)]
        datos=datos[datos>0]
        new=as.data.frame(datos)
        #SELECCION AMBOS SEXOS
        datos1=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos2=Notas$Sexo[Notas[,nombre_variable]==nombre_nivel]
        data=cbind(datos1,datos2)
        data=na.omit(data)
        newdata=as.data.frame(data)
        newdata$datos1=as.numeric(paste(newdata$datos1))
        colnames(newdata)=c("Nota","Sexo")
        mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
        #PARA GRAFICAS EVOLUTIVAS
        Matriz2=MatrizC[MatrizC$Convocatoria==nombre_nivel,]
        Matriz2$Minimo=as.numeric(paste(Matriz2$Minimo))
        Matriz2$Cuartil1=as.numeric(paste(Matriz2$Cuartil1))
        Matriz2$Mediana=as.numeric(paste(Matriz2$Mediana))
        Matriz2$Media=as.numeric(paste(Matriz2$Media))
        Matriz2$Cuartil3=as.numeric(paste(Matriz2$Cuartil3))
        Matriz2$Maximo=as.numeric(paste(Matriz2$Maximo))
        output$hist3_1 <- renderPlot({
          ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                     geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist3_2 <- renderPlot({
          ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist3_3 <- renderPlot({
          ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                     geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist3_4 <- renderPlot({
          ggplot() + geom_point(aes(x=Año,y=Media),data = Matriz2,shape=21,fill=rainbow(1),size=3) + 
                        geom_line(aes(x=Año,y=Media,color="Media",group=1),data = Matriz2,lwd=1) + 
                     geom_point(aes(x=Año,y=Minimo),data = Matriz2,shape=21,fill=heat.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Minimo,color="Minimo",group=1),data = Matriz2,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil1),data = Matriz2,shape=21,fill=terrain.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil1,color="Cuartil1",group=1),data = Matriz2,lwd=1) + 
                     geom_point(aes(x=Año,y=Mediana),data = Matriz2,shape=21,fill=topo.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Mediana,color="Mediana",group=1),data = Matriz2,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil3),data = Matriz2,shape=21,fill=cm.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil3,color="Cuartil3",group=1),data = Matriz2,lwd=1) +  
                     geom_point(aes(x=Año,y=Maximo),data = Matriz2,shape=21,fill="blue",size=3) + 
                        geom_line(aes(x=Año,y=Maximo,color="Maximo",group=1),data = Matriz2,lwd=1) + 
                     labs(x="Año", y="Nota") +
                     scale_colour_manual("", 
                                        breaks = c("Media","Minimo","Cuartil1","Mediana","Cuartil3","Maximo"),
                                        values = c(heat.colors(1),rainbow(1),topo.colors(1),terrain.colors(1),"blue",cm.colors(1))) +
                     theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                     ggtitle("Evolucion Temporal")
        })
        output$hist3_5 <- renderPlot({
          ggplot(aes(x=Año,y=Media),data = Matriz2) + geom_point(shape=21,color="black",fill=cm.colors(1),size=6) + 
                                                      geom_line(color=heat.colors(1),lwd=2,group=1) +
                                                      theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                                                      labs(x="Año", y="Nota") + 
                                                      ggtitle("Evolucion Temporal Nota Media")
        })
        output$stats3 <- renderPrint({
          summary(datos)  
        })
        output$Box3 <- renderInfoBox({
          infoBox(
            "Número de estudiantes", length(datos), icon = icon("list"),
            color = "purple", fill = TRUE
          ) 
        })
        output$BoxBox3 <- renderInfoBox({
          infoBox(
            "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
            color = "yellow", fill = TRUE
          ) 
        })
      })
    }
    if(input$Variable=="Variable_ModalPAU"){
      nombre_variable=gsub("Variable_","",input$Variable)
      observeEvent(input$Nivel4, {
        nombre_nivel=gsub("Nivel_","",input$Nivel4)
        #SELECCION UNILATERAL
        datos=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos=datos[!is.na(datos)]
        datos=datos[datos>0]
        new=as.data.frame(datos)
        #SELECCION AMBOS SEXOS
        datos1=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos2=Notas$Sexo[Notas[,nombre_variable]==nombre_nivel]
        data=cbind(datos1,datos2)
        data=na.omit(data)
        newdata=as.data.frame(data)
        newdata$datos1=as.numeric(paste(newdata$datos1))
        colnames(newdata)=c("Nota","Sexo")
        mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
        #PARA GRAFICAS EVOLUTIVAS
        Matriz3=MatrizD[MatrizD$Modalidad==nombre_nivel,]
        Matriz3$Minimo=as.numeric(paste(Matriz3$Minimo))
        Matriz3$Cuartil1=as.numeric(paste(Matriz3$Cuartil1))
        Matriz3$Mediana=as.numeric(paste(Matriz3$Mediana))
        Matriz3$Media=as.numeric(paste(Matriz3$Media))
        Matriz3$Cuartil3=as.numeric(paste(Matriz3$Cuartil3))
        Matriz3$Maximo=as.numeric(paste(Matriz3$Maximo))
        output$hist4_1 <- renderPlot({
          ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                     geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist4_2 <- renderPlot({
          ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist4_3 <- renderPlot({
          ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                     geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist4_4 <- renderPlot({
          ggplot() + geom_point(aes(x=Año,y=Media),data = Matriz3,shape=21,fill=rainbow(1),size=3) + 
                        geom_line(aes(x=Año,y=Media,color="Media",group=1),data = Matriz3,lwd=1) + 
                     geom_point(aes(x=Año,y=Minimo),data = Matriz3,shape=21,fill=heat.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Minimo,color="Minimo",group=1),data = Matriz3,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil1),data = Matriz3,shape=21,fill=terrain.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil1,color="Cuartil1",group=1),data = Matriz3,lwd=1) + 
                     geom_point(aes(x=Año,y=Mediana),data = Matriz3,shape=21,fill=topo.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Mediana,color="Mediana",group=1),data = Matriz3,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil3),data = Matriz3,shape=21,fill=cm.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil3,color="Cuartil3",group=1),data = Matriz3,lwd=1) +  
                     geom_point(aes(x=Año,y=Maximo),data = Matriz3,shape=21,fill="blue",size=3) + 
                        geom_line(aes(x=Año,y=Maximo,color="Maximo",group=1),data = Matriz3,lwd=1) + 
                     labs(x="Año", y="Nota") +
                     scale_colour_manual("", 
                                        breaks = c("Media","Minimo","Cuartil1","Mediana","Cuartil3","Maximo"),
                                        values = c(heat.colors(1),rainbow(1),topo.colors(1),terrain.colors(1),"blue",cm.colors(1))) +
                     theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                     ggtitle("Evolucion Temporal")
        })
        output$hist4_5 <- renderPlot({
          ggplot(aes(x=Año,y=Media),data = Matriz3) + geom_point(shape=21,color="black",fill=cm.colors(1),size=6) + 
                                                      geom_line(color=heat.colors(1),lwd=2,group=1) +
                                                      theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                                                      labs(x="Año", y="Nota") + 
                                                      ggtitle("Evolucion Temporal Nota Media")
        })
        output$stats4 <- renderPrint({
          summary(datos)  
        })
        output$Box4 <- renderInfoBox({
          infoBox(
            "Número de estudiantes", length(datos), icon = icon("list"),
            color = "purple", fill = TRUE
          ) 
        })
        output$BoxBox4 <- renderInfoBox({
          infoBox(
            "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
            color = "yellow", fill = TRUE
          ) 
        })
      })
    }
    if(input$Variable=="Variable_Ensenyanza"){
      nombre_variable=gsub("Variable_","",input$Variable)
      observeEvent(input$Nivel5, {
        nombre_nivel=gsub("Nivel_","",input$Nivel5)
        #SELECCION UNILATERAL
        datos=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos=datos[!is.na(datos)]
        datos=datos[datos>0]
        new=as.data.frame(datos)
        #SELECCION AMBOS SEXOS
        datos1=Notas$NotaPAU[Notas[,nombre_variable]==nombre_nivel]
        datos2=Notas$Sexo[Notas[,nombre_variable]==nombre_nivel]
        data=cbind(datos1,datos2)
        data=na.omit(data)
        newdata=as.data.frame(data)
        newdata$datos1=as.numeric(paste(newdata$datos1))
        colnames(newdata)=c("Nota","Sexo")
        mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
        #PARA GRAFICAS EVOLUTIVAS
        Matriz4=MatrizE[MatrizE$`Tipo Centro`==nombre_nivel,]
        Matriz4$Minimo=as.numeric(paste(Matriz4$Minimo))
        Matriz4$Cuartil1=as.numeric(paste(Matriz4$Cuartil1))
        Matriz4$Mediana=as.numeric(paste(Matriz4$Mediana))
        Matriz4$Media=as.numeric(paste(Matriz4$Media))
        Matriz4$Cuartil3=as.numeric(paste(Matriz4$Cuartil3))
        Matriz4$Maximo=as.numeric(paste(Matriz4$Maximo))
        output$hist5_1 <- renderPlot({
          ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                     geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist5_2 <- renderPlot({
          ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist5_3 <- renderPlot({
          ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                     geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                     labs(title = paste0("NOTAS ",toupper(nombre_nivel)), x = "Notas", y = "Estudiantes") +
                     theme_gray(base_size = 14)
        })
        output$hist5_4 <- renderPlot({
          ggplot() + geom_point(aes(x=Año,y=Media),data = Matriz4,shape=21,fill=rainbow(1),size=3) + 
                        geom_line(aes(x=Año,y=Media,color="Media",group=1),data = Matriz4,lwd=1) + 
                     geom_point(aes(x=Año,y=Minimo),data = Matriz4,shape=21,fill=heat.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Minimo,color="Minimo",group=1),data = Matriz4,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil1),data = Matriz4,shape=21,fill=terrain.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil1,color="Cuartil1",group=1),data = Matriz4,lwd=1) + 
                     geom_point(aes(x=Año,y=Mediana),data = Matriz4,shape=21,fill=topo.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Mediana,color="Mediana",group=1),data = Matriz4,lwd=1) + 
                     geom_point(aes(x=Año,y=Cuartil3),data = Matriz4,shape=21,fill=cm.colors(1),size=3) + 
                        geom_line(aes(x=Año,y=Cuartil3,color="Cuartil3",group=1),data = Matriz4,lwd=1) +  
                     geom_point(aes(x=Año,y=Maximo),data = Matriz4,shape=21,fill="blue",size=3) + 
                        geom_line(aes(x=Año,y=Maximo,color="Maximo",group=1),data = Matriz4,lwd=1) + 
                     labs(x="Año", y="Nota") +
                     scale_colour_manual("", 
                                        breaks = c("Media","Minimo","Cuartil1","Mediana","Cuartil3","Maximo"),
                                        values = c(heat.colors(1),rainbow(1),topo.colors(1),terrain.colors(1),"blue",cm.colors(1))) +
                     theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                     ggtitle("Evolucion Temporal")
        })
        output$hist5_5 <- renderPlot({
          ggplot(aes(x=Año,y=Media),data = Matriz4) + geom_point(shape=21,color="black",fill=cm.colors(1),size=6) + 
                                                      geom_line(color=heat.colors(1),lwd=2,group=1) +
                                                      theme_ipsum(axis_title_size = 20, axis_title_just = "mc") +
                                                      labs(x="Año", y="Nota") + 
                                                      ggtitle("Evolucion Temporal Nota Media")
        })
        output$stats5 <- renderPrint({
          summary(datos)
        })
        output$Box5 <- renderInfoBox({
          infoBox(
            "Número de estudiantes", length(datos), icon = icon("list"),
            color = "purple", fill = TRUE
          ) 
        })
        output$BoxBox5 <- renderInfoBox({
          infoBox(
            "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
            color = "yellow", fill = TRUE
          ) 
        })
      })
    }
    if(input$Variable=="Variable2"){
      observeEvent(c(input$Variable21, input$Variable22), {
        if(input$Variable21=="Variable21_Curso" && input$Variable22=="Variable22_Curso"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Curso,input$Nivel22Curso), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Curso)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Curso" && input$Variable22=="Variable22_Municipio"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Curso,input$Nivel22Municipio), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Curso)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Curso" && input$Variable22=="Variable22_Conv"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Curso,input$Nivel22Convocatoria), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Curso)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Curso" && input$Variable22=="Variable22_ModalPAU"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Curso,input$Nivel22Modalidad), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Curso)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Curso" && input$Variable22=="Variable22_Ensenyanza"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Curso,input$Nivel22TipoCentro), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Curso)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Municipio" && input$Variable22=="Variable22_Curso"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Municipio,input$Nivel22Curso), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Municipio)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Municipio" & input$Variable22=="Variable22_Municipio"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Municipio,input$Nivel22Municipio), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Municipio)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Municipio" && input$Variable22=="Variable22_Conv"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Municipio,input$Nivel22Convocatoria), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Municipio)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Municipio" && input$Variable22=="Variable22_ModalPAU"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Municipio,input$Nivel22Modalidad), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Municipio)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Municipio" && input$Variable22=="Variable22_Ensenyanza"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Municipio,input$Nivel22TipoCentro), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Municipio)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Conv" && input$Variable22=="Variable22_Curso"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Convocatoria,input$Nivel22Curso), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Convocatoria)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Conv" && input$Variable22=="Variable22_Municipio"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Convocatoria,input$Nivel22Municipio), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Convocatoria)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Conv" && input$Variable22=="Variable22_Conv"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Convocatoria,input$Nivel22Convocatoria), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Convocatoria)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Conv" && input$Variable22=="Variable22_ModalPAU"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Convocatoria,input$Nivel22Modalidad), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Convocatoria)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Conv" && input$Variable22=="Variable22_Ensenyanza"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Convocatoria,input$Nivel22TipoCentro), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Convocatoria)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_ModalPAU" && input$Variable22=="Variable22_Curso"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Modalidad,input$Nivel22Curso), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Modalidad)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_ModalPAU" && input$Variable22=="Variable22_Municipio"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Modalidad,input$Nivel22Municipio), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Modalidad)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_ModalPAU" && input$Variable22=="Variable22_Conv"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Modalidad,input$Nivel22Convocatoria), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Modalidad)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_ModalPAU" && input$Variable22=="Variable22_ModalPAU"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Modalidad,input$Nivel22Modalidad), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Modalidad)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_ModalPAU" && input$Variable22=="Variable22_Ensenyanza"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21Modalidad,input$Nivel22TipoCentro), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21Modalidad)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Ensenyanza" && input$Variable22=="Variable22_Curso"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21TipoCentro,input$Nivel22Curso), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21TipoCentro)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Ensenyanza" && input$Variable22=="Variable22_Municipio"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21TipoCentro,input$Nivel22Municipio), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21TipoCentro)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Municipio)
            #SELECCION UNILATERAL
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Ensenyanza" && input$Variable22=="Variable22_Conv"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21TipoCentro,input$Nivel22Convocatoria), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21TipoCentro)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable21=="Variable21_Ensenyanza" && input$Variable22=="Variable22_ModalPAU"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21TipoCentro,input$Nivel22Modalidad), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21TipoCentro)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }        
        if(input$Variable21=="Variable21_Ensenyanza" && input$Variable22=="Variable22_Ensenyanza"){
          nombre_variable1=gsub("Variable21_","",input$Variable21)
          nombre_variable2=gsub("Variable22_","",input$Variable22)
          observeEvent(c(input$Nivel21TipoCentro,input$Nivel22TipoCentro), {
            nombre_nivel1=gsub("Nivel21_","",input$Nivel21TipoCentro)
            nombre_nivel2=gsub("Nivel22_","",input$Nivel22TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist22_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPOS CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPOS CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist22_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPOS CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats21 <- renderPrint({
              summary(datos)
            })
            output$Box21 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox21 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }        
      })
    }
    if(input$Variable=="Variable3"){
      observeEvent(c(input$Variable31, input$Variable32, input$Variable33), {
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Curso, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Curso, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Curso, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Curso, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Curso, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)

            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Municipio, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Municipio, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Municipio, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Municipio, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Municipio, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Convocatoria, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Convocatoria, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Convocatoria, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Convocatoria, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Convocatoria, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Modalidad, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Modalidad, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Modalidad, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Modalidad, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32Modalidad, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32TipoCentro, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CURSOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32TipoCentro, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32TipoCentro, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32TipoCentro, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Curso" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Curso,input$Nivel32TipoCentro, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Curso)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Curso, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Curso, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Curso, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Curso, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Curso, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Municipio, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Municipio, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Municipio, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Municipio, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Municipio, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Convocatoria, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Convocatoria, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2] 
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2] 
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2] 
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Convocatoria, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Convocatoria, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Convocatoria, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Modalidad, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Modalidad, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Modalidad, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Modalidad, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32Modalidad, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32TipoCentro, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32TipoCentro, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MUNICIPIOS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32TipoCentro, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32TipoCentro, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Municipio" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Municipio,input$Nivel32TipoCentro, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Municipio)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ",toupper(nombre_variable1)," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Curso, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Curso, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Curso, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Curso, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Curso, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Municipio, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Municipio, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Municipio, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Municipio, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Municipio, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Convocatoria, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Convocatoria, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Convocatoria, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Convocatoria, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Convocatoria, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Modalidad, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Modalidad, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Modalidad, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Modalidad, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32Modalidad, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32TipoCentro, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32TipoCentro, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32TipoCentro, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIAS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32TipoCentro, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Conv" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Convocatoria,input$Nivel32TipoCentro, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Convocatoria)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","CONVOCATORIA"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Curso, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Curso, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Curso, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Curso, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Curso, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Municipio, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Municipio, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Municipio, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Municipio, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Municipio, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3 & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Convocatoria, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Convocatoria, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Convocatoria, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Convocatoria, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Convocatoria, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Modalidad, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Modalidad, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Modalidad, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Modalidad, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32Modalidad, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","TIPO CENTRO"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32TipoCentro, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32TipoCentro, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32TipoCentro, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1),"-","TIPO CENTRO"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32TipoCentro, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDADES"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","TIPO CENTRO"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_ModalPAU" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31Modalidad,input$Nivel32TipoCentro, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31Modalidad)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 |  Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","MODALIDAD"," ",toupper(nombre_nivel1)," - ","TIPO CENTROS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Curso, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CURSOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Curso, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Curso, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Curso, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2) & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Curso" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Curso, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Curso)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2)," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Municipio, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Municipio, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MUNICIPIOS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Municipio, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Municipio, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-",toupper(nombre_variable2)," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Municipio" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Municipio, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Municipio)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ",toupper(nombre_variable2),"-",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Convocatoria, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Convocatoria, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Convocatoria, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","CONVOCATORIAS"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Convocatoria, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","CONVOCATORIA"," ",toupper(nombre_nivel2),"-","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Conv" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Convocatoria, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Convocatoria)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","CONVOCATORIA"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Modalidad, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            datos2=Notas$Sexo[(Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable3]==nombre_nivel3) & Notas[,nombre_variable2]==nombre_nivel2]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Modalidad, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Modalidad, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & Notas[,nombre_variable2]==nombre_nivel2 & Notas[,nombre_variable3]==nombre_nivel3]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1),"-","MODALIDAD"," ",toupper(nombre_nivel2),"-","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Modalidad, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable1]==nombre_nivel1 & (Notas[,nombre_variable2]==nombre_nivel2 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTRO"," ",toupper(nombre_nivel1)," - ","MODALIDADES"," ",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_ModalPAU" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32Modalidad, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32Modalidad)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            datos2=Notas$Sexo[Notas[,nombre_variable2]==nombre_nivel2 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable3]==nombre_nivel3)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel3)," - ","MODALIDAD"," ",toupper(nombre_nivel2)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Curso"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32TipoCentro, input$Nivel33Curso), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Curso)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos2=Notas$Sexo[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Municipio"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32TipoCentro, input$Nivel33Municipio), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Municipio)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos2=Notas$Sexo[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ",toupper(nombre_variable3)," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Conv"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32TipoCentro, input$Nivel33Convocatoria), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Convocatoria)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos2=Notas$Sexo[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","CONVOCATORIA"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_ModalPAU"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32TipoCentro, input$Nivel33Modalidad), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33Modalidad)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos2=Notas$Sexo[Notas[,nombre_variable3]==nombre_nivel3 & (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2)," - ","MODALIDAD"," ",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
        if(input$Variable31=="Variable31_Ensenyanza" && input$Variable32=="Variable32_Ensenyanza" && input$Variable33=="Variable33_Ensenyanza"){
          nombre_variable1=gsub("Variable31_","",input$Variable31)
          nombre_variable2=gsub("Variable32_","",input$Variable32)
          nombre_variable3=gsub("Variable33_","",input$Variable33)
          observeEvent(c(input$Nivel31TipoCentro,input$Nivel32TipoCentro, input$Nivel33TipoCentro), {
            nombre_nivel1=gsub("Nivel31_","",input$Nivel31TipoCentro)
            nombre_nivel2=gsub("Nivel32_","",input$Nivel32TipoCentro)
            nombre_nivel3=gsub("Nivel33_","",input$Nivel33TipoCentro)
            #SELECCION UNILATERAL
            datos=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 | (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos=datos[!is.na(datos)]
            datos=datos[datos>0]
            new=as.data.frame(datos)
            #SELECCION AMBOS SEXOS
            datos1=Notas$NotaPAU[Notas[,nombre_variable3]==nombre_nivel3 | (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            datos2=Notas$Sexo[Notas[,nombre_variable3]==nombre_nivel3 | (Notas[,nombre_variable1]==nombre_nivel1 | Notas[,nombre_variable2]==nombre_nivel2)]
            data=cbind(datos1,datos2)
            data=na.omit(data)
            newdata=as.data.frame(data)
            newdata$datos1=as.numeric(paste(newdata$datos1))
            colnames(newdata)=c("Nota","Sexo")
            mu <- ddply(newdata, "Sexo", summarise, grp.mean=mean(Nota))
            output$hist33_1 <- renderPlot({
              ggplot() + geom_histogram(aes(x=datos), data=new, bins=40, color="darkblue", fill="lightgreen") +
                         geom_vline(aes(xintercept=mean(datos)), color = "black", linetype="dashed", size=1) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_2 <- renderPlot({
              ggplot() + geom_density(aes(x=datos), data=new, color="darkblue", fill="lightblue", alpha=0.7) +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$hist33_3 <- renderPlot({
              ggplot() + geom_density(aes(x=Nota,fill=Sexo), data=newdata, alpha=0.7) +
                         geom_vline(data=mu, aes(xintercept=grp.mean, color=Sexo), linetype="dashed") +
                         labs(title = paste0("NOTAS ","TIPO CENTROS"," ",toupper(nombre_nivel1),"-",toupper(nombre_nivel2),"-",toupper(nombre_nivel3)), x = "Notas", y = "Estudiantes") +
                         theme_gray(base_size = 14)
            })
            output$stats31 <- renderPrint({
              summary(datos)
            })
            output$Box31 <- renderInfoBox({
              infoBox(
                "Número de estudiantes", length(datos), icon = icon("list"),
                color = "purple", fill = TRUE
              ) 
            })
            output$BoxBox31 <- renderInfoBox({
              infoBox(
                "Número de aprobados", length(which(datos>=5)), icon = icon("thumbs-up"),
                color = "yellow", fill = TRUE
              ) 
            })
          })
        }
      })
    }
    if(input$Variable=="Mapa_General"){
      observeEvent(input$MapaGeneral1,{
        clase=gsub("Clase_","",input$MapaGeneral1)
        if (clase=="General"){
          Cursos=CursoG
        }
        else if (clase=="2010"){
          Cursos=Curso1
        }
        else if (clase=="2011"){
          Cursos=Curso2
        }
        else if (clase=="2012"){
          Cursos=Curso3
        }
        else if (clase=="2013"){
          Cursos=Curso4
        }
        else if (clase=="2014"){
          Cursos=Curso5
        }
        else if (clase=="2015"){
          Cursos=Curso6
        }
        else if (clase=="2016"){
          Cursos=Curso7
        }
        else if (clase=="2017"){
          Cursos=Curso8
        }
        else if (clase=="2018"){
          Cursos=Curso9
        }
        else if (clase=="2019"){
          Cursos=Curso10
        }
        content <- paste('<strong>',"Municipio: ",
                         '</strong>',Cursos@data$Municipio,"<dd>",
                         '<strong>',"Estudiantes: ",
                         '</strong>',Cursos@data$Estudiantes,"<dd>",
                         '<strong>',"Nota Media: ",
                         '</strong>',round(Cursos@data$NotaMedia,3),"<dd>",
                         '<strong>',"Nota Media Hombres: ",
                         '</strong>',round(Cursos@data$NotaMediaH,3),"<dd>",
                         '<strong>',"Nota Media Mujeres: ",
                         '</strong>',round(Cursos@data$NotaMediaM,3),"<dd>")
        
        output$MapaGeneral <- renderLeaflet({
        zoom_mapa_general <<- 10
        center_mapa_general <<- list(x=-0.5067351104621309,y=39.463901271181754)
        
        leaflet(Cursos) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          setView(center_mapa_general$x, center_mapa_general$y, zoom = zoom_mapa_general) %>%
          addCircleMarkers(radius = ~if(clase=="General"){
                                        ifelse(Cursos@data$Estudiantes > 0 & Cursos@data$Estudiantes <= 500,10,ifelse(Cursos@data$Estudiantes > 500 & Cursos@data$Estudiantes <= 1000,20,30))
                                      }
                                      else{
                                        ifelse(Cursos@data$Estudiantes > 0 & Cursos@data$Estudiantes <= 50,10,
                                        ifelse(Cursos@data$Estudiantes > 50 & Cursos@data$Estudiantes <= 100,20,30))
                                      },
                           stroke = TRUE,
                           color = "red",
                           weight = 5,
                           opacity = 0.5,
                           fill = TRUE,
                           fillColor = "blue",
                           fillOpacity = 0.5,
                           popup = content,
                           popupOptions = popupOptions(closeOnClick = TRUE,
                                                       minWidth = 350,
                                                       maxWidth = 350))
      })
      })
    }
    if(input$Variable=="Mapa_Municipal"){
      observeEvent(input$MapaMunicipal1,{
        claseM=gsub("Clase_","",input$MapaMunicipal1)
        if (claseM=="Global"){
          Institutos=InstitutoG
          center_mapa = list(x=-0.5067351104621309,y=39.463901271181754)
          zoom_mapa = 10
        }
        else if (claseM=="ADEMUZ"){
          Institutos=Instituto1
          center_mapa = list(x=-1.286,y=40.063)
          zoom_mapa = 15
        }
        else if (claseM=="ALAQUAS"){
          Institutos=Instituto2
          center_mapa = list(x=-0.461,y=39.457)
          zoom_mapa = 15
        }
        else if (claseM=="ALBAIDA"){
          Institutos=Instituto3
          center_mapa = list(x=-0.518,y=38.838)
          zoom_mapa = 15
        }
        else if (claseM=="ALBAL"){
          Institutos=Instituto4
          center_mapa = list(x=-0.415,y=39.395)
          zoom_mapa = 15
        }
        else if (claseM=="ALBERIC"){
          Institutos=Instituto5
          center_mapa = list(x=-0.520,y=39.116)
          zoom_mapa = 15
        }
        else if (claseM=="ALBORAYA"){
          Institutos=Instituto6
          center_mapa = list(x=-0.349,y=39.501)
          zoom_mapa = 15
        }
        else if (claseM=="ALCASSER"){
          Institutos=Instituto7
          center_mapa = list(x=-0.444,y=39.367)
          zoom_mapa = 15
        }
        else if (claseM=="ALFAFAR"){
          Institutos=Instituto8
          center_mapa = list(x=-0.389,y=39.420)
          zoom_mapa = 15
        }
        else if (claseM=="ALGINET"){
          Institutos=Instituto9
          center_mapa = list(x=-0.471,y=39.265)
          zoom_mapa = 15
        }
        else if (claseM=="ALMUSSAFES"){
          Institutos=Instituto10
          center_mapa = list(x=-0.413,y=39.292)
          zoom_mapa = 15
        }
        else if (claseM=="AYORA"){
          Institutos=Instituto11
          center_mapa = list(x=-1.057,y=39.059)
          zoom_mapa = 15
        }
        else if (claseM=="BENAGUASIL"){
          Institutos=Instituto12
          center_mapa = list(x=-0.588,y=39.594)
          zoom_mapa = 15
        }
        else if (claseM=="BENETUSSER"){
          Institutos=Instituto13
          center_mapa = list(x=-0.397,y=39.422)
          zoom_mapa = 15
        }
        else if (claseM=="BENIFAIO"){
          Institutos=Instituto14
          center_mapa = list(x=-0.426,y=39.285)
          zoom_mapa = 15
        }
        else if (claseM=="BENIMAMET"){
          Institutos=Instituto15
          center_mapa = list(x=-0.421,y=39.502)
          zoom_mapa = 15
        }
        else if (claseM=="BETERA"){
          Institutos=Instituto16
          center_mapa = list(x=-0.459,y=39.589)
          zoom_mapa = 15
        }
        else if (claseM=="BOCAIRENT"){
          Institutos=Instituto17
          center_mapa = list(x=-0.611,y=38.766)
          zoom_mapa = 15
        }
        else if (claseM=="BUÑOL"){
          Institutos=Instituto18
          center_mapa = list(x=-0.791,y=39.418)
          zoom_mapa = 15
        }
        else if (claseM=="BURJASSOT"){
          Institutos=Instituto19
          center_mapa = list(x=-0.411,y=39.509)
          zoom_mapa = 15
        }
        else if (claseM=="CAMPOLIVAR"){
          Institutos=Instituto20
          center_mapa = list(x=-0.431,y=39.531)
          zoom_mapa = 15
        }
        else if (claseM=="CARCER"){
          Institutos=Instituto21
          center_mapa = list(x=-0.567,y=39.070)
          zoom_mapa = 15
        }
        else if (claseM=="CARLET"){
          Institutos=Instituto22
          center_mapa = list(x=-0.520,y=39.225)
          zoom_mapa = 15
        }
        else if (claseM=="CASTELLO_DE_RUGAT"){
          Institutos=Instituto23
          center_mapa = list(x=-0.381,y=38.874)
          zoom_mapa = 15
        }
        else if (claseM=="CATADAU"){
          Institutos=Instituto24
          center_mapa = list(x=-0.571,y=39.274)
          zoom_mapa = 15
        }
        else if (claseM=="CATARROJA"){
          Institutos=Instituto25
          center_mapa = list(x=-0.402,y=39.403)
          zoom_mapa = 15
        }
        else if (claseM=="CULLERA"){
          Institutos=Instituto26
          center_mapa = list(x=-0.254,y=39.164)
          zoom_mapa = 15
        }
        else if (claseM=="EL_PUIG"){
          Institutos=Instituto27
          center_mapa = list(x=-0.306,y=39.588)
          zoom_mapa = 15
        }
        else if (claseM=="EL_SALER"){
          Institutos=Instituto28
          center_mapa = list(x=-0.332,y=39.383)
          zoom_mapa = 15
        }
        else if (claseM=="GODELLA"){
          Institutos=Instituto29
          center_mapa = list(x=-0.410,y=39.518)
          zoom_mapa = 15
        }
        else if (claseM=="GUADASSUAR"){
          Institutos=Instituto30
          center_mapa = list(x=-0.476,y=39.185)
          zoom_mapa = 15
        }
        else if (claseM=="JALANCE"){
          Institutos=Instituto31
          center_mapa = list(x=-1.076,y=39.191)
          zoom_mapa = 15
        }
        else if (claseM=="L'ALCUDIA"){
          Institutos=Instituto32
          center_mapa = list(x=-0.504,y=39.194)
          zoom_mapa = 15
        }
        else if (claseM=="L'OLLERIA"){
          Institutos=Instituto33
          center_mapa = list(x=-0.553,y=38.914)
          zoom_mapa = 15
        }
        else if (claseM=="LA_CAÑADA-PATERNA"){
          Institutos=Instituto34
          center_mapa = list(x=-0.486,y=39.527)
          zoom_mapa = 15
        }
        else if (claseM=="LA_ELIANA"){
          Institutos=Instituto35
          center_mapa = list(x=-0.530,y=39.565)
          zoom_mapa = 15
        }
        else if (claseM=="LA_POBLA_DE_FARNALS"){
          Institutos=Instituto36
          center_mapa = list(x=-0.326,y=39.577)
          zoom_mapa = 15
        }
        else if (claseM=="LA_POBLA_DE_VALLBONA"){
          Institutos=Instituto37
          center_mapa = list(x=-0.553,y=39.591)
          zoom_mapa = 15
        }
        else if (claseM=="LLIRIA"){
          Institutos=Instituto38
          center_mapa = list(x=-0.595,y=39.625)
          zoom_mapa = 15
        }
        else if (claseM=="MASSAMAGRELL"){
          Institutos=Instituto39
          center_mapa = list(x=-0.330,y=39.570)
          zoom_mapa = 15
        }
        else if (claseM=="MASSANASSA"){
          Institutos=Instituto40
          center_mapa = list(x=-0.400,y=39.411)
          zoom_mapa = 15
        }
        else if (claseM=="MISLATA"){
          Institutos=Instituto41
          center_mapa = list(x=-0.417,y=39.475)
          zoom_mapa = 15
        }
        else if (claseM=="MONCADA"){
          Institutos=Instituto42
          center_mapa = list(x=-0.394,y=39.545)
          zoom_mapa = 15
        }
        else if (claseM=="MONTECAÑADA-PATERNA"){
          Institutos=Instituto43
          center_mapa = list(x=-0.467,y=39.536)
          zoom_mapa = 15
        }
        else if (claseM=="OLIVA"){
          Institutos=Instituto44
          center_mapa = list(x=-0.121,y=38.920)
          zoom_mapa = 15
        }
        else if (claseM=="ONTINYENT"){
          Institutos=Instituto45
          center_mapa = list(x=-0.609,y=38.821)
          zoom_mapa = 15
        }
        else if (claseM=="PATERNA"){
          Institutos=Instituto46
          center_mapa = list(x=-0.442,y=39.503)
          zoom_mapa = 15
        }
        else if (claseM=="PICASSENT"){
          Institutos=Instituto47
          center_mapa = list(x=-0.461,y=39.363)
          zoom_mapa = 15
        }
        else if (claseM=="PUZOL"){
          Institutos=Instituto48
          center_mapa = list(x=-0.302,y=39.617)
          zoom_mapa = 15
        }
        else if (claseM=="RAFELBUNYOL"){
          Institutos=Instituto49
          center_mapa = list(x=-0.332,y=39.588)
          zoom_mapa = 15
        }
        else if (claseM=="REQUENA"){
          Institutos=Instituto50
          center_mapa = list(x=-1.102,y=39.488)
          zoom_mapa = 15
        }
        else if (claseM=="S._ANTONIO_BENAGEBER"){
          Institutos=Instituto51
          center_mapa = list(x=-0.499,y=39.561)
          zoom_mapa = 15
        }
        else if (claseM=="SEDAVI"){
          Institutos=Instituto52
          center_mapa = list(x=-0.383,y=39.424)
          zoom_mapa = 15
        }
        else if (claseM=="SILLA"){
          Institutos=Instituto53
          center_mapa = list(x=-0.411,y=39.363)
          zoom_mapa = 15
        }
        else if (claseM=="SUECA"){
          Institutos=Instituto54
          center_mapa = list(x=-0.311,y=39.202)
          zoom_mapa = 15
        }
        else if (claseM=="TAVERNES_BLANQUES"){
          Institutos=Instituto55
          center_mapa = list(x=-0.362,y=39.506)
          zoom_mapa = 15
        }
        else if (claseM=="TAVERNES_DE_VALLDIGNA"){
          Institutos=Instituto56
          center_mapa = list(x=-0.267,y=39.071)
          zoom_mapa = 15
        }
        else if (claseM=="TURÍS"){
          Institutos=Instituto57
          center_mapa = list(x=-0.710,y=39.389)
          zoom_mapa = 15
        }
        else if (claseM=="UTIEL"){
          Institutos=Instituto58
          center_mapa = list(x=-1.205,y=39.568)
          zoom_mapa = 15
        }
        else if (claseM=="VALENCIA"){
          Institutos=Instituto59
          center_mapa = list(x=-0.376,y=39.469)
          zoom_mapa = 15
        }
        else if (claseM=="VILAMARXANT"){
          Institutos=Instituto60
          center_mapa = list(x=-0.622,y=39.568)
          zoom_mapa = 15
        }
        else if (claseM=="VILLANUEVA_CASTELLON"){
          Institutos=Instituto61
          center_mapa = list(x=-0.512,y=39.078)
          zoom_mapa = 15
        }
        else if (claseM=="VILLAR_DEL_ARZOBISPO"){
          Institutos=Instituto62
          center_mapa = list(x=-0.827,y=39.734)
          zoom_mapa = 15
        }
        content <- paste('<strong>',"Instituto: ",
                         '</strong>',Institutos@data$Instituto,"<dd>",
                         '<strong>',"Estudiantes: ",
                         '</strong>',Institutos@data$Estudiantes,"<dd>",
                         '<strong>',"Nota Media: ",
                         '</strong>',round(Institutos@data$NotaMedia,3),"<dd>",
                         '<strong>',"Nota Media Hombres: ",
                         '</strong>',round(Institutos@data$NotaMediaH,3),"<dd>",
                         '<strong>',"Nota Media Mujeres: ",
                         '</strong>',round(Institutos@data$NotaMediaM,3),"<dd>")
        output$MapaMunicipal <- renderLeaflet({
        zoom_mapa_municipal <<- zoom_mapa
        center_mapa_municipal <<- center_mapa
          
        leaflet(Institutos) %>%
          addProviderTiles("Esri.WorldStreetMap") %>%
          setView(center_mapa_municipal$x, center_mapa_municipal$y, zoom = zoom_mapa_municipal) %>%
          addCircleMarkers(radius = ~ifelse(Institutos@data$Estudiantes > 0 & Institutos@data$Estudiantes <= 100,10,
                                            ifelse(Institutos@data$Estudiantes > 100 & Institutos@data$Estudiantes <= 500,20,30)),
                           stroke = TRUE,
                           color = "red",
                           weight = 5,
                           opacity = 0.5,
                           fill = TRUE,
                           fillColor = "blue",
                           fillOpacity = 0.5,
                           popup = content,
                           popupOptions = popupOptions(closeOnClick = TRUE,
                                                       minWidth = 350,
                                                       maxWidth = 350))
      })
      })
    }
  })
}

  
  
  
  
  
  
