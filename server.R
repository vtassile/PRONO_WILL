library(shiny)
library(DT)
library(nlme)
library(stats)
library(Matrix)
library(splines)
library(car)
library(lme4)
library(nloptr)
library(MASS) 
library(lattice)
library(ggplot2)
library(optimx)
library(datasets)
library(Hmisc)
library(plyr)
library(dplyr)
library(manipulate)
library(shinyjs)
library(plotly)
library(knitr)
library(markdown)

shinyServer(function(input, output,session) {

 PARA <- reactiveValues(data = NULL)
 PARA2 <- reactiveValues(data = NULL)
 
 
RESTRINGE<-observe(
   {
    input$TIPO_D
    input$E_MODELO
    input$E_SITIO
    input$E_DDF
    input$E_Ciclo
    input$E_Delta
    input$IDENTIFICADO
    input$ENFOQUE
    PARA2$PP<-NULL     
   }   
 )
 
 
 output$menu <- renderMenu({
      input$goButton3
    RESTRINGE
   if (is.null(PARA2$PP)){
     sidebarMenu(
       menuItem("Introducción", tabName = "intro", icon = icon("info-circle"),
                badgeLabel = "nuevo", badgeColor = "green"),
       menuItem("Datos de Validación", tabName = "datos2", icon = icon("archive")),
       menuItem("Modelos", tabName = "modelos", icon = icon("th")),
       menuItem("Calibrado", tabName = "calibrado", icon = icon("history"))
     )  
   } else {
     sidebarMenu(
       menuItem("Introducción", tabName = "intro", icon = icon("info-circle"),
                badgeLabel = "nuevo", badgeColor = "green"),
       menuItem("Datos de Validación", tabName = "datos2", icon = icon("archive")),
       menuItem("Modelos", tabName = "modelos", icon = icon("th")),
       menuItem("Calibrado", tabName = "calibrado", icon = icon("history")),
       menuItem("Predicción", tabName = "prediccion", icon = icon("taxi"),
                badgeLabel = "nuevo", badgeColor = "red")
     )  
   } 
 })
 
 output$markdown <- renderUI({
    inclRmd("MODELOS.Rmd")
 })
 
 CListInput <- function (inputId, ANO, SITIO, DDF, value = "") 
 {
  tagList(tags$label(ANO, `for` = inputId),tags$label("  !        ", `for` = inputId),tags$label(SITIO, `for` = inputId), 
          tags$label("  !        ", `for` = inputId),tags$label(DDF, `for` = inputId),
          tags$label("   ", `for` = inputId),tags$input(id = inputId, 
                                    type = "text", value = value),tags$br())
 } 

 output$SALIDA_1 <- renderUI({
   
   if(input$n_modelo==17) {
     RETORNA<-withMathJax(helpText("
                                   Considera como efectos FIJOS: $$ \\beta_1 {_{TIPO_r}} , \\beta_2 , \\beta_3 {_{ TIPO_r}}$$  
                                   Efectos ALEATORIOS a nivel de SITIO: $$ b_{TRANS,3,SITIO_j} $$ 
                                   Efectos ALEATORIOS a nivel de FRUTO:$$ b_{2,FRUTO_{m(j)}} , b_{LONGI,1,FRUTO_{m(j)}} , b_{LONGI,3,FRUTO_{m(j)}} , b_{TRANS,3,FRUTO_{m(j)}}$$" 
     ))    
   }
   if (input$n_modelo == 21) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$\\beta_{1_{TIPO_r}}, \\beta_2 , \\beta_{3_{ TIPO_r}}$$  
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{2,SITIO_j} $$
                                     Eectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}} , b_{LONGI,1,FRUTO_{m(j)}} , b_{LONGI,3,FRUTO_{m(j)}} , b_{TRANS,3,FRUTO_{m(j)}}$$
                                     "))    
   }
   if (input$n_modelo == 24) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}} , \\beta_2, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$\\beta_3 (a_{3_{TEMP }})$$
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{LONGI,3,SITIO_j}, b_{TRANS,3,SITIO_j} $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}} $$
                                     "))    
   }
   if (input$n_modelo == 25) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}}, \\beta_2, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$ \\beta_3 (a_{3_{TEMP }}) $$
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{2,SITIO_j} $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}} $$
                                     "))    
   }
   
   if (input$n_modelo == 26) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}}, \\beta_2, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$ \\beta_3 (a_{3_{TEMP }}) $$
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{LONGI,3,SITIO_j} $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}} $$
                                     "))    
   }
   if (input$n_modelo == 27) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}}, \\beta_, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$ \\beta_3 (a_{3_{TEMP }}) $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}}  $$
                                     "))    
   }
   if (input$n_modelo == 30) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}}, \\beta_2, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$ \\beta_3 (a_{3_{TEMP }}) $$
                                     La covariable ambiental B.60.T.100.MIN.17 en los efectos FIJOS: $$ \\beta_2 (a_{2_{TEMP }}) $$
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{2,SITIO_j} $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}} $$
                                     "))    
   }
   if (input$n_modelo == 32) {
     RETORNA <- withMathJax(helpText("
                                     Considera como efectos FIJOS: $$ \\beta_{1_{TIPO_r}}, \\beta_2, \\beta_{3_{ TIPO_r}} $$
                                     La covariable ambiental B.122.T.150.MIN.22 en los efectos FIJOS: $$ \\beta_3 (a_{3_{TEMP }}) $$
                                     La covariable ambiental B.60.T.100.MIN.17 en los efectos FIJOS: $$ \\beta_2 (a_{2_{TEMP }}) $$
                                     Efectos ALEATORIOS a nivel de SITIO: $$ b_{LONGI,3,SITIO_j}, b_{2,SITIO_j} $$
                                     Efectos ALEATORIOS a nivel de FRUTO: $$ b_{2,FRUTO_{m(j)}}, b_{LONGI,1,FRUTO_{m(j)}}, b_{LONGI,3,FRUTO_{m(j)}}, b_{TRANS,3,FRUTO_{m(j)}} $$
                                     "))    
   }
   RETORNA   
   
   })
 
 output$SALIDA_2 <- renderUI({
   RETORNA2 <-" "
   if (input$n_modelo == 24) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   if (input$n_modelo == 25) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   
   if (input$n_modelo == 26) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   if (input$n_modelo == 27) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   if (input$n_modelo == 30) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   if (input$n_modelo == 32) {
     RETORNA2 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene la covariable ambiental B.122.T.150.MIN.22 que se obtiene por acumulación diaria de temperaturas que superan los 22 grados centígrados desde los 122 Días Posteriores al primero de Agosto (DDA) hasta los 150 DDA, que se corresponden aproximadamente a 64 y 92 DDPF respectivamente.
                                      "))    
   }
   RETORNA2   
   })
 
 output$SALIDA_3 <- renderUI({
   RETORNA3 <-" "
   if (input$n_modelo == 32) {
     RETORNA3 <- withMathJax(helpText("
                                      El MODELO CANDIDATO contiene, además, la covariable ambiental B.46.T.100.MIN.16 que se obtiene por acumulación diaria de temperaturas que superan los 17 grados centígrados desde los 60 Días Posteriores al primero de Agosto (DDA) hasta los 100 DDA, que se corresponden aproximadamente a 2 y 42 DDPF respectivamente.
                                      "))    
   }
   RETORNA3   
   })
 
 
output$E_ANO5 <- renderUI({
   if(input$E_TIPO=="Longitudinal") BASE_L<-LONGI_V
   if(input$E_TIPO=="Transversal") BASE_L<-TRANS_V
   if(input$E_TIPO=="Todos") { BASE_L<-rbind(TRANS_V,LONGI_V)    }
    OPCIONES<-as.list(unique(BASE_L$"ANO"))
   LARGO<-length(OPCIONES)
   if(LARGO==1) ELEGIDO<-OPCIONES[[1]][1]
   if(LARGO>1) ELEGIDO<-OPCIONES[1:LARGO]   
   selectInput("NE_ANO5",label = h5("AÑO:"),multiple=TRUE,
               choices = OPCIONES,selected=ELEGIDO)    
 })
 
 output$E_SITIO5 <- renderUI({
   if (is.null(input$NE_ANO5)) return()
   if(input$E_TIPO=="Longitudinal") BASE_L<-LONGI_V
   if(input$E_TIPO=="Transversal") BASE_L<-TRANS_V
   if(input$E_TIPO=="Todos") { BASE_L<-rbind(TRANS_V,LONGI_V)    }
   NEE_ANO<-as.character(levels(as.factor(input$NE_ANO5)))
   BASE_L <- filter(BASE_L, ANO %in% NEE_ANO)
   OPCIONES<-as.list(unique(BASE_L$"SITIO2"))
   LARGO<-length(OPCIONES)
   if(LARGO==1) ELEGIDO<-OPCIONES[[1]][1]
   if(LARGO>1) ELEGIDO<-OPCIONES[1:LARGO]   
   selectInput("NE_SITIO5",label = h5("SITIO:"),multiple=TRUE,
               choices = OPCIONES, 
               selected=ELEGIDO)    
 })
 
  output$E_FRUTO5 <- renderUI({
    if (is.null(input$NE_SITIO5)) return()
   if(input$E_TIPO=="Longitudinal") BASE_L<-LONGI_V
   if(input$E_TIPO=="Transversal") BASE_L<-TRANS_V
   if(input$E_TIPO=="Todos") { BASE_L<-rbind(TRANS_V,LONGI_V)    }
   NEE_ANO<-as.character(levels(as.factor(input$NE_ANO5)))
   BASE_L <- filter(BASE_L, ANO %in% NEE_ANO)
   NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO5))) 
   TOTO_2<- filter(BASE_L, SITIO2 %in% NEE_SITIO) 
   OPCIONES<-as.list(as.character(unique(BASE_L$"FRUTO4")))
   LARGO<-length(OPCIONES)+1
   OPCIONES[[LARGO]]<-as.character("Todos")
   if(LARGO==1) ELEGIDO<-OPCIONES[[1]][1]
   if(LARGO>1) ELEGIDO<-OPCIONES[LARGO]    
   selectInput("NE_FRUTO5",label = h5("FRUTO:"),multiple=TRUE,
               choices = OPCIONES,selected=ELEGIDO)    
 })
 
FRUTOS_S5 <- reactive({
   if(input$E_TIPO=="Longitudinal") BASE_L<-LONGI_V
   if(input$E_TIPO=="Transversal") BASE_L<-TRANS_V
   if(input$E_TIPO=="Todos") { BASE_L<-rbind(TRANS_V,LONGI_V)    }
   NEE_ANO<-as.character(levels(as.factor(input$NE_ANO5)))
   BASE_L <- filter(BASE_L, ANO %in% NEE_ANO)
   NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO5))) 
   TOTO_1<- filter(BASE_L, SITIO2 %in% NEE_SITIO)    
   
   if(input$NE_FRUTO5!="Todos"){
     NEE_FRUTO<-as.numeric(levels(as.factor(input$NE_FRUTO5))) 
     TOTO_1<- filter(TOTO_1, FRUTO4 %in% NEE_FRUTO)   
   }   
  TOTO_1 
})  

output$contents7_1 <- DT::renderDataTable(
  DT::datatable(FRUTOS_S5(), options = list(paging = TRUE,scrollX = TRUE))
  , options = list(pageLength = 2,scrollX = TRUE))


output$INFORMA7 <- renderPlotly({
  if (is.null(input$NE_ANO5)) return()
  if (is.null(input$NE_SITIO5)) return()
  if (is.null(input$NE_FRUTO5)) return()
  TOTO_5<-FRUTOS_S5()
  M_ALFA<-input$ALFA
  if(input$SOLAPA==FALSE){
    if(input$SUAVIZA==FALSE){
      p <- ggplot(data = TOTO_5, aes(x = DDF, y = DIAM)) +
        geom_point(aes(colour = SITIO2,shape=TIPO),alpha = M_ALFA) +
        facet_wrap(~ SITIO2)           
    } else   
    {
      p <- ggplot(data = TOTO_5, aes(x = DDF, y = DIAM)) +
        geom_point(aes(shape=TIPO),alpha = M_ALFA) +
        geom_smooth(aes(colour = SITIO2, fill = SITIO2)) + facet_wrap(~ SITIO2)      
    }
  } else
  {
    if(input$SUAVIZA==FALSE){
      p <- ggplot(data = TOTO_5, aes(x = DDF, y = DIAM)) +
      geom_point(aes(colour = SITIO2,shape=TIPO),alpha = M_ALFA)  
    } else
    {
      p <- ggplot(data = TOTO_5, aes(x = DDF, y = DIAM)) +
        geom_point(aes(colour = SITIO2,shape=TIPO),alpha = M_ALFA) +
        geom_smooth(aes(colour = SITIO2, fill = SITIO2))       
    }  
  }  
  ggplotly(p)
})

#### CARGA DE ARCHIVO DE DIAMETROS DE FRUTOS
 output$CLAVE <- renderUI({
  inFile <- input$file1
  if (is.null(inFile)) return(NULL)
  inFile <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
  checkboxGroupInput('show_vars', " ", names(inFile), selected = names(inFile),
                     inline=TRUE)
  }) 
  
  output$contents <- DT::renderDataTable({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    tabla_1<-datatable(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)[, input$show_vars, drop = FALSE],
             extensions = 'Scroller', options = list(deferRender = TRUE,dom = "frtiS",
             scrollY = 200,scrollX = TRUE,scrollCollapse = TRUE))
   }, options = list(pageLength = 2))  
  
  #### CARGA DE ARCHIVO DE TEMPERATURAS 
  output$CLAVE2 <- renderUI({
    inFile2 <- input$file2
    if (is.null(inFile2)) return(NULL)
      inFile2 <- read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, 
                       quote=input$quote2)
    checkboxGroupInput('show_vars2', " ", names(inFile2), selected = names(inFile2),
                       inline=TRUE)
  }) 
  
  output$contents2 <- renderDataTable({
    inFile2 <- input$file2
    if (is.null(inFile2)) return(NULL)
    datatable(read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, 
             quote=input$quote2)[, input$show_vars2, drop = FALSE],
             extensions = 'Scroller', options = list(deferRender = TRUE,dom = "frtiS",
             scrollY = 200,scrollX = TRUE,scrollCollapse = TRUE))
  }, options = list(pageLength = 2))
  
  
  output$contents3 <- renderDataTable({
    if (input$goButton1 == 0) return(NULL)
    inFile2 <- input$file2
    TEMPERATURA<-read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, 
             quote=input$quote2)[, input$show_vars2, drop = FALSE]
    covariables(CRITERIOS.COVARIABLES,TEMPERATURA,"T_MED","ANO")     
  }, options = list(pageLength = 2))  
  
  output$contents4 <- renderDataTable({
    if (input$goButton2 == 0) return(NULL)
    inFile2 <- input$file2
    TEMPERATURA<-read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, 
                          quote=input$quote2)[, input$show_vars2, drop = FALSE]
    AMBIENTALES<-covariables(CRITERIOS.COVARIABLES,TEMPERATURA,"T_MED","ANO")   
    inFile <- input$file1
    DIAMETROS<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)[, input$show_vars, drop = FALSE]
    datatable(merge(DIAMETROS,AMBIENTALES,by.x="ANO",by.y="ANO",all.x =TRUE),
              extensions = 'Scroller', options = list(deferRender = TRUE,dom = "frtiS",
                scrollY = 200,scrollX = TRUE,scrollCollapse = TRUE))
  }, options = list(pageLength = 2))  
  
# FUNCION REACTIVA QUE PERMITE CARGAR DIRECTAMENTE LOS ARCHIVOS PREDEFINIDOS
MEZCLA_R<-reactive({
  if(input$TIPO_D=="Longitudinal"){
    MEZCLA_1<-merge(LONGI_V,AMBIENTALES,by.x="ANO",by.y="ANO",all.x =TRUE)
  }
  if(input$TIPO_D=="Transversal"){
    MEZCLA_1<-merge(TRANS_V,AMBIENTALES,by.x="ANO",by.y="ANO",all.x =TRUE)
  }
  MEZCLA_1  
})  

observe({
  # TRUE if input$controller is odd, FALSE if even.
  if(input$TIPO_D=="Longitudinal"){
  updateCheckboxInput(session, "IDENTIFICADO", value = TRUE)
 }
  if(input$TIPO_D=="Transversal"){
    updateCheckboxInput(session, "IDENTIFICADO", value = FALSE)
  }
})

output$E_ANO <- renderUI({
  MEZCLA_1<-MEZCLA_R()
    selectInput("NE_ANO",label = h5("Año:"),
                 choices = as.character(MEZCLA_1$ANO), 
                 multiple = TRUE,
                 helpText("Elija"))    
})

output$E_SITIO <- renderUI({
  if (is.null(input$NE_ANO)) return()
  MEZCLA_1<-MEZCLA_R()
  MAESTRO_0 <- tbl_df(MEZCLA_1)
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO)))
  TOTO_1 <- filter(MAESTRO_0, ANO %in% NEE_ANO)
  selectInput("NE_SITIO",label = h5("Sitio:"),
              choices = as.character(TOTO_1$SITIO2), 
              multiple = TRUE,
              helpText("Elija"))    
}) 

output$E_DDF <- renderUI({
  if (is.null(input$NE_SITIO)) return("    ")
  MEZCLA_1<-MEZCLA_R()
  MAESTRO_0 <- tbl_df(MEZCLA_1)
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO)))
  TOTO_1 <- filter(MAESTRO_0, ANO %in% NEE_ANO)
  NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO))) 
  TOTO_2<- filter(TOTO_1, SITIO2 %in% NEE_SITIO) 
  DDF_INIC<-73
  if(input$E_MODELO =="17"){DDF_INIC<-0}
  if(input$E_MODELO =="21"){DDF_INIC<-0}
  TOTO_2<-TOTO_2[TOTO_2$DDF>DDF_INIC,]
  selectInput("NE_DDF",label = h5("DDF:"),
              choices = as.character(levels(as.factor(TOTO_2$DDF))), 
              multiple = TRUE,
              helpText("Elija"))    
}) 

B_AJUSTE<-eventReactive(input$goButton3, {
  if (is.null(input$NE_SITIO)) return()
  if (is.null(input$NE_DDF)) return()
  MEZCLA_1<-MEZCLA_R()
  MAESTRO_0 <- tbl_df(MEZCLA_1)
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO)))
  TOTO_1 <- filter(MAESTRO_0, ANO %in% NEE_ANO)
  NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO))) 
  TOTO_2<- filter(TOTO_1, SITIO2 %in% NEE_SITIO) 
  DDF_INIC<-73
  if(input$E_MODELO =="17"){DDF_INIC<-0}
  if(input$E_MODELO =="21"){DDF_INIC<-0}
  TOTO_2<-TOTO_2[TOTO_2$DDF>DDF_INIC,]
  NEE_DDF<-as.character(levels(as.factor(input$NE_DDF)))    
  BASE_0<- filter(TOTO_2, DDF %in% NEE_DDF)
  BASE_1<-BASE_0[order(BASE_0$ANO,BASE_0$SITIO2,BASE_0$FRUTO4,BASE_0$DDF),]
  BASE_1<-as.data.frame(BASE_1)
  
  #mensaje<-"Inicializando Calibración.....Macro Blups Versión 4.0"
  mensaje<-"15%"
  mensaje2 <- " "
  mensaje3 <- "Inicializando Calibración....."
  mensaje4 <- " "
  session$sendCustomMessage(type = 'testmessage', message = list(a = mensaje, b = "olive", c = 15,
                                                                 d = mensaje2, e = mensaje3, f = mensaje4))  
  
  ################################################
  ##### CALIBRAMOS EL MODELO CON LOS DATOS #######
  ################################################
  MAXIMO_CICLO=input$Ciclo
  VAR_DEP_CORR="DDF"
  DELTA_MINIMO=input$Delta
  if(input$ENFOQUE=="BLUP"){METODO<-"BLUP"}
  if(input$ENFOQUE=="FO"){METODO<-"ZERO"}
  if(input$IDENTIFICADO==FALSE){TIPO<-"TRANS"}
  if(input$IDENTIFICADO==TRUE){TIPO<-"LONGI"}
  
  if(input$E_MODELO =="17"){
    MODELO<-AB_MOD17
    MODELO_N<-"AB_MOD17"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  
    n_aleat<-2
    NOMBRES<- c("FRUTO4","SITIO2")
    if(TIPO=="LONGI")     {
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
    } else {
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
    }    
    nivel_2<-XX
    LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
    names(LISTA)[1]<-"SITIO2"
    names(LISTA)[2]<-"FRUTO4"    
  }
  if(input$E_MODELO =="21"){
    MODELO<-AB_MOD21
    MODELO_N<-"AB_MOD21"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  
    if(TIPO=="LONGI") {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"      
    }    
  }  
  if(input$E_MODELO =="24"){
    MODELO<-AB_MOD24
    MODELO_N<-"AB_MOD24"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  
    if(TIPO=="LONGI")     {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[6]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"      
    }         
  }  
  if(input$E_MODELO =="25"){
    MODELO<-AB_MOD25
    MODELO_N<-"AB_MOD25"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  
    if(TIPO=="LONGI")     {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"      
    }     
  }  
  if(input$E_MODELO =="26"){
    MODELO<-AB_MOD26
    MODELO_N<-"AB_MOD26"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))    
    if(TIPO=="LONGI")      {
      n_aleat<-2
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      LISTA<-list(NOMBRE_1=XX)
      names(LISTA)[1]<-"FRUTO4"
    }    
  }  
  if(input$E_MODELO =="27"){
    MODELO<-AB_MOD27
    MODELO_N<-"AB_MOD27"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))    
    if(TIPO=="LONGI") {
      n_aleat<-1
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      LISTA<-list(NOMBRE_1=XX)
      names(LISTA)[1]<-"FRUTO4"
    } else {
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      LISTA<-list(NOMBRE_1=XX)
      names(LISTA)[1]<-"FRUTO4"
    }    
  }  
  if(input$E_MODELO =="30"){
    MODELO<-AB_MOD30          
    MODELO_N<-"AB_MOD30"
    EXPRESION<-expression(1/((A+a1+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a1=D(EXPRESION,c("a1")),b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))   
    if(TIPO=="LONGI") {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      n_aleat<-2
      NOMBRES<- c("FRUTO4","SITIO2")
      nivel_1<-as.matrix(VarCorr(MODELO)[[5]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"      
    }    
  }  
  if(input$E_MODELO =="32"){
    MODELO<-AB_MOD32          
    MODELO_N<-"AB_MOD32"
    EXPRESION<-expression(1/((A+a1+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a1=D(EXPRESION,c("a1")),b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))    
    if(TIPO=="LONGI") {
      n_aleat<-2
      bb1<-as.numeric(VarCorr(MODELO)[[6]])
      cc1<-as.numeric(VarCorr(MODELO)[[5]])
      XX1<-matrix(data=0,nrow=2,ncol=2)
      diag(XX1)<-c(bb1,cc1)  
      nivel_1<-XX1
      aa<-as.numeric(VarCorr(MODELO)[[1]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[2]])
      XX<-matrix(data=0,nrow=3,ncol=3)
      diag(XX)<-c(aa,bb,cc)  
      nivel_2<-XX
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=nivel_2)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"
    } else {
      nivel_1<-as.matrix(VarCorr(MODELO)[[6]])
      bb<-as.numeric(VarCorr(MODELO)[[3]])
      cc<-as.numeric(VarCorr(MODELO)[[4]])
      XX<-matrix(data=0,nrow=2,ncol=2)
      diag(XX)<-c(bb,cc)  
      LISTA<-list(NOMBRE_1=nivel_1,NOMBRE_2=XX)
      names(LISTA)[1]<-"SITIO2"
      names(LISTA)[2]<-"FRUTO4"  
    }        
  } 

  if(class(MODELO)[1]=="nlme") {  # DEFINICION DE FORMULA PARA NLME
   VAR_DEP<-formula(MODELO)[[2]]  
  } else { # DEFINICION DE FORMULA PARA LMER
    VAR_DEP<-as.character(formula(MODELO)[[2]][[2]])  }  # FIN DEFINICION DE FORMULA PARA LMER
  
  CUENTA<-1
  T_ANO<-unique(BASE_1$ANO)
  for(i in 1:length(T_ANO)) {  
    TEMPO1<-BASE_1[BASE_1$ANO ==T_ANO[i],] 
    T_SITIO<-unique(as.character(TEMPO1$SITIO2))
    for(iii in 1:length(T_SITIO)) {
      VALIDA<-TEMPO1[TEMPO1$SITIO2 ==T_SITIO[iii],]
      ##############################################################
      #      BLUP_OF<-function(MODELO,VALIDA,NOMBRE_FIJOS,VAR_DEP_CORR="DDF",METODO="BLUP",
      #                     MAXIMO_CICLO=1,TIPO="LONGI",MODELO_N,DERIVADAS,EXPRESION){
      # PREPARAMOS LAS MATRICES PARA PODER OBTENER LOS BLUPs
      MAX_NIVEL<-length(unique(VALIDA[,as.character(names(LISTA)[1])]))
      MANTIENE<-1
      CICLO<-1
      # ENE LA LONGITUD TOTAL DE LOS EFECTOS ALEATORIOS PARA EL PRESENTE SITIO
      LONGITUD<-LARGO_UU(VALIDA,LISTA,1)
      UU<-diag(x=0,nrow=LONGITUD,ncol=MAX_NIVEL)
      DELTA1<-diag(x=0,nrow=LONGITUD,ncol=MAXIMO_CICLO)
      DELTA2<-diag(x=0,nrow=LONGITUD,ncol=MAXIMO_CICLO)
      # INICIA EL CICLO PARA OBTENER LOS BLUPS
      while(MANTIENE==1){
        for(i in 1:MAX_NIVEL) {    
          # OBTENEMOS LA MATRIZ D PARA EL SITIO2 i     
          # ESTE CORRE
          D<-OBTIENE_D(LISTA,VALIDA,i) 
          # OBTENEMOS LA MATRIZ Z PARA EL SITIO2 i   
          ZQ<-OBTIENE_Z(VALIDA,LISTA,DERIVADAS,UU[,i],i,MODELO,NOMBRE_FIJOS,MODELO_N,TIPO) 
          # OBTENEMOS LOS VALORES AJUSTADOS PARA EL SITIO2 i     
          PP<-AJUSTADO(VALIDA,LISTA,EXPRESION,DERIVADAS,UU[,i],i,MODELO,NOMBRE_FIJOS,MODELO_N,TIPO)
          DIF<-PP[,as.character(VAR_DEP)]-PP[,"Pred"]
          # OBTENEMOS LA MATRIZ R PARA EL SITIO2 i      
          RR<-OBTIENE_R(VALIDA,LISTA,MODELO,PP,VAR_DEP_CORR="DDF",i) 
          # OPERACIONES MATRICIALES
          TEMPO1 <- D%*%t(ZQ)
          TEMPO2 <- ZQ%*%D
          TEMPO3<- TEMPO2%*%t(ZQ)
          TEMPO4<-TEMPO3+RR
          TEMPO5<-solve(TEMPO4)
          TEMPO6<-TEMPO1%*%TEMPO5
          TEMPO7<-ZQ%*%UU[,i]
          DIF2<-DIF+TEMPO7
          if (METODO=="BLUP") UV<-TEMPO6%*%DIF2
          if (METODO=="ZERO") UV<-TEMPO6%*%DIF      
          DELTA1[,CICLO]<-UV-UU[,i]
          DELTA2[,CICLO]<-(UV-UU[,i])/UV*100
          UU[,i]<-UV
          #############################################################
          ##### ACTUALIZAMOS EN EL CLIENTE EL PROCESO ITERACTIVO ######
          ############################################################# 
          #mensaje<-paste("Proceso iteractivo en ejecución............ Macro Blups Versión 4.0. Ciclo: ",
          #               toString(CICLO))
          if (CICLO>5) {
            mensaje <-"90%"
            c_mensaje<-90
          } else           {
          c_mensaje <- 15+(CICLO*15)
          mensaje <- paste(toString(c_mensaje),"%")
          }
          mensaje2 <- paste("AÑO: ",T_ANO[i])
          mensaje3 <- paste("SITIO: ",T_SITIO[iii])
          mensaje4 <- paste ("DDF: ",NEE_DDF)
          session$sendCustomMessage(type = 'testmessage', message = list(a = mensaje, b = "olive",c = c_mensaje,
                                                                         d = mensaje2, e = mensaje3, f = mensaje4))    
        }
        U_FINAL<-UU
        for(i in 1:MAX_NIVEL) {
          PP_F<-AJUSTADO(VALIDA,LISTA,EXPRESION,DERIVADAS,U_FINAL[,i],i,MODELO,NOMBRE_FIJOS,MODELO_N,TIPO)
          if(i==1) PP_FINAL<-PP_F
          if(i>1) PP_FINAL<-rbind(PP_FINAL,PP_F)
        } 
        PP_FINAL$ERROR<-PP_FINAL[,as.character(VAR_DEP)]-PP_FINAL[,"Pred"]
        MM<-mean(PP_FINAL$ERROR)
        VV<-var(PP_FINAL$ERROR)
        PARA$DATO<-paste("Ciclo: ",toString(CICLO))
        if(CICLO==MAXIMO_CICLO) break
        if(max(abs(DELTA2[,CICLO]))<DELTA_MINIMO) break
        if(METODO=="ZERO") break
        CICLO<-CICLO+1    
      }
      PP_FINAL$METODO1<-METODO 
      PP_F<-PP_FINAL
      PP_F$MODELO<-MODELO_N
      if(CUENTA==1) PP_FINAL2<-PP_F
      if(CUENTA>1) {
        l <- list(PP_FINAL2, PP_F)
        PP_FINAL2<-do.call(rbind.fill, l)
      } 
      CUENTA<-CUENTA+1
    }   
  }
  #########################################################
  ##### FINALIZAMOS EN CLIENTE EL PROCESO ITERACTIVO ######
  ######################################################### 
  #mensaje<-paste("100%",
  #               toString(CICLO))
  mensaje <- "100%"
  mensaje2 <- " "
  mensaje3 <- "Calibración Finalizada "
  mensaje4 <- " "
  session$sendCustomMessage(type = 'testmessage', message = list(a = mensaje, b = "olive", c = 100,
                                                                 d = mensaje2, e = mensaje3, f = mensaje4)) 
  PARA2$PP<-PP_FINAL2
  PP_FINAL2
})

output$INFORMA <- renderPlot({
  BASE_G<-B_AJUSTE()
  if (is.null(BASE_G)) return(NULL)
  hist(BASE_G$ERROR,xlab = "Error",ylab = "Frecuencia",
       main = "Errores de Calibración",col = "lightblue")
})

output$contents5_1 <- DT::renderDataTable({
  BASE_G<-B_AJUSTE()
  if (is.null(BASE_G)) return(NULL)
  datatable(BASE_G,extensions = 'Scroller', options = list(deferRender = TRUE,dom = "frtiS",
            scrollY = 200,scrollX = TRUE,scrollCollapse = TRUE))
}, options = list(pageLength = 2,scrollX = TRUE))  


output$IMP_ORIGEN <- renderText({
  RETORNA<-paste("Datos de Tipo ",input$TIPO_D)
  RETORNA
})

output$IMP_MODELO <- renderText({
  RETORNA<-paste("MODELO ",input$E_MODELO)
  RETORNA
})


###############################################
#### COMIENZA EL PROCESO DE PREDICCION ########
###############################################

output$E_ANO2 <- renderUI({
  if (is.null(PARA2$PP)) return( )
   MEZCLA_1<-PARA2$PP
  selectInput("NE_ANO2",label = h5("Año:"),
              choices = as.character(MEZCLA_1$ANO), 
              multiple = FALSE,
              helpText("Elija"))    
})

output$E_SITIO2 <- renderUI({
  if (is.null(input$NE_ANO2)) return( )
  if (is.null(PARA2$PP)) return( )
  MEZCLA_1<-PARA2$PP
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO2)))
  TOTO_1 <- filter(MEZCLA_1, ANO %in% NEE_ANO)
  
  selectInput("NE_SITIO2",label = h5("Sitio:"),
              choices = as.character(TOTO_1$SITIO2), 
              multiple = FALSE,
              helpText("Elija"))    
}) 

output$E_DDF2 <- renderUI({
  if (is.null(input$NE_SITIO2)) return( )
  MEZCLA_1<-PARA2$PP
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO2)))
  TOTO_1 <- filter(MEZCLA_1, ANO %in% NEE_ANO)
  NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO2))) 
  TOTO_2<- filter(TOTO_1, SITIO2 %in% NEE_SITIO) 
  selectInput("NE_DDF2",label = h5("DDF:"),
              choices = as.character(levels(as.factor(TOTO_2$DDF))), 
              multiple = FALSE,
              helpText("Elija"))    
}) 

output$E_DDF2_2 <- renderText({
  if (is.null(input$NE_SITIO2)) return( )
  MEZCLA_1<-PARA2$PP
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO2)))
  TOTO_1 <- filter(MEZCLA_1, ANO %in% NEE_ANO)
  NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO2))) 
  TOTO_2<- filter(TOTO_1, SITIO2 %in% NEE_SITIO) 
  RETORNA <- as.character(levels(as.factor(TOTO_2$DDF)))
})  

output$E_DDF3 <- renderUI({
   if (is.null(input$NE_SITIO2)) return(  )
  TEMPORAL_0<-MEZCLA_R()
  TEMPORAL<-as.data.frame(TEMPORAL_0%>%filter(ANO==as.character(input$NE_ANO2),SITIO2==as.character(input$NE_SITIO2)))
  DDF_INIC<-73
  if(input$E_MODELO =="17"){DDF_INIC<-0}
  if(input$E_MODELO =="21"){DDF_INIC<-0}
  TOTO_1<-TEMPORAL[TEMPORAL$DDF>DDF_INIC,]
  OPCIONES<-as.list(as.character(unique(TOTO_1[,"DDF"])))  
  selectInput("NE_DDF3",label = h5("DDF Cosecha:"),
              choices = OPCIONES, 
              selected=OPCIONES[[length(OPCIONES)]])    
}) 

VALI_F1 <- reactive({
  #  DATOS LONGITUDINALES  ARCHIVO DE DIAMETROS OBSERVADOS A COSECHA
  if (is.null(is.null(input$NE_SITIO2))) return()
  BASE_0<-MEZCLA_R()  
  BASE_1<-BASE_0[order(BASE_0$ANO,BASE_0$SITIO2,BASE_0$FRUTO4,BASE_0$DDF),]
  TEMPORAL<-BASE_1%>%filter(ANO==as.character(input$NE_ANO2),
                            SITIO2==as.character(input$NE_SITIO2), 
                            DDF==as.numeric(input$NE_DDF3))  
  TEMPORAL<-TEMPORAL[,c("ANO","SITIO2","DDF","DIAM","FRUTO4")]
  
})

VALI_F2 <- reactive({
  if (is.null(input$NE_SITIO2)) return()
  MEZCLA_1<-PARA2$PP
  NEE_ANO<-as.character(levels(as.factor(input$NE_ANO2)))
  TOTO_1 <- filter(MEZCLA_1, ANO %in% NEE_ANO)
  NEE_SITIO<-as.character(levels(as.factor(input$NE_SITIO2))) 
  TOTO_2<- filter(TOTO_1, SITIO2 %in% NEE_SITIO) 
  F_DDF <- as.character(levels(as.factor(TOTO_2$DDF)))

  BASE_0<-PARA2$PP
  BASE_0$DDF<-as.numeric(BASE_0$DDF)
  BASE_1<-BASE_0[order(BASE_0$ANO,BASE_0$SITIO2,BASE_0$FRUTO4,BASE_0$DDF),]
  TEMPORAL<-as.data.frame(BASE_1%>%filter(ANO==as.character(input$NE_ANO2),
                                              SITIO2==as.character(input$NE_SITIO2),
                                              DDF==as.numeric(F_DDF)))
  TEMPORAL$DDF_COS<-as.numeric(input$NE_DDF3)
  ############################################
  ## PREDECIMOS LA VARIABLE DE RESPUESTA PARA LOS DDF A COSECHA ELEGIDOS  ############
     ######### FRUTOS TRANSVERSALES  ############
  if(input$ENFOQUE2=="BLUP"){METODO<-"BLUP"}
  if(input$ENFOQUE2=="FO"){METODO<-"ZERO"}
  if(input$E_MODELO =="17"){
    MODELO<-AB_MOD17
    MODELO_N<-"AB_MOD17"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  }
  if(input$E_MODELO =="21"){
    MODELO<-AB_MOD21
    MODELO_N<-"AB_MOD21"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  }  
  if(input$E_MODELO =="24"){
    MODELO<-AB_MOD24
    MODELO_N<-"AB_MOD24"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  }  
  if(input$E_MODELO =="25"){
    MODELO<-AB_MOD25
    MODELO_N<-"AB_MOD25"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))  }  
  if(input$E_MODELO =="26"){
    MODELO<-AB_MOD26
    MODELO_N<-"AB_MOD26"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c1+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(c1=D(EXPRESION,c("c1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))    }  
  if(input$E_MODELO =="27"){
    MODELO<-AB_MOD27
    MODELO_N<-"AB_MOD27"
    EXPRESION<-expression(1/((A+a2)*0.01+((B+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))    }  
  if(input$E_MODELO =="30"){
    MODELO<-AB_MOD30          
    MODELO_N<-"AB_MOD30"
    EXPRESION<-expression(1/((A+a1+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a1=D(EXPRESION,c("a1")),b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))   }  
  if(input$E_MODELO =="32"){
    MODELO<-AB_MOD32          
    MODELO_N<-"AB_MOD32"
    EXPRESION<-expression(1/((A+a1+a2)*0.01+((B+b1+b2)*0.01)*(((C+c2)*0.1)**(DDF))))
    NOMBRE_FIJOS<-c("A","B","C")
    DERIVADAS<-list(a1=D(EXPRESION,c("a1")),b1=D(EXPRESION,c("b1")),
                    a2=D(EXPRESION,c("a2")),b2=D(EXPRESION,c("b2")),c2=D(EXPRESION,c("c2")))      } 
    VALIDA<-TEMPORAL
  ##########################################        
  ###### LLAMA A LA FUNCION PREDICHO #######
  ##########################################
  PP_F<-PREDICHO(MODELO,EXPRESION,VALIDA,VAR_DEP_CORR="DDF",METODO,DDF_PRED=0,EXTIENDE=1,DERIVADAS)          
  PP_F<-as.data.frame(PP_F)
  PP_F<-PP_F[,c("ANO","SITIO2","DDF","DIAM","FRUTO4","Pred_f")]
})  

MERGE_1 <- reactive({
  if (is.null(input$NE_SITIO2)) return()
  if (input$TIPO_D=="Transversal" ) return()
    
  # PREPARA EL ARCHIVO PARA LOS DATOS LONGITUDINALES
  # ARCHIVO DE DIAMETROS OBSERVADOS A COSECHA
  BASE_G1<-VALI_F1()
  BASE_G1<-BASE_G1[,c("ANO","SITIO2","DDF","FRUTO4","DIAM")]
  BASE_G2<-VALI_F2()
  BASE_G2<-BASE_G2[,c("ANO","SITIO2","DDF","FRUTO4","Pred_f")]
  RETORNA<-merge(BASE_G1,BASE_G2,by="FRUTO4",all.x=FALSE,all.y=FALSE)   
})

MERGE_2 <- reactive({
  #  DATOS LONGITUDINALES  ARCHIVO DE DIAMETROS OBSERVADOS A COSECHA
  if (is.null(input$NE_SITIO2)) return()
  BASE_G1<-VALI_F1()
  BASE_G1<-BASE_G1[,c("ANO","SITIO2","DDF","FRUTO4","DIAM")]
  BASE_G2<-VALI_F2()
  BASE_G2<-BASE_G2[,c("ANO","SITIO2","DDF","FRUTO4","Pred_f")]
  # MEZCLO PARA ASEGURAR QUE LOS FRUTOS COMPARADOS SEAN IGUALES
   if (input$TIPO_D=="Longitudinal" ){
     RETORNA<-merge(BASE_G1,BASE_G2,by="FRUTO4",all.x=FALSE,all.y=FALSE)   
     BASE_G1_1<-RETORNA
     BASE_G1_1$CLASE<-"Observado"
     BASE_G1_1<-BASE_G1_1[,c("CLASE","DIAM")]
     BASE_G2_1<-RETORNA
     BASE_G2_1$DIAM<-BASE_G2_1$Pred_f
     BASE_G2_1$CLASE<-"Predicho"
     BASE_G2_1<-BASE_G2_1[,c("CLASE","DIAM")]
     BASE_G1_1<-as.data.frame(BASE_G1_1)
     BASE_G2_1<-as.data.frame(BASE_G2_1)
     # ARCHIVO DE DIAMETROS PREDICHOS A COSECHA  
     l <- list(BASE_G1_1,BASE_G2_1)
     TOTO_5<-do.call(rbind.fill, l)                  
     } else {
       BASE_G1$CLASE<-"Observado"
       BASE_G1<-BASE_G1[,c("CLASE","DIAM")]
       
       BASE_G2$DIAM<-BASE_G2$Pred_f
       BASE_G2$CLASE<-"Predicho"
       BASE_G2<-BASE_G2[,c("CLASE","DIAM")]
       BASE_G1<-as.data.frame(BASE_G1)
       BASE_G2<-as.data.frame(BASE_G2)
       # ARCHIVO DE DIAMETROS PREDICHOS A COSECHA  
       l <- list(BASE_G1,BASE_G2)
       TOTO_5<-do.call(rbind.fill, l)                       
   }
RETORNA<-TOTO_5
})

output$INFORMA2 <- renderPlot({
  input$NE_ANO2
  input$NE_SITIO2
  input$NE_DDF3
  if (is.null(input$NE_SITIO2)) return()
  TOTO_5<-MERGE_2()
  out <- histbackback(split(TOTO_5$DIAM, TOTO_5$CLASE), probability=TRUE, main = " ")
  # AGREGAR EL COLOR 
  barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
})

output$INFORMA33 <- renderPlotly({
  input$NE_ANO2
  input$NE_SITIO2
  input$NE_DDF3
  if (is.null(input$NE_SITIO2)) return()
  TOTO_5<-MERGE_2()
  TOTO_5
  p <- plot_ly(TOTO_5, y = ~DIAM, color = ~CLASE, type = "box")
  p
})

output$METRICA_0 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M <- MERGE_2()
    METRICA <- dim(BASE_M)[1]
    if (is.null(METRICA)) METRICA <- 0
    infoBox("N", METRICA, icon = icon("scale", lib = "glyphicon"),
            color = "red", fill = TRUE )    
  } else {
    BASE_M<-MERGE_1()
    METRICA <- dim(BASE_M)[1]
    if (is.null(METRICA)) METRICA <- 0
    infoBox("N", METRICA, icon = icon("scale", lib = "glyphicon"),
            color = "red", fill = TRUE )    
  }
})

output$METRICA_1 <- renderInfoBox({
  if (input$TIPO_D == "Transversal" ) {
    BASE_M <- MERGE_2()
    METRICA_1 <- mean(BASE_M[BASE_M$CLASE == "Observado",]$DIAM)
    METRICA_2 <- mean(BASE_M[BASE_M$CLASE == "Predicho",]$DIAM)
    MEDIDA <- as.numeric(METRICA_1 - METRICA_2)
    METRICA <- round(MEDIDA,3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Promedio", METRICA, icon = icon("king", lib = "glyphicon"),
            color = "fuchsia", fill = TRUE )          
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",1),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Medio", METRICA , icon = icon("king", lib = "glyphicon"),
            color = "fuchsia", fill = TRUE )      
  }
})

output$METRICA_2 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M<-MERGE_2()
    METRICA_1<-mean(BASE_M[BASE_M$CLASE=="Observado",]$DIAM)
    METRICA_2<-mean(BASE_M[BASE_M$CLASE=="Predicho",]$DIAM)
    MEDIDA<-(METRICA_1-METRICA_2)/METRICA_1*100
    METRICA<-round(MEDIDA,3)  
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Promedio Relativo", paste0(METRICA, "%"), icon = icon("king", lib = "glyphicon"),
            color = "purple", fill = TRUE )          
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",2),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Medio Absoluto", paste0(METRICA), icon = icon("king", lib = "glyphicon"),
            color = "purple", fill = TRUE )      
  } 
})

output$METRICA_3 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M<-MERGE_2()
    METRICA_1<-median(BASE_M[BASE_M$CLASE=="Observado",]$DIAM)
    METRICA_2<-median(BASE_M[BASE_M$CLASE=="Predicho",]$DIAM)
    MEDIDA<-METRICA_1-METRICA_2
    METRICA<-round(MEDIDA,3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Mediano", paste0(METRICA), icon = icon("tasks", lib = "glyphicon"),
            color = "yellow", fill = TRUE )          
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",3),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Medio Relativo", paste0(METRICA, "%"), icon = icon("list"),
            color = "yellow", fill = TRUE )      
  }
})

output$METRICA_4 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M<-MERGE_2()
    METRICA_1<-median(BASE_M[BASE_M$CLASE=="Observado",]$DIAM)
    METRICA_2<-median(BASE_M[BASE_M$CLASE=="Predicho",]$DIAM)
    MEDIDA<-(METRICA_1-METRICA_2)/METRICA_1*100
    METRICA<-round(MEDIDA,3)  
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Mediano Relativo", paste0(METRICA, "%"), icon = icon("tasks", lib = "glyphicon"),
            color = "aqua", fill = TRUE )          
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",4),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Sesgo Relativo Absoluto", paste0(METRICA, "%"), icon = icon("list"),
            color = "aqua", fill = TRUE )      
  }
})

output$METRICA_5 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M<-MERGE_2()
    METRICA_1<-mean(BASE_M[BASE_M$CLASE=="Observado",]$DIAM)
    METRICA_2<-sd(BASE_M[BASE_M$CLASE=="Observado",]$DIAM)
    MEDIDA<-METRICA_2/METRICA_1 *100
    METRICA<-round(MEDIDA,3)  
    if (is.null(METRICA)) METRICA <- 0
    infoBox("CV Observado", paste0(METRICA, "%"), icon = icon("tasks", lib = "glyphicon"),
            color = "teal", fill = TRUE )          
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",5),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Precisión de los Errores", paste0(METRICA), icon = icon("list"),
            color = "teal", fill = TRUE )      
  }
})

output$METRICA_6 <- renderInfoBox({
  if (input$TIPO_D=="Transversal" ) {
    BASE_M<-MERGE_2()
    METRICA_1<-mean(BASE_M[BASE_M$CLASE=="Predicho",]$DIAM)
    METRICA_2<-sd(BASE_M[BASE_M$CLASE=="Predicho",]$DIAM)
    MEDIDA<-METRICA_2/METRICA_1 *100
    METRICA<-round(MEDIDA,3)  
    if (is.null(METRICA)) METRICA <- 0
    infoBox("CV Predicho", paste0(METRICA, "%"), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green", fill = TRUE )  
  } else {
    BASE_M<-MERGE_1()
    METRICA<-round(CAPACIDAD(BASE_M,"DIAM","Pred_f",6),3)
    if (is.null(METRICA)) METRICA <- 0
    infoBox("Precisión Global", paste0(METRICA), icon = icon("list"),
            color = "green", fill = TRUE )      
  }
})

output$contents6_1 <- DT::renderDataTable(
  DT::datatable(VALI_F1(), options = list(paging = TRUE,scrollX = TRUE))
  , options = list(pageLength = 2,scrollX = TRUE))

output$contents6_2 <- DT::renderDataTable(
  DT::datatable(VALI_F2(), options = list(paging = TRUE,scrollX = TRUE))
  , options = list(pageLength = 2,scrollX = TRUE))


output$contents6_3 <- DT::renderDataTable(
  DT::datatable(MERGE_2(), options = list(paging = TRUE,scrollX = TRUE))
  , options = list(pageLength = 2,scrollX = TRUE))

})
