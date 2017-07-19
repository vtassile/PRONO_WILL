library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)
library(markdown)
library(rmarkdown)

dashboardPage(
  dashboardHeader(title = h3("Predicción de Calibre en Pera Williams"),titleWidth = 440),
  dashboardSidebar(width = 200,withMathJax(),
                   sidebarMenuOutput("menu"),
                   br(),br(),br(),br(),br(),br(),
                   div(img(src="foto.png")),br(),p("Valentín Tassile"), tags$p(h6("vtassile@gmail.com")),
                   a(href= "https://www.linkedin.com/in/valentin-tassile-724b2b51/",target="_blank",icon("linkedin-square", "fa-2x")),
                   br(),
                   br(),
                   div(img(src="LOGO_IOTA_2.png"))
                     ),
  dashboardBody(withMathJax(),
     tags$head(tags$style(HTML('.info-box {min-height: 40px;} .info-box-icon {height: 40px; line-height: 40px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
     tags$head(
       tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}
                       .selectize-dropdown .active {background: yellow !important;}"))
       ),
     
    ##############################################
    ####### PRIMER TABULACION: INTRODUCCION ######
    ##############################################
    tabItems(  
      tabItem(tabName = "intro", 
                box(title = "INTRODUCCION",status = "primary", solidHeader = TRUE,width = 18,
                    includeHTML("INTRO2.html")
                    #includeMarkdown("INTRO.Rmd")
                    )),
      #################################################################
      ####### SEGUNDA TABULACION: CONSOLIDACION DE BASES DE DATOS ######
      #################################################################      
      tabItem(tabName = "datos2",       # TERCERA TABULACION: CALIBRADO
              fluidRow( 
                box(title = "DATOS DE VALIDACIÓN",status = "primary", solidHeader = TRUE,width = 12,  
                    fluidRow(   
                      column(2,selectInput("E_TIPO", label=h5("Origen:"),
                                           c("Todos","Longitudinal","Transversal"),
                                           multiple = FALSE, helpText("Elija"))),
                      column(2,uiOutput("E_ANO5",align="center")),
                      column(2,uiOutput("E_SITIO5",align="center")),
                      column(2,uiOutput("E_FRUTO5",align="center")),
                      column(2,fluidRow(checkboxInput('SOLAPA', h5('Solapamiento'), TRUE),
                                        checkboxInput('SUAVIZA', h5('Suavizado'), FALSE))),
                      column(2,sliderInput("ALFA", h5("Transparencia:"), 0.5, min = 0.1, max = 1))
                    ) 
                )
                ,
                tabBox(title = "Visualización",width = 12,height = "500px",
                       #                   selected="Graficas",
                       tabPanel("Graficas",
                                fluidRow(
                                  column(12,plotlyOutput(outputId = "INFORMA7",width = "100%", height = "400px")))) ,
                       tabPanel("Datos",DT::dataTableOutput('contents7_1',width = "100%", height = "400px"))
                      )
                )),
            tabItem(tabName = "datos1",
              tabBox(title = "Gestion de Archivos",width = 12,id = "Gestion", height = "250px",
                     tabPanel("Calibres",
                              fluidRow( # INICIO DE fluidRow 1
                                box(title = "Selección",status = "primary", solidHeader = TRUE,width = 12,  
                                    column(3,fileInput('file1', 'Elija el Archivo CSV',
                                                       accept=c('text/csv','text/comma-separated-values,text/plain', 
                                                                '.csv'))), 
                                    column(3,checkboxInput('header', 'Encabezado', TRUE)),
                                    column(3,radioButtons('sep', 'Separador', c(Coma=',',Punto_y_Coma=';', Tabulación='\t'), ';')),
                                    column(3,radioButtons('quote', 'Caracteres', c(None='','Comilla Doble'='"', 'Comilla Simple'="'"), '"'))
                                )
                                ,
                                box(title = "Visualización",status = "warning", solidHeader = TRUE,width = 12,
                                    fluidRow(
                                    column(6, uiOutput("CLAVE",align="center")),
                                    column(6, DT::dataTableOutput('contents'))))
                              )  # FIN DE fluidRow 1               
                     ),
                     tabPanel("Temperaturas", 
                              fluidRow(   # INICIO DE fluidRow 2
                                box(title = "Selección",status = "primary", solidHeader = TRUE,width = 12,  
                                    column(3,fileInput('file2', 'Elija el Archivo CSV',
                                                       accept=c('text/csv','text/comma-separated-values,text/plain', 
                                                                '.csv'))), column(3,checkboxInput('header2', 'Encabezado', TRUE)),
                                    column(3,radioButtons('sep2', 'Separador', c(Coma=',',
                                                                                 Punto_y_Coma=';', Tabulación='\t'), ';')),
                                    column(3,radioButtons('quote2', 'Caracteres', c(None='',
                                                                                    'Comilla Doble'='"', 'Comilla Simple'="'"), '"'))
                                ),
                                box(title = "Visualización",status = "warning", solidHeader = TRUE,width = 12,
                                    column(12, uiOutput("CLAVE2",align="center")  ),
                                    column(12, dataTableOutput('contents2')))
                              ) # FIN DE fluidRow 2               
                     ),
                     tabPanel("Termoacumulación", 
                              fluidRow( # INICIO DE fluidRow 3
                                box(title = "Termuacumulación",status = "primary", solidHeader = TRUE,width = 12,  
                                    column(6, actionButton("goButton1", "Calcular Indices de Termoacumulación"))
                                ),
                                box(title = "Visualización",status = "warning", solidHeader = TRUE,width = 12,
                                    column(12, dataTableOutput('contents3'))
                                )
                              )  # FIN DE fluidRow 2               
                     ),
                     tabPanel("Consolidación", 
                              fluidRow( ### INICIO DE fluidRow 3
                                box(title = "Consolidacion",status = "primary", solidHeader = TRUE,width = 12,  
                                    column(6, actionButton("goButton2", "Consolidar Bases de Diámetros e Indices"))
                                ),
                                box(title = "Visualización",status = "warning", solidHeader = TRUE,width = 12,
                                    column(12, dataTableOutput('contents4'))
                                )
                              )  # FIN DE fluidRow 3               
                     ))),
      ###############################################
      # TERCER TABULACION: SELECCION DE MODELOS ####
      ###############################################
      tabItem(tabName = "modelos", 
                box(title = "MODELOS",status = "primary", solidHeader = TRUE,width = 18,
              includeHTML("MODELOS3.html"))
              #includeMarkdown("MODELOS3.Rhtml")
                ),
      ###############################################
      # CUARTA TABULACION: CALIBRADO DE MODELOS ####
      ###############################################
      tabItem(tabName = "calibrado",       # TERCERA TABULACION: CALIBRADO
              fluidRow( 
                box(title = "CALIBRADO",status = "primary", solidHeader = TRUE,width = 12,  
                    fluidRow(  
                      column(2,radioButtons("TIPO_D", label=h5("Tipo de Dato:"),
                                   c("Longitudinal" = "Longitudinal",
                                     "Transversal" = "Transversal"))),
                      
                      column(2,selectInput("E_MODELO", label=h5("Modelo:"),
                                           c("17" = "17","21" = "21","24" = "24","25" = "25",
                                             "26" = "26","27" = "27","30" = "30","32" = "32"),
                                           multiple = FALSE, helpText("Elija"))),
                      column(3,uiOutput("E_ANO",align="center")),
                      column(3,uiOutput("E_SITIO",align="center")),
                      column(2,uiOutput("E_DDF",align="center"))
                    ),  
                    fluidRow(   
                      column(2,sliderInput("Ciclo", h5("Máxima Iteracion:"), 30, min = 1, max = 100)),
                      column(2, sliderInput("Delta", h5("Delta Mínimo:"), 0.01, min = 0.001, max = 0.1)),  
                      column(3, checkboxInput('IDENTIFICADO', h5('Frutos identificados'), TRUE)),
                      column(2, radioButtons('ENFOQUE', h5('Enfoque'), c('FO'='FO','BLUP'="BLUP"), 'BLUP')),
                      column(2, actionButton("goButton3", h4("Iniciar Calibrado")))
                    ) 
                )
                       , box(title = "Estado del Proceso",status = "info", solidHeader = TRUE,
                    width = 12
                    ,fluidRow(
                      column(4,tags$p(" ",id="EST_ANO"),align="center"),
                      column(4,tags$p("",id="EST_SITIO"),align="center"),
                      column(4,tags$p("",id="EST_DDF"),align="center")
                             )
                    ,fluidRow(column(12,
                      tags$div(
                        class = "progress",
                        tags$div(id="demo3",
                          class = "progress-bar",
                          class = "progress-bar-info",
                          style = "min-width: 0em; width: 0%;",
                          role = "progressbar",
                          `aria-valuenow` = 0,
                          `aria-valuemin` = 0,
                          `aria-valuemax` = 100
                        )
                      )) 
                      ,
                             tags$head(tags$script('
                                Shiny.addCustomMessageHandler("testmessage",
                                  function changeContent (message) {
                                   var myelement = document.getElementById("demo3");
                                   saca= message.a;
                                   myelement.textContent= saca;

                                   myelement.style.width = message.c +"%";
                                   document.getElementById("EST_ANO").textContent=message.d;
                                   document.getElementById("EST_SITIO").textContent=message.e;
                                   document.getElementById("EST_DDF").textContent=message.f;
                                                                }
                                                                
                                                                   );')))
                       )
                  , tabBox(title = "Visualización",width = 12,id = "Procesa", height = "400px",
                     selected="Errores de Calibración",
                     tabPanel("Datos Procesados",column(12, DT::dataTableOutput('contents5_1'))),
                       tabPanel("Errores de Calibración",plotOutput(outputId = "INFORMA",width = "100%", height = "400px"))
                  )
                                                           ))
      ############################################        
      ###### CUARTA TABULACION: PREDICCION #######
      ############################################
          ,tabItem(tabName = "prediccion", 
                   fluidRow(
                     box(title = "Predicción",width = 12,status = "primary", solidHeader = TRUE,
                         
                               fluidRow(
                                 column(4,h5(textOutput("IMP_ORIGEN")),align="center",offset=1),
                                 column(2,h5(textOutput("IMP_MODELO")),align="center",offset=4),
                                 tags$head(tags$style("#IMP_ORIGEN{color: white;
                                                        font-size: 30px;background-color: blue;}"
                                 )),
                                 tags$head(tags$style("#IMP_MODELO{color: white;
                                                        font-size: 30px;background-color: blue;}"
                                 ))
                               ),
                               fluidRow(tags$style(HTML("div.box-header {
                                  text-align: center;}")),
                                  column(2,uiOutput("E_ANO2",align="center")),
                                  column(3,uiOutput("E_SITIO2",align="center")),
                                  column(2,box(title = "DDF",status = "primary", solidHeader = TRUE,width = 15,
                                         textOutput("E_DDF2_2",inline=TRUE),align="center"
                                              )
                                         ),
                                  column(2,uiOutput("E_DDF3",align="center")),
                                  column(2,radioButtons('ENFOQUE2', h5("Enfoque"), c('FO'='FO','BLUP'="BLUP"), 'BLUP'),align="center")
                                )),
                            box(title = "Métricas",width = 12,height = "230px",status = "info", solidHeader = TRUE,
                                fluidRow(  
                                  column(4, align="center",infoBoxOutput("METRICA_0")),   
                                  column(4, align="center",infoBoxOutput("METRICA_1")),    
                                  column(4, align="center",infoBoxOutput("METRICA_2")),
                                  column(4, align="center",infoBoxOutput("METRICA_3")),
                                  column(4, align="center",infoBoxOutput("METRICA_4")),
                                  column(4, align="center",infoBoxOutput("METRICA_5")),
                                  column(4, align="center",infoBoxOutput("METRICA_6"),offset=4)
                                  )
                                ,tags$style("#METRICA_0 {width:310px;}")
                                ,tags$style("#METRICA_1 {width:310px;}")
                                ,tags$style("#METRICA_2 {width:310px;}")
                                ,tags$style("#METRICA_3 {width:310px;}")
                                ,tags$style("#METRICA_4 {width:310px;}")
                                ,tags$style("#METRICA_5 {width:310px;}")
                                ,tags$style("#METRICA_6 {width:310px;}")
                                )
                            ,
                            tabBox(title = "Visualización",width = 12,height = "500px",
                                   #                   selected="Graficas",
                            tabPanel("Graficas",
                                     fluidRow(
                                       column(6,plotOutput(outputId = "INFORMA2",width = "100%", height = "400px")),
                                       column(6,plotlyOutput(outputId = "INFORMA33",width = "100%", height = "400px")))
                                   )
                              ,
                                   tabPanel("Datos Observados",DT::dataTableOutput('contents6_1',width = "100%", height = "400px"))
                            ,
                                   tabPanel("Datos Ajustados",DT::dataTableOutput('contents6_2',width = "100%", height = "400px")),
                                   tabPanel("Datos Consolidados",DT::dataTableOutput('contents6_3',width = "100%", height = "400px"))
                                 )    
                    )        
                    ) 
      ### FIN CUARTA TABULACION
              )
    )
    )