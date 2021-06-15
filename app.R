#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### ---------------------------------------------------------------------------

### PREPARACION DEL ENTORNO

### ---------------------------------------------------------------------------


# DIRECTORIO DE TRABAJO ---------------------------------------------------

### ---------------------------------------------------------------------------


# LIMPIEZA DE VARIABLES Y WARNINGS --------------------------------------------
rm(list=ls())
options(warn=-1) #warnings desactivados
### ---------------------------------------------------------------------------


# PAQUETES A INSTALAR ---------------------------------------------------------
## LISTADO ------------------------------------------------
required_packages <- c(
    "checkpoint"
)
## INSTALACION --------------------------------------------
new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
    install.packages(new.packages)
}

rm(new.packages)
### ---------------------------------------------------------------------------


# LIBRERIAS Y WARNING ON ------------------------------------------------------
library(checkpoint)
checkpoint(snapshotDate ='2021-05-01')

## LIBRERIAS
library(readxl)
library(readxlsb)
library(data.table)
library(xlsx)
library(ggplot2)

## MÁS LIBRERIAS
library(BatchGetSymbols)
library(broom)
library(datos)
library(DescTools)
library(dplyr)
library(DT)
library(forecast)
library(GGally)
library(openxlsx)
library(PMCMR)
library(rgdal)
library(shiny)
library(shinydashboard)
library(sp)
library(stringr)
library(stringi)
library(tidyverse)
library(TTR)
library(writexl)
library(zip)

library(AMR)
library(data.table)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shinyBS)
library(shinycssloaders)

library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(viridis)
library(zoo)

library(scales)
library(RCurl)
library(openair)
library(xts)

library(jsonlite)
library(httr)
library(knitr)
library(kableExtra)

options(warn=0) #warnings activos

### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------

### OBTENCION, PROCESADO Y ALMACENAMIENTO DE LOS DATOS

### ---------------------------------------------------------------------------


# CREAR CARPETA PARA GUARDAR DESCARGAS ----------------------------------------
directorio=list.files()
if (!"DatosPublicos" %in% list.files()){
    dir.create("DatosPublicos")
}
### ---------------------------------------------------------------------------



# DESCARGA CONJUNTO DE DATOS --------------------------------------------------
url <- "https://analisis.datosabiertos.jcyl.es/explore/dataset/"

## FORMATO EXCEL ------------------------------------------
#url_d <- "tasa-enfermos-acumulados-por-areas-de-salud/download/?format=csv&timezone=Europe/Berlin&lang=es&use_labels_for_header=true&csv_separator=%3B"
#destino <- "DatosPublicos/tasa-enfermos-acumulados-por-areas-de-salud_URL.csv"
#download.file(paste(url,url_d, sep=""), destino)
#URL <- read.csv(destino, sep=";", dec=".")

## FORMATO JSON -------------------------------------------
###	TASA_ENFERMOS - Tasa de enfermos por zonas básicas de salud
url_j <- "tasa-enfermos-acumulados-por-areas-de-salud/download/?format=json&timezone=Europe/Berlin&lang=es"
#URL <- fromJSON (paste(url,url_j,sep=""))
#tasa_enfermos <- URL$fields
tasa_enfermos <- fromJSON("DatosPublicos/tasa-enfermos-acumulados-por-areas-de-salud.json")

###	TASA_MORTALIDAD - Tasa de mortalidad COVID por zonas básicas de salud
url_j <- "tasa-mortalidad-covid-por-zonas-basicas-de-salud/download/?format=json&timezone=Europe/Berlin&lang=es"
#URL <- fromJSON (paste(url,url_j,sep=""))
#tasa_mortalidad <- URL$fields
tasa_mortalidad <- fromJSON("DatosPublicos/tasa-mortalidad-covid-por-zonas-basicas-de-salud.json")

###	FALLECIDOS - Mortalidad por COVID-19 por tramos de edad y sexo
url_j <- "mortalidad-por-tramos-de-edad-y-sexo/download/?format=json&timezone=Europe/Berlin&lang=es"
URL <- fromJSON (paste(url,url_j,sep=""))
fallecidos <- URL$fields

###	UCIS - Situación de hospitalizados por coronavirus en Castilla y León 
url_j <- "situacion-de-hospitalizados-por-coronavirus-en-castilla-y-leon/download/?format=json&timezone=Europe/Madrid&lang=es"
URL <- fromJSON (paste(url,url_j,sep=""))
ucis <- URL$fields

###	SITUACION - Situación epidemiológica por coronavirus en Castilla y León 
url_j <- "situacion-epidemiologica-coronavirus-en-castilla-y-leon/download/?format=json&timezone=Europe/Madrid&lang=es"
URL <- fromJSON (paste(url,url_j,sep=""))
situacion <- URL$fields

###	Personas Vacunadas COVID-19 
url_j <- "personas-vacunadas-covid/download/?format=json&timezone=Europe/Madrid&lang=es"
URL <- fromJSON (paste(url,url_j,sep=""))
vacunas <- URL$fields

## AUX FORMATO JSON ---------------------------------------
###	TASA_MORTALIDAD - Tasa de mortalidad por zonas básicas de salud
url_j <- "tasa-mortalidad-por-centros-de-salud/download/?format=json&timezone=Europe/Madrid&lang=es"
#URL <- fromJSON (paste(url,url_j,sep=""))
#tasa_mortalidad_T <- URL$fields
tasa_mortalidad_T <- fromJSON("DatosPublicos/tasa-mortalidad-por-centros-de-salud.json")


rm(URL)

### CARGAR MAPAS ------------------------------------------
contorno_area_salud = readOGR("Informacion_mapas/Areas_salud/hh.salud_cyl_areas.shp") # Mapa del area de salud de Cy
contorno_zbs = readOGR("Informacion_mapas/Zona_basica/hh.salud_cyl_zonas_basicas.shp") # Mapa de la Zona Básica de Salud

contorno_area_salud_df <- fortify(contorno_area_salud)
contorno_zbs_df <- fortify(contorno_zbs)
### ---------------------------------------------------------------------------


# FORMATO CAMPOS --------------------------------------------------------------
fallecidos$grupo_edad <- gsub("oct-19","10-19",fallecidos$grupo_edad)
fallecidos$grupo_edad <- gsub(">80","80+",fallecidos$grupo_edad)
vacunas$provincia <- gsub("TotalCyL","Castilla y León",vacunas$provincia)

etiqueta_Area_salud = data.frame(Nombre=c("El Bierzo","León","Palencia","Burgos","Zamora","Valladolid","Soria","Salamanca","Ávila","Segovia"),
                                 x=c( 175384, 285152, 385829, 467735, 180022, 345682, 539403, 210071, 339903, 449259), 
                                 y=c(4770254,4782969,4785437,4797262,4617477,4572358,4684879,4440224,4434655,4525332)) # Etiquetas del Area
numero_zbs = as.numeric(paste0("17",contorno_zbs$c_area_id,contorno_zbs$c_zbs_sec)) #Se concatena la información para el código de ZBS.
ciudades = data.frame(X=c(357423, 441802, 289488, 372599, 266362, 406566, 543158, 350196, 270698),
                      Y=c(4503793, 4696586, 4738108, 4659006, 4540650, 4526701, 4630930, 4613644, 4605358)) # Puntos donde se localizan las ciudades
### ---------------------------------------------------------------------------


# CAMPOS AUX FECHAS -----------------------------------------------------------
tasa_enfermos <- tasa_enfermos %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))

tasa_mortalidad <- tasa_mortalidad %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))

fallecidos <- fallecidos %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))

ucis <- ucis %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))

situacion <- situacion %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))

vacunas <- vacunas %>%
    mutate(fecha=as.Date(fecha),
           anio = year(fecha),
           anioMes = as.yearmon(fecha),
           anioTrim = as.yearqtr(fecha))
### ---------------------------------------------------------------------------


# INFORMACION e INTRO ---------------------------------------------------------
steps = read.csv("DatosAplicacion/help.csv", sep=";", dec=".")
intro = read.csv("DatosAplicacion/intro.csv", sep=";", dec=".")
### ---------------------------------------------------------------------------

# COLORES ---------------------------------------------------------------------
colores9GyRd=c("#1a1a1a","#4d4d4d","#878787","#bababa","#dbcbc2","#fcc6a7","#f4a582","#d6604d","#ba283b")
colores9RdGy=c("#ba283b","#d6604d","#f4a582","#fcc6a7","#dbcbc2","#bababa","#878787","#4d4d4d","#1a1a1a")
colores2RdGy=c("#ba283b","#878787")
colores1RdGy=c("#d6979f")
#gris negro rojo
colores9GyRd=c("#bbbbbb","#727272","#484848","#232323","#000000","#341515","#581c20","#85232c","#ba283b")
colores9RdGy=c("#ba283b","#85232c","#581c20","#341515","#000000","#232323","#484848","#727272","#bbbbbb")
colores7GyRd=c("#e5e5e5","#bababa","#b9a4a6","#b8888e","#b87079","#b85965","#ba283b")#"#b73c4b",

colores5GyRd=c("#b9a4a6","#b8888e","#b87079","#b85965","#ba283b")
#colores9GyRd1=c("#ba283b","#1a1a1a","#4d4d4d","#878787","#bababa","#dbcbc2","#fcc6a7","#f4a582","#d6604d")
### ---------------------------------------------------------------------------


### --------------------------------------------------------------------
### Inteface - UI 
### --------------------------------------------------------------------

ui = dashboardPage(
    
    
    # 1 - CABECERA ------------------------------------------------------------------		
    dashboardHeader(
        # Titulo de arriba a la izquierda
        title = span(icon("diagnoses"), "COVID CyL"), #virus
        titleWidth = 300,
        # Menu desplegable con el interrogante de la derecha
        dropdownMenu(
            type = "notifications", 
            headerText = strong("AYUDA"), 
            icon = icon("question"), 
            badgeStatus = NULL,
            notificationItem(
                text = (steps$text[1]),
                icon = icon("spinner")
            ),
            notificationItem(
                text = steps$text[2],
                icon = icon("address-card")
            ),
            notificationItem(
                text = steps$text[3],
                icon = icon("map-marked-alt")
            ),
            notificationItem(
                text = steps$text[4],
                icon = icon("clinic-medical")
            ),
            notificationItem(
                text = steps$text[5],
                icon = icon("hospital-alt")
            ),
            notificationItem(
                text = steps$text[6],
                icon = icon("dungeon")
            ),
            notificationItem(
                text = steps$text[7],
                icon = icon("calendar")
            ),
            notificationItem(
                text = strong(steps$text[8]),
                icon = icon("exclamation")
            )
        ),
        # Enlace de más acerca de COVID CyL arriba a la derecha 
        
        tags$li(
            a(introBox(data.step = 6, data.intro = intro$text[6], # intro tour	
                       strong("Acerca de COVID CyL"),
                       height = 40,
                       href = "https://analisis.datosabiertos.jcyl.es/pages/home/",
                       #href = "https://www.jcyl.es/web/es/informacion-coronavirus.html",
                       title = "",
                       target = "_blank"
            ) ),
            class = "dropdown"
        )
    ),
    #FIN_dashboardHeader 1 - CABECERA
    
    # 2 - BARRA LATERAL -----------------------------------------------------------------	
    
    dashboardSidebar(
        width = 300,
        introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
                 div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),	
                 sidebarMenu(
                     introBox(data.step = 1, data.intro = intro$text[1], # intro tour
                              div(id = "sidebar_button",
                                  bsButton(inputId = "confirm", 
                                           label = "EMPEZAR COVID CyL", 
                                           icon = icon("play-circle"), 
                                           style = "danger")
                              )
                     ),
                     div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                     br(),
                     # PACIENTES - primer nivel
                     menuItem(
                         "PACIENTES",
                         tabName = "pacientes",
                         icon = icon("address-card"),
                         checkboxGroupInput(
                             inputId = "genero_Input",
                             label = "Género",
                             #choices = unique(admissions$gender),
                             #selected = unique(admissions$gender),
                             choices = list("G. Hombre" = "Hombres", "G. Mujer" = "Mujeres"), #list("Hombre","Mujer"),
                             selected = list("Hombre","Mujer"),
                             inline = TRUE #Todo en una misma linea? si
                         ),
                         sliderInput(
                             inputId = "edad_Input",
                             label = "Edad",
                             #value = c(min(admissions$age, na.rm = TRUE), max(admissions$age, na.rm = TRUE)),
                             #min = min(admissions$age, na.rm = TRUE),
                             #max = max(admissions$age, na.rm = TRUE),
                             value = c(0, 100),
                             min = 0,
                             max = 100,            
                             step = 1,
                             sep = ""
                         )
                     ),
                     # PROVINCIAS - primer nivel
                     menuItem(
                         "PROVINCIAS",
                         tabName = "provincias",
                         icon = icon("map-marked-alt"),
                         checkboxGroupInput(
                             inputId = "prov_Input",
                             label = "PROVINCIA",
                             choices = sort(factor(unique(situacion$provincia))),
                             selected = sort(factor(unique(situacion$provincia))),
                             inline = FALSE #Todo en una misma linea? no
                         )
                     ),
                     # Centro Salud - primer nivel
                     menuItem(
                         "CENTROS DE SALUD",
                         tabName = "centroS",
                         icon = icon("clinic-medical"),
                         checkboxGroupInput(
                             inputId = "cs_Input",
                             label = "CENTROS DE SALUD",
                             choices = sort(factor(unique(tasa_mortalidad$centro))),
                             selected = sort(factor(unique(tasa_mortalidad$centro))),
                             inline = FALSE #Todo en una misma linea? no
                         )
                     ),
                     # Centro Hospitalario - primer nivel
                     menuItem(
                         "CENTROS HOSPITALARIOS",
                         tabName = "centroH",
                         icon = icon("hospital-alt"),
                         checkboxGroupInput(
                             inputId = "ch_Input",
                             label = "CENTROS HOSPITALARIOS",
                             choices = sort(factor(unique(ucis$hospital))),
                             selected = sort(factor(unique(ucis$hospital))),
                             inline = FALSE #Todo en una misma linea? no
                         )
                     ),
                     # Tipo centro - primer nivel
                     menuItem(
                         "TIPO DE CENTROS",
                         tabName = "centroT",
                         icon = icon("dungeon"),
                         checkboxGroupInput(
                             inputId = "ct_Input",
                             label = "TIPO CENTRO",
                             choices = sort(factor(unique(tasa_enfermos$tipo_centro))),
                             selected = sort(factor(unique(tasa_enfermos$tipo_centro))),
                             inline = FALSE #Todo en una misma linea? no
                         )
                     ),
                     # FECHA - primer nivel
                     menuItem(
                         "FECHA",
                         tabName = "fecha",
                         icon = icon("calendar"),
                         sliderInput(
                             inputId = "fecha_Input",
                             label = "Fecha",
                             value = c(min(unique(as.Date(fallecidos$fecha))), max(unique(as.Date(fallecidos$fecha)))),
                             min=min(unique(as.Date(fallecidos$fecha))),
                             max=max(unique(as.Date(fallecidos$fecha)))
                             #step = 1L,
                             #sep = ""
                         )
                     ),
                     br(),      
                     # VACUNAS - primer nivel               
                     #	menuItem(
                     #                 "VACUNAS",
                     #                 tabName = "vacunas",
                     #                 icon = icon("syringe"),#flask
                     #		selectInput(
                     #                   inputId = "diagnosticsInput",
                     #                   label = "",
                     #                   choices = list("Blood cultures" = "bc_timing", "Urine cultures" = "uc_timing")
                     #                 ),
                     #		sliderInput(
                     #                   inputId = "checkInput",
                     #                   label = "VACUNAS ADMINISTRADAS",
                     #                   #value = c(-1L, 1L),
                     #                   #min = min(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                     #                   #max = max(c(admissions$bc_timing, admissions$uc_timing), na.rm = TRUE),
                     #                   value = c(min(vacunas_acumuladas$dosis_administradas, na.rm = TRUE), max(vacunas_acumuladas$dosis_administradas, na.rm = TRUE)),
                     #                   min = min(vacunas_acumuladas$dosis_administradas, na.rm = TRUE),
                     #                   max = max(vacunas_acumuladas$dosis_administradas, na.rm = TRUE),
                     #                   step = 1L
                     #                 )
                     #               ),
                     #	br(),
                     menuItem(
                         "DESCARGAR DATOS",
                         tabName = "download",
                         icon = icon("download"),
                         textInput(
                             inputId = "filename",
                             placeholder = "Nombre de archivo de descarga",
                             label = ""
                         ),
                         div(
                             downloadButton(
                                 outputId = "descargaMortDatos",
                                 label = "Guardar datos de Mortalidad",
                                 icon = icon("download"),
                                 style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                             )
                         ),
                         div(
                             downloadButton(
                                 outputId = "descargaVacDatos",
                                 label = "Guardar datos de Vacunaciones",
                                 icon = icon("download"),
                                 style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                             )
                         )
                     ),
                     br()
                 ) #sidebarMenu
        )#introBox
    ),#FIN_dashboardSidebar 2 - BARRA LATERAL
    
    # 3 - BODY --------------------------------------------------------------------
    dashboardBody(
        introBox(data.step = 2, data.intro = intro$text[2], #  intro tour
                 div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),	
                 sidebarMenu(
                     tags$head(
                         tags$link(
                             rel = "stylesheet", 
                             type = "text/css", 
                             href = "COVIDCyL_style.css"
                         )
                     ),
                     includeCSS("www/COVIDCyL_style.css"), 
                     useShinyjs(),
                     introjsUI(),
                     
                     #		#plotOutput(outputId = "distPlot")
                     #		fluidRow(
                     #		  column(
                     #			width = 12,
                     #			introBox(
                     #			  bsButton("tab_1_confirm", 
                     #					   label = "Situación", 
                     #					   icon = icon("user"), 
                     #					   style = "success"),
                     #			  bsButton("tab_2_confirm", 
                     #					   label = "Pruebas", 
                     #					   icon = icon("microscope", class = "spinner-box"), 
                     #					   style = "success"),
                     #			  bsButton("tab_3_confirm", 
                     #					   label = "Hospitalizados", 
                     #					   icon = icon("hospital-user", class = "flask-box"), 
                     #					   style = "success"),
                     #			  bsButton("tab_4_confirm", 
                     #					   label = "Fallecidos", 
                     #					   icon = icon("cross"), 
                     #					   style = "success"),
                     #			  bsButton("tab_5_confirm", 
                     #					   label = "Modelo Predictivo", 
                     #					   icon = icon("chart-line"), 
                     #					   style = "success"),
                     #			  data.step = 2, data.intro = intro$text[2])
                     #		  )
                     #		),
                     #		
                     #		fluid_design("tab_2_panel", "box1", "box2", "box3", "box4"),
                     #		fluid_design("tab_3_panel", "box5", "box6", "box7", "box8"),
                     #		fluid_design("tab_4_panel", "box5", "box6", "box7", "box8"),
                     #		fluid_design("tab_5_panel", "box_los1", "box_los2", "box_los3", NULL),
                     #		
                     #		fluidRow(
                     #		  div(
                     #			id = "tab_1_panel", 
                     #			column(
                     #			  width = 12,
                     #			  introBox(data.step = 4, data.intro = intro$text[4],
                     #					   uiOutput("box_pat")
                     #			  )
                     #			),
                     #			column(
                     #			  width = 6,
                     #			  uiOutput("box_pat2")
                     #			),
                     #			column(
                     #			  width = 6,
                     #			  uiOutput("box_year")
                     #			)
                     #		  )
                     #		)
                     
                     
                     tabsetPanel(
                         # TAB_1 -----------------------------------------------------------
                         tabPanel(
                             bsButton(inputId = "tab_1_confirm", 
                                      label = "Situación", 
                                      icon = icon("heartbeat"), #diagnoses
                                      style = "success"),
                             div(			
                                 fluidRow(					
                                     box(
                                         introBox(data.step = 4, data.intro = intro$text[4],
                                                  #title = "Contagios positivos por provincias",
                                                  uiOutput("box_pat1")#, height = 400)
                                         )
                                     ),
                                     box(
                                         #title = "Hospitalizados en Planta y UCI",
                                         uiOutput("box_pat2")#, height = 400)
                                     )
                                     #	introBox(data.step = 4, data.intro = intro$text[4])
                                 ),
                                 fluidRow(			
                                     box(
                                         uiOutput("box_pat3", height = 400)
                                     ),
                                     box(
                                         uiOutput("box_pat4", height = 400)
                                     )
                                 )
                             )			
                         ),
                         # TAB_2 -----------------------------------------------------------
                         tabPanel(
                             bsButton(inputId = "tab_2_confirm", 
                                      label = "Pruebas", 
                                      icon = icon("microscope"), 
                                      style = "success"),
                             div(			
                                 fluidRow(			
                                     box(
                                         uiOutput("box1", height = 600)
                                     ),
                                     box(
                                         uiOutput("box2", height = 2000)
                                     )
                                 )
                             )
                             
                         ),
                         # TAB_3 -----------------------------------------------------------
                         tabPanel(
                             bsButton(inputId = "tab_3_confirm",
                                      label = "Hospitalizados", 
                                      icon = icon("hospital-user"), 				
                                      style = "success"),
                             div(
                                 fluidRow(			
                                     box(
                                         uiOutput("box5", height = 600)
                                     ),
                                     box(
                                         uiOutput("box6", height = 600)
                                     )
                                 )
                             )
                         ),
                         # TAB_4 -----------------------------------------------------------
                         tabPanel(
                             bsButton(inputId = "tab_4_confirm", 
                                      label = "Fallecidos", 
                                      icon = icon("cross"),  
                                      style = "success"),
                             div(			
                                 fluidRow(			
                                     box(
                                         uiOutput("box9", height = 400)
                                     ),
                                     box(
                                         uiOutput("box11", height = 400)
                                     )
                                 ),
                                 fluidRow(			
                                     box(
                                         uiOutput("box10", height = 2000)
                                     )
                                 )
                             )
                             
                         ),	
                         # TAB_5 -----------------------------------------------------------
                         tabPanel(
                             bsButton(inputId = "tab_5_confirm", 
                                      label = "Modelo Predictivo", 
                                      icon = icon("chart-line"), 
                                      style = "success")						
                         )	
                         #tabPanel("Summary", verbatimTextOutput("summary")),
                         #tabPanel("Table", tableOutput("table"))
                     )#tabsetPanel
                 )#sidebarmenu
        )#introBox
    ),#FIN_dashboardSidebar 3 - BODY
    title = "COVID CyL",                        
    skin = "black"
)





server <- function(input, output,session) {
    
    # 0 - INTRO -------------------------------------------------------------------
    # Mostrar cuadro de dialogo en el navegador.
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            footer = tagList(
                actionButton(inputId = "intro", label = "TOUR POR COVID CyL", icon = icon("info-circle"))
            )
        ))
    })
    
    # Eliminar cuadro de dialogo en el navegador.
    observeEvent(input$intro,{
        removeModal()
    })
    # Opcion dentro del cuadro de dialogo, Tour por el software. Botones del cuadro dialogo - tour. Las frases provienen del excel cargado como intro.
    observeEvent(input$intro,
                 introjs(session, 
                         options = list("nextLabel" = "Continuar",
                                        "prevLabel" = "Volver atrás",
                                        "doneLabel" = "¡Allá vamos!")
                 )
    )
    ###
    # Seleccionar la pestaña
    update_all <- function(x) {
        updateSelectInput(session, "tab",
                          choices = c("", "tab_1", "tab_2", "tab_3", "tab_4", "tab_5", "tab_6"),
                          label = "",
                          selected = x
        )
    }
    
    observeEvent(input$tab_1_confirm, {
        update_all("Situación")
    })
    observeEvent(input$tab_2_confirm, {
        update_all("Pruebas")
    })
    observeEvent(input$tab_3_confirm, {
        update_all("Hospitalizados")
    })
    observeEvent(input$tab_4_confirm, {
        update_all("Fallecidos")
    })
    observeEvent(input$tab_5_confirm, {
        update_all("Modelo Predictivo")
    })
    observeEvent(input$tab_6_confirm, {
        update_all("Mapa de Calor")
    })
    
    # confirmar la selección con el botón confirmar seleccion
    
    observeEvent(input$confirm, {
        updateButton(
            session, 
            inputId = "confirm", 
            label = "CONFIRMAR SELECCIÓN", 
            icon = icon("bar-chart-o"), 
            style = "primary")
    })
    
    # Ocultar el panel de seleccion de entradas para mejor visualización
    observeEvent("", {
        hide("tab")
    })
    
    
    ## REGLAS DINÁMICAS ---------------------------------------
    
    observeEvent("", {
        show("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_3_panel")
        hide("tab_4_panel")
        hide("tab_5_panel")
        hide("tab_6_panel")
    }, once = TRUE)
    
    observeEvent(input$tab_1_confirm, {
        show("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_3_panel")
        hide("tab_4_panel")
        hide("tab_5_panel")
        hide("tab_6_panel")
    })
    observeEvent(input$tab_2_confirm, {
        show("tab_2_panel")
        hide("tab_1_panel")
        hide("tab_3_panel")
        hide("tab_4_panel")
        hide("tab_5_panel")
        hide("tab_6_panel")
    })
    observeEvent(input$tab_3_confirm, {
        show("tab_3_panel")
        hide("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_4_panel")
        hide("tab_5_panel")
        hide("tab_6_panel")
    })
    observeEvent(input$tab_4_confirm, {
        show("tab_4_panel")
        hide("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_3_panel")
        hide("tab_5_panel")
        hide("tab_6_panel")
    })
    observeEvent(input$tab_5_confirm, {
        show("tab_5_panel")
        hide("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_3_panel")
        hide("tab_4_panel")
        hide("tab_6_panel")
    })
    observeEvent(input$tab_6_confirm, {
        show("tab_6_panel")
        hide("tab_1_panel")
        hide("tab_2_panel")
        hide("tab_3_panel")
        hide("tab_4_panel")
        hide("tab_5_panel")
    })
    
    ## Mostrar el botón activo con color
    observeEvent(input$tab, {
        x <- input$tab
        updateButton(session, "tab_1_confirm", style = {
            if (x == "Situación") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "tab_2_confirm", style = {
            if (x == "Pruebas") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "tab_3_confirm", style = {
            if (x == "Hospitalizados") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "tab_4_confirm", style = {
            if (x == "Fallecidos") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "tab_5_confirm", style = {
            if (x == "Modelo Predictivo") {
                paste("warning")
            } else {
                paste("success")
            }
        })
        updateButton(session, "tab_6_confirm", style = {
            if (x == "Mapa de Calor") {
                paste("warning")
            } else {
                paste("success")
            }
        })
    })
    
    ### 3
    
    #    x    <- faithful$waiting
    #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #    hist(x, breaks = bins, col = "#75AADB", border = "white",
    #         xlab = "Waiting time to next eruption (in mins)",
    #         main = "Histogram of waiting times")
    
    # 1 - FILTRADO DE ENTRADAS ----------------------------------------------------
    
    situacion_set<- reactive({
        #input$confirm 
        isolate({
            situacion_set <- situacion %>%
                filter(
                    provincia %in% "Valladolid"#input$prov_Input#, 
                    #fecha %in% c(min(input$fecha_Input):max(input$fecha_Input))
                )
        })
    })
    
    
    fallecidos_set<- reactive({
        #input$confirm 
        isolate({
            fallecidos_set <- fallecidos_OPr2 %>%
                filter(
                    provincia %in% input$prov_Input, 
                )
        })
    })
    
    
    
    # UI TAB 1 -------------------------------------------------------------- 
    
    fallecidos_tabl <- reactive({
        fallecidos_set <- fallecidos_OPr2 %>%
            filter(
                provincia %in% input$prov_Input
            )
        group_by(provincia)%>%
            summarise(mean(fallecidos_OPr2$hombres),  
                      mean(fallecidos_OPr2$mujeres),
                      median(fallecidos_OPr2$hombres),  
                      median(fallecidos_OPr2$mujeres))
        datatable(
            fallecidos_set, 
            rownames = FALSE, 
            extensions = "Buttons",
            options = list(
                dom = 'Bfrtp',
                buttons = c('csv', 'excel', 'pdf'),
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    })
    output$fallecidos_tabl <- DT::renderDataTable({
        fallecidos_tabl
    }, server = FALSE)
    
    
    
    #fallecidos_set=subset(fallecidos, date>min(unique(as.Date(fallecidos$fecha))))
    
    ## BOX TAB 1 -------------------------------------------------------------------
    ## UI - SITUACIÓN - 1 ----------------------------------------------------------
    #output$box_pat <- renderUI({
    #	div(
    #	  style = "position: relative; backgroundColor: #ecf0f5",
    #	  tabBox(
    #		id = "box_pat",
    #		width = NULL,
    #		height = 400,
    #		tabPanel(
    #			title = "Contagios positivos",
    #				div(
    #					style = "position: absolute; left: 0.5em; bottom: 0.5em;",
    #					introBox(data.step = 5, data.intro = intro$text[5],
    #							 dropdown(
    #							   radioGroupButtons(
    #								 inputId = "box_pat1",
    #								 label = NULL, 
    #								 choices = c("Show all", "Show top 10 only"), 
    #								 selected = "Show all", 
    #								 direction = "vertical"
    #							   ),
    #							   size = "xs",
    #							   icon = icon("gear", class = "opt"), 
    #							   up = TRUE
    #							 )
    #					)
    #				),
    #			div(
    #				style = "position: absolute; left: 4em; bottom: 0.5em;",
    #				dropdown(
    #				  downloadButton(outputId = "down_tab1_1", label = "Descarga"),
    #				  size = "xs",
    #				  icon = icon("download", class = "opt"), 
    #				  up = TRUE
    #				)
    #			),
    #		  withSpinner(
    #			plotlyOutput("grafico_tab1_1", height = 300),
    #			type = 4,
    #			color = "#d33724", 
    #			size = 0.7 
    #			),
    #		    div(
    #				style = "position: absolute; right: 0.5em; bottom: 0.5em;",
    #				conditionalPanel(
    #					"input.box_pat2 == 'Hospitalizados'",
    #					actionBttn(
    #						inputId = "los_km", ######
    #						icon = icon("search-plus", class = "opt"),
    #						style = "fill",
    #						color = "danger",
    #						size = "xs"
    #					)
    #				)
    #			)
    #		)
    #	  )
    #	)
    #})
    #
    #   	 
    ### BOX - SITUACIÓN - 1 --------------------------------------------------------
    #grafico_tab1_1 <- reactive({
    #    
    #	pat_select <- situacion
    ##   pat_select <- set_reac_1() %>%
    ##     group_by(sub_specialty) %>%
    ##     summarise(n = n()) %>% 
    ##     arrange(desc(n))
    #    
    #    if (input$box_pat1 == "Show top 10 only") {
    #      pat_select <- pat_select[1:10,]
    #    } else{
    #      pat_select
    #    }
    #    
    #    plot <- 
    #	ggplot(data = pat_select, aes(x = as.Date(fecha), y = nuevos_positivos))+
    #		labs(x = "Fecha", y = "Número de contagios diario") +
    #		theme(
    #			panel.grid.major = element_line(color = "lightgrey", size = 0.2),
    #			panel.grid.major.y = element_blank(),
    #			panel.background = element_rect(fill = "white"),
    #			axis.text.y = element_text(size = 6),
    #			axis.text.x = element_text(size = 6),
    #			axis.title.x = element_text(size = 10, margin = margin(t = 10)),
    #			axis.title.y = element_text(size = 10, margin = margin(t = 10)),
    #			axis.ticks.y = element_blank(),
    #			plot.title = element_text(size = 10, hjust = 0),
    #			panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)) +		
    #		geom_area(stat = "identity", color = "#ba283b", fill="#d1351b")
    #		#facet_grid(rows=vars(as.factor(provincia)))
    #    style(
    #      hide_legend(
    #        ggplotly(tooltip = c("text", "Count"))), 
    #      hoverlabel = list(bgcolor = "white")
    #    )
    #})
    #  
    #output$grafico_tab1_1 <- renderPlotly({
    #    grafico_tab1_1()
    #})
    # 
    ## UI - SITUACIÓN - 1 ---------------------------------------------------------
    output$box_pat1 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box_pat1",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Contagios Positivos",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        introBox(data.step = 5, data.intro = intro$text[5],
                                 dropdown(
                                     radioGroupButtons(
                                         inputId = "box_pat1.1",
                                         label = "Selecciona una opción", 
                                         choiceNames = c("Nuevos Positivos", "Casos Confirmados"),
                                         choiceValues = c("all", "confirmados"), 
                                         selected = "all", 
                                         direction = "vertical"
                                     ),
                                     size = "xs",
                                     icon = icon("gear", class = "opt"), 
                                     up = TRUE
                                 )
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab1_1", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab1_1", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box_pat1 == 'Contagios Positivos'",
                            actionBttn(
                                inputId = "box_pat1.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Contagios Positivos - tabla",
                    htmlOutput("total_tab1_1"), #patients_total
                    withSpinner(
                        DT::dataTableOutput("tabla_tab1_1"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab1_1 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box_pat1.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab1_1() + 
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = c(0.9, 0.75))
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    
    ## BOX - SITUACIÓN - 1 --------------------------------------------------------
    
    situacion_P <- situacion %>%
        group_by(as.Date(anioMes),provincia)%>%
        summarise_at(vars(casos_confirmados=casos_confirmados,nuevos_positivos=nuevos_positivos),mean)%>%
        rename("anioMes" = "as.Date(anioMes)")
    
    # DEFINIR DATASET -----------------------------------------
    situacion_In2 <- reactive({
        input$confirm
        isolate({
            situacion_In2 <- situacion_P %>%
                filter(
                    provincia %in% input$prov_Input,
                    anioMes %in% c(min(input$fecha_Input):max(input$fecha_Input))
                )		
        })
    })
    situacion_In <- reactive({
        input$confirm
        isolate({
            situacion_In <- situacion %>%
                filter(
                    provincia %in% input$prov_Input,
                    fecha %in% c(min(input$fecha_Input):max(input$fecha_Input))
                )		
        })
    })
    
    
    grafico_tab1_1 <- reactive({
        if (input$box_pat1.1 == "confirmados") {
            ggplot(data = situacion_In2(), aes(x = as.Date(anioMes), y = casos_confirmados))+
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Número de casos confirmados"   # título del eje y  
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 8),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "right") +
                
                geom_point(aes(colour = provincia), size = 2.5) +				
                scale_colour_manual(values=colores9RdGy)		
            #geom_area(stat = "identity", color = "#ba283b", fill="#d1351b")
        }
        else {
            ggplot(data = situacion_In(), aes(x = as.Date(fecha), y = nuevos_positivos))+
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Número de nuevos positivos diario"   # título del eje y  
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 8),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = c(0.9, 0.75)) +		
                geom_area(stat = "identity", color = "#ba283b", fill="#d1351b")
        }
    })
    
    output$grafico_tab1_1 <- renderPlot({
        grafico_tab1_1()
    })
    
    
    tabla_tab1_1 <- reactive(
        DT::datatable(
            ucis_In_U() %>% 
                count(anioMes,provincia,hospital,nuevos_hospitalizados_planta, nuevos_hospitalizados_uci) %>% 
                arrange(desc(nuevos_hospitalizados_uci)) %>% 
                rename("Fecha" = anioMes) %>%
                rename("Hospital" = hospital) %>%
                rename("Hosp. Planta" = nuevos_hospitalizados_planta) %>%
                rename("Hosp. UCI" = nuevos_hospitalizados_uci) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab1_1 <- DT::renderDataTable({
        tabla_tab1_1()
    })
    
    
    ## UI - SITUACIÓN - 2 ---------------------------------------------------------
    output$box_pat2 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box_pat2",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Hospitalizados",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box_pat2.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("Planta", "UCI"),
                                choiceValues = c("all", "uci"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab1_2", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab1_2", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box_pat2 == 'Hospitalizados'",
                            actionBttn(
                                inputId = "box_pat2.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Hospitalizados - tabla",
                    htmlOutput("total_tab1_2"), #patients_total
                    withSpinner(
                        DT::dataTableOutput("tabla_tab1_2"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab1_2 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box_pat2.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab1_2() + 
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = c(0.9, 0.75))
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    
    ## BOX - SITUACIÓN - 2 --------------------------------------------------------
    
    ucis_P_U <- ucis %>%
        group_by(as.Date(anioMes),provincia,hospital)%>%
        summarise_at(vars(nuevos_hospitalizados_uci=nuevos_hospitalizados_uci,nuevos_hospitalizados_planta=nuevos_hospitalizados_planta),sum)%>%
        rename("anioMes" = "as.Date(anioMes)")
    
    # DEFINIR DATASET -----------------------------------------
    ucis_In_U <- reactive({
        input$confirm
        isolate({
            ucis_In_U <- ucis_P_U %>%
                filter(
                    provincia %in% input$prov_Input,
                    anioMes %in% c(min(input$fecha_Input):max(input$fecha_Input)),
                    hospital %in% input$ch_Input
                )		
        })
    })
    ucis_In_P <- reactive({
        input$confirm
        isolate({
            ucis_In_P <- ucis %>%
                filter(
                    provincia %in% input$prov_Input,
                    fecha %in% c(min(input$fecha_Input):max(input$fecha_Input)),
                    hospital %in% input$ch_Input
                )		
        })
    })
    
    
    medLey4Hosp <- max(ucis$nuevos_hospitalizados_uci, na.rm=TRUE)/2  
    grafico_tab1_2 <- reactive({
        if (input$box_pat2.1 == "uci") {
            ggplot(ucis_In_U(), aes(anioMes, nuevos_hospitalizados_uci)) + 
                geom_path(colour = "gray", lty = 3) +
                geom_point(aes(colour = provincia), size = 2.5) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Hospitalizados en UCI",   # título del eje y
                    color = "Provincia"   # título de la leyenda     
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 8),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +
                scale_colour_manual(values=colores9RdGy)				
            
        }
        else {
            ggplot(ucis_In_P(), aes(fecha, nuevos_hospitalizados_planta)) + 
                geom_path(colour = "gray", lty = 3) +
                geom_point(aes(colour = nuevos_hospitalizados_uci), size = 1.4) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Hospitalizados en planta",   # título del eje y
                    color = "Hospitalizados en UCI"   # título de la leyenda        
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 8),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +	
                scale_colour_gradient2(high="#ba283b",
                                       mid="black",  
                                       low="gray",
                                       midpoint=medLey4Hosp)
        }
    })
    
    output$grafico_tab1_2 <- renderPlot({
        grafico_tab1_2()
    })
    
    
    tabla_tab1_2 <- reactive(
        DT::datatable(
            ucis_In_U() %>% 
                count(anioMes,provincia,hospital,nuevos_hospitalizados_planta, nuevos_hospitalizados_uci) %>% 
                arrange(desc(nuevos_hospitalizados_uci)) %>% 
                rename("Fecha" = anioMes) %>%
                rename("Hospital" = hospital) %>%
                rename("Hosp. Planta" = nuevos_hospitalizados_planta) %>%
                rename("Hosp. UCI" = nuevos_hospitalizados_uci) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab1_2 <- DT::renderDataTable({
        tabla_tab1_2()
    })
    
    ## UI - SITUACIÓN - 3 ---------------------------------------------------------
    output$box_pat3 <- renderUI({
        div(
            style = "position: relative",
            tabBox( 
                id = "box_pat3",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Fallecidos",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box_pat3.1",
                                label = "Selecciona una opción",
                                #choiceNames = c("All", "Gender"),
                                choiceValues = c("all", "gender"), 				
                                choiceNames = c("Mapa", "Grafico"),
                                #choiceValues = c("mapa", "grafico"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab1_3", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab1_3", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box_pat3 == 'Fallecidos'",
                            actionBttn(
                                inputId = "box_pat3.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Fallecidos - tabla",
                    htmlOutput("total_tab1_3"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab1_3"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab1_3 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box_pat3.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab1_3() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = "bottom")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - SITUACIÓN - 3 --------------------------------------------------------
    
    # DEFINIR DATASET -----------------------------------------
    tasa_mortalidad_In <- reactive({
        input$confirm
        isolate({
            tasa_mortalidad_In <- tasa_mortalidad %>%
                filter(
                    provincia %in% input$prov_Input,
                    fecha %in% c(min(input$fecha_Input):max(input$fecha_Input)),
                    centro %in% input$cs_Input
                )		
        })
    })
    
    zbs_plot<-tasa_mortalidad %>%
        group_by(cs)%>%
        summarise_at(vars(fallecidos=fallecidos),sum)
    nombre<-"Número de personas fallecidas"
    
    colnames(zbs_plot)=c("ZBS","N") # Dos columnas ZBS y cantidad
    q=quantile(zbs_plot$N, prob=seq(0, 1, 1/5), na.rm=T)
    q=as.integer(q)
    
    ### LEYENDA
    etiquetas_leyenda= c(paste0(q[1]," - ",q[2]-1), 
                         paste0(q[2]," - ",q[3]-1), 
                         paste0(q[3]," - ",q[4]-1),
                         paste0(q[4]," - ",q[5]-1),						
                         paste0(q[5]," - o más"))
    
    zbs_color = ifelse(zbs_plot$N<q[2],colores5GyRd[1], 
                       ifelse(zbs_plot$N<q[3],colores5GyRd[2],
                              ifelse(zbs_plot$N<q[4],colores5GyRd[3],		
                                     ifelse(zbs_plot$N<q[5],colores5GyRd[4],colores5GyRd[5]))))
    
    cbind(zbs_plot, ifelse(zbs_plot$N<q[2],colores5GyRd[1], 
                           ifelse(zbs_plot$N<q[3],colores5GyRd[2],
                                  ifelse(zbs_plot$N<q[4],colores5GyRd[3],		
                                         ifelse(zbs_plot$N<q[5],colores5GyRd[4],colores5GyRd[5])))))
    
    medLey2Fall <- max(tasa_mortalidad$tasax100, na.rm=TRUE)/2
    
    grafico_tab1_3 <- reactive({
        if (input$box_pat3.1 == "gender") {
            ggplot(data = tasa_mortalidad_In(),
                   aes(x = fecha, y = fallecidos, fill=tasax100)) +
                labs(x = "Fecha", 
                     y = "Fallecidos", 
                     fill="Tasa de fallecidos por ZBS") +
                geom_bar(stat = "identity") +
                #coord_flip()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 8),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey2Fall)
        }
        else {
            # quito margenes
            par(mar=c(0.5,0.5,1,0.5)) #comando para quitar el marco que por defecto pone R. 
            # El mapa de la ZBS
            plot(contorno_zbs, col=zbs_color, border="grey80") #establece los contornos de las zbs en gris, para que se distingan de los contornos de las areas de salud. 
            legend("bottomright", etiquetas_leyenda, bty="n", cex=0.9,
                   col=colores7GyRd,
                   fill=colores7GyRd,ncol=2) # La leyenda: bottomright Donde queremos la leyend?abajo a la derecha. bty=queremos marco? n. ncol: en cuantas columnas queremos distribuirlo? 2
            # El mapa del Area
            plot(contorno_area_salud, add=TRUE, lwd=2, border="white")  #SOLAPA con el anterior para que ponga una capa sobre la otra, poner add=true. Como la segunda capa no tiene fondo, no tapa a la primera. 
            text(etiqueta_Area_salud$x, etiqueta_Area_salud$y, etiqueta_Area_salud$Nombre, font=2) #poner el nombre de las provincias en las coordenadas que se han declarado previamente. 
            # Añado con un punto las ciudades
            points(ciudades, col="black", pch=18, bg="black")
            legend("bottomleft", legend = c("Capital"),col =  c("black"),pch = c(18), bty="n",cex=0.9)
        }
    })
    
    output$grafico_tab1_3 <- renderPlot({
        grafico_tab1_3()
    })
    
    tabla_tab1_3 <- reactive(
        DT::datatable(
            set_reac_1() %>% 
                count(sub_specialty) %>% 
                arrange(desc(n)) %>% 
                rename("Subspecialty" = sub_specialty),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab1_3 <- DT::renderDataTable({
        tabla_tab1_3()
    })
    
    ## UI - SITUACIÓN - 4 ---------------------------------------------------------
    output$box_pat4 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box_pat4",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Vacunaciones",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box_pat4.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("Porcentajes", "Dato numérico"),
                                choiceValues = c("all", "numero"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab1_4", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab1_4", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box_pat4 == 'Vacunaciones'",
                            actionBttn(
                                inputId = "box_pat4.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Vacunaciones - tabla",
                    htmlOutput("total_tab1_4"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab1_4"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab1_4 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box_pat4.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab1_4() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = "bottom")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - SITUACIÓN - 4 --------------------------------------------------------
    vacunas_P_U <- subset(vacunas, provincia!="Castilla y León")
    
    
    # DEFINIR DATASET -----------------------------------------
    vacunas_In <- reactive({
        input$confirm
        isolate({
            vacunas_In <- vacunas_P_U %>%
                filter(
                    provincia %in% input$prov_Input,
                    fecha %in% c(min(input$fecha_Input):max(input$fecha_Input))
                )		
        })
    })
    
    medLey6Vac <- max(vacunas_P_U$porcentaje_residentes_2a_dosis, na.rm=TRUE)/2
    medLey6Vac2 <- max(vacunas_P_U$personas_vacunadas_ciclo_completo, na.rm=TRUE)/2
    grafico_tab1_4 <- reactive({
        if (input$box_pat4.1 == "numero") {
            ggplot(data = subset(vacunas_In(), provincia!="TotalCyL"),
                   aes(x = as.Date(fecha), y = personas_vacunadas_1a_dosis,fill=personas_vacunadas_ciclo_completo))+
                labs(title = "Vacunados por provincia", 
                     x = "Fecha", y = "Personas con la 1ª dosis", fill="Personas con la 2ª dosis") +
                geom_bar(stat = "identity") +
                facet_grid(rows=vars(as.factor(provincia))) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +	
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey6Vac2)
            
        }
        else {
            ggplot(data = subset(vacunas_In(), provincia!="TotalCyL"),
                   aes(x = as.Date(fecha), y = porcentaje_residentes_1a_dosis,fill=porcentaje_residentes_2a_dosis))+
                labs(title = "Porcentaje de vacunaciones por provincia", 
                     x = "Fecha", y = "Porcentaje de residentes con la 1ª dosis", fill="Porcentaje de residentes con la 2ª dosis") +
                geom_bar(stat = "identity") +
                #facet_grid(rows=vars(as.factor(provincia)))+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +	
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey6Vac)
        }
    })
    
    output$grafico_tab1_4 <- renderPlot({
        grafico_tab1_4()
    })
    
    tabla_tab1_4 <- reactive(
        DT::datatable(
            vacunas_In() %>% 
                count(fecha,provincia,personas_vacunadas_1a_dosis,personas_vacunadas_ciclo_completo, porcentaje_residentes_1a_dosis,porcentaje_residentes_2a_dosis ) %>% 
                arrange(desc(fecha)) %>% 
                rename("Fecha" = fecha) %>%
                rename("Personas 1a dosis" = personas_vacunadas_1a_dosis) %>%
                rename("Personas 2a dosis" = personas_vacunadas_ciclo_completo) %>%
                rename("% 1a dosis" = porcentaje_residentes_1a_dosis) %>%
                rename("% 2a dosis" = porcentaje_residentes_2a_dosis) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab1_4 <- DT::renderDataTable({
        tabla_tab1_4()
    })
    
    # BOX TAB 2 -------------------------------------------------------------------
    ## UI - PRUEBAS - 1 ---------------------------------------------------------
    output$box1 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box1",
                width = NULL,
                height = 700,
                tabPanel(
                    title = "PCR positivos sobre PCR realizados",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box1.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("All", "Gender"),
                                choiceValues = c("all", "gender"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab2_1", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab2_1", height = 600),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box1 == 'PCR positivos sobre PCR realizados'",
                            actionBttn(
                                inputId = "box1.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "PCR positivos sobres PCR realizados - tabla",
                    htmlOutput("total_tab2_1"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab2_1"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab2_1 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box1.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab2_1() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.key.width = unit(1.5, "cm"),
                        legend.key.height = unit(1.5, "cm"),
                        legend.position = "bottom")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - PRUEBAS - 1 --------------------------------------------------------
    
    
    tasa_enfermosZBS_P <- tasa_enfermos %>%
        group_by(as.Date(anioMes),tipo_centro,provincia)%>%
        summarise_at(vars(pcr_realizados=pcr_realizados,pcr_positivos=pcr_positivos),sum)%>%
        rename("anioMes" = "as.Date(anioMes)")
    
    # DEFINIR DATASET -----------------------------------------
    tasa_enfermosZBS_P_In <- reactive({
        input$confirm
        isolate({
            tasa_enfermosZBS_P_In <- tasa_enfermosZBS_P %>%
                filter(
                    provincia %in%input$prov_Input,
                    tipo_centro %in% input$ct_Input,
                    anioMes %in% c(min(input$fecha_Input):max(input$fecha_Input))
                )
        })
    })
    
    grafico_tab2_1 <- reactive({
        if (input$box1.1 == "gender") {
            ggplot(tasa_enfermosZBS_P_In(), 
                   aes(x = factor(anioMes), fill = factor(provincia), y = (pcr_positivos*100/pcr_realizados))) +
                geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize = 1.5)	+
                labs(x = "Fecha",              # título del eje x
                     y = "Porcentaje de PCR positivas sobre el total de realizadas",   # título del eje y
                     fill = "Provincia"   # título de la leyenda        
                ) +	
                facet_grid(rows=vars(as.factor(tipo_centro)))  +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +
                scale_fill_manual(values=colores9GyRd)
        }
        else {
            ggplot(tasa_enfermosZBS_P_In(), 
                   aes(x = factor(anioMes), fill = factor(provincia), y = (pcr_positivos*100/pcr_realizados))) +
                geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge",dotsize = 1.5)	+
                labs(x = "Fecha",              # título del eje x
                     y = "Porcentaje de PCR positivas sobre el total de realizadas",   # título del eje y
                     fill = "Provincia"   # título de la leyenda        
                ) +	
                facet_grid(rows=vars(as.factor(tipo_centro)))  +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +
                scale_fill_manual(values=colores9RdGy)
        }
    })
    
    output$grafico_tab2_1 <- renderPlot({
        grafico_tab2_1()
    })
    
    tabla_tab2_1 <- reactive(
        DT::datatable(
            tasa_enfermosZBS_P_In() %>% 
                count(anioMes,tipo_centro,provincia,pcr_realizados,pcr_positivos) %>% 
                arrange(desc(pcr_realizados)) %>% 
                rename("Fecha" = anioMes) %>%	
                rename("Tipo de Centro" = tipo_centro) %>%
                rename("PCR Realizados" = pcr_realizados) %>%
                rename("PCR Positivos" = pcr_positivos) %>%
                rename("Provincia" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    
    output$tabla_tab2_1 <- DT::renderDataTable({
        tabla_tab2_1()
    })
    ## UI - PRUEBAS - 2 ---------------------------------------------------------
    output$box2 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box2",
                width = NULL,
                height = 2100,
                tabPanel(
                    title = "Número de PCR realizados y enfermos por COVID-19",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box2.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("All", "Gender"),
                                choiceValues = c("all", "gender"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab2_2", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab2_2", height = 2000),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box2 == 'Número de PCR realizados y enfermos por COVID-19'",
                            actionBttn(
                                inputId = "box2.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Número de PCR realizados y enfermos por COVID-19 - tabla",
                    htmlOutput("total_tab2_2"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab2_2"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab2_2 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box2.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab2_2() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = c(0.9, 0.75))
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - PRUEBAS - 2 --------------------------------------------------------
    
    tasa_enfermosZBS_P2 <- tasa_enfermos %>%
        group_by(provincia,centro,tipo_centro)%>%
        summarise(totalenfermedad=sum(totalenfermedad),
                  pcr_realizados=sum(pcr_realizados))
    
    
    # DEFINIR DATASET -----------------------------------------
    tasa_enfermosZBS_P2_In <- reactive({
        input$confirm
        isolate({
            tasa_enfermosZBS_P2_In <- tasa_enfermosZBS_P2 %>%
                filter(
                    provincia %in%input$prov_Input,
                    centro %in%input$cs_Input,
                    tipo_centro %in% input$ct_Input
                )
        })
    })
    
    medLey1PCR <- max(tasa_enfermosZBS_P2$totalenfermedad, na.rm=TRUE)/2
    grafico_tab2_2 <- reactive({
        if (input$box2.1 == "gender") {
            ggplot(data = tasa_enfermosZBS_P2_In(),
                   aes(x = reorder(centro, desc(centro)), y = pcr_realizados, fill=totalenfermedad))+
                labs(x = "Zonas básicas", 
                     y = "PCR realizadas", 
                     fill="Nº de enfermos por COVID-19") +
                geom_bar(stat = "identity") +
                coord_flip()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey1PCR)
        }
        else {
            ggplot(data = tasa_enfermosZBS_P2_In(),
                   aes(x = reorder(centro, desc(centro)), y = pcr_realizados, fill=totalenfermedad))+
                labs(x = "Zonas básicas", 
                     y = "PCR realizadas", 
                     fill="Nº de enfermos por COVID-19") +
                geom_bar(stat = "identity") +
                coord_flip()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "bottom") +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey1PCR)
        }
    })
    
    output$grafico_tab2_2 <- renderPlot({
        grafico_tab2_2()
    })
    
    tabla_tab2_2 <- reactive(
        DT::datatable(
            set_reac_1() %>% 
                count(sub_specialty) %>% 
                arrange(desc(n)) %>% 
                rename("Subspecialty" = sub_specialty),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab2_2 <- DT::renderDataTable({
        tabla_tab2_2()
    })
    
    # BOX TAB 3 -------------------------------------------------------------------
    ## UI - HOSPITALIZADOS - 1 ---------------------------------------------------------
    output$box5 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box5",
                width = NULL,
                height = 700,
                tabPanel(
                    title = "Fallecimientos en Hospitales",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box5.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("All", "Gender"),
                                choiceValues = c("all", "gender"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab3_1", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab3_1", height = 600),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box5 == 'Fallecimientos en Hospitales'",
                            actionBttn(
                                inputId = "box5.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Fallecimientos en Hospitales - tabla",
                    htmlOutput("total_tab3_1"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab3_1"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab3_1 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box5.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab3_1() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.key.width = unit(1.5, "cm"),
                        legend.key.height = unit(1.5, "cm"),
                        legend.position = "bottom")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - HOSPITALIZADOS - 1 --------------------------------------------------------
    ucis_H_U <- ucis %>%
        group_by(as.Date(anioMes),provincia,hospital)%>%
        summarise_at(vars(nuevos_fallecimientos=nuevos_fallecimientos,nuevas_altas=nuevas_altas),sum)%>%
        rename("anioMes" = "as.Date(anioMes)")
    
    # DEFINIR DATASET -----------------------------------------
    ucis_In_UH <- reactive({
        input$confirm
        isolate({
            ucis_In_UH <- ucis_H_U %>%
                filter(
                    provincia %in% input$prov_Input,
                    anioMes %in% c(min(input$fecha_Input):max(input$fecha_Input)),
                    hospital %in% input$ch_Input
                )		
        })
    })
    
    grafico_tab3_1 <- reactive({
        if (input$box5.1 == "gender") {
            ggplot(data = ucis_In_UH(), aes(x = anioMes, y = nuevos_fallecimientos, fill=provincia)) +
                geom_bar(stat="identity")+#,width = 0.4, position=position_dodge(width = 0.5))  + 
                facet_wrap( ~ hospital) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Fallecimientos",   # título del eje y
                    fill = "Provincia"   # título de la leyenda
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +	
                scale_fill_manual(values=colores9RdGy)
        }
        else {
            ggplot(data = ucis_In_UH(), aes(x = anioMes, y = nuevos_fallecimientos, fill=provincia)) +
                geom_bar(stat="identity")+#,width = 0.4, position=position_dodge(width = 0.5))  + 
                facet_wrap( ~ hospital) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Fallecimientos",   # título del eje y
                    fill = "Provincia"   # título de la leyenda
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +	
                scale_fill_manual(values=colores9RdGy)
        }
    })
    
    output$grafico_tab3_1 <- renderPlot({
        grafico_tab3_1()
    })
    
    tabla_tab3_1 <- reactive(
        DT::datatable(
            ucis_In_UH() %>% 
                count(anioMes,provincia,hospital,nuevos_fallecimientos) %>% 
                arrange(desc(anioMes)) %>% 
                rename("Fecha" = anioMes) %>%	
                rename("Hospital" = hospital) %>%
                rename("Nuevos Fallecidos" = nuevos_fallecimientos) %>%
                rename("Provincia" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    
    output$tabla_tab3_1 <- DT::renderDataTable({
        tabla_tab3_1()
    })
    
    
    ## UI - HOSPITALIZADOS - 2 ---------------------------------------------------------
    output$box6 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box6",
                width = NULL,
                height = 700,
                tabPanel(
                    title = "Altas en Hospitales",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box6.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("All", "Gender"),
                                choiceValues = c("all", "gender"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab3_2", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab3_2", height = 600),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box6 == 'Altas en Hospitales'",
                            actionBttn(
                                inputId = "box6.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Altas en Hospitales - tabla",
                    htmlOutput("total_tab3_2"),
                    withSpinner(
                        DT::dataTableOutput("tabla_tab3_2"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab3_2 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box6.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab3_2() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.key.width = unit(1.5, "cm"),
                        legend.key.height = unit(1.5, "cm"),
                        legend.position = "bottom")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    })
    
    ## BOX - HOSPITALIZADOS - 1 --------------------------------------------------------
    #ucis_H_U <- ucis %>%
    #	group_by(as.Date(anioMes),provincia,hospital)%>%
    #	summarise_at(vars(nuevos_fallecimientos=nuevos_fallecimientos,nuevas_altas=nuevas_altas),sum)%>%
    #	rename("anioMes" = "as.Date(anioMes)")
    #
    ## DEFINIR DATASET -----------------------------------------
    #ucis_In_UH <- reactive({
    #    input$confirm
    #    isolate({
    #		ucis_In_UH <- ucis_H_U %>%
    #			filter(
    #				provincia %in% input$prov_Input,
    #				anioMes %in% c(min(input$fecha_Input):max(input$fecha_Input)),
    #				hospital %in% input$ch_Input
    #			)		
    #    })
    #})
    
    grafico_tab3_2 <- reactive({
        if (input$box6.1 == "gender") {
            ggplot(data = ucis_In_UH(), aes(x = anioMes, y = nuevas_altas, fill=provincia)) +
                geom_bar(stat="identity")+#,width = 0.4, position=position_dodge(width = 0.5))  + 
                facet_wrap( ~ hospital) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Altas",   # título del eje y
                    fill = "Provincia"   # título de la leyenda
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +	
                scale_fill_manual(values=colores9RdGy)
        }
        else {
            ggplot(data = ucis_In_UH(), aes(x = anioMes, y = nuevas_altas, fill=provincia)) +
                geom_bar(stat="identity")+#,width = 0.4, position=position_dodge(width = 0.5))  + 
                facet_wrap( ~ hospital) +
                labs(
                    x = "Fecha",              # título del eje x
                    y = "Altas",   # título del eje y
                    fill = "Provincia"   # título de la leyenda
                ) +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.key.width = unit(1.2, "cm"),
                    legend.key.height = unit(1.2, "cm"),
                    legend.position = "bottom") +	
                scale_fill_manual(values=colores9RdGy)
        }
    })
    
    output$grafico_tab3_2 <- renderPlot({
        grafico_tab3_2()
    })
    
    tabla_tab3_2 <- reactive(
        DT::datatable(
            ucis_In_UH() %>% 
                count(anioMes,provincia,hospital,nuevas_altas) %>% 
                arrange(desc(anioMes)) %>% 
                rename("Fecha" = anioMes) %>%	
                rename("Hospital" = hospital) %>%
                rename("Nuevas Altas" = nuevas_altas) %>%
                rename("Provincia" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    
    output$tabla_tab3_2 <- DT::renderDataTable({
        tabla_tab3_2()
    })
    
    ## UI - FALLECIDOS - 1 ---------------------------------------------------------
    output$box9 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box9",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Fallecidos por provincia",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box9.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("COVID-19", "Total"),
                                choiceValues = c("all", "total"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab4_1", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab4_1", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box9 == 'Fallecidos por provincia'",
                            actionBttn(
                                inputId = "box9.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Fallecidos por provincia - tabla",
                    htmlOutput("total_tab4_1"), #patients_total
                    withSpinner(
                        DT::dataTableOutput("tabla_tab4_1"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab4_1 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box9.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab4_1() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = "right")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    }) 
    
    ## BOX - FALLECIDOS - 1 --------------------------------------------------------
    
    tasa_mortalidadZBS_P <- tasa_mortalidad %>%
        group_by(provincia)%>%
        summarise(fallecidos=sum(fallecidos),
                  tasax100=mean(tasax100))
    
    tasa_mortalidadZBS_TP <- tasa_mortalidad_T %>%
        group_by(provincia)%>%
        summarise(fallecidos=sum(fallecidos),
                  tasax100=mean(tasax100))
    
    # DEFINIR DATASET -----------------------------------------
    tasa_mortalidadZBS_P_In <- reactive({
        input$confirm
        isolate({
            tasa_mortalidadZBS_P_In <- tasa_mortalidadZBS_P %>%
                filter(provincia %in%input$prov_Input)
        })
    })
    tasa_mortalidadZBS_TP_In <- reactive({
        input$confirm
        isolate({
            tasa_mortalidadZBS_TP_In <- tasa_mortalidadZBS_TP %>%
                filter(provincia %in%input$prov_Input)	
        })
    })
    
    medLey2FallTP <- ((max(tasa_mortalidadZBS_TP$tasax100, na.rm=TRUE)-min(tasa_mortalidadZBS_TP$tasax100, na.rm=TRUE))/2) +min(tasa_mortalidadZBS_TP$tasax100, na.rm=TRUE)
    medLey2FallP <- ((max(tasa_mortalidadZBS_P$tasax100, na.rm=TRUE)-min(tasa_mortalidadZBS_P$tasax100, na.rm=TRUE))/2) +min(tasa_mortalidadZBS_P$tasax100, na.rm=TRUE)
    
    grafico_tab4_1 <- reactive({
        if (input$box9.1 == "total") {
            ggplot(data = tasa_mortalidadZBS_TP_In(),
                   aes(x = reorder(provincia, desc(provincia)), y = fallecidos, fill=(tasax100)))+
                labs(x = "Provincias", 
                     y = "Numero de fallecidos - totales", 
                     fill="Tasa de fallecidos") +
                geom_bar(stat = "identity")+
                coord_polar()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x.bottom = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y.right = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "right",
                    legend.text=element_text(size = 10, margin = margin(t = 10))) +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey2FallTP)
        }
        else {
            ggplot(data = tasa_mortalidadZBS_P_In(),
                   aes(x = reorder(provincia, desc(provincia)), y = fallecidos, fill=(tasax100)))+
                labs(x = "Provincias", 
                     y = "Numero de fallecidos - COVID-19", 
                     fill="Tasa de fallecidos") +
                geom_bar(stat = "identity")+
                coord_polar()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x.bottom = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y.right = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "right",
                    legend.text=element_text(size = 10, margin = margin(t = 10))) +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey2FallP)
        }
    })
    
    output$grafico_tab4_1 <- renderPlot({
        grafico_tab4_1()
    })
    
    tabla_tab4_1 <- reactive(
        DT::datatable(
            tasa_mortalidadZBS_P_In() %>% 
                count(provincia,fallecidos, round(tasax100,5)) %>% 
                arrange(desc(fallecidos)) %>%
                rename("Tasa de Fallecidos" = "round(tasax100, 5)") %>%
                rename("Fallecidos" = fallecidos) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab4_1 <- DT::renderDataTable({
        tabla_tab4_1()
    })
    
    ## UI - FALLECIDOS - 2 ---------------------------------------------------------
    output$box10 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box10",
                width = NULL,
                height = 2100,
                tabPanel(
                    title = "Fallecidos por provincia",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box10.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("COVID-19", "Total"),
                                choiceValues = c("all", "gender"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab4_2", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab4_2", height = 2000),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box10 == 'Fallecidos por provincia'",
                            actionBttn(
                                inputId = "box10.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Fallecidos por provincia - tabla",
                    htmlOutput("total_tab4_2"), #patients_total
                    withSpinner(
                        DT::dataTableOutput("tabla_tab4_2"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab4_2 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    observeEvent((input$box10.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab4_2() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = c(0.9, 0.75))
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    }) 
    
    
    ## BOX - FALLECIDOS - 2 --------------------------------------------------------
    
    tasa_mortalidadZBS_P2 <- tasa_mortalidad %>%
        group_by(provincia,centro)%>%
        summarise(fallecidos=sum(fallecidos),
                  tasax100=mean(tasax100))
    
    
    # DEFINIR DATASET -----------------------------------------
    tasa_mortalidadZBS_P2_In <- reactive({
        input$confirm
        isolate({
            tasa_mortalidadZBS_P2_In <- tasa_mortalidadZBS_P2 %>%
                filter(
                    provincia %in%input$prov_Input,
                    centro %in%input$cs_Input
                )
        })
    })
    
    medLey2FallZBS <- max(tasa_mortalidadZBS_P2$tasax100, na.rm=TRUE)/2
    grafico_tab4_2 <- reactive({
        if (input$box10.1 == "gender") {
            ggplot(data = tasa_mortalidadZBS_P2_In(),
                   aes(x = reorder(centro, desc(centro)), y = fallecidos, fill=(tasax100)))+
                labs(x = "Zonas básicas", 
                     y = "Fallecidos", 
                     title = "Número de fallecidos",
                     fill="Porcentaje de fallecidos por ZBS") +
                geom_bar(stat = "identity")+
                coord_flip()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x.bottom = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y.right = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = c(0.79, 0.07),
                    legend.text=element_text(size = 10, margin = margin(t = 10))) +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey2FallZBS)
        }
        else {
            ggplot(data = tasa_mortalidadZBS_P2_In(),
                   aes(x = reorder(centro, desc(centro)), y = fallecidos, fill=(tasax100)))+
                labs(x = "Zonas básicas", 
                     y = "Fallecidos", 
                     title = "Número de fallecidos",
                     fill="Porcentaje de fallecidos por ZBS") +
                geom_bar(stat = "identity")+
                coord_flip()+
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title.x.bottom = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y.right = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = c(0.79, 0.07),
                    legend.text=element_text(size = 10, margin = margin(t = 10))) +
                scale_fill_gradient2(high="#ba283b",
                                     mid="black",  
                                     low="gray",
                                     midpoint=medLey2FallZBS)
        }
    })
    
    output$grafico_tab4_2 <- renderPlot({
        grafico_tab4_2()
    })
    
    tabla_tab4_2 <- reactive(
        DT::datatable(
            tasa_mortalidadZBS_P2_In() %>% 
                count(provincia,fallecidos, round(tasax100,5)) %>% 
                arrange(desc(fallecidos)) %>%
                rename("Tasa de Fallecidos" = "round(tasax100, 5)") %>%
                rename("Fallecidos" = fallecidos) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab4_2 <- DT::renderDataTable({
        tabla_tab4_2()
    })
    
    
    
    ## UI - FALLECIDOS - 3 ---------------------------------------------------------
    output$box11 <- renderUI({
        div(
            style = "position: relative",
            tabBox(
                id = "box11",
                width = NULL,
                height = 400,
                tabPanel(
                    title = "Fallecidos por provincia y grupos de edad",
                    div(
                        style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                        dropdown(
                            radioGroupButtons(
                                inputId = "box11.1",
                                label = "Selecciona una opción", 
                                choiceNames = c("Mujeres", "Hombres"),
                                choiceValues = c("all", "hombres"), 
                                selected = "all", 
                                direction = "vertical"
                            ),
                            size = "xs",
                            icon = icon("gear", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    div(
                        style = "position: absolute; left: 4em; bottom: 0.5em;",
                        dropdown(
                            downloadButton(outputId = "down_tab4_3", label = "Descarga"),
                            size = "xs",
                            icon = icon("download", class = "opt"), 
                            up = TRUE
                        )
                    ),
                    withSpinner(
                        plotOutput("grafico_tab4_3", height = 300),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    ),
                    div(
                        style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                        conditionalPanel(
                            "input.box11 == 'Fallecidos por provincia y grupos de edad'",
                            actionBttn(
                                inputId = "box11.9", ######
                                icon = icon("search-plus", class = "opt"),
                                style = "fill",
                                color = "danger",
                                size = "xs"
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Fallecidos por provincia y grupos de edad - tabla",
                    htmlOutput("total_tab4_3"), #patients_total
                    withSpinner(
                        DT::dataTableOutput("tabla_tab4_3"),
                        type = 4,
                        color = "#d33724",
                        size = 0.7
                    )
                )
            )
        )
    })
    
    output$total_tab4_3 <- renderText({
        HTML(
            paste("Total number of admissions:", 
                  strong(
                      unique(
                          paste(set_reac_1()$id, set_reac_1()$adm_id) %>% 
                              length()
                      )
                  )
            )
        )
    })
    
    observeEvent((input$box11.9), {
        showModal(modalDialog(
            renderPlot({
                grafico_tab4_3() + 		
                    theme(
                        panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                        panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                        panel.background = element_rect(fill = "white"),
                        axis.text.y = element_text(size = 8),
                        axis.text.x = element_text(size = 8),
                        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                        axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                        axis.ticks.y = element_blank(),
                        plot.title = element_text(size = 10, hjust = 0),
                        panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                        legend.position = "right")
            }, height = 600),
            easyClose = TRUE,
            size = "l",
            footer = NULL
        ))
    }) 
    
    ## BOX - FALLECIDOS - 3 --------------------------------------------------------
    
    fallecidos_OPr <- subset(fallecidos, siglas!="CyL") #Original - Solo provincias
    fallecidos_OPr2 <- fallecidos_OPr %>%	#Original - Solo provincias agrupado
        group_by(fecha,provincia,grupo_edad)%>%
        summarise(hombres=hombres, mujeres=mujeres)
    fallecidos$diferencia<-fallecidos$hombres-fallecidos$mujeres
    fallecidos_hoy = subset(fallecidos, fecha==(today()-1))
    fallecidos_hoy = subset(fallecidos_hoy, siglas!="CyL")
    fallecidos_hoy_S <- fallecidos_hoy %>%
        group_by(provincia) #%>%
    
    
    
    # DEFINIR DATASET -----------------------------------------
    fallecidos_OPr_In <- reactive({
        input$confirm
        isolate({
            fallecidos_OPr <- fallecidos %>%
                mutate(HM=hombres+mujeres) %>%
                filter(
                    provincia %in%input$prov_Input,
                    fecha %in% c(max(input$fecha_Input))
                    ###INPUTgrupoedad
                )
        })
    })
    
    grafico_tab4_3 <- reactive({
        if (input$box11.1 == "hombres") {
            ggplot(data = fallecidos_OPr_In(),
                   aes(x = provincia, y = hombres,fill = grupo_edad))+
                geom_bar(stat = "identity")+
                labs(title = "Fallecidos por provincias y grupos de edades", 
                     x = "Provincias", 
                     y = "Hombres fallecidos", 
                     fill="Grupo de edad") +
                guides(title="Grupo de edad") +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "right") +
                scale_fill_manual(values=colores9GyRd)
        }
        else {
            ggplot(data = fallecidos_OPr_In(),
                   aes(x = provincia, y = mujeres,fill = grupo_edad))+
                geom_bar(stat = "identity")+
                labs(title = "Fallecidos por provincias y grupos de edades", 
                     x = "Provincias", 
                     y = "Mujeres fallecidas", 
                     fill="Grupo de edad") +
                guides(title="Grupo de edad") +
                theme(
                    panel.grid.major = element_line(color = "lightgrey", size = 0.2),
                    panel.grid.major.y = element_line(color = "lightgrey", size = 0.2),
                    panel.background = element_rect(fill = "white"),
                    axis.text.y = element_text(size = 8),
                    axis.text.x = element_text(size = 10),
                    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                    axis.title.y = element_text(size = 12, margin = margin(t = 10)),
                    axis.ticks.y = element_blank(),
                    plot.title = element_text(size = 10, hjust = 0),
                    panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1),
                    legend.position = "right") +
                scale_fill_manual(values=colores9GyRd)	
        }
    })
    
    output$grafico_tab4_3 <- renderPlot({
        grafico_tab4_3()
    })
    
    tabla_tab4_3 <- reactive(
        DT::datatable(
            fallecidos_OPr_In() %>% 
                count(provincia, hombres, mujeres, HM, grupo_edad) %>% 
                arrange(desc(HM)) %>% 
                rename("Hombres" = hombres) %>%
                rename("Mujeres" = mujeres) %>%
                rename("Hombres y Mujeres" = HM) %>%
                rename("Grupo Edad" = grupo_edad) %>%
                rename("Provincias" = provincia),
            rownames = FALSE,
            options = list(
                dom = 'frtp',
                style = "bootstrap",
                lengthMenu = c(seq(5, 150, 5))
            )
        )
    )
    
    output$tabla_tab4_3 <- DT::renderDataTable({
        tabla_tab4_3()
    })
    
    # UI TAB 2 --------------------------------------------------------------------   	
    output$tab2_grafico1 <- renderPlot({
        hist(situacion_O$nuevos_positivos, col="blue",border="darkred")
    })	
    output$tab2_grafico2 <- renderPlot({
        hist(situacion_O$nuevos_positivos, col="purple",border="darkgreen")
    })	
    output$tab2_grafico3 <- renderPlot({
        hist(situacion_O$nuevos_positivos, col="gray",border="darkgreen")
    })	
    output$tab2_grafico4 <- renderPlot({
        hist(situacion_O$nuevos_positivos, col="black",border="darkgreen")
    })
    
    
    
    # DESCARGAS ----------------------------------------------------------------
    output$descargaMortDatos <- downloadHandler(
        filename = function() {
            paste(input$filename, "_mortalidad_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(fallecidos, file, col.names=T, row.names=F, append=F)
        }
    )
    
    output$descargaVacDatos <- downloadHandler(
        filename = function() {
            paste(input$filename, "_vacunaciones_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write.xlsx(vacunas, file, col.names=T, row.names=F, append=F)
        }
    )
    
    download_box <- function(exportname, plot) {
        downloadHandler(
            filename = function() {
                paste(exportname, Sys.Date(), ".png", sep = "")
            },
            content = function(file) {
                ggsave(file, plot = plot, device = "png", width = 8)
            }
        )
    }
    
    output$down_tab1_1 <- download_box("Contagios_", grafico_tab1_1())  
    output$down_tab1_2 <- download_box("Hospitalizados_", grafico_tab1_2())
    output$down_tab1_3 <- download_box("pat_year", grafico_tab1_3())
    output$down_tab1_4 <- download_box("Vacunaciones_", grafico_tab1_4())
    output$down_tab2_1 <- download_box("PCRsPositivos_", grafico_tab2_1())
    output$down_tab2_2 <- download_box("PCRsRealizados_", grafico_tab2_2())
    
    output$down_tab4_1 <- download_box("FallProvinciasy_", grafico_tab4_1())
    output$down_tab4_3 <- download_box("FallProvinciasyEdad_", grafico_tab4_3())
    
    output$down_box_5 <- download_box("diagnostics_year", dia_adm())
    output$down_box_6 <- download_box("diagnostics_timing", plot_dia_timing())
    output$down_box_7 <- download_box("diagnostics_perform", plot_dia_perform())
    output$down_box_micro <- download_box("first_isolates", micro_plot())
    output$down_box_res <- download_box("resistance", isolate_plot())
    output$down_box_res_ts <- download_box("resistance_time", isolate_ts())
    output$down_box_ddd_ts <- download_box("ddd time", ddd_ts())
    output$down_box_dot_ts <- download_box("dot time", dot_ts())
    output$down_box_los1.0 <- download_box("los_groups", plot_los()) 
    output$down_box_los2 <- download_box("km-curve", kaplan_los()$plot)
    
    
    
}



shinyApp(ui = ui, server = server)
