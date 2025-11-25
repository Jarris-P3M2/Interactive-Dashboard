# app.R
# Instalación de paquetes si faltan
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("viridis")) install.packages("viridis")
if (!require("shinyWidgets")) install.packages("shinyWidgets")

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(plotly)
library(purrr)
library(tidyr)
library(viridis)
library(shinyWidgets)

# ==============================================================================
# LÓGICA DE CÁLCULO (SCORING)
# ==============================================================================

# Función para calcular scores
# data_q: Dataframe que contiene SOLO las preguntas numéricas (sin columnas de metadatos)
calculate_score <- function(data_q, items_rel, type = "symptom", range_val = 3, reverse_rel = NULL) {
  
  # Validar que los índices solicitados existan en el dataframe de preguntas
  valid_items <- items_rel[items_rel <= ncol(data_q)]
  
  if (length(valid_items) == 0) return(rep(NA, nrow(data_q)))
  
  # Seleccionar las columnas de preguntas
  selected_data <- data_q[, valid_items, drop = FALSE]
  
  # Aplicar Reverse Scoring (Si aplica)
  # Para rango 3 (1-4), la inversión es: 5 - valor.
  # Para rango 6 (1-7), la inversión es: 8 - valor.
  max_score <- range_val + 1
  sum_reverse <- 1 + max_score
  
  if (!is.null(reverse_rel)) {
    # Filtrar solo los reverse que estén en los items seleccionados
    valid_reverse <- reverse_rel[reverse_rel %in% valid_items]
    
    for (idx in valid_reverse) {
      # Encontrar el nombre de columna correspondiente a este índice relativo
      col_name <- names(data_q)[idx]
      if (col_name %in% names(selected_data)) {
        selected_data[[col_name]] <- sum_reverse - selected_data[[col_name]]
      }
    }
  }
  
  # Calcular Raw Score (RS) = Promedio por fila
  rs <- rowMeans(selected_data, na.rm = TRUE)
  
  # Transformación Lineal a 0-100
  score <- rep(NA, length(rs))
  
  if (type == "functional") {
    # Funcional: 1 - ((RS - 1) / range) * 100
    score <- (1 - ((rs - 1) / range_val)) * 100
  } else {
    # Síntoma / Global: ((RS - 1) / range) * 100
    score <- ((rs - 1) / range_val) * 100
  }
  
  return(round(score, 2))
}

# ==============================================================================
# DICCIONARIO DE CONFIGURACIÓN DE ENCUESTAS
# ==============================================================================
# meta_cols: Cantidad de columnas de datos personales antes de que empiecen las preguntas.
# items: Índices RELATIVOS a la sección de preguntas (La pregunta 1 del excel es el índice 1).

survey_configs <- list(
  "GENERICO" = list(type = "generic", meta_cols = 7),
  
  # 1. EORTC QLQ-C30 (Core)
  "EORTC QLQ-C30" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Salud Global / QoL", items = 29:30, type = "global", range = 6),
      list(name = "Func. Física", items = 1:5, type = "functional"),
      list(name = "Func. de Rol", items = 6:7, type = "functional"),
      list(name = "Func. Emocional", items = 21:24, type = "functional"),
      list(name = "Func. Cognitiva", items = c(20, 25), type = "functional"),
      list(name = "Func. Social", items = 26:27, type = "functional"),
      list(name = "Fatiga", items = c(10, 12, 18), type = "symptom"),
      list(name = "Náuseas/Vómitos", items = 14:15, type = "symptom"),
      list(name = "Dolor", items = c(9, 19), type = "symptom"),
      list(name = "Disnea", items = 8, type = "symptom"),
      list(name = "Insomnio", items = 11, type = "symptom"),
      list(name = "Pérdida Apetito", items = 13, type = "symptom"),
      list(name = "Estreñimiento", items = 16, type = "symptom"),
      list(name = "Diarrea", items = 17, type = "symptom"),
      list(name = "Dificultad Financiera", items = 28, type = "symptom")
    )
  ),
  
  # 2. EORTC QLQ-STO22 (Gástrico)
  # Manual: 31-52. Excel: 1-22.
  "EORTC QLQ-STO22 (Gástrico)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Disfagia", items = 1:3, type = "symptom"),
      list(name = "Dolor", items = 4:7, type = "symptom"),
      list(name = "Reflujo", items = 8:10, type = "symptom"),
      list(name = "Comer", items = c(11:13, 16), type = "symptom"),
      list(name = "Ansiedad", items = c(17, 18, 20), type = "symptom"),
      list(name = "Boca Seca", items = 14, type = "symptom"),
      list(name = "Gusto", items = 15, type = "symptom"),
      list(name = "Imagen Corporal", items = 19, type = "symptom"),
      list(name = "Pérdida Cabello", items = 22, type = "symptom")
    )
  ),
  
  # 3. EORTC QLQ-BR23 (Mama Antigua)
  # Manual: 31-53. Excel: 1-23.
  "EORTC QLQ-BR23 (Mama)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 9:12, type = "functional"),
      list(name = "Func. Sexual", items = 14:15, type = "functional", reverse = 14:15),
      list(name = "Disfrute Sexual", items = 16, type = "functional", reverse = 16),
      list(name = "Perspectiva Futura", items = 13, type = "functional"),
      list(name = "Efectos Sec. Sist.", items = c(1:4, 6:8), type = "symptom"),
      list(name = "Síntomas Mama", items = 20:23, type = "symptom"),
      list(name = "Síntomas Brazo", items = 17:19, type = "symptom"),
      list(name = "Pérdida Cabello", items = 5, type = "symptom")
    )
  ),
  
  # 4. EORTC QLQ-BR42 (Mama Actualizada)
  # Manual: 31-72. Excel: 1-42.
  "EORTC QLQ-BR42 (Mama V.Nueva)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 9:12, type = "functional"),
      list(name = "Func. Sexual", items = 14:15, type = "functional", reverse = 14:15),
      list(name = "Satisfacción Mama", items = 41:42, type = "functional", reverse = 41:42), # Orig 71,72 -> 41,42
      list(name = "Perspectiva Futura", items = 13, type = "functional"),
      list(name = "Disfrute Sexual", items = 16, type = "functional", reverse = 16),
      list(name = "Quimio Efectos", items = c(1:6, 27:28), type = "symptom"), # Orig 31-36, 57-58 -> 1-6, 27-28
      list(name = "Sínt. Endocrinos", items = c(7:8, 24:26), type = "symptom"),
      list(name = "Sínt. Brazo", items = 17:19, type = "symptom"),
      list(name = "Sínt. Mama", items = 20:23, type = "symptom"),
      list(name = "Neuropatía", items = 29:32, type = "symptom"),
      list(name = "Esquelético", items = 33:36, type = "symptom"),
      list(name = "Sínt. Vaginales", items = 38:40, type = "symptom"),
      list(name = "Aumento Peso", items = 37, type = "symptom")
    )
  ),
  
  # 5. EORTC QLQ-CR29 (Colorectal)
  # Manual: 31-59. Excel: 1-29.
  "EORTC QLQ-CR29 (Colorectal)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 15:17, type = "functional"),
      list(name = "Ansiedad", items = 13, type = "functional"),
      list(name = "Peso", items = 14, type = "functional"),
      list(name = "Interés Sex. (H)", items = 26, type = "functional", reverse = 26),
      list(name = "Interés Sex. (M)", items = 28, type = "functional", reverse = 28),
      list(name = "Frec. Urinaria", items = 1:2, type = "symptom"),
      list(name = "Sangre Heces", items = 8:9, type = "symptom"),
      list(name = "Frec. Heces", items = 22:23, type = "symptom"),
      list(name = "Incontinencia Urin.", items = 3, type = "symptom"),
      list(name = "Disuria", items = 4, type = "symptom"),
      list(name = "Dolor Abdominal", items = 5, type = "symptom"),
      list(name = "Dolor Glúteo", items = 6, type = "symptom"),
      list(name = "Hinchazón", items = 7, type = "symptom"),
      list(name = "Boca Seca", items = 10, type = "symptom"),
      list(name = "Pérdida Cabello", items = 11, type = "symptom"),
      list(name = "Gusto", items = 12, type = "symptom"),
      list(name = "Flatulencia", items = 19, type = "symptom"),
      list(name = "Incont. Fecal", items = 20, type = "symptom"),
      list(name = "Piel Irritada", items = 21, type = "symptom"),
      list(name = "Vergüenza", items = 24, type = "symptom"),
      list(name = "Probl. Estoma", items = 25, type = "symptom"),
      list(name = "Impotencia", items = 27, type = "symptom"),
      list(name = "Dispareunia", items = 29, type = "symptom")
    )
  ),
  
  # 6. EORTC QLQ-ELD14 (Adulto Mayor)
  # Manual: 31-44. Excel: 1-14.
  "EORTC QLQ-ELD14 (Adulto Mayor)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Mantener Propósito", items = 11:12, type = "functional"),
      list(name = "Apoyo Familiar", items = 5, type = "functional"),
      list(name = "Movilidad", items = c(1, 3, 4), type = "symptom"),
      list(name = "Preocup. x Otros", items = 6:7, type = "symptom"),
      list(name = "Preocup. Futuro", items = 8:10, type = "symptom"),
      list(name = "Carga Enfermedad", items = 13:14, type = "symptom"),
      list(name = "Rigidez Articular", items = 2, type = "symptom")
    )
  ),
  
  # 7. EORTC QLQ-LC13 (Pulmón)
  # Manual: 31-42 (43 opcional). Excel: 1-12.
  "EORTC QLQ-LC13 (Pulmón)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Disnea", items = 3:5, type = "symptom"),
      list(name = "Tos", items = 1, type = "symptom"),
      list(name = "Hemoptisis", items = 2, type = "symptom"),
      list(name = "Boca Dolorida", items = 6, type = "symptom"),
      list(name = "Disfagia", items = 7, type = "symptom"),
      list(name = "Neuropatía", items = 8, type = "symptom"),
      list(name = "Alopecia", items = 9, type = "symptom"),
      list(name = "Dolor Pecho", items = 10, type = "symptom"),
      list(name = "Dolor Brazo", items = 11, type = "symptom"),
      list(name = "Dolor Otros", items = 12, type = "symptom")
    )
  ),
  
  # 8. EORTC QLQ-LC29 (Pulmón V.Nueva)
  # Manual: 31-59. Excel: 1-29.
  "EORTC QLQ-LC29 (Pulmón V.Nueva)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Tos", items = c(1, 22), type = "symptom"),
      list(name = "Falta Aire", items = 3:5, type = "symptom"),
      list(name = "Efectos Sec.", items = c(6:9, 13:18, 20, 23), type = "symptom"),
      list(name = "Miedo Progresión", items = c(19, 21), type = "symptom"),
      list(name = "Probl. Cirugía", items = 25:29, type = "symptom"),
      list(name = "Hemoptisis", items = 2, type = "symptom"),
      list(name = "Dolor Pecho", items = 10, type = "symptom"),
      list(name = "Dolor Brazo", items = 11, type = "symptom"),
      list(name = "Dolor Otro", items = 12, type = "symptom"),
      list(name = "Pérdida Peso", items = 24, type = "symptom")
    )
  ),
  
  # 9. EORTC QLQ-OV28 (Ovario) -> META COLS = 6
  # Manual: 31-58. Excel: 1-28.
  "EORTC QLQ-OV28 (Ovario)" = list(
    type = "defined", meta_cols = 6, range_global = 3,
    scales = list(
      list(name = "Imagen Corporal", items = 20:21, type = "functional"),
      list(name = "Actitud Enferm.", items = 22:24, type = "functional"),
      list(name = "Sexualidad", items = 25:28, type = "functional", reverse = 25:27),
      list(name = "Sínt. Abdominales", items = 1:7, type = "symptom"),
      list(name = "Neuropatía", items = 11:13, type = "symptom"),
      list(name = "Quimio Efectos", items = c(8:10, 14:17), type = "symptom"),
      list(name = "Sínt. Hormonales", items = 18:19, type = "symptom")
    )
  ),
  
  # 10. EORTC QLQ-PAN26 (Páncreas)
  # Manual: 31-56. Excel: 1-26.
  "EORTC QLQ-PAN26 (Páncreas)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Satisfacción", items = 23:24, type = "functional", reverse = 23:24),
      list(name = "Sexualidad", items = 25:26, type = "functional"),
      list(name = "Dolor Pancreático", items = c(1, 3:5), type = "symptom"),
      list(name = "Hinchazón", items = 2, type = "symptom"),
      list(name = "Sínt. Digestivos", items = 6:7, type = "symptom"),
      list(name = "Gusto", items = 8, type = "symptom"),
      list(name = "Indigestión", items = 9, type = "symptom"),
      list(name = "Flatulencia", items = 10, type = "symptom"),
      list(name = "Pérdida Peso", items = 11, type = "symptom"),
      list(name = "Debilidad", items = 12, type = "symptom"),
      list(name = "Boca Seca", items = 13, type = "symptom"),
      list(name = "Sínt. Hepáticos", items = 14:15, type = "symptom"),
      list(name = "Hábito Intest.", items = 16:17, type = "symptom"),
      list(name = "Imagen Corporal", items = 18:19, type = "symptom"), # Symptom en PAN26
      list(name = "Efectos Sec.", items = 20, type = "symptom"),
      list(name = "Preocup. Futuro", items = 21, type = "symptom"),
      list(name = "Planificación", items = 22, type = "symptom")
    )
  ),
  
  # 11. EORTC QLQ-PR25 (Próstata) -> META COLS = 6
  # Manual: 31-55. Excel: 1-25.
  "EORTC QLQ-PR25 (Próstata)" = list(
    type = "defined", meta_cols = 6, range_global = 3,
    scales = list(
      list(name = "Actividad Sexual", items = 20:21, type = "functional", reverse = 20:21),
      list(name = "Func. Sexual", items = 22:25, type = "functional", reverse = 22),
      list(name = "Sínt. Urinarios", items = c(1:7, 9), type = "symptom"),
      list(name = "Ayuda Incont.", items = 8, type = "symptom"),
      list(name = "Sínt. Intestinales", items = 10:13, type = "symptom"),
      list(name = "Sínt. Hormonales", items = 14:19, type = "symptom")
    )
  ),
  
  # 12. EORTC QLQ-H&N35 (Cabeza y Cuello)
  # Manual: 31-65. Excel: 1-35.
  "EORTC QLQ-H&N35 (Cabeza/Cuello)" = list(
    type = "defined", meta_cols = 7, range_global = 3,
    scales = list(
      list(name = "Dolor", items = 1:4, type = "symptom"),
      list(name = "Tragar", items = 5:8, type = "symptom"),
      list(name = "Sentidos", items = 13:14, type = "symptom"),
      list(name = "Habla", items = c(16, 23:24), type = "symptom"),
      list(name = "Comer Social", items = 19:22, type = "symptom"),
      list(name = "Contacto Social", items = c(18, 25:28), type = "symptom"),
      list(name = "Sexualidad", items = 29:30, type = "symptom"),
      list(name = "Dientes", items = 9, type = "symptom"),
      list(name = "Abrir Boca", items = 10, type = "symptom"),
      list(name = "Boca Seca", items = 11, type = "symptom"),
      list(name = "Saliva Espesa", items = 12, type = "symptom"),
      list(name = "Tos", items = 15, type = "symptom"),
      list(name = "Sentirse Mal", items = 17, type = "symptom"),
      list(name = "Analgésicos", items = 31, type = "symptom"),
      list(name = "Suplementos", items = 32, type = "symptom"),
      list(name = "Sonda", items = 33, type = "symptom"),
      list(name = "Pérdida Peso", items = 34, type = "symptom"),
      list(name = "Aumento Peso", items = 35, type = "symptom")
    )
  )
)

# ==============================================================================
# UI MEJORADA
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(icon("heart-pulse"), "Dashboard Calidad de Vida"),
    titleWidth = 350
    # Se quitó el botón "Sistema de Evaluación EORTC"
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Carga de Datos", tabName = "carga", icon = icon("upload")),
      menuItem("Vista Individual", tabName = "individual", icon = icon("user")),
      menuItem("Vista General (Scores)", tabName = "general", icon = icon("users")),
      menuItem("Evolución Temporal", tabName = "evolucion", icon = icon("chart-line")),
      menuItem("Estructura de Datos", tabName = "config", icon = icon("cog"))
    ),
    
    # Información adicional en el sidebar
    conditionalPanel(
      condition = "input.tabs != 'carga'",
      div(
        style = "padding: 15px; border-top: 1px solid #eee; margin-top: 20px;",
        h5("Información del Sistema", style = "font-weight: bold;"),
        uiOutput("system_info")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML("
        /* Estilos generales mejorados */
        .content-wrapper, .right-side { 
          background-color: #f8f9fa; 
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        
        .box { 
          border-radius: 10px; 
          box-shadow: 0 4px 8px rgba(0,0,0,0.1); 
          border-top: 3px solid #3c8dbc;
        }
        
        .box.box-primary {
          border-top-color: #3c8dbc;
        }
        
        .box.box-success {
          border-top-color: #00a65a;
        }
        
        .box.box-info {
          border-top-color: #00c0ef;
        }
        
        .box.box-warning {
          border-top-color: #f39c12;
        }
        
        .small-box { 
          border-radius: 8px; 
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .scrollable-table { 
          overflow-x: auto; 
        }
        
        /* Mejoras para value boxes */
        .small-box h3, .small-box p {
          font-weight: 600;
        }
        
        /* Mejoras para tablas */
        .dataTables_wrapper {
          border-radius: 8px;
        }
        
        /* Colores personalizados para gráficos */
        .functional-score { color: #00a65a; }
        .symptom-score { color: #dd4b39; }
        .global-score { color: #3c8dbc; }
        
        /* Estilos para títulos */
        .box-title {
          font-weight: 600;
          color: #2c3e50;
        }
        
        /* Mejoras en selectores */
        .selectize-input {
          border-radius: 6px;
        }
      "))
    ),
    
    tabItems(
      # Pestaña CARGA MEJORADA
      tabItem(tabName = "carga",
              fluidRow(
                box(
                  title = span(icon("upload"), "Cargar y Configurar Datos"), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = FALSE,
                  fluidRow(
                    column(6,
                           selectizeInput("survey_type", "1. Selecciona el Tipo de Cuestionario:", 
                                          choices = names(survey_configs), 
                                          width = "100%",
                                          options = list(placeholder = 'Selecciona un cuestionario...'))
                    ),
                    column(6,
                           fileInput("file_upload", "2. Sube el Archivo Excel",
                                     accept = c(".xlsx", ".xls"), 
                                     buttonLabel = "Examinar...", 
                                     placeholder = "Ningún archivo seleccionado",
                                     width = "100%")
                    )
                  ),
                  hr(),
                  uiOutput("file_status_ui"),
                  br(),
                  h4("Vista Previa de Datos"),
                  DTOutput("data_preview")
                )
              )
      ),
      
      # Pestaña INDIVIDUAL MEJORADA - MODIFICADA
      tabItem(tabName = "individual",
              fluidRow(
                box(
                  title = span(icon("search"), "Seleccionar Paciente"), 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("rut_select", "Seleccionar RUT:", 
                              choices = NULL,
                              selectize = TRUE),
                  uiOutput("patient_details"),
                  hr()
                ),
                # Reemplazamos el gráfico por información del paciente (como en la versión anterior)
                box(
                  title = span(icon("user"), "Información del Paciente"), 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 8,
                  uiOutput("patient_info")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("calculator"), "Scores Individuales de Escalas EORTC (0-100)"), 
                  status = "success", solidHeader = TRUE, width = 12,
                  plotlyOutput("individual_scores_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = span(icon("table"), "Historial de Respuestas"), 
                  width = 12, 
                  status = "info",
                  div(class="scrollable-table", 
                      DTOutput("individual_responses"))
                )
              )
      ),
      
      # Pestaña GENERAL MEJORADA
      tabItem(tabName = "general",
              fluidRow(
                valueBoxOutput("total_patients", width = 4),
                valueBoxOutput("total_encuestas", width = 4),
                valueBoxOutput("mean_age", width = 4)
              ),
              
              # SECCIÓN DE SCORES CALCULADOS
              conditionalPanel(
                condition = "input.survey_type != 'GENERICO'",
                fluidRow(
                  box(
                    title = span(icon("calculator"), "Promedios de Escalas EORTC (0-100)"), 
                    status = "success", solidHeader = TRUE, width = 12,
                    p("Este gráfico muestra el promedio de las escalas calculadas según el manual oficial de la encuesta seleccionada."),
                    plotlyOutput("scores_plot", height = "500px")
                  )
                )
              ),
              
              # SECCIÓN DE FILTROS INDIVIDUALES
              fluidRow(
                box(title = span(icon("chart-bar"), "Distribución por Pregunta Individual"), 
                    status = "warning", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, uiOutput("pregunta_select_ui")),
                      column(8, plotlyOutput("general_plot", height = "350px"))
                    )
                )
              ),
              
              fluidRow(
                box(title = span(icon("database"), "Tabla de Datos Completa"), 
                    width = 12, collapsible = TRUE, collapsed = TRUE,
                    status = "primary",
                    div(class="scrollable-table", DTOutput("data_table")))
              )
      ),
      
      # Pestaña EVOLUCIÓN MEJORADA - CORREGIDA
      tabItem(tabName = "evolucion",
              fluidRow(
                box(title = span(icon("sliders-h"), "Configuración de Evolución"), 
                    status = "primary", solidHeader = TRUE, width = 3,
                    selectizeInput("rut_evolution", "Paciente:", 
                                   choices = NULL,
                                   options = list(placeholder = 'Selecciona un paciente...')),
                    radioButtons("evol_mode", "Modo de Visualización:", 
                                 choices = c("Preguntas" = "preguntas", 
                                             "Scores" = "scores"),
                                 selected = "scores"),
                    uiOutput("variable_evolution_ui")
                ),
                box(title = span(icon("chart-line"), "Evolución Temporal"), 
                    status = "primary", solidHeader = TRUE, width = 9,
                    plotlyOutput("evolution_plot", height = "500px"))
              )
      ),
      
      # Pestaña CONFIG MEJORADA
      tabItem(tabName = "config",
              fluidRow(
                box(title = span(icon("cogs"), "Estructura de Datos Detectada"), 
                    status = "info", width = 12, 
                    verbatimTextOutput("data_structure"))
              )
      )
    )
  )
)

# ==============================================================================
# SERVER MEJORADO
# ==============================================================================
server <- function(input, output, session) {
  
  # Variables Reactivas
  data_raw <- reactiveVal()       # Datos tal cual vienen del excel
  data_scores <- reactiveVal()    # Datos con scores 0-100 calculados
  survey_metadata <- reactiveVal() # Info de columnas
  
  # AGREGAR ESTA LÍNEA - Variable reactiva para compatibilidad
  data_reactive <- reactive({ data_raw() })
  
  # Información del sistema
  output$system_info <- renderUI({
    req(data_raw())
    tagList(
      strong("Cuestionario:"), br(), input$survey_type, br(), br(),
      strong("Pacientes:"), br(), length(unique(data_raw()[[2]])), br(), br(),
      strong("Evaluaciones:"), br(), nrow(data_raw())
    )
  })
  
  # ----------------------------------------------------------------------------
  # Lógica de Carga y Procesamiento
  # ----------------------------------------------------------------------------
  # En el observer de carga de datos, agregar conversión de fecha
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      # 1. Leer Excel
      df <- read_excel(input$file_upload$datapath)
      
      # Validar columnas mínimas (Primeras 7 fijas)
      if (ncol(df) < 8) stop("El archivo debe tener al menos 8 columnas (7 de metadatos + preguntas)")
      
      # 2. CONVERTIR MARCA TEMPORAL A FECHA - AGREGAR ESTO
      if (inherits(df[[1]], "POSIXt") || inherits(df[[1]], "Date")) {
        # Ya es fecha, no hacer nada
      } else {
        # Intentar convertir a fecha
        df[[1]] <- as.POSIXct(df[[1]], tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y"))
      }
      
      # 3. Convertir preguntas a numérico
      fixed_cols <- names(df)[1:7]
      question_cols <- names(df)[8:ncol(df)]
      
      for(col in question_cols) {
        df[[col]] <- as.numeric(as.character(df[[col]]))
      }
      
      data_raw(df)
      
      # 3. Calcular SCORES según configuración
      config_name <- input$survey_type
      config <- survey_configs[[config_name]]
      
      if (config$type == "defined") {
        # Crear dataframe base con los metadatos
        scores_df <- df[, 1:7]
        
        # Iterar sobre cada escala definida
        for (scale in config$scales) {
          # Mapeo de items a nombres de columna reales
          valid_indices <- scale$items[scale$items <= length(question_cols)]
          
          if (length(valid_indices) > 0) {
            col_names <- question_cols[valid_indices]
            
            # Obtener nombres de columnas para reverse (si hay)
            reverse_cols <- NULL
            if (!is.null(scale$reverse)) {
              valid_rev <- scale$reverse[scale$reverse <= length(question_cols)]
              if (length(valid_rev) > 0) {
                reverse_cols <- question_cols[valid_rev]
              }
            }
            
            # Determinar rango (global o específico)
            rango_uso <- if (!is.null(scale$range)) scale$range else config$range_global
            
            # Calcular Score
            scores_df[[scale$name]] <- calculate_score(
              df[, question_cols], 
              scale$items, 
              type = scale$type, 
              range_val = rango_uso,
              reverse_rel = scale$reverse
            )
          }
        }
        data_scores(scores_df)
        showNotification(paste("Scores calculados para:", config_name), type = "message")
        
      } else {
        data_scores(NULL) # Modo genérico no calcula scores
      }
      
      # Actualizar metadatos
      survey_metadata(list(
        fixed = fixed_cols,
        questions = question_cols,
        scales = if(!is.null(data_scores())) names(data_scores())[8:ncol(data_scores())] else NULL
      ))
      
      # Actualizar UI Inputs
      ruts <- unique(df[[2]]) # Asumiendo RUT en col 2
      updateSelectInput(session, "rut_select", choices = ruts)
      updateSelectizeInput(session, "rut_evolution", choices = ruts, server = TRUE)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Estado del archivo
  output$file_status_ui <- renderUI({
    if (is.null(data_raw())) {
      div(class = "alert alert-warning", role = "alert",
          icon("exclamation-triangle"), 
          strong(" Esperando archivo..."),
          " Por favor, sube un archivo Excel para comenzar."
      )
    } else {
      div(class = "alert alert-success", role = "alert",
          icon("check-circle"), 
          strong(" Archivo cargado exitosamente!"),
          br(),
          paste("Registros:", nrow(data_raw()), "| Configuración:", input$survey_type)
      )
    }
  })
  
  output$data_preview <- renderDT({
    req(data_raw())
    datatable(
      head(data_raw(), 5), 
      options = list(
        dom = 't',
        scrollX = TRUE,
        pageLength = 5
      ),
      class = 'cell-border stripe hover'
    )
  })
  
  # ----------------------------------------------------------------------------
  # VISTA INDIVIDUAL MEJORADA - MODIFICADA
  # ----------------------------------------------------------------------------
  # Información del paciente seleccionado - CORREGIDA
  output$patient_info <- renderUI({
    req(input$rut_select, data_reactive())
    
    paciente <- data_reactive() %>%
      filter(if_any(2, ~ . == input$rut_select))  # Reemplazar across por if_any
    
    if (nrow(paciente) == 0) return(NULL)
    
    # VERIFICAR Y CONVERTIR FECHAS SI ES NECESARIO
    fecha_col <- names(paciente)[1]
    
    # Asegurarse de que la columna de fecha sea POSIXct
    if (!inherits(paciente[[1]], "POSIXct")) {
      paciente[[1]] <- as.POSIXct(paciente[[1]], tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y"))
    }
    
    
    # Estadísticas del paciente - CALCULAR CORRECTAMENTE
    evaluaciones <- nrow(paciente)
    primera_eval <- min(paciente[[1]], na.rm = TRUE)
    ultima_eval <- max(paciente[[1]], na.rm = TRUE)
    dias_seguimiento <- as.numeric(difftime(ultima_eval, primera_eval, units = "days"))
    
    # Si hay solo una evaluación, el seguimiento es 0 (correcto)
    # Si hay múltiples pero fechas iguales, también será 0
    
    tagList(
      fluidRow(
        column(6,
               h4("📊 Estadísticas de Seguimiento"),
               p(icon("calendar-check"), strong("Total de evaluaciones:"), evaluaciones),
               p(icon("clock"), strong("Período de seguimiento:"), 
                 if(evaluaciones > 1) paste(round(dias_seguimiento), "días") else "0 días (evaluación única)"),
               p(icon("calendar-plus"), strong("Primera evaluación:"), 
                 if(!is.na(primera_eval)) format(primera_eval, "%d/%m/%Y %H:%M") else "No disponible"),
               p(icon("calendar-day"), strong("Última evaluación:"), 
                 if(!is.na(ultima_eval)) format(ultima_eval, "%d/%m/%Y %H:%M") else "No disponible")
        ),
        column(6,
               h4("👤 Información Demográfica"),
               p(icon("user"), strong("RUT:"), input$rut_select),
               p(icon("birthday-cake"), strong("Edad:"), paciente$`Edad (en años)`[1], "años"),
               p(icon("venus-mars"), strong("Sexo:"), paciente$Sexo[1]),
               p(icon("graduation-cap"), strong("Educación:"), paciente$`Nivel Educacional`[1]),
               p(icon("heart"), strong("Estado Civil:"), paciente$`Estado Civil`[1]),
               p(icon("briefcase"), strong("Ocupación:"), paciente$`Ocupación actual`[1])
        )
      )
    )
  })
  
  
  # REEMPLAZAR TODOS LOS FILTROS ACROSS POR IF_ANY:
  
  # En individual_responses
  output$individual_responses <- renderDT({
    req(input$rut_select, data_raw())
    df <- data_raw() %>% filter(if_any(2, ~ . == input$rut_select))  # if_any en lugar de across
    
    datatable(
      df,
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
      class = 'cell-border stripe hover'
    )
  })
  
  # En individual_plot (si lo mantienes)
  output$individual_plot <- renderPlotly({
    req(input$rut_select, data_raw(), survey_metadata())
    # Tomar última evaluación
    df <- data_raw() %>% 
      filter(if_any(2, ~ . == input$rut_select)) %>%  # if_any en lugar de across
      arrange(desc(across(1))) %>% 
      slice(1)
    
    q_cols <- survey_metadata()$questions
    vals <- as.numeric(df[1, q_cols])
    
    # Crear dataframe para el gráfico
    plot_data <- data.frame(
      Pregunta = factor(q_cols, levels = q_cols),
      Valor = vals
    )
    
    plot_ly(plot_data, x = ~Pregunta, y = ~Valor, type = 'bar',
            marker = list(color = '#3498db',
                          line = list(color = '#2980b9', width = 1.5))) %>%
      layout(
        title = list(
          text = paste("Última Evaluación -", input$rut_select),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Preguntas",
          tickangle = -45
        ),
        yaxis = list(
          title = "Respuesta (1-4)",
          range = c(0, 5)
        ),
        margin = list(b = 100)
      )
  })
  
  # En individual_scores_plot
  output$individual_scores_plot <- renderPlotly({
    req(input$rut_select, data_scores())
    
    # Filtrar datos del paciente seleccionado
    df_pt <- data_scores() %>% 
      filter(if_any(2, ~ . == input$rut_select)) %>%  # if_any en lugar de across
      arrange(desc(across(1))) %>% 
      slice(1)
    
    if (nrow(df_pt) == 0) {
      return(plotly_empty() %>%
               layout(
                 title = list(
                   text = "No hay datos disponibles para este paciente",
                   font = list(color = '#7f8c8d')
                 )
               ))
    }
    
    # Obtener columnas de escalas (a partir de la columna 8)
    scale_cols <- names(df_pt)[8:ncol(df_pt)]
    scores <- as.numeric(df_pt[1, scale_cols])
    
    # Obtener tipos de escalas para colorear
    config <- survey_configs[[input$survey_type]]
    scale_types <- setNames(rep("Desconocido", length(scale_cols)), scale_cols)
    
    if (!is.null(config$scales)) {
      for(s in config$scales) {
        if (s$name %in% scale_cols) {
          scale_types[s$name] <- s$type
        }
      }
    }
    
    # Colores según tipo de escala
    color_map <- c(
      "functional" = "#2ecc71", 
      "global" = "#3498db", 
      "symptom" = "#e74c3c", 
      "Desconocido" = "#95a5a6"
    )
    
    # Crear dataframe para el gráfico
    plot_data <- data.frame(
      Escala = factor(scale_cols, levels = scale_cols),
      Score = scores,
      Tipo = factor(scale_types[scale_cols], levels = c("functional", "global", "symptom", "Desconocido"))
    )
    
    plot_ly(plot_data, x = ~Escala, y = ~Score, type = 'bar',
            color = ~Tipo, colors = color_map,
            text = ~paste("Score:", round(Score, 1)),
            textposition = 'auto',
            marker = list(line = list(color = 'rgba(0,0,0,0.2)', width = 1))) %>%
      layout(
        title = list(
          text = paste("Scores Individuales -", input$rut_select),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Escalas",
          tickangle = -45
        ),
        yaxis = list(
          title = "Score (0-100)",
          range = c(0, 100)
        ),
        margin = list(b = 100),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.3
        )
      )
  })
  
  # ----------------------------------------------------------------------------
  # VISTA GENERAL (sin cambios mayores, solo mejoras estéticas)
  # ----------------------------------------------------------------------------
  # Value Boxes
  output$total_patients <- renderValueBox({
    req(data_raw())
    valueBox(
      length(unique(data_raw()[[2]])), 
      "Pacientes Únicos", 
      icon = icon("users"), 
      color = "aqua"
    )
  })
  
  output$total_encuestas <- renderValueBox({
    req(data_raw())
    valueBox(
      nrow(data_raw()), 
      "Total de Evaluaciones", 
      icon = icon("clipboard-list"), 
      color = "green"
    )
  })
  
  output$mean_age <- renderValueBox({
    req(data_raw())
    avg <- mean(as.numeric(data_raw()[[3]]), na.rm=TRUE) # Asumiendo Edad col 3
    valueBox(
      round(avg, 1), 
      "Edad Promedio", 
      icon = icon("calendar-alt"), 
      color = "purple"
    )
  })
  
  # Gráfico de promedios de escalas
  output$scores_plot <- renderPlotly({
    req(data_scores())
    
    # Calcular promedios globales de cada escala
    df_long <- data_scores() %>%
      pivot_longer(cols = 8:ncol(data_scores()), names_to = "Escala", values_to = "Score") %>%
      group_by(Escala) %>%
      summarise(Mean = mean(Score, na.rm=TRUE), .groups = 'drop')
    
    # Intentar categorizar colores según diccionario para Functional vs Symptom
    config <- survey_configs[[input$survey_type]]
    
    # Asignar tipos a las escalas para colorear
    df_long$Type <- "Desconocido"
    if (!is.null(config$scales)) {
      for(s in config$scales) {
        df_long$Type[df_long$Escala == s$name] <- s$type
      }
    }
    
    # Colores: Functional/Global (Alto=Bueno) -> Verde, Symptom (Alto=Malo) -> Rojo
    cols <- c("functional" = "#2ecc71", "global" = "#3498db", "symptom" = "#e74c3c", "Desconocido" = "#95a5a6")
    
    plot_ly(df_long, x = ~Escala, y = ~Mean, type = 'bar', 
            color = ~Type, colors = cols,
            text = ~round(Mean, 1), textposition = 'auto',
            marker = list(line = list(color = 'rgba(0,0,0,0.2)', width = 1))) %>%
      layout(
        title = list(
          text = paste("Perfil Promedio de Salud -", input$survey_type),
          font = list(size = 16)
        ),
        yaxis = list(
          title = "Score Promedio (0-100)", 
          range = c(0, 100)
        ),
        xaxis = list(
          title = "Escalas",
          tickangle = -45
        ),
        margin = list(b = 100),
        showlegend = TRUE
      )
  })
  
  # Filtro Pregunta Individual
  output$pregunta_select_ui <- renderUI({
    req(survey_metadata())
    selectizeInput("pregunta_gral", "Seleccionar Pregunta:", 
                   choices = survey_metadata()$questions)
  })
  
  output$general_plot <- renderPlotly({
    req(data_raw(), input$pregunta_gral)
    col_name <- input$pregunta_gral
    df <- data_raw()
    
    counts <- table(df[[col_name]], useNA = "ifany")
    plot_data <- data.frame(
      Respuesta = names(counts),
      Frecuencia = as.numeric(counts)
    )
    
    # Reemplazar NA por "No respondido" si existe
    plot_data$Respuesta[is.na(plot_data$Respuesta)] <- "No respondido"
    
    plot_ly(plot_data, x = ~Respuesta, y = ~Frecuencia, type = 'bar',
            marker = list(color = '#f39c12',
                          line = list(color = '#e67e22', width = 1.5))) %>%
      layout(
        title = list(
          text = paste("Distribución:", col_name),
          font = list(size = 16)
        ),
        xaxis = list(title = "Respuesta"),
        yaxis = list(title = "Frecuencia")
      )
  })
  
  output$data_table <- renderDT({
    req(data_raw())
    df_show <- if(!is.null(data_scores())) data_scores() else data_raw()
    datatable(
      df_show, 
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    )
  })
  
  # ----------------------------------------------------------------------------
  # EVOLUCIÓN TEMPORAL - CORREGIDA
  # ----------------------------------------------------------------------------
  output$variable_evolution_ui <- renderUI({
    req(survey_metadata(), input$evol_mode)
    
    if (input$evol_mode == "preguntas") {
      choices <- survey_metadata()$questions
    } else {
      req(data_scores())
      # Usar los nombres reales de las columnas de scores
      scale_cols <- names(data_scores())[8:ncol(data_scores())]
      choices <- setNames(scale_cols, scale_cols)
    }
    
    selectizeInput("var_evol", "Seleccionar Variables:", 
                   choices = choices, 
                   multiple = TRUE,
                   options = list(maxItems = 5, placeholder = 'Selecciona variables...'))
  })
  
  # En evolution_plot (ya corregido anteriormente, pero mantener if_any)
  output$evolution_plot <- renderPlotly({
    req(input$rut_evolution, input$var_evol, input$evol_mode)
    
    tryCatch({
      # Seleccionar dataset correcto
      if(input$evol_mode == "scores") {
        df_source <- data_scores()
      } else {
        df_source <- data_raw()
      }
      
      req(df_source)
      
      # Filtrar por RUT usando if_any
      df_pt <- df_source %>% 
        filter(if_any(2, ~ . == input$rut_evolution)) %>%  # if_any en lugar de across
        arrange(across(1))
      
      if(nrow(df_pt) == 0) {
        return(plotly_empty() %>%
                 layout(
                   title = list(
                     text = "No hay datos disponibles para este paciente",
                     font = list(color = '#7f8c8d')
                   )
                 ))
      }
      
      # Asumiendo col 1 es Fecha/Marca temporal
      fecha_col <- names(df_source)[1]
      x_vals <- df_pt[[fecha_col]]
      
      p <- plot_ly(df_pt, x = x_vals)
      
      # Paleta de colores para múltiples variables
      colors <- c('#e74c3c', '#3498db', '#2ecc71', '#f39c12', '#9b59b6')
      
      for(i in seq_along(input$var_evol)) {
        var <- input$var_evol[i]
        color <- colors[(i-1) %% length(colors) + 1]
        
        # Verificar que la variable existe en el dataframe
        if(var %in% names(df_pt)) {
          y_vals <- df_pt[[var]]
          
          p <- p %>% add_trace(y = y_vals, 
                               name = var, type = 'scatter', mode = 'lines+markers',
                               line = list(color = color, width = 3),
                               marker = list(color = color, size = 8))
        }
      }
      
      y_title <- if(input$evol_mode == "scores") "Score (0-100)" else "Respuesta"
      
      # Rango fijo de 0-100 para scores
      y_range <- if(input$evol_mode == "scores") c(0, 100) else NULL
      
      p %>% layout(
        title = list(
          text = paste("Evolución Temporal -", input$rut_evolution),
          font = list(size = 16)
        ),
        yaxis = list(
          title = y_title,
          range = y_range
        ),
        xaxis = list(title = "Fecha"),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
    }, error = function(e) {
      showNotification(paste("Error en gráfico de evolución:", e$message), type = "error")
      return(plotly_empty())
    })
  })
  
  # ----------------------------------------------------------------------------
  # CONFIGURACIÓN
  # ----------------------------------------------------------------------------
  output$data_structure <- renderPrint({
    req(data_raw())
    cat("=== ESTRUCTURA DE DATOS CRUDOS ===\n")
    str(data_raw())
    if(!is.null(data_scores())) {
      cat("\n\n=== ESTRUCTURA DE SCORES CALCULADOS ===\n")
      str(data_scores())
    }
  })
}

shinyApp(ui, server)

