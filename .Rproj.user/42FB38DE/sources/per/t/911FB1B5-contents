##LIBRARIES

library(shiny)
library(plotly)
library(shinydashboard)
library(kableExtra)
library(shinythemes)
library(shinycssloaders)
library(lubridate)
library(leaflet)
library(shinyauthr)
library(bslib)

##UI

tagList(
  tags$head(tags$link(rel="shortcut icon", href='cv.ico')),
  tags$head(tags$link(rel="icon", href='cv.ico')),
  tags$style(HTML("
    .navbar-brand {
    padding: 5px 15px 10px 15px;
    } 
    
  ")),
  
  ui <- fluidPage(
    
    conditionalPanel(
      condition = "input.loadButton == 0",
      div(
        style = "text-align: center;",
        h2("Bienvenido"),
        p("Haga clic en el botón para cargar el informe."),
        actionButton("loadButton", "Cargar Informe", class = "btn btn-primary btn-lg")
      )
    ),
    conditionalPanel(
      condition = "input.loadButton == 1",
      navbarPage(
        title = img(src = "cv.ico", height = 45, width = 45),
        tabPanel("Informe de quejas y sugerencias",
                 sidebarLayout(
                   sidebarPanel(width = 3,
                                uiOutput("dateRangeInput"),
                                h4("Distritos"),
                                leafletOutput(outputId = "map") %>% withSpinner(color = "#2C3E50"),
                                uiOutput("Generalcheckbox"),
                                uiOutput("uirender_distritos")
                   ),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("General", 
                                div(style = "margin-top: 10px;",  
                                    h3("Información general")
                                ),
                                fluidRow(
                                  column(6, plotlyOutput("GeneralTipo", height = "280px") %>% withSpinner(color = "#2C3E50")),
                                  column(6, plotlyOutput("GeneralCanal", height = "280px") %>% withSpinner(color = "#2C3E50")),
                                  column(12, plotlyOutput("GeneralTema", height = "300px") %>% withSpinner(color = "#2C3E50"))
                                ),
                                fluidRow(
                                  h3("Registros"),
                                  column(12, DT::dataTableOutput("tbl") %>% withSpinner(color = "#2C3E50"))
                                )
                       ),
                       tabPanel("Evolución temporal", 
                                div(style = "margin-top: 10px;",  
                                    uiOutput("uirender_year")
                                ),
                                h3("Solicitudes por día"),
                                plotlyOutput("grafico_temp"),
                                h3("Solicitudes por mes"),
                                plotlyOutput("grafico_mes")
                       ),
                       tabPanel("Relaciones 2-2",
                                div(style = "margin-top: 10px;",
                                    h3("Relación entre tipo de solicitud y tema"),
                                ),
                                selectInput(inputId = "Tipos", label = h4("Seleccione tipo de solicitud", style = 'padding-top: 10px'),
                                            choices = c("Sugerencia", "Defensor", "Queja", "Sindic", "Otras"), selected = "Sugerencia"),
                                plotlyOutput("Tipos_", height = "800px"),
                                h3("Relación entre tipo de solicitud y canal"),
                                plotlyOutput("comb")
                       ),
                       tabPanel("Subtemas",
                                checkboxGroupInput(inputId = "Tipus", label = h4("Seleccione tipo de solicitud"), inline = TRUE,
                                                   choices = c("Sugerencia", "Defensor", "Queja", "Sindic", "Otras"), selected = "Sugerencia"),
                                uiOutput("slider_"),
                                plotlyOutput("SuggerimentsTipus", height = "1250px")
                       )
                     )
                   )
                 )
        )
      )
    )
  )
)