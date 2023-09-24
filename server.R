shinyServer <- function(input, output, session){
  
  ##LIBRARIES
  require(shiny)
  library(ggplot2)
  library(DT)
  library(data.table)
  library(dplyr)
  library(plotly)
  library(lubridate)
  library(rgdal)
  library(igraph)
  library(leaflet)
  library(raster)
  library(tidyr)
  library(readr)
  library(forcats)
  library(ggmosaic)
  library(httr)
  library(jsonlite)
  library(lubridate)
  
  
  ##DATA 
  
  loaded_data <- reactive({
    if (input$loadButton == 1) {
      export_api_url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/total-castellano/exports/json"
      api_call <- GET(export_api_url)
      Datos <<- fromJSON(rawToChar(api_call$content), flatten = TRUE)
      return(Datos)
    
    } 
    else {
      return(data.frame())
    }
  })
  
  
  observe({
    if (!is.null(loaded_data()) && nrow(loaded_data()) > 0) {
      
      output$dateRangeInput <- renderUI({
        min_date <- min(loaded_data()$fecha_entrada_ayuntamiento)
        max_date <- max(loaded_data()$fecha_entrada_ayuntamiento)
        
        dateRangeInput("dateRange", "Intervalo de fechas:",
                       start = min_date,
                       end = max_date,
                       format = "yyyy-mm-dd",
                       separator = "a"
        )
      })
  
      distr <- readOGR("./Distritos.JSON")
      names(distr) <- c("name", "id")
      distr$name <- as.factor(distr$name)
      
      names(Datos) <- c("id", "Tipo", "Canal", "Fecha", "Tema", "Subtema", "Distrito_solic", "Barrio_solic", "Distrito_loc", "Barrio_loc")
      Datos$Tema <- as.factor(Datos$Tema)
      Datos$Tipo <- as.factor(Datos$Tipo)
      Datos$Canal <- as.factor(Datos$Canal)
      Datos$Year <- year(Datos$Fecha)
      
      Datos$Barrio_loc <- na_if(Datos$Barrio_loc, "No hi consta")
      Datos$Distrito_loc <- na_if(Datos$Distrito_loc, "No hi consta")
      Datos$Barrio_solic <- na_if(Datos$Barrio_solic, "No hi consta")
      Datos$Distrito_solic <- na_if(Datos$Distrito_solic, "No hi consta")
      
      Datos$Distrito <- coalesce(Datos$Distrito_loc, Datos$Distrito_solic)
      Datos$Barrio <- coalesce(Datos$Barrio_loc, Datos$Barrio_solic)
      
      Datos$Barrio <- as.factor(Datos$Barrio)
      Datos$Subtema <- as.factor(Datos$Subtema)
      
      Datos$Fecha <- as.Date(Datos$Fecha, "%Y-%m-%d", tz="UTC")
      
      Datos$Distrito <- replace_na(Datos$Distrito, "DESCONEGUT")
      Datos$Distrito <- toupper(Datos$Distrito)
      Datos$Distrito <- as.factor(Datos$Distrito)
      
      Tabla <- Datos %>% dplyr::select(-Barrio_loc, -Distrito_loc, -Barrio_solic, -Distrito_solic, -Year)
      
      levels(Tabla$Distrito)[match("ALGIRÓS",levels(Tabla$Distrito))] <- "ALGIROS"
      levels(Tabla$Distrito)[match("LA SAÏDIA",levels(Tabla$Distrito))] <- "LA SAIDIA"
      levels(Tabla$Distrito)[match("LA SAÏDA",levels(Tabla$Distrito))] <- "LA SAIDIA"
      levels(Tabla$Distrito)[match("JESÚS",levels(Tabla$Distrito))] <- "JESUS"
      levels(Tabla$Distrito)[match("POBLATS MARÍTIMS",levels(Tabla$Distrito))] <- "POBLATS MARITIMS"
      levels(distr$name)[match("POBLATS DE L'OEST",levels(distr$name))] <- "POBLES DE L'OEST"
      levels(distr$name)[match("POBLATS DEL NORD",levels(distr$name))] <- "POBLES DEL NORD"
      levels(distr$name)[match("POBLATS DEL SUD",levels(distr$name))] <- "POBLES DEL SUD"
      
      unique_years <- reactive({
        sort(unique(Datos$Year))
      })
      
      observe({
        years <- as.character(unique_years())
        updateSelectInput(session, "selectedYear", label = "Select Year", choices = years, selected = years[1])
      })
      
      
      ##UI
      
      output$Generalcheckbox <- renderUI({
        checkboxInput(inputId = "allSelect", label="Seleccionar todos", value=TRUE)
      })
      
      
      output$uirender_distritos<- renderUI({
        selectInput(inputId = "selected_locations", label = "", choices = levels(Tabla$Distrito), selected = selected$groups, multiple = TRUE)
        
      })
      
      
      ##DISTRICTS MAP
      
      output$map <- renderLeaflet({
        leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, doubleClickZoom = FALSE)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(data = distr, fillColor = "white", fillOpacity = 0.5, color = "black", 
                      stroke = TRUE, weight = 1, layerId = ~name, group = "regions", label = ~name) %>%
          addPolygons(data = distr, fillColor = "#2C3E50", fillOpacity = 1, weight = 1, color = "black",
                      stroke = TRUE, layerId = ~id, group = ~name) %>%
          hideGroup(group = distr$name) 
      })
      
      proxy <- leafletProxy("map")
      
      selected <- reactiveValues(groups = vector())
      
      observeEvent(input$map_shape_click, {
        if(input$map_shape_click$group == "regions"){
          selected$groups <- c(selected$groups, input$map_shape_click$id)
          proxy %>% showGroup(group = input$map_shape_click$id)
        } else {
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
          proxy %>% hideGroup(group = input$map_shape_click$group)
        }
        updateSelectizeInput(session, inputId = "selected_locations", label = "", choices = levels(Tabla$Distrito), selected = selected$groups)
      })
      
      observeEvent(input$selected_locations, {
        removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
        added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
        
        if(length(removed_via_selectInput) > 0){
          selected$groups <- input$selected_locations
          proxy %>% hideGroup(group = removed_via_selectInput)
        }
        
        if(length(added_via_selectInput) > 0){
          selected$groups <- input$selected_locations
          proxy %>% showGroup(group = added_via_selectInput)
        }
      }, ignoreNULL = FALSE)
      
      observeEvent(input$allSelect, {
        if(input$allSelect){
          selected$groups <- levels(Tabla$Distrito)
          proxy %>% showGroup(group = selected$groups)
        } else{
          proxy %>% hideGroup(group = selected$groups)
          selected$groups <- vector()
        }
        updateSelectizeInput(session, inputId = "selected_locations", label = "", choices = levels(Tabla$Distrito), selected = selected$groups)
      })
      
      
      ##GENERAL
      
      GeneralData <- reactive(
          Tabla %>% 
          filter(Fecha > input$dateRange[1] & Fecha < input$dateRange[2]) %>% 
          filter(Distrito %in% selected$groups) %>% 
          dplyr::select(-id)
        
      )
      
      output$GeneralTipo <- renderPlotly({
        gg <- ggplot(GeneralData()) +
          geom_bar(aes(x = fct_infreq(Tipo), text = paste("Cantidad: ", after_stat(count))), fill = "#2C3E50") +
          xlab("Tipo") +
          ylab("Cantidad") +
          theme(axis.text.x = element_text(angle = 0))
        
        ggplotly(gg, tooltip = "text")
      })
      
      
      output$GeneralCanal <- renderPlotly({
        ggplot(GeneralData()) + 
          geom_bar(aes(x = fct_infreq(Canal), text = paste("Cantidad: ", after_stat(count))), fill = "#2C3E50") + 
          xlab("Canal") + 
          ylab("") + 
          theme(axis.text.x = element_text(angle = 0))
        
        ggplotly(tooltip = "text")  
      })
      
      
      GeneralTm <- reactive({
        filtered_data <- Datos %>%
          dplyr::select(-Barrio_loc, -Distrito_loc, -Barrio_solic, -Distrito_solic) %>%
          filter(Distrito %in% selected$groups) %>%
          group_by(Tema) %>%
          summarise(Cantidad = n()) %>%
          arrange(Cantidad) %>%
          mutate(Tema = factor(Tema, Tema))
        filtered_data
      })
      
      
      output$GeneralTema <- renderPlotly({
        ggplot(GeneralTm(), aes(x=Tema, y = Cantidad)) + 
          geom_col(fill="#2C3E50") + 
          coord_flip() +
          xlab("Temas") + 
          ylab("Cantidad")
      })
      
      
      output$tbl <- DT::renderDataTable(
        GeneralData() %>%
          arrange(Fecha) %>%
          DT::datatable(
            options = list(
              language = list(
                search = "Buscar:",
                lengthMenu = "Mostrar _MENU_ entradas por página",
                zeroRecords = "No se encontraron registros",
                info = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
                infoEmpty = "Mostrando 0 a 0 de 0 entradas",
                infoFiltered = "(filtrado de _MAX_ entradas totales)",
                paginate = list(
                  first = "Primero",
                  previous = "Anterior",
                  "next" = "Siguiente",
                  last = "Último"
                )
              )
            )
          )
      )
      
      
      ##EVOLUCIÓN TEMPORAL
      
      Datos_mes <- reactive({
        if (is.null(loaded_data())) return(NULL)
        
        year_selected <- as.integer(input$selectedYear)
        
        Datos_mes <- cbind(Datos, Mes = month(Datos$Fecha))
        Datos_mes <- cbind(Datos_mes, cont = rep(1, dim(Datos)[1]))
        
        df <- Datos_mes %>%
          filter(Fecha > input$dateRange[1] & Fecha < input$dateRange[2]) %>%
          filter(Distrito %in% selected$groups) %>%
          filter(year(Fecha) == year_selected) %>%  
          dplyr::select(Mes, cont) %>%
          group_by(Mes) %>%
          summarise(sum = sum(cont))
        
        df
      })
      
      
      output$grafico_mes <- renderPlotly({
        if (is.null(Datos_mes())) return(NULL)
        
        # Define Spanish month names
        month_names_spanish <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
        
        gg <- ggplot(data = Datos_mes(), aes(x = Mes, y = sum)) +
          geom_line(aes(text = paste("Mes: ", month_names_spanish[round(Mes)], "<br>Solicitudes: ", sum), group = 1)) +
          geom_point(aes(text = paste("Mes: ", month_names_spanish[round(Mes)], "<br>Solicitudes: ", sum))) +
          ylab("Solicitudes")
        
        gg <- gg + scale_x_continuous(breaks = 1:12, labels = month_names_spanish)
        
        p <- ggplotly(gg, tooltip = "text")
        
        p
      })
      
      
      Datos_temp <- reactive({
        Datos_temp <- Datos
        Datos_temp <- cbind(Datos_temp, cont = rep(1, dim(Datos)[1]))
        
        year_selected <- as.integer(input$selectedYear)
        Tabla_b <- Datos_temp %>%
          dplyr::select(Fecha, cont, Distrito) %>%
          filter(year(Fecha) == year_selected) %>%
          filter(Distrito %in% selected$groups) %>%
          group_by(Fecha) %>%
          summarise(sum = sum(cont))
      })
      
      
      output$grafico_temp <- renderPlotly({
        if (is.null(Datos_temp())) return(NULL)

        Sys.setlocale("LC_TIME", "Spanish")

        gg <- ggplot(Datos_temp(), aes(x = Fecha, y = sum, text = paste("Fecha:", Fecha, "<br>Solicitudes: ", sum))) +
          geom_bar(stat = "identity", aes(fill = sum)) +
          xlab("Mes") +
          ylab("Solicitudes") +
          labs(fill = 'Cantidad')

        ggplotly(gg, tooltip = "text")
      })


       output$uirender_year <- renderUI({
         selectInput(inputId = "selectedYear", label = HTML("<h4>Seleccione año</h4>"), choices = unique_years(), selected = ifelse(is.null(input$selectedYear), "2023", input$selectedYear))
       })
       
       
      ##RELACIONES 2-2
       
      TipoData <- reactive({
        dat<-(Datos %>% 
              filter(between(Fecha, input$dateRange[1], input$dateRange[2])) %>% 
              filter(Distrito %in% selected$groups) %>% 
              filter(Tipo %in% input$Tipos) %>% 
              group_by(Tema) %>%
              summarise(Cantidad = n()) %>%
              arrange(Cantidad) %>%
              mutate(Tema = factor(Tema, Tema)))
      })
      
      
      output$Tipos_ <- renderPlotly({
        ggplot(TipoData(), aes(x = reorder(Tema, -Cantidad), y = Cantidad, text = paste("Tema: ", Tema, "<br>Cantidad: ", Cantidad))) + 
          geom_bar(fill = "#2C3E50", stat = "identity") + 
          xlab("Tema") + 
          ylab("") + 
          theme(axis.text.x = element_text(angle = 25))
        
        ggplotly(tooltip = "text")  
      })
      
      
      Tabla_c <- reactive({
        (Datos %>%filter(Fecha > input$dateRange[1] & Fecha < input$dateRange[2]) %>% 
           filter(Distrito %in% selected$groups)%>% dplyr::select(Tipo,Canal))
      })
      
      
      output$comb <- renderPlotly({
        ggplot(Tabla_c()) + 
          geom_mosaic(aes(x= product(Canal,Tipo), fill = Canal)) +
          xlab("Canal") + 
          ylab("Tipo") +
          theme(axis.text.x=element_text(angle=90),axis.text.y = element_blank(),axis.ticks.y=element_blank())
      })
      
      
      ##SUBTEMAS
      
      SuggerimentsData <- reactive({
        dat<-(Datos %>% 
              filter(Fecha > input$dateRange[1] & Fecha < input$dateRange[2]) %>% 
              filter(Distrito %in% selected$groups) %>% 
              filter(Tipo %in% input$Tipus) %>% 
              group_by(Subtema) %>%
              summarise(Cantidad = n()) %>%
              arrange(Cantidad) %>%
              mutate(Subtema = factor(Subtema, Subtema)))
        le<-nrow(dat)
        dat<-dat[(le):(le-input$num+1),]
      })
      
      output$SuggerimentsTipus <- renderPlotly({
        ggplot(SuggerimentsData(), aes(x=Subtema,y=Cantidad)) + 
          geom_bar(fill="#2C3E50",stat='identity') + 
          coord_flip() +
          xlab("") + 
          ylab("Cantidad")
      })
      
      output$slider_ = renderUI({
        dat<-(Datos %>% 
              filter(Fecha > input$dateRange[1] & Fecha < input$dateRange[2]) %>% 
              filter(Distrito %in% selected$groups) %>% 
              filter(Tipo %in% input$Tipus) %>% 
              group_by(Subtema) %>%
              summarise(Cantidad = n()) %>%
              arrange(Cantidad) %>%
              mutate(Subtema = factor(Subtema, Subtema)))
        
        le<-nrow(dat)
        
        sliderInput(inputId = "num",
                    label=h3("Seleccione cantidad de subtemas"),
                    min=3,max=le,value=10,width="1000px",step=1
        )
      })
      
   }
  }
 )
}


