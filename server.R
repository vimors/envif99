library(sf)
library(leaflet)
library(dplyr)
library(foreign)
library(readxl)
library(data.table)
library(magrittr)
library(foreign)
# server()


#data <- read.csv("data/data.csv")
#map <- readOGR("data/fe_2007_39_county/fe_2007_39_county.shp")
source('global.R')

server <- function(input, output) {

    # Use a reactive() function to prepare the base
    # SQL query that all the elements in the dashboard
    # will use. The reactive() allows us to evaluate
    # the input variables
    # Total violence (server) ------------------------------------------
    output$total <- renderValueBox({
        salida_violencia <- total_con_violencia_municipio %>%
        filter(ENT == input$selESTADO) %>%
        filter(MUNICIPIO == input$mapa_shape_click$id)
        salida <- salida_violencia$pob_total %>%
            paste0(" personas")
            valueBox(
                "Poblacion total",
                salida
            )
    })

    output$maltrato <- renderValueBox({
        salida_violencia <- tipo_violencia_municipio$`Maltrato Emocional` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        salida <- salida_violencia$pob_total %>%
            paste0(" personas")
        valueBox(
            "Maltrato emocional",
            salida
        )
    })
    output$intimidacion <- renderValueBox({
        salida_violencia <- tipo_violencia_municipio$`Intimidacion` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        salida <- salida_violencia$pob_total %>%
            paste0(" personas")
        valueBox(
            "Intimidacion",
            salida
        )
    })
    output$fisico <- renderValueBox({
        salida_violencia <- tipo_violencia_municipio$`Abuso Fisico` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        salida <- salida_violencia$pob_total %>%
            paste0(" personas")
        valueBox(
            "Abuso fisico",
            salida
        )
    })
    output$sexual <- renderValueBox({
        salida_violencia <- tipo_violencia_municipio$`Abuso Sexual` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        salida <- salida_violencia$pob_total %>%
            paste0(" personas")
        valueBox(
            "Abuso sexual",
            salida
        )
    })

    # Maltrato Leer (server) -------------------------------------------
    output$violenciaLeer <- renderPlotly({

        salida_violencia_intimidacion <- tipo_violencia_leer_municipio$`Intimidacion` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        if (nrow(salida_violencia_intimidacion) > 0){
            salida_violencia_intimidacion$tipo_text <- "Intimidacion"
        }

        salida_violencia_maltrato <- tipo_violencia_leer_municipio$`Maltrato Emocional` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        if (nrow(salida_violencia_maltrato) > 0){
            salida_violencia_maltrato$tipo_text <- "Maltrato"
        }

        salida_violencia_fisico <- tipo_violencia_leer_municipio$`Abuso Fisico` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        if (nrow(salida_violencia_fisico) > 0){
            salida_violencia_fisico$tipo_text <- "Abuso Fisico"
        }

        salida_violencia_sexual <- tipo_violencia_leer_municipio$`Abuso Sexual` %>%
            filter(ENT == input$selESTADO) %>%
            filter(MUNICIPIO == input$mapa_shape_click$id)
        if (nrow(salida_violencia_sexual) > 0){
            salida_violencia_sexual$tipo_text <- "Abuso Sexual"
        }

        dataMaltratos = rbind(salida_violencia_maltrato,salida_violencia_intimidacion,  salida_violencia_fisico, salida_violencia_sexual)

        p<-ggplot(data=dataMaltratos, aes(x=SABE_LEER, y=pob_total, fill=tipo_text)) +
            geom_bar(stat="identity", position=position_dodge())
        p
    })

    # Rendereamos el texto
    output$mapa <- renderLeaflet({
        mun_estado <- mun %>%
            filter(CVE_ENT == input$selESTADO)
            
        mun_estado = mun_estado[order(mun_estado$CVE_MUN),]
        # Graficamos el mapa resultante
        
        pal <- colorBin("YlOrRd", domain = total_con_violencia_df_names$pob_total, bins = 4)
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g personas maltratadas",
            total_con_violencia_df_names$NOM_MUN, total_con_violencia_df_names$pob_total
        ) %>% lapply(htmltools::HTML)
        
        ids = total_con_violencia_df_names$MUNICIPIO
        
        
        leaflet(mun_estado) %>%
            addTiles() %>%
            addPolygons(
                layerId = ids,
                fillColor = ~pal(total_con_violencia_df_names$pob_total),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
                addLegend(pal = pal, values = total_con_violencia_df_names$pob_total, opacity = 0.7, title = NULL,
                      position = "bottomright"                
            )
    })
    
    observe({
        print("entra click")
        click <- input$mapa_shape_click
        if (is.null(click))
            return()
    })
}