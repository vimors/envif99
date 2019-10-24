library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(shinydashboard)
library(plotly)

# ui object
ui <- dashboardPage(
    dashboardHeader(
        title = "Encuesta sobre violencia intrafamiliar",
        titleWidth = 400
    ),
    dashboardSidebar(
        selectInput(inputId = "selESTADO", label = "Seleccione estado",
                    choices = opciones_estado)             
    ),
    dashboardBody(
        tabsetPanel(
            id = "tabs",
            tabPanel(
                title = "Main Dashboard",
                value = "page1",
                fluidRow(
                    valueBoxOutput("total"),
                    valueBoxOutput("maltrato"),
                    valueBoxOutput("intimidacion"),
                    valueBoxOutput("fisico"),
                    valueBoxOutput("sexual")
                ),
                fluidRow(
                    leafletOutput(outputId = "mapa")                  
                    
                ),
                fluidRow(),
                fluidRow(
                    column(
                        width = 12,
                        plotlyOutput('violenciaLeer', height = "400px")
                    )                             
                )
            )
        )        
    )
)
