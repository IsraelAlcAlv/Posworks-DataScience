library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(ggplot2)
library(dplyr)

ui <- 
    
    fluidPage(
        
        dashboardPage(
            
            dashboardHeader(title = "Dashboard - Análisis"),
            
            dashboardSidebar(
                
                sidebarMenu(
                    menuItem("Resumen de Goles", tabName = "graph", icon = icon("bar-chart")),
                    menuItem("Probabilidades Estimadas", tabName = "img-goals", icon = icon("futbol-o")),
                    menuItem("Datos", tabName = "data_table", icon = icon("table")),
                    menuItem("Factores de Ganancia", tabName = "img", icon = icon("file-picture-o"))
                )
                
            ),
            
            dashboardBody(
                
                tabItems(
                    
                    #Gráfico de Barras
                    tabItem(tabName = "graph",
                            fluidRow(
                                titlePanel("Goles a Favor y en Contra Distribuidos por Equipos"), 
                                selectInput("x", "Seleccione el valor de X",
                                            choices = c("home.score","away.score")),
                                plotOutput("plot1", height = 550, width = 850)
                                
                           )
                    ),
                    
                    # Probabilidades Estimadas 
                        tabItem(tabName = "img-goals",
                            fluidRow(
                                titlePanel(h3("Probabilidades del N° Goles - Equipo de Casa")),
                                img( src = "PW3_1.png",  height = 400, width = 500)
                            ),
                            
                            fluidRow(
                                titlePanel(h3("Probabilidades del N° Goles - Equipo Visitante")),
                                img( src = "PW3_2.png",  height = 400, width = 500)
                                
                            ),
                            
                            fluidRow(
                                titlePanel(h3("Probabilidades conjuntas del N° Goles de goles")),
                                img( src = "PW3_3.png",  height = 400, width = 500), 
                                
                            )
                            
                    ),
                    
                    
                    #Data Table
                    tabItem(tabName = "data_table",
                            fluidRow(        
                                titlePanel(h3("Data Table")),
                                dataTableOutput ("data_table")
                            )
                    ), 
                    
                    tabItem(tabName = "img",
                            fluidRow(
                                titlePanel(h3("Factor de Ganancia Máximo")),
                                img( src = "1.png",  height = 550, width = 750)
                            ),
                            fluidRow(
                                titlePanel(h3("Factor de Ganancia Promedio")),
                                img( src = "2.png",  height = 550, width = 750)
                                
                    )
                    
                )
            )
        )
    )
)

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
    library(ggplot2)
    
    #Gráfico de Barras
     output$plot1 <- renderPlot({
        
       Datos <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv", header =  T) 
       x <- Datos[,input$x]
 
       Datos %>% ggplot()+ 
            aes(x) +
            geom_bar() +
            facet_wrap("away.team") + 
            theme_light() + 
            xlab(input$x) + ylab("Goles") +
            ylim(0,100)
     })
    
    #Data Table
    output$data_table <- renderDataTable( {Datos}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 5)
    )
    
}

shinyApp(ui = ui, server = server)
