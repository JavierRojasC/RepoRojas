
library(shiny)
#library(shinythemes)
library(shinydashboard)
library(readxl)
library(lubridate)
library(highcharter)
library(xts)
library(dplyr)
library(shinyWidgets)
library(shinymanager)

Resultados_modelo <- read.csv2("https://raw.githubusercontent.com/JavierRojasC/JavierRCam/master/resultadosmodn.csv")

credentials <- data.frame(
    user = c("javierrcam", "oruiz","jvera"), # mandatory
    password = c("javier11213", "or2021","jv2021"), # mandatory
    start = c("2019-04-15"), # optinal (all others)
    expire = c(NA, NA, NA),
    admin = c(TRUE, FALSE,FALSE),
    comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
)

# Define UI for application that draws a histogram
ui <- navbarPage(
    div(
    div(
        id = "img-id",
        img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Nestl%C3%A9_textlogo.svg/1200px-Nestl%C3%A9_textlogo.svg.png', width='80')
    ),
    "Producción de cacao"
),
tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Montserrat:thin,extra-light,light,100,200,300,400,500,600,700,800');
      body {
        background-color: #FFFFFF;
        color: #424242;
      }
      #img-id{
      position: fixed;
      right: 10px;
      top: 10px;
    }
      h2 {
        font-family: 'Montserrat', sans-serif;
      }
       span {
        font-family: 'Montserrat', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }
                    
                    
                    "))
),
tabPanel("Modelamiento",
         
         
         box(title='Modelo predictivo de producción de cacao',width=12,highchartOutput("st", height = 550))
         
         
),
tabPanel("Análisis Exploratorio",
         selectInput("year", "Seleccione Año", c('Todos',2014,2015,2016,2017,2018,2019,2020)),
         infoBoxOutput("max", width = 4),
         infoBoxOutput("min", width = 4),
         infoBoxOutput("mean", width = 4),
         
         useShinydashboard(), # added this
         
         box(title='',width=12,highchartOutput("boxplot", height = 450))
         
)
)
ui <- secure_app(ui,background  = "linear-gradient(rgba(230, 230, 230, 0.3),rgba(177, 200, 230, 0.3))",
                 tags_bottom = tags$div(
                     tags$p(
                         "Para consultas, contáctese con el",
                         tags$a(
                             href = "mailto:maujroja@espol.edu.ec?Subject=Administrador%20Sistema%20de%20Observaciones",
                             target="_top", "administrador"
                         )
                     )
                 ),
                 theme = shinythemes::shinytheme("flatly"),
                 
                 tags_top = 
                     tags$div(
                         tags$h4("Estimaciones de producción", style = "align:center"),
                         tags$img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Nestl%C3%A9_textlogo.svg/1200px-Nestl%C3%A9_textlogo.svg.png', width='100')
                     )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials )
    )
    
    output$st <- renderHighchart({
        datos <- Resultados_modelo
        datos$Meses <- match(datos$Mes, month.abb)
        dates <- paste0(datos$Año,'-', datos$Meses)
        datos$fechas <- format(parse_date_time(dates, order=c("ym")), "%h-%Y")
        datos$date <- parse_date_time(dates, order=c("ym"))
        real <- xts(datos$`Valor.real`, order.by = as.Date(datos$date))
        ajustado <- xts(datos$`Valor.ajustado`, order.by = as.Date(datos$date))
        dif <- xts(round(datos$`X..variacion`*100,2), order.by = as.Date(datos$date))
        
        highchart(type = "stock")%>%
            hc_yAxis_multiples(create_yaxis(2, height = c(2, 2), turnopposite = TRUE)) %>% 
            hc_add_series(real, color='#28BBAD', name='Real',yAxis = 0, showInLegend=TRUE,tooltip = list(pointFormat = "Valor real: <b> {point.y} ton/ha </b> "))%>%
            hc_add_series(ajustado, color='#2A547E',name='Ajustado',yAxis = 0,tooltip = list(pointFormat = "Valor ajustado: <b> {point.y} ton/ha </b> "))%>%
            hc_add_series(dif, color='#296F94', type='area',name='Variación porcentual',yAxis = 1,tooltip = list(pointFormat = "Variación porcentual: <b> {point.y} %</b> "))%>%
            hc_legend(enabled = TRUE)%>%
            hc_title(text='')%>%
            hc_add_theme(hc_theme_gridlight())
        
        
    })
    
    output$boxplot <- renderHighchart({
        datosInicial <- Resultados_modelo
        
       
        if (input$year=="Todos"){
        datos <- datosInicial
        datos$Meses <- match(datos$Mes, month.abb)
        dates <- paste0(datos$Año,'-', datos$Meses)
        datos$fechas <- format(parse_date_time(dates, order=c("ym")), "%h-%Y")
        datos$date <- parse_date_time(dates, order=c("ym"))
        real <- xts(datos$`Valor.real`, order.by = as.Date(datos$date))
        ajustado <- xts(datos$`Valor.ajustado`, order.by = as.Date(datos$date))
        dif <- xts(round(datos$`X..variacion`*100,2), order.by = as.Date(datos$date))
        
        Means <- aggregate(Valor.real ~ as.factor(Mes), data = datos, mean)
        colnames(Means) <- c('Names', 'Mean')
        datos$Mes <- factor(datos$Mes, levels=c("Jan", "Feb" ,"Mar", "Apr" ,"May" ,"Jun" ,"Jul", "Aug", "Sep" ,"Oct" ,"Nov" ,"Dec" ))
        hcboxplot(x=datos$Valor.real, var=datos$Mes, name = "Boxplot", color = "#0E1142", outliers = TRUE,
                  showInLegend=TRUE)%>%
            hc_yAxis(title = list(text = 'ton/ha'))%>%
            hc_xAxis(title = list(text = "Levels"))%>%
            hc_chart(type = "column")%>%
            hc_plotOptions(showInLegend=TRUE,dataLabels=TRUE)%>%
            hc_add_series(Means, type='bubble', hcaes(x =Names,y=Mean),maxSize = "7%",
                          tooltip=list(pointFormat='<br> {point.y} ',headerFormat='<b> Mean'), name='Means',
                          showInLegend=TRUE)
        
        } else {
            datos <- datosInicial%>%
                filter(Año==input$year)
            
            datos$Meses <- match(datos$Mes, month.abb)
            dates <- paste0(datos$Año,'-', datos$Meses)
            datos$fechas <- format(parse_date_time(dates, order=c("ym")), "%h-%Y")
            datos$date <- parse_date_time(dates, order=c("ym"))
            real <- xts(datos$`Valor.real`, order.by = as.Date(datos$date))
            ajustado <- xts(datos$`Valor.ajustado`, order.by = as.Date(datos$date))
            dif <- xts(round(datos$`X..variacion`*100,2), order.by = as.Date(datos$date))
            
           highchart(type='stock')%>%
               hc_add_series(real, type='scatter')%>%
               hc_add_series(real, type='line', tooltip = list(pointFormat = 'Prod. cacao = {point.y} ton/ha'))
               
           
        }
        
    })
    
    output$max <- renderInfoBox({
        
        datosInicial <- Resultados_modelo
        if (input$year=="Todos"){
            datos <- datosInicial
            my_value=max(datos$Valor.real)
        }else {
        datos <- datosInicial%>%
            filter(Año==input$year)
        my_value=max(datos$Valor.real)
    }
        infoBox(title = HTML("Máximo<br>"),
                icon = icon("chevron-up"),
                value = HTML("<p style='font-size:35px'>",
                my_value,"</p>"),
                color = "navy",fill = TRUE)
    })
    output$min <- renderInfoBox({
        
        datosInicial <- Resultados_modelo
        if (input$year=="Todos"){
            datos <- datosInicial
            my_value=min(datos$Valor.real)
        }else {
            datos <- datosInicial%>%
                filter(Año==input$year)
            my_value=min(datos$Valor.real)
        }
        infoBox(title = HTML("Mínimo<br>"),
                icon = icon("chevron-down"),
                value = HTML("<p style='font-size:35px'>",
                             my_value,"</p>"),
                color = "navy",fill = TRUE)
    })
    
    output$mean <- renderInfoBox({
        
        datosInicial <- Resultados_modelo
        if (input$year=="Todos"){
            datos <- datosInicial
            my_value=mean(datos$Valor.real)
        }else {
            datos <- datosInicial%>%
                filter(Año==input$year)
            my_value=mean(datos$Valor.real)
        }
        infoBox(title = HTML("Media<br>"),
                icon = icon("dot-circle-o"),
                value = HTML("<p style='font-size:35px'>",
                             round(my_value,2),"</p>"),
                color = "navy",fill = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
