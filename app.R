
# Descripciones 
library(shiny)
library(dplyr)
library(ggplot2)
library(moments)
library(cowplot)
library(DT)
library(plotly)

# Modelo
library(tseries)
library(forcats)
library(forecast)
library(stats)
library(shinyWidgets)
library(formattable)

# Mapa 
library(leaflet)
library(shinyWidgets)

# CONSUMO -----------------------------------------------------------------


  df.consumo <- read.csv('./data/consumo_agua_historico_2019.csv')
  df.consumo$fecha_referencia <- as.Date.character(df.consumo$fecha_referencia)
  df.consumo.alcaldia <- complete.cases(df.consumo$alcaldia)
  df.consumo.clean <- df.consumo[df.consumo.alcaldia,]
  df.consumo.clean <- df.consumo.clean[,-2]
  df.consumo.clean$bimestre  <- as.factor(df.consumo.clean$bimestre)
  df.consumo.clean$indice_des  <- as.factor(df.consumo.clean$indice_des)
  df.consumo.clean$alcaldia  <- as.factor(df.consumo.clean$alcaldia)

  # Función para calcular medidas de tendencia central y dispersión.

calcular_medidas <- function(dataframe, columna) {
  
  # Calcular las medidas descriptivas
  
  Max <- max(dataframe[[columna]], na.rm = TRUE)
  Min <- min(dataframe[[columna]], na.rm = TRUE)
  
  # Tendencia Central
  Media <- mean(dataframe[[columna]], na.rm = TRUE)
  Mediana <- median(dataframe[[columna]], na.rm = TRUE)
  
  # Medidas de Dispersión 
  Varianza <- var(dataframe[[columna]], na.rm = TRUE)
  Desv <- sd(dataframe[[columna]], na.rm = TRUE)
  
  RangoIQ <- IQR(dataframe[[columna]], na.rm = TRUE)
  cuartiles <- quantile(dataframe[[columna]], probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  deciles <- quantile(dataframe[[columna]], probs = c(0.2, 0.9), na.rm = TRUE)
  
  skw <- skewness(dataframe[[columna]], na.rm = TRUE)
  kts <- kurtosis(dataframe[[columna]], na.rm = TRUE)
  
  
  q.1 <- cuartiles[1]
  q.2 <- cuartiles[2]  
  q.3 <- cuartiles[3]
  d.2 <- deciles[1]
  d.9 <- deciles [2]
  
  # Crear un vector con las medidas
  medidas_vector <- round(c(Max, Min, Media, Mediana, Varianza, Desv, RangoIQ, 
                      q.1, q.2, q.3, d.2, d.9, skw, kts), digits = 2)
  
  return(medidas_vector)
}


# FUGAS -------------------------------------------------------------------

# Datos de la base de datos de reporte de fugas en Ciudad de México de 2018 a 2022.
df.fugas <- read.csv('./data/reportes_agua_hist.csv')
df.fugas <- df.fugas %>% mutate(fecha = as.Date(fecha))
df.fugas <- df.fugas %>% mutate(codigo_postal = as.character(codigo_postal))

df.fugas.alcaldia <- complete.cases(df.fugas$alcaldia)
df.fugas.clean <- df.fugas[df.fugas.alcaldia,]

df.fugas.tf <- complete.cases(df.fugas.clean$tipo_de_falla)
df.fugas.clean <- df.fugas.clean[df.fugas.tf,]

df.fugas.clean$tipo_de_falla  <- as.factor(df.fugas.clean$tipo_de_falla)
df.fugas.clean$quien_atiende  <- as.factor(df.fugas.clean$quien_atiende)
df.fugas.clean$alcaldia  <- as.factor(df.fugas.clean$alcaldia)



# MODELO ------------------------------------------------------------------

  
  options(scipen=999)
  
  #Redireccionar la dirección del archivo donde se guardo la base de datos
  df_agua<-read.csv("./data/reportes_agua_hist.csv")
  
  df_agua<- df_agua %>% select(fecha) %>% group_by(fecha ) %>% summarise(reportes=n())
  df_agua$fecha<- as.Date(df_agua$fecha)
  
  serie_comprobadora<-df_agua %>% filter(fecha>="2021-12-01")
  
  #Filtramos la fecha
  agua_serie<-df_agua %>% filter(fecha<"2021-12-01")
  
  #se crea la serie de tiempo
  serie_agua<-ts(agua_serie,frequency=365, start=c(2018, 1))
  decompose(serie_agua[,2]) %>% plot()
  
  d1_agua<-diff(serie_agua[,2])
  
  #ARIMA(p,d,q)
  #dado estos valores podemos decir que se plantean los siguientes modelos:
  #Modelo 1 (2,1,2)
  m1<-arima(serie_agua[,2],order = c(2,1,2))
  bx_m1<-Box.test(residuals(m1),type="Ljung-Box")
  s1<-shapiro.test(m1$residuals)
  
  #Modelo 2 (3,1,2)
  m2<-arima(serie_agua[,2],order = c(3,1,2))
  m2
  bx_m2<-Box.test(residuals(m2),type="Ljung-Box")
  s2<-shapiro.test(m2$residuals)
  
  #Modelo 3 (4,1,2)
  m3<-arima(serie_agua[,2],order = c(4,1,2))
  bx_m3<-Box.test(residuals(m3),type="Ljung-Box")
  s3<-shapiro.test(m3$residuals)
  
  #Modelo 4 (5,1,2)
  m4<-arima(serie_agua[,2],order = c(5,1,2))
  m4
  bx_m4<-Box.test(residuals(m4),type="Ljung-Box")
  s4<-shapiro.test(m4$residuals)
  
  #Modelo 5 (2,1,3)
  m5<-arima(serie_agua[,2],order = c(2,1,3))
  m5
  bx_m5<-Box.test(residuals(m5),type="Ljung-Box")
  s5<-shapiro.test(m5$residuals)
  
  #Modelo 6 (3,1,3)
  m6<-arima(serie_agua[,2],order = c(3,1,3))
  bx_m6<-Box.test(residuals(m6),type="Ljung-Box")
  s6<-shapiro.test(m6$residuals)
  
  #Modelo 7 (4,1,3)
  m7<-arima(serie_agua[,2],order = c(4,1,3))
  bx_m7<-Box.test(residuals(m7),type="Ljung-Box")
  s7<-shapiro.test(m7$residuals)
  
  #Modelo 8 (5,1,3)
  m8<-arima(serie_agua[,2],order = c(5,1,3))
  bx_m8<-Box.test(residuals(m8),type="Ljung-Box")
  s8<-shapiro.test(m8$residuals)
  
  #Pronostico
  pronostico<-forecast(m4, h=31, level = 90)
  
  
  # Mapa 
  agua_cdmx<-read.csv("./data/reportes_agua_hist.csv")
  
  delegaciones<-list(as.data.frame(table(agua_cdmx$alcaldia))[1])[[1]]
  



# SHINY -------------------------------------------------------------------



ui <- fluidPage(
  
  navbarPage("Agua CDMX",
                           
  tabPanel('Consumo de Agua',
           
   titlePanel("Consumo de Agua en m3"),
    
      sidebarLayout(
        sidebarPanel(
          selectInput("alcaldia", label = 'Alcaldía',
                    choices = list("ALVARO OBREGON","AZCAPOTZALCO",          
                                    "BENITO JUAREZ", "COYOACAN",              
                                    "CUAJIMALPA DE MORELOS", 'CUAUHTEMOC',            
                                    "GUSTAVO A. MADERO", "IZTACALCO",             
                                    "IZTAPALAPA", "LA MAGDALENA CONTRERAS",
                                    "MIGUEL HIDALGO", "MILPA ALTA",            
                                    "TLAHUAC", "TLALPAN",               
                                    "VENUSTIANO CARRANZA", "XOCHIMILCO"),
                    selected = "ALVARO OBREGON"),
          
          selectInput("consumo", label = 'Tipo de Consumo',
                      choices = list('consumo_prom_mixto', 'consumo_total_mixto',
                                      'consumo_prom_dom', 'consumo_total_dom',
                                      'consumo_prom_no_dom', 'consumo_total_no_dom', 
                                      'consumo_prom', 'consumo_total')
                       ),
          
          p("Nota: Consumo Mixto: "),
          
          numericInput("outliers", "Outliers:", 50, min = 50, max = 120000),
          
          h4('Objetivo'),
          
          p("El consumo general de agua en México es uno de las preocupaciones
            más grandes de los últimos años debido a la creciente escasez. 
            Se han registrado cientos de casos donde las demandas de agua potable 
            han aumentado debido al incremento de viviendas, población y la falta de 
            mantenimiento en el servicio de tuberías, lo que ha causado reportes de 
            fallas de agua."),

          p("Nuestro trabajo pretende analizar datos del consumo del agua en las alcaldías de la 
            Ciudad de México obtenidos de SACMEX, comparando los resultados de los tres primeros 
            bimestres de 2019."),
            
          p("También queremos analizar los registros de reportes de fugas de agua, 
            mala calidad y falta de agua en la Ciudad de México, obtenidos de SACMEX a partir de 
            2018 con un modelo de series de tiempo."),

          p("El objetivo del proyecto es poder evaluar y predecir la cantidad de fugas de agua
            para comprender a través de los reportes el incremento en demandas de agua.")
            ),
        
          mainPanel(
            
            tabsetPanel(
              
              tabPanel("General", plotlyOutput("GraficosGeneral"),
                       p("Por Alcaldía, las tablas de frecuencia muestran un comportamiento similar 
                          en cada bimestre."), 
                        p("El sector con índice de desarrollo bajo presenta mayor consumo por metro cúbico."),
                       
                       DT::dataTableOutput("TablaGeneral")), 
              
              tabPanel("Gráficos", fluidRow(plotlyOutput("grafico1")),
                                    
                                    fluidRow(plotlyOutput("grafico2")),
                                    
                                    fluidRow(plotlyOutput("grafico3"))), 
              
              tabPanel("Medidas", DT::dataTableOutput("Tabla")),
              
              tabPanel("Frecuencias", plotlyOutput("Graficos2"),
                       DT::dataTableOutput("Tabla2")) 
              
            )
          )
        )
      ),
    
      tabPanel('Reportes de Agua',
               
               titlePanel("Reportes de Fallas, 2018-2022"),
               
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("alcaldiatf", label = 'Alcaldía',
                                        choices = list("ALVARO OBREGON","AZCAPOTZALCO",          
                                                       "BENITO JUAREZ", "COYOACAN",              
                                                       "CUAJIMALPA DE MORELOS", 'CUAUHTEMOC',            
                                                       "GUSTAVO A. MADERO", "IZTACALCO",             
                                                       "IZTAPALAPA", "LA MAGDALENA CONTRERAS",
                                                       "MIGUEL HIDALGO", "MILPA ALTA",            
                                                       "TLAHUAC", "TLALPAN",               
                                                       "VENUSTIANO CARRANZA", "XOCHIMILCO"),
                                        selected = "ALVARO OBREGON"),
                            
                            selectInput("falla", label = 'Tipo de Falla',
                                        choices = list("Falta de agua", "Fuga", "Mala calidad")
                            ),
                            
                            numericInput("outliers_2", "Outliers:", 100, min = 0, max = 120000)
                            
                          ),
                          
                          mainPanel(
                            
                            tabsetPanel(
                              tabPanel("General", plotlyOutput("GraficosGeneraltf"),
                                       DT::dataTableOutput("TablaGeneraltf")), 
                              
                              tabPanel("Gráficos", fluidRow(plotlyOutput("grafico1tf")),
                                       
                                       fluidRow(plotlyOutput("grafico2tf")),
                                       
                                       fluidRow(plotlyOutput("grafico3tf")),
                                       
                                       fluidRow(plotlyOutput("grafico4tf"))), 
                              
                              tabPanel("Medidas", DT::dataTableOutput("Tablatf")),
                              
                              tabPanel("Frecuencias", plotlyOutput("Graficos2tf"),
                                       DT::dataTableOutput("Tabla2tf")) 
       
                            )
                            
                          )
                        )
               
      ),

      tabPanel('Modelo',
                   
                   # Application title
                   
                   h1("Modelo ARIMA",align="center"),
                   HTML("<center><h2><br>En el análisis de series de tiempo, los modelos ARIMA (Autoregressive Integrated Moving Average)</br> 
         son usados para realizar pronosticos a partir de los mismos datos de la serie.</h2></center>"),
                   br(),
                   
                   h2("PRONOSTICO DE REPORTES",style = "color:#FDFEFE;background-color:#21618C",align = "center"),
                   
                   HTML("<center><h3><br>A partir de nuestra base de datos procederemos a realizar un pronostico sobre los reportes de fallas en el Sistema de Aguas generados por los usuarios.</h3></center>"),
                   fluidRow(column( HTML("<center><h4><br>Nuestra serie se compone de 254,730  registros que fueron hechos de manera diaria, abarcando desde el 01 de enero de 2018 al 31 de diciembre del 2018</h4></center>"),
                                    HTML("<center><h4><br>Para nuestro análisis requerimos agrupar por día los reportes hechos, lo que nos generara una Serie de Tiempo con registros diarios. Una vez agrupados, procederemos a filtrar el ultimo mes, asi, nuestra serie para relizar el modelo
                           abarcara hasta el 30 de noviembre de 2021, como podemos visualizar en el gráfico siguiente una vez agrupados los registros:</h4></center>"),
                                    width=4),  
                            column(plotlyOutput("grf_ser"),width=8)
                   ),
                   HTML("<center><h4><br>Para plantear el modelo ARIMA, es encontrar los valores que daran los parametros para estimar los cuales son (p,d,q), que respectivamente son los autorregresivos, el valor integrado y la(s) media movil</h4></center>"),
                   HTML("<center><h4><br>El primer paso es definir si nuestra serie es estacionaria, con la prueba Dickey-Fuller, siguiendo la siguiente regla: </br>
                      <br>Si  p-valor > 0.05: Serie No Estacionaria </br>
                      <br>Si  p-valor < 0.05: Serie Estacionaria </br>
         </h4></center>"),
                   
                   
                   HTML("<center><h4><br>Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un</br>
    <br>p-valor = 0.01. </br>
    <br>Como el p-valor < 0.05.</br>
    <br>Por lo tanto,la serie es estacionaria. </br>"),
                   
                   br(),
                   
                   fluidRow(column(HTML("<center><h4><br>Realizando la prueba de autocorrelación podemos ver que la serie no tiene una caida exponencial</br></h4></center>"),
                                   width=4), 
                            column(plotOutput("graf1"),width=8)
                   ),
                   
                   fluidRow(column(HTML("<center><h4><br>Aunque la serie presenta de origen estacionariedad, probamos corriendo la función ndiffs(), para verificar si es necesario aplicar alguna diferencia al momento de correr el modelo.
                                 La función ndiffs nos muestra que la serie debería tener 1 diferencia</br></h4></center>"),
                                   HTML("<center><h4><br>A partir de este grafico podemos ver que ya se crea un cambio exponencial en la autocorrelación</br></h4></center>"),
                                   HTML("<center><h4><br>Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.01, comprobando que es estacionaria</br>"),
                                   width=4), 
                            column(plotOutput("graf2"),width=8)
                   ),
                   
                   HTML("<center><h3><br>El valor a usar en el parametro d sera igual a 1</br></h3></center>"),
                   
                   fluidRow(column(HTML("<center><h4><br>Para encontrar los valores que ocuparan los parametros para p y q del Modelo, aplicaremos las pruebas de autocorrelación y autocorrelación parcial, 
                          esta prueba la aplicaremos a la serie generada con una diferencia</br></h4></center>"),
                                   HTML("<center><h4><br>Dado el grafico obtenido tenemos que los lags posibles son para p: </br>
                                           p=2,3,4,5</br></h4></center>"),
                                   width=4),
                            
                            column(plotOutput("graf3"),width=8)
                            
                   ),
                   
                   
                   fluidRow(column(HTML("<center><h4><br>Como vemos en el grafico el lag 2 es el que sobre sale, asi como el lag 3, por ende probaremos q = 2  </br></h4></center>"),
                                   width=4),
                            column(plotOutput("graf4"),width=8)
                   ),
                   br(),
                   br(),
                   fluidRow(column(HTML("<center><h4><br> La tabla nos muestra los valores AIC, y el p_value de la prueba Ljung-Box.
                      Donde el modelo 4 y 8 presenta los valores mas pequeños para el AIC, pero si revisamos los residuales de donde nace el P_value de la prueba Ljunk Box, 
                      vemos que en el modelo 8 el p-value es menor a 0.05 por lo que se muestra que existe autocorrelación en los residuos y se descarta este Modelo.
                      <br>Dado esto nos quedamos con el Modelo 4</br>
                      <br>Para validar si los residuos se distribuyen de forma normal en el modelo seleccionado, se aplica el test de normalidad Shapiro-Wilk, que es una prueba estadística utilizada para evaluar si una muestra de datos sigue una distribución normal.
                      Las hipótesis nula y alternativa del test de Shapiro-Wilk son las siguientes:
                      <br>H0: Los residuos siguen una distribución normal. </br>
                      <br>H1: Los residuos NO siguen una distribución normal.</br>
                      <br>El p-value es menor que un nivel de significancia de 0.05, se puede rechazar la hipótesis nula y concluir que la muestra no sigue una distribución normal.</br></h4></center>")
                                   , width=4),
                            column(DT::dataTableOutput("tab_models"),width=8)
                   ),
                   
                   fluidRow(column(HTML("<center><h4><br>Al analizar los residuales del Modelo 4 observamos que los valores son normales ya que descansan en una línea y no están por todos lados</br></h4></center>")
                                   , width=4),
                            
                            column(plotOutput("graf5"),width=8)
                   ),
                   
                   
                   fluidRow(HTML("<center><h4><br> Una vez elegido el mejor modelo procedemos a crear nuestro pronostico.</br>
                  <br>Proyección de Modelo ARIMA(5,1,2) al 90% de exactitud</br></h4></center>")),
                   
                   plotlyOutput("graf6")
                   
                   
                   
                   ),
  
          tabPanel('Mapa', 
                   fluidRow(
                     column(pickerInput(inputId = "alcadia_c",label = "Alcaldia:",choices = delegaciones),width = 4,align="center"),
                     column(uiOutput("colonias_final"),
                            h5("Solo se pueden seleccionar 8 colonias al mismo tiempo."),width = 4,align="center"),
                     column(uiOutput("fallas_final"),width = 4,align="center")
                     
                   ),
                   leafletOutput("map_final",height = 790)
                   ),


          tabPanel('Conclusiones',
                   
                   h1("Conclusiones",align="center"),
                   
                   fluidRow(HTML("<center>Podemos observar que en el mes de prueba de Diciembre 2021, la línea roja representa los datos reales de la base de datos de SACMEX, mientras que el modelo sigue la línea azul del modelo ARIMA(5,1,2), y las líneas verdes representan una banda de ajuste del pronóstico. 
                      <br>Creemos que el resultado es bastante bueno para predecir fallas en el sistema de agua de la Ciudad de México y para saber cuándo podemos esperar un aumento en el número de reportes.</br>
                      
                      <br>Consideramos de suma importancia seguir analizando estos temas, debido a la crisis hídrica que sufre actualmente la CDMX y que al discutir sobre este, se pone realce a la importancia de llevar a cabo acciones necesarias en el mantenimiento de la red de agua potable, y de esta forma garantizar el adecuado suministro para toda la población del Valle de México.</br></center>")),
                   
                   h1("Integrantes",align="center"),
                   
                   fluidRow(HTML("<center> <br>Diego Rosales</br> 
                                 <br>Jorge Limas</br>
                                 <br>Pedro García</br>
                                 <br>Alexis Leal</br>
                                 <br>Taryn Galindo</br>
                                 <br>Carlos Guerra</br><center>")
                            )
        )
  )
)

server <- function(input, output) {
  
      ### Consumo
            
        base.al <- reactive({
          df.consumo.clean %>%
          filter(alcaldia == input$alcaldia) %>%
          filter(across(all_of(input$consumo)) < input$outliers) %>%
          na.omit() 
         
      })
  
        output$grafico1 <- renderPlotly({
          aa1 <- ggplot(base.al(), aes_string(input$consumo, fill="indice_des")) +
            geom_histogram(bins = 10) +
            labs(title = "Histograma", 
                 x = "Consumo",
                 y = "Frequency") + 
            theme_classic()
          
            ggplotly(aa1)
    
        })
        
        output$grafico2 <- renderPlotly({
          aa2 <- ggplot(base.al(), aes_string(input$consumo, fill="indice_des")) +
            geom_boxplot(varwidth = TRUE, alpha=0.2) +
            #facet_wrap(~indice_des) +
            labs(title = "Boxplot", 
                 x = "Consumo",
                 y = "Frequency")+ 
            theme_classic()
          
          # Convertir a plotly
          p <- ggplotly(aa2)
          
          # Obtener el rango del eje y
          min_value <- -1
          max_value <- 4
          
          # Ajustar el rango del eje y manualmente
          p <- p %>% layout(yaxis = list(range = c(min_value, max_value)))
          
          return(p)

        })
        
        output$grafico3 <- renderPlotly({
          aa3 <- ggplot(base.al(), aes_string(input$consumo, fill="indice_des")) +
            geom_density() + 
            facet_wrap(~indice_des) +
            guides(fill = guide_legend(title = "Indice Desarrollo"))+
            labs(title = "Densidad", 
                 x = "Consumo",
                 y = "Frequency")+ 
            theme_classic()
          
          ggplotly(aa3)
        })
        
      
      medidas <- reactive({
                  
        base1 <- df.consumo.clean %>%
                  filter(alcaldia == input$alcaldia) %>%
                  filter(indice_des == 'ALTO')
          
        df1 <- data.frame(ALTO = calcular_medidas(base1, input$consumo), 
                                 row.names = c('Máximo', 'Mínimo',
                                               'Media', 'Mediana', 'Varianza', 
                                               'Desviación Estándar', 'Rango IQ',
                                               '1q', '2q', '3q', '2d', '9d',
                                               "Skewness", 'Curtosis'))
        
        base2 <- df.consumo.clean %>%
          filter(alcaldia == input$alcaldia) %>%
          filter(indice_des == 'MEDIO')
        
        df2 <- data.frame(MEDIO = calcular_medidas(base2, input$consumo), 
                         row.names = c('Máximo', 'Mínimo',
                                       'Media', 'Mediana', 'Varianza', 
                                       'Desviación Estándar', 'Rango IQ',
                                       '1q', '2q', '3q', '2d', '9d',
                                       "Skewness", 'Curtosis'))
        
        base3 <- df.consumo.clean %>%
          filter(alcaldia == input$alcaldia) %>%
          filter(indice_des == 'BAJO')
        
        df3 <- data.frame(BAJO = calcular_medidas(base3, input$consumo), 
                         row.names = c('Máximo', 'Mínimo',
                                       'Media', 'Mediana', 'Varianza', 
                                       'Desviación Estándar', 'Rango IQ',
                                       '1q', '2q', '3q', '2d', '9d',
                                       "Skewness", 'Curtosis'))
        
        base4 <- df.consumo.clean %>%
          filter(alcaldia == input$alcaldia) %>%
          filter(indice_des == 'POPULAR')
        
        
        
        df4 <- data.frame(POPULAR = calcular_medidas(base4, input$consumo), 
                         row.names = c('Máximo', 'Mínimo',
                                       'Media', 'Mediana', 'Varianza', 
                                       'Desviación Estándar', 'Rango IQ',
                                       '1q', '2q', '3q', '2d', '9d',
                                       "Skewness", 'Curtosis'))
        
        cbind(df1, df2, df3, df4)

      })
      
      output$Tabla <- DT::renderDataTable({
            datos <- medidas()  
            datatable(datos, options = list(pageLength = -1, dom = "t")) 
         
        })
      
      # Gráfico de Frecuencias absolutas
      output$Graficos2 <- renderPlotly({
        graf_id <- ggplot(base.al(), aes_string('indice_des', fill= 'indice_des')) +
            geom_bar() +
            facet_wrap(~bimestre)+
            labs(title = "Consumo en m3 de agua", 
                 subtitle = "Por alcaldía e índice de desarrollo. 2019",
                 x = "Indice de Desarrollo",
                 y = "Frecuencia") +
            theme_minimal() +
            theme(axis.text.x  = element_text(angle = 90))+
            coord_flip()
          
          graf_id
            
      })
      
      # Tabla de Frecuencias Absolutas, Relativas
      freq_id <- reactive({
        
        df.consumo.clean %>%
          filter(alcaldia == input$alcaldia) %>%
          group_by(alcaldia, indice_des) %>%
          summarise(n = n()) %>%
          mutate(prop =  paste(round(100*prop.table(n), 2), "%", sep=""))
        
      })
      
      output$Tabla2 <- DT::renderDataTable({
        freq_id() 
        
      })
      
      # Gráfico de Frecuencias absolutas
      output$GraficosGeneral <- renderPlotly({
        graf_id <- df.consumo.clean %>%
          ggplot(aes(x = indice_des, fill = alcaldia)) +
          geom_bar() +
          facet_wrap(~bimestre)+
          labs(title = "Registros de consumo total en metros cúbicos", 
               subtitle = "Por alcaldía e índice de desarrollo. 2019",
               x = "Indice de Desarrollo",
               y = "Frecuencia") +
          theme_minimal() +
          theme(axis.text.x  = element_text(angle = 90))
        
        graf_id
        
      })
      
      # Tabla de Frecuencias Absolutas, Relativas
      freq_g <- reactive({
        
        df.consumo.clean %>%
          group_by(alcaldia, bimestre, indice_des) %>%
          summarise(n = n()) %>%
          mutate(prop =  paste(round(100*prop.table(n), 2), "%", sep="")) 
        
      })
      
      output$TablaGeneral <- DT::renderDataTable({
        freq_g() 
        
      })
      
      
      ### FUGAS
      
      base.tf <- reactive({
        
        df.fugas.clean %>% 
          select(fecha, alcaldia, folio, tipo_de_falla) %>%
          filter(alcaldia == input$alcaldiatf, tipo_de_falla == input$falla) %>%
          group_by(fecha) %>%
          summarise(Reportes = n()) %>%
          filter(Reportes < input$outliers_2)
        
        
      })
      
      output$grafico1tf <- renderPlotly({
        aa1tf <- ggplot(base.tf(), aes(Reportes)) +
          geom_histogram(bins = 10, fill="lightblue", colour="#000000") +
          labs(title = "Histograma", 
               x = "Reportes de Fallas",
               y = "Frequency") + 
          theme_classic()
        
        
        ggplotly(aa1tf)
        
      })
      
      output$grafico2tf <- renderPlotly({
        aa2tf <- ggplot(base.tf(), aes(Reportes)) +
          geom_boxplot(varwidth = TRUE, alpha=0.2, fill="lightblue", colour="#000000") +
          #facet_wrap(~indice_des) +
          labs(title = "Reportes de Falla", 
               x = "Consumo",
               y = "Frequency")+ 
          theme_classic()
        
        
        # Convertir a plotly
        p <- ggplotly(aa2tf)
        
        # Obtener el rango del eje y
        min_value <- -1
        max_value <- 4
        
        # Ajustar el rango del eje y manualmente
        p <- p %>% layout(yaxis = list(range = c(min_value, max_value)))
        
        return(p)
        
      })
      
      output$grafico3tf <- renderPlotly({
        aa3tf <- ggplot(base.tf(), aes(Reportes)) +
          geom_density(fill="lightblue", colour="#000000") + 
          guides(fill = guide_legend(title = "Reportes de Fallas"))+
          labs(title = "Densidad", 
               x = "Reportes de Fallas",
               y = "Frequency")+ 
          theme_classic()
        
        ggplotly(aa3tf)
      })
      
      output$grafico4tf <- renderPlotly({
        aa4tf <- ggplot(base.tf(), aes(x= fecha, y=Reportes)) +
          geom_line() + 
          guides(fill = guide_legend(title = "Reportes de Fallas"))+
          labs(title = "Serie de Tiempo", 
               x = "Tiempo: días",
               y = "Reportes de Fallas")+ 
          theme_classic()
        
        ggplotly(aa4tf)
      })
      
      
      
      medidastf <- reactive({
        
        base1tf <- df.fugas.clean %>%
          filter(alcaldia == input$alcaldiatf) %>%
          filter(tipo_de_falla == "Falta de agua") %>%
          group_by(fecha) %>%
          summarise(Reportes = n())
        
        df1tf <- data.frame(Falta_de_Agua = calcular_medidas(base1tf, 'Reportes')) 
        row.names = c('Máximo', 'Mínimo',
                      'Media', 'Mediana', 'Varianza', 
                      'Desviación Estándar', 'Rango IQ',
                      '1q', '2q', '3q', '2d', '9d',
                      "Skewness", 'Curtosis')
        
        base2tf <- df.fugas.clean %>%
          #select(fecha, alcaldia, folio, tipo_de_falla) %>%
          filter(alcaldia == input$alcaldiatf) %>%
          filter(tipo_de_falla == 'Fuga') %>%
          group_by(fecha) %>%
          summarise(Reportes = n())
        
        df2tf <- data.frame(Fugas = calcular_medidas(base2tf, 'Reportes'), 
                            row.names = c('Máximo', 'Mínimo',
                                          'Media', 'Mediana', 'Varianza', 
                                          'Desviación Estándar', 'Rango IQ',
                                          '1q', '2q', '3q', '2d', '9d',
                                          "Skewness", 'Curtosis'))
        
        base3tf <- df.fugas.clean %>%
          filter(alcaldia == input$alcaldiatf) %>%
          filter(tipo_de_falla == "Mala calidad")%>%
          group_by(fecha) %>%
          summarise(Reportes = n())
        
        df3tf <- data.frame(Mala_Calidad = calcular_medidas(base3tf, 'Reportes'), 
                            row.names = c('Máximo', 'Mínimo',
                                          'Media', 'Mediana', 'Varianza', 
                                          'Desviación Estándar', 'Rango IQ',
                                          '1q', '2q', '3q', '2d', '9d',
                                          "Skewness", 'Curtosis'))
        
        
        cbind(df1tf, df2tf, df3tf)
        
      })
      
      output$Tablatf <- DT::renderDataTable({
        datos <- medidastf()  
        datatable(datos, options = list(pageLength = -1, dom = "t")) 
        
      })
      
      # Gráfico de Frecuencias absolutas
      output$Graficos2tf <- renderPlotly({
        
        graf_2tf<- df.fugas.clean %>%
          filter(alcaldia == input$alcaldiatf)%>%
          ggplot( aes(x = tipo_de_falla)) +
          geom_bar(fill="lightgreen", colour="#000000") +
          labs(title = "Reportes por Tipo de Falla", 
               subtitle = "Por alcaldía. 2018-2022",
               x = "Alcaldia",
               y = "Frecuencia") +
          theme_minimal() 
        
        graf_2tf
        
      })
      
      # Tabla de Frecuencias Absolutas, Relativas
      freq_tf <- reactive({
        df.fugas.clean %>%
          filter(alcaldia == input$alcaldiatf)%>%
          group_by(alcaldia, tipo_de_falla) %>%
          summarise(n = n()) %>%
          mutate(prop =  paste(round(100*prop.table(n), 2), "%", sep="")) 
        
      })
      
      output$Tabla2tf <- DT::renderDataTable({
        freq_tf() 
        
      })
      
      
      
      # Tabla de Frecuencias Absolutas, Relativas
      freq_gtf <- reactive({
        
        
        df.fugas.clean %>%
          group_by(alcaldia, tipo_de_falla) %>%
          summarise(n = n()) %>%
          mutate(prop =  paste(round(100*prop.table(n), 2), "%", sep="")) 
        
      })
      
      output$TablaGeneraltf <- DT::renderDataTable({freq_gtf()})
      
      # Gráfico de Frecuencias absolutas
      output$GraficosGeneraltf <- renderPlotly({
        graf_tf <- df.fugas.clean %>%
          ggplot(aes(x = alcaldia, fill = tipo_de_falla)) +
          geom_bar() +
          #facet_wrap(~tipo_de_falla)+
          labs(title = "Reportes por Tipo de Falla", 
               subtitle = "Por alcaldía. 2018-2022",
               x = "Alcaldia",
               y = "Frecuencia") +
          theme_minimal()+
          theme(axis.text.x  = element_text(angle = 90))
        
        graf_tf
        
      })
      
      
      
      ### MODELO
      
      output$grf_ser<-renderPlotly( ggplotly( ggplot(df_agua, aes(x=fecha,y = reportes)) +
                                                geom_line() 
      )
      )
      
      output$graf1<-renderPlot(autoplot(acf(serie_agua[,2]) ) )
      
      output$graf2<-renderPlot(autoplot(acf(diff(serie_agua[,2]))))
      
      output$graf3<-renderPlot(autoplot(pacf(d1_agua)))
      output$graf4<-renderPlot(autoplot(acf(d1_agua)))
      
      
      output$tab_models<-DT::renderDataTable (
        DT::datatable(data.frame("ARIMA"=c("Modelo 1 (2,1,2)","Modelo 2 (3,1,2)","Modelo 3 (4,1,2)","Modelo 4 (5,1,2)","Modelo 5 (2,1,3)","Modelo 6 (3,1,3)","Modelo 7 (4,1,3)","Modelo 8 (5,1,3)"),
                                 "AIC"=c(round(m1$aic,2),round(m2$aic,2),round(m3$aic,2),round(m4$aic,2),round(m5$aic,2),round(m6$aic,2),round(m7$aic,2),round(m8$aic,2)),
                                 "p_value_Ljunk_Box"=c(round(bx_m1$p.value,4),round(bx_m2$p.value,4),round(bx_m3$p.value,4),round(bx_m4$p.value,4),round(bx_m5$p.value,4),round(bx_m6$p.value,6),round(bx_m7$p.value,4),round(bx_m8$p.value,4))) %>%
                        mutate("p_value_Ljunk_Box>0.05"=ifelse(p_value_Ljunk_Box >0.05,"VERDADERO" ,"FALSO"))%>%
                        mutate("p_value_Shapiro_Wilk"=c(s1$p.value,s2$p.value,s3$p.value,s4$p.value,s5$p.value,s6$p.value,s7$p.value,s8$p.value ))%>% 
                        mutate("p_value_Shapiro_Wilk>0.05"=ifelse(p_value_Shapiro_Wilk < 0.05,"VERDADERO" ,"FALSO"))
        )
      )
      
      output$graf5<-renderPlot( ggplot(residuals(m4), aes(sample = residuals(m4))) +
                                  geom_qq() +
                                  geom_qq_line(slope = 1, intercept = 0, color =
                                                 "red", linetype = "dashed"))
      
      output$graf6<-renderPlotly(ggplotly(autoplot(pronostico$mean, col="blue")+
                                            geom_line(aes(y=pronostico$mean),col="blue")+geom_point(aes(y=pronostico$mean),col="blue")+
                                            geom_line(aes(y=pronostico$upper),col="green")+ geom_point(aes(y=pronostico$upper))+
                                            geom_line(aes(y=pronostico$lower),col="green")+ geom_point(aes(y=pronostico$lower))+
                                            geom_line(aes(y=serie_comprobadora$reportes),col="red")+ geom_point(aes(y=serie_comprobadora$reportes))
                                          
      
      )
    )
      
      
      # Mapa 
      
      lista_colonias<-reactive({
        l1<-agua_cdmx %>% 
          filter(alcaldia %in% input$alcadia_c) %>% 
          select(colonia_datos_abiertos) %>% 
          group_by(colonia_datos_abiertos) %>% 
          summarise(n=n()) %>% 
          select(colonia_datos_abiertos) %>% 
          arrange(colonia_datos_abiertos)
        l1<-as.list(l1[1])[[1]]
      })
      
      
      
      lista_fallas<-reactive({
        l1<-agua_cdmx %>% 
          filter(alcaldia %in% input$alcadia_c) %>% 
          filter(colonia_datos_abiertos %in% input$colonias_c)%>% 
          select(tipo_de_falla) %>% 
          group_by(tipo_de_falla) %>% 
          summarise(n=n()) %>% 
          select(tipo_de_falla) %>% 
          arrange(tipo_de_falla)
        l1<-as.list(l1[1])[[1]]
      })
      
      
      data_map<-reactive({ b1<-agua_cdmx %>%
        filter(alcaldia %in% input$alcadia_c) %>% 
        filter(colonia_datos_abiertos %in% input$colonias_c)%>% 
        filter(tipo_de_falla %in% input$falla_c) %>%
        select(latitud, longitud,folio,tipo_de_falla ) %>% 
        mutate(info=paste("<a>Folio: <b>",folio,"</b></a><br>","Falla: <b>",tipo_de_falla,"</b>",sep="")) %>%
        select(-folio)
      
      })
      
      
      map_leaf<-reactive({
        
        icons <- awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion',
          markerColor = ifelse(data_map()$tipo_de_falla=="Falta de agua","red",ifelse(data_map()$tipo_de_falla=="Fuga","orange","green"))
        )
        
        leaflet(data_map()) %>%
          addTiles()%>%
          setView(lng =  -99.13873 ,lat =19.43707, zoom = 12) %>%
          addAwesomeMarkers(lng = ~longitud,lat=~latitud,popup = ~info, icon=icons)
        
      })
      
      
      output$colonias_final<-renderUI(pickerInput(inputId = "colonias_c",label = "Colonia",choices = lista_colonias(),selected = lista_colonias()[1],multiple = T ,options = pickerOptions(liveSearch = T,selectedTextFormat =  "count > 2",maxOptions = 8 )))
      output$fallas_final<-renderUI(pickerInput(inputId = "falla_c",label = "Tipo de Falla:",choices = lista_fallas(),selected =lista_fallas() , multiple = T ))
      
      output$map_final<-renderLeaflet(map_leaf())
      
  }


shinyApp(ui = ui, server = server)
