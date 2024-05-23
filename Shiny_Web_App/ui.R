#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggvis)

#---------------------------------------------
# SPOTIFY
spotify<-read.csv("www/Spotify.csv")

#Convertimos si es explícita o no, y el género en factores
columns_to_convert<-c(7,11)
spotify[,columns_to_convert]<-lapply(spotify[,columns_to_convert], as.factor)
genres<-sapply(spotify[11],levels)

#Pasamos a segundos la duracion
spotify[,"durations_ms"]<-spotify[,"duration_ms"]/1000

#Creamos las opciones de los ejes del gráfico
axis_vars_X <- c(
  "Popularidad" = "popularity",
  "Duración" = "duration_ms",
  "Energía" = "energy",
  "Bailabilidad" = "danceability",
  "Volúmen (dB)" = "loudness"
)

axis_vars_Y <- c(
  "Duración" = "duration_ms",
  "Popularidad" = "popularity",
  "Energía" = "energy",
  "Bailabilidad" = "danceability",
  "Volúmen (dB)" = "loudness"
)

#---------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .top-space {
        background-color: white;
        padding: 5px; /* Reducir el padding */
        display: flex;
        align-items: center;
        justify-content: space-between;
        border-bottom: 2px solid #0073b7;
        position: relative;
        z-index: 1;
      }
      .navbar-fixed-top {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 2;
      }
      .navbar-custom .navbar-nav > li > a {
        color: white; /* Cambia el color del texto de la barra de navegación */
      }
      .navbar-custom .navbar-right {
        margin-right: 10px; /* Espacio a la derecha */
      }
      body {
        padding-top: 20px; /* Espacio para la barra de navegación fija */
      }
    "))
  ),
  div(class = "top-space",
      img(src = "shiny-solo.png", height = "50px"),
      h1(style = "font-size: 40px; margin: 0;", "Trabajo final Estadística Computacional") # Ajustar tamaño del título
  ),
  div(class = "navbar-custom",
      navbarPage(
        theme = shinythemes::shinytheme("cerulean"),
        title = NULL,
        id = "navbar",
        tabPanel("Home", 
                 div(
                   h2("Objetivo del Proyecto"),
                   p("Nuestro objetivo en este proyecto es profundizar en el uso del paquete Shiny de R, una potente herramienta para la creación de aplicaciones web interactivas. A lo largo del proyecto, exploraremos diversas funcionalidades de Shiny y aprenderemos cómo podemos utilizarlo para resolver problemas específicos en el ámbito de la estadística computacional."),
                   h2("Acerca de Shiny"),
                   p("Shiny es un paquete de R que permite construir aplicaciones web interactivas directamente desde R. Con Shiny, podemos crear visualizaciones de datos, paneles de control y otras aplicaciones interactivas sin necesidad de tener conocimientos avanzados de desarrollo web."),
                   h3("Características Principales de Shiny"),
                   tags$ul(
                     tags$li("Interactividad: Permite a los usuarios interactuar con la aplicación a través de diversos controles como deslizadores, botones y cuadros de selección."),
                     tags$li("Integración con R: Shiny se integra perfectamente con R, permitiendo utilizar cualquier paquete o función de R en la aplicación."),
                     tags$li("Visualización de Datos: Podemos crear gráficos y tablas interactivas que se actualizan en tiempo real en respuesta a las entradas del usuario."),
                     tags$li("Despliegue Fácil: Las aplicaciones Shiny se pueden desplegar fácilmente en servidores Shiny, así como en otros servicios de alojamiento web.")
                   ),
                   h3("Estructura de una Aplicación Shiny"),
                   p("Una aplicación Shiny típica consta de dos componentes principales:"),
                   tags$ul(
                     tags$li("UI (Interfaz de Usuario): Define el diseño y la apariencia de la aplicación. Utiliza funciones de Shiny para crear la estructura HTML de la aplicación."),
                     tags$li("Servidor: Contiene la lógica de la aplicación. Define cómo deben reaccionar los elementos de la interfaz de usuario a las entradas del usuario y cómo deben actualizarse en consecuencia.")
                   ),
                   p("El código de una aplicación Shiny se organiza en dos archivos: `ui.R` para la interfaz de usuario y `server.R` para la lógica del servidor. También es posible tener ambos componentes en un solo archivo."),
                   h3("Ejemplo de Código"),
                   p("A continuación, se muestra un ejemplo básico de una aplicación Shiny:"),
                   pre(
                     'library(shiny)
ui <- fluidPage(
  titlePanel("Mi Primera Aplicación Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Número de intervalos:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white", xlab = "Tiempo de espera", main = "Histograma del tiempo de espera")
  })
}
shinyApp(ui = ui, server = server)'
                   ),
                 p("En este ejemplo, hemos creado una aplicación Shiny que muestra un histograma del tiempo de espera entre erupciones del géiser Old Faithful. El número de intervalos del histograma se puede ajustar mediante un control deslizante en la barra lateral."),
                 h2("Conclusión"),
                 p("A lo largo de este proyecto, exploraremos más a fondo las capacidades de Shiny y cómo podemos utilizarlo para crear aplicaciones interactivas y visualizaciones de datos efectivas. Nuestro objetivo es no solo aprender a usar Shiny, sino también aplicarlo para resolver problemas reales en el campo de la estadística computacional.")
                 )
        ),
      tabPanel("Spotify",
               sidebarLayout(
                 sidebarPanel(
                   h4("Filtros"),
                   wellPanel(sliderInput("popularity", "Popularidad mínima en Spotify:",
                                         min = 10, max = 100, value = 70
                   ),
                   sliderInput("duration", "Duración máxima de la canción (segundos):",
                               min = 15, max = 600, value = 180
                   ),
                   checkboxInput("explicit", "Explícita", FALSE),
                   selectInput("genre", "Género (a movie can have multiple genres)",
                               c("Cualquiera", genres[,1])
                   ),
                   textInput("artist", "Artista (puedes buscar por nombre parcial)"),
                   textInput("name", "Nombre de la canción"),
                   textInput("album", "Nombre del álbum")),
                   
                   wellPanel(
                     selectInput("xvar", "Variable del eje X", axis_vars_X),
                     selectInput("yvar", "Variable del eje Y", axis_vars_Y)
                   )
                 ),
                 mainPanel(
                   ggvisOutput("spotify_plot"),
                   wellPanel(
                     span("Número de canciones seleccionadas:",
                          textOutput("n_songs"))
                   )
                 )
               )
      ),
      tabPanel("Angel", "Contenido de Angel"),
      tabPanel("Javier", "Contenido de Javier"),
      tabPanel("Pablo", "Contenido de Pablo"),
      tabPanel("Valentin", "Contenido de Valentin"),
      navbarMenu("About Us",
                 tabPanel("Misión", "Información sobre nuestra misión"),
                 tabPanel("Visión", "Información sobre nuestra visión"),
                 tabPanel("Equipo", "Información sobre nuestro equipo")
      )
      )
  )
)