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
      # ------------------------------------------------------------------------------------
      # MOTECARLO
      # ------------------------------------------------------------------------------------
      tabPanel("Montecarlo", 
               h2("Método Montecarlo"),
               tags$div(
                 style = "display: flex; flex-direction: column; align-items: flex-start; margin-bottom: 20px;",
                 tags$h3("Orígenes del Método"),
                 tags$p(HTML("El método de Montecarlo fue inventado por <strong>Stanislaw Ulam</strong> y <strong>John von Neumann</strong> en la década de 1940, poco después de la Segunda Guerra Mundial. Ulam tuvo la idea en 1946 mientras jugaba al solitario durante una enfermedad. Se dio cuenta de que era más fácil estimar el resultado del juego realizando múltiples pruebas aleatorias en lugar de calcular todas las combinaciones posibles. Pensó que este enfoque podía aplicarse a la <strong>difusión de neutrones</strong> en su trabajo en <strong>Los Álamos</strong>, donde las ecuaciones complejas hacían casi imposible una solución exacta.")),
                 tags$div(
                   style = "display: flex; align-items: flex-start; width: 100%;",
                   tags$div(
                     style = "margin-right: 20px; width: 150px; max-width: 150px; text-align: center;",
                     tags$img(src = "img/montecarlo01.jpg", alt = "Jon Von Neumann y Robert Oppenheimer en los Álamos", style = "width: 100%;"),
                     tags$div(style = "font-size: 0.9em; color: #555; text-align: center; margin-top: 5px; width: 100%; box-sizing: border-box;", "Jon Von Neumann y Robert Oppenheimer en los Álamos")
                   ),
                   tags$div(
                     style = "flex: 1;",
                     tags$p(HTML("Con la disponibilidad de <strong>máquinas de computación</strong>, estas podían utilizarse para realizar simulaciones numéricas y reemplazar los experimentos físicos. Durante una visita de von Neumann a Los Álamos en 1946, Ulam le mencionó el método. Aunque inicialmente escéptico, von Neumann se entusiasmó y comenzó a desarrollar el método de forma sistemática. Montecarlo empezó a tomar forma y a desarrollarse después de esta conversación."), style = "margin-bottom: 20px;"),
                     tags$p(HTML("A principios de 1947, von Neumann envió una carta a Richtmyer en Los Álamos explicando el método y sugiriendo su aplicación para rastrear la generación de neutrones en una esfera. Estimaba que el cálculo de 100 neutrones a través de 100 colisiones llevaría 5 horas con el <strong>ENIAC</strong>."), style = "margin-bottom: 20px;"),
                     tags$p(HTML("Una de las primeras aplicaciones del método a un problema determinista fue en 1948, cuando <strong>Enrico Fermi</strong>, Ulam y von Neumann lo utilizaron para calcular los <strong>valores singulares de la ecuación de Schrödinger</strong>. Ulam también estaba interesado en usar Montecarlo para evaluar integrales múltiples."), style = "margin-bottom: 20px;")
                   )
                 )
               ),
               h3("Simulación de Difusión Montecarlo"),
               tags$p(HTML("La simulación de difusión de Montecarlo es una técnica que utiliza métodos estocásticos para modelar y analizar sistemas que evolucionan en el tiempo. En esta simulación, se representan partículas que se mueven aleatoriamente en un espacio, siguiendo una caminata aleatoria o proceso de difusión. 
                 <br><br>
                 Cada partícula realiza una serie de pasos diagonales aleatorios en un plano, y se puede observar cómo se dispersan con el tiempo. Por simplicidad no hemos tenido en cuenta colisiones. Esta técnica es útil para estudiar fenómenos físicos como la difusión de gases, el movimiento browniano y otros procesos que involucran movimiento aleatorio.")),
               numericInput("num_particles", "Número de partículas:", value = 10, min = 1, max = 100),
               numericInput("num_steps_diffusion", "Número de pasos:", value = 100, min = 1, max = 100),
               actionButton("run_diffusion", "Ejecutar Simulación"),
               sliderInput("time_step", "Seleccionar instante t:", min = 1, max = 100, value = 1, step = 1, animate = TRUE),
               checkboxInput("show_trajectory", "Mostrar Trayectorias", value = TRUE),
               plotOutput("diffusion_plot")
      ),
      
      # ------------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------------

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