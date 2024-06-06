# Carga de librerías
library(shiny)
library(ggvis)
library(shiny.molstar)

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
               h2("Spotify: La revolución musical"),
               tags$div(
                 style = "display: flex; flex-direction: column; align-items: flex-start; margin-bottom: 20px;",
                 tags$h3("¿Qué es Spotify?"),
                 tags$p(HTML("<strong>Spotify</strong> es una empresa sueca de servicios multimedia fundada en 2006, cuyo producto es la aplicación homónima empleada para la reproducción de música vía streaming. Su modelo de negocio es el denominado freemium. ")),
                   tags$div(
                     style = "flex: 1;",
                     tags$p(HTML("Spotify ofrece música grabada y podcasts digitales restringidos por derechos de autor que incluyen más de 100 millones de canciones, de sellos discográficos y compañías de medios. También ofrece más de 3 millones de vídeos musicales. Como servicio freemium, las funciones básicas son gratuitas con anuncios y control limitado, mientras que las funciones adicionales, como escuchar sin conexión, sin anuncios comerciales y vídeos musicales hasta 8K, se ofrecen a través de suscripciones pagas. Spotify está actualmente disponible en más de 184 países, a partir de julio de 2023. Los usuarios pueden buscar música según el artista, el álbum o el género y pueden crear, editar y compartir listas de reproducción."), style = "margin-bottom: 20px;"),
                     tags$p(HTML("Empresas como Spotify han legitimado el acceso a la música en línea mediante el streaming. Una propuesta que da acceso inmediato a 30 millones de títulos musicales (estos varían en función del país). Hoy en día hay alrededor de 100 millones de títulos publicados, con lo cual Spotify no contiene ni el 30% de la totalidad. Aún así, el usuario puede tener la sensación de disponer de una infinidad de opciones puesto que no hay tiempo físico en una vida para escuchar todas ellas."), style = "margin-bottom: 20px;"),
                   )
               ),
               h3("Filtrado de música según distintos "),
               tags$p(HTML("El siguiente programa permite realizar una representación de las canciones de Spotify según distintos criterios. Además, si posicionas el cursor encima y seleccionas alguna de las canciones, te informa de su título, álbum al que pertenece y los artistas que participan en la canción.
                           <br><br>
                           Por último, indica cuantas canciones existen en Spotify con esos criterios. Cabe destacar que puesto que nuestro mínimo de popularidad está fijado a 10, hay canciones que no pueden aparecer en este programa.")),
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
                   selectInput("genre", "Género (una película puede tener varios géneros)",
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
      # MONTECARLO
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
               plotOutput("diffusion_plot", height = "400px", width = "600px")
      ),
      
      # ------------------------------------------------------------------------------------
      # PROTEIN FOLDING
      # ------------------------------------------------------------------------------------
      tabPanel("AlphaFold", 
               fluidRow(
                 column(12,
                        h2("Plegamiento de Proteínas"),
                        p("El plegamiento de proteínas es un proceso crucial en la biología molecular donde una cadena de aminoácidos adopta una estructura tridimensional específica y funcional. Esta estructura determina su función biológica y su capacidad para interactuar con otras moléculas en el organismo."),
                        p("Asegurar que las proteínas se plieguen correctamente es vital para mantener la salud celular y el funcionamiento adecuado del cuerpo. Sin embargo, cuando el plegamiento no es correcto, pueden formarse agregados tóxicos que están relacionados con enfermedades neurodegenerativas como el Alzheimer y el Parkinson, así como otras condiciones patológicas."),
                        p("Entender los mecanismos detrás del plegamiento de las proteínas y los factores que influyen en este proceso es uno de los desafíos más importantes en biología molecular. Históricamente, la predicción precisa de las estructuras proteicas ha sido difícil y requería técnicas experimentales costosas, como la cristalografía de rayos X y la resonancia magnética nuclear (RMN)."),
                        h3("AlphaFold"),
                        p("AlphaFold, desarrollado por DeepMind, ha revolucionado este campo mediante el uso de inteligencia artificial. Utilizando redes neuronales profundas, AlphaFold puede predecir con alta precisión las estructuras tridimensionales de las proteínas a partir de sus secuencias de aminoácidos. Este avance ha abierto nuevas perspectivas en la investigación biomédica, facilitando el diseño de fármacos y el estudio de las bases moleculares de las enfermedades."),
                        p("A continuación, puedes seleccionar una proteína específica para visualizar su estructura tridimensional y obtener información detallada sobre su funcionalidad y origen.")
                 )
               ),
               
               # Segunda fila: Parte inferior dividida en dos columnas
               fluidRow(
                 # Columna izquierda: Opciones a modificar
                 column(4,
                        h3("ID de la Proteína"),
                        selectInput("proteinId", "", choices = c("A0A2K5QXN6", "A0A0D9XQI0", "Q8W3K0", "Q5VSL9", "P76011")),
                        uiOutput("protein_info")
                 ),
                 
                 # Columna derecha: Gráfico
                 column(8,
                        h3("Gráfico"),
                        class = "box",
                        uiOutput("alphafold_output"),
                        p("Esta estructura ha sido obtenida por el modelo AlphaFold, un tipo de red neuronal especializado en predecir este tipo de estructuras mediante técnicas de Inteligencia Artificial, en concreto, con técnicas de Deep Learning."),
                        tags$hr()
                 )
               )
      ),
      
      
      
      
      
      # ------------------------------------------------------------------------------------
      # CONVOLUCION
      # ------------------------------------------------------------------------------------
      tabPanel("Convolucion", 
               sidebarLayout(
                 sidebarPanel(
                   fileInput("image", "Sube tu imagen", accept = c('image/png', 'image/jpeg')),
                   selectInput("filter", "Selecciona un filtro", 
                               choices = c("Original", "Desenfoque", "Detección de bordes", "Sharpen", "Emboss", "Canny")),
                   actionButton("apply", "Aplicar Filtro"),
                   actionButton("reset", "Reiniciar"),
                   br(),
                   h4("Explicación de Filtros Convolucionales"),
                   p("Los filtros convolucionales son matrices que se aplican a una imagen para extraer características específicas. Algunos ejemplos comunes son:"),
                   tags$ul(
                     tags$li(tags$b("Desenfoque:"), " Aplica un desenfoque a la imagen."),
                     tags$li(tags$b("Detección de bordes:"), " Realza los bordes de la imagen."),
                     tags$li(tags$b("Sharpen:"), " Realza los detalles en la imagen."),
                     tags$li(tags$b("Emboss:"), " Crea un efecto de relieve en la imagen."),
                     tags$li(tags$b("Canny:"), " Utiliza un algoritmo para la detección de bordes.")
                   ),
                   h4("Historia de la Visión por Computadora"),
                   p("La visión por computadora es un campo de la inteligencia artificial que se enfoca en replicar aspectos del sistema visual humano. Se originó en la década de 1960, cuando se comenzaron a desarrollar algoritmos para la interpretación de imágenes. Con el tiempo, y gracias a los avances en la tecnología de los computadores y el surgimiento de nuevas técnicas de aprendizaje automático, la visión por computadora ha evolucionado significativamente."),
                   p("En la década de 1980, surgieron métodos para la segmentación de imágenes y la detección de bordes, lo cual permitió el desarrollo de aplicaciones más complejas. En la década de 1990, la llegada de técnicas de aprendizaje profundo revolucionó el campo, permitiendo el desarrollo de redes neuronales convolucionales (CNNs) que pueden aprender características directamente de los datos de imagen, mejorando drásticamente el rendimiento en tareas de reconocimiento y clasificación de imágenes.")
                 ),
                 mainPanel(
                   uiOutput("image_ui")
                 )
               )
      ),
      navbarMenu("About Us",
                 tabPanel("Misión", 
                          div(
                            h2("Objetivo"),
                            p("Somos alumnos del doble grado de Informática y Matemáticas que en este proyecto hemos desarrollado la práctica final de Estadística Computacional. Esta consistía en profundizar en un tema en R studio no visto en la asignatura y adquirir los conocimientos necesarios para poder mostrar su utilidad."),
                            p("Buscando entre los posibles tema sobre los que hacer el proyecto, surgió la idea del paquete Shiny. Este paquete si bien no se centra concretamente en analizar datos, realizar estudios estadístico o funciones similares, se centra en la creación de simples páginas web para la visualización de estos estudios, mediante R studio."),
                            p("Vimos por tanto en este paquete, una manera de salirnos de lo convencional y hacer algo más ilustrativo e interesante. Además, el inmenso número de facilidades que presenta Shiny y su larga cantidad de aplicaciones nos dieron la posibilidad de ser creativos y desarollar casi cualquier cosa que pudiésemos pensar.")
                          )
                 ),
                 tabPanel("Visión", 
                          div(
                            h2("Nuestra Visión"),
                            p("Aspiramos a aplicar los conocimientos adquiridos en nuestras carreras para desarrollar aplicaciones que integren la informática y la matemática."),
                            p("Nuestro enfoque en Shiny nos permitirá crear herramientas interactivas y accesibles para la visualización y análisis de datos, contribuyendo a una mejor comprensión sobre el tema.")
                          )
                 ),
                 tabPanel("Equipo", 
                          div(
                            h2("Nuestro Equipo"),
                            p("Nuestro equipo está compuesto por estudiantes dedicados del doble grado de Informática y Matemáticas. Cada uno de nosotros aporta una combinación única de habilidades técnicas y analíticas que nos permiten abordar problemas complejos desde múltiples perspectivas."),
                            p("Estamos comprometidos con el aprendizaje continuo y la aplicación práctica de nuestros conocimientos para resolver desafíos del mundo real. Reflejar nuestros conocimientos mediante visualizaciones interactivas de distintas tareas nos anima a desarollar aplicaciones semejantes a las vistas en este proyecto.")
                          ),
                          div(
                            div(
                              h3("Integrante 1: Valentín"),
                              img(src = "valentin.jpg", height = "150px"),
                              p("Suelo ir en patín a clase y me gusta jugar al fútbol. Mi película favorita es Cadena Perpetua."),
                              p("Lugar de procedencia: El Puerto de Santa María"),
                              p(
                                tags$a(href = "https://www.linkedin.com/in/juan-valentin-guerrero-cano/", target = "_blank",
                                       img(src = "linkedin.jpg", title = "LinkedIn", height = "30px")
                                ),
                                tags$a(href = "https://www.instagram.com/valentin18gc/", target = "_blank",
                                       img(src = "instagram.png", title = "Instagram", height = "30px")
                                )
                              )
                            ),
                            hr(),
                            div(
                              h3("Integrante 2: Pablo"),
                              img(src = "Pablo.jpg", height = "150px"),
                              p("Soy un apasionado del deporte y la música, dos cosas que no podrían faltar en mi día a día."),
                              p("Lugar de procedencia: Martos"),
                              p(
                                tags$a(href = "https://www.linkedin.com/in/pablo-fuentes-jim%C3%A9nez-62a497311/", target = "_blank",
                                       img(src = "linkedin.jpg", title = "LinkedIn", height = "30px")
                                ),
                                tags$a(href = "https://www.instagram.com/pablofj_/", target = "_blank",
                                       img(src = "instagram.png", title = "Instagram", height = "30px")
                                )
                              )
                            ),
                            hr(),
                            div(
                              h3("Integrante 3: Javier"),
                              img(src = "Javi.jpg", height = "150px"),
                              p("Apasionado del deporte y de la tecnología. Me gusta conocer las últimas novedades en todo momento."),
                              p("Lugar de procedencia: Torrijos (Toledo)"),
                              p(
                                tags$a(href = "https://www.linkedin.com/in/javier-g%C3%B3mez-l%C3%B3pez-642068248/", target = "_blank",
                                       img(src = "linkedin.jpg", title = "LinkedIn", height = "30px")
                                ),
                                tags$a(href = "https://github.com/javi5454", target = "_blank",
                                       img(src = "github.png", title = "Github", height = "30px")
                                )
                              )
                            ),
                            hr(),
                            div(
                              h3("Integrante 4: [Nombre]"),
                              img(src = "ruta_a_la_foto_4.jpg", height = "150px"),
                              p("Descripción breve sobre el integrante 4."),
                              p("Lugar de procedencia: [Lugar de procedencia]"),
                              p(
                                tags$a(href = "https://www.linkedin.com/in/juan-valentin-guerrero-cano/", target = "_blank",
                                       img(src = "linkedin.jpg", title = "LinkedIn", height = "30px")
                                ),
                                tags$a(href = "https://www.instagram.com/valentin18gc/", target = "_blank",
                                       img(src = "instagram.png", title = "Instagram", height = "30px")
                                )
                              )
                            )
                        )
                 )
      )
      
      )
  )
)

