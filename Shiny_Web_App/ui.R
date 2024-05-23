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
        padding: 10px;
        text-align: center;
        border-bottom: 2px solid #0073b7; /* Color similar al del ejemplo */
      }
      .navbar-custom {
        margin-top: 0px; /* Ajustar si es necesario */
      }
    "))
  ),
  div(class = "top-space",
      img(src = "shiny-solo.png", height = "50px"),
      h1("Trabajo final Estadística Computacional")
  ),
  navbarPage(
    title = NULL, # Para que no se muestre el título en la barra de navegación
    id = "navbar",
    tabPanel("Home", "Contenido de Home"),
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
    tabPanel("Articles", "Contenido de Articles"),
    tabPanel("Contact", "Contenido de Contact")
  )
)