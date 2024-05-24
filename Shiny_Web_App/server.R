#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggvis)
library(dplyr)

#---------------------------------------------
# SPOTIFY

spotify<-read.csv("www/Spotify.csv")

#Convertimos si es explícita o no, y el género en factores
columns_to_convert<-c(7,11)
spotify[,columns_to_convert]<-lapply(spotify[,columns_to_convert], as.factor)
genres<-sapply(spotify[11],levels)

#Filtramos por los que tienen al menos 10 de popularidad
spotify<-filter(spotify, popularity>=10)

#Pasamos a segundos la duracion
spotify[,"duration_ms"]<-spotify[,"duration_ms"]/1000

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

# Define server logic required to draw a histogram
function(input, output, session) {

  #---------------------------------------------
  # SPOTIFY
  
  #Comenzamos filtrando las canciones
  songs<-reactive({
    popularityF <- input$popularity
    durationF <- input$duration
    explicitF <- input$explicit
    genreF <- input$genre
    artistF <- input$artist
    nameF <- input$name
    albumF <- input$album
    
    if(explicitF == FALSE){
      explicitF <- "False"
    }
    else{
      explicitF <- "True"
    }
    
    # Aplicamos los filtros
    s <- filter(spotify,
                popularity >= popularityF,
                duration_ms <= durationF,
                explicit == explicitF) %>%
      arrange(popularity)
    
    # Opcional: filtrar por género
    if(input$genre != "Cualquiera"){
      genreF<-paste0("%",input$genre,"%")
      s <- filter(s, track_genre %like% genreF)
    }
    
    # Opcional: filtrar por artista
    if(!is.null(input$artist) && input$artist != ""){
      artistF <- paste0("%",input$artist,"%")
      s <- filter(s,artists %like% artistF)
    }
    
    # Opcional: filtrar por nombre de cancion
    if(!is.null(input$name) && input$name != ""){
      nameF <- paste0("%",input$name,"%")
      s <- filter(s,track_name %like% nameF)
    }
    
    # Opcional: filtrar por nombre de album
    if(!is.null(input$album) && input$album != ""){
      albumF <- paste0("%",input$album,"%")
      s <- filter(s,album_name %like% albumF)
    }
    
    #Convertimos a data frame
    s <- as.data.frame(s)
    s
  
  })
  
  # Funcion para mostrar el texto de la cancion
  song_tooltip <- function(x){
    if(is.null(x)) return(NULL)
    if(is.null(x$ID)) return(NULL)
    
    # Seleccionamos la cancion con su ID
    canciones<-isolate(songs())
    cancion<-canciones[canciones$ID == x$ID,]
    
    paste0("<b>", cancion$track_name, "</b><br>",
           cancion$artists, "<br>",
           "Álbum: ", cancion$album_name)
  }
  
  #Ahora usamos ggvis plot
  vis<- reactive({
    #Labels para los ejes
    xvar_name <- names(axis_vars_X)[axis_vars_X == input$xvar]
    yvar_name <- names(axis_vars_Y)[axis_vars_Y == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
      songs() %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 300,
                     fillOpacity := 0.4, fillOpacity.hover := 0.9,
                     key := ~ID) %>%
        add_tooltip(song_tooltip, "hover") %>%
        add_axis("x", title= xvar_name) %>%
        add_axis("y", title= yvar_name) %>%
        set_options(width=800, height=500)
  })
  
  vis %>% bind_shiny("spotify_plot")
  
  output$n_songs <- renderText({ nrow(songs())})
  
  #---------------------------------------------

}
