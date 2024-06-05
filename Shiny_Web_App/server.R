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
library(ggplot2)


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
  #---------------------------------------------
  # MONTECARLO
  
  
  # Variable para almacenar las simulaciones
  diffusion_data <- reactiveValues(data = NULL)
  
  # Se ejecuta cuando pulsamos Ejecutar Simulación
  observeEvent(input$run_diffusion, {
    num_particles <- input$num_particles
    num_steps <- input$num_steps_diffusion
    
    # Simulación de la difusión en 2D
    diffusion_sim <- replicate(num_particles, {
      x <- cumsum(sample(c(-1, 1), num_steps, replace = TRUE)) # Movimiento horizontal +-1
      y <- cumsum(sample(c(-1, 1), num_steps, replace = TRUE)) # Movimiento vertical +-1
      data.frame(step = 1:num_steps, x = x, y = y) # Creamos el dataframe con cada instante y la posicion
    }, simplify = FALSE)
    
    diffusion_df <- do.call(rbind, lapply(seq_along(diffusion_sim), function(i) {
      cbind(diffusion_sim[[i]], particle = i)
    }))
    
    diffusion_data$data <- diffusion_df # Añadimos los datos al objeto diffusion_data
    
    updateSliderInput(session, "time_step", max = num_steps, value = 1) # Esto nos permitira seleccionar el instante deseado
  })
  
  # 
  output$diffusion_plot <- renderPlot({
    req(diffusion_data$data)
    step_data <- diffusion_data$data %>% filter(step == input$time_step) # Se obtienen los datos del instante t seleccionado en time_step
    
    # Creamos el gráfico
    plot <- ggplot(step_data, aes(x = x, y = y, color = as.factor(particle))) +
      geom_point(size = 5, show.legend = FALSE) + # Hacemos puntos más grandes
      labs(title = paste("Difusión de Montecarlo - Instante t =", input$time_step), x = "Posición X", y = "Posición Y") +
      theme_minimal() +
      xlim(min(diffusion_data$data$x), max(diffusion_data$data$x)) + # Fijamos los límites del eje x 
      ylim(min(diffusion_data$data$y), max(diffusion_data$data$y))   # Fijamos los límites del eje y 
    
    # Si queremos ver las trayectorias las añadimos al grafico
    if (input$show_trajectory) {
      all_steps_data <- diffusion_data$data %>% filter(step <= input$time_step) # Tomamos las posiciones anteriores al instante t
      plot <- plot + geom_path(data = all_steps_data, aes(x = x, y = y, group = particle), alpha = 0.5, size = 1.2, show.legend = FALSE) # Dibujamos los caminos entre las posiciones de cada particula 
    }
    
    plot
  })
  
  #Actualizamos la barra deslizante si se modifican los datos (se realiza una nueva simulacion)
  observeEvent(diffusion_data$data, {
    updateSliderInput(session, "time_step", max = max(diffusion_data$data$step))
  })
  
  # Actualizacion del gráfico si se modifica el valor de time_step (cambiamos de instante)
  observeEvent(input$time_step, {
    output$diffusion_plot <- renderPlot({
      req(diffusion_data$data)
      step_data <- diffusion_data$data %>% filter(step == input$time_step) # Se obtienen los datos del instante t seleccionado en time_step
      
      # Creamos el gráfico
      plot <- ggplot(step_data, aes(x = x, y = y, color = as.factor(particle))) +
        geom_point(size = 5, show.legend = FALSE) + # Hacemos puntos más grandes
        labs(title = paste("Difusión de Montecarlo - Instante t =", input$time_step), x = "Posición X", y = "Posición Y") +
        theme_minimal() +
        xlim(min(diffusion_data$data$x), max(diffusion_data$data$x)) + # Fijamos los límites del eje x 
        ylim(min(diffusion_data$data$y), max(diffusion_data$data$y))   # Fijamos los límites del eje y 
      
      # Si queremos ver las trayectorias las añadimos al grafico
      if (input$show_trajectory) {
        all_steps_data <- diffusion_data$data %>% filter(step <= input$time_step) # Tomamos las posiciones anteriores al instante t
        plot <- plot + geom_path(data = all_steps_data, aes(x = x, y = y, group = particle), alpha = 0.5, size = 1.2, show.legend = FALSE) # Dibujamos los caminos entre las posiciones de cada particula 
      }
      
      plot
    })
  })

  
  
  
  
  #---------------------------------------------

}




