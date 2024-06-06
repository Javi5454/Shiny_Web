# Carga de librerías
library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)
library(magick)

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

# Función para aplicar filtros convolucionales
apply_filter <- function(image, filter) {
  switch(filter,
         "Desenfoque" = image %>% image_blur(radius = 10, sigma = 5),
         "Detección de bordes" = image %>% image_edge(radius = 2),
         "Sharpen" = image %>% image_convolve(kernel = "3x3: 0, -1, 0, -1, 5, -1, 0, -1, 0"),
         "Emboss" = image %>% image_convolve(kernel = "3x3: -2, -1, 0, -1, 1, 1, 0, 1, 2"),
         "Canny" = image %>% image_convert(colorspace = "gray") %>% 
           image_morphology(method = "Convolve", kernel = "DoG:0,1,0.5"),
         "Original" = image
  )
}

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
  # AlphaFold
    # Crear el renderUI para actualizar AlphaFoldMolstar
    output$alphafold_output <- renderUI({
      req(input$proteinId)  # Asegurarse de que hay una ID de proteína seleccionada
      
      # Devuelve el widget AlphaFoldMolstar con la ID seleccionada
      AlphaFoldMolstar(
        uniProtId = input$proteinId,
        showAxes = FALSE,
        useCif = TRUE,
        dimensions = c(500, 300)
      )
    })
  
  # Crea un dataframe con la información de las proteínas
  protein_frame <- data.frame(
    id = c("A0A2K5QXN6", "A0A0D9XQI0", "Q8W3K0", "Q5VSL9", "P76011"),
    organismo = c("Cebus imitator", "Leersia perrieri", "Arabidopsis thaliana", "Homo sapiens", "Escherichia coli (cepa K12)"),
    nombre = c("Quinasa 2 Dependiente de la Ciclina ", "Proteína no caracterizada", "Proteína con probable resistencia a las enfermedades At1g58602", "Proteína 1 interactuante con estriatina", "UPF0410 proteína YmgE"),
    descripcion = c("Regulador clave en la progresión del ciclo celular.", "Desconocida.", "Necesaria para interacciones incompatibles con cepas avirulentas de Hyaloperonospora arabidopsidis.", "Desempeña un papel en la regulación de la morfología celular y la organización del citoesqueleto." , "Respuesta al estrés celular y en la regulación de la morfología celular y la estructura del citoesqueleto."),
    stringsAsFactors = FALSE
  )
  
  # Función para obtener la información de la proteína según el ID seleccionado
  get_protein_info <- function(proteinId) {
    protein_frame[protein_frame$id == proteinId, ]
  }
  
  # Crear el renderUI para mostrar la información de la proteína según el ID
  output$protein_info <- renderPrint({
    proteinId <- input$proteinId
    protein <- get_protein_info(proteinId)
    HTML(paste0(
      "<strong>Nombre:</strong> ", protein$nombre, "<br><br>",
      "<strong>Organismo de origen:</strong> ", protein$organismo, "<br><br>",
      "<strong>Funcionalidad:</strong> ", protein$descripcion, "<br><br>"
    ))
  })
  
  
  #---------------------------------------------
  # Funcionalidad de Valentin
  
  image_reactive <- reactiveVal(NULL)
  original_image <- reactiveVal(NULL)
  
  observeEvent(input$image, {
    req(input$image)
    image <- image_read(input$image$datapath)
    image_reactive(image)
    original_image(image)
  })
  
  observeEvent(input$apply, {
    req(image_reactive())
    filtered_image <- apply_filter(image_reactive(), input$filter)
    image_reactive(filtered_image)
  })
  
  observeEvent(input$reset, {
    req(original_image())
    image_reactive(original_image())
  })
  
  output$image_ui <- renderUI({
    req(image_reactive())
    output$image_output <- renderImage({
      tmpfile <- image_write(image_reactive(), tempfile(fileext = 'jpg'), format = 'jpg')
      list(src = tmpfile, contentType = 'image/jpeg', width = '80%', height = 'auto')
    }, deleteFile = TRUE)
    imageOutput("image_output", width = "80%", height = "auto")
  })
}
