#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
      img(src = "./www/shiny-solo.png", height = "50px"),
      h1("Trabajo final Estadística Computacional")
  ),
  navbarPage(
    title = NULL, # Para que no se muestre el título en la barra de navegación
    id = "navbar",
    tabPanel("Home", "Contenido de Home"),
    tabPanel("Gallery", "Contenido de Gallery"),
    tabPanel("Articles", "Contenido de Articles"),
    tabPanel("Contact", "Contenido de Contact")
  )
)