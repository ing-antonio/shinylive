

# app.R
library(shiny)
library(here)
library(dplyr)
library(leaflet)
library(yaml)

#here::i_am("procesamiento/app.R")

# Cargar datos
datos <- readRDS("datos.rds")

datos <- datos %>%
  mutate(
    latitud  = as.numeric(latitud),
    longitud = as.numeric(longitud)
  ) %>%
  filter(!is.na(latitud) & !is.na(longitud))

usuarios <- yaml::read_yaml("usuarios.yml")$usuarios

usuarios <- do.call(rbind.data.frame, lapply(usuarios, as.data.frame))
usuarios[] <- lapply(usuarios, as.character)

print(usuarios)

ui <- fluidPage(
  uiOutput("login_ui"),
  uiOutput("app_ui")
)

server <- function(input, output, session){
  
  user <- reactiveVal(NULL)
  
  output$login_ui <- renderUI({
    if(is.null(user())){
      fluidRow(
        column(4, offset = 4,
               textInput("usuario", "Usuario:"),
               passwordInput("password", "Contraseña:"),
               actionButton("entrar", "Entrar")
        )
      )
    }
  })
  
  observeEvent(input$entrar, {
    fila <- usuarios %>%
      filter(usuario == input$usuario, password == input$password)
    if(nrow(fila) == 1){
      user(fila$alcaldia)  
    } else {
      showModal(modalDialog(
        title = "Error",
        "Usuario o contraseña incorrectos",
        easyClose = TRUE
      ))
    }
  })
  
  output$app_ui <- renderUI({
    req(user())
    fluidPage(
      h3(paste("Bienvenido,", user())),
      dateRangeInput("fechas", "Selecciona rango de fechas:",
                     start = min(datos$fecha_inicio),
                     end = max(datos$fecha_inicio)),
      leafletOutput("mapa", height = 600)
    )
  })
  
  datos_filtrados <- reactive({
    req(user())
    if(user() == "TODAS"){
      datos %>%
        filter(fecha_inicio >= input$fechas[1],
               fecha_inicio <= input$fechas[2])
    } else {
      filtrar_datos(
        datos,
        alcaldia_seleccionada = user(),
        fecha_inicio_seleccionada = input$fechas[1],
        fecha_fin_seleccionada = input$fechas[2]
      )
    }
  })
  
  
  output$mapa <- renderLeaflet({
    req(datos_filtrados())
    df <- datos_filtrados()
    leaflet(df) %>%
      addTiles() %>%
      addCircles(~longitud, ~latitud)
  })
}

shinyApp(ui = ui, server = server)
