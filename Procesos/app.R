library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(RSQLite)

ui <- dashboardPage(title = "Marketing Services", skin = "black",
  dashboardHeader(title = "Logo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Administrar", icon = icon("gear"), tabName = "Admin",
               menuSubItem(text = "Areas", tabName = "area", icon = icon("building-o")),
               menuSubItem(text = "Encargados", tabName = "persona", icon = icon("user-o")),
               menuSubItem(text = "Procesos", tabName = "proceso", icon = icon("gears"))))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "area", 
              fluidRow(
                box(title = tagList(icon("building-o"), strong("Areas")), 
                    width = 12, status = "info", solidHeader = T,
                    column(3, valueBoxOutput("NAreas", width = "100%")),
                    column(9, box(title = "Agregar", collapsible = T, width = "100%", collapsed = T,
                                  column(4, textInput("CodArea", 
                                                      value = "", 
                                                      label = "Codigo del area", 
                                                      placeholder = "Ingrese", 
                                                      width = "100%")),
                                  column(4, textInput("DescArea", 
                                                      value = "", 
                                                      label = "Descripcion", 
                                                      placeholder = "Ingrese", 
                                                      width = "100%")),
                                  column(4, actionButton("BAgregarArea", 
                                                         label = "Agregar", 
                                                         icon = icon("plus"))))),
                    column(12, hr()),
                    column(12, dataTableOutput("TablaAreas")),
                    column(12, hr()),
                    column(12, box(title = "Eliminar", status = "warning", width = "100%", collapsible = T, collapsed = T, 
                                   column(4, uiOutput("DeleteArea")),
                                   column(4, actionButton("BDeleteArea", label = "Eliminar", icon = icon("times")))))))),
      tabItem(tabName = "persona", 
              fluidRow(
                box(title = tagList(icon("user-o"), strong("Encargados")), 
                    width = 12, status = "info", solidHeader = T,
                    column(3, valueBoxOutput("NEncargados", width = "100%")),
                    column(12, box(title = "Agregar", collapsible = T, width = "100%", collapsed = T,
                                  column(4, textInput("Nombre", 
                                                      value = "", 
                                                      label = "Nombre", 
                                                      placeholder = "Ingrese", 
                                                      width = "100%")),
                                  column(4, uiOutput("SelectAreaEncargado")),
                                  column(4, actionButton("BAgregarEncargado", 
                                                         label = "Agregar", 
                                                         icon = icon("plus"))))),
                    column(12, hr()),
                    column(12, dataTableOutput("TablaEncargados")),
                    column(12, hr()),
                    column(12, box(title = "Eliminar", status = "warning", width = "100%", collapsible = T, collapsed = T, 
                                   column(4, uiOutput("DeleteEncargado")),
                                   column(4, actionButton("BDeleteEncargado", label = "Eliminar", icon = icon("times")))))))),
      tabItem(tabName = "proceso", 
              fluidRow(
                box(title = tagList(icon("gears"), strong("Procesos")), 
                    width = 12, status = "info", solidHeader = T,
                    column(3, valueBoxOutput("NProcesos", width = "100%")),
                    column(12, box(title = "Agregar", collapsible = T, width = "100%", collapsed = T,
                                   column(3, textInput("Proceso", 
                                                       value = "", 
                                                       label = "Proceso", 
                                                       placeholder = "Ingrese", 
                                                       width = "100%")),
                                   column(3, uiOutput("SelectEncargadoProceso")),
                                   column(3, selectInput("Periodo", 
                                                         label = "Periodo", 
                                                         choices = c("Diario", 
                                                                     "Semanal", 
                                                                     "Mensual", 
                                                                     "Trimestral", 
                                                                     "Semestral", 
                                                                     "A pedido", 
                                                                     "Otro"))),
                                   column(3, selectInput("Prioridad", 
                                                         label = "Prioridad", 
                                                         choices = c("Alta", "Media", "Baja"))),
                                   column(3, uiOutput("SelectOrigenProceso")),
                                   column(3, uiOutput("SelectDestinoProceso")),
                                   column(3, uiOutput("SelectTablas")),
                                   column(3, textInput("Observaciones", 
                                                       label = "Observaciones", 
                                                       value = "Ninguna", 
                                                       placeholder = "Ingrese")),
                                   column(3, actionButton("BAgregarProceso", 
                                                          label = "Agregar", 
                                                          icon = icon("plus"))))),
                    column(12, hr()),
                    column(12, dataTableOutput("TablaProcesos")),
                    column(12, hr()),
                    column(12, box(title = "Eliminar", status = "warning", width = "100%", collapsible = T, collapsed = T, 
                                   column(4, uiOutput("DeleteProceso")),
                                   column(4, actionButton("BDeleteProceso", label = "Eliminar", icon = icon("times"))))))))
    )
  )
)
   
   
db <- src_sqlite(path = "/home/oscar/Procesos/database")
tblAreas      <- tbl(db, "Areas")
tblEncargados <- tbl(db, "Encargados")
tblProcesos   <- tbl(db, "Procesos")

server <- function(input, output, session) {
#### Areas ####  
  dArea <- reactivePoll(intervalMillis = 1000, 
                 checkFunc = function(){
                   Valor <- tblAreas %>% 
                     collect(n = Inf)  
                  return(Valor)
                 }, 
                 valueFunc = function(){
                   return(tblAreas)
                 }, session = session) 
  
  
  output$NAreas <- renderValueBox({
    withProgress(expr = {
      N <- dArea() %>% count() %>% collect(n = Inf)
    }, value = 1, message = "Contando", detail = "Areas")
    valueBox(value = N, subtitle = "Areas", icon = icon("building-o"), color = "aqua")
  })
  
  observeEvent(input$BAgregarArea, {
    if ((input$CodArea=="") && (input$DescArea=="")){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Ingrese:", detail = "Codigo y descripcion")
    }else if((input$CodArea=="")){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Ingrese:", detail = "Codigo")
    }else if((input$DescArea=="")){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Ingrese:", detail = "Descripcion")
    }else{
      withProgress(value = 1, message = "Agregando", detail = "Area", expr = {
        NewArea <- data_frame(CodArea = input$CodArea, Descripcion = input$DescArea)
        Areas   <- dArea() %>% collect(n = Inf) 
        Areas <- union(Areas, NewArea, by = "CodArea") %>% distinct(CodArea, .keep_all = T)
        dbWriteTable(conn = db$con, name = "Areas", Areas, overwrite = T)
      })
    }
  })
  
  output$TablaAreas <- renderDataTable({
    withProgress(value = 1, message = "Rescatando", detail = "Areas", expr = {
      df <- dArea() %>% collect(n = Inf)
    })
    datatable(df, rownames = F, style = "bootstrap", caption =  "Areas",filter = "top", options = list(searching = F, paging = F))
  })
  
  output$DeleteArea <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Codigos", expr = {
      Areas <- dArea() %>% distinct(CodArea) %>% collect(n = Inf)  
    })
    selectInput("AreaSelect", label = "Selecione", multiple = T, selected = NA, choices = Areas$CodArea)
  })
  
  observeEvent(input$BDeleteArea, {
    if (is.null(input$AreaSelect)){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Seleccione:", detail = "Area")
    }else{
      withProgress(value = 1, message = "Eliminando", detail = "Areas", expr = {
        Areas   <- dArea() %>% collect(n = Inf) %>% filter(!CodArea %in% input$AreaSelect) 
        dbWriteTable(conn = db$con, name = "Areas", Areas, overwrite = T)
      })
    }
  })
  
#### Encargados ####
  dEncargados <- reactivePoll(intervalMillis = 1000, 
                        checkFunc = function(){
                          Valor <- tblEncargados %>% 
                            collect(n = Inf)  
                          return(Valor)
                        }, 
                        valueFunc = function(){
                          return(tblEncargados)
                        }, session = session) 
  
  
  output$NEncargados <- renderValueBox({
    withProgress(expr = {
      N <- dEncargados() %>% count() %>% collect(n = Inf)
    }, value = 1, message = "Contando", detail = "Encargados")
    valueBox(value = N, subtitle = "Encargados", icon = icon("user-o"), color = "aqua")
  })
  
  output$SelectAreaEncargado <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Areas", expr = {
      Areas <- dArea() %>% collect(n = Inf) %>% distinct(Descripcion)
    })
    selectInput("AreaEncargado", label = "Area", choices = Areas$Descripcion, selected = Areas$Descripcion[1])
  })
  
  observeEvent(input$BAgregarEncargado, {
    if((input$Nombre=="")){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Ingrese:", detail = "Nombre")
    }else{
      withProgress(value = 1, message = "Agregando", detail = "Encargado", expr = {
        NewEncargado <- data_frame(Encargado = input$Nombre, Area = input$AreaEncargado)
        Encargados   <- dEncargados() %>% collect(n = Inf) 
        Encargados <- union(Encargados, NewEncargado, by = "Encargado") %>% distinct(Encargado, .keep_all = T)
        dbWriteTable(conn = db$con, name = "Encargados", Encargados, overwrite = T)
      })
    }
  })
  

  output$TablaEncargados <- renderDataTable({
    withProgress(value = 1, message = "Rescatando", detail = "Encargados", expr = {
      df <- dEncargados() %>% collect(n = Inf)
    })
    datatable(df, rownames = F, style = "bootstrap", caption = "Encargados",filter = "top", options = list(searching = F, paging = F))
  })
  
  output$DeleteEncargado <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Encargados", expr = {
      Encargados <- dEncargados() %>% distinct(Encargado) %>% collect(n = Inf)  
    })
    selectInput("NombreSelect", label = "Selecione", multiple = T, selected = NA, choices = Encargados$Encargado)
  })
  
  observeEvent(input$BDeleteEncargado, {
    if (is.null(input$NombreSelect)){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Seleccione:", detail = "Nombre")
    }else{
      withProgress(value = 1, message = "Eliminando", detail = "Encargados", expr = {
        Encargados  <- dEncargados() %>% collect(n = Inf) %>% filter(!Encargado %in% input$NombreSelect) 
        dbWriteTable(conn = db$con, name = "Encargados", Encargados, overwrite = T)
      })
    }
  })

#### Procesos #### 
  dProcesos <- reactivePoll(intervalMillis = 1000, 
                              checkFunc = function(){
                                Valor <- tblProcesos %>% 
                                  collect(n = Inf)  
                                return(Valor)
                              }, 
                              valueFunc = function(){
                                return(tblProcesos)
                              }, session = session) 
  
  
  output$NProcesos <- renderValueBox({
    withProgress(expr = {
      N <- dProcesos() %>% count() %>% collect(n = Inf)
    }, value = 1, message = "Contando", detail = "Procesos")
    valueBox(value = N, subtitle = "Procesos", icon = icon("gears"), color = "aqua")
  })
  
  output$SelectEncargadoProceso <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Encargados", expr = {
      Encargados <- dEncargados() %>% collect(n = Inf) %>% distinct(Encargado)
    })
    selectInput("EncargadoProceso", label = "Encargado", choices = Encargados$Encargado, selected = Encargados$Encargado[1])
  })
  
  output$SelectOrigenProceso <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Origenes", expr = {
      #Encargados <- dEncargados() %>% collect(n = Inf) %>% distinct(Encargado)
    })
    selectInput("OrigenProceso", 
                label = "Origen", 
                choices = c("FTP", 
                            "Email", 
                            "Fisico", 
                            "Buro", 
                            "Servidor 115", 
                            "Servidor 130", 
                            "Analisis", 
                            "Otro"), 
                selected = "Otro")
  })
  
  output$SelectDestinoProceso <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Destinos", expr = {
      #Encargados <- dEncargados() %>% collect(n = Inf) %>% distinct(Encargado)
    })
    selectInput("DestinoProceso", 
                label = "Destino", 
                choices = c("FTP", 
                            "Email", 
                            "Fisico", 
                            "Buro", 
                            "Servidor 115", 
                            "Servidor 130", 
                            "Analisis", 
                            "Otro"), selected = "Otro")
  })
  
  output$SelectTablas <- renderUI({
    if (length(input$OrigenProceso)!=0){
      if (input$OrigenProceso == "Servidor 130" | input$DestinoProceso == "Servidor 130"){
        selectInput("Tablas", label = "Tablas", multiple = T, choices = c("a", "b", "c", "d"), selected = NA)
      }
    }
    
  })
  
  observeEvent(input$BAgregarProceso, {
    if((input$Proceso=="")){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Ingrese:", detail = "Nombre del proceso")
    }else{
      withProgress(value = 1, message = "Agregando", detail = "Proceso", expr = {
        if (length(input$Tablas)>1){
          NewProceso <- data_frame(Proceso = rep(input$Proceso, length(input$Tablas)), 
                                   Encargado = rep(input$EncargadoProceso, length(input$Tablas)), 
                                   Periodo = rep(input$Periodo, length(input$Tablas)),
                                   Prioridad = rep(input$Prioridad, length(input$Tablas)),
                                   Origen = rep(input$OrigenProceso, length(input$Tablas)),
                                   Destino = rep(input$DestinoProceso, length(input$Tablas)),
                                   Tablas = c(input$Tablas),
                                   Observaciones = rep(input$Observaciones, length(input$Tablas)))
          Procesos   <- dProcesos() %>% collect(n = Inf) 
          Procesos <- union(NewProceso, Procesos) %>% distinct()
          dbWriteTable(conn = db$con, name = "Procesos", Procesos, overwrite = T)
        }else{
          NewProceso <- data_frame(Proceso = input$Proceso, 
                                   Encargado = input$EncargadoProceso, 
                                   Periodo = input$Periodo,
                                   Prioridad = input$Prioridad,
                                   Origen = input$OrigenProceso, 
                                   Destino = input$DestinoProceso,
                                   Tablas = ifelse(is.null(input$Tablas), "", input$Tablas),
                                   Observaciones = input$Observaciones)
          Procesos   <- dProcesos() %>% collect(n = Inf) 
          Procesos <- union(Procesos, NewProceso) %>% distinct()
          dbWriteTable(conn = db$con, name = "Procesos", Procesos, overwrite = T)
        }
      })
    }
  })

  output$TablaProcesos <- renderDataTable({
    withProgress(value = 1, message = "Rescatando", detail = "Procesos", expr = {
      df <- dProcesos() %>% collect(n = Inf)
    })
    datatable(df, rownames = F, style = "bootstrap", caption = "Procesos", filter = "top", options = list(searching = F, paging = F))
  })
  
  output$DeleteProceso <- renderUI({
    withProgress(value = 1, message = "Rescatando", detail = "Procesos", expr = {
      Procesos <- dProcesos() %>% distinct(Proceso) %>% collect(n = Inf)  
    })
    selectInput("ProcesoSelect", label = "Selecione", multiple = T, selected = NA, choices = Procesos$Proceso)
  })
  
  observeEvent(input$BDeleteProceso, {
    if (is.null(input$ProcesoSelect)){
      withProgress(expr = Sys.sleep(2), value = 1, message = "Seleccione:", detail = "Proceso")
    }else{
      withProgress(value = 1, message = "Eliminando", detail = "Procesos", expr = {
        Procesos  <- dProcesos() %>% collect(n = Inf) %>% filter(!Proceso %in% input$ProcesoSelect) 
        dbWriteTable(conn = db$con, name = "Procesos", Procesos, overwrite = T)
      })
    }
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

