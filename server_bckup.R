packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
packages(leaflet)
packages(raster)
packages(shiny)
packages(shinydashboard)
packages(plyr)
packages(rgeos)
packages(rgdal)
packages(shinyBS)
packages(shinyFiles)
packages(htmltools)

library(shiny)
library(leaflet)
library(shinyFiles)
library(rgdal)
library(rgeos)
library(raster)
library(shinydashboard)

shinyServer(function(input, output,session){
  
  ########################################################################################################
  # creation de la carte 
  shape <- readOGR("./data/CIV_adm0.shp",layer = "CIV_adm0")
  shape <- spTransform(shape,"+init=epsg:4326")
  
  output$map <- renderLeaflet({
    
    map <-leaflet()%>%
      addTiles()%>%
      addPolygons(data = shape, opacity = 100, 
                  color = "red", 
                  weight = 1.25,popup = NULL,
                  options = list(clickable = FALSE), 
                  fill = F, fillColor = "red", 
                  fillOpacity = 100) %>%
      setView(lng = -5.4267075,lat =6.8153801, zoom = 7)
    
  })
  
 
  roots <-  getVolumes()
  observe({  
  ########################################################################################################
  # selection du fichier shapefile  
  shinyFileChoose(input, 
                  'file',
                  filetype='shp', 
                  roots=getVolumes(), 
                  session=session)
    
  ########################################################################################################
  # selection un dossier de sortie 
  shinyDirChoose(input,
                 'outdir',
                 roots=getVolumes(),
                 session=session)
  
  ########################################################################################################
  # Chemin du shapefile
  if(!is.null(input$file)){
    
    output$filepath <- renderPrint({
    df <- parseFilePaths(roots, input$file)
    file_path <- as.character(df[,"datapath"])
    nofile <- as.character("Pas de fichier selectionné")
    if(is.null(file_path)){
      cat(nofile)
    }else{
      cat(file_path)}
  })
  }
  
  ########################################################################################################
  # Affiffer le Chemin du dossier de sortie
  if(!is.null(input$outdir)){
  
    output$outdirpath <- renderPrint({
      req(input$outdir)
      dirpath <- parseDirPath(roots,input$outdir)
      #dirpath <- gsub(" ","",dirpath)
      if(is.null(dirpath)){
        cat(as.character("Pas de dossier selectioné"))
      }else{
        cat(dirpath)} 
     })
    
    }

  })
  ########################################################################################################
  # lecture du shapefile d'entrÃ©e
  shp <- reactive({
    req (input$file)
    df <- parseFilePaths(roots, input$file)
    file_path <- as.character(df[,"datapath"])
    basen <- substr(basename(file_path),0,nchar(basename(file_path))-4)
    direc <- dirname(file_path)
    withProgress(
      message= 'Paientez svp! Lecture du Shapefile en cours', 
      value = 0, 
      {
        setProgress(value=.1)
        shp <-readOGR(direc,basen)
        
        
      })
  })
  ########################################################################################################
  # Ajouter la zone d'étude à la carte "map"
  observe({
  if(!is.null(input$file)){ 
  shp <- shp()
  shp <- spTransform(shp,"+init=epsg:4326")
  
  leafletProxy("map")%>% 
        addPolygons(data = shp, opacity = 100, 
                color = "red", 
                weight = 1.25,popup = NULL,
                options = list(clickable = FALSE),
                fill = T, fillColor = "red", 
                fillOpacity = 0.1)
 
  }
  })
  
  ########################################################################################################
  # Afficher les colonnes du shapefile pour la coordonnée X
  output$Xcoor <- renderUI({
    req (input$file)
    shp <- shp()
    categories <- names(shp@data)
    print(categories)
    selectInput("Xcoor",
                label = h5(paste("Choisir les coordonnées X")),
                choices = categories,
                selected = "",
                multiple = FALSE
    )
  })
  
  ########################################################################################################
  # Afficher les colonnes du shapefile pour la coordonnée Y
  output$Ycoor <- renderUI({
    req (input$file)
    shp <- shp()
    categories <- names(shp@data)
    print(categories)
    selectInput("Ycoor",
                label = h5(paste("Choisir les coordonnées Y")),
                choices = categories,
                selected = "",
                multiple = FALSE
    )
  })  
  
})