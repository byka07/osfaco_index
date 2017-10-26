
#setwd()
getwd()
rci <- readOGR("data/rci_shape/CIV_adm0.shp",layer = "CIV_adm0")
rci <- spTransform(rci,"+init=epsg:4326")


shinyServer(function(input, output,session){
  
  roots <-  getVolumes()
  #observe({  
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
  output$aoipath <- renderPrint({
    if(!is.null(input$file)){
      df <- parseFilePaths(roots, input$file)
      aoi_path <- as.character(df[,"datapath"])
      nofile    <- as.character("Pas de fichier selectionné")
      if(is.null(aoi_path)){
        cat(nofile)
      }else{
        cat(aoi_path)}
    }
  })
  
  
  ########################################################################################################
  # Afficher le Chemin du dossier de sortie
  output$outdirpath <- renderPrint({
    if(!is.null(input$outdir)){
      req(input$outdir)
      dirpath <- parseDirPath(roots,input$outdir)
      #dirpath <- gsub(" ","",dirpath)
      if(is.null(dirpath)){
        cat(as.character("Pas de dossier selectioné"))
      }else{
        cat(dirpath)} 
    }
  })
  
  #})
  ########################################################################################################
  # lecture du shapefile d'entree ou aoi
  aoi <- reactive({
    req (input$file)
    df <- parseFilePaths(roots, input$file)
    aoi_path <- as.character(df[,"datapath"])
    basen <- substr(basename(aoi_path),0,nchar(basename(aoi_path))-4)
    direc <- dirname(aoi_path)
    withProgress(
      message= 'Patientez svp! Lecture du Shapefile en cours', 
      value = 0, 
      {
        setProgress(value=1)
        aoi <- readOGR(direc,basen)
      })
    aoi
  })
  
  
  ########################################################################################################
  # lire le shape de la donnée qu'on veut copier
  osfaco <- reactive({
    req(input$arch_name)
    archive <- input$arch_name
    base    <- substr(basename(archive),1,nchar(basename(archive))-4)
    osfaco  <- readOGR(dsn = archive,
                       layer = base,verbose = F)
    osfaco
    })
  ########################################################################################################
  # selection par localisation
  shape_select <- reactive({
    req(input$arch_name)
    req (input$file)
    
    osfaco <- osfaco()
    osfaco <- spTransform(osfaco,"+init=epsg:4326")
    aoi    <- aoi()
    aoi <- spTransform(aoi,"+init=epsg:4326")
    shape_select <- osfaco[aoi,] 

  })
  
  ########################################################################################################
  # compter le nombre de d'images selectionnés
  nb_sel <- reactive({
    nrow(shape_select())
    })
  
  ########################################################################################################
  # Afficher le nombre d'entités 
  output$compt <- renderText({
    paste0("Nombre d'entités sélectionnées: ",nb_sel())
    })
  
  ########################################################################################################
  # Liste des données à copier
  liste_command <- reactive({
    
  shape_select <-shape_select()
  liste_command <-shape_select@data
  
  # ## transformer les "\"en "/" et forcer la lettre du chemin dans la colonne Path
  liste_command$Path <- gsub("[\\]", "/", liste_command$Path)
  liste_command$Path <- gsub(str_sub(liste_command[1,2], 1, 3), dirname(getwd()), liste_command$Path)
  liste_command
  })
  
  ########################################################################################################
  # taille (size) des images selectionnés
 size <- reactive({
   liste_command <-liste_command()
   size <-0
  for(img in liste_command$Image){
    dir  <- liste_command[liste_command$Image==img,"Path"]
    size <- file.size(paste(dir,img, sep="/"))/1000000
  }
   size
 })
 
 ########################################################################################################
 # afficher la taille des élement selectionnés
 output$size <- renderText({
   if (!is.na(size())){
   paste0("De taille: ",size()," Mo")
   }else{
     "SVP!! INSÉREZ LE DISC CONTENANT CES IMAGES"
   }
 })
  ########################################################################################################
  # creation de la carte 
  output$map <- renderLeaflet({
    
    # afficher les scènes selectionnées
    if(!is.null(input$file) & !is.null(input$arch_name)){
      shape_select <- shape_select()
      shape_select <- spTransform(shape_select,"+init=epsg:4326")
      aoi <- aoi()
      aoi <- spTransform(aoi,"+init=epsg:4326")
      
      map <- leaflet()%>%
        addTiles()%>%
        # shape de la CI
        addPolygons(data = rci, opacity = 100, 
                    color = "red", 
                    weight = 1.25,popup = NULL,
                    options = list(clickable = FALSE), 
                    fill = F, fillColor = "red", 
                    fillOpacity = 100)%>%
        # images selectionnées
        addPolygons(data = shape_select, opacity = 100,
                    color = "yellow",
                    weight = 2.25,popup = NULL,
                    options = list(clickable = FALSE),
                    fill = F, fillColor = "yellow",
                    fillOpacity = 0.1)%>%
        # zone d'intérêt
        addPolygons(data = aoi, opacity = 100,
                    color = "red",
                    weight = 1.25,popup = NULL,
                    options = list(clickable = FALSE),
                    fill = T, fillColor = "red",
                    fillOpacity = 0.1)%>%
        
        setView(lng = -5.4267075,lat =6.8153801, zoom = 7)
      
      #afficher le shape du aoi
    }else{
      if(!is.null(input$file)){
        aoi <- aoi()
        aoi <- spTransform(aoi,"+init=epsg:4326")
        
        map <- leaflet()%>%
          addTiles()%>%
          # shape de la CI
          addPolygons(data = rci, opacity = 100, 
                      color = "red", 
                      weight = 1.25,popup = NULL,
                      options = list(clickable = FALSE), 
                      fill = F, fillColor = "red", 
                      fillOpacity = 100)%>%
          # zone d'intérêt
          addPolygons(data = aoi, opacity = 100,
                      color = "red",
                      weight = 1.25,popup = NULL,
                      options = list(clickable = FALSE),
                      fill = T, fillColor = "red",
                      fillOpacity = 0.1)%>%
          setView(lng = -5.4267075,lat =6.8153801, zoom = 7)
        
        
        # affichae de depart ou affichage des limtes du pays
      }else{
      map <- leaflet()%>%
        addTiles()%>%
        # shape de la CI
        addPolygons(data = rci, opacity = 100,
                    color = "red",
                    weight = 1.25,popup = NULL,
                    options = list(clickable = FALSE),
                    fill = F, fillColor = "red",
                    fillOpacity = 100) %>%
        setView(lng = -5.4267075,lat =6.8153801, zoom = 7)
      }
    }
  })
  
  ########################################################################################################
  # Copier les images
  observeEvent(input$copie,{
    if(!is.na(size()) & !is.null(input$outdir)){
    # boite de progressions
    withProgress(
      message= 'Copie en cours... Ceci peut prend quelques minutes', 
      value = 0, 
      {
        #setProgress(value=1)

    #rappel de la liste de copie
    liste_command <-liste_command()
    
    #chemin de copie
    req(input$outdir)
    dirpath <- parseDirPath(roots,input$outdir)
    
    n <- 1
    nb <- nrow(shape_select())
    
    #boucle de la copie
    for(img in liste_command$Image){
      dir  <- liste_command[liste_command$Image==img,"Path"]
      file.copy(paste(dir,img, sep="/"),dirpath)
      
      incProgress(n/nb, detail = paste(n,"/",nb))
      n <- n+1
    
      Sys.sleep(0.1)
    }
    
      })
    }else{
      withProgress(
        message= 'SVP: CHOISIR LE BON DISC DE DONNÉES ET LE DOSSIER DE COPIE!', 
        value = 0, 
        {
          j <- 1
          for(j in 1:60){
          incProgress(j/100)
          Sys.sleep(0.2)
          }
        })
      
      }
  })
  
})