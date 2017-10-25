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

shinyUI(
  
  bootstrapPage(
    tags$head(
    # Include our custom CSS
    includeCSS("styles.css")#,
    #includeScript("gomap.js")
  ),
  h5("Application de copie des données OSFACO"),
  leafletOutput("map",height = 900),
  
  #leafletOutput("addmap",height = 900),
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left ="auto" , right =20 , bottom = "auto",
                width = 330, height = "auto",
                
    # titre
    h2("OSFACO REQUETE"),
    
    # Bouton de selection de fichier shp
    shinyFilesButton('file', 
                     'Entrer votre zone d\'intérêt (shapefile)', 
                     'Choisir un fichier', 
                     FALSE),
    # texte affichant le chemein du fichier selectionnée 
    # s'affiche quand le fichier selectionné es lu
    textOutput("aoipath"),
    br(),
    
    # bouton radio pour choisir les données qu'on veut copier
    radioButtons(inputId="arch_name",
                 label = "Choisir la donnée à copier",
                 selected = character(0),
                 choiceNames = list("Pivot 2015","heritage","Pivot 2015 (suite)"),
                 choiceValues = list("data/osfaco_P2015_shape/OSFACO..shp",
                                     "data/osfaco_heritage_shape/SWH_RCI.shp",
                                     "data/osfaco_partiel_shape/RCI_partiel_OSFACO_MS_P.shp"
                                     )
                 ),
    
    br(),
    # zone pour afficher le nombre d'image dans la zone d'étude
    # s'affiche quand le traitement est fait
    textOutput("compt"),
    
    # zone pour afficher lla taille des images selectionnées
    # s'affiche quand le traitement est fait
    textOutput("size"),
    
    # bouton de selection du dossier de copie
    shinyDirButton('outdir', 
                   'Selectionnez un dossier de sortie',
                   'veuillez choisir une dossier', 
                   FALSE),
    textOutput("outdirpath"),
    br(),
    
    # bouton de selection du dossier de copie
    actionButton('copie','Lancez la copie'),
    br()
  )         
))