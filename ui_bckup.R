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
    textOutput("filepath"),
    br(),
    
    # zones de selection des coordonnés
    # s'affichent quand le fichier selectionné es lu
    uiOutput("Xcoor"),
    uiOutput("Ycoor"),
    br(),
    
    # zone pour afficher le nombre d'image dans la zone d'étude
    # s'affiche quand le traitement est fait
    textOutput("compt"),
    
    # bouton de selection du dossier de copie
    shinyDirButton('outdir', 
                   'Slectionnez un dossier de sortie',
                   'veuillez choisir une dossier', 
                   FALSE),
    textOutput("outdirpath"),
    br(),
    
    # bouton de selection du dossier de copie
    actionButton('copie','Lancez la copie'),
    br()
  )         
))