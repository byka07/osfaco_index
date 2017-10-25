####################################################################################################################
# # SCRIPT POUR COPIER AUTOMATIQUEMENT LES IMAGES D'UNE COMMANDE OSFACO # ##### ####################################

# # POUR FONCTIONNER L'OPERATEUR DEVRA PRODUIRE LE SHAPEFILE DE SA ZONE D'ETUDE
# # ET L'INSERER A LA LIGNE 29 CORRESPONDANT A L'INPUT DU aoi

#### Abraham BIO - abraham.bio@reddplus.ci
#### 2017 1024 - version revisée
####################################################################################################################
####################################################################################################################
# # Packages 

options(stringsAsFactors =FALSE)
library(foreign)
library(dplyr)
library(stringr)
library(sp)

#####################################################################################################################
# ## Parametrer le chemin d'accès  
getwd()

#####################################################################################################################
# ##  Fichers d'entrée et sortie
# repertoire des images OSFACO
osfaco_rep <- paste(getwd(),"P2015_RCI",sep="")

#shape zone d'éetude
aoi <- readOGR("Mes_Scripts/osfaco_app/JacquesZone/JacqueZone.shp",layer="JacqueZone")

# selection par localisation entre la zone d'intérêt et les scènes des images
slect <- osfaco[aoi,] 

# le fichier CSV de la selection
liste_command <- slect@data

# dossier de sortir (ou la copie doit être faite)
### inscrire le nom de doosier dans outfile - sinon vous ecraserez le contenu du dossier actuel 
### ou aurez en plus de vos image d'autres images du dossier actuel
Outfile <- "Assamoi"

Outfile <- paste(getwd(),Outfile,sep="")

#####################################################################################################################
# ## transformer les "\"en "/" et forcer la lettre du chemin dans la colonne Path
liste_command$Path <- gsub("[\\]", "/", liste_command$Path)
liste_command$Path <- gsub(str_sub(liste_command[1,2], 1, 3), getwd(), liste_command$Path)

#####################################################################################################################
# ## Passer à la selection et copie des images concernées
debut_des_copies  <- Sys.time()
for(img in liste_command$Image){
  ## marqueur du temps inital de traitement
  debut_traitement  <- Sys.time()
  
      dir <- liste_command[liste_command$Image==img,"Path"]
      file.copy(paste(dir,img, sep="/"),Outfile)
  
  ## temps d'une copie
  print(paste("Temps de copie de",img,"est de",Sys.time()-debut_traitement))
}
print(paste("Temps total des copies",Sys.time()-debut_des_copies))
