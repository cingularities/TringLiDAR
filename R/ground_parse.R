#' Load a EXCEL ground metadata file
#'
#' This function loads a path file of EXCEL format xlsx field metadata. Field metadata needs to have columns:
#' Azimuth, Distance, Species, Height, DBH and TreeID in the exact format
#'
#' @param coor c(x,y) coordinate of plot center
#' @param ground Path to ground metadata
#' @return A file with new lat an lon coordinates of each tree based on distance and Azimuth
#' @export

ground_parse <- function(coor, ground) {
  
  dist_azim <- readxl::read_excel(ground) #read xlsx ground metadata file
  coor_list <- list() #creates empty list for new coordinates
  for (i in 1:nrow(dist_azim)) { #for loop to create a lat long for each azimuth and distance from plot center lat and lon
    stemAzimuth <- dist_azim$Azimuth[i] #grab azimuth
    stemDistance <- dist_azim$Distance[i] #grab distance
    
    new_coordinates <- geosphere::destPoint(coor, stemAzimuth,  stemDistance) %>% #estimates new lat lon coordinate
      as.data.frame() %>% #make as data frame
      add_column(treeID = dist_azim$TreeID[i]) %>% #add treeID column of that tree
      add_column(stemAzimuth = stemAzimuth) %>% #add Azimuth column of that tree
      add_column(stemDistance = stemDistance) %>% #add Distance column of that tree
      add_column(species = dist_azim$Species[i]) %>% #add Species column of that tree
      add_column(Height = dist_azim$Height[i])%>% #add Height column of that tree
      add_column(DBH = dist_azim$DBH[i]) #add DBH column of that tree
    coor_list[[i]] <- new_coordinates #put result of loop in list
  }
  coor_df<- coor_list %>% bind_rows() %>% na.omit() #remove NA rows
  return(coor_df)
}
