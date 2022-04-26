#' Input tree ring and ground joined file
#'
#' This function loads a path file of EXCEL format xlsx field metadata. Field metadata needs to have columns:
#' Azimuth, Distance, Species, Height, DBH and TreeID in the exact format
#' Rows are the year increments
#'
#' @param treering_ground_join inputs from tree_ring_ground function results
#' @return A file a merged ground data and tree ring data reconstructed as DBH
#' @export


DBH_recon <- function(treering_ground_join){

  treering_ground_join[is.na(treering_ground_join)] <- 0  #zero all NA
  year_cols <- which(substr(x = colnames(treering_ground_join), start = 1, stop = 4) == "year")  # Identify the columns that have data of interest which are year columns
  treering_ground_join$lp <- rowSums(x = treering_ground_join[, year_cols], na.rm = TRUE)  #row sums all the years into column lp, removing NA
  lh <- treering_ground_join  # Make a copy of that data frame that we will use for differences
  lh[, year_cols] <- sapply(1:ncol(lh[, year_cols]), function(col){rowSums(lh[, year_cols][1:col])}) #convert years to cumulative sum of year columns
  diff <- lh # Make a copy of that data frame that we will use for difference
  diff[, year_cols] <- (diff$lp) - (diff[, year_cols]) #calculate difference
  fraction <- lh # Make a copy of that data frame that we will use for difference
  fraction[, year_cols] <- diff[, year_cols]/diff$lp #calculate difference
  diameter <- lh # Make a copy of that data frame that we will use for difference
  diameter[, year_cols] <- as.numeric(fraction$DBH)*fraction[, year_cols] #calculate difference

  # Reality check to see that percentages add up to 100

  return(diameter)
}

