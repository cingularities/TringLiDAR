#' Load a RWL file
#'
#' This function loads a path file of rwl format. rwl column names are 'XX00YY'XX as plot, 00 tree, YY core.
#' Rows are the year increments
#'
#' @param tree Path to the input file
#' @return A parsed, transposed file with added name columns for tree, plot and core identifier
#' @export

treering_parse <- function(tree) {
  rwl<-dplR::read.rwl(tree) #read file path of rwl
  detrend.rwi <- dplR::detrend(rwl = rwl, method = "Spline") %>% #detrend rwl
    t() %>% #transpose years as columns and row as each core sample 
    as.data.frame() %>% #as data frame
    dplyr::rename_with( ~ paste0("year_", .x)) %>% #adding "year_" to each year column name for unique identifiers
    rownames_to_column()%>% #converts sample row name into a column to be parsed
    tidyr::separate(rowname,
                    into = c("site", "coreID"),
                    sep = "(?<=[A-ZA-Z])(?=[0-9])") %>% ## separates column of site and treeID
    tidyr::separate(coreID,
                    into = c("treeID", "core"),
                    sep = "(?<=[0-9])(?=[A-ZA-Za-z])")  ##additional splot of CoreID to separate core sample from tree
  return(detrend.rwi)}