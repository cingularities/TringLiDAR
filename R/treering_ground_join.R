#' Load treering_parse and ground_parse file
#'
#' Function creates a mean summary of all cores for a single tree and merges with ground data. 
#' User must select plot if more than one plot in file.
#'
#' @param treering_parse file results from treering_parse
#' @param ground_parse file results from ground_parse
#' @param plot user adds "PLOTID" to subset e.g. "HF" typically a two letter identifier
#' @return A joined summarized treering file and ground metadata
#' @export

treering_ground <- function (treering_parse, ground_parse, plot) {
  
  treering_ground_join <- treering_parse %>% 
    subset(site == plot) %>% #subsets to input plot
    lapply(as.numeric)%>% #makes numeric
    as.data.frame() %>% #makes into df
    group_by(treeID) %>% #groups data by the treeID
    summarise(across(-c(site, core), mean, na.rm = TRUE)) %>% #a mean summary of multiple cores per tree
    rev()%>% #rev the entire data frame
    left_join(ground_parse, by = "treeID") #joins tree ring data with ground metadat using treeID
  
  return(treering_ground_join)
}
