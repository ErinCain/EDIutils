#' Add Keyword Set Element
#' @description Adds keyword set according to EML standards.
#' @param parent_element A list representing the EML project or dataset.
#' @param keyword_set A keyword set or list of keyword sets.
#' @details A keyword set is a list with two elements: 
#' \enumerate{
#'   \item keyword - A list of keywords
#'   \item keywordThesauraus - (optional) A string identifying the controlled 
#'   vocabulary the keywords originate from, do not include if keywords are not
#'   from a controlled vocabulary
#' }
#' 
#' @section Controlled Vocabularies:
#' 
#' In order to promote consistency, please search the following resources for keywords:
#' 
#' \href{https://vocab.lternet.edu/vocab/vocab/index.php}{LTER} - Long Term Ecological Research.
#' 
#' \href{http://aims.fao.org/standards/agrovoc/functionalities/search}{AGROVOC} - 
#' A controlled vocabulary covering all areas of interest of the Food and Agriculture 
#' Organization (FAO) of the United Nations, including food, nutrition, agriculture, 
#' fisheries, forestry, environment.
#' 
#' \href{https://geonames.usgs.gov/apex/f?p=138:1:5668294677959}{U.S. Board on Geographic Names} - 
#' USGS place names dictionary.
#' 
#' @return The project or dataset list with the keyword set appended.
#' @examples 
#' add_keyword_set(parent_element = list(), 
#'                 keyword_set = list(keyword = list('stream discharge', 'discharge'), 
#'                                    keywordThesaurus = 'LTER Controlled Vocabulary'))
#'                                    
#' add_keyword_set(parent_element = list(), 
#'                 keyword_set = list(list(keyword = list('stream discharge', 'discharge'), 
#'                                         keywordThesaurus = 'LTER Controlled Vocabulary'),
#'                                    list(keyword = list('Sacramento River'))))                                    
#' @export
add_keyword_set <- function(parent_element, keyword_set) {
  
  if (length(keyword_set) < 1) {
    warning("Please supply at least one keyword")
  }
  
  if (is.null(parent_element$keywordSet)) {
    parent_element$keywordSet <- keyword_set
  } else {
    parent_element$keywordSet <- list(parent_element$keywordSet, keyword_set)
  }
  
  return(parent_element)
  
}
