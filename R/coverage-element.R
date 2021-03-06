#' @title Add Coverage Element
#' @description Adds the coverage information of a dataset based off of EML standards.
#' @param parent_element A list representing the EML project or dataset.
#' @param geographic_description A description of the locations	of research sites
#' and areas related to the data.
#' @param west_bounding_coordinate The west cardinality limit.
#' @param east_bounding_coordinate The east cardinality limit.
#' @param north_bounding_coordinate The north cardinality limit.
#' @param south_bounding_coordinate The south cardinality limit.
#' @param begin_date The starting date for the dataset or project. Dates must be
#' provided in ISO 8601 format, YYYY-MM-DD.
#' @param end_date The projected or actual end date for the dataset or project. 
#' Dates must be provided in ISO 8601 format, YYYY-MM-DD.
#' @param taxonomic_coverage Addition of taxonomic coverage is optional. The 
#' taxonomic information for all organisms relevant to the study should be included
#' and mandates the input of the taxonomy levels of kingdom-species. An example 
#' of how to append this information using the add_taxonomic_coverage
#' function is given down below.  
#' @return The dataset or project with coverage information appended.
#' @examples 
#' add_coverage(parent_element = list(), geographic_description = "North Slope
#'              drainage basin: Bounding box encompasses 42 drainage basins
#'              totaling the North Slope drainage basin, Alaska, USA.",
#'              west_bounding_coordinate = "-160.594000",
#'              east_bounding_coordinate = "-134.104800",
#'              north_bounding_coordinate = "71.238300",
#'              south_bounding_coordinate = "67.865000", 
#'              begin_date = "1980-01-01",
#'              end_date = "2010-12-31")
#'              
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
#' add_coverage(parent_element = list(), geographic_description = "Description",
#'              west_bounding_coordinate = "-160.594000", 
#'              east_bounding_coordinate = "-134.104800",
#'              north_bounding_coordinate = "71.238300", 
#'              south_bounding_coordinate = "67.865000",
#'              begin_date = "1980-01-01", 
#'              end_date = "2010-12-31", 
#'              taxonomic_coverage = taxonomic_coverage) 
#'              
#' # For adding multiple taxonomies to the coverage: 
#' chinook <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
#' delta <- add_taxonomic_coverage(CVPIA_common_species = "delta_smelt")
#' taxonomic_coverage <- list(chinook, delta)
#' add_coverage(parent_element = list(), geographic_description = "Description",
#'              west_bounding_coordinate = "-160.594000", 
#'              east_bounding_coordinate = "-134.104800",
#'              north_bounding_coordinate = "71.238300", 
#'              south_bounding_coordinate = "67.865000",
#'              begin_date = "1980-01-01", 
#'              end_date = "2010-12-31", 
#'              taxonomic_coverage = taxonomic_coverage)
#' @export
add_coverage <- function(parent_element, geographic_description, west_bounding_coordinate,
                         east_bounding_coordinate, north_bounding_coordinate,
                         south_bounding_coordinate, begin_date, end_date, taxonomic_coverage = NULL
                         ) {
  required_arguments <- c("geographic_description", "west_bounding_coordinate",
                          "east_bounding_coordinate", "north_bounding_coordinate",
                          "south_bounding_coordinate", "begin_date", "end_date")
  missing_argument_index <- which(c(missing(geographic_description),
                            missing(west_bounding_coordinate),
                            missing(east_bounding_coordinate),
                            missing(north_bounding_coordinate),
                            missing(south_bounding_coordinate),
                            missing(begin_date), missing(end_date)))
  
  if (length(missing_argument_index) > 0) {
    coverage_error <- required_arguments[missing_argument_index][1]
    coverage_error_message <- switch(coverage_error,
                               geographic_description = 
                                 "Please supply a brief description of the locations of research sites and areas related to this dataset.",
                               west_bounding_coordinate = "Please supply the west cardinality limit.",
                               east_bounding_coordinate = "Please supply the east cardinality limit.",
                               north_bounding_coordinate = "Please supply the north cardinality limit.",
                               south_bounding_coordinate = "Please supply the south cardinality limit.",
                               begin_date = "Please suppply the starting date of this project.",
                               end_date = "Please supply the end or projected end date for this project.")
    stop(coverage_error_message, call. = FALSE)
  }
  
  coverage <- list(geographicCoverage =
                     list(geographicDescription = geographic_description,
                          boundingCoordinates =
                            list(westBoundingCoordinate = west_bounding_coordinate,
                                 eastBoundingCoordinate = east_bounding_coordinate,
                                 northBoundingCoordinate = north_bounding_coordinate,
                                 southBoundingCoordinate = south_bounding_coordinate)),
                   temporalCoverage = list(rangeOfDates =
                                             list(beginDate = list(calendarDate = begin_date),
                                                  endDate = list(calendarDate = end_date))))

  if (!is.null(taxonomic_coverage)) {
    coverage$taxonomicCoverage <- taxonomic_coverage
  }
  if (is.null(parent_element$coverage)) {
    parent_element$coverage <- coverage 
  } else {
    parent_element$coverage <- list(parent_element$coverage, coverage)
  }
    
  return(parent_element)
}
