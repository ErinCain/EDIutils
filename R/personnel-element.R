#' Add Personnel Element
#' @description Adds personel according to EML standards
#' @param parent_element A list representing the EML project or dataset
#' @param role Use "creator" if you are one of the primary originators of the data. 
#' Other possible roles "Data Manager", "Field Technician", or "Assistant Researcher". 
#' There can be multiple personnel on a project with the same role.
#' @param first_name Person's given name
#' @param last_name Person's surname
#' @param email Person's email address
#' @param organization Person's employer or the entity they are associated
#' with for this dataset or project
#' @param orcid (Optional) ORCID iD is a persistent digital identifier for researchers,
#' register at \url{http://orcid.org/}
#' @examples 
#' add_personnel(parent_element = list(),
#'               first_name = 'Katherine', 
#'               last_name = "Johnson", 
#'               email = 'kjohnson@nasa.gov', 
#'               role = 'creator', 
#'               organization = 'NASA',
#'               orcid = '12345')
#' 
#' add_personnel(parent_element = list(), 
#'               first_name = "Edith", 
#'               last_name = "Windsor", 
#'               email = 'ewindsor@ibm.com', 
#'               role = 'Data Manager', 
#'               organization = 'IBM')
#' @export
add_personnel <- function(parent_element, first_name, last_name, email, 
                          role, organization, orcid = NULL) {
  
  required_arguments <- c("first_name", "last_name", "email", "role", "organization")
  missing_argument_index <- which(c(missing(first_name), missing(last_name), 
                                missing(email), missing(role), 
                                missing(organization)))
  
  if (length(missing_argument_index) > 0) {
    person_error <- required_arguments[missing_argument_index][1]
    person_error_message <- switch(person_error,
                                   first_name = "Please supply a first name.",
                                   last_name = "Please supply a last name.",
                                   email = "Please supply an email.",
                                   role = "Please supply a role. Use 'creator' if you are the main originator of the dataset or project",
                                   organization = "Please supply the name of the organization employing the personnel")
    stop(person_error_message, call. = FALSE)
  } 
  
  person <- list(individualName = list(givenName = first_name, 
                                       surName = last_name),
                 electronicMailAddress = email, 
                 organizationName = organization)
  
  if (tolower(role) == "creator") {
    parent_element$contact <- person
    if (!is.null(orcid)) {
      person$'@id' <- orcid 
    }
    
    if (is.null(parent_element$creator)) {
      parent_element$creator <- person
    } else {
      parent_element$creator <- list(parent_element$creator, person)
    }
  } else {
    person$role <- role
    if (is.null(parent_element$associatedParty)) {
      parent_element$associatedParty <- person
    } else {
      parent_element$associatedParty <- list(parent_element$associatedParty, person)
    }
  }
  
  return(parent_element)
  
}
