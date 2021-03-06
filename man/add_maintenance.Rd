% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maintenance-element.R
\name{add_maintenance}
\alias{add_maintenance}
\title{Add Maintenance Element}
\usage{
add_maintenance(
  parent_element,
  status = c("complete", "ongoing"),
  update_frequency = NULL
)
}
\arguments{
\item{parent_element}{A list representing the EML project or dataset.}

\item{status}{Provide the status of your project or dataset as either complete or ongoing.}

\item{update_frequency}{Only needed if the status of the project or dataset is ongoing.
If this is the case, please provide the frequency of which the project or dataset is updated.}
}
\value{
The dataset or project with maintenance information appended.
}
\description{
Adds the maintenance information of a dataset based off of EML standards.
}
\examples{
add_maintenance(parent_element = list(),
                status = "complete")

add_maintenance(parent_element = list(),
                status = "ongoing",
                update_frequency = "Data are updated 
                annually at the end of the calendar year.")
}
