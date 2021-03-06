% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publication-date-element.R
\name{add_pub_date}
\alias{add_pub_date}
\title{Add Publication Date}
\usage{
add_pub_date(parent_element, date = NULL)
}
\arguments{
\item{parent_element}{A list representing the EML project or dataset.}

\item{date}{The publication date. If left null, it will be assigned the current
date. This can be overriden by putting in a specific date.}
}
\value{
The dataset or project with the publication date appended.
}
\description{
Adds publication date according to EML standards.
}
\examples{
add_pub_date(parent_element = list(),
             date = "2020-08-19")
             
# Appends current date:              
add_pub_date(parent_element = list()) 
}
