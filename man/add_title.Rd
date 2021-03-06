% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/title-element.R
\name{add_title}
\alias{add_title}
\title{Add Title Elements}
\usage{
add_title(parent_element, title, short_name)
}
\arguments{
\item{parent_element}{A list representing the EML project or dataset.}

\item{title}{The title of the project and/or dataset. A complete title is between
7 and 20 words long and includes: What, Where, and When (and Who, if relevant).}

\item{short_name}{Short name or nickname you use to refer to this dataset.}
}
\value{
The project or dataset list with title and short name appended.
}
\description{
Adds title elements according to EML standards.
}
\details{
For a dataset, the title should be between 7 and 20 words long, with a short
name which is shorter than the title. If the title is too short, ensure that it
is covering the 'what', 'where', and 'when'. If the title is too long, attempt
to make it as concise as possible.
}
\examples{
add_title(parent_element = list(),
          title = "Eight Mile Lake Research Watershed, Carbon in Permafrost Experimental Heating Research.",
          short_name = "knb-lter-bnz.501.17") 
}
