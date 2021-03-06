% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster-element.R
\name{add_raster}
\alias{add_raster}
\title{Add Raster Element}
\usage{
add_raster(
  parent_element,
  file_name,
  file_description,
  attribute_list,
  physical,
  spatial_reference,
  horizontal_accuracy,
  vertical_accuracy,
  cell_size_x,
  cell_size_y,
  number_of_bands,
  raster_origin,
  rows,
  columns,
  verticals,
  cell_geometry
)
}
\arguments{
\item{parent_element}{A list to append \code{spatialRaster} to}

\item{file_name}{The name of your raster file}

\item{file_description}{A short description of your file}

\item{attribute_list}{A list of all the columns on your datatable.
Use \code{\link{add_attribute}} to generate this attribute list and see the
documentation for \code{\link{add_attribute}} for more information.}

\item{physical}{A list of the physical descriptions of your file. Use
\code{\link{add_physical}} to generate this physical list and see the
documentation for \code{\link{add_physical}} for more information.}

\item{spatial_reference}{This is the name of a predefined coordinate system.
The acceptable values for \code{spatial_reference} can be found at
\href{https://eml.ecoinformatics.org/schema/index.html}{EDI Schema}.}

\item{horizontal_accuracy}{The accuracy of horizontal locational measurements within the data.}

\item{vertical_accuracy}{The accuracy of vertical locational measurements within the data.}

\item{cell_size_x}{The width of the cell in the x direction.}

\item{cell_size_y}{The width of the cell in the y direction.}

\item{number_of_bands}{The number of bands in the image.}

\item{raster_origin}{The corner location of the pixel it should have the
minimum x and y values}

\item{rows}{Max number of raster object along the y-axis}

\item{columns}{Max number of raster objects along the x-axis}

\item{verticals}{Max number of raster objects along the z-axis}

\item{cell_geometry}{Geometric representation of the cell's content}
}
\value{
A list that contains all the required elements of the spatialRaster
section of an EML document.
}
\description{
This function adds all the required elements for a \code{spatialRaster} section of
an ELM document.
}
\examples{
add_raster(parent_element = list(),
           file_name = "Rasterfiles.zip" ,
           file_description = "A Raster File",
           attribute_list =  add_attribute(attribute_name = "Yrs", attribute_label = "Years", 
                                           attribute_definition = "Calendar year of the observation from years 1990 - 2010.", 
                                           storage_type = EDIutils::storage_type$date, 
                                           measurement_scale = EDIutils::measurement_scale$dateTime, 
                                           date_time_format = "YYYY",
                                           date_time_precision = "1", minimum = "1993", maximum = "2003"),
           physical = add_physical("Rasterfiles.zip"),
           spatial_reference = "NAD_1983_StatePlane_California_I_FIPS_0401",
           horizontal_accuracy = "No Information",
           vertical_accuracy =  "No Information",
           cell_size_x = "25",
           cell_size_y = "25",
           number_of_bands = "10",
           raster_origin = "Upper Left",
           rows = "200",
           columns = "6",
           verticals = "1",
           cell_geometry = "pixel")
           
}
