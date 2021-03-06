% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attribute-element.R
\name{add_ordinal}
\alias{add_ordinal}
\title{Add Ordinal Measurment Scale}
\usage{
add_ordinal(domain = c("text", "enumerated"), definition, text_pattern = NULL)
}
\arguments{
\item{domain}{Either "text" or "enumerated".}

\item{definition}{If domain is "text", provide a word description of your attribute.
If the domain is "enumerated", provide a list of the attributes code definitions.
Examples are seen in exported documentation.}

\item{text_pattern}{Optional. Only needed if applicable and the domain given is "text".}
}
\description{
Add Ordinal Measurment Scale
}
\examples{
# Ordinal(text):
add_attribute(attribute_name = "LatitudeDD", 
              attribute_definition = "Latitude",
              storage_type = EDIutils::storage_type$string,
              measurement_scale = EDIutils::measurement_scale$ordinal,
              domain= "text", 
              definition = "Latitude")
              
# Ordinal(enumerated): 
code_def_0 = list(code = "0", definition = "0 insects per meter of branch")
code_def_1 = list(code = "1", definition = "1-10 insects per meter")
code_def_2 = list(code = "2", definition = "11 – 100 insects per meter")
code_def_3 = list(code = "3", definition = "more than 100 insects per meter")
code_definition = list(code_def_0, code_def_1, code_def_2, code_def_3)
add_attribute(attribute_name = "hwa",
              attribute_definition = "Hemlock woolly adelgid density per meter of branch",
              storage_type = EDIutils::storage_type$decimal,
              measurement_scale = EDIutils::measurement_scale$ordinal,
              domain = "enumerated",
              definition = code_definition)
}
\keyword{internal}
