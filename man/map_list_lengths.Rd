% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_list_lengths.R
\name{map_list_lengths}
\alias{map_list_lengths}
\title{Generate a data.frame map of a list's lengths, or the lengths of lists within a list}
\usage{
map_list_lengths(data1, data.frame = TRUE)
}
\arguments{
\item{data1}{a list or list of lists composed of the same elements}

\item{data.frame}{logical, if TRUE return output as data.frame}
}
\value{
Returns a table or data.frame with the lengths of each list element
}
\description{
Generate a data.frame map of a list's lengths, or the lengths of lists within a list
}
\examples{
map_list_lengths(htests[[1]])
map_list_lengths(htests[[5]])
map_list_lengths(htests[[5]][[1]])

}

