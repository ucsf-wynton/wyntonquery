% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mem_info.R
\name{mem_info}
\alias{mem_info}
\title{Lists the Memory Information for the Current Machine}
\usage{
mem_info()
}
\value{
A data.frame (tibble) with the two columns \code{field} and \code{value}.
For most CPUs, there will be duplicated \code{field}:s.
}
\description{
Lists the Memory Information for the Current Machine
}
\details{
This function reads/queries \verb{/proc/meminfo} on the current machine.
}
\examples{
info <- mem_info()
print(info)

}
