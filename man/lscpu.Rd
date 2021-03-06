% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lscpu.R
\name{lscpu}
\alias{lscpu}
\title{Lists the CPU Information for the Current Machine}
\usage{
lscpu()
}
\value{
A data.frame (tibble) with the two columns \code{field} and \code{value}.
For most CPUs, there will be duplicated \code{field}:s.
}
\description{
Lists the CPU Information for the Current Machine
}
\details{
This function queries \code{lscpu} on the current machine.
}
\examples{
info <- lscpu()
print(info)

}
\seealso{
\code{\link[=cpu_info]{cpu_info()}}
}
