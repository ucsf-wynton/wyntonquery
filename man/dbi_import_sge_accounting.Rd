% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sge_accounting_sql.R
\name{dbi_import_sge_accounting}
\alias{dbi_import_sge_accounting}
\title{Imports an SGE Accounting file into a database}
\usage{
dbi_import_sge_accounting(conn, file, chunk_size = 10000L, deduplicate = FALSE)
}
\arguments{
\item{conn}{A \link[DBI:DBIConnection-class]{DBI::DBIConnection}.}

\item{file}{(character) The SGE \file{accounting} file to read.}

\item{chunk_size}{(integer) The number of records to import in each chunk.}

\item{deduplicate}{(logical) If TRUE, duplicated records in the database
are removed at the end, otherwise not.}
}
\value{
(invisibly) The number of SGE accounting records ("jobs") in
the input file.
}
\description{
Imports an SGE Accounting file into a database
}
\seealso{
Internally, \code{\link[=read_raw_sge_accounting]{read_raw_sge_accounting()}} is used to parse the SGE
Accounting file.
}
