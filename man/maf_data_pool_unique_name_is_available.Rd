% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_maf_data_pool.R
\name{maf_data_pool_unique_name_is_available}
\alias{maf_data_pool_unique_name_is_available}
\title{Check availability of a name in the data pool}
\usage{
maf_data_pool_unique_name_is_available(maf_data_pool, unique_name)
}
\arguments{
\item{maf_data_pool}{The data pool of interest  (maf_data_pool)}

\item{unique_name}{some string you want to check is not currently used as the 'unique_name' of any object in the datapool (string).}
}
\value{
TRUE/FALSE depending on whether the unique_name is available (logical)
}
\description{
Checks if a particular 'unique_name' is already being used by an object in the some data pool.
}
