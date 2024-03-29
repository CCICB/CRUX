% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_maf_data_pool.R
\name{maf_data_pool_get_unique_names}
\alias{maf_data_pool_get_unique_names}
\title{Get Unique Names from Maf Data Pool}
\usage{
maf_data_pool_get_unique_names(maf_data_pool)
}
\arguments{
\item{maf_data_pool}{A data pool to mine unique_names from (maf_data_pool)}
}
\value{
a vector listing the 'unique_name' of each maf_dataset_wrapper in the data pool (character)
}
\description{
Extract 'unique_name' from each maf_dataset_wrapper within a maf_data_pool.
\strong{NOTE: } 'unique_name' is a property of maf_dataset_wrapper, however since these wrappers are created independently from each other (no check for uniqueness at the time of creation) they may not actually be unique. 
Please check this using maf_data_pool_validate_unique_names
}
\concept{maf_data_pool_utils}
