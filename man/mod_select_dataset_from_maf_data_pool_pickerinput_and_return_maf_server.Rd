% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf.R
\name{mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server}
\alias{mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server}
\title{smod_select_dataset_from_data_pool_dropdown_and_return_maf_ Server Functions}
\usage{
mod_select_dataset_from_maf_data_pool_pickerinput_and_return_maf_server(
  id,
  maf_data_pool,
  label = "Dataset"
)
}
\arguments{
\item{maf_data_pool}{the maf_data_pool (maf_data_pool; reactiveVal)}
}
\value{
selected maf object or NULL if none is selected (maf; reactive)
}
\description{
wraps mod_select_dataset_from_maf_data_pool_pickerinput_server.
Instead of returning a unique_name, it returns a maf object, having loaded the dataset into memory if required and updated maf_data_pool to indicate as much
}
