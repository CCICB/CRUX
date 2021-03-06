% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/mod_select_datasets_from_maf_data_pool_and_return_maf.R
\name{mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_wrapper_server}
\alias{mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_wrapper_server}
\title{mod_select_datasets_from_maf_data_pool_and_return_maf_server Server Functions}
\usage{
mod_select_datasets_from_maf_data_pool_and_return_maf_dataset_wrapper_server(
  id,
  maf_data_pool,
  max_selected_datasets = -1
)
}
\arguments{
\item{maf_data_pool}{the maf_data_pool (maf_data_pool; reactiveVal)}

\item{max_selected_datasets}{max number of items that can be selected. -1 means unlimited (int)}
}
\value{
a list of maf dataset wrappers or NULL if none were selected (NULL / maf_dataset_wrapper)
}
\description{
Similar to mod_select_datasets_from_maf_data_pool_server but returns list of MAF dataset wrappers
}
