% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_wrapping_pcawg.R
\name{pcawg_datasets_to_data_pool}
\alias{pcawg_datasets_to_data_pool}
\title{Add all PCAWG datasets to a data pool}
\usage{
pcawg_datasets_to_data_pool(maf_data_pool)
}
\arguments{
\item{maf_data_pool}{the datapool to add pcawg study too (maf_data_pool)}
}
\value{
data pool with all pcawg datasets added as maf_dataset_wrappers (maf_data_pool)
}
\description{
Add all PCAWG datasets to a data pool
}
\examples{
CRUX:::pcawg_dataset_to_data_pool(CRUX:::new_maf_data_pool(), "Bone-Cart")

}
