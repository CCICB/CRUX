% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_wrapping_zero.R
\name{zero_dataset_to_maf_dataset_wrapper}
\alias{zero_dataset_to_maf_dataset_wrapper}
\title{ZERO dataset to maf_dataset_wrapper}
\usage{
zero_dataset_to_maf_dataset_wrapper(maf_data_pool, zero_study_abbreviation)
}
\arguments{
\item{maf_data_pool}{the data pool to add the dataset wrapper to (maf_data_pool)}

\item{zero_study_abbreviation}{a ZER cohort abbreviation (see ZEROmutations::zero_available()) (string)}
}
\value{
functions and values associated with specified ZERO cohort (maf_dataset_wrapper)
}
\description{
This function takes a ZERO childhood cancer 'cancer type' abbreviation and returns the relevant data in the form of a 'maf_dataset_wrapper'.
}
\details{
This function is powered by ZEROmutationsCCI, which pulls data from the rdrive. The problem with this, is it means that
\strong{maf_dataset_wrapper$load_data will ERROR if Rdrive isn't mounted}
}
\seealso{
Other DataToWrapper: 
\code{\link{tcga_dataset_to_data_pool}()},
\code{\link{tcga_dataset_to_maf_dataset_wrapper}()},
\code{\link{tcga_datasets_to_data_pool}()},
\code{\link{user_data_filepath_to_class_maf_dataset_wrapper}()},
\code{\link{user_to_dataset_to_data_pool}()},
\code{\link{zero_dataset_to_data_pool}()},
\code{\link{zero_datasets_to_data_pool}()}
}
\concept{DataToWrapper}
