% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_loading_utils.R
\name{maf_data_wrapper_add_rnaseq}
\alias{maf_data_wrapper_add_rnaseq}
\title{Add RNAseq slot to maf_dataset_wrapper}
\usage{
maf_data_wrapper_add_rnaseq(maf_dataset_wrapper, rnaseq_path)
}
\arguments{
\item{maf_dataset_wrapper}{any maf object (maf_dataset_wrapper)}

\item{rnaseq_df}{Dataframe containing at least three columns, named "Tumor_Sample_Barcode", "Hugo_Symbol" and "TPM". May optionally include columns named "Fold_Change" and "Transcript". See ?read_rnaseq_file (dataframe)}
}
\value{
the input maf_dataset_wrapper with rnaseq_path in rnaseq_filepath slot
}
\description{
Add rnaseq data to maf_dataset_wrapper object
}
