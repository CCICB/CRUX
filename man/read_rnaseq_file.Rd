% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_loading_utils.R
\name{read_rnaseq_file}
\alias{read_rnaseq_file}
\title{Read RNAseq file}
\usage{
read_rnaseq_file(rnaseq_file)
}
\arguments{
\item{rnaseq_file}{(string)}
}
\value{
Dataframe containing at least three columns, named "Tumor_Sample_Barcode", "Hugo_Symbol" and "TPM". May optionally include columns named "Fold_Change" and "Transcript" (dataframe)
}
\description{
Read RNAseq file
}
