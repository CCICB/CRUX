% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_validating_clinical_feature_files.R
\name{is_valid_clinicalfeaturefile_return_error}
\alias{is_valid_clinicalfeaturefile_return_error}
\title{Is a clinical_feature_file valid for a given maf}
\usage{
is_valid_clinicalfeaturefile_return_error(clinicalData, maf)
}
\arguments{
\item{clinicalData}{Clinical data associated with each sample/Tumor_Sample_Barcode in MAF. Could be a text file or a data.frame. Default NULL.}

\item{maf}{tab delimited MAF file. File can also be gz compressed. Required. Alternatively, you can also provide already read MAF file as a dataframe.}
}
\value{
Metadata and MAF are valid if they are. The relevant string if they are not (character)
}
\description{
Runs read.maf with a given clinicalData and maf object pair and returns the relevant error messages
If maf object is not valid, this function will also return the appropriate error message.
By using clinicalData=NULL the function allows testing validity of a MAF file in isolation
}
\details{
If clinical feature file has duplicated Tumor_Sample_Barcodes, only the first entry is considered and the file is still 'valid'.
}
