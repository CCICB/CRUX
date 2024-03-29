% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_maftools_extending_functions.R
\name{maftools_extract_geneset_by_altered_samples}
\alias{maftools_extract_geneset_by_altered_samples}
\title{Extract geneset}
\usage{
maftools_extract_geneset_by_altered_samples(maf, topn = 100)
}
\arguments{
\item{maf}{tab delimited MAF file. File can also be gz compressed. Required. Alternatively, you can also provide already read MAF file as a dataframe.}

\item{topn}{How many genes to include in the gene-set.}
}
\value{
The names of genes mutated in the most samples (character vector).
}
\description{
Create a genelist containing the genes mutated in the most samples in a maf. 
If topn > total number of mutated genes then all mutated genes are returned.
}
\examples{
CRUX:::maftools_extract_geneset_by_altered_samples(maftools::tcgaLoad("GBM"), topn=50)
}
