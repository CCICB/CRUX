% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_plot_survival.R
\name{mod_plot_survival_server}
\alias{mod_plot_survival_server}
\title{plot_survival Server Functions}
\usage{
mod_plot_survival_server(id, maf, geneset, time, status, or, is_tcga)
}
\arguments{
\item{id}{Internal parameters for {shiny}.}

\item{maf}{MAF object, usually produced by maftools::read_maf (reactive MAF object)}

\item{geneset}{character vector where each element is the hugo_symbol of a gene.  (reactive character vector)}

\item{time}{name of column in clinical data describing time to event (reactive string)}

\item{status}{name of column in clinical data describing event status (reactive string)}

\item{or}{should samples be classified as mutant if ANY gene in geneset is mutated (default is ALL must be mutated) (reactive flag)}

\item{is_tcga}{is sample from TCGA? (flag)}
}
\description{
plot_survival Server Functions
}
