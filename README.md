# CRUX

## Description
An R package that provides a graphical interface for cohort-level tertiary cancer analysis. 
You can import your own data, or use public TCGA or PCAWG datasets.

## Manual
A **manual** describing the installation and use of **CRUX** can be found [**here**](https://crux-docs.readthedocs.io/en/latest/index.html)

## Installation

#### Windows / MAC
Download the [**latest release**](https://github.com/CCICB/CRUX/releases) and follow the instructions for your operating system.

There are **no** dependencies other than having a browser (Chrome, Firefox, Edge or Safari)





## Other:
#### Installing on linux
Requires R > 3.5.0 is installed
```
# Install required libraries
install.packages("remotes", ask=FALSE)
install.packages("BiocManager", ask=FALSE)

# Install tool
BiocManager::install("PoisonAlien/TCGAmutations", ask=FALSE, upgrade=FALSE)
remotes::install_github("CCICB/CRUX", ask=FALSE, upgrade=FALSE)
```

To start the app, run:
```
CRUX::run_app()
```
