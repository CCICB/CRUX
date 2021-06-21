# CRUX

## Description
An R package that provides a graphical interface for cohort-level tertiary cancer analysis. 
You can import your own data, or use public TCGA or PCAWG datasets.


## Installation

```
# Install required libraries
install.packages("remotes", ask=FALSE)
install.packages("BiocManager", ask=FALSE)

# Load Libraries
library(remotes)
library(BiocManager)

# Install tool
install("PoisonAlien/TCGAmutations", ask=FALSE, upgrade=FALSE)
install_github("CCICB/CRUX", ask=FALSE, upgrade=FALSE)
```

## Usage

Run a single line of code to open the app and start exploring somatic cancer datasets
```
CRUX::run_app()
```
