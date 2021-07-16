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

#### Linux / Windows / MAC (from source code)
Requires R > 3.5.0 is installed
```
# Install tool
BiocManager::install("PoisonAlien/TCGAmutations")
remotes::install_github("CCICB/CRUX")
```

To start the app, run:
```
run_app()
```
