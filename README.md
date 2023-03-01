# CRUX


## Description

A shiny app that simplifies cohort-level, tertiary cancer analysis.
You can import your own data, or use public TCGA or PCAWG datasets.

> **Note**
> CRUX is now live at https://ccicb.shinyapps.io/crux/


> **Note**
> For analysing sensitive datasets, we reccomend installing CRUX binaries, or installing it as an R package so you can run CRUX on a system of your choice.


## Manual
A **manual** describing the installation and use of **CRUX** can be found [**here**](https://crux-docs.readthedocs.io/en/latest/index.html)

https://readthedocs.org/projects/crux-docs/badge/?version=latest&style=plastic

## Installation

#### Windows / MAC
Download the [**latest release**](https://github.com/CCICB/CRUX/releases) and follow the instructions for your operating system.

There are **no** dependencies other than having a browser (Chrome, Firefox, Edge or Safari)

#### Linux / Windows / MAC (from source code)
Requires R > 3.5.0 is installed

```
# Install the `remotes` package
install.packages("remotes")

# Install CRUX
remotes::install_github("CCICB/CRUX")
```

To start the app, run:
```
run_app()
```

# Funding
This project was supported by grant 1165556 awarded through the 2018 Priority-driven Collaborative Cancer Research Scheme and co-funded by Cancer Australia and My Room. We would also like to acknowledge Luminesce Alliance – Innovation for Children’s Health for its contribution and support. Luminesce Alliance, is a not-for-profit cooperative joint venture between the Sydney Children’s Hospitals Network, the Children’s Medical Research Institute, and the Children’s Cancer Institute. It has been established with the support of the NSW Government to coordinate and integrate paediatric research. Luminesce Alliance is also affiliated with the University of Sydney and the University of New South Wales Sydney.

![Supporting Organisation Logos](inst/logos/support_organisations_extensive_wide.png)


# Acknowledgements

**Thanks to all the those who contribute to the development of CRUX**

- Computational Biology & Bioinformatics teams at CCIA

- Heena Sareen (extensive testing)

**As well as those who build and maintain the tools which underpin CRUX functionality**

- Anand Mayakonda (maftools, TCGAmutations)
