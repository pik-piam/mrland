# MadRaT land data package

R package **mrland**, version **0.5.8**

[![Travis build status](https://travis-ci.com/pik-piam/mrland.svg?branch=master)](https://travis-ci.com/pik-piam/mrland) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3822083.svg)](https://doi.org/10.5281/zenodo.3822083) 

## Purpose and Functionality

The package provides land related data via the madrat framework.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrland")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **mrland** in publications use:

Dietrich J, Mishra A, Weindl I, Bodirsky B, Wang X, Baumstark L, Kreidenweis U, Klein D, Steinmetz N, Chen D, Humpenoeder
F, Wirth S (2020). _mrland: MadRaT land data package_. https://github.com/pik-piam/mrland,
https://doi.org/10.5281/zenodo.3822083.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrland: MadRaT land data package},
  author = {Jan Philipp Dietrich and Abhijeet Mishra and Isabelle Weindl and Benjamin Leon Bodirsky and Xiaoxi Wang and Lavinia Baumstark and Ulrich Kreidenweis and David Klein and Nele Steinmetz and David Chen and Florian Humpenoeder and Stephen Wirth},
  year = {2020},
  note = {https://github.com/pik-piam/mrland, https://doi.org/10.5281/zenodo.3822083},
}
```

