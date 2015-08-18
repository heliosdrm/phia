[![Build Status](https://travis-ci.org/heliosdrm/phia.svg?branch=master)](https://travis-ci.org/heliosdrm/phia)

# R package &ldquo;phia&rdquo;
## Post-hoc Interaction Analysis in R
### Getting started

The official release of the package is on CRAN:
http://cran.r-project.org/web/packages/phia/

To get it installed, just write in an R session:

```R
install.packages("phia", repos="http://cran.r-project.org")
```

To work with the newest development version, check the build status icon at the top of this page. If it says &ldquo;passing&rdquo;, you should be able to install it from the source. Clone this Git repository in your machine, and if you have the tools to build R packages, do it and install it as appropriate for your OS.

If you cannot build it, you may still install it from an R session. Set the working directory to the parent folder where `phia` is copied, and then do:

```R
install.packages("phia", repos=NULL, type="source")
```

In this case, the package dependencies and the PDF docs will not be installed. You should at least install package `car` (from CRAN) separately to have a  fully operative package.

After installing, you have to load to use the package, with:

```R
library(phia)
```

For further details about how to use the package, read the [vignette](http://cran.r-project.org/web/packages/phia/vignettes/phia.pdf) that comes with it.

And feel free to comment on any issues, file bugs or suggest improvements. All contributions are welcome! If you don't have a GitHub account, try submitting your issues through [Git Reports](https://gitreports.com/issue/heliosdrm/phia).