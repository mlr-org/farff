# farff: A faster ARFF parser. 

[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/farff)](http://cran.rstudio.com/web/packages/farff/index.html)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/farff)](http://cran.r-project.org/web/packages/farff)
[![Build Status](https://travis-ci.org/mlr-org/farff.svg?branch=master)](https://travis-ci.org/mlr-org/farff)

This is a subproject for better file handling with [mlr](https://github.com/mlr-org/mlr) and [OpenML](http://www.openml.org/).

## Installation instructions

Please install the proper CRAN releases in the usual way. 
If you absolutely have to install from here (you should not):

```splus
devtools::install_github("mlr-org/farff")
```

## What is ARFF

[ARFF](http://www.cs.waikato.ac.nz/ml/weka/arff.html) files are like [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) files, with a little bit of added meta information in a header and standardized NA values. They are quite often used for machine learning data sets and were introduced for the [WEKA machine learning java toolbox](http://www.cs.waikato.ac.nz/ml/weka/).

## RWeka's read.arff and write.arff already exist?

Several reasons motivated the development of *farff*:
* The java dependency of RWeka is annoying.
* The I/O code in RWeka is pretty slow, at least the reading of files in farff is much faster.


## How does it work?

```splus
library(farff)
# import arff format file
d = readARFF("iris.arff")
# export arff format file
writeARFF(iris, path = "iris.arff")
```

## How does it work under the hood?

* We read the ARFF header with pure R code.
* We preprocess the data section a bit with custom C code and write the result into a temporary file TEMP.
* The TEMP file, i.e., the data section, is parsed with [readr](https://cran.r-project.org/web/packages/readr/index.html)::read_delim. Support for [data.table](https://cran.r-project.org/web/packages/data.table/index.html)::fread is planned for future releases.
