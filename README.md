# farff: A faster ARFF parser

[![Build Status](https://travis-ci.org/mlr-org/farff.svg?branch=master)](https://travis-ci.org/mlr-org/farff)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/farff)](http://cran.r-project.org/web/packages/farff)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/farff)](http://cran.rstudio.com/web/packages/farff/index.html)

* [Offical CRAN release site](http://cran.r-project.org/web/packages/farff/)
* Install the development version

    ```splus
    devtools::install_github("mlr-org/farff")
    ```i

## What is ARFF

ARFF files are like CSV files, with a little bit of added meta information in a header and standardized NA values. They are quite often used for machine learning data sets and were introduced for the WEKA machine learning java toolbox. You can read more here

http://www.cs.waikato.ac.nz/ml/weka/arff.html

and here

http://weka.wikispaces.com/ARFF

## Why farff? RWeka::read.arff and RWeka::write.arff already exist?

* The java dependency is annoying
* The I/O code in RWeka is pretty slow, at least the reading of files in farff is much faster.
* 

## How does it work

* We read the ARFF header with pure R code
* We preprocess the data section a bit with custom C code and write the result into a TEMP file
* The TEMP file (i.e. the data section) is parsed with data.table::fread or readr::read_delim. The former being faster, the latter being more robust if your file contains strange chars. 
We preprocess the 





