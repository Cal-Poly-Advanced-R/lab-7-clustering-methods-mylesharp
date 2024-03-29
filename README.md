
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clust431

<!-- badges: start -->
<!-- badges: end -->

The goal of clust431 is to provide a mini-package that implements basic
k-means and hierarchical clustering algorithms. These functions aim to
facilitate clustering analysis by allowing users to easily perform
clustering on their data without relying on dedicated clustering
functions like kmeans() or hclust(). The package offers two main
functions:

k_means(): This function performs k-means clustering on the input data.
Users can choose the value of k, and the function randomly selects k
observations from the data as the initial cluster centroids. The
function also provides an option to automatically perform Principal
Component Analysis (PCA) on the data before clustering, using only the
first 2 dimensions. The output of this function includes the cluster
assignments for each observation and the total sum of squares.

hier_clust(): This function implements agglomerative hierarchical
clustering on the input data. It computes the distance matrix and
performs hierarchical clustering using the hclust() function. The
function cuts the hierarchical tree at a certain height to obtain
cluster assignments. The output of this function includes the cluster
assignments for each observation.

By providing these clustering functions in the clust431 package, users
can easily apply k-means and hierarchical clustering algorithms to their
data, enabling them to gain insights and identify patterns within their
datasets.

## Installation

You can install the released version of clust431 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("clust431")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(clust431)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
