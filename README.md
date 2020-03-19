# bwsTools


<img src="https://www.r-pkg.org/badges/version/bwsTools"> <img src="https://cranlogs.r-pkg.org/badges/grand-total/bwsTools"> [![Build Status](https://travis-ci.org/markhwhiteii/bwsTools.svg?branch=master)](https://travis-ci.org/markhwhiteii/bwsTools) <img src="https://codecov.io/gh/markhwhiteii/bwsTools/branch/master/graph/badge.svg">


Tools for Case 1 Best-Worst Scaling (MaxDiff) Designs

### Installation

```
install.packages("bwsTools")
```

### Tutorial

A paper introducing the package and showing basic usage information can be found at the Open Science Framework: https://osf.io/xftvq/

### Contents

- Aggregate estimates, based on: analytical estimation of the multinomial logit model using `ae_mnl()` and Elo scores using `elo()`

- Individual estimates, based on: difference scores (best minus worst) using `diffscoring()`, random walks in directed networks using `walkscoring()`, empirical Bayes using `e_bayescoring()`, Elo scores using `eloscoring()`, and page rank scores using `prscoring()`

- A data.frame of balanced incomplete block designs for creating these studies, `bibds`, and a function to generate a balanced incomplete block design from this, `make_bibd()`
