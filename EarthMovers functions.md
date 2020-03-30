---
title: "EarthMovers functions"
author: "Mai Lazarus"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
---



Earth movers distance index (EarthMovers) is a measure of distances between distributions. It is derived from the Wasserstein distance metric (Villani, 2003), which represents the minimal cost of transforming one distribution to another (Rubner et al. 2000). 

EarthMovers qunatifies the difference between the distribution of the depths along a bottom profile to a theoretical distribution composed only from the mode value of the depth's distribution. As the depth values along the transect are more heterogenous, it will 'cost' more to transform it to the theoretical uniform distribution.

For example, EarthMovers value would be larger for the first bottom profile than the second one: the plots present the bottom profile, depths distribution and the theoretical depths distribution.

![](C:/Users/Maila/Google Drive/structural_complexity/eilat/wass wide.png)

![](C:/Users/Maila/Google Drive/structural_complexity/eilat/wass narrow.png)

Required packages:

```r
library(transport)
library(tidyverse)
```

Get the mode value function

```r
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
```


```r
wass_fun = function (data) {
wass_data = data %>% group_by(ID) %>%
  mutate(common = c(rep(getmode(depth), n()))) %>%
  summarize(earth_movers = wasserstein1d(depth,common)) 
}
```
