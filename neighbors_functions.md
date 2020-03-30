---
title: "Neighbors functions"
author: "Mai Lazarus"
output:
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
---


This script includes the calculations of two indices: Consecutive substratum height difference index and the Neighbors distance index, noth refer to the differences in height and distance between measurements.


```r
library(dplyr)
library(reshape2)
```

**Consecutive substratum height difference index - McCormick index**
This index (McCormick, 1994) represents the vertical heterogeneity in depths along the bottom profile between consecutive depth measurements, and therefore captures an aspect of elements' spatial arrangement. However, McC index does not consider the distances between depth measurements, thus it ignores the horizontal arrangement of complexity-generating elements, which may have considerable ecological meaning. It is calculated as the sum of squared differences between consecutive depth measurements.

![](mcc.png)


**Neighbors distance index**
This index (Yanovski et.al., 2017) is a modification of McCormicks' "Consecutive substratum height difference index", It referrs to consecutive depth measurements differences and also, contrary to McCormicks' index, accounts for the horizontal distances between consecutive depth measurements, therefore it is a measure of the rate of change in depth as a function of the distance, i.e. the slope between measurements.

![](ND.png)


```r
neighbors_indices_fun = function(data){
  neighbors_indices = data %>%
    group_by(ID) %>%
    mutate(depth_diff = (depth - lead(depth))^2,
           depth_diff_ND = abs(depth-lead(depth))) %>%
    summarize(ConsecutiveHeightDiff = sqrt(sum(na.omit(depth_diff))),
              dist = 25/length(depth),
              pairs_num = length(depth)-1,
              Neighbors_dist = sum(na.omit(depth_diff_ND)/dist) / pairs_num) %>%
    select(-pairs_num, -dist)
  return(neighbors_indices)
  }
```


