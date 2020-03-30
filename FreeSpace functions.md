---
title: "FreeSpace functions"
author: "Mai Lazarus"
output:
  prettydoc::html_pretty:
    theme: Caymen
    highlight: github
---

This script includes functions needed to calculate the FreeSpace index. 



Required packages:

```r
library(reshape2)
library(tidyverse)
library(dplyr)
library(stringr)
library(pracma)
```

This index measures the amount of free space within a bottom profile.
To calculate it, I fitted a curve to the bottom profile and a linear function tangent to the profiles' shallowest depth measurement. Then, I calculated the integral of each of the functions, and subtracted the bottom profile function integral from the linear (top) function integral - which represents the area between the two functions. 


```r
FreeSpace_fun = function (data) {
  
  FreeSpace_data = data %>% 
    group_by(ID) %>%
    mutate(y = depth * (-1),
           upper_y = rep(min(y), n()),
           x = c(1:n())) %>%
              # calc. integral of top function
    summarize(top_f_integral = integral(approxfun(x, upper_y), xmin = 1, xmax = n()),
              #calc. integral of bottom
              profile_integral = integral(approxfun(y), xmin = 1, xmax = n()),
              # subtract bottom profile integral from top function integral
              FreeSpace = profile_integral - top_f_integral) %>%
    select(ID, FreeSpace)
  
  return(FreeSpace_data)
}  
```
