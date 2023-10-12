---
title: 'validateHOT - Validate your Holdout Task'
tags:
  - R
authors:
  - name: Joshua Schramm
    orcid: 0000-0001-5602-4632
    corresponding: True
    affiliation: 1
affiliations:
 - name: Chemnitz University of Technology, Germany
   index: 1
citation_author: Schramm, J.
date: 10 October 2023
year: 2023
bibliography: paper.bib
link-citations: true
output: rticles::joss_article
journal: JOSS
---




# Summary
validateHOT is a package that provides functions to both validate a validation/holdout task and run market simulations for results obtained a (adaptive) choice-based conjoint analysis and MaxDiff using [Sawtooth Software](https://sawtoothsoftware.com/). Having valid data is essential to predict future choice behavior.

dfsdfdjf


# Statement of need


# Overview and features


```r
data("MaxDiff")
HOT <- createHOT(
  data = MaxDiff, # data frame
  id = 1, # column index of unique identifier
  None = 19, # column index of none alternative
  prod = 7, # no of alternatives in HOT excluding none
  prod.levels = list(3, 10, 11, 15, 16, 17, 18), # column index of alternatives
  method = "MaxDiff", # method applied
  choice = 20 # column index of choice alternative
)
```


```r
hitrate(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice # column name of choice
) %>% 
  round(3)
```

```
## # A tibble: 1 x 5
##      HR    se chance   cor     n
##   <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1  55.7  5.98   12.5    39    70
```


```r
turf(
  data = MaxDiff,
  opts = c(Option_01:Option_16),
  none = none,
  size = 3,
  approach = "thres"
) %>% 
  head(., n = 5) %>% 
  mutate_if(is.numeric, round, 2)
```

```
##     combo reach freq Option_01 Option_02 Option_03 Option_04 Option_05
## 1 Combo 1 84.29 1.50         1         0         0         1         0
## 2 Combo 2 82.86 1.64         0         0         0         1         0
## 3 Combo 3 82.86 1.60         0         0         0         1         0
## 4 Combo 4 82.86 1.46         1         0         0         1         0
## 5 Combo 5 81.43 1.63         0         0         0         0         0
##   Option_06 Option_07 Option_08 Option_09 Option_10 Option_11 Option_12
## 1         0         0         0         0         0         0         0
## 2         0         0         0         0         0         0         0
## 3         1         0         0         0         0         0         0
## 4         1         0         0         0         0         0         0
## 5         0         0         1         0         0         0         0
##   Option_13 Option_14 Option_15 Option_16
## 1         1         0         0         0
## 2         1         0         1         0
## 3         0         0         1         0
## 4         0         0         0         0
## 5         1         0         1         0
```



# Availability
validateHOT is available on [Github](https://github.com/JoshSchramm94/validateHOT). 


# References
