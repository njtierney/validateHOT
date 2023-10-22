---
title: 'validateHOT - Validate your Holdout Task'
tags:
  - R
  - MaxDiff
  - Conjoint Analysis
  - Market Simulations
  - Predictive Validity
authors:
  - name: Joshua Schramm
    orcid: 0000-0001-5602-4632
    corresponding: True
    affiliation: 1
  - name: Marcel Lichters
    orcid: 0000-0002-3710-2292
    corresponding: FALSE
    affiliation: 1, 2    
affiliations:
 - name: Chemnitz University of Technology, Germany
   index: 1
 - name: Otto von Guericke University of Magdeburg, Germany
   index: 1
citation_author: Schramm & Lichters.
date: 22 October 2023
year: 2023
bibliography: paper.bib
link-citations: true
output: rticles::joss_article
journal: JOSS
---



# Summary

validateHOT is a package that provides functions to both validate a validation/holdout task and run market simulations for results obtained in a (adaptive) choice-based conjoint analysis (hereafter ACBC and CBC, respectively) and maximum difference scaling (hereafter MaxDiff) using [Sawtooth Software](https://sawtoothsoftware.com/). The ultimate goal of preference measurement techniques such as (A)CBC or MaxDiff is to predict future behavior [@green1990]. Therefore, it is essential for both academics and practitioners to ensure that the collected data is valid and can also predict outside tasks. The easiest way to test this is to include so-called holdout or validation task [@Orme2015]. Despite the important role of validation tasks, most of the conjoint studies done in practice do not include them [@yang2018], which is unsatisfactory, given the fact that the model is used to estimate market shares which is the basis for relevant marketing decisions.

validateHOT combines both validation and market simulation in one package. validateHOT's advantages are the following: a) it helps you to decide which is the best model to proceed by validating it, b) it runs relevant market simulations that help to find the right product combinations, and finally, c) it is open source tool for function that are usually implemented in Sawtooth Software and are ofen a black-box for researchers and practitioners.

# Statement of need

# Overview and features


```r
data("MaxDiff")
HOT <- createHOT(
  data = MaxDiff, # data frame
  id = 1, # index unique identifier
  none = 19, # index of none alternative
  prod = 7, # no of alternatives in HOT excluding none
  prod.levels = list(3, 10, 11, 15, 16, 17, 18), # index of alternatives
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
