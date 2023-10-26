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
   index: 2
citation_author: Schramm & Lichters
date: 22 October 2023
year: 2023
bibliography: paper.bib
link-citations: true
output: rticles::joss_article
journal: JOSS
---

```{=tex}
\setlength{\headheight}{63.55022pt}
\addtolength{\topmargin}{-0.95425pt}
\newcommand{\colcod}[1]{\texttt{\color{purple}#1}}
```


# Summary

validateHOT is a package that provides functions to both validate a validation/holdout task and run market simulations for results obtained in a (adaptive) choice-based conjoint analysis (hereafter ACBC and CBC, respectively) and maximum difference scaling (hereafter MaxDiff) using [Sawtooth Software](https://sawtoothsoftware.com/).

Preference measurement techniques', such as (A)CBC or MaxDiff, ultimate goal is to predict future behavior [@green1990]. Hence, it is essential for both academics and practitioners to ensure that the collected data is valid and can also predict outside tasks. The easiest way to test this is to include so-called holdout or validation task [@Orme2015]. Despite the important role of validation tasks, practictioners often do not include them [@yang2018], which is unsatisfactory, given the fact that the model is used to estimate market shares which is the basis for relevant marketing decisions.

validateHOT combines both validation and market simulation in one package. validateHOT's advantages are the following: a) it helps you to decide which is the best model to proceed by validating it, b) it runs relevant market simulations that help to find the right product combinations, and finally, c) it is open source tool for functions that are usually implemented in Sawtooth Software and are often a black-box for researchers and practitioners.

# Statement of need

validateHOT is a practical tool for Sawtooth Software users in industry as well as academia. It provides an open source solution for a) validating a validation/holdout task and therefore ensuring that the model has predictive validity; b) running market simulations. Other packages, for example, Metrics [@Metrics] provide functions to run validation metrics such as *mean absolute error*, *root mean squared error*, or the five metrics of the confusion matrix (see \autoref{tab:table1}). However, to put the Sawtooth export into the right format, the user needs some data wrangling which can be a barrier. Other packages mainly focus on the analysis of conjoint analysis (e.g., ChoiceModelR [@ChoiceModelR], choicetools [@choicetools], logitR [@logitr], bayesm [@bayesm] etc.). To the best of our knowledge, a package that converts raw utility scores into validation metrics or running a variety of marketing simulations is still missing. The goal of validateHOT is still to fill this research gap. validateHOT creates market shares based on *share of preferences* or *first choice rule*, which can be helpful to report results for research articles according to open science standards. In addition, it also provides the function \texttt{\color{purple}turf}, which was offered formerly by the turfR package, which is no longer available on the Comprehensive R Archive Network (see [CRAN](https://cran.r-project.org/web/packages/turfR/index.html)). So, currently practitioners and academics mainly have to stick to paid solutions, for example, Sawtooth Software.

# Key functions

The main function that creates the validation/holdout task is the \texttt{\color{purple}createHOT} function, which creates the total utility of each alternative by applying the additive utility model [@rao2014, p. 82]. The other functions in the package mainly focus on four different components which are outlined in \autoref{tab:table1}.

| Validation metrics | Confusion matrix | Market Simulations | Rescaling scores |
|:----------------:|:----------------:|:----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |                  |
|      medae()       |     recall()     |       turf()       |                  |
|       mhp()        |  specificity()   |                    |                  |
|       rmse()       |                  |                    |                  |

: Overview of main four components of validateHOT and their corresponding functions \label{tab:table1}

# Typical workflow

In the following, we provide the workflow for a MaxDiff study (the vignette also provides detailed examples for a CBC as well as an ACBC).

After running the Hierarchical Bayes estimation in Sawtooth Software, the **raw** utility scores have to be exported and afterwards read into *R* including the actual choice in the validation/holdout task.

To define the validation/holdout task, which has a total of 7 items (\texttt{\color{purple}prod}) plus the no-buy alternative (\texttt{\color{purple}none}), we use the \texttt{\color{purple}createHOT} function. Here, the user can define the attributes as well as the method (in this case \texttt{\color{purple}MaxDiff}).


```r
data("MaxDiff") # read in the data
HOT <- createHOT(
  data = MaxDiff, # data frame
  id = 1, # index unique identifier
  none = 19, # index of none alternative
  prod = 7, # no of alternatives in HOT excluding none
  prod.levels = list(3, 10, 11, 15, 16, 17, 18), # index of alternatives
  method = "MaxDiff", # method applied
  choice = 20, # column index of choice alternative
  varskeep = 21
)
```

Next, to get the relevant validation metrics that are often reported in conjoint studies, for example, hit rate [e.g., @ding2005], mean hit probability [mhp, @voleti2017], or mean absolute error [mae, @wlömert2014], we only need to provide the data, the alternatives in the validation/holdout task (\texttt{\color{purple}opts}), and the actual choice (\texttt{\color{purple}choice}). Everything can be implemented in the tidyverse [@tidyverse] logic.

\pagebreak


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

validateHOT also provides the five metrics for the confusion matrix. The underlying logic hereby is that the user has to provide a no-buy alternative (\texttt{\color{purple}none}). validateHOT calculates, for example, how often a buy or no-buy was correctly predicted, therefore, it is testing whether the model correctly predicts general demand (exemplary showed by applying the \texttt{\color{purple}accuracy} function and results split by \texttt{\color{purple}Group}).


```r
accuracy(
  data = HOT, # data frame
  group = Group, # optional grouping variable
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  none = None # column name
)
```

```
## # A tibble: 3 x 2
##   Group accuracy
##   <int>    <dbl>
## 1     1     73.9
## 2     2     72  
## 3     3     63.6
```

Finally, we show two functions for market simulations, namely \texttt{\color{purple}marksim} as well as \texttt{\color{purple}turf}. First, we calculate the market shares based on the multinomial logit model [@McFadden1974]. Besides the aggregated shares, \texttt{\color{purple}marksim} also provides standard errors and the 95th confidence interval.


```r
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "shareofpref"
)
```

```
## # A tibble: 8 x 5
##   Option      mw    se  lo.ci up.ci
##   <chr>    <dbl> <dbl>  <dbl> <dbl>
## 1 Option_1 18.3  4.12  10.2   26.3 
## 2 Option_2 11.3  2.69   6.05  16.6 
## 3 Option_3  4.08 1.49   1.16   6.99
## 4 Option_4 32.5  4.45  23.8   41.2 
## 5 Option_5  1.93 0.916  0.131  3.72
## 6 Option_6 10.4  2.68   5.12  15.6 
## 7 Option_7  5.58 1.75   2.15   9.01
## 8 None     16.0  3.29   9.53  22.4
```

\texttt{\color{purple}turf}, a "product line extension model" [@miaoulis1990, p. 29] is a tool to find the perfect assortment that creates the highest reach and a powerful tool for MaxDiff studies [@chrzan2019, p. 108]. To optimize the search for the optimal bundle, we could include as well the arguments \texttt{\color{purple}fixed}, to define alternatives that have to be part of the assortment, as well as \texttt{\color{purple}prohib}, to define prohibitions of combinations of items that should not be part of the assortment (please see the vignette for more details and also to see how to apply \texttt{\color{purple}turf} with data obtained by a likert scale).

For the following example, let us assume that you conducted an anchored MaxDiff analysis with 10 items (\texttt{\color{purple}opts}) and you want to find the best assortment with a size of 3 (\texttt{\color{purple}size = 3}). As a threshold (\texttt{\color{purple}none}) you use the anchor (no-buy alternative).


```r
turf(
  data = MaxDiff, # define data
  opts = c(Option_01:Option_10), # define items
  none = none, # define threshold variable
  size = 3, # define size of assortment
  approach = "thres" # define approach
) %>%
  head(., n = 5) %>%
  mutate_if(is.numeric, round, 2) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>%
  rename_all(., ~ paste0("Combo ", c(1:5)))
```

```
##           Combo 1 Combo 2 Combo 3 Combo 4 Combo 5
## reach       82.86   81.43   81.43   81.43   80.00
## freq         1.46    1.57    1.43    1.41    1.44
## Option_01       1       1       1       1       1
## Option_02       0       0       1       0       0
## Option_03       0       1       0       0       0
## Option_04       1       0       1       1       0
## Option_05       0       0       0       0       0
## Option_06       1       1       0       0       1
## Option_07       0       0       0       0       0
## Option_08       0       0       0       0       1
## Option_09       0       0       0       0       0
## Option_10       0       0       0       1       0
```

# Availability

validateHOT is available on [Github](https://github.com/JoshSchramm94/validateHOT).

# References
