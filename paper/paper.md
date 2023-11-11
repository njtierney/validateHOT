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

validateHOT is a package that provides functions to both validate a validation/holdout task and run market simulations for results obtained in a (adaptive) choice-based conjoint analysis (hereafter ACBC and CBC, respectively) and maximum difference scaling (hereafter MaxDiff) using `ChoiceModelR` [@ChoiceModelR] or [Sawtooth Software](https://sawtoothsoftware.com/).

Preference measurement techniques', such as (A)CBC or MaxDiff, ultimate goal is to predict future behavior [@green1990]. Hence, it is essential for both academics and practitioners to ensure that the collected data is valid and predicts outside tasks (i.e., the model has external validity) well.[^1] The easiest way to test it is to include so-called validation or holdout tasks [@Orme2015], which are tasks that are fixed (i.e., same across participant) and are usually not used for estimating the part-worth utilities in hierarchical Bayes estimation. Practitioners often do not include them [@yang2018], which is unsatisfactory given the fact that the model is used to estimate market shares which poses the basis for relevant marketing decisions.

[^1]: In terms of external validity, we refer to the generalizations to different settings [see, @calder1982, p.240]. 

validateHOT combines both validation and market simulation in one package and has three key advantages, it a) helps to opt for the best model, b) runs relevant market simulations that help to find the right product combinations, and finally, c) is an open source tool including functions that are usually implemented in paid software, and therefore, remain a black-box for researchers and practitioners.

# Statement of need

validateHOT is a practical tool for Sawtooth Software users in industry as well as academia. It provides an open source solution for a) validating a validation/holdout task and ensuring that the model has predictive validity; b) running market simulations (e.g., **T**otal **U**nduplicated **R**each and **F**requency, hereafter TURF). Other packages, for example, Metrics [@Metrics] provide functions to run validation metrics such as *mean absolute error*, *root mean squared error*, or the five metrics of the confusion matrix. However, to put the Sawtooth export into the right format, the user needs some data wrangling which could pose a barrier. Moreover, there are also packages that however mainly focus on the analysis of conjoint analysis (e.g., ChoiceModelR [@ChoiceModelR], choicetools [@choicetools], logitR [@logitr], bayesm [@bayesm] etc.). To the best of our knowledge, a package that converts raw utility scores into validation metrics or running a variety of marketing simulations (especially TURF) is missing. 

# Key functions

validateHOT's functions can be categorized into four main components, see \autoref{tab:table1}. To bring the data into the right format, users can run the \texttt{\color{purple}createHOT} function, which creates the total utility of each alternative by applying the additive utility model [@rao2014, p. 82]. \colcod{turf} as well as the 3 rescaling functions, however, are not dependent on \texttt{\color{purple}createHOT}, and can be run using the raw logit scores.

| Validation metrics | Confusion matrix | Market simulations | Rescaling scores |
|:----------------:|:----------------:|:----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |    zc_diffs()    |
|      medae()       |     recall()     |       turf()       |                  |
|       mhp()        |  specificity()   |                    |                  |
|       rmse()       |                  |                    |                  |

: Overview of main four components of validateHOT and their corresponding functions \label{tab:table1}

# Typical workflow

In the following, we provide the workflow for a MaxDiff study (the vignette also provides detailed examples for a CBC as well as an ACBC).

After running the Hierarchical Bayes estimation, the **raw** utility scores have to be exported and read into an *R* data frame. This data frame has to include the actual choice in the validation/holdout task.

Assuming you included a validation/holdout task with a total of 7 alternatives plus the no-buy alternative (\texttt{\color{purple}none}). To create this validation task in *R*, we use the \texttt{\color{purple}createHOT} function.


```r
HOT <- createHOT(
  data = MaxDiff, 
  id = "ID", 
  none = "none", 
  prod.levels = list(3, 10, 11, 15, 16, 17, 18), 
  method = "MaxDiff", 
  choice = "HOT", 
  varskeep = "Group"
)
```

To get the relevant validation metrics that are reported in conjoint studies, for example, hit rate [e.g., @ding2005], mean hit probability [mhp, @voleti2017], or mean absolute error [mae, @wloemert2014], we provide the data, the alternatives in the validation/holdout task (\texttt{\color{purple}opts}), and the actual choice (\texttt{\color{purple}choice}), which can be implemented using the tidyverse [@tidyverse] logic.


```r
hitrate(
  data = HOT, 
  opts = c(Option_1:None), 
  choice = choice 
) %>%
  round(3)
```

```
## # A tibble: 1 x 5
##      HR    se chance   cor     n
##   <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1  55.7  5.98   12.5    39    70
```

The underlying logic of the confusion matrix is that the user has to provide a no-buy alternative (\texttt{\color{purple}none}). validateHOT calculates how often a buy or no-buy was correctly predicted, therefore, it is testing whether the model correctly predicts general demand (here by applying \texttt{\color{purple}accuracy}).


```r
accuracy(
  data = HOT, 
  group = Group, 
  opts = c(Option_1:None), 
  choice = choice, 
  none = None 
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

Finally, we show two functions for market simulations, namely \texttt{\color{purple}marksim} and \texttt{\color{purple}turf}. In the following example, the market share is calculated according to the multinomial logit model [@McFadden1974].


```r
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "sop"
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

Finally, \texttt{\color{purple}turf}, a "product line extension model" [@miaoulis1990, p. 29], is a tool to find the perfect assortment that creates the highest reach and is especially powerful for MaxDiff studies [@chrzan2019, p. 108]. To optimize the search for the optimal bundle, we also include the arguments \texttt{\color{purple}fixed}, to define alternatives that have to be part of the assortment, and \texttt{\color{purple}prohib}, to prohibit certain item combinations of being part of the assortment (see the vignette for more details and how to apply \texttt{\color{purple}turf} with data obtained using a likert scale).

For the following example, we assume that the user conducted an anchored MaxDiff analysis with 10 items (\texttt{\color{purple}opts}) and now wants to find the best assortment with a size of 3. As a threshold (\texttt{\color{purple}none}), the user uses the anchor (no-buy alternative).


```r
turf(
  data = MaxDiff, 
  opts = c(Option_01:Option_10),
  none = none, 
  size = 3, 
  approach = "thres" 
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
