---
title: 'validateHOT - an R package for holdout task validation and market simulation'
tags:
  - R
  - MaxDiff
  - Conjoint Analysis
  - Market Simulations
  - Predictive Validity
authors:
  - name: Joshua Benjamin Schramm
    orcid: 0000-0001-5602-4632
    corresponding: True
    affiliation: 1, 2
  - name: Marcel Lichters
    orcid: 0000-0002-3710-2292
    corresponding: FALSE
    affiliation: 2    
affiliations:
 - name: Chemnitz University of Technology, Germany
   index: 1
 - name: Otto von Guericke University of Magdeburg, Germany
   index: 2
citation_author: Schramm & Lichters
date: 02 January
year: 2024
bibliography: paper.bib
link-citations: true
output: rticles::joss_article
journal: JOSS
header-includes: 
 - \usepackage{float}
 - \usepackage{fancyhdr}
---

```{=tex}
\setlength{\headheight}{63.55022pt}
\addtolength{\topmargin}{-0.95425pt}
\newcommand{\colcod}[1]{\texttt{\color{purple}#1}}
```





# Summary

validateHOT is an R package that provides functions to both validate a validation/holdout task and run market simulations for results obtained in a (adaptive) choice-based conjoint analysis (hereafter ACBC and CBC, respectively) and maximum difference scaling (hereafter MaxDiff) using, for example, ChoiceModelR [@ChoiceModelR] or Sawtooth's Lighthouse Studio.

# Statement of need

Preference measurement techniques' (e.g., (A)CBC or MaxDiff) aim is to predict behavior [@green1990]. Hence, it is essential for both academics and practitioners to ensure that the collected data is valid and predicts outside tasks (i.e., the model has external validity) well.[^1] The easiest way for testing validity is by including so-called validation or holdout tasks [e.g., @rao2014; @Orme2015], which are tasks that are fixed (i.e., same across participants) and are usually not used for estimating the part-worth utilities in hierarchical Bayes estimation. Practitioners often do not include them [@yang2018], which is unsatisfactory given the fact that the model is used to estimate market shares which poses the basis for relevant marketing decisions.

[^1]: In terms of external validity, we refer to the generalizations to different settings [see, @calder1982, p.240].

validateHOT combines both validation and market simulation in one package and has three key advantages, it (1) helps to opt for the best model, (2) runs relevant market simulations that help to find the right product combinations or assortments, and finally, (3) is an open source tool including functions that are usually implemented in property commercial, and therefore, remain a black-box for researchers and practitioners.

# State of the field in R

Other packages provide functions to calculate validation metrics, however, these are not specified for individual raw logit coefficients which are usually the output when running random parameter logit / hierarchical Bayes models. Metrics [@Metrics], for example, provide functions to run validation metrics such as *mean absolute error*, *root mean squared error*, or the five metrics of the confusion matrix. However, to get the output of, for example, Sawtooth Software or ChoiceModelR [@ChoiceModelR] into the right format, the user needs some data wrangling. The package conjoint [@conjoint] provides functions that are most similar to validateHOT's ones. However, no functions for validation are included and moreover, conjoint [@conjoint] focuses on classical conjoint analysis, and thus is limited when applying more common CBC methods, for example, (A)CBC. support.BWS [@support.BWS] only covers best-worst scaling case 1 (also known as MaxDiff) and only provides market simulations based on conditional logit rule. logitr [@logitr] provides market simulations tools, however, no validation metrics such as mean hit probability [@voleti2017] or hit rate [@netzer2011]. A comparison of validateHOT's functions with current R packages is provided in \autoref{comparison}. To the best of our knowledge, a package that converts raw utility scores into validation metrics or running a variety of marketing simulations (especially TURF) is missing.

```{=tex}
\begin{figure}[h]
  \includegraphics{figures/FunctionComparison.png}
  \caption{Comparison of validateHOT's function to existing R packages}
  \label{comparison}
\end{figure}
```

validateHOT is introduced with data estimated with Lighthouse Studio using effects-coding for creating the design matrix. It, however, can easily be used with data estimated with ChoiceModelR [@ChoiceModelR], bayesm [@bayesm], or STAN [@rstan], if used with similar settings (ChoiceModelR, for example, automatically implements effects-coding).


# Key functions

validateHOT's functions can be categorized into four main components, see \autoref{tab:table1}. To bring the data into the right format, users can run the \texttt{\color{purple}createHOT()} function, which creates the total utility of each alternative by applying the additive utility model [@rao2014, p. 82]. \colcod{turf()} as well as the four rescaling functions, however, are not dependent on \texttt{\color{purple}createHOT()}, and can be run using the raw logit scores.

| Validation metrics | Confusion matrix | Market simulations | Rescaling scores |
|:----------------:|:----------------:|:----------------:|:----------------:|
|     hitrate()      |    accuracy()    |    freqassort()    |    att_imp()     |
|        kl()        |       f1()       |     marksim()      |  prob_scores()   |
|       mae()        |   precision()    |      reach()       |    zc_diffs()    |
|      medae()       |     recall()     |       turf()       | zero_anchored()  |
|       mhp()        |  specificity()   |                    |                  |
|       rmse()       |                  |                    |                  |

: Overview of validateHOT's main four components and their corresponding functions \label{tab:table1}

# Typical workflow

In the following, we provide the workflow for a MaxDiff study and a CBC study with only part-worth coded attributes (the vignette also provides detailed examples for a CBC including linear-coded attributes as well as an ACBC).

## MaxDiff

### Creating Holdout Task / Market Scenario

After running the hierarchical Bayes estimation [@allenby1995; @lenk1996], the **raw** utility scores have to be exported and read into an *R* data frame. This data frame has to include the actual choice in the validation/holdout task (if only a market scenario is created, the \texttt{\color{purple}choice} argument in \texttt{\color{purple}createHOT()} can be left empty)..

Assuming you included a validation/holdout task with a total of 7 alternatives plus the no-buy alternative (\texttt{\color{purple}none}). To create this validation task in *R*, we use the \texttt{\color{purple}createHOT()} function.


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

### Validating Holdout Task

To get the relevant validation metrics that are reported in conjoint studies, for example, hit rate [e.g., @ding2005], mean hit probability [mhp, @voleti2017], or mean absolute error [mae, @wloemert2014], we provide the data, the alternatives in the validation/holdout task (\texttt{\color{purple}opts}), and the actual choice (\texttt{\color{purple}choice}), which can be implemented using the tidyverse [@tidyverse] logic.


```r
hitrate(
  data = HOT, 
  opts = c(Option_1:None), 
  choice = choice 
) %>%
  round(2)
```

```
## # A tibble: 1 x 5
##      HR    se chance   cor     n
##   <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1  55.7  5.98   12.5    39    70
```

The underlying logic of the confusion matrix is that the user has to provide a no-buy alternative (\texttt{\color{purple}none}). validateHOT calculates how often a buy or no-buy was correctly predicted, therefore, it is testing whether the model correctly predicts general demand (here by applying \texttt{\color{purple}accuracy()}).


```r
accuracy(
  data = HOT, 
  group = Group, 
  opts = c(Option_1:None), 
  choice = choice, 
  none = None 
) %>% 
  round(2)
```

```
## # A tibble: 3 x 2
##   Group accuracy
##   <dbl>    <dbl>
## 1     1     73.9
## 2     2     72  
## 3     3     63.6
```

### Market Simulations

Lastly, we introduce two functions for market simulations, namely \texttt{\color{purple}marksim()} and \texttt{\color{purple}turf()}. In the following example, the market share is calculated according to the multinomial logit model [@McFadden1974].


```r
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "sop"
) %>% 
  mutate_if(is.numeric, round, 2)
```

```
## # A tibble: 8 x 5
##   Option      mw    se lo.ci up.ci
##   <chr>    <dbl> <dbl> <dbl> <dbl>
## 1 Option_1 18.3   4.12 10.2  26.4 
## 2 Option_2 11.3   2.69  6.05 16.6 
## 3 Option_3  4.08  1.49  1.16  6.99
## 4 Option_4 32.5   4.45 23.8  41.2 
## 5 Option_5  1.93  0.92  0.13  3.72
## 6 Option_6 10.4   2.68  5.12 15.6 
## 7 Option_7  5.58  1.75  2.15  9.01
## 8 None     16.0   3.29  9.53 22.4
```

Next, \texttt{\color{purple}turf()}, a "product line extension model" [@miaoulis1990, p. 29], is a tool to find the perfect assortment that creates the highest reach and is especially powerful for MaxDiff studies [@chrzan2019, p. 108]. To optimize the search for the optimal bundle, we also include the arguments \texttt{\color{purple}fixed}, to define alternatives that have to be part of the assortment, and \texttt{\color{purple}prohib}, to prohibit certain item combinations of being part of the assortment (see the vignette for more details and how to apply \texttt{\color{purple}turf()} with data obtained using a likert scale).

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

## CBC

### Creating Holdout Task / Market Scenario

The setup is almost the same, only the arguments \texttt{\color{purple}prod.levels}, \texttt{\color{purple}coding}, and \texttt{\color{purple}method} are different or new, respectively.
\newpage


```r
HOT_CBC <- createHOT(
  data = CBC,
  id = "ID",
  none = "none",
  prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
  coding = c(0, 0, 0),
  method = "CBC",
  choice = "HOT"
)
```

### Validating Holdout Task

This time we calculate the mean hit probability (i.e., MHP).


```r
HOT_CBC %>% 
  mhp(
  data = ., 
  opts = c(Option_1:None), 
  choice = choice 
) %>% 
  round(2)
```

```
## # A tibble: 1 x 2
##     MHP    se
##   <dbl> <dbl>
## 1  40.6  3.53
```

### Rescaling Scores

Finally, we can also display the attributes importance scores. Therefore, we need to define the attribute levels as well as the coding of the attributes.


```r
att_imp(
  data = CBC,
  attrib = list(
    c(4:8),
    c(9:13),
    c(14:20)
  ),
  coding = c(rep(0, 3)),
  res = "agg"
) %>% 
  mutate_if(is.numeric, round, 2)
```

```
## # A tibble: 3 x 3
##   Option       mw   std
##   <chr>     <dbl> <dbl>
## 1 att_imp_1  35.7 11.3 
## 2 att_imp_2  27.7 10.0 
## 3 att_imp_3  36.6  9.32
```

# Availability

validateHOT is available on [Github](https://github.com/JoshSchramm94/validateHOT).

# References
