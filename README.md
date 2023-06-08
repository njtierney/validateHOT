
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validateHOT 🎯

<!-- badges: start -->
<!-- badges: end -->

The goal of validateHOT is to validate the results of your validation
task (also known as holdout task). A validation task is essential to
make sure that your collected data of a *MaxDiff*, *CBC*, or *ACBC* are
valid and can also predict outside task that were **not** included in
estimating your utility scores. At the moment, <code>validateHOT</code>
provides functions for 3 key areas:

<ul>
<li>
validation metrics mainly reported in preference measurement studies
</li>
<li>
metrics that are usually reported in machine learning (i.e., confusion
matrix)
</li>
<li>
simulation methods, for example, to determine optimal product
combinations
</li>
</ul>

For all 3 key areas, the <code>createHOT</code> function is essential.
This function creates the total utilities for each alternative in the
validation/holdout task and also in the simulation method, respectively.
Rao (2014, 82) mentions the additive utility model stating that the
total utility of a profile or conjoint model is the sum of its attribute
levels. <code>createHOT</code> will do exactly this for you.

### Classical validation metrics

<ul>
<li>
<code>hitrate</code>:
</li>
<li>
<code>mhp</code>:
</li>
<li>
<code>kl</code>:
</li>
<li>
<code>mae</code>:
</li>
<li>
<code>medae</code>:
</li>
<li>
<code>mhp</code>:
</li>
<li>
<code>rmse</code>:
</li>
</ul>

### Confusion Matrix

<ul>
<li>
<code>accuracy</code>:
</li>
<li>
<code>f1</code>:
</li>
<li>
<code>precision</code>:
</li>
<li>
<code>recall</code>:
</li>
<li>
<code>specificity</code>:
</li>
</ul>

### Simulation Methods

<ul>
<li>
<code>freqassort</code>:
</li>
<li>
<code>reach</code>:
</li>
<li>
<code>shareofpref</code>:
</li>
</ul>

### Data Frames provided by <code>validateHOT</code>

<ul>
<li>
<code>ACBC_interpolate</code>:
</li>
<li>
<code>ACBC</code>:
</li>
<li>
<code>CBC_lin</code>:
</li>
<li>
<code>CBC</code>:
</li>
<li>
<code>MaxDiff</code>:
</li>
</ul>
<ul>
<li>
<code>createHOT</code>: creates the validation/holdout task for you. For
example, in a *CBC* the total utilities are calculated by the sum of
each attribute level. You have to specify the attribute levels for each
alternative in the validation/holdout task and it will calculate the
total utility for each alternative in the validation/holdout task.
</li>
<li>
<code>accuracy</code>: generates the number of correct predicted choice
or no-choice divided by the total number of predictions. Only possible
for a binary output, e.g., buy vs. no-buy correctly predicted.
</li>
<li>
<code>f1</code>: generates the F1-Score. F1-Score is calculated by the
following formula $\frac{2 * precision * recall}{precision + recall}$.
Only possible for a binary output, e.g., buy vs. no-buy correctly
predicted.
</li>
</ul>

## Installation

You can install the development version of validateHOT from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JoshSchramm94/validateHOT")
```

## Example

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-rao2014a" class="csl-entry">

Rao, Vithala R. 2014. *Applied Conjoint Analysis*. Springer Berlin
Heidelberg. <https://doi.org/10.1007/978-3-540-87753-0>.

</div>

</div>