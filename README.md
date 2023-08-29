
<!-- README.md is generated from README.Rmd. Please edit that file -->

# validateHOT 🎯

<!-- badges: start -->
<!-- badges: end -->

The goal of validateHOT is to validate the results of your validation
task (also known as holdout task). A validation task is essential to
make sure that your collected data of a MaxDiff, CBC, or ACBC are valid
and can also predict outside tasks that were not included in estimating
your utility scores. Although commercial studies often do not include a
validation/holdout task (Yang et al., 2018), it is highly recommended to
do so (Orme, 2015; Rao, 2014). This validation/holdout task does not
only help to check whether everything went right during data collection
but also to determine your final model. <code>validatHOT</code> provides
some of the relevant metrics to test the performance of your data in
predicting a holdout task. In [Sawtooth
Software’s](https://sawtoothsoftware.com/) CBC a fixed validation/
holdout task is automatically implemented. If you conduct a MaxDiff or
ACBC study, you have to program these on your own.

👉🏾 <u>**What you need to provide**</u>: <br> After collecting your data,
and running your initial Hierarchical Bayes models, you can turn to
<code>validateHOT</code> and test how good your model predicts choices
in the validation/holdout task. Herefore, you only have to read in your
raw utility scores as well as the actual choice of your validation/
holdout task. You can use the <code>merge()</code> provided by the
<code>base</code> package (R Core Team, 2023). Afterward, you can read
in your data file and enjoy <code>validateHOT</code>. We provide a
tutorial a short tutorial in the vignette.

👈🏾 <u>**What you get**</u>:<br> At the moment, <code>validateHOT</code>
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
Rao (2014, p. 82) mentions the additive utility model stating that the
total utility of a profile or conjoint model is the sum of its attribute
levels. <code>createHOT</code> will do exactly this for you.

### Classical validation metrics

<ul>
<li>
<code>hitrate</code>: creates the Hit Rate (correctly predicted choices)
of your validation task. The output will contain the percentage of how
many choices were correctly predicted, the chance level in your
validation task ($\frac{1}{alternatives}$) in percentage, the number of
correctly predicted participants’ choices as well as the number of
observations.
</li>
<li>
<code>kl</code>: Kullback-Leibler-Divergence measures the divergence
between the actual choice distribution and the predicted choice
distribution (Ding et al., 2011; Drost, 2018). Output provides both
divergence between predicted from observed and observed from predicted
due to the asymmetry of the Kullback-Leibler divergence. If you specify
an optional <code>group</code> argument the output is split by groups.
Currently, you can choose between and as logarithm base. The default is
set to .
</li>
<li>
<code>mae</code>: mean absolute error, i.e., deviation between predicted
and stated choice share.
</li>
<li>
<code>medae</code>: median absolute error, which is less affected by
outliers compared to the mean absolute error.
</li>
<li>
<code>mhp</code>: averaged hit probability of participant’s actual
choice in the validation/holdout task.
</li>
<li>
<code>rmse</code>: provides the root-mean-square error of deviation
between predicted and stated choice share.
</li>
</ul>

All functions can be extended with the `group` argument to get output by
group.

### Confusion Matrix

We also include metrics from machine learning, i.e., the confusion
matrix (e.g., Burger, 2018). For all of the 5 provided functions, you
currently have to include a **none** alternative in your
validation/holdout task. We currently predict, e.g., whether a buy or
no-buy was correctly predicted. Information could be used for
overestimating and underestimating, respectively, of product purchases.
In the following <code>TP</code> stands for true positives,
<code>FP</code> for false positives, <code>TN</code> for true negatives,
and <code>FN</code> for false negatives (Burger, 2018). To translate
this to the `validateHOT` logic, imagine you have a validation/holdout
task with 5 alternatives plus the alternative not to buy any of those
chosen. `validateHOT` now measures whether or not a buy (participant
opts for one of the 5 alternatives) or a no-buy (participant opts for
the no-buy alternative), respectively, is correctly predicted.

<ul>
<li>
<code>accuracy</code>: calculates the number of correctly predicted
choices (buy or no-buy); $\frac{TP + TN}{TP + TN + FP + FN}$ (Burger,
2018).
</li>
<li>
<code>f1</code>: defined as
$\frac{2 * precision * recall}{precision + recall}$ or stated
differently by Burger (2018) $\frac{2TP}{2TP + FP + FN}$.
</li>
<li>
<code>precision</code>: defined as $\frac{TP}{TP + FP}$ (Burger, 2018).
</li>
<li>
<code>recall</code>: defined as $\frac{TP}{TP + FN}$ (Burger, 2018).
</li>
<li>
<code>specificity</code>: defined as $\frac{TN}{TN + FP}$ (Burger,
2018).
</li>
</ul>

Again, all functions can be extended with the `group` argument to get
output by group.

### Simulation Methods

<ul>
<li>
<code>turf</code>: **T**(otal) **U**(nduplicated) **R**(each) and
**F**(requency) is a “product line extension model” (Miaoulis et al.,
1990, p. 29) that helps to find the perfect product bundle based on the
<code>reach</code> (e.g., how many participants consider buying at least
one product of that assortment) and the <code>frequency</code> (how many
products are on average a purchase option). <code>turf</code> currently
provides both the threshold approach (<code>approach = ‘thres’</code>;
all products that exceed a threshold are considered, e.g., a purchase
option; Chrzan & Orme, 2019, p. 112) and the first choice approach
(<code>approach = ‘fc’</code>; only product with highest utility is
considered as purchase option; Chrzan & Orme, 2019, p. 111).
</li>
<li>
<code>freqassort</code>: Similar to <code>turf</code>,
<code>freqassort</code> will give you the averaged frequency, how many
products the participants will choose from your in the function
determined potential assortment. Again, you have to define a `none`
alternative, because `freqassort` uses the *threshold* approach, meaning
if the utility of one product is above the utility of `none`, it is
marked as potential purchase option. While `turf` calculates the reach
and frequency for **all** combinations, you specify the combination you
are interested in <code>freqassort</code>.
</li>
<li>
<code>reach</code>: Similar to <code>turf</code>,, <code>reach</code>
will give you the averaged percentage of how many participants you can
reach (at least one of the products resemble a purchase option) with
your in the function determined potential assortment. `reach` also uses
the *threshold* approach (see above). While `turf` calculates the reach
and frequency for **all** combinations, you specify the combination you
are interested in <code>reach</code>
</li>
<li>
<code>shareofpref</code>: provides you the aggregated share of
preference, including the standard error, as well as the lower and upper
confidence interval, which is calculated according to the
$mean +/- 1.96 x \frac{sd}{\sqrt(n)}$ (Orme, 2020, p. 94).
</li>
</ul>

### Data Frames provided by <code>validateHOT</code>

<ul>
<li>
<code>ACBC</code>: Example data set with raw utilities of an ACBC study
conducted in Sawtooth. Price was linear-coded while the other attributes
were coded as part-worths.
</li>
<li>
<code>ACBC_interpolate</code>: Example data set with raw utilities of an
ACBC study conducted in Sawtooth. Price was piecewise-coded, another
attribute was linear-coded while the other attributes were coded as
part-worths.
</li>
<li>
<code>CBC</code>: Example data set with raw utilities of an CBC study
conducted in Sawtooth. All attributes were coded as part-worth.
</li>
<li>
<code>CBC_lin</code>: Example data set with raw utilities of an CBC
study conducted in Sawtooth. One attribute was linear coded while the
other attributes are part-worth coded.
</li>
<li>
<code>MaxDiff</code>: Example data set with raw utilities of an MaxDiff
study conducted in Sawtooth.
</li>
</ul>

## Why <code>validateHOT</code>

We are teaching a preference measurement seminar for students. Often
these students did not have any prior experience (or only sparsely) with
*R*. One of the chapters in this class is about model validating the
results obtained and we teach this, of course, in *R*. Of course, there
are other great packages which are faster in running (i.e., `Metrics` by
Hamner & Frasco, 2018), however, these packages need some more data
wrangling in order to use the appropriate functions, which might be a
burden or barrier for one or the other.

Moreover, as Yang et al. (2018) report, commercial studies often do not
use any validation task. Again, the missing experience in *R* could be
one explanation. Since these functions are not always implemented in
other softwares, this might be one reason why they do not include one
simply because they do not know how to use it correctly. Having a
package to evaluate the validation/holdout task can also be beneficial
from this perspective.

## Installation

You can install the development version of validateHOT from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JoshSchramm94/validateHOT")
```

## Example

First, we load the package.

``` r
library("validateHOT")
```

### Example I - CBC

Since *CBC’s* are most commonly used between *CBC*, *ACBC*, and
*MaxDiff*, we will provide an example with a *CBC*. Let us begin with a
*CBC* where all of the variables are coded as part-worth, so we need no
interpolation. Let us load the <code>CBC</code> data frame for this
example.

``` r
data("CBC")
```

The data frame has a total of 79 participants and 23 columns.

Now imagine you included a validation/ holdout task with three
alternatives as well as an no-buy option. We specify the
<code>data</code> argument and the column index of <code>id</code>.
Since we also have a *no-buy* alternative in our validation task, we
next specify the <code>None</code> argument, otherwise we would have
left it empty. Afterwards, we specify the number of products
(<code>prod</code>) we include (excluding the *no-buy* alternative),
which we define in the next step with the argument
<code>prod.levels</code>. If we look back at the data frame, we can see
that the first alternative in the holdout task <code>c(4, 9, 19)</code>
is composed of the following attribute levels <code>Att1_Lev1</code>,
<code>Att2_Lev1</code>, and <code>Att3_Lev5</code>.

As mentioned above, all the attributes are part-worth coded, therefore,
we set <code>coding = c(0, 0, 0)</code>. Finally, we specify the method
of preference measurement technique, which is <code>method =
“CBC”</code> in our case and the column index of the actual
participant’s choice (<code>choice</code>). If you run the code, a data
frame called <code>HOT</code> (short form of **H**old**o**ut **t**ask)
will be returned to the global environment.

> ❗ <code>createHOT</code> is currently just taking indexes instead of
> column names, please be aware of this. However, you can easily find
> out the index by using the <code>names()</code> function or by using
> <code>which()</code> and <code>colnames()</code>, both functions are
> provided by the base package (R Core Team, 2023). For example, if you
> want to find out the column index of <code>ID</code>, we could also
> use <code>which(colnames(CBC) == “ID”)</code>.

``` r
HOT <- createHOT(
  data = CBC, # data frame
  id = 1, # column index of the id
  None = 21, # column index of none alternative
  prod = 3, # number of alternatives in validation task (excluding none)
  prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)), # column index of the attribute levels for each alternative
  coding = c(0, 0, 0), # how the attributes were coded
  method = "CBC", # method
  choice = 22 # column index of choice
)
```

Let us take a glimpse at the output, which shows the total raw utilities
for each of the alternatives included in the validation/holdout task.

``` r
head(HOT)
#>   ID   Option_1   Option_2   Option_3       None choice
#> 1  1 -0.2029166  0.5851903  1.3231991 -3.2921817      1
#> 2  2 -0.1431625 -0.7186898 -1.7759660 -0.9123018      1
#> 3  3 -0.5995552 -0.4783988 -2.2596407 -0.7447178      1
#> 4  4 -1.3542603 -1.8028929 -0.9638149  2.5995588      4
#> 5  5 -0.1875285 -0.6541611 -0.9273235 -2.8965076      2
#> 6  6  1.1212906 -0.9250507 -1.3261888 -1.0404554      2
```

In the next step, we would like to see how well our model (from which we
took the raw utilities) predict the actual choices in the
validation/holdout task. First, we will test the <code>hitrate()</code>
function. We specify the <code>data</code>, the column names of the
alternatives (<code>opts</code>; remember we have three alternatives +
the *no-buy* alternative), and finally the actual choice.

``` r
hitrate(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 4
#>      HR chance   cor     n
#>   <dbl>  <dbl> <int> <int>
#> 1  48.1     25    38    79
```

Let us also check the magnitude of the mean absolute error by running
the <code>mae()</code> function.

``` r
mae(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice # column name of choice
)
#> # A tibble: 1 × 1
#>     mae
#>   <dbl>
#> 1  13.1
```

To cover also one example on how to use the metrics of the confusion
matrix, we could test whether our model overestimates the purchase
behavior (our model predicts a *buy* although participant opts for a
*no* buy) or underestimates it (i.e., model predicts *no-buy* but
participant opts for a *buy*). We will test the accuracy of the model by
running the <code>accuracy()</code> function.

``` r
accuracy(
  data = HOT, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  none = None # column name of none alternative
)
#> # A tibble: 1 × 1
#>   accuracy
#>      <dbl>
#> 1     93.7
```

Finally, let us test, how many participants would at least buy one of
the three products, assuming that this is one potential assortment we
would like to offer to our consumers. We will use the
<code>reach()</code> function and use the threshold approach. To specify
the bundles we are offering we use the <code>opts</code> argument in our
function.

``` r
reach(
  data = HOT, # data frame
  opts = c(Option_1:Option_3), # products that should be considered
  none = None # column name of none alternative
)
#> # A tibble: 1 × 1
#>   reach
#>   <dbl>
#> 1  88.6
```

### Example II - CBC with linear coding

In a second example, we again use a *CBC*, however, this time we show
how to use <code>validateHOT</code> if one of the variables are linear
coded. All other examples are provided in the accompanied vignette.

We are using the data frame <code>CBC_lin</code> which is also provided
by the <code>validateHOT</code> package. We first load the data frame.

``` r
data("CBC_lin")
```

Next, we create the validation/holdout task to evaluate it in the next
step. We use a validation/holdout task with three alternatives plus the
*no-buy* alternative, just as we did in the previous example. The only
difference to the previous example is that for our model estimation, the
third attribute (<code>Att3_Lin</code>) was coded as linear.

Again, we first define <code>data</code>. The <code>id</code> is saved
in the first column. The utilities for the <code>None</code> parameter
are stored in the 15th column, and we again set <code>prod</code> to 3
(**excluding** the *no-buy* alternative). Next, we define the
<code>prod.levels</code> for each alternative. For example, we can see
that the second see that the second alternative is composed of
<code>Att1_Lev5</code>, <code>Att2_Lev4</code>, and <code>60</code>
which is the value that should be interpolated. We can see that this
value is the one that needs to be interpolated if we take a closer look
at the <code>coding</code> argument. This tells us that the first two
attributes (<code>Att1_Lev5</code>, <code>Att2_Lev4</code> in the case
of alternative 2) are part-worth coded (<code>0</code>) while the third
attribute is linear coded (<code>1</code>).

To interpolate the value, we have to provide <code>validateHOT</code>
the <code>interpolate.levels</code>. These **need** to be the same as
provided to Sawtooth as levels. Moreover, it is important that the value
that should be interpolated needs to lie within the lower and upper
bound of <code>interpolate.levels</code>. In our case, we had 7 levels
that range from 10 to 70.

Next, we define the column index of the linear coded variable
(<code>lin.p</code>) and specify the <code>coding</code> we talked about
above. Again, we are running a *CBC* specified by the
<code>method</code> argument. This time, we would like to keep some of
the variables in the data frame, which we specify in
<code>varskeep</code>. We only keep one further variable, however, you
can specify as many as you want. This could be relevant if you would
like to display results per group. Finally, we just tell
<code>validateHOT</code> the column index of the final choice
(<code>choice</code>) and we are all set.

``` r
CBC <- createHOT(
  data = CBC_lin, # data frame
  id = 1, # column index of the id
  None = 15, # column index of none alternative
  prod = 3, # number of alternatives in validation task (excluding none)
  prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)), # column index of the attribute levels for each alternative (for linear coded we specify value to be interpolated)
  interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)), # actual values for the levels that should be interpolated
  lin.p = 14, # column index of the linear coded variable
  coding = c(0, 0, 1), # coding of the 3 attributes; 0 = part-worth, 1 = linear, 2 = piecewise
  method = "CBC", # method
  varskeep = 17, # column index of variables that should be kept in the dataframe
  choice = 16 # column index of choice
)
```

The next steps are the same as above. However, let us take a look at
some examples in which we display the results per group. Let us again
begin with the <code>hitrate()</code> function. To do so, we specify the
column name of the grouping variable in the <code>group</code> argument.

``` r
hitrate(
  data = CBC, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  group = Group # column name of Grouping variable
)
#> # A tibble: 3 × 5
#>   Group    HR chance   cor     n
#>   <int> <dbl>  <dbl> <int> <int>
#> 1     1  59.1     25    13    22
#> 2     2  37.0     25    10    27
#> 3     3  56.7     25    17    30
```

In this case, the group assignment is stored in form of integers.
However, the output is the same if it is a factor or if it is labelled
data. To proof this we just quickly change <code>group</code> into a
factor by using the <code>factor()</code> function provided by R Core
Team (2023).

``` r
CBC$Group <- base::factor(
  CBC$Group,
  levels = c(1:3),
  labels = paste0("Group_", c(1:3))
)
```

Afterward, we display the *mean hit probability* by running the
<code>mhp()</code> function.

``` r
mhp(
  data = CBC, # data frame
  opts = c(Option_1:None), # column names of alternatives
  choice = choice, # column name of choice
  group = Group # column name of Grouping variable
)
#> # A tibble: 3 × 2
#>   Group     MHP
#>   <fct>   <dbl>
#> 1 Group_1  41.7
#> 2 Group_2  39.0
#> 3 Group_3  47.8
```

For more examples, please see the accompanied vignette.

## References

Burger, Scott V. 2018. <em>Introduction to Machine Learning with R:
Rigorous Mathematical Analysis</em>. O’Reilly.

Chrzan, K., & Orme, B. K. (2019). <em>Applied MaxDiff: A Practitioner’s
Guide to Best-Worst Scaling</em> Provo, UT: Sawtooth Software.

Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang,
SU Chenting, and Steven P. Gaskin. 2011. “Unstructured Direct
Elicitation of Decision Rules.” <em>Journal of Marketing Research
48</em>(1): 116-27. <https://doi.org/10.1509/jmkr.48.1.116>

Drost, Hajk-Georg. 2018. “Philentropy: Information Theory and Distance
Quantification with R” <em>Journal of Open Source Software 3</em>(26),
765, <https://joss.theoj.org/papers/10.21105/joss.00765>.

Hamner, Ben, and Michael Frasco. 2018. “Metrics: Evaluation Metrics for
Machine Learning.” <https://CRAN.R-project.org/package=Metrics>.

Miaoulis, G., Parsons, H., & Free, V. (1990). Turf: A New Planning
Approach for Product Line Extensions. <em>Marketing Research 2</em> (1):
28-40.

Orme, Bryan K. 2015. “Including Holdout Choice Tasks in Conjoint
Studies.”
<https://sawtoothsoftware.com/resources/technical-papers/including-holdout-choice-tasks-in-conjoint-studies>.

Orme, B. K. (2020). <em>Getting Started with Conjoint Analysis:
Strategies for Product Design and Pricing Research</em>. 4th edition.
Manhattan Beach, CA: Research Publishers LLC.

R Core Team. 2023. “R: A Language and Environment for Statistical
Computing.” <https://www.R-project.org/>.

Rao, Vithala R. 2014. <em>Applied Conjoint Analysis.</em> Springer
Berlin Heidelberg. <https://doi.org/10.1007/978-3-540-87753-0>.

Yang, Liu (Cathy), Olivier Toubia, and Martijn G. de Jong. 2018.
“Attention, Information Processing, and Choice in Incentive-Aligned
Choice Experiments.” <em>Journal of Marketing Research 55</em>(6):
783–800. <https://doi.org/10.1177/0022243718817004>.
