# I. 💉 ARDS Survival Prediction
Survival prediction on [ECMO](https://en.wikipedia.org/wiki/Extracorporeal_membrane_oxygenation) treatment on patients diagnosed with [ARDS](https://en.wikipedia.org/wiki/Acute_respiratory_distress_syndrome)

## Background


## 🌱 Motivation
Thesis project for MSc Biostatistics at University of Glasgow 2019

## 📓 Notebooks
I will include a write up similar to the final thesis in an html RMarkdown.

## 📁 Datasets
ARDSdata.csv - I don't have information on who, where, how this data was optained.



# II. 🛠️ Libraries

## Caret 🥕

- [Package 'caret'](https://cran.r-project.org/web/packages/caret/caret.pdf)
- [caret](http://topepo.github.io/caret/index.html)

### Books

- [Applied Predictive Modeling](http://appliedpredictivemodeling.com/toc)

### Vignettes

1. [A Short Introduction to the caret Package](https://cran.r-project.org/web/packages/caret/vignettes/caret.html)
2. [Caret Practice](https://rpubs.com/phamdinhkhanh/389752)



## Parallel Computing
- [A Quick Intro to Parallel Computing in R](https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)

### Vignette
- [Getting Started with doParallel and foreach](https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf)


## Imputation
The [`mice`](https://cran.r-project.org/package=mice) package
implements a method to deal with missing data. The package creates
multiple imputations (replacement values) for multivariate missing
data. The method is based on Fully Conditional Specification, where
each incomplete variable is imputed by a separate model. The `MICE`
algorithm can impute mixes of continuous, binary, unordered
categorical and ordered categorical data. In addition, MICE can impute
continuous two-level data, and maintain consistency between
imputations by means of passive imputation. Many diagnostic plots are
implemented to inspect the quality of the imputations.

-[Package 'mice'](https://cran.r-project.org/web/packages/mice/mice.pdf)

### Books 

- Van Buuren, S. (2018). [Flexible Imputation of Missing Data. Second Edition.](https://stefvanbuuren.name/fimd/). Chapman & Hall/CRC. Boca Raton, FL.

### Vignettes

1. [Ad hoc methods and the MICE algorithm](https://gerkovink.github.io/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html)
2. [Convergence and pooling](https://gerkovink.github.io/miceVignettes/Convergence_pooling/Convergence_and_pooling.html)
3. [Inspecting how the observed data and missingness are related](https://gerkovink.github.io/miceVignettes/Missingness_inspection/Missingness_inspection.html)
4. [Passive imputation and post-processing](https://gerkovink.github.io/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html)
5. [Imputing multilevel data](https://gerkovink.github.io/miceVignettes/Multi_level/Multi_level_data.html)
6. [Sensitivity analysis with `mice`](https://gerkovink.github.io/miceVignettes/Sensitivity_analysis/Sensitivity_analysis.html)
7. [Generate missing values with `ampute`](https://rianneschouten.github.io/mice_ampute/vignette/ampute.html)

### More Examples
- [Imputing Missing Data with R; MICE package](https://datascienceplus.com/imputing-missing-data-with-r-mice-package/)
- [How do I perform Multiple Imputation using Predictive Mean Matching in R?](https://stats.idre.ucla.edu/r/faq/how-do-i-perform-multiple-imputation-using-predictive-mean-matching-in-r/)



# III. Methods

## Logistic Regression + LASSO Regularization
### References

## Linear Discriminant Analysis
### References

## Quadratic Discriminant Analysis
### References

## K-Nearest Neighbors
### References

- Hechenbichler K. and Schliep K.P. (2004)Weighted k-Nearest-Neighbor Techniques and OrdinalClassification, Discussion Paper 399, SFB 386, Ludwig-Maximilians University Munich (http://www.stat.uni-muenchen.de/sfb386/papers/dsp/paper399.ps)
- Hechenbichler K. (2005)Ensemble-Techniken und ordinale Klassifikation, PhD-thesis
- Samworth, R.J. (2012)Optimal weighted nearest neighbour classifiers.Annals of Statistics, 40,2733-2763. (avaialble from http://www.statslab.cam.ac.uk/~rjs57/Research.html)


## Random Forests
### References


## Support Vector Machines
### References



# IV. To-Do

- [ ] Try out caret "recipes" (http://topepo.github.io/caret/using-recipes-with-train.html)
- [ ] ROC plots
- [ ] Performance Tables
- [x] Switch to "caret" package
- [x] Add Parallel Processing
