# I. 💉 ARDS Survival Prediction
Survival prediction on [ECMO](https://en.wikipedia.org/wiki/Extracorporeal_membrane_oxygenation) treatment on patients diagnosed with [ARDS](https://en.wikipedia.org/wiki/Acute_respiratory_distress_syndrome)

## 🧛🏼‍♂️Background


## 🌱 Motivation
Thesis project for MSc Biostatistics at University of Glasgow 2019

## 📓 Notebooks
I will include a write up similar to the final thesis in an html RMarkdown.

## 📁 Datasets
ARDSdata.csv - I don't have information on who, where, how this data was optained.



# II. 📦 Packages

## Caret 🥕

- [Package 'caret'](https://cran.r-project.org/web/packages/caret/caret.pdf)
- [caret](http://topepo.github.io/caret/index.html)

### Books 📚

- [Applied Predictive Modeling](http://appliedpredictivemodeling.com/toc)

### Vignettes 🎻

1. [A Short Introduction to the caret Package](https://cran.r-project.org/web/packages/caret/vignettes/caret.html)
2. [Caret Practice](https://rpubs.com/phamdinhkhanh/389752)



## Parallel Computing 💾
- [A Quick Intro to Parallel Computing in R](https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html)

### Vignette 🎻
- [Getting Started with doParallel and foreach](https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf)


## Imputation 🐁
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

### Books 📚

- Van Buuren, S. (2018). [Flexible Imputation of Missing Data. Second Edition.](https://stefvanbuuren.name/fimd/). Chapman & Hall/CRC. Boca Raton, FL.

### Vignettes 🎻

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



# III. 🛠️ Methods

## Logistic Regression + LASSO Regularization
### References

## Linear Discriminant Analysis
### References
### Vignettes 🎻

- [Foundation of LDA and QDA for prediction, dimensionality reduction or forecasting](http://www.socr.umich.edu/people/dinov/2017/Spring/DSPA_HS650/notes/20_PredictionCrossValidation.html#73_foundation_of_lda_and_qda_for_prediction,_dimensionality_reduction_or_forecastin)

## Quadratic Discriminant Analysis
### References
### Vignettes 🎻

- [Foundation of LDA and QDA for prediction, dimensionality reduction or forecasting](http://www.socr.umich.edu/people/dinov/2017/Spring/DSPA_HS650/notes/20_PredictionCrossValidation.html#73_foundation_of_lda_and_qda_for_prediction,_dimensionality_reduction_or_forecastin)


## K-Nearest Neighbors
### References

- Hechenbichler K. and Schliep K.P. (2004)Weighted k-Nearest-Neighbor Techniques and OrdinalClassification, Discussion Paper 399, SFB 386, Ludwig-Maximilians University Munich (http://www.stat.uni-muenchen.de/sfb386/papers/dsp/paper399.ps)
- Hechenbichler K. (2005)Ensemble-Techniken und ordinale Klassifikation, PhD-thesis
- Samworth, R.J. (2012)Optimal weighted nearest neighbour classifiers.Annals of Statistics, 40,2733-2763. (avaialble from http://www.statslab.cam.ac.uk/~rjs57/Research.html)


## Random Forests
### References


## Support Vector Machines

### References
### Vignettes 🎻

- [SVM Classifier Implementation in R with caret package](https://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/)


# IV. Misc Topics
## Imbalanced Data Sets
### Vignettes 🎻

- [Methods for Dealing with Imbalanced Data](https://towardsdatascience.com/methods-for-dealing-with-imbalanced-data-5b761be45a18)


## Rank Deficiency

Ran into some rank deficiency problems when training QDA on imputed datasets

- [What is Rank Deficiency?](https://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it)

## Tuning Parameters 📻

- [Tune ML Algorithms in R](https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/)
- [Accuracy Metrics in caret](https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/)
  - Accuracy & Cohen's Kappa - Kappa good for imbalanced datasets
  - ROC & AUC - Good for binary outcome
  - RMSE & R^2 - Good for continuous outcome
  - Logarithmic Loss - Good for multilevel outcome.  A linear translation of the log likelihood for a binary outcome model (Bernoulli trials).

# V. To-Do

- [x] Save trained models
- [ ] Try out caret "recipes" (http://topepo.github.io/caret/using-recipes-with-train.html)
- [x] ROC plots
- [x] Performance Tables
- [x] Switch to "caret" package
- [x] Add Parallel Processing


# Bibliography

- [Multiple Imputation and Ensemble Learning for Classification with Incomplete Data](https://www.researchgate.net/publication/309176108_Multiple_Imputation_and_Ensemble_Learning_for_Classification_with_Incomplete_Data)
  - **Method**: Build multiple datasets using Multilple Imputation.  Use Ensemble method to combine classification results. 
  - **Results**:  .."using the diversity of imputed datasets in an ensemble method leads to a more effective classifier."
  - **Notes**: Flow charts showing imputation / training/ ensembling

- [Methods to Combine Multiple Imputations](https://stats.stackexchange.com/questions/332659/binary-classification-with-multiple-imputations)
  - **Method**:  Ensemble method / stacked dataset with dummy variables for imputed values
  - **Results**:  **No Sources**
 
 - [Inference and Missing Data](https://www.jstor.org/stable/2335739?seq=1#metadata_info_tab_contents)
  - **Notes**: Original paper on missing data - MCAR, MAR, MNAR

- [Classification Uncertainty of Multiple Imputed Data](https://ieeexplore.ieee.org/document/7376605)
  - **Method**: Random Forest imputation with trees = 10
  - **Notes**: Discusses methods for making classifications on imputed data.  
  - **Keywords**: White paper, Uncertainty measures, Discussion of Rubin's Rules
 
- [Handling missing values in kernel methods with application to microbiology data](https://www.sciencedirect.com/science/article/pii/S0925231214003907)
  - **Method**: 
    1. Concatenate the multiple imputed data sets and optimize an SVM classifier in the resulting set; this not only accounts for the variability of the parameter estimates but also for the variability of the training observations in relation to the imputed values.  (IMI Algorithm) In the first algorithm the training data set was imputed m times, merged into a single large data set, and then used to train a classifier, SVM in this case. Test data set was then concatenated with the stacked training data set, imputed m times, and extracted from the training samples for prediction. Each of the m now complete test data sets were used for prediction using the classifier that was trained in the previous step. Therefore, for each sample in the test data set, m predictions were produced and a majority vote was used to form the final prediction for each test sample.  
    2.  A more standard procedure, involves fitting separate SVMs to each imputed data set and get the pooled (i.e., averaged) performance of the different SVMs.   In the second algorithm, training data was again imputed m times and for each of the complete data sets, a classifier was trained. Test data set was then concatenated with each of the m imputed training data sets, imputed once (i.e. m=1) and then used for prediction. Again, m predictions were produced and a majority vote was used to form the final prediction.  
  - The IMI algorithm was determined to work generally better.
  - **Notes**: Two general principles should be kept in mind for performing MI at test time:
    1. Imputation of test data must be done in test time, that is, it is not possible to do the imputation of all data altogether (training and test). 
    2. When imputing the missing values in test data, it is not possible to use the class (target) variable for the imputation (only the predictors can be used).
  - **Keywords**: Kendall coefficient, Proportion of Useable Cases

- [Multiple Imputation for Nonresponse in Surveys](https://books.google.co.uk/books?hl=en&lr=&id=bQBtw6rx_mUC&oi=fnd&pg=PR24&dq=+Multiple+Imputation+for+Nonresponse+in+Surveys&ots=8OtK8O2WhU&sig=JWLpVxM2VztZfhP2aQIQUiIZZZo&redir_esc=y#v=onepage&q=Multiple%20Imputation%20for%20Nonresponse%20in%20Surveys&f=false)
  - **Notes**: Rubin's Rules (pooling estimates)
  
- Khan, S. Shehroz and Ahmad, Amir and Mihailidis, Alex (2018)  Bootstrapping and Multiple Imputation Ensemble Approaches for Missing Data. 
  - **Notes**: Good literature review on Multiple Imputation and ensembling results.  
  - **Keywords**: Ensemble, Multiple Imputation, Bagging, Flow Chart, Classifier Fusion Techniques, Expectation Maximization

- [Impact of imputation of missing values on classification error for discrete data](https://www.sciencedirect.com/science/article/pii/S003132030800201X#tbl1)
  - **Notes**: Comparison of imputation methods.  Good write up on missing data and imputation methods.

 - Barnard, J. and Rubin, D.B. (1999). Small sample degrees of freedom with multiple imputation. Biometrika, 86, 948-955.

- Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York: John Wiley and Sons.

- van Buuren S and Groothuis-Oudshoorn K (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. https://www.jstatsoft.org/v45/i03/
    - **Notes**: MICE package

