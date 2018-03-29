# Machine-Learning-on-Bankruptcy

The objective of the study on bankruptcy data was to identify the best classification methodology for the given data to predict bankruptcy. The Bankruptcy data was collected from COMPUSTAT for the years 1980 to 2000 and has 5436 observations with 13 variables. For the study, as there was no clear trend in bankruptcy, it was assumed that the data across the years can be pooled together and studied. 
Of the 13 variables, one of them was “DLRSN”- a categorical variable indicating default, the dependent variable of the prediction. Overall, the bankruptcy is about 14% of the entire sample. After the initial EDA was performed, the data was separated randomly into test and train datasets with 20-80 split. 

The various techniques applied for the prediction of bankruptcy were- Generalized Linear Regression- Logistic, Classification Tree, Generalized Additive Model, Linear Discriminant Analysis and Neural Networks. The models were chosen on the basis of misclassifcation errors. As per industry standard, a weight of 15 was added to penalize the False negative responses which brought down the cut off probability down to 0.06.  

When fitting the logistic regression, a step BIC model selection was used that resulted in a classification, in line with the rest of the methodologies. The approach to Classification tree was to pick the tree that gave the best classification, which was later pruned for complexity, while also retaining the best performance. In GAM modeling, the initial model was fitted again on all of the variables, but upon observing the smoothing plots and edf, only few of the variables were retained for the final model. For LDA, since the variable distribution was not perfectly normal, a Quadratic Discriminant Analysis was also performed. Although the LDA and QDA results are very similar, it could be suggested, based on variable distribution, that QDA should be preferred.
Model	In sample- AMR (Train data 80%)	Out of sample-AMR (Test data 20%)
GLM-Logstic	0.34	0.36
CART	0.35	0.37
GAM	0.32	0.34
QDA	0.33	0.34
Neural Net	0.31	0.35
LDA	0.32	0.34

Overall misclassification rates for the in-sample and out-of-sample were consistent across the models. Ranking the performances was much harder for the classification as the results were very close and there was no clear winners to pick. Neural networks had very slightly improved performance over in-sample. 
