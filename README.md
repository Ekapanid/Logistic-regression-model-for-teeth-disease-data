# Logistic-regression-model-for-teeth-disease-data Synopsis

Analyzing data from a public health study with the goal of finding underlying factors that influence the probability of having a specific teeth disease. Variables of the dataset were:

age, tooth type, gender, number of healthy teeth and RF which took values of 1 or 0 with 1=sick.

The first part of the analysis was based on applying exploratory data analysis (EDA) for the variables of the dataset. Since most variables are factors barplots were used to visualize them. A histogram was used for the variable of age. EDA uncovered the fact that for two of the levels of the variable tooth type (tooth type no.5 and toothtype no.7) no cases of the disease were found (every person in these two groups had RF=0). These levels were dropped to avoid problems of complete separation when applying logistic regression models. The age variable was skewed and a log transformation was applied. EDA also uncovered the fact that the data was highly unbalanced and that above 90% percent of the sample did not have the disease. Thus deviance residuals after grouping the data were not likely to follow a normal distribution.

The second part of the analysis was grouping the data and aplying logistic regression models. Probit, Logit and cloglog models were fit using backwards selection methods (starting with all the interactions). In all the three models the only significant variable proved out to be tooth type. 
