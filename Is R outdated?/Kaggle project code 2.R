rm(list = ls());

# Input your dataset
mydata <- read.csv("r_kaggle_num.csv",header=TRUE)

# Turn on output to a file (in addition to the screen). This way we've got a record of
# what we did.
sink("OtherCodingRegressionOutput.txt", append=FALSE, split=TRUE);

pythonfit = glm(Q7_python ~ Q7_R, data = mydata, family=binomial(link = "logit"))
cfit = glm(Q7_c ~ Q7_R, data = mydata, family=binomial(link = "logit"))
c..fit = glm(Q7_c.. ~ Q7_R, data = mydata, family=binomial(link = "logit"))
javafit = glm(Q7_java ~ Q7_R, data = mydata, family=binomial(link = "logit"))
javascriptfit = glm(Q7_javascript ~ Q7_R, data = mydata, family=binomial(link = "logit"))
juliafit = glm(Q7_julia ~ Q7_R, data = mydata, family=binomial(link = "logit"))
swiftfit = glm(Q7_swift ~ Q7_R, data = mydata, family=binomial(link = "logit"))
bashfit = glm(Q7_bash ~ Q7_R, data = mydata, family=binomial(link = "logit"))
matlabfit = glm(Q7_matlab ~ Q7_R, data = mydata, family=binomial(link = "logit"))
nonefit = glm(Q7_none ~ Q7_R, data = mydata, family=binomial(link = "logit"))
other2fit = glm(Q7_other2 ~ Q7_R, data = mydata, family=binomial(link = "logit"))

summary(pythonfit)
summary(cfit)
summary(c..fit)
summary(javafit)
summary(javascriptfit)
summary(juliafit)
summary(swiftfit)
summary(bashfit)
summary(matlabfit)
summary(nonefit)
summary(other2fit)

sink();

sink("RProbRegressionOutput.txt", append=FALSE, split=TRUE);

correlation <- cor(mydata)

rprobfit = glm(Q7_R ~age+woman + Country+job+yrs_coding+Q7_python + Q7_sql + Q7_c+ Q7_c.. + Q7_java+Q7_javascript+Q7_julia+Q7_swift+Q7_bash+Q7_matlab+Q7_none 
        +Q7_other2+Q8_recommend+Q10_Kaggle+Q10_Colab+Q10_azure+Q10_paper+Q10_binder+Q10_ocean+Q10_ibm+Q10_sage+Q10_emr
        +Q10_google.notebook+Q10_google_datalab+Q10_databricks+Q10_zeppelin+Q10_deepnote+Q10_observable+Q10_none+Q10_other+Q11_equipment
        +Q14_Matplotlib+Q14_seaborn+Q14_plotly+Q14_ggplot+Q14_shiny+Q14_d3+Q14_altair+Q14_bokeh
        +Q14_geoplotlib+Q14_leaflet+Q14_none+Q14_Other+Q15_yrs_ml+Q16_scikit+Q16_tensor+Q16_keras+Q16_pytorch
        +Q16_fast.ai+Q16_mxnet+Q16_xgboost+Q16_lightgbm+Q16_catboost+Q16_prophet+Q16_h2o+Q16_caret+Q16_tidymodels
        +Q16_jax+Q16_pytorch_lightning+Q16_huggingface+Q16_none+Q16_Other+Q17_linear+Q17_decision+Q17_gradient
        +Q17_bayesian+Q17_evolutionary+Q17_dense+Q17_convolutional+Q17_generative+Q17_recurrent+Q17_transformer+Q17_none+Q17_other
        +Q20_industry+Q21_num_emp+Q22_num_data_empl+Q23_bus_use_ml+Q24_ch1+Q24_ch2+Q24_ch3+Q24_ch4+Q24_ch5+Q24_ch6+Q24_ch7+Q24_ch8+Q25_income
        ,data = mydata, family=binomial(link = "logit")) 
                


summary(rprobfit)
sink();

if (!require("rpart")) { install.packages("rpart")
  require("rpart") }

if (!require("rpart.plot")) { install.packages("rpart.plot")
  require("rpart.plot") }

set.seed(123)

trainIndex  <- sample(1:nrow(mydata), size = round(0.5*nrow(mydata)), replace = F)

# Put the subset of data assigned as the training partition (specified by p) in trainingSet
trainingSet <- mydata[ trainIndex,]

# Put everything that wasn't in that subset (1-p) in validationSet
validationSet  <- mydata[-trainIndex,]

MyTree <- rpart(Q7_R ~age+woman + Country+job+yrs_coding+Q7_python + Q7_sql + Q7_c+ Q7_c.. + Q7_java+Q7_javascript+Q7_julia+Q7_swift+Q7_bash+Q7_matlab+Q7_none 
                +Q7_other2+Q8_recommend+Q10_Kaggle+Q10_Colab+Q10_azure+Q10_paper+Q10_binder+Q10_ocean+Q10_ibm+Q10_sage+Q10_emr
                +Q10_google.notebook+Q10_google_datalab+Q10_databricks+Q10_zeppelin+Q10_deepnote+Q10_observable+Q10_none+Q10_other+Q11_equipment
                +Q14_Matplotlib+Q14_seaborn+Q14_plotly+Q14_ggplot+Q14_shiny+Q14_d3+Q14_altair+Q14_bokeh
                +Q14_geoplotlib+Q14_leaflet+Q14_none+Q14_Other+Q15_yrs_ml+Q16_scikit+Q16_tensor+Q16_keras+Q16_pytorch
                +Q16_fast.ai+Q16_mxnet+Q16_xgboost+Q16_lightgbm+Q16_catboost+Q16_prophet+Q16_h2o+Q16_caret+Q16_tidymodels
                +Q16_jax+Q16_pytorch_lightning+Q16_huggingface+Q16_none+Q16_Other+Q17_linear+Q17_decision+Q17_gradient
                +Q17_bayesian+Q17_evolutionary+Q17_dense+Q17_convolutional+Q17_generative+Q17_recurrent+Q17_transformer+Q17_none+Q17_other
                +Q20_industry+Q21_num_emp+Q22_num_data_empl+Q23_bus_use_ml+Q24_ch1+Q24_ch2+Q24_ch3+Q24_ch4+Q24_ch5+Q24_ch6+Q24_ch7+Q24_ch8+Q25_income
                , data=trainingSet, method="class", 
                control=rpart.control(minsplit=25, cp=0.05))

sink("rProbDecisionTree_25_05.txt", append=FALSE, split=TRUE);

cat("\n###### Display the text output from each decision tree: ######\n")
printcp(MyTree)

# Show the result graphically
plotcp(MyTree, minline = FALSE)

#######################################################################
######                     Pruning the tree                      ######
# This builds a modified version of the original tree
# cp defines the minimum reduction in error needed to add an additional split.
prunedTree <- prune(MyTree, cp=0.01)

predTraining <- predict(MyTree, trainingSet, type="class") 
predValidation <- predict(MyTree, validationSet, type="class")

cat("\n###### Confusion Matrix for the training set ######\n")
table(Predicted=predTraining,Observed=trainingSet[, 8] )

cat("\n###### Confusion Matrix for the validation set ######\n")
table(Predicted=predValidation,Observed=validationSet[, 8] )

predRateTraining <- mean(predTraining == trainingSet[, 8])
predRateValidation <- mean(predValidation == validationSet[, 8])
sink()

prp(MyTree, main=paste("Decision Tree\n(Correct classification rate ",
                       round(predRateTraining,4)*100,
                       "% for the training set\n ",
                       round(predRateValidation,4)*100,
                       "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)

# And now turn on PDF output to send the same plot to a PDF file, saved to the working directory.
pdf("rProbDecisionTree_25_05.pdf");
prp(MyTree, main=paste("Decision Tree\n(Correct classification rate ",
                       round(predRateTraining,4)*100,
                       "% for the training set\n ",
                       round(predRateValidation,4)*100,
                       "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)
dev.off()

#More complex tree

MyTree <- rpart(Q7_R ~age+woman + Country+job+yrs_coding+Q7_python + Q7_sql + Q7_c+ Q7_c.. + Q7_java+Q7_javascript+Q7_julia+Q7_swift+Q7_bash+Q7_matlab+Q7_none 
                +Q7_other2+Q8_recommend+Q10_Kaggle+Q10_Colab+Q10_azure+Q10_paper+Q10_binder+Q10_ocean+Q10_ibm+Q10_sage+Q10_emr
                +Q10_google.notebook+Q10_google_datalab+Q10_databricks+Q10_zeppelin+Q10_deepnote+Q10_observable+Q10_none+Q10_other+Q11_equipment
                +Q14_Matplotlib+Q14_seaborn+Q14_plotly+Q14_ggplot+Q14_shiny+Q14_d3+Q14_altair+Q14_bokeh
                +Q14_geoplotlib+Q14_leaflet+Q14_none+Q14_Other+Q15_yrs_ml+Q16_scikit+Q16_tensor+Q16_keras+Q16_pytorch
                +Q16_fast.ai+Q16_mxnet+Q16_xgboost+Q16_lightgbm+Q16_catboost+Q16_prophet+Q16_h2o+Q16_caret+Q16_tidymodels
                +Q16_jax+Q16_pytorch_lightning+Q16_huggingface+Q16_none+Q16_Other+Q17_linear+Q17_decision+Q17_gradient
                +Q17_bayesian+Q17_evolutionary+Q17_dense+Q17_convolutional+Q17_generative+Q17_recurrent+Q17_transformer+Q17_none+Q17_other
                +Q20_industry+Q21_num_emp+Q22_num_data_empl+Q23_bus_use_ml+Q24_ch1+Q24_ch2+Q24_ch3+Q24_ch4+Q24_ch5+Q24_ch6+Q24_ch7+Q24_ch8+Q25_income
                , data=trainingSet, method="class", 
                control=rpart.control(minsplit=25, cp=0.005))

sink("rProbDecisionTree_25_005.txt", append=FALSE, split=TRUE);

cat("\n###### Display the text output from each decision tree: ######\n")
printcp(MyTree)

# Show the result graphically
plotcp(MyTree, minline = FALSE)

#######################################################################
######                     Pruning the tree                      ######
# This builds a modified version of the original tree
# cp defines the minimum reduction in error needed to add an additional split.
prunedTree <- prune(MyTree, cp=0.01)

predTraining <- predict(MyTree, trainingSet, type="class") 
predValidation <- predict(MyTree, validationSet, type="class")

cat("\n###### Confusion Matrix for the training set ######\n")
table(Predicted=predTraining,Observed=trainingSet[, 8] )

cat("\n###### Confusion Matrix for the validation set ######\n")
table(Predicted=predValidation,Observed=validationSet[, 8] )

predRateTraining <- mean(predTraining == trainingSet[, 8])
predRateValidation <- mean(predValidation == validationSet[, 8])
sink()

prp(MyTree, main=paste("Decision Tree\n(Correct classification rate ",
                       round(predRateTraining,4)*100,
                       "% for the training set\n ",
                       round(predRateValidation,4)*100,
                       "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)

# And now turn on PDF output to send the same plot to a PDF file, saved to the working directory.
pdf("rProbDecisionTree_25_005.pdf");
prp(MyTree, main=paste("Decision Tree\n(Correct classification rate ",
                       round(predRateTraining,4)*100,
                       "% for the training set\n ",
                       round(predRateValidation,4)*100,
                       "% for the validation set)"), 
    type=4, extra=6, faclen=0, under=TRUE)
dev.off()
