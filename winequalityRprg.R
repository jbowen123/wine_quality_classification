### Create wine data set,  
##########################################################

# Note: this process could take a couple of minutes
if(!require(Rtools)) install.packages("Rtools", repos = "http://cran.us.r-project.org")
#if(!require(bookdown)) install.packages("bookdown", repos = "http://cran.us.r-project.org")
#library(bookdown)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gglot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")

if(!require(tidyselect)) install.packages("tidyselect", repos = "http://cran.us.r-project.org")

###load the applications required for the cross validated logistic regression, the randomForest model and the gbs
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
library(randomForest)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
library(caTools)  # For Logistic regression
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
library(pROC)  # For ROC curve to evaluate model
if(!require(mltools)) install.packages("mltools", repos = "http://cran.us.r-project.org")
library(mltools)  # For ROC curve to evaluate model
#if(!require(predictABEL)) install.packages("predictABEL", repos = "https://rdocumentation.org/packages/PredictABEL/versions/1.2-4")

#library(predictABEL)  # For ROC curve to evaluate model
if(!require(plotROC)) install.packages("plotROC", repos = "http://cran.us.r-project.org")
library(plotROC)  # For ROC curve to evaluate model

if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
library(ROCR)  # For ROC curve to evaluate model

if(!require(MLeval)) install.packages("MLeval", repos = "http://cran.us.r-project.org")
library(MLeval) 
if(!require(MLmetrics)) install.packages("MLmetrics", repos = "http://cran.us.r-project.org")
library(MLmetrics) 
######tests for multicollinearity######
if(!require(multiColl)) install.packages("multiColl", repos = "http://cran.us.r-project.org")
library(multiColl) # for tests of multicollinearity

if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")

library(purrr) # for functional programming (map)

if(!require(robustbase)) install.packages("robustbase", repos = "http://cran.us.r-project.org")

if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")

if(!require(rstatix)) install.packages("rstatix", repos = "http://cran.us.r-project.org")
library(rstatix)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
library(RColorBrewer)
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
library(car)

##install the factoMineR and factoextra packages
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(FactoMineR)) install.packages("FactoMineR", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
library(FactoMineR)
library(factoextra)
library(cluster)

if(!require(ellipse)) install.packages("ellipse", repos = "http://cran.us.r-project.org")
library(ellipse)
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.us.r-project.org")
library(corrgram)



library(matrixStats)
library(robustbase)
if(!require(latexpdf)) install.packages("latexpdf", repos = "http://cran.us.r-project.org")

library(latexpdf)


if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tinytex)
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(vctrs)
library(forcats)
library(ggplot2)

library(tidyselect)
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

library(corrplot)
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
library(GGally)
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

library(ggpubr)
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
library(e1071)
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
library(rmarkdown)
library(knitr)
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
library(utils)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(kableExtra)
if(!require(rio)) install.packages("rio", repos = "http://cran.us.r-project.org")
library(rio)


winedataimport <- import("https://raw.githubusercontent.com/jbowen123/wine_quality_classification/main/winequality.csv")

winequality <- winedataimport
head(winequality)
table_1 <- summary(winequality)
table_1
str(winequality)
##look at the dataset
table(winequality$quality)
get_counts <- winequality %>% mutate(quality=as.factor(quality)) %>% group_by(quality) %>% summarise(n=n())
data.frame(get_counts)
str(get_counts)

##remove the duplicate records
winequality_nd <- winequality[!duplicated(winequality),]
table_2 <- summary(winequality_nd)
str(table_2)
winequality_nd_count <- winequality_nd %>% group_by(quality) %>% summarise(count=n())
winequality_nd_count
table(winequality_nd$quality)
duplicates <- winequality[duplicated(winequality),]
summary(duplicates)
str(duplicates)
duplicates1 <- duplicates %>%mutate(quality=as.factor(quality)) %>% group_by(quality) %>% summarise(duplicate_count=n())
str(duplicates1)
##add two rows to duplicates1 for zero 3 and zero 4
rows <- rbind(c(3,0), c(4,0)) 
colnames(rows) <- c("quality", "duplicate_count")
rows1 <- as.tibble(rows) %>% mutate(quality=as.factor(quality), duplicate_count=as.numeric(duplicate_count))
str(rows1)
rows2 <- data.frame(rows1)
str(rows2)
duplicates3 <- data.frame(duplicates1) #%>% mutate(quality=as.factor(quality))
str(duplicates3)
colnames(get_counts) <- c("quality", "original_count")

all_duplicate_counts <- rbind(rows2,duplicates3) 
str(all_duplicate_counts) 


##now left join the get_counts tibble (convert to a data.frame first) with all_duplicate_counts so we get a portion of totals
more_duplicate <- left_join(all_duplicate_counts, as.data.frame(get_counts), by="quality") %>% mutate(original_count=as.numeric(original_count))
str(more_duplicate)

duplicate.portions <- more_duplicate %>% mutate(Total_duplicates=sum(duplicate_count), Original_Total=sum(original_count), Fraction_of_original_count= (duplicate_count/original_count))
duplicate.portions

str(duplicate.portions)
duplicate_portions <- format(duplicate.portions[, c(1,2,3,6)],digits=1)
duplicate_portions
##look at the distribution of the wine quality, no duplicates
distribquality <- ggplot(winequality_nd, aes(x=quality))+geom_histogram(bins=20)+geom_vline(data=winequality, aes(xintercept=mean(quality)), col="red")+geom_vline(data=winequality, aes(xintercept=median(quality)), col="green")+scale_x_continuous(breaks=c(1:10))+ggtitle("distribution of wine quality; green line= median, red line= average quality")
distribquality

###describe the variables by mean, median, skewness, min and max:
mean <- as.matrix(format(colMeans(winequality_nd), digits=1))
median <- as.matrix(format(colMedians(as.matrix(winequality_nd)),digits=1))
maximum <- as.matrix(format(colMaxs(as.matrix(winequality_nd)),digits=1))
minimum <- as.matrix(format(colMins(as.matrix(winequality_nd)),digits=1))

#maximum
#minimum
#median
#mean
stats <- cbind(mean, median,minimum, maximum)
colnames(stats) <- c("mean", "median", "min", "max")
stats<- noquote(stats)
stats

##standardize the variables contributing to wine quality
#function for standardizing to z distribution: 
mean_sd_standard <- function(x) {
  (x-mean(x))/sd(x)
}
# Apply the function to each numeric variable in the data set
winequality_standardized <- winequality_nd %>% mutate(quality=as.factor(quality)) %>%
  mutate_if(is.numeric, mean_sd_standard)
str(winequality_standardized)

winequality_standardizedA <- winequality_standardized %>% mutate(quality=as.numeric(as.character(quality)))
#Summarize the standardized data
table_3 <- summary(winequality_standardizedA)

mean2 <- as.matrix(round(colMeans(winequality_standardizedA), digits=2))
median2 <- as.matrix(format(colMedians(as.matrix(winequality_standardizedA)),digits=1))
maximum2 <- as.matrix(format(colMaxs(as.matrix(winequality_standardizedA)),digits=3))
minimum2 <- as.matrix(format(colMins(as.matrix(winequality_standardizedA)),digits=3))

#maximum2
#minimum2
#median2
mean2
stats2 <- cbind(mean2, median2,minimum2, maximum2)
colnames(stats2) <- c("mean", "median", "min", "max")
stats2a <-noquote(stats2[c(1:11),])
stats2a


##rearrange the columns so that the quality column is first
winequalitystandardized <- winequality_standardized %>% select(quality, everything()) 
#str(winequalitystandardized)

##try another correlogram form



##the data in this procedure must be numeric
#str(winequalitystandardized)
winequality_numeric <- winequalitystandardized %>% mutate(quality=as.numeric(quality))
#str(winequality_numeric)
# Use of the winequality_standardized data proposed by R...wine quality is a numeric (not standardized)
winedata <- cor(winequality_numeric)
#winedata
# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(200)

##create corellogram......this is the database of no duplicates, not standardized, quality is a number
figure_2 <- ggcorr(winedata, method = c("everything", "pearson"), hjust = 0.8, size = 3, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")  + ggplot2::labs(title = "correlation (Pearson method) of wine variables")
figure_2


####this also works...insignificant correlations marked by an x
cor.mat <- winequality_numeric %>% cor_mat()


##do the correlogram under corrgram....use the 'order=TRUE' option to identify 'like' contributors 
##order=TRUE will cause the variables to be ordered 
##using principal component analysis of the correlation matrix.

##keep##significance level for correlation is at 0.01 for p.mat
testRes <- cor.mtest(winequality_numeric, conf.level = 0.95)

Fig3_winequality <- corrplot(cor(winequality_numeric), type="upper", order="FPC", 
                             p.mat=testRes$p, sig.level = 0.01, insig='blank', title = "correlations  - ordered by first principal component order", mar=c(0,0,.75,0), number.cex = .7)

#print(Fig3_winequality)

#library(corrplot)
### mat : is a matrix of data correlations analyzing the p-value  pvalue gt .01 means correlation not significant
# ... : further arguments to pass to the native R cor.test function



#fig7corrplot <- corrplot(cor(winequality_numeric), method="number", par(cex= 0.55))
#fig7corrplot

###START HERE.....CREATE TWO GROUP PCA...winequalitystandardized2G because wine quality over 6 is good, 6 and under is poor

winequalitystandardized2G <- winequalitystandardized %>% mutate(quality=as.numeric(as.character(quality)),group=ifelse(quality > 6, "good","poor"))
str(winequalitystandardized2G) 
summary(winequalitystandardized2G)

str(winequalitystandardized2G)
winequal1 <- as.factor(winequalitystandardized2G[,13])
winequal2 <- cbind(winequal1, winequalitystandardized2G[,2:12])
str(winequal2)
summary(winequal2)


##Reduce the dimensions with Principal Component Analysis
winequality_pca <- PCA(winequal2[,2:12])
## the results indicate that the first two principal components explain 28% and 17% of the variance.

###not for the write up
pca_var_plot <- fviz_pca_var(winequality_pca)

##sum the variance preserve by th first two component
variance_first_two_pca <- winequality_pca$eig[1,2] + winequality_pca$eig[2,2]

##keep###visualize the variables on the grid - contribution to pc1 and pc2
contrib_var <- fviz_pca_var(winequality_pca, col.var="contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"))
print(contrib_var)
###positively correlated variables are grouped together; negatively correlated variables are 
###positioned on opposite sides of the plot origin (opposed quadrants)
##the distance from origin measures the quality of the variables on the factor map






###examine the eigenvalues (which measure the amount of variation retained by each principal component)
eig.val <- get_eigenvalue(winequality_pca)
eig.val
##the proportion of variation explained by each eigenvalue is given in the second column
##to look at patterns in the data, a cumulative.variance.percent of 70% is considered satisfactory
##in this case, the first four or five eigenvalues (principal components)
##explain 80% of the variance

##keep##a scree plot presents the eigenvalues in a plot
scree_plot_eigenvalues <- fviz_eig(winequality_pca, addlabels=TRUE, ylim=c(0,30))
scree_plot_eigenvalues

###The Results
##to extract the results for variables
#qualityvar <- get_pca_var(winequality_pca)
##quality of representation is called cos2 (squared coordinates)
##plot the quality measure cos2 of the variables
##a high cos2 indicates a good representation of the variable..it is positioned close
##to the circumference of the circle
#res.desc <- dimdesc(winequality_pca, axes=c(1,2), proba=0.05)
##describe dimension 1 and dimension 2
#res.desc$Dim.1
#res.desc$Dim.2
#qualityvar <- fviz_cos2(winequality_pca, choice="var", axes = 1:2)
#qualityvar

##dimension description
###identify the most significantly associated variables wth a given principal
###component
##circle of correlations, colored by cos2 quality of representation
##cos2circle <- fviz_pca_var(winequality_pca, col.var="cos2", gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),repel=TRUE)
##cos2circle

##keep####contributions of variables to the principal components
one <- fviz_contrib(winequality_pca, choice="var", axes=1, top =10)
two <- fviz_contrib(winequality_pca, choice="var", axes=2, top=10)
one
two
###graph the top contributors to each dimension
#topcontrib <- fviz_contrib(winequality_pca, choice="var", axes=1:2, top=10)
#topcontrib

##keep#title <- "Contributors to each dimension"
var_wine <- get_pca_var(winequality_pca)
corrplott <-corrplot(var_wine$contrib, is.corr=FALSE, col = COL2('RdBu', 10))
corrplott



#assess ability to cluster....shows scatter of quality groups
clustdf <- winequalitystandardized[,2:12]
#str(clustdf)

##keep# Plot grouped quality data set####group good versus group poor
tendency <- fviz_pca_ind(prcomp(clustdf), title = "PCA - wine data", 
                         habillage = winequal2$winequal1,  palette = "jco",
                         geom = "point", ggtheme = theme_classic(),
                         legend = "bottom")
tendency


###going forward, it might be useful to consider dropping the chloride, residual.sugar and sulfates variables...
###these variables have low cos2 values and contribute the least to the principal components
###alcohol and volatile.acidity are kept, even as they are on the lower end for cos2
###the position of the alcohol and volatile.acidity change between dimensions

############################################################################################
#####################################################################################
#################################################################################

############from here begins the modelling for predictions and estimation of ROC and PR curves
#############################################################################################


##set a seedvalue to set the seed

set.seed(5627)


##first up, create the dataset with the groups and the contributing chemical descriptors
str(winequal2)
winequalcount <- table(winequal2$winequal1)
winequalcount
#create test and train datasets....use winequal2 for the first run of cross validated logistic regression
split_dummy <- sample(c(rep(0, 0.8 * nrow(winequal2)),  # Create dummy for splitting
                        rep(1, 0.2 * nrow(winequal2))))
table(split_dummy) 

data_train2 <- winequal2[split_dummy == 0, ]             # Create train data with all variables
data_test2 <- winequal2[split_dummy == 1, ]      #Create test data with all variables
str(data_train2)
str(data_test2)
winequalcounttrain <-table(data_train2$winequal1)
winequalcounttrain
winequalcounttest <- table(data_test2$winequal1)
winequalcounttest
######Part 1#### the cross validated logistic regressions

###the cross validation model for the logistic regression with all variables..
cntrlspecs_cv <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)


set.seed(5627)

logistic_model_cv <- train(winequal1 ~ ., data=data_train2, method="glm", family="binomial", trControl=cntrlspecs_cv)

#print(logistic_model_cv)
#print.train(logistic_model_cv)
summary(logistic_model_cv)
logistic_model_cv



str(data_train2)
head(data_train2)
cte <- array(1,length(data_train2[,1]))
train2.X <- cbind(cte,data_train2[,-(1)])

CN.test <-CN(train2.X)
CN.test
####Values of CN between 20 and 30 indicate near moderate multicollinearity while values higher than
###30 indicate near worrying collinearity. https://cran.r-project.org/web/packages/multiColl/multiColl.pdf
CN.test.var <- CNs(train2.X)
CN.test.var

ki.test <- ki(train2.X)
ki.test

multiCol.test <- multiCol(train2.X, dummy=FALSE, pos=NULL)
multiCol.test
VIFmeasures <- multiCol.test$`Variance Inflation Factors`
VIFmeasures

#####all tests indicate that multicollinearity is not a problem

###variable importance
var_logistic_model_cv <- varImp(logistic_model_cv)
var_logistic_model_cv

###create the variable importance table
Variable_importance_models <- tibble(method = "cross validated logistic model", sulphates=100, alcohol=68.553, volatile.acidity=52.120,
                                     fixed.acidity=44.430, density=35.858, total.sulfur.dioxide=35.786, residual.sugar=33.608,
                                     chlorides=28.272, pH=10.019, citric.acid=3.141, free.sulfur.dioxide=0.00)

predict_logistic_model_cv <- predict(logistic_model_cv, newdata=data_test2)
predict_logistic_model_cv

###now, do the confusion matrix

###put the predicted values and the true values into a table
table_cv <- table(predict_logistic_model_cv, data_test2$winequal1)
table_cv


recalllm <- table_cv[1,1]/(table_cv[1,1]+table_cv[2,1])
precisionlm <- table_cv[1,1]/(table_cv[1,1]+table_cv[1,2])
f1lm <- 2/((1/precisionlm)+(1/recalllm))
precisionlm
recalllm
f1lm
###test the predictions in confusion matrix
resultslogisticcv <- confusionMatrix(table_cv)
resultslogisticcv
str(resultslogisticcv)

####take apart the confusion matrix
gettable <- as.data.frame(resultslogisticcv$table)
gettable
true_good <- gettable[1,3]
false_good <- gettable[3,3]
false_poor <- gettable[2,3]
true_poor <- gettable[4,3]

misclassificationlm <- (table_cv[1,2]+table_cv[2,1])/(table_cv[1,1]+table_cv[1,2]+table_cv[2,1]+table_cv[2,2])
misclassificationlm

getKappa <- as.data.frame(resultslogisticcv$overall)
Kappacv <- getKappa[2,]
Kappacv
Kappa_results <- tibble(method = "cross validated logistic model", Kappa= Kappacv,
                        Recall=recalllm, Precision = precisionlm, F1 = f1lm, misclassification = misclassificationlm, 
                        true_good=true_good, false_good=false_good, false_poor=false_poor, true_poor=true_poor)



###create the confusion matrix for the precision recall indicators
lm_pr <- confusionMatrix(table_cv, mode = "prec_recall")
lm_pr
#####testing something
##tableup2 <- table(predict_logistic_model_cv,data_test2$winequal1)
##tableup2
##recallup2 <- tableup2[1,1]/(tableup2[1,1]+tableup2[2,1])
##precisionup2 <- tableup2[1,1]/(tableup2[1,1]+tableup2[1,2])
##f1up2 <- 2/((1/precisionup2)+(1/recallup2))
##precisionup2
##recallup2
##f1up2
##observed from the above, sulphates, alcohol, density, total.sulfur.dioxide, chlorides, residual.sugar. volatile.acidity, fixed.acidity are
###all significant contributors (at varying levels of significance)
##accuracy seems pretty good.  but the kappa indicates that guessing is whether a wine is good or not is better than the model...so, why bother with a model?
####https://www.r-bloggers.com/2019/12/how-to-make-a-precision-recall-curve-in-r/




#####
####
####create the precision recall curve

## run MLeval
x <- evalm(list(logistic_model_cv))
## curves and metrics are in the 'x' list object

#######does class imbalance affect the results of the logistic model?
###the model may suffer from class bias, as there are more group 0 than group 1 in the data_train set
table(data_train2$winequal1)
## to resolve the class bias, resample such that the group 0 and group 1 are in similar proportions in the train set

####try downsampling....taking the train set and removing some of the 'poor' wines to create a balanced ratio of good to poor wines
#####test set remains unaffected
poor <- which(data_train2$winequal1 == "poor")
good <- which(data_train2$winequal1 == "good")
length(poor)
length(good)
poor.downsample <- sample(poor, length(good)) 
str(poor.downsample)
newtrainpoor <- data_train2[c(poor.downsample),]
str(newtrainpoor)
newtraingood <- data_train2[c(good),]
str(newtraingood)
newtraindown <- rbind(newtraingood, newtrainpoor)
str(newtraindown)

###the cross validation model for the logistic regression with all variables..
cntrlspecs_cv <- trainControl(method="cv", number=10, savePredictions="all", classProbs=TRUE)


set.seed(5627)

logistic_model_cvdown <- train(winequal1 ~ ., data=newtraindown, method="glm", family="binomial", trControl=cntrlspecs_cv)

print(logistic_model_cvdown)
print.train(logistic_model_cvdown)
summary(logistic_model_cvdown)
logistic_model_cvdown

######look at the results of the logistic regression ....recall and precision from confusion matrix
###variable importance
var_logistic_model_cvdown <- varImp(logistic_model_cvdown)
var_logistic_model_cvdown

###create the variable importance table
Variable_importance_models <- tibble(method = "cross validated logistic model down sample", sulphates=100, alcohol=68.553, volatile.acidity=52.120,
                                     fixed.acidity=44.430, density=35.858, total.sulfur.dioxide=35.786, residual.sugar=33.608,
                                     chlorides=28.272, pH=10.019, citric.acid=3.141, free.sulfur.dioxide=0.00)

predict_logistic_model_cvdown <- predict(logistic_model_cvdown, newdata=data_test2)
predict_logistic_model_cvdown

###now, do the confusion matrix

###put the predicted values and the true values into a table
table_cvdown <- table(predict_logistic_model_cvdown, data_test2$winequal1)
table_cvdown


recalllmdown <- table_cvdown[1,1]/(table_cvdown[1,1]+table_cvdown[2,1])
precisionlmdown <- table_cvdown[1,1]/(table_cvdown[1,1]+table_cvdown[1,2])
f1lmdown <- 2/((1/precisionlmdown)+(1/recalllmdown))
precisionlmdown
recalllmdown
f1lmdown
###test the predictions in confusion matrix
resultslogisticcvdown <- confusionMatrix(table_cvdown)
resultslogisticcvdown
str(resultslogisticcvdown)

####take apart the confusion matrix
gettabledown <- as.data.frame(resultslogisticcvdown$table)
gettabledown
true_good <- gettabledown[1,3]
false_good <- gettabledown[3,3]
false_poor <- gettabledown[2,3]
true_poor <- gettabledown[4,3]

misclassificationlmdown <- (table_cvdown[1,2]+table_cvdown[2,1])/(table_cvdown[1,1]+table_cvdown[1,2]+table_cvdown[2,1]+table_cvdown[2,2])
misclassificationlmdown

getKappa <- as.data.frame(resultslogisticcvdown$overall)
Kappacvdown <- getKappa[2,]
Kappacvdown
Kappa_results <- add_row(Kappa_results,method = "cross validated logistic model down sample", Kappa= Kappacvdown,
                         Recall=recalllmdown, Precision = precisionlmdown, F1 = f1lmdown, misclassification = misclassificationlmdown, 
                         true_good=true_good, false_good=false_good, false_poor=false_poor, true_poor=true_poor)



###create the confusion matrix for the precision recall indicators
lm_prdown <- confusionMatrix(table_cvdown, mode = "prec_recall")
lm_prdown


## run MLeval
comparelogisticmodels <- evalm(list(logistic_model_cv, logistic_model_cvdown))
comparelogisticmodels$roc
## curves and metrics are in the 'x' list object

###the Random Forest model to predict a wine in group 1 from the chemical components 
###the reference for the Random Forest model is https://www.guru99.com/r-random-forest-tutorial.html  and https://rpubs.com/Mentors_Ubiqum/tunegrid_tunelength
### a detailed discussion for 'tuning' mtry, nodesize and sample size is found under https://arxiv.org/pdf/1804.03515.pdf
#####from the article above:  Note that the convergence rate of RF does not only depend on the considered dataset's characteristics but possibly
##also on hyperparameters. Lower sample size (see Section 2.1.2), higher node size values (see Section 2.1.3) and smaller
##mtry values (see Section 2.1.1) lead to less correlated trees. These trees are more different from each other and
##are expected to provide more different predictions. Therefore, we suppose that more trees are needed to get clear
##predictions for each observation which leads to a higher number of trees for obtaining convergence.




###the cross validated model is to select the best mtry, the best number of trees to select, the best number of nodes
###the cross validated selection for the best model is done by a grid pattern, as it is easiest to understand
###the random forest models tested are on the full set of variables as described in the dataset winequal2

#####use class weights to correct for class imbalance
#####now, bring in the weights, to see if 'sample up' or 'sample down' would improve the results
# Create model weights for the random forest model, to correct for class imbalance (they sum to one)
####not required for report...no improvements in the model
#model_weights <- ifelse(data_train2$winequal1 == "poor", (1/table(data_train2$winequal1)[2])*.5, (1/table(data_train2$winequal1)[1])*.5)
#model_weights



#####The random forest models....####this first section has been used to 'tune' the model for mtry, nodesize, maxnodes and ntree....
# Define the control
tuneGrid <- expand.grid(.mtry = c(1: 10))
set.seed(5627)
trControlRF <- trainControl(method = "cv", number = 10,search = "grid", classProbs=TRUE)

rf_wine_default <- train(winequal1 ~ ., data=data_train2, metric="Accuracy", tuneGrid = tuneGrid, trControl=trControlRF, importance=TRUE, nodesize = 14,
                         ntree = 300)
#-The first parameter specifies our formula: winequal1 ~ . (we want to predict wine quality using each of the remaining columns of data).
#ntree defines the number of trees to be generated. It is typical to test a range of values for this parameter (i.e. 100,200,300,400,500) and choose the one that minimises the OOB estimate of error rate.
#mtry is the number of features used in the construction of each tree. These features are selected at random, which is where the ârandomâ in ârandom forestsâ comes from. The default value for this parameter, when performing classification, is sqrt(number of features).
#importance enables the algorithm to calculate variable importance
results_rf_default <- rf_wine_default
results_rf_default




###store the best mtry for Accuracy
(bestmtry <- rf_wine_default$bestTune$mtry)
(accuracywbestmtry <- max(rf_wine_default$results$Accuracy))

##
###find the best number of nodes in the random forest model, using bestmtry
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = bestmtry)
for (maxnodes in c(8: 20)) {
  set.seed(5627)
  rf_maxnode <- train(winequal1~.,
                      data = data_train2,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControlRF,
                      # weights = model_weights,#
                      importance = TRUE,
                      nodesize = 8,
                      maxnodes = maxnodes,
                      ntree = 450)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
best_nodes <- summary(results_mtry)
(best_nodes)
#if i go for the best accuracy and the best kappa combination, i think the optimal number of nodes is 8 or more (best accuracy and best kappa)
###the program describes recommends taking the best nodes and then setting a maximum nodes to tune for the optimal number of trees

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800)) {
  set.seed(5627)
  rf_maxtrees <- train(winequal1~.,
                       data = data_train2,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControlRF,
                       # weights = model_weights,#
                       importance = TRUE,
                       nodesize = 8,
                       maxnodes = 18,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
####put in the best tuned parameters for the random forest model
#####lots of work done to test the parameters of the random forest model
####but all that has been revealed is that mytry is rule of thumb, nodes-12-24, ntrees=300
#
####the results indicate that there is no improvement to accuracy or kappa by increasing the number of trees past 250
###max accuracy is at 300 trees;  max kappa is also at 300 trees (except that i ran the rf model and got better results with 450 trees)
###optimal max number of nodes is 18 ;
##optimal mtry is 3


# Define the control
#tuneGrid <- expand.grid(.mtry = c(1: 10))
set.seed(5627)
tuneGrid <- expand.grid(.mtry = c(1: 10))
trControlRF <- trainControl(method = "cv", number = 10,search = "grid", classProbs=TRUE, summaryFunction=twoClassSummary)

rf_wine <- train(winequal1 ~ ., data=data_train2,
                 metric="ROC",
                 tuneGrid = tuneGrid,
                 trControl=trControlRF,
                 #weights = model_weights,#
                 importance=TRUE,
                 nodesize = 8,
                 maxnodes=19,
                 ntree = 450)

summary(rf_wine)


var_rfmodel <- varImp(rf_wine)
var_rfmodel
Variable_importance_models <- add_row(Variable_importance_models,method = "random forest", sulphates=74.62, alcohol=100.00, volatile.acidity=42.61,
                                      fixed.acidity=27.50, density=30.91, total.sulfur.dioxide=24.60, residual.sugar=0.00,
                                      chlorides=5.96, pH=10.43, citric.acid=13.42, free.sulfur.dioxide=11.09)

predict_rfmodel <- predict(rf_wine, newdata=data_test2)
#predict_rfmodel

###put the predicted values and the true values into a table
table_rf <- table(predict_rfmodel, data_test2$winequal1)



###create the confusion matrix for the precision recall indicators
rf_pr <- confusionMatrix(table_rf, mode = "prec_recall")
rf_pr


###test the predictions in confusion matrix
resultsrfmodel <- confusionMatrix(table_rf)
getKappa <- as.data.frame(resultsrfmodel$overall)
Kapparf <- getKappa[2,]


tablerfc <- table(predict_rfmodel,data_test2$winequal1)
tablerfc
recallrfc <- tablerfc[1,1]/(tablerfc[1,1]+tablerfc[2,1])
precisionrfc <- tablerfc[1,1]/(tablerfc[1,1]+tablerfc[1,2])
f1rfc <- 2/((1/precisionrfc)+(1/recallrfc))
precisionrfc
recallrfc
f1rfc
misclassificationrf <- (tablerfc[1,2]+tablerfc[2,1])/(tablerfc[1,1]+tablerfc[1,2]+tablerfc[2,1]+tablerfc[2,2])
misclassificationrf
gettablerf <- as.data.frame(resultsrfmodel$table)
gettablerf
true_goodrf <- gettablerf[1,3]
false_goodrf <- gettablerf[3,3]
false_poorrf <- gettablerf[2,3]
true_poorrf <- gettablerf[4,3]

Kappa_results <- add_row(Kappa_results, method = "random forest model", Kappa= Kapparf,
                         Recall=recallrfc, Precision = precisionrfc, F1 = f1rfc, misclassification = misclassificationrf,
                         true_good=true_goodrf, false_good=false_goodrf, false_poor=false_poorrf, true_poor=true_poorrf)




#####
####


#Build custom AUC function to extract AUC
# from the caret model object
###this is a function created here only as a test...to ensure that i can put 
#the data into the martin model later... to put the data in (to create various models adjusting for bias, later)
test_roc <- function(model, data) {
  roc(data$winequal1,predict(model, data, type = "prob")[, "good"])
}

random_forest_auc <- rf_wine %>%test_roc(data = data_test2) %>% auc()
random_forest_auc

####################################################################################
#####################################################################################
###############THIS IS THE SECOND RANDOM FOREST MODEL...MERCIFULLY NOT TUNED ##############

# Define the control
#tuneGrid <- expand.grid(.mtry = c(1: 10))
set.seed(5627)
tuneGrid <- expand.grid(.mtry = c(1: 10))
trControlRF2 <- trainControl(method = "cv", number = 10,search = "grid", classProbs=TRUE, summaryFunction=twoClassSummary)

rf_wineNT <- train(winequal1 ~ ., data=data_train2,
                   metric="ROC",
                   tuneGrid = tuneGrid,
                   trControl=trControlRF2,
                   #weights = model_weights,#
                   importance=TRUE,
                   #nodesize = 8,#
                   #maxnodes=19,#
                   #ntree = 450#)
)

summary(rf_wineNT)


var_rfmodelNT <- varImp(rf_wineNT)
var_rfmodelNT

Variable_importance_models <- add_row(Variable_importance_models,method = "random forest Not Tuned", sulphates=84.820, alcohol=100.00, volatile.acidity=58.022,
                                      fixed.acidity=31.813, density=46.174, total.sulfur.dioxide=46.953, residual.sugar=0.00,
                                      chlorides=34.441, pH=6.084, citric.acid=25.752, free.sulfur.dioxide=2.101)



predict_rfmodelNT <- predict(rf_wineNT, newdata=data_test2)
#predict_rfmodel

###put the predicted values and the true values into a table
table_rfNT <- table(predict_rfmodelNT, data_test2$winequal1)



###create the confusion matrix for the precision recall indicators
rf_prNT <- confusionMatrix(table_rfNT, mode = "prec_recall")
rf_prNT


###test the predictions in confusion matrix
resultsrfmodelNT <- confusionMatrix(table_rfNT)
getKappa <- as.data.frame(resultsrfmodel$overall)
KapparfNT <- getKappa[2,]


tablerfcNT <- table(predict_rfmodelNT,data_test2$winequal1)
tablerfcNT
recallrfcNT <- tablerfcNT[1,1]/(tablerfcNT[1,1]+tablerfcNT[2,1])
precisionrfcNT <- tablerfcNT[1,1]/(tablerfcNT[1,1]+tablerfcNT[1,2])
f1rfcNT <- 2/((1/precisionrfcNT)+(1/recallrfcNT))
precisionrfcNT
recallrfcNT
f1rfcNT
misclassificationrfNT <- (tablerfcNT[1,2]+tablerfcNT[2,1])/(tablerfcNT[1,1]+tablerfcNT[1,2]+tablerfcNT[2,1]+tablerfcNT[2,2])
misclassificationrfNT
gettablerfNT <- as.data.frame(resultsrfmodelNT$table)
gettablerfNT
true_goodrfNT <- gettablerfNT[1,3]
false_goodrfNT <- gettablerfNT[3,3]
false_poorrfNT <- gettablerfNT[2,3]
true_poorrfNT <- gettablerfNT[4,3]

Kappa_results <- add_row(Kappa_results, method = "random forest model not tuned", Kappa= KapparfNT,
                         Recall=recallrfcNT, Precision = precisionrfcNT, F1 = f1rfcNT, misclassification = misclassificationrfNT,
                         true_good=true_goodrfNT, false_good=false_goodrfNT, false_poor=false_poorrfNT, true_poor=true_poorrfNT)

#####
####


#Build custom AUC function to extract AUC
# from the caret model object
###this is a function created to put the data in (to create various models adjusting for bias, later)
test_roc <- function(model, data) {
  roc(data$winequal1,predict(model, data, type = "prob")[, "good"])
}

random_forest_aucNT <- rf_wineNT %>%test_roc(data = data_test2) %>% auc()
random_forest_aucNT

##put in the boosted tree per https://dpmartin42.github.io/posts/r/imbalanced-classes-part-1
#### part 2 of the above reference is http://dpmartin42.github.io/posts/r/imbalanced-classes-part-2

#prop.table(table(data_train2$winequal1))
#prop.table(table(data_test2$winequal1))


####per the above, the class bias does not seem to be serious...but, if i correct for class bias, 
####per the author referenced above:  I have actually found that in many cases, there is no
####huge benefit in using either weighting or sampling techniques when classes are moderately imbalanced
####(i.e., no worse than 10:1) in conjunction with a threshold-invariant metric like the AUC.
#################################################################
###############################################################
##############################################################
####proceed with the gbm as described and adapted from https://dpmartin42.github.io/posts/r/imbalanced-classes-part-1
##installations required



# Build a standard classifier using a gradient boosted machine

set.seed(5627)

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)



orig_fit <- train(winequal1 ~ .,
                  data = data_train2,
                  method = "gbm",
                  distribution="bernoulli", ##for binomial##
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)


print(orig_fit)
summary(orig_fit)
predictorigin <- predict(orig_fit, newdata=data_test2)
predictorigin
tableorigin <- table(predictorigin, data_test2$winequal1)
confusionMatrix(tableorigin)
Variable_importance_models <- add_row(Variable_importance_models,method = "gbm - original: relative influence", sulphates=17.96, alcohol=29.22, volatile.acidity=12.84,
                                      fixed.acidity=6.50, density=5.23, total.sulfur.dioxide=8.67, residual.sugar=3.41,
                                      chlorides=3.67, pH=3.14, citric.acid=6.00, free.sulfur.dioxide=3.45)

# Build custom AUC function to extract AUC
# from the caret model object

###this is a function created to put the data in (to create various models adjusting for bias, later)
test_roc <- function(model, data) {
  
  roc(data$winequal1,
      predict(model, data, type = "prob")[, "good"])
  
}




#####now, bring in the weights, to see if 'sample up' or 'sample down' would improve the results
# Create model weights (they sum to one)

model_weights <- ifelse(data_train2$winequal1 == "poor", (1/table(data_train2$winequal1)[2])*.5, (1/table(data_train2$winequal1)[1])*.5)

prop.table(table(model_weights))

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model...

weighted_fit <- train(winequal1 ~ .,
                      data = data_train2,
                      method = "gbm",
                      distribution="bernoulli", ##for binomial##
                      verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)

summary(weighted_fit)
Variable_importance_models <- add_row(Variable_importance_models,method = "gbm - weighted fit: relative influence", sulphates=26.15, alcohol=47.88, volatile.acidity=15.81,
                                      fixed.acidity=2.92, density=0.7, total.sulfur.dioxide=3.61, residual.sugar=0.00,
                                      chlorides=1.12, pH=0.00, citric.acid=1.82, free.sulfur.dioxide=0.00)
# Build down-sampled model

ctrl$sampling <- "down"

down_fit <- train(winequal1 ~ .,
                  data = data_train2,
                  method = "gbm",
                  distribution="bernoulli", ##for binomial##
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

summary(down_fit)
Variable_importance_models <- add_row(Variable_importance_models,method = "gbm - down fit: relative influence", sulphates=13.44, alcohol=43.40, volatile.acidity=11.30,
                                      fixed.acidity=3.25, density=0.80, total.sulfur.dioxide=6.20, residual.sugar=1.17,
                                      chlorides=9.02, pH=1.8, citric.acid=6.88, free.sulfur.dioxide=2.71)

# Build up-sampled model

ctrl$sampling <- "up"

up_fit <- train(winequal1 ~ .,
                data = data_train2,
                method = "gbm",
                distribution="bernoulli", ##for binomial##
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)

summary(up_fit)
Variable_importance_models <- add_row(Variable_importance_models,method = "gbm - up fit: relative influence", sulphates=23.50, alcohol=47.8, volatile.acidity=13.49,
                                      fixed.acidity=2.08, density=1.38, total.sulfur.dioxide=3.86, residual.sugar=0.78,
                                      chlorides=1.98, pH=0.73, citric.acid=3.66, free.sulfur.dioxide=0.74)
Variable_importance_models <- as.data.frame(Variable_importance_models) %>% format(digits=0)



model_list <- list(original = orig_fit,
                   down = down_fit,
                   up = up_fit,
                   weighted=weighted_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = data_test2)


model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)


# Plot ROC curve for all 3 models

custom_col <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

fourgbm <-ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
fourgbm

model_list <- list(original = orig_fit,
                   weighted=weighted_fit,
                   rf=rf_wine,
                   rfNT=rf_wineNT,
                   logistic=logistic_model_cv)



model_list_roc <- model_list %>%
  map(test_roc, data = data_test2)


model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)


# Plot ROC curve for  the models

custom_col <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00") 

comparefinalmodel <- ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

comparefinalmodel

#get confusion matrix
###necessary to run predict function outside of the function for ROC curve


predictwted <- predict(weighted_fit, newdata=data_test2)
predictup  <- predict(up_fit, newdata=data_test2)
predictdown <- predict(down_fit, newdata=data_test2)

tableorigin <- table(predictorigin, data_test2$winequal1)
tableorigin
###get precision from the tables as predictup[1,1]/predictup[1,1]+predictup[2,1]
###get recall from the tables as predictup[1,1]/predictup[1,1]+predictup[1,2]
recallorigin <- tableorigin[1,1]/(tableorigin[1,1]+tableorigin[2,1])
precisionorigin <- tableorigin[1,1]/(tableorigin[1,1]+tableorigin[1,2])
f1origin <- 2/((1/precisionorigin)+(1/recallorigin))
precisionorigin
recallorigin
f1origin


tabledown <- table(predictdown, data_test2$winequal1)
tabledown
recalldown <- tabledown[1,1]/(tabledown[1,1]+tabledown[2,1])
precisiondown <- tabledown[1,1]/(tabledown[1,1]+tabledown[1,2])
f1down <- 2/((1/precisiondown)+(1/recalldown))
precisiondown
recalldown
f1down


###estimate precision and recalland f1 measure from tables
###precision = tP/tp+fp; recall=tp/tp+fn; f1=2/((1/precision)+(1/recall))

###get precision from the tables as predictup[1,1]/predictup[1,1]+predictup[2,1]
###get recall from the tables as predictup[1,1]/predictup[1,1]+predictup[1,2]
recallorigin <- tableorigin[1,1]/(tableorigin[1,1]+tableorigin[2,1])
precisionorigin <- tableorigin[1,1]/(tableorigin[1,1]+tableorigin[1,2])
f1origin <- 2/((1/precisionorigin)+(1/recallorigin))
precisionorigin
recallorigin
f1origin

confusionorigin <- confusionMatrix(tableorigin)
confusionorigin
getKappa <- as.data.frame(confusionorigin$overall)
Kappaorigin <- getKappa[2,]
Kappaorigin


gettableorigin <- as.data.frame(confusionorigin$table)
gettableorigin
true_goodorigin <- gettableorigin[1,3]
false_goodorigin <- gettableorigin[3,3]
false_poororigin <- gettableorigin[2,3]
true_poororigin <- gettableorigin[4,3]
misclassificationorigin <- (tableorigin[1,2]+tableorigin[2,1])/(tableorigin[1,1]+tableorigin[1,2]+tableorigin[2,1]+tableorigin[2,2])
misclassificationorigin
Kappa_results <- add_row(Kappa_results, method = "gbm_original", Kappa= Kappaorigin,
                         Recall=recallorigin, Precision = precisionorigin, F1 = f1origin, misclassification=misclassificationorigin, 
                         true_good=true_goodorigin, false_good=false_goodorigin,
                         false_poor=false_poororigin, true_poor=true_poororigin)

confusionwted <- confusionMatrix((table(predictwted,data_test2$winequal1))) 
tableweighted <-table(predictwted,data_test2$winequal1)
tableweighted
recallwtd <- tableweighted[1,1]/(tableweighted[1,1]+tableweighted[2,1])
precisionwtd <- tableweighted[1,1]/(tableweighted[1,1]+tableweighted[1,2])
f1wtd <- 2/((1/precisionwtd)+(1/recallwtd))
precisionwtd
recallwtd
f1wtd

getKappa <- as.data.frame(confusionwted$overall)
Kappawted <- getKappa[2,]
Kappawted

gettablewted <- as.data.frame(confusionwted$table)
gettablewted
true_goodwted <- gettablewted[1,3]
false_goodwted <- gettablewted[3,3]
false_poorwted <- gettablewted[2,3]
true_poorwted <- gettablewted[4,3]
misclassificationwtd <- (tableweighted[1,2]+tableweighted[2,1])/(tableweighted[1,1]+tableweighted[1,2]+tableweighted[2,1]+tableweighted[2,2])
misclassificationwtd
Kappa_results <- add_row(Kappa_results, method = "gbm_weighted", Kappa= Kappawted,
                         Recall=recallwtd, Precision = precisionwtd, F1 = f1wtd,misclassification=misclassificationwtd, 
                         true_good=true_goodwted, false_good=false_goodwted,
                         false_poor=false_poorwted, true_poor=true_poorwted)


confusionup <- confusionMatrix((table(predictup,data_test2$winequal1)))
confusionup
getKappa <- as.data.frame(confusionup$overall)
Kappaup <- getKappa[2,]
Kappaup

tableup <- table(predictup,data_test2$winequal1)
tableup
recallup <- tableup[1,1]/(tableup[1,1]+tableup[2,1])
precisionup <- tableup[1,1]/(tableup[1,1]+tableup[1,2])
f1up <- 2/((1/precisionup)+(1/recallup))
precisionup
recallup
f1up

gettableup <- as.data.frame(confusionup$table)
gettableup
true_goodup <- gettableup[1,3]
false_goodup <- gettableup[3,3]
false_poorup <- gettableup[2,3]
true_poorup <- gettableup[4,3]
misclassificationup <- (tableup[1,2]+tableup[2,1])/(tableup[1,1]+tableup[1,2]+tableup[2,1]+tableup[2,2])
misclassificationup
Kappa_results <- add_row(Kappa_results, method = "gbm_up", Kappa= Kappaup,
                         Recall=recallup, Precision = precisionup, F1 = f1up, misclassification=misclassificationup, 
                         true_good=true_goodup, false_good=false_goodup,
                         false_poor=false_poorup, true_poor=true_poorup)




confusiondown <- confusionMatrix((table(predictdown, data_test2$winequal1)))
confusiondown
getKappa <- as.data.frame(confusiondown$overall)
Kappadown <- getKappa[2,]
Kappadown

tabledown <- table(predictdown, data_test2$winequal1)
tabledown
recalldown <- tabledown[1,1]/(tabledown[1,1]+tabledown[2,1])
precisiondown <- tabledown[1,1]/(tabledown[1,1]+tabledown[1,2])
f1down <- 2/((1/precisiondown)+(1/recalldown))
precisiondown
recalldown
f1down
misclassificationdown <- (tabledown[1,2]+tabledown[2,1])/(tabledown[1,1]+tabledown[1,2]+tabledown[2,1]+tabledown[2,2])
misclassificationdown
gettabledown <- as.data.frame(confusiondown$table)
gettabledown
true_gooddown <- gettabledown[1,3]
false_gooddown <- gettabledown[3,3]
false_poordown <- gettabledown[2,3]
true_poordown <- gettabledown[4,3]

Kappa_results <- add_row(Kappa_results, method = "gbm_down", Kappa= Kappadown,
                         Recall=recalldown, Precision = precisiondown, F1 = f1down, misclassification=misclassificationdown,
                         true_good=true_gooddown, false_good=false_gooddown,
                         false_poor=false_poordown, true_poor=true_poordown)

Table_results <- as.data.frame(Kappa_results)
Table_results<- format(Table_results, digits=2)


Variable_importance_models 

portionsofwine <-table(data_test2$winequal1)
portionsofwine

library(FactoMineR)
library(factoextra)

##keep# Plot grouped quality data set####group good versus group poor
tendency2 <- fviz_pca_ind(prcomp(clustdf), title = "PCA - wine data", 
                          habillage = winequalitystandardized$quality,  palette = "jco",
                          geom = "point", ggtheme = theme_classic(),
                          legend = "bottom")
tendency2





