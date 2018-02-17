# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

##########################################################################################

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
train.rawdata<- read.csv("../input/train.csv")

#data pre-processing
#---------------------------
#---------------------------

#Dealing with missing values
#---------------------------
train.rawdata[train.rawdata==-1]<-NA

#count the missing values in each column
sapply(train.rawdata, function(x) sum(is.na(x)))

#ps_car_03_cat contains 411231(almost 90%) missing values, so this column is removed
train.rawdata=subset(train.rawdata, select=-c(ps_car_03_cat))
#ps_car_05_cat contains 266551 (44%) missing values, so this column is removed
train.rawdata=subset(train.rawdata, select=-c(ps_car_05_cat))

#change the datatype according to problem statement
train1.rawdata=train.rawdata
str(train.rawdata)
train.rawdata$target=as.factor(train.rawdata$target)
train.rawdata$ps_ind_02_cat=as.factor(train.rawdata$ps_ind_02_cat)
train.rawdata$ps_ind_04_cat=as.factor(train.rawdata$ps_ind_04_cat)
train.rawdata$ps_ind_05_cat=as.factor(train.rawdata$ps_ind_05_cat)
train.rawdata$ps_car_01_cat=as.factor(train.rawdata$ps_car_01_cat)
train.rawdata$ps_car_02_cat=as.factor(train.rawdata$ps_car_02_cat)
train.rawdata$ps_car_04_cat=as.factor(train.rawdata$ps_car_04_cat)
train.rawdata$ps_car_06_cat=as.factor(train.rawdata$ps_car_06_cat)
train.rawdata$ps_car_07_cat=as.factor(train.rawdata$ps_car_07_cat)
train.rawdata$ps_car_08_cat=as.factor(train.rawdata$ps_car_08_cat)
train.rawdata$ps_car_09_cat=as.factor(train.rawdata$ps_car_09_cat)
train.rawdata$ps_car_10_cat=as.factor(train.rawdata$ps_car_10_cat)
train.rawdata$ps_car_11_cat=as.factor(train.rawdata$ps_car_11_cat)

library(plyr)
library(dplyr)

#Imputing Missing values
#-----------------------
#Median
train.rawdata$ps_reg_03[is.na(train.rawdata$ps_reg_03)]=median(train.rawdata$ps_reg_03,na.rm=TRUE)
train.rawdata$ps_car_14[is.na(train.rawdata$ps_car_14)]=median(train.rawdata$ps_car_14,na.rm=TRUE)

#Mode - imputing the categorical variables
library(Hmisc)
train.rawdata$ps_ind_02_cat=impute(train.rawdata$ps_ind_02_cat, mode)
train.rawdata$ps_ind_04_cat=impute(train.rawdata$ps_ind_04_cat, mode)
train.rawdata$ps_ind_05_cat=impute(train.rawdata$ps_ind_05_cat, mode)
train.rawdata$ps_car_01_cat=impute(train.rawdata$ps_car_01_cat, mode)
train.rawdata$ps_car_07_cat=impute(train.rawdata$ps_car_07_cat, mode)
train.rawdata$ps_car_09_cat=impute(train.rawdata$ps_car_09_cat, mode)

#Removing the rows containing negligible missing values w.r.t column
train.rawdata=train.rawdata[complete.cases(train.rawdata),]



#visualization of feature
library(ggplot2)
library(Rmisc)
p1 <- train.rawdata %>%
  ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train.rawdata %>%
  ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train.rawdata %>%
  ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train.rawdata %>%
  ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train.rawdata %>%
  ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train.rawdata %>%
  ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train.rawdata %>%
  ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p8 <- train.rawdata %>%
  ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout=layout)


#separate target data
train=subset(train.rawdata, select=-c(target))
train1=subset(train.rawdata, select=c(id,target))

str(train)
library(dummies)
new_my_data <- dummy.data.frame(train, names = c("ps_ind_02_cat","ps_ind_04_cat",
                                "ps_ind_05_cat","ps_car_01_cat",
                                "ps_car_02_cat","ps_car_04_cat","ps_car_06_cat","ps_car_07_cat","ps_car_08_cat","ps_car_09_cat","ps_car_10_cat","ps_car_11_cat"))
      
#correlation plot
library(magrittr)
library(corrplot)

train1.rawdata %>%
    select(ps_ind_12_bin, ps_ind_14, ps_ind_16_bin, ps_ind_17_bin, ps_ind_18_bin, ps_reg_02,
         ps_reg_03, ps_car_12, ps_car_13, ps_car_14, ps_car_15, ps_car_02_cat, ps_car_04_cat) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE, method = "number")

#Data Sample
#--------------------------
#stratified sampling
set.seed(32984)
library(caret)
indexes <- createDataPartition(train.rawdata$target, times = 1,
                               p = 0.7, list = FALSE)
                               
pca.train <- new_my_data[indexes,]
pca.train1 <- train1[indexes,]
pca.test <- new_my_data[-indexes,]
test1 <- train1[-indexes,]

#Feature Engineering
#----------------------------
#----------------------------
#principal component analysis
prin_comp <- prcomp(pca.train, scale. = T)
prin_comp$rotation[1:5,1:4]
dim(prin_comp$x)

biplot(prin_comp, scale = 0)

std_dev <- prin_comp$sdev

pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)


#understanding the variance by plots
plot(prop_varex, xlab = "Principal Component",
            ylab = "Proportion of Variance Explained",
            type = "b")

#cumulative plot            
plot(cumsum(prop_varex), xlab = "Principal Component",
            ylab = "Cumulative Proportion of Variance Explained",
            type = "b")

#Model building
#---------------------------
#---------------------------
train.data <- data.frame(target = pca.train1$target, prin_comp$x)
str(train.data)
train.data <- train.data[,1:40]

#logistic regression
logistic <- glm(target~.,family=binomial(link='logit'),data=train.data)
summary(logistic)

#test data preparation
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:40]

#Predict with test data
predicted=predict(logistic,test.data,type='response')


library(gplots)
#Model performance
#------------------------
#------------------------
#Accuracy calculation
results <- ifelse(predicted > 0.07,1,0)
misClasificError <- mean(results != test1$target)
print(paste('Accuracy',1-misClasificError))

#Another approach
#Finding the area under curve using true positive and false positive rate
library(ROCR)
pr <- prediction(results, test1$target)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

library(InformationValue)
optCutOff = optimalCutoff(test1$target,predicted)[1]
optCutOff = 0.07
confMtx=confusionMatrix(test1$target, predicted, threshold = optCutOff)
print("Specificity")
print(specificity(test1$target, predicted, threshold = optCutOff))
