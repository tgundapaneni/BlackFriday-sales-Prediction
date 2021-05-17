#install.packages("ISLR")
library(ISLR)
#install.packages("magrittr")
library(magrittr)
#install.packages("yrdpl")
library(dplyr)
#install.packages("rmarkdown")
library(rmarkdown)
#install.packages("dslabs")
library(ggplot2)
library(scales)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("data.table", type="source", dependencies=TRUE)
library(data.table)
#install.packages("corrplot")
library(corrplot)
library(funModeling) 
library(tidyverse)
library(Hmisc)
#install.packages("caTools")
library(caTools)
#install.packages("Caret")
library(caret)
library(rpart)
library(glmnet)
library(e1071)

blackfriday <- as.data.frame(read.csv("train.csv"))
str(blackfriday)
# data set is cleaned from the original data set which contained 3 additional variables - Product_Category_2 and Product_Category_3. 
# These three variables are excluded from this analysis because Product_Category_2 and Product_Category_3 contain missing observations. 
# Excluding Product_Category_2 and Product_Category_3 will not affect the results we have drawn. 
blackfriday_clean = blackfriday %>% select(User_ID, Product_ID, Gender, Age, City_Category, Occupation, Stay_In_Current_City_Years, Marital_Status, Product_Category_1, Purchase)
# TransformDataTypes - Changing Product_ID, Gender, Age, City_Category, Marital_Status and Product_Category from character variables to factors. 
blackfriday_clean$`Product_ID`=factor(blackfriday_clean$Product_ID)
blackfriday_clean$Gender = factor(blackfriday_clean$Gender)
blackfriday_clean$Age = factor(blackfriday_clean$Age)
blackfriday_clean$Occupation = factor(blackfriday_clean$Occupation)
blackfriday_clean$Stay_In_Current_City_Years = factor(blackfriday_clean$Stay_In_Current_City_Years)
blackfriday_clean$City_Category = factor(blackfriday_clean$City_Category)
blackfriday_clean$Marital_Status = factor(blackfriday_clean$Marital_Status)
blackfriday_clean$Product_Category_1 = factor(blackfriday_clean$Product_Category_1)
names(blackfriday_clean)[9] <- 'Product_Category'
str(blackfriday_clean)

#correlation Matrix
corr_blackfriday <- blackfriday_clean
corr_blackfriday <- subset(corr_blackfriday, select = -c(Stay_In_Current_City_Years))
corr_blackfriday$User_ID <- as.numeric(blackfriday_clean$User_ID)
corr_blackfriday$Product_ID <- as.numeric(blackfriday_clean$Product_ID)
corr_blackfriday$Gender <- as.numeric(ifelse(blackfriday_clean$Gender=="M", 1, 0))
corr_blackfriday$Age <- as.numeric(ifelse(blackfriday_clean$Age=='0-17', 17, ifelse(blackfriday_clean$Age=='18-25', 25, ifelse(blackfriday_clean$Age=='26-35', 35, ifelse(blackfriday_clean$Age=='36-45', 45, ifelse(blackfriday_clean$Age=='46-50', 50, ifelse(blackfriday_clean$Age=='51-55', 55, 65)))))))
corr_blackfriday$Marital_Status <- as.numeric(blackfriday_clean$Marital_Status)
corr_blackfriday$Occupation <- as.numeric(blackfriday_clean$Occupation)
corr_blackfriday$City_Category <- as.numeric(ifelse(blackfriday_clean$City_Category=='A', 1, ifelse(blackfriday_clean$City_Category=='B', 2, 3)))
corr_blackfriday$Product_Category <- as.numeric(blackfriday_clean$Product_Category)
corr_blackfriday$Purchase <- as.numeric(blackfriday_clean$Purchase)

#cor(corr_blackfriday)
corrplot(cor(corr_blackfriday), method="number")

# Dtls By user_ID
df1 = blackfriday_clean %>% group_by(User_ID) %>% summarise(`Shopper Purchase` = sum(Purchase),`Number of Products Sold` = n())
blackfriday_user = select(blackfriday_clean, User_ID, Gender, Age, City_Category, Marital_Status, Occupation, Stay_In_Current_City_Years)
blackfriday_user = unique(blackfriday_user)
blackfriday_user = blackfriday_user %>% left_join(df1, by = 'User_ID')
# User_ID by Product Category: to see the rev. for each category
df2 = blackfriday_clean %>% group_by(User_ID,Product_Category) %>% summarise(`Shopper Purchase` = sum(Purchase),`Number of Products Sold` = n())
User_Category = select(blackfriday_clean, User_ID, Gender, Age, City_Category, Marital_Status, Occupation, Stay_In_Current_City_Years)
User_Category = unique(User_Category)
User_Category = User_Category %>% left_join(df2, by = 'User_ID')
# distinct Users
bf_dis_user = n_distinct(blackfriday_clean$User_ID)
# Distinct products
bf_dis_product = n_distinct(blackfriday$Product_ID)
# Total BlackFriday Sale
total_purchase = sum(blackfriday$Purchase)
# No oF MALE AND FEMALE
gender=as.data.frame(blackfriday_clean %>% group_by(Gender) %>% summarise(`Number of Distinct shoppers` = n_distinct(`User_ID`)))
#gender
knitr::kable(as.data.table(gender), caption = "Table 1:  Gender")
# Summarize by City - To focus which City generates more revenue and also avg purchase per shopper
avg_district = as.data.frame(blackfriday_clean %>% 
                               group_by(City_Category) %>% 
                               summarise(`Product Revenue` = sum(Purchase), 
                                         `Number of Distinct shoppers` = n_distinct(`User_ID`), 
                                         `Avg. Purchase per Shopper` = `Product Revenue`/`Number of Distinct shoppers`, 
                                         `Number of Product` = n(),  
                                         `Avg. Unit Price per Product` = `Product Revenue`/`Number of Product`,
                                         `Product per Shopper` = round(`Number of Product`/`Number of Distinct shoppers`,0)))
knitr::kable(as.data.table(avg_district), caption = "Table 2: Summary by Each City")


ggplot(blackfriday_clean, aes(Gender,Product_Category,fill=Gender))+geom_col(width=0.4)+facet_wrap(~Age)+labs(title ="Age Group/Gender Vs Product Category")


blank_theme <- theme_minimal()+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),panel.border = element_blank(),panel.grid=element_blank(),axis.ticks = element_blank(),plot.title=element_text(size=14, face="bold"))

p1 = blackfriday_clean %>% filter(City_Category == 'A') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/1045)) %>% 
  ggplot(aes(x="", y=n, fill=Age)) + 
  geom_bar(width = 1, stat = 'identity', position= "stack") + 
  coord_polar("y") +
  geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+theme_void()+ theme(plot.title = element_text(size=14, face="bold"),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank(),axis.title=element_blank(), legend.title = NULL,legend.position="bottom") +labs(tag = 'Figure 1\n') +labs(subtitle = "City A")

p2= blackfriday_clean %>% filter(City_Category == 'B') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/1707)) %>% ggplot(aes(x="", y=n, fill=Age)) + geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+theme_void()+ theme(plot.title = element_text(size=14, face="bold"),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid  = element_blank(),axis.title=element_blank(),legend.position="bottom") +labs(title = 'Age Distribution for Each City',subtitle = "City B")

p3=blackfriday_clean %>% filter(City_Category == 'C') %>% group_by(City_Category,Age) %>% summarise(n = n_distinct(User_ID)) %>%mutate(prop = percent(n/3139)) %>% 
  ggplot(aes(x="", y=n, fill=Age)) + 
  geom_bar(width = 1, stat = 'identity', position= "stack") + coord_polar("y") +
  geom_text(aes(label = prop), position = position_stack(vjust = 0.8), size = 3)+ theme_void() +
  theme(plot.title = element_text(size=14, face="bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(), 
        axis.title=element_blank(), legend.position="bottom")  +
  labs(subtitle = "City C")

ggarrange(p1,p2,p3, ncol = 3, nrow = 1, common.legend = TRUE, legend = 'bottom', align = 'v', widths = c(1, 1, 1))

unit.price = as.data.frame(User_Category %>% group_by(Product_Category) %>% summarise(`Product Revenue` = sum(`Shopper Purchase`), `Products Sold` = sum(`Number of Products Sold`), `Unit Price ($)` = round(`Product Revenue`/`Products Sold`, 2)))

g1 = ggplot(unit.price, aes(x = Product_Category, y = `Unit Price ($)`, fill = `Unit Price ($)` )) + geom_col() + scale_fill_gradient(high="#cb4b16",low="#b58900") + labs(title = "Comparing Luxury vs. Normal \nProduct Categories", tag = 'Figure 2\n', x = 'Product Category', y = 'Unit Price ($)')+ annotate("text", x=10, y=21000, label= "$19680", fontface = "bold",size = 4)+ annotate("text", x=13, y=1500, label= "$723", fontface = "bold",size = 4) + labs(x = NULL, subtitle = 'City A')

g2 = ggplot(unit.price, aes(x = Product_Category, y = `Product Revenue`/100000, fill = `Product Revenue`/100000)) + geom_col() + scale_fill_gradient(high="#cb4b16",low="#b58900")+ annotate("text", x=1, y=20000, label= "$18827m", fontface = "bold",size = 4)+ annotate("text", x=13, y=1000, label= "$39m", fontface = "bold",size = 4) +labs(x = 'Product Category',y = 'Product Revenue \n($ Million)', fill = 'Product Revenue \n($ Million)', subtitle = 'City B') 

g3 = ggplot(unit.price, aes(x = Product_Category, y = `Products Sold`/1000, fill = `Products Sold`/1000)) + geom_col() + scale_fill_gradient(high="#cb4b16",low="#b58900")+ annotate("text", x=5, y=160, label= "149k", fontface = "bold",size = 4)+ annotate("text", x=9, y=10, label= "0.4k", fontface = "bold",size = 4) +labs(x = NULL, y = 'Products Sold \n(Thousands)', fill = 'Products Sold \n(Thousands)')

ggarrange(g1,g3,g2, widths = 1:3, nrow = 3, align = 'v', heights=c(1.5,1,1)) 

# The table below and the graph helps understand average shopper's purchasing habits by product category
knitr::kable(as.data.table(unit.price), caption = "Table 3:  Product Price")

# Looking at the Graph we can Hypothesise the below statement -
blackfriday_user %>% group_by(City_Category, Age, Gender) %>% summarise(n=n(), purchase = sum(`Shopper Purchase`) ,avg = sum(`Shopper Purchase`)/n) %>% 
  ggplot(aes(x =Age, y = avg/100000, fill=factor(Gender))) + 
  geom_col(position="stack") + facet_grid(~City_Category) + 
  labs(title = "Average Purchase Comparison  \nby Age and City", tag = 'Figure 3\n', x="City Category", y= "Purchase \n($ Million)", fill = "Gender") + 
  theme(plot.title = element_text(size=14, face="bold"), axis.text.x = element_text(angle=60, hjust=1), axis.title.x = element_text(size=12, face="bold"), axis.title.y = element_text(size=12, face="bold"), legend.position="bottom") + scale_fill_brewer(palette="Dark2")

# Hypothesis : People living in City A spend more than their peers in City B and C. 
# The variables Shopper Purchase, City_Category, Gender and Age are selected to conduct a two-sample t-test. 
# Based on the categorical nature of City_Category, Gender and Age (measured as factor levels of age ranges), 
# the two-sample t-test was chosen. 

city_a = blackfriday_user %>% filter(City_Category =='A') %>% select(`Shopper Purchase`)
city_b = blackfriday_user %>% filter(City_Category =='B') %>% select(`Shopper Purchase`)
city_c = blackfriday_user %>% filter(City_Category =='C') %>% select(`Shopper Purchase`)

# Alternate hypothesis - People in City A spend more than people in City B

t.test(city_a, city_b, alternative = 'greater') 
t.test(city_a, city_c, alternative = 'greater') 
t.test(city_b, city_c, alternative = 'greater')

# The results yield that 
# shoppers in city A did not spend significantly more than their people in city B 
# Shoppers in City A did spend more than people in City C
# Shoppers in city B spent significantly more than their peers in city C.

# Therefore, the shopping habits of customers in city A are significantly different from the remaining cities. 
# The results also indicate shoppers in city C spend significantly less than cities A and B. 
# Based on this point, the stores can focus more on learning the spending habits in city C to 
# further understand factors that may lead to the low purchases in city C. 
# For example, is it because that the income level (Occupation), 
# that shoppers in city C are new to the city and are not familiar with local stores (Stay_In_Current_City_Years) 
# or that the products sold (Product_Category) do not meet the needs of shoppers in city C?

###################Backward Selection Process ##########################

step(lm(Purchase ~ . - User_ID - Product_ID, data = blackfriday_clean),direction = "backward",method = "lmStepAIC")

############################################################################
set.seed(101)
sample <- sample.split(blackfriday_clean, SplitRatio = .70)
train <- subset(blackfriday_clean, sample == T)
test <- subset(blackfriday_clean, sample == F)
head(train)
head(test)

#####################MUltiple Linear Regression############################

model<- lm(Purchase ~Age+Gender+Marital_Status+City_Category+Product_Category+Occupation, data=train)
summary(model)


############Evaluation for Multiple Linear Model##########################

linear_train_forecasts <- round(predict(model, test[,-10]))
results <- cbind(linear_train_forecasts, test$Purchase)
colnames(results) <- c('pred','real')
head(results)

#RMSE and R-square for Train dataset
Linear_RMSE_Train<-sqrt(mean(model$residuals^2))
Linear_rsq_train<- summary(model)$r.squared

#RMSE and R-square using caret library

linear_RMSE <- RMSE(linear_train_forecasts,test$Purchase)
linear_Rsq <-  R2(linear_train_forecasts,test$Purchase)

##########################Ridge Regression##################################

#create matrix for training set and test set
set.seed(101)
train.mat<-model.matrix(Purchase~ Age+Gender+Marital_Status+City_Category+Occupation+Product_Category,data=train)
test.mat<-model.matrix(Purchase~ Age+Gender+Marital_Status+City_Category+Occupation+Product_Category,data=test)

#defining grid to covering all the range of lambda.
#This will be used to find best value of lambda


grid<-10^seq(10,-2, length=100)

#fitting the ridge regression model

ridge<-glmnet(train.mat,train$Purchase,alpha=0,lambda=grid,thresh = 1e-12)

#doing cross validation on model

cv.ridge<-cv.glmnet(train.mat,train$Purchase,alpha=0,lambda=grid,thresh=1e-12)
plot(cv.ridge)
#finding the lambda for which cv error is minimum on training data

choosing_S<-cv.ridge$glmnet.fit
head(choosing_S)

bestlam.ridge<- cv.ridge$lambda.min

#Predicting the value using the bestlambda
pred.ridge<-predict(ridge,s=bestlam.ridge,newx =test.mat)
pred.ridge_train<- predict(ridge,s=bestlam.ridge,newx = train.mat)


#############Evaluation for Ridge model###########################

#RMSE and R-square for train
Ridge_RMSE_train <- RMSE(train$Purchase,pred.ridge_train)
Ridge_Rsq_train <-  R2(pred.ridge_train,train$Purchase)

#RMSE from caret package & R-square for test
Ridge_RMSE <- RMSE(test$Purchase,pred.ridge)
Ridge_Rsq <-  R2(pred.ridge,test$Purchase)


###########################################################################

#Lasso Model

lasso<-glmnet(train.mat,train$Purchase,alpha=1,lambda=grid,thresh = 1e-12)

#doing cross validation on model to find the best s value

cv.lasso<-cv.glmnet(train.mat,train$Purchase,alpha=1,lambda=grid,thresh=1e-12)
plot(cv.lasso)

#finding the lambda for which cv error is minimum on training data
chooing_lasso_s<-cv.lasso$glmnet.fit
head(chooing_lasso_s)

bestlam.lasso<- cv.lasso$lambda.min

#using the lambda value obtained from cross validation for the lasso model directly on test data set to get the predicted values

pred.lasso_train<-predict(lasso,s=bestlam.lasso,newx =train.mat)
pred.newlasso<-predict(lasso,s=bestlam.lasso,newx =test.mat)

###################Evaluation for Lasso Model########################

#RMSE and R-square for train
Lasso_RMSE_train <- RMSE(train$Purchase,pred.lasso_train)
Lasso_Rsq_train <-  R2(pred.lasso_train,train$Purchase)

#RMSE and R-square from caret package
Lasso_RMSE <- RMSE(pred.newlasso,test$Purchase)
Lasso_Rsq <-  R2(pred.newlasso,test$Purchase)

#########################Results for Linear, Ridge and Lasso##############################

RMSE_Total_Train<- cbind(Linear_RMSE_Train,Ridge_RMSE_train,Lasso_RMSE_train)
RMSE_Total_Test<- cbind(linear_RMSE,Ridge_RMSE,Lasso_RMSE)
Rsquare_Test<- cbind(linear_Rsq,Ridge_Rsq,Lasso_Rsq)
Rsquare_Train<- cbind(Linear_rsq_train,Ridge_Rsq_train,Lasso_Rsq_train)
colnames(RMSE_Total_Test) <- c('Linear_Model','Ridge_Model','Lasso_Model')
colnames(RMSE_Total_Train) <- c('Linear_Model','Ridge_Model','Lasso_Model')
colnames(Rsquare_Test) <- c('Linear_Model','Ridge_Model','Lasso_Model')
colnames(Rsquare_Train) <- c('Linear_Model','Ridge_Model','Lasso_Model')
RMSE_Total_Train
Rsquare_Train
RMSE_Total_Test
Rsquare_Test

############################## Decision Tree####################################


val_actual<-train$Purchase
head(val_actual)
length(val_actual)
model_dt <- rpart(Purchase ~ Gender+Age+Occupation+City_Category+Marital_Status+Product_Category, data = train)
summary(model_dt)
printcp(model_dt)
plotcp(model_dt)

#plotting the tree
#plot(model_dt)
#text(model_dt, pretty = 1)

#pruning the tree
model_dt_prune<- prune(model_dt, cp=0.01)

# make predictions
pred_dt <- predict(model_dt_prune, train)
length(pred_dt)
rmse_dt <- sqrt(mean((pred_dt - val_actual)^2))
rmse_dt

#predicting on the test dataset 

val_actual_test <- test$Purchase
pred_test_dt <- predict(model_dt, test)
test_sub_dt<- data.frame(test$User_ID,test$Product_ID,pred_test_dt)#test dataset to be kept here
head(test_sub_dt)
rmse_dt_test <- sqrt(mean((pred_test_dt - val_actual_test)^2))
rmse_dt_test


################################################## Akash's Code ###########################################################


black_f<-subset(blackfriday,User_ID<1000512)
str(black_f)
black_f$`Product_ID`=factor(black_f$Product_ID)
df<-(black_f %>% select(User_ID,Product_ID))
df$User_ID<-as.factor(df$User_ID)
df$User_ID
df$Product_ID
coc_mat <- matrix(nrow = length(levels(df$User_ID)),ncol = length(levels(df$Product_ID)))
colnames(coc_mat) <-  levels(df$Product_ID)
rownames(coc_mat) <- levels(df$User_ID)

for(i in 1:length(levels(df$Product_ID))){
  for(j in 1:length(levels(df$User_ID))){
    coc_mat[j,i] <- length(which(df$User_ID==levels(df$User_ID)[j] &df$Product_ID==levels(df$Product_ID)[i]))
    
  }
}

coc_mat[1,78]
coc_mat

#coc_mat<- as.matrix(coc_mat[,-1])
install.packages("recommenderlab")
library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
coc_mat<- as(coc_mat, "realRatingMatrix")

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(coc_mat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four products
similarity_products <- similarity(coc_mat[, 1:4], method =
                                    "cosine", which = "items")
as.matrix(similarity_products)
image(as.matrix(similarity_products), main = "Item similarity")

####### recommendation using popularity matrix ####

#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering

recommender_model <- Recommender(coc_mat, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
names(getModel(recommender_model))
m=model_details$nn

recom <- predict(recommender_model, 
                 coc_mat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom@items

#recc_matrix <- sapply(recom@items, 
#                      function(x){ colnames(ratingmat)[x] })
#dim(recc_matrix)

recom_list <- as(recom, "list") #convert recommenderlab object to readable list
recom_list

# Evaluation:
# creation of recommender model based on ubcf
e1 <- evaluationScheme(coc_mat, method="split", train=0.7, given=30)
Rec.ubcf <- Recommender(getData(e1, "train"), "UBCF")
p.ubcf <- predict(Rec.ubcf, getData(e1, "known"), type="ratings")
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e1, "unknown"))

