# Big Sales Mart - EDA
# meaning full patterns of the data  and prepare it for the ML algo
# the first thing is to create the hypothesis
# target is to increase the sales
# factors that effect the sales
# i.e MRP , store location , area, customer service
# after constructing the hypothesis we will extract variables according to these hypothesis 
# numerical data - we use statistical summary/ histogram
# categorical data - tables/ mode/ create bar plot

library(ggplot2)
library(dplyr)
library(cowplot)

train <- read.csv(file.choose(),header = T,sep = ",",stringsAsFactors = T,na.strings = "")
test <- read.csv(file.choose(),header = T,sep = ",",stringsAsFactors = T,na.strings = "")
str(train)
# combine the the data sets
colnames(train)
test$Item_Outlet_Sales <- test$Item_Outlet_Sales <- NA

combine <- rbind(train,test)

# we do this to find the pattern and therefore we use test as well to find the pattern

# Univariate Analysis ####
# Numeric Variables 
# Item_Outlet_Sales
summary(train$Item_Outlet_Sales)
p1 <- ggplot(train,aes(Item_Outlet_Sales))+geom_histogram(binwidth = 250,color="white")
# it is highly skewed 

# Item_Visibility
summary(train$Item_Visibility)
p2 <- ggplot(train,aes(Item_Visibility))+geom_histogram(binwidth = 0.01,color="white",fill="blue")
# positively skewed

# Item_Weight
summary(train$Item_Weight)
p3 <- ggplot(train,aes(Item_Weight))+geom_histogram(binwidth = 0.5,color="white",fill="red")
# No pattern

# Item_MRP
summary(train$Item_MRP)
p4 <- ggplot(train,aes(Item_MRP))+geom_histogram(binwidth = 1,fill="blue")
# 4 different distributions
X <- plot_grid(p1,p2,p3,nrow = 1)
plot_grid(X,p4,nrow = 2)

# Inference
# 1) Item visibility is skewed in nature and lot of products have zero visibility,
# we will address this in feature engineering
# 2) Item weight is not showing pattern in terms of distributin
# 3) we see interesting 4 distribution in MRP

# Categorical Var- Bar plot
# Item_Type

c1 <- combine %>% group_by(Item_Type) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Item_Type,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Item_Type,count,label=count))

# Item_Fat_Content

combine$Item_Fat_Content[combine$Item_Fat_Content=="LF"] <- "Low Fat"
combine$Item_Fat_Content[combine$Item_Fat_Content=="low fat"] <- "Low Fat"
combine$Item_Fat_Content[combine$Item_Fat_Content=="reg"] <- "Regular"

c2 <- combine %>% group_by(Item_Fat_Content) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Item_Fat_Content,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Item_Fat_Content,count,label=count))

# Outlet_Identifier

c3 <- combine %>% group_by(Outlet_Identifier) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Outlet_Identifier,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Outlet_Identifier,count,label=count))

# Outlet_Type

c4 <- combine %>% group_by(Outlet_Type) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Outlet_Type,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Outlet_Type,count,label=count))

# Outlet_Size

c5 <- combine %>% group_by(Outlet_Size) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Outlet_Size,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Outlet_Size,count,label=count))

# Outlet_Establishment_Year

c6 <- combine %>% group_by(Outlet_Establishment_Year) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(Outlet_Establishment_Year,count))+geom_bar(stat = "identity",fill="coral")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 10))+
  geom_label(aes(Outlet_Establishment_Year,count,label=count))


plot_grid(c2,c4,c5,nrow = 1)
plot_grid(c1,c3,c6,ncol = 1)

# Bivariate Analysis ####
# here we will explore the independent var with respect to the target variable
# we will make scatter plots for the numerical var and violin for the categorical var

# For numeric variables
# Item_Weight vs Sales

ggplot(train,aes(Item_Weight,Item_Outlet_Sales))+
  geom_point(color="hotpink")

# item visibility vs sales

ggplot(train,aes(Item_Visibility,Item_Outlet_Sales))+
  geom_point(color="hotpink")

# item MRP vs sales

ggplot(train,aes(Item_MRP,Item_Outlet_Sales))+
  geom_point(color="hotpink")

# For categorical var
# item_type vs sale

ggplot(combine,aes(Item_Type,Item_Outlet_Sales))+
  geom_violin(fill="hotpink")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# item fat vs sale

ggplot(combine,aes(Item_Fat_Content,Item_Outlet_Sales))+
  geom_violin(fill="hotpink")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# item identifer vs sales

ggplot(combine,aes(Outlet_Identifier,Item_Outlet_Sales))+
  geom_violin(fill="hotpink")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Missing Value ####
# there is a pattern in missing value i.e it is dependent on the item identifier

mean(combine[combine$Item_Identifier=="FDP10","Item_Weight"],na.rm = T)

# loop...

miss.data <- which(is.na(combine$Item_Weight))

for (i in miss.data) {
  itemscroller <- combine$Item_Identifier[i]
 combine$Item_Weight[i] <- mean(combine[combine$Item_Identifier==itemscroller,"Item_Weight"],na.rm = T)
}

sum(is.na(combine$Item_Weight))

sapply(combine, function(x) sum(is.na(x))) # no missing value

# Outlet_Size
library(mlr)
str(combine$Outlet_Size)
impute.data <- impute(combine,classes = list(factor=imputeMode()))
combine <- impute.data$data

# Feature Engineering ####
# lets convert the items type to perishable and non-perishable
names(table(combine$Item_Type))

perishable <- c("Breads","Breakfast","Dairy","Fruits and Vegetables","Meat","Seafood")
non_perishable <- c("Baking Goods","Canned","Frozen Foods","Hard Drinks","Health and Hygiene","Household",
                    "Others","Snack Foods","Soft Drinks","Starchy Foods")

# creating a new var - Item_Type_New
combine$Item_Type_New <- ifelse(combine$Item_Type %in% perishable,"perishable","non-perishable")

# another col cateogry
substr(combine$Item_Identifier,1,2)

# now if we want to check basis the item type
table(combine$Item_Type,substr(combine$Item_Identifier,1,2))

# basis this table we will create categories
combine$Item_Category <- substr(combine$Item_Identifier,1,2)

# now these things will increase the pattern in the data which means more robust ML model

# another var - priceperunit

combine$Price_Per_Unit <- combine$Item_MRP/combine$Item_Weight

# scaling and encoding
# most of the ML algorithm work better with numerical data and hence it is essential to convert the category variable
# to numeric data. this can be done in two ways

# OHE - One Hot Encoding
# lets learn this by an example
id <- factor(1:10)
height <- round(175+rnorm(10)*10)
nationality <-  c("AUS","UK","NZ","NZ","AUS","UK","NZ","UK","NZ","NZ")
df <- data.frame(id,height,nationality)
df

for (i in unique(df$nationality)){
  df[paste("nationality",i,sep = ".")] <- ifelse(df$nationality==i,1,0)
}
df





combine.backup <- combine

library(caret)
?dummyVars

dmy <- dummyVars( ~ Item_Identifier,data = combine.backup,fullRank = T)
dmy1 <- dummyVars(" ~ Item_Identifier",data = combine.backup,fullRank = T)
dmy
trsf <- data.frame(predict(dmy,newdata = combine.backup))
View(trsf)



customers <- data.frame(
  id=c(10,20,30,40,50),
  gender=c('male','female','female','male','female'),
  mood=c('happy','sad','happy','sad','happy'),
  outcome=c(1,1,0,0,0))
customers

dmy <- dummyVars(" ~ .", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)
