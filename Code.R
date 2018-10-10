db=read.csv("F:\\Aegis\\Machine Learning\\Topic 2\\MART_data.csv")
View(db)
head(db)
str(db)
summary(db)

## cleaning the column for fat content
summary(db$Item_Fat_Content)
i<-1
for(i in 1:nrow(db))
{
  if(db$Item_Fat_Content[i]=="reg"){
    db$Item_Fat_Content[i]="Regular"
  }
if(db$Item_Fat_Content[i]=="LF" || db$Item_Fat_Content[i]=="low fat"){
  db$Item_Fat_Content[i] = "Low Fat"
}
}
summary(db)
summary(db$Item_Fat_Content) 
levels(db$Item_Fat_Content) 

## cleaning the data for the Item_Weight
n=sum(is.na(db$Item_Weight==T))
db1<-db[is.na(db$Item_Weight)==F,]   # data without NA weights
db2<-db[is.na(db$Item_Weight==T),]   # data with NA weights

i<-1
for(i in 1:n)
{
  t=db2[i,"Item_Identifier"]
  db2[i,"Item_Weight"]<-db1[db1$Item_Identifier== t,"Item_Weight"][1]
  db[row.names(db2[i,]),"Item_Weight"]<-db2[i,"Item_Weight"] ## saving the values back to 
                                                ## the main data-frame using the rownames
  
}

## Running for loop to replace the NA values of the weight with the non-NA values,
## as the same item identifiers has the same weights.

View(db)
summary(db)

## To impute the remaining 37 NA values for the weight column
db1<-db[is.na(db$Item_Weight)==F,]   # data without NA weights
db2<-db[is.na(db$Item_Weight==T),]   # data with NA weights

## Predicting the model for replacing the NA values for the Item Weight
model<-lm(Item_Weight~Item_Fat_Content+Item_Type+Outlet_Location_Type,data=db1)
db2[,"Item_Weight"]=predict(model,db2)
View(db2)
i<-1
for(i in 1:nrow(db2)){
  db[row.names(db2[i,]),"Item_Weight"]<-db2[i,"Item_Weight"]
}
View(db)

## Now our data is cleaned and ready to do linear regression

## Part a)
## Linear Regression on the entire data
model1<-lm(Item_Outlet_Sales~Item_Visibility+Item_Type+Item_MRP+Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+Outlet_Type,data=db)
summary(model1)

## Part b)
## For seeing the outliers in our data
i<-1
x<-1
for(i in 1:ncol(db)){
  if(is.numeric(db[,i])==T){
    if(length(boxplot(db[,i],plot=T)$out)!=0){
      while(x==1){
        print("The numeric variable in the Census dataset having outliers are :")
        x=x+1
      }      
      print((colnames(db)[i]))
      boxplot((db)[i]) # boxplot of the variabes having outliers
    }
  }
}


# the loop will find and print all the Numeric variables having outliers

## Removing the outliers for 2 variables, Item Visibility and Item Outlet Sales
i<-1
outlier_item_visibility<-boxplot(db$Item_Visibility,plot=F)$stats[5]
outlier_item_outlet_sales<-boxplot(db$Item_Outlet_Sales,plot=F)$stats[5]
for(i in 1:nrow(db)){
  if(db$Item_Visibility[i]>outlier_item_visibility){
    db$Item_Visibility[i]=outlier_item_visibility
  }
  if(db$Item_Outlet_Sales[i]>outlier_item_outlet_sales){
    db$Item_Outlet_Sales[i]=outlier_item_outlet_sales
  }
}
boxplot(db$Item_Outlet_Sales)
boxplot(db$Item_Visibility,plot=F)


## Part c)  Plotting the Linear Regression Models 

par(mfcol=c(2,2))
plot(model1)


## Part d) Splitting data into train and test
library(caTools)
x<-sample.split(db$Item_Outlet_Sales,SplitRatio = 0.7)
train<-subset(db,x==T)
test<-subset(db,x==F)


## Part e) Building the model  with dependent variable as Item_Outlet_Sales
model2<-lm(Item_Outlet_Sales~Item_Visibility+Item_Type+Item_MRP+Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+Outlet_Type,data=train)
summary(model2)


## Part f) Predicting the Test data using model
result=predict(model2,test)


## Part g)  Calculating the MSE
mse=mean((result-test$Item_Outlet_Sales)^2)
mse
sqrt(mse)