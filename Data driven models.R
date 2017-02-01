
energy_data <- read.csv("energydata_complete.csv")
#View(energy_data)

energy_data$date <- strptime(as.character(energy_data$date),format="%Y-%m-%d %H:%M:%S")
energy_data$date <- as.POSIXct(energy_data$date,tz = "UTC")
class(energy_data$date)
str(energy_data)

names(energy_data)

#save.image(file="Results_prediction.RData")

load("Results_prediction.RData")

# Loading libraries

library(rpart)              # CART algorithm for decision trees
library(partykit)           # Plotting trees
library(gbm)                  # Boosting algorithms
library(doParallel)     # parallel processing
library(pROC)                 # plot the ROC curve
library(corrplot)  
library(psych)
library(Hmisc)
library(lubridate)
###

names(energy_data)



second_day <- function(x) {
  # x is an object in posixct format
  s <- hour(x)*3600+minute(x)*60+second(x)
  
}

weekend_weekday <- function(x) {
  val <- weekdays(x)
  if (val == "Saturday" | val == "Sunday") {
    val2 = "Weekend"
  }
  else {
    val2= "Weekday"
  }
  return(val2)
}



energy_data$NSM <- second_day(energy_data$date)









energy_data$WeekStatus <- unlist(lapply(energy_data$date,weekend_weekday))

energy_data$Day_of_week <-weekdays(energy_data$date)

unique(energy_data$WeekStatus)

unique(energy_data$Day_of_week)

class(energy_data$Day_of_week)

energy_data$Day_of_week <-as.factor(energy_data$Day_of_week)

energy_data$WeekStatus <- as.factor(energy_data$WeekStatus)

str(energy_data)

dim(energy_data)
names(energy_data)

# Here it is how the random variables were added do the data set...
#set.seed(1)
#energy_data$rv1 <- runif(dim(energy_data)[1],0,50)
#set.seed(1)
#energy_data$rv2 <- runif(dim(energy_data)[1],0,50)


#energy_data_ori <- read.csv("energydata_paper.csv")
#dim(energy_data_ori)
#names(energy_data_ori)
#19735 x 32
#dim(energy_data)
#19735 x 34


#energy_data_ori$date <- strptime(as.character(energy_data_ori$date),format="%Y-%m-%d %H:%M:%S")
#energy_data_ori$date <- as.POSIXct(energy_data_ori$date,tz = "UTC")

#all(energy_data ==energy_data_ori)



#energy_data_ori$my <- floor_date(energy_data_ori$date,"month")


#energy_data_ori$mhr <- floor_date(energy_data_ori$date,"hour")



# Plotting the APpliances profile
#
require(gridExtra)

plot1 <-qplot(energy_data$date,energy_data$Appliances,xlab='Time',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18) 
plot1
plot2 <-qplot(energy_data$date[1:1008],energy_data$Appliances[1:1008],xlab='Time (1 week)',ylab='Appliances Wh',geom="line")+theme_grey(base_size = 18) 
plot2


png('Appliances_profile_3_Jan29.png',width = 14, height = 10, units = 'in', res = 300)
grid.arrange(plot1, plot2, nrow=2)
dev.off()

# Plotting a boxplot

png('Appliances_histogram_boxplot2_Jan29.png',width = 14, height = 10, units = 'in', res = 300)
par(mfrow=c(2,1))
hist(energy_data$Appliances,main="",xlab = "Appliances Wh",breaks = 40,
     col='lightblue',xlim=c(0,1200),ylim=c(0,9000),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

boxplot(energy_data$Appliances,
        boxfill = "lightblue",horizontal=TRUE,ylim=c(0,1200),xlab="Appliances Wh",frame=F,
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
dev.off()





# HEAT MAP visualization
# Visualization of the Energy use per week with heat map


energy_data$my <- floor_date(energy_data$date,"month")


energy_data$mhr <- floor_date(energy_data$date,"hour")

library(plyr)
energy_data_Total_per_hour <-  ddply(energy_data, "mhr", summarise,
                                     Appliances=sum(Appliances))

energy_data_Total_per_hour

energy_data_Total_per_hour$Day_week <- wday(energy_data_Total_per_hour$mhr,label=TRUE)

head(energy_data_Total_per_hour)

class(energy_data_Total_per_hour)
summary(energy_data_Total_per_hour)
energy_data_Total_per_hour_na_removed <- na.omit(energy_data_Total_per_hour)

dim(energy_data_Total_per_hour)
names(energy_data_Total_per_hour)
dim(energy_data_Total_per_hour_na_removed)
names(energy_data_Total_per_hour_na_removed)
summary(energy_data_Total_per_hour_na_removed)


# getting now the week of the year
energy_data_Total_per_hour_na_removed$week_year <- week(energy_data_Total_per_hour_na_removed$mhr)
head(energy_data_Total_per_hour_na_removed)

unique(energy_data_Total_per_hour_na_removed$week_year)
# [1]  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
# first week only

energy_data_Total_per_hour_na_removed_w1 <- energy_data_Total_per_hour_na_removed[energy_data_Total_per_hour_na_removed$week_year ==3,]

energy_data_Total_per_hour_na_removed_w1

energy_data_Total_per_hour_na_removed_w1$Hour <- hour(energy_data_Total_per_hour_na_removed_w1$mhr)


names(energy_data_Total_per_hour_na_removed_w1)

library(ggplot2)
library(lubridate)
# "mhr"        "Appliances" "Day_week"   "week_year" "Hour" 
gg1 <-ggplot(energy_data_Total_per_hour_na_removed_w1,aes(x=Day_week,y=Hour,
                                                          fill=Appliances)) 

gg1

min(energy_data_Total_per_hour_na_removed_w1$Appliances)
# 190
max(energy_data_Total_per_hour_na_removed_w1$Appliances)

gg1 <- gg1 + geom_tile(color="white", size=0.50)+scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg1

library(viridis)
library(ggthemes)
#gg1 <- gg1 +scale_fill_viridis(name="Appliances energy",option="A")
#gg <- gg +scale_y_continuous(breaks=seq(0,23,1),trans="reverse")
gg1 <- gg1 +scale_y_continuous(breaks=seq(0,23,1)) 

gg1


gg1 <- gg1 + coord_equal()
gg1
gg1 <- gg1 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg1 <- gg1 + theme_tufte(base_family="Helvetica")
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text=element_text(size=9))
gg1 <- gg1 + theme(legend.title=element_text(size=9))
gg1 <- gg1 + theme(legend.text=element_text(size=9))
gg1



Total_per_hour_na_removed_w2 <- energy_data_Total_per_hour_na_removed[energy_data_Total_per_hour_na_removed$week_year ==4,]

Total_per_hour_na_removed_w2

Total_per_hour_na_removed_w2$Hour <- hour(Total_per_hour_na_removed_w2$mhr)




names(Total_per_hour_na_removed_w2)

gg2 <-ggplot(Total_per_hour_na_removed_w2,aes(x=Day_week,y=Hour,fill=Appliances))

min(Total_per_hour_na_removed_w2$Appliances)
#170

max(Total_per_hour_na_removed_w2$Appliances)
#3650


gg2 <- gg2 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))
gg2

#library(viridis)
library(ggthemes)
#gg2 <- gg2 +scale_fill_viridis(name="Appliances energy", option="A")
#gg <- gg +scale_y_continuous(breaks=seq(0,23,1),trans="reverse")
gg2 <- gg2 +scale_y_continuous(breaks=seq(0,23,1))
gg2 <- gg2 + coord_equal()

gg2 <- gg2 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg2 <- gg2 + theme_tufte(base_family="Helvetica")
gg2 <- gg2 + theme(plot.title=element_text(hjust=0))
gg2 <- gg2 + theme(axis.ticks=element_blank())
gg2 <- gg2 + theme(axis.text=element_text(size=9))
gg2 <- gg2 + theme(legend.title=element_text(size=9))
gg2 <- gg2 + theme(legend.text=element_text(size=9))
gg2


Total_per_hour_na_removed_w3 <- energy_data_Total_per_hour_na_removed[energy_data_Total_per_hour_na_removed$week_year ==5,]

Total_per_hour_na_removed_w3

Total_per_hour_na_removed_w3$Hour <- hour(Total_per_hour_na_removed_w3$mhr)


names(Total_per_hour_na_removed_w3)

gg3 <-ggplot(Total_per_hour_na_removed_w3,aes(x=Day_week,y=Hour,fill=Appliances))

min(Total_per_hour_na_removed_w3$Appliances)
max(Total_per_hour_na_removed_w3$Appliances)
# 3420

gg3 <- gg3 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))

library(viridis)
library(ggthemes)
#gg3 <- gg3 +scale_fill_viridis(name="Appliances energy", option="A")
#gg <- gg +scale_y_continuous(breaks=seq(0,23,1),trans="reverse")
gg3 <- gg3 +scale_y_continuous(breaks=seq(0,23,1))
gg3 <- gg3 + coord_equal()

gg3 <- gg3 + labs(x="Day of Week", y="Hour of day")#, title="Appliances energy consumption")
gg3 <- gg3 + theme_tufte(base_family="Helvetica")
gg3 <- gg3 + theme(plot.title=element_text(hjust=0))
gg3 <- gg3 + theme(axis.ticks=element_blank())
gg3 <- gg3 + theme(axis.text=element_text(size=9))
gg3 <- gg3 + theme(legend.title=element_text(size=9))
gg3 <- gg3 + theme(legend.text=element_text(size=9))
gg3


Total_per_hour_na_removed_w4 <- energy_data_Total_per_hour_na_removed[energy_data_Total_per_hour_na_removed$week_year ==6,]

Total_per_hour_na_removed_w4

Total_per_hour_na_removed_w4$Hour <- hour(Total_per_hour_na_removed_w4$mhr)


names(Total_per_hour_na_removed_w4)

gg4 <-ggplot(Total_per_hour_na_removed_w4,aes(x=Day_week,y=Hour,fill=Appliances))
min(Total_per_hour_na_removed_w4$Appliances)
# 180
max(Total_per_hour_na_removed_w4$Appliances)
#2740

gg4 <- gg4 + geom_tile(color="white", size=0.50) + scale_fill_gradient(low="yellow", high="red",limit=c(150,3800))

library(viridis)
library(ggthemes)
#gg4 <- gg4 +scale_fill_viridis(name="Appliances energy", option="A")
#gg <- gg +scale_y_continuous(breaks=seq(0,23,1),trans="reverse")
gg4 <- gg4 +scale_y_continuous(breaks=seq(0,23,1))
gg4 <- gg4 + coord_equal()

gg4 <- gg4 + labs(x="Day of Week", y="hour of day")#, title="Appliances energy consumption")
gg4 <- gg4 + theme_tufte(base_family="Helvetica")
gg4 <- gg4 + theme(plot.title=element_text(hjust=0))
gg4 <- gg4 + theme(axis.ticks=element_blank())
gg4 <- gg4 + theme(axis.text=element_text(size=9))
gg4 <- gg4 + theme(legend.title=element_text(size=9))
gg4 <- gg4 + theme(legend.text=element_text(size=9))
#gg4 <- gg4 +  theme(plot.margin=unit(c(0,0,0,0), "cm"))
gg4


require(gridExtra)
g <- grid.arrange(gg1, gg2,gg3, gg4,ncol=4)
g

ggsave(file="appliances_Scaled_Jan_29.png", plot = g,width=400,units="mm",limitsize = FALSE)

##


# Reading the training data

energy_data$my <- NULL
energy_data$mhr <- NULL
names(energy_data)



library(caret)
set.seed(1)
train_index <- createDataPartition(energy_data$Appliances,p=0.75,list=FALSE)

train_data <- energy_data[train_index,]
dim(train_data)
names(train_data)

# Saving training data in csv format

write.table(format(train_data, digits=19),file = "training.csv", sep = ",", row.names=FALSE)


# checkin if the two training (objects) files are the same afer saving

train_data2 <- read.csv("training.csv")
#View(energy_data)

train_data2$date <- strptime(as.character(train_data2$date),format="%Y-%m-%d %H:%M:%S")
train_data2$date <- as.POSIXct(train_data2$date,tz = "UTC")
class(train_data2$date)
str(train_data2)

#all(train_data ==train_data2) # TRUE
## they are the same

train_data <-train_data2

# creating the testing set here

test_data <- energy_data[-train_index,]
dim(test_data)
# 4932,32

# Saving testing data in csv format

write.table(format(test_data, digits=19),file = "testing.csv", sep = ",", row.names=FALSE)

testing2 <- read.csv("testing.csv")

testing2$date <- strptime(as.character(testing2$date),format="%Y-%m-%d %H:%M:%S")
testing2$date <- as.POSIXct(testing2$date,tz = "UTC")



# checking to see if the testings files (objects) are the same
all(testing2==test_data)
# TRUE


# # 
png('firstpairsplot_train_set_Jan29.png',width = 10, height = 10, units = 'in', res = 300)
pairs.panels(train_data[,c(2, 3, 4, 5, 6, 7, 8, 9)],lm=TRUE,
             ellipses = FALSE,rug=FALSE,scale=FALSE,
             cex.cor=1.1,cex.labels=1.7, cex.axis=1.7)
dev.off()

rcorr(as.matrix(train_data2[,c(2, 3, 4, 5, 6, 7, 8, 9)]))

# Second pairs plot
png('secondpairsplot_train_set_Jan29.png',width = 10, height = 10, units = 'in', res = 300)
pairs.panels(train_data2[,c(2,10,11,12,13,14,15)],lm=TRUE,ellipses = FALSE,rug=FALSE,scale=FALSE,
             cex.cor=1.1,cex.labels=1.7, cex.axis=1.7)
dev.off()

rcorr(as.matrix(train_data2[,c(2,10,11,12,13,14,15)]))

# Third pairs plot
png('thirdpairsplot_train_set_Jan29.png',width = 10, height = 10, units = 'in', res = 300)
pairs.panels(train_data2[,c(2,16,17,18,19,20,21)],lm=TRUE,ellipses = FALSE,rug=FALSE,scale=FALSE,
             cex.cor=1.1,cex.labels=1.7, cex.axis=1.7)
dev.off()

rcorr(as.matrix(train_data2[,c(2,16,17,18,19,20,21)]))

# Fourth pairs plot
png('fourthpairsplot_train_set_Jan29.png',width = 10, height = 10, units = 'in', res = 300)
pairs.panels(train_data2[,c(2,22,23,24,25,26,27,30,14)],lm=TRUE,ellipses = FALSE,rug=FALSE,scale=FALSE,
             cex.cor=1.1,cex.labels=1.7, cex.axis=1.7)
dev.off()


rcorr(as.matrix(train_data2[,c(2,22,23,24,25,26,27,30,14)]))

names(train_data2)

# Restarting R here 
.rs.restartR()

# Feature selection with Boruta



library(Boruta)
dim(train_data)
names(train_data[,c(2:32)])
str(train_data[,c(2:32)])



# Running Boruta in a smaller training subset for speed 

set.seed(1234)
train_data_ss<- train_data[,c(2:32)][sample(1:nrow(train_data), 4440,
                          replace=FALSE),]

dim(train_data_ss)

set.seed(1234)
Boruta.Appliances_ss    <- Boruta(Appliances~., data=train_data_ss,  doTrace = 2,ntree = 115)
Boruta.Appliances_ss    




print(Boruta.Appliances_ss)
plot(Boruta.Appliances_ss)

png('Borutaplot_Jan31_2017.png',width = 14, height = 10, units = 'in', res = 300)
par( mar=c(7.5, 5, 2, 1)) 
plot(Boruta.Appliances_ss,cex.axis=1,las=2,xlab="")
dev.off()

# Since rv1 and rv2 are not relevant, then removing them from the data sets
train_data2 <- subset(train_data,select=-c(rv1,rv2))

str(train_data2)
test_data2 <- subset(test_data,select=-c(rv1,rv2))
str(test_data2)


names(train_data2)
str(train_data2)
dim(train_data2)

dim(test_data2)




# adding dummy variables to use rfe from caret

library(dummies)
str(train_data)
head(train_data2)
str(train_data2)
new_train_data <- dummy.data.frame(train_data2,names=c("WeekStatus","Day_of_week"))
str(new_train_data)
dim(new_train_data)
#14803,37

new_test_data <- dummy.data.frame(test_data2,names=c("WeekStatus","Day_of_week"))
str(new_test_data)
dim(new_test_data)
# 4932x37



# using rfe # remember that all the fields need to be numeric

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
#control <- rfeControl(functions=lmFuncs, method="repeatedcv",repeats=10)
# run the RFE algorithm
str(new_train_data)
head(new_train_data[,c(3:37)])
head(new_train_data[,2])
set.seed(1)
registerDoParallel(4)
getDoParWorkers()
ptm <- proc.time()
results <- rfe(new_train_data[,c(3:37)], new_train_data[,2] , sizes=c(1:35), rfeControl=control)
results_time <- proc.time() - ptm

print(results)
# list the chosen features
names(new_train_data[,c(3:37)])[!names(new_train_data[,c(3:37)]) %in% predictors(results)]

names(new_train_data[,c(3:37)])[!names(new_train_data[,c(3:37)]) %in% unique(vari[vari$Variables==30,]$var)]


selectedVars <- results$variables
# Top 5 variables
bestVar5 <- results$control$functions$selectVar(selectedVars, 5)
bestVar5

# Top 20 variables
bestVar20 <- results$control$functions$selectVar(selectedVars, 20)
bestVar20
#


bestVar35 <- results$control$functions$selectVar(selectedVars, 35)
bestVar35
#

# plot the results
plot(results, type=c("g", "o"))

results$optVariables
results$optsize
results$perfNames

results

png('Number of variables.png',width = 8, height = 6, units = 'in', res = 300)
plot(results, type=c("g", "o"))
dev.off()


# training gbm model


fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  
                           verboseIter = TRUE,returnResamp = "all")

gbmGrid <-  expand.grid(interaction.depth = c(1,3,5),
                        n.trees = seq(100,10901,400),
                        shrinkage = 0.1,
                        n.minobsinnode = c(10))


set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

gbm_model <- train(Appliances~., data=new_train_data[,c(2:37)],  method="gbm",
                   metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time <- proc.time() - ptm

gbm_model$call



# user  system elapsed 
# 491.18    1.83 9322.28 


#testing linear model in caret

lmcvFit <- train(Appliances~., data=new_train_data[,c(2:37)],  method="lm",trControl = fitControl,
                 metric='RMSE')

lmcvFit

summary(lmcvFit)

plot(lmcvFit)

residuals<-resid(lmcvFit)

predictedValues<-predict(lmcvFit)

plot(new_train_data$Appliances,residuals,xlab="Appliances Energy consumption", ylab="Residuals")

abline(0,0)

png('lm_model_Jan27.png',width = 8, height = 6, units = 'in', res = 300)
plot(new_train_data$Appliances,residuals,xlab="Appliances Energy consumption", ylab="Residuals")
abline(0,0)
dev.off()


# some more plots here
plot(new_train_data$Appliances,predictedValues)

lmcvFit$finalModel

varImp(lmcvFit)
length(varImp(lmcvFit))
plot(varImp(lmcvFit))




#The final values used for the model were n.trees = 10900, interaction.depth = 5, shrinkage = 0.1 and
#n.minobsinnode = 10. 
plot(gbm_model)

png('Gbm_model.png',width = 8, height = 6, units = 'in', res = 300)
plot(gbm_model)
dev.off()

plot(varImp(gbm_model))

# best tunning parameters
gbm_model$bestTune


descrCor <-  cor(train_data[,c(2:31)])
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)



library(Metrics)
predict(gbm_model,energy_data_tr[,c(2:36)])


# in the training set
rmse(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))
# 17.56036
rmse(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))
# 66.65

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

MAE <- function(y, yhat) {
  mean(abs((y - yhat)))
}

MAPE2 <- function(y, yhat) {
  
  n <- length(y)
  su <- 0
  for (i in 1:n) {
    su <-su+ abs((y[i]-yhat[i])/y[i])
    
  }
  print (su/n)
}



MAE2 <- function(y, yhat) {
  
  n <- length(y)
  su <- 0
  for (i in 1:n) {
    su <-su+ abs((y[i]-yhat[i]))
    
  }
  print (su/n)
}



Rsqu <- function(y, yhat) {
  
  mdata <- mean(y)
  
  1 - sum((y-yhat)^2)/(sum((y-mdata)^2))
  
}


Rmse_f <- function(y, yhat) {
  
  sqrt(mean((y-yhat)^2))
  
}




MAPE(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))*100
# 0.1627125
MAPE(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))*100
# 0.3829354

MAE(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))
# 11.97
MAE2(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))
# 11.97

MAPE(c(1,2,3,4),c(0,2,3,4))



Rsqu(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))


MAPE2(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))


# Training SVM model



grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)


set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()
svm_model <- train(Appliances~., data=new_train_data[,c(2:37)],  method="svmRadial",
                   metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                   tuneGrid = grid)
svm_time <- proc.time() - ptm
svm_time
print(svm_model)
plot(svm_model)


grid2 <- expand.grid(sigma = c(0.1,0.25,0.30),
                     C = c( 1.1,1.5,1.8,2)
)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()
svm_model2 <- train(Appliances~., data=new_train_data[,c(2:37)],  method="svmRadial",
                    metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                    tuneGrid = grid2)
svm_time2 <- proc.time() - ptm
svm_time2
print(svm_model2)
plot(svm_model2)

png('svm_model.png',width = 8, height = 6, units = 'in', res = 300)
plot(svm_model2)
dev.off()


# svm 3 


grid3 <- expand.grid(sigma = c(0.35,0.4,0.1),
                     C = c(1,3.0,5.0,8.0,10,12)
)
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()
svm_model3 <- train(Appliances~., data=new_train_data[,c(2:37)],  method="svmRadial",
                    metric='RMSE',trControl = fitControl, preProc=c("center","scale"),
                    tuneGrid = grid3)
svm_time3 <- proc.time() - ptm
svm_time3
print(svm_model3)
plot(svm_model3)

png('svm_model_3.png',width = 8, height = 6, units = 'in', res = 300)
plot(svm_model3)
dev.off()






plot(gbm_model)

rvalues <- resamples(list(svm=svm_model3,gbm=gbm_model,lm=lmcvFit))


rvalues$values

dotplot(rvalues,metric = "RMSE")


##
# tRAINING THE rf MODEL
fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  # 10,3
                           verboseIter = TRUE,returnResamp = "all")
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

rf_default <- train(Appliances~., data=new_train_data[,c(2:37)],
                    method="rf",metric='RMSE',
                    trControl=fitControl,importance = TRUE )

rf_default_time <- proc.time() - ptm
rf_default 
rf_default_time
plot(rf_default)

print(rf_default)
plot(rf_default)




library(gridBase)
library(gridExtra)
library(lattice)
x <-1:500
y <- sqrt(rf_default$finalModel$mse)
n_trees <- xyplot(y ~ x, 
                  ylab="RMSE", xlab="Number of Trees")
dev.off()
rf_g <-plot(1:500,sqrt(rf_default$finalModel$mse),type='l',col='blue',axes=TRUE,xlab="Number of Trees",ylab="RMSE")

tress_g <- plot(rf_default)

class(tress_g)

panel_plot <- grid.arrange(n_trees, tress_g,ncol=2)
ggsave(file="rf_model.png", panel_plot)



# 

plot(varImp(rf_default))
plot(varImp(svm_model))
plot(varImp(gbm_model))

rvalues <- resamples(list(SVM_Radial=svm_model3,GBM=gbm_model,Lm=lmcvFit,RF=rf_default))

rvalues$values

RMSE_ALL <- dotplot(rvalues,metric = "RMSE")


RSQ_ALL <- dotplot(rvalues,metric="Rsquared")

panel_plot_models <- grid.arrange(RMSE_ALL, RSQ_ALL,ncol=2)
panel_plot_models

ggsave(file="panel_plot_models2.png", panel_plot_models)

##

# Evaluating the RMSE for different models



rmse(train_data2$Appliances,predict(lmcvFit,new_train_data[,c(2:37)]))
# 93.20819

Rmse_f(train_data2$Appliances,predict(lmcvFit,new_train_data[,c(2:37)]))

MAE(train_data2$Appliances,predict(lmcvFit,new_train_data[,c(2:37)]))
#53.13

MAPE(train_data2$Appliances,predict(lmcvFit,new_train_data[,c(2:37)]))*100
# 61.32
Rsqu(train_data2$Appliances,predict(lmcvFit,new_train_data[,c(2:37)]))
# 0.18

rmse(test_data2$Appliances,predict(lmcvFit,new_test_data[,c(2:37)]))
# 93.17608 

MAE(test_data2$Appliances,predict(lmcvFit,new_test_data[,c(2:37)]))
# 51.97

MAPE(test_data2$Appliances,predict(lmcvFit,new_test_data[,c(2:37)]))*100
# 59.93

Rsqu(test_data2$Appliances,predict(lmcvFit,new_test_data[,c(2:37)]))
#0.16


rmse(train_data2$Appliances,predict(svm_model3,new_train_data[,c(2:37)]))
# 39.35
Rmse_f(train_data2$Appliances,predict(svm_model3,new_train_data[,c(2:37)]))


MAE(train_data2$Appliances,predict(svm_model3,new_train_data[,c(2:37)]))
# 15.08

MAPE(train_data2$Appliances,predict(svm_model3,new_train_data[,c(2:37)]))*100
#15.60
Rsqu(train_data2$Appliances,predict(svm_model3,new_train_data[,c(2:37)]))
# 0.85


rmse(test_data2$Appliances,predict(svm_model3,new_test_data[,c(2:37)]))
# 70.74
MAE(test_data2$Appliances,predict(svm_model3,new_test_data[,c(2:37)]))
#  31.36

MAPE(test_data2$Appliances,predict(svm_model3,new_test_data[,c(2:37)]))*100
#29.76
Rsqu(test_data2$Appliances,predict(svm_model3,new_test_data[,c(2:37)]))
# 0.5152



set.seed(1)
rmse(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))
# 17.56

MAE(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))
# 11.97

set.seed(1)
MAPE2(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))*100
MAPE(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))*100

set.seed(1)
Rsqu(train_data2$Appliances,predict(gbm_model,new_train_data[,c(2:37)]))


#16.27
set.seed(1)
rmse(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))
# 66.65093
set.seed(1)
MAE(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))

MAPE(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))*100
MAPE2(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))*100
# 38.29 
set.seed(1)
Rsqu(test_data2$Appliances,predict(gbm_model,new_test_data[,c(2:37)]))
#0.57


rf_default

set.seed(1)
rmse(train_data2$Appliances,predict(rf_default,new_train_data[,c(2:37)]))
# 29.61314
set.seed(1)
MAE(train_data2$Appliances,predict(rf_default,new_train_data[,c(2:37)]))
# 13.75

set.seed(1)
MAPE(train_data2$Appliances,predict(rf_default,new_train_data[,c(2:37)]))*100
# 13.43
set.seed(1)
Rsqu(train_data2$Appliances,predict(rf_default,new_train_data[,c(2:37)]))
# 0.917


set.seed(1)
rmse(test_data2$Appliances,predict(rf_default,new_test_data[,c(2:37)]))
# 68.48478

set.seed(1)
MAE(test_data2$Appliances,predict(rf_default,new_test_data[,c(2:37)]))
# 31.85
set.seed(1)
MAPE(test_data2$Appliances,predict(rf_default,new_test_data[,c(2:37)]))*100
# 31.39

set.seed(1)
Rsqu(test_data2$Appliances,predict(rf_default,new_test_data[,c(2:37)]))
#0.54


RF_imp   <- plot(varImp(rf_default),main="RF Variable Importance")
SVM_imp  <- plot(varImp(svm_model3),main="SVM Variable Importance")
gbm_imp  <- plot(varImp(gbm_model),main="GBM Variable Importance")


panelVIMP_plot_models <- grid.arrange(RF_imp, gbm_imp,SVM_imp,ncol=3)

ggsave(file="panel_plot_VIMP2.png", panelVIMP_plot_models)


# 

#Now only with temperature and weather data (no lights from submetering)
names(new_train_data)

new_train_data_b <- subset(new_train_data,select=-c(lights))

str(new_train_data_b)
names(new_train_data_b)



new_test_data_b <- subset(new_test_data,select=-c(lights))
names(new_test_data_b)

#Training gbm model without light


fitControl <- trainControl(method = "repeatedcv", # cv
                           number=10,repeats=3,  # 10  
                           verboseIter = TRUE,returnResamp = "all")

gbmGrid <-  expand.grid(interaction.depth = c(1,3,5),
                        n.trees = seq(100,10901,400),
                        shrinkage = 0.1,
                        n.minobsinnode = c(10))


set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

gbm_model_no_lights <- train(Appliances~., data=new_train_data_b[,c(2:36)],  method="gbm",
                             metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_no_lights <- proc.time() - ptm

plot(gbm_model_no_lights)

gbm_model_no_lights$call


dim(new_train_data_b)









# no weather data but with T, and RH

#Now only with temperature and weather data (no lights from submetering)
names(new_train_data)

new_train_data_c <- subset(new_train_data,select=-c(lights,T_out,
                                                    Press_mm_hg,RH_out,
                                                    Windspeed,Visibility,
                                                    Tdewpoint))

str(new_train_data_c)
names(new_train_data_c)



new_test_data_c <- subset(new_test_data,select=-c(lights,T_out,
                                                  Press_mm_hg,RH_out,
                                                  Windspeed,Visibility,
                                                  Tdewpoint))
names(new_test_data_c)

# lightst and weather data , no T and Humidity
names(new_train_data)

new_train_data_d <- subset(new_train_data,select=-c(T1,RH_1,T2,RH_2,
                                                    T3,RH_3,T4,RH_4,
                                                    T5,RH_5,T6,RH_6,
                                                    T7,RH_7,T8,RH_8,
                                                    T9,RH_9))

str(new_train_data_d)
names(new_train_data_d)



new_test_data_d <- subset(new_test_data,select=-c(T1,RH_1,T2,RH_2,
                                                  T3,RH_3,T4,RH_4,
                                                  T5,RH_5,T6,RH_6,
                                                  T7,RH_7,T8,RH_8,
                                                  T9,RH_9))

names(new_test_data_d)

names(new_test_data)


new_train_data_e <- subset(new_train_data,select=-c(T1,RH_1,T2,RH_2,
                                                    T3,RH_3,T4,RH_4,
                                                    T5,RH_5,T6,RH_6,
                                                    T7,RH_7,T8,RH_8,
                                                    T9,RH_9,lights))


new_test_data_e <- subset(new_test_data,select=-c(T1,RH_1,T2,RH_2,
                                                  T3,RH_3,T4,RH_4,
                                                  T5,RH_5,T6,RH_6,
                                                  T7,RH_7,T8,RH_8,
                                                  T9,RH_9,lights))

names(new_train_data_e)
names(new_test_data_e)





### training with c and d data sets

set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

gbm_model_no_lights_no_weather <- train(Appliances~., data=new_train_data_c[,c(2:30)],  method="gbm",
                                        metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_no_lights_no_weather <- proc.time() - ptm

plot(gbm_model_no_lights_no_weather)


names(new_train_data_c)
dim(new_train_data_c)

# d data set
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

gbm_model_lights_weather <- train(Appliances~., data=new_train_data_d[,c(2:19)],  method="gbm",
                                  metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_time_lights_weather <- proc.time() - ptm

plot(gbm_model_lights_weather)


names(new_train_data_d)
dim(new_train_data_e)


#e data set
set.seed(1)
registerDoParallel(4)
getDoParWorkers()

ptm <- proc.time()

gbm_model_weather <- train(Appliances~., data=new_train_data_e[,c(2:18)],  method="gbm",
                           metric='RMSE',trControl = fitControl,bag.fraction=0.5,tuneGrid=gbmGrid)
gbm_model_time_weather <- proc.time() - ptm

plot(gbm_model_weather)

gbm_model_weather$coefnames




m1 <- plot(varImp(gbm_model_lights_weather),main="GBM Lght & Weath")
m2 <- plot(varImp(gbm_model_no_lights_no_weather),main="GBM NO Lght & NO Weath")
m3 <- plot(varImp(gbm_model_no_lights),main="GBM NO Lght")
m4 <- plot(varImp(gbm_model_weather),main="GBM Only Weath")


panelVIMP_plots_2 <- grid.arrange(m1,m2,m3,m4,ncol=4)

ggsave(file="panel_plot_VIMP_data_subsets.png", 
       panelVIMP_plots_2,
       width=14,height=8)


gbm_model_no_lights$coefnames

gbm_model_no_lights_no_weather$coefnames
gbm_model_lights_weather$coefnames
gbm_model_weather$coefnames


rvalues2 <- resamples(list(GBM_NO_LIGHTS=gbm_model_no_lights,
                           GBM_NO_LIGHTS_NO_WEATHER=gbm_model_no_lights_no_weather,
                           GBM_LIGHTS_WEATHER=gbm_model_lights_weather,
                           GBM_WEATHER=gbm_model_weather))

dotplot(rvalues2,metric="Rsquared")
dotplot(rvalues2,metric="RMSE")



dotplot(rvalues2)



RMSE_GBM_subsets <- dotplot(rvalues2,metric = "RMSE")
RSQ_GBM_Subsets <- dotplot(rvalues2,metric="Rsquared")

panel_plot_GBMmodels_confidence <- grid.arrange(RMSE_GBM_subsets , RSQ_GBM_Subsets,ncol=2)
panel_plot_GBMmodels_confidence

ggsave(file="panel_plot_GBM_models.png", panel_plot_GBMmodels_confidence,
       width=14,height=8)



# Testing in the training and test sets RMSE
#gbm models with no lights
set.seed(1)
rmse(new_train_data_b$Appliances,predict(gbm_model_no_lights,new_train_data_b))
# 17.9028

set.seed(1)
MAE(new_train_data_b$Appliances,predict(gbm_model_no_lights,new_train_data_b))
#12.24

set.seed(1)
MAPE(new_train_data_b$Appliances,predict(gbm_model_no_lights,new_train_data_b))*100
# 16.66

set.seed(1)
Rsqu(new_train_data_b$Appliances,predict(gbm_model_no_lights,new_train_data_b))


set.seed(1)
rmse(new_test_data_b$Appliances,predict(gbm_model_no_lights,new_test_data_b))
#66.2134

set.seed(1)
MAE(new_test_data_b$Appliances,predict(gbm_model_no_lights,new_test_data_b))
# 35.25


set.seed(1)
MAPE(new_test_data_b$Appliances,predict(gbm_model_no_lights,new_test_data_b))*100

set.seed(1)
Rsqu(new_test_data_b$Appliances,predict(gbm_model_no_lights,new_test_data_b))
#0.575


#gbm_model_no_lights_no_weather
set.seed(1)
rmse(new_train_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_train_data_c))
# 18.83

set.seed(1)
MAE(new_train_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_train_data_c))
# 12.85

set.seed(1)
MAPE(new_train_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_train_data_c))*100
# 17.44

set.seed(1)
Rsqu(new_train_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_train_data_c))
#0.97


set.seed(1)
rmse(new_test_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_test_data_c))
# 68.59

set.seed(1)
MAE(new_test_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_test_data_c))
# 36.21

set.seed(1)
MAPE(new_test_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_test_data_c))*100
#39.23
set.seed(1)
Rsqu(new_test_data_c$Appliances,predict(gbm_model_no_lights_no_weather,new_test_data_c))


# gbm_model_lights_weather
set.seed(1)
rmse(new_train_data_d$Appliances,predict(gbm_model_lights_weather,new_train_data_d))
#27.47
set.seed(1)
MAE(new_train_data_d$Appliances,predict(gbm_model_lights_weather,new_train_data_d))
# 18.29

set.seed(1)
MAPE(new_train_data_d$Appliances,predict(gbm_model_lights_weather,new_train_data_d))*100
# 23.71
set.seed(1)
Rsqu(new_train_data_d$Appliances,predict(gbm_model_lights_weather,new_train_data_d))
# 0.93


set.seed(1)
rmse(new_test_data_d$Appliances,predict(gbm_model_lights_weather,new_test_data_d))
#72.64

set.seed(1)
MAE(new_test_data_d$Appliances,predict(gbm_model_lights_weather,new_test_data_d))
# 40.32

set.seed(1)
MAPE(new_test_data_d$Appliances,predict(gbm_model_lights_weather,new_test_data_d))*100

set.seed(1)
Rsqu(new_test_data_d$Appliances,predict(gbm_model_lights_weather,new_test_data_d))
# 0.49



# gbm_model_weather
set.seed(1)
rmse(new_train_data_e$Appliances,predict(gbm_model_weather,new_train_data_e))
# 28.29
set.seed(1)
MAE(new_train_data_e$Appliances,predict(gbm_model_weather,new_train_data_e))
# 18.85


set.seed(1)
MAPE(new_train_data_e$Appliances,predict(gbm_model_weather,new_train_data_e))*100
# 24.41198

set.seed(1)
Rsqu(new_train_data_e$Appliances,predict(gbm_model_weather,new_train_data_e))
# 0.92


set.seed(1)
rmse(new_test_data_e$Appliances,predict(gbm_model_weather,new_test_data_e))
# 72.45

set.seed(1)
MAE(new_test_data_e$Appliances,predict(gbm_model_weather,new_test_data_e))
# 40.72


set.seed(1)
MAPE(new_test_data_e$Appliances,predict(gbm_model_weather,new_test_data_e))*100
# 46.53

set.seed(1)
Rsqu(new_test_data_e$Appliances,predict(gbm_model_weather,new_test_data_e))
# 0.49



