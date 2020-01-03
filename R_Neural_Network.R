## Author: Rajesh Jakhotia
## Company Name: K2 Analytics Finishing School Pvt. Ltd
## Email : ar.jakhotia@k2analytics.co.in
## Website : k2analytics.co.in


## Let us first set the working directory path
setwd ("D:/K2Analytics/Neural_Network/")
getwd()


## Ideally for any modeling you should have Training & Testing dataset
## Typically you would use sampling strategy
## However for the Neural Net training I am supplying the Training & Testing data separately


nn.dev <- read.table("datafile/DEV_SAMPLE.csv", sep = ",", header = T)
nn.holdout <- read.table("datafile/HOLDOUT_SAMPLE.csv", sep = ",", header = T)

##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.dev)
##nn.dev <- data.frame(nn.dev, Gender.matrix)



##occ.matrix <- model.matrix(~ Occupation - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, occ.matrix)

##Gender.matrix <- model.matrix(~ Gender - 1, data = nn.holdout)
##nn.holdout <- data.frame(nn.holdout, Gender.matrix)

c(nrow(nn.dev), nrow(nn.holdout))
str(nn.dev)

## Installing the Neural Net package; 
## If already installed do not run the below step
##install.packages("neuralnet")


library(neuralnet)

nn1 <- neuralnet(formula = Target ~  Age +  Balance  + SCR +  No_OF_CR_TXNS + Holding_Period , 
                 data = nn.dev, 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)

quantile(nn1$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
## The distribution of the estimated results

misClassTable = data.frame(Target = nn.dev$Target, Prediction = nn1$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.143,1,0)
with(misClassTable, table(Target, Classification))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
library(caret)
confusionMatrix(misClassTable$Target, misClassTable$Classification)





## build the neural net model by scaling the variables
x <- subset(nn.dev, 
            select = c("Age","Balance", "SCR", "No_OF_CR_TXNS", "Holding_Period"
##,"OccupationPROF", "OccupationSAL", "OccupationSELF.EMP", "OccupationSENP","GenderF", "GenderM", "GenderO"
                       )
)
nn.devscaled <- scale(x)
nn.devscaled <- cbind(nn.dev[2], nn.devscaled)
View(nn.devscaled)

nn2 <- neuralnet(formula = Target ~  Age + Balance  + SCR +  No_OF_CR_TXNS + Holding_Period ,
      ## + OccupationPROF + OccupationSAL + OccupationSELF.EMP + OccupationSENP + GenderF + GenderM + GenderO,
                      data = nn.devscaled, 
                      hidden = 3,
                      err.fct = "sse",
                      linear.output = FALSE,
                      lifesign = "full",
                      lifesign.step = 10,
                      threshold = 0.1,
                      stepmax = 2000
                    )
nn2
plot(nn2)


quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable = data.frame(Target = nn.devscaled$Target, Predict.score = nn2$net.result[[1]] )
misClassTable$Predict.class = ifelse(misClassTable$Predict.score>0.21,1,0)
with(misClassTable, table(Target, Predict.class))


confusionMatrix(misClassTable$Predict.class, misClassTable$Target)

## Error Computation
sum((misClassTable$Target - misClassTable$Predict.score)^2)/2


## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## deciling
misClassTable$deciles <- decile(misClassTable$Predict.score)


## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(misClassTable)
rank <- tmp_DT[, list(
  cnt = length(Target), 
  cnt_resp = sum(Target), 
  cnt_non_resp = sum(Target == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);


library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)



## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute
x <- subset(nn.holdout, 
            select = c("Age","Balance", "SCR", "No_OF_CR_TXNS", "Holding_Period")
          )
x.scaled <- scale(x)
compute.output = compute(nn2, x.scaled)
nn.holdout$Predict.score = compute.output$net.result
View(nn.holdout)
quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)



## Building Neural Net Moodel using caret package

dev_trainplot = predict(
                  preProcess(nn.dev[,-1], method="range"),
                  nn.dev[,-1]
                )
nrow(nn.dev)
featurePlot(dev_trainplot[, lapply(dev_trainplot, class) %in% c("numeric", "integer")], 
            as.factor(nn.dev$Target), "box")




##install.packages("e1071")
##install.packages("nnet")
cv_opts = trainControl(method="cv", number=10)
library(e1071)
head(nn.dev)
results_nnet = train(as.factor(Target)~., data=nn.dev[,-1], method="avNNet",
                     trControl=cv_opts, preProcess="range",
                     tuneLength=2, trace=T, maxit=1000)
results_nnet


preds_nnet = predict(results_nnet, nn.dev[,-1]) 
confusionMatrix(preds_nnet, as.factor(nn.dev$Target))
