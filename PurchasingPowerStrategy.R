##importing importnat libraries
library(e1071)
library(party)
library(caret)
library(randomForest)
library(plyr)

##reading data files from saved csv files in documemt folder
testData = read.csv("TEST.csv")[,1:85]    ##test Data set
ValData = read.csv("VAL.csv")      ## ValidationData

CustomerValueData = read.csv("customerRatingMeasures.csv") ## customer values for each attribute and levels
ProductValueData = read.csv("ProductValueMeasures.csv") ## product values by each attribute and levels

## create intitial tree to find out important columns
ForestModel = cforest(col43~.,testData)
pt <- prettytree(ForestModel@ensemble[[1]], names(ForestModel@data@get("input"))) 
nt <- new("BinaryTree")
nt@tree <- pt 
nt@data <- ForestModel@data 
nt@responses <- ForestModel@responses 
plot(nt, type="simple")

## Use of previous tree to select attributes train new tree for recommendations
selectedColumn = c("col39","col42","col40","col34","col5","col25","col64")
testData$col86 = as.character(testData$col43)
testData$col86 = as.factor(testData$col43)
ValidationData = testData[,selectedColumn]
RandomForestModel = randomForest(col43~., testData[,c(selectedColumn,"col43")])

## Use this tree to create strategies for selected attributes by stopping the flow of tree in effective way
RecommendationModel = cforest(col43~., testData[,c(selectedColumn,"col43")])
ptNew = prettytree(RecommendationModel@ensemble[[1]], names(RecommendationModel@data@get("input"))) 
ntNew = new("BinaryTree")
ntNew@tree = ptNew
ntNew@data = RecommendationModel@data
ntNew@responses = RecommendationModel@responses
plot(ntNew, type="simple")

## pie chat of the distribution of important attributes, keeping predicted data as reference 
predictions = round(predict(RandomForestModel,ValData[,selectedColumn]))
ResultData = ValData[,selectedColumn]
ResultData$col43 = predictions

levels = unique(predictions)
levelProfit = 0

for (level in levels){
	profitCount = 0
	levelData = ResultData[ResultData[,"col43"]==level,]
	col39_count = count(levelData$col39)
	col42_count = count(levelData$col42)
	col40_count = count(levelData$col40)
	col34_count = count(levelData$col34)
	col5_count = count(levelData$col5)
	col25_count = count(levelData$col25)
	col64_count = count(levelData$col64)
	
	pie(col39_count[,2],labels = col39_count[,1], main="Distribution of Income 45-75", col = rainbow(length(col39_count[,1])))
	legend("topright", as.character(col39_count[,1]), cex = 0.8,fill = rainbow(length(col39_count[,1])))
	for (row in 1:nrow(col39_count)){
		profitCount = profitCount+ CustomerValueData[col39_count[row,1]+1,"col39"]*col39_count[row,2]+ProductValueData[col39_count[row,1]+1,"col39"]*col39_count[row,2]}
		
	pie(col42_count[,2],labels = col42_count[,1], main="Distribution of Avg Income", col = rainbow(length(col42_count[,1])))
	legend("topright", as.character(col42_count[,1]), cex = 0.8,fill = rainbow(length(col42_count[,1])))
	for (row in 1:nrow(col42_count)){
		profitCount = profitCount+ CustomerValueData[col42_count[row,1]+1,"col42"]*col42_count[row,2]+ProductValueData[col42_count[row,1]+1,"col42"]*col42_count[row,2]}
		
	pie(col40_count[,2],labels = col40_count[,1], main="Distribution of Income 75-122", col = rainbow(length(col40_count[,1])))
	legend("topright", as.character(col40_count[,1]), cex = 0.8,fill = rainbow(length(col40_count[,1])))
	for (row in 1:nrow(col40_count)){
		profitCount = profitCount+ CustomerValueData[col40_count[row,1]+1,"col40"]*col40_count[row,2]+ProductValueData[col40_count[row,1]+1,"col40"]*col40_count[row,2]}
		
	pie(col34_count[,2],labels = col34_count[,1], main="Distribution of No Car", col = rainbow(length(col34_count[,1])))
	legend("topright", as.character(col34_count[,1]), cex = 0.8,fill = rainbow(length(col34_count[,1])))
	for (row in 1:nrow(col34_count)){
		profitCount = profitCount+ CustomerValueData[col34_count[row,1]+1,"col34"]*col34_count[row,2]+ProductValueData[col34_count[row,1]+1,"col34"]*col34_count[row,2]}
		
	pie(col5_count[,2],labels = col5_count[,1], main="Distribution of Customer main type", col = rainbow(length(col5_count[,1])))
	legend("topright", as.character(col5_count[,1]), cex = 0.8,fill = rainbow(length(col5_count[,1])))
	for (row in 1:nrow(col5_count)){
		profitCount = profitCount+ CustomerValueData[col5_count[row,1],"col5"]*col5_count[row,2]+ProductValueData[col5_count[row,1],"col5"]*col5_count[row,2]}
		
	pie(col25_count[,2],labels = col25_count[,1], main="Distribution of Social CLass A", col = rainbow(length(col25_count[,1])))
	legend("topright", as.character(col25_count[,1]), cex = 0.8,fill = rainbow(length(col25_count[,1])))
	for (row in 1:nrow(col25_count)){
		profitCount = profitCount+ CustomerValueData[col25_count[row,1]+1,"col25"]*col25_count[row,2]+ProductValueData[col25_count[row,1]+1,"col25"]*col25_count[row,2]}
		
	pie(col64_count[,2],labels = col64_count[,1], main="Distribution of Contribution social security insurance policies", col = rainbow(length(col64_count[,1])))
	legend("topright", as.character(col64_count[,1]), cex = 0.8,fill = rainbow(length(col64_count[,1])))
	for (row in 1:nrow(col64_count)){
		profitCount = profitCount+ CustomerValueData[col64_count[row,1]+1,"col64"]*col64_count[row,2]+ProductValueData[col64_count[row,1]+1,"col64"]*col64_count[row,2]}
		
	levelProfit = c(levelProfit,profitCount)
}

##accuracy = confusionMatrix(data=factor(predictions), reference=factor(ValData$col43))$overall["Accuracy"]


levelProfit = levelProfit[2:length(levelProfit)]
TotalProfit = sum(levelProfit)
PercentageDistribution = round(levelProfit*100/sum(levelProfit),2)
pie(levelProfit,labels = PercentageDistribution, main="Distribution of Profit for Purchasing Power", col = rainbow(length(levelProfit)))
legend("topright", as.character(levels), cex = 0.8,fill = rainbow(length(levels)))