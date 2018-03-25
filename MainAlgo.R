##importing importnat libraries
library(e1071)
library(party)
library(caret)
library(randomForest)
library(plyr)

##reading data files from saved csv files in documemt folder
testData = read.csv("TEST.csv")    ##test Data set
ValData = read.csv("VAL.csv")      ## ValidationData
results = read.csv("RESULTS.csv") ## Mobile Home policy attribute data
CustomerValueData = read.csv("customerRatingMeasures.csv") ## customer values for each attribute and levels
ProductValueData = read.csv("ProductValueMeasures.csv") ## product values by each attribute and levels

## create intitial tree to find out important columns
ForestModel = cforest(col86~.,testData)
pt <- prettytree(ForestModel@ensemble[[1]], names(ForestModel@data@get("input"))) 
nt <- new("BinaryTree")
nt@tree <- pt 
nt@data <- ForestModel@data 
nt@responses <- ForestModel@responses 
plot(nt, type="simple")

## Use of previous tree to select attributes train new tree for recommendations
selectedColumn = c("col1","col13","col47","col9","col59","col37")
testData$col86 = as.character(testData$col86)
testData$col86 = as.factor(testData$col86)
ValidationData = testData[,selectedColumn]
RandomForestModel = randomForest(col86~., testData[,c(selectedColumn,"col86")])

## Use this tree to create strategies for selected attributes by stopping the flow of tree in effective way
RecommendationModel = cforest(col86~., testData[,c(selectedColumn,"col86")])
ptNew = prettytree(RecommendationModel@ensemble[[1]], names(RecommendationModel@data@get("input"))) 
ntNew = new("BinaryTree")
ntNew@tree = ptNew
ntNew@data = RecommendationModel@data
ntNew@responses = RecommendationModel@responses
plot(ntNew, type="simple")

## pie chat of the distribution of important attributes, keeping predicted data as reference 
predictions = predict(RandomForestModel,ValData[,selectedColumn])
ResultData = ValData[,selectedColumn]
ResultData$col86 = predictions

levels = unique(predictions)
levelProfit = 0

for (level in levels){
	profitCount = 0
	levelData = ResultData[ResultData[,"col86"]==level,]
	col1_count = count(levelData$col1)
	col13_count = count(levelData$col13)
	col47_count = count(levelData$col47)
	col9_count = count(levelData$col9)
	col59_count = count(levelData$col59)
	col37_count = count(levelData$col37)
	print (profitCount)

	pie(col1_count[,2],labels = col1_count[,1], main="Distribution of Customer Subtype", col = rainbow(length(col1_count[,1])))
	legend("topright", as.character(col1_count[,1]), cex = 0.8,fill = rainbow(length(col1_count[,1])))
	for (row in 1:nrow(col1_count)){
		profitCount = profitCount+ CustomerValueData[col1_count[row,1],"col1"]*col1_count[row,2]+ProductValueData[col1_count[row,1],"col1"]*col1_count[row,2]}
	print (profitCount)
	
	pie(col13_count[,2],labels = col13_count[,1], main="Distribution of Singles", col = rainbow(length(col13_count[,1])))
	legend("topright", as.character(col13_count[,1]), cex = 0.8,fill = rainbow(length(col13_count[,1])))
	for (row in 1:nrow(col13_count)){
		profitCount = profitCount+ CustomerValueData[col13_count[row,1]+1,"col13"]*col13_count[row,2]+ProductValueData[col13_count[row,1]+1,"col13"]*col13_count[row,2]}
	print (profitCount)
	
	pie(col47_count[,2],labels = col47_count[,1], main="Distribution of Contribution car policies", col = rainbow(length(col47_count[,1])))
	legend("topright", as.character(col47_count[,1]), cex = 0.8,fill = rainbow(length(col47_count[,1])))
	for (row in 1:nrow(col47_count)){
		profitCount = profitCount+ CustomerValueData[col47_count[row,1]+1,"col47"]*col47_count[row,2]+ProductValueData[col47_count[row,1]+1,"col47"]*col47_count[row,2]}
	print (profitCount)
	
	pie(col9_count[,2],labels = col9_count[,1], main="Distribution of High status", col = rainbow(length(col9_count[,1])))
	legend("topright", as.character(col9_count[,1]), cex = 0.8,fill = rainbow(length(col9_count[,1])))
	for (row in 1:nrow(col9_count)){
		profitCount = profitCount+ CustomerValueData[col9_count[row,1]+1,"col9"]*col9_count[row,2]+ProductValueData[col9_count[row,1]+1,"col9"]*col9_count[row,2]}
	print (profitCount)
	
	pie(col59_count[,2],labels = col59_count[,1], main="Distribution of Contribution private accident insurance policies", col = rainbow(length(col59_count[,1])))
	legend("topright", as.character(col59_count[,1]), cex = 0.8,fill = rainbow(length(col59_count[,1])))
	for (row in 1:nrow(col59_count)){
		profitCount = profitCount+ CustomerValueData[col59_count[row,1]+1,"col59"]*col59_count[row,2]+ProductValueData[col59_count[row,1]+1,"col59"]*col59_count[row,2]}
	print (profitCount)
	
	pie(col37_count[,2],labels = col37_count[,1], main="Distribution of Income < 30.000", col = rainbow(length(col37_count[,1])))
	legend("topright", as.character(col37_count[,1]), cex = 0.8,fill = rainbow(length(col37_count[,1])))
	for (row in 1:nrow(col37_count)){
		profitCount = profitCount+ CustomerValueData[col37_count[row,1]+1,"col37"]*col37_count[row,2]+ProductValueData[col37_count[row,1]+1,"col37"]*col37_count[row,2]}
	
	print (profitCount)
	levelProfit = c(levelProfit,profitCount)
}

accuracy = confusionMatrix(data=factor(predictions), reference=factor(results$col86), positive="1")$overall["Accuracy"]


levelProfit = levelProfit[2:length(levelProfit)]
TotalProfit = sum(levelProfit)
PercentageDistribution = round(accuracy*levelProfit*100/sum(levelProfit),2)
pie(levelProfit,labels = PercentageDistribution, main="Distribution of Profit for mobile home policies", col = rainbow(length(levelProfit)))
legend("topright", as.character(levels), cex = 0.8,fill = rainbow(length(levels)))





##Changing the data after deciding the strategies and calculating PNL to validate and find distribution again.
for (row in 1:nrow(ValData)){
	if (ValData[row,"col1"]>=23){
		ValData[row,"col1"] = 23}
	if (ValData[row,"col13"]>=4){
		ValData[row,"col13"] = 4}
	if (ValData[row,"col47"]==0){
		ValData[row,"col47"] = 1}
	if (ValData[row,"col37"]>=4){
		ValData[row,"col37"] = 4}}

		
predictions = predict(RandomForestModel,ValData[,selectedColumn])
ResultData = ValData[,selectedColumn]
ResultData$col86 = predictions

levels = unique(predictions)
levelProfit = 0

for (level in levels){
	profitCount = 0
	levelData = ResultData[ResultData[,"col86"]==level,]
	col1_count = count(levelData$col1)
	col13_count = count(levelData$col13)
	col47_count = count(levelData$col47)
	col9_count = count(levelData$col9)
	col59_count = count(levelData$col59)
	col37_count = count(levelData$col37)
	print (profitCount)

	for (row in 1:nrow(col1_count)){
		profitCount = profitCount+ CustomerValueData[col1_count[row,1],"col1"]*col1_count[row,2]+ProductValueData[col1_count[row,1],"col1"]*col1_count[row,2]}
	
	
	for (row in 1:nrow(col13_count)){
		profitCount = profitCount+ CustomerValueData[col13_count[row,1]+1,"col13"]*col13_count[row,2]+ProductValueData[col13_count[row,1]+1,"col13"]*col13_count[row,2]}

	
	for (row in 1:nrow(col47_count)){
		profitCount = profitCount+ CustomerValueData[col47_count[row,1]+1,"col47"]*col47_count[row,2]+ProductValueData[col47_count[row,1]+1,"col47"]*col47_count[row,2]}

	
	for (row in 1:nrow(col9_count)){
		profitCount = profitCount+ CustomerValueData[col9_count[row,1]+1,"col9"]*col9_count[row,2]+ProductValueData[col9_count[row,1]+1,"col9"]*col9_count[row,2]}

	
	for (row in 1:nrow(col59_count)){
		profitCount = profitCount+ CustomerValueData[col59_count[row,1]+1,"col59"]*col59_count[row,2]+ProductValueData[col59_count[row,1]+1,"col59"]*col59_count[row,2]}
	
	for (row in 1:nrow(col37_count)){
		profitCount = profitCount+ CustomerValueData[col37_count[row,1]+1,"col37"]*col37_count[row,2]+ProductValueData[col37_count[row,1]+1,"col37"]*col37_count[row,2]}
	

	levelProfit = c(levelProfit,profitCount)
}



levelProfit = levelProfit[2:length(levelProfit)]
NewTotalProfit = sum(levelProfit)
PercentageDistribution = round(levelProfit*100/sum(levelProfit),2)
pie(levelProfit,labels = PercentageDistribution, main="Distribution of Profit for mobile home policies", col = rainbow(length(levelProfit)))
legend("topright", as.character(levels), cex = 0.8,fill = rainbow(length(levels)))
