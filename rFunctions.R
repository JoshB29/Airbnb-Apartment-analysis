#For a ROC Curve (blah), given a False positive threshold (num) and the number of positives (num_pos), tell me on the that ROC curve the correspondoing true positive rate
#example: Roc_FPR(.01,perf_ELRON_no_interactions_KRB.R,250)
#This function reports back the FPR threshold used (which will be close to the threshold you input), the Trupe postive rate, the number of true positive found at this threshold and the number of false pistives at this threshold
Roc_FPR <- function(num,blah,num_pos)
{
	closest=1
	guess=abs(blah@x.values[[1]][1] -num)
	for (i in 2:length(blah@x.values[[1]])){
	guess2=abs(blah@x.values[[1]][[i]]-num)
		if (guess2<guess){
			guess=guess2
			closest=i
		}

	}
	#print(closest)
	#print(guess)
	print(blah@x.values[[1]][closest])
	print(blah@y.values[[1]][closest])


	print(blah@y.values[[1]][closest]*num_pos)
	print(blah@x.values[[1]][closest]*(19548-num_pos))
}

#Remove NA's from a dataframe row by row, and replace with mean of Row
replaceNARowMean <- function (df)
{
	for (i in 1:nrow(df)){
		my_mean=mean(as.numeric(df_luad[1,]),na.rm=TRUE)
     	df[i,is.na(df[i,])] <- my_mean
     	message(i)
	}
	return(df)
}

#Plot many ROC Curves given the ROC curves, the colors to plot with, and the xlim
plotROC_curves <- function (ROC_curves,colors,xlim)
{
	plot(ROC_curves[1],col=colors[1],xlim=xlim)
	for (i in 2:length(ROC_curves))
	{
		par(new=T)
		plot(ROC_curves[i],col=colors[i],xlim=c(0,1))
	}
}

#An alias for View for Rstudio, since less is in bash
less <- function (blah)
{
	View(blah)
}

#Write a description of the figure in the margins in a small font
description <-function (blah)
{
	mtext(blah,cex=.3,outer =TRUE)
}
cd <- function (blah)
{
	setwd(blah)
	
}
#Given a named vector and a named gold standard vector (names is the same as context), context must be charachter, determine the best split (starting from the highest score) and the information gained at the split
informationGain <-function(vector,gold_standard){
	max_igain=0
	vector=sort(vector,decreasing =T)
	thresholds=unique(sort(vector,decreasing = T))
	negative=setdiff(names(vector),gold_standard)
	length_gold=length(gold_standard)
	length_negative=length(negative)
	names(negative)=negative
	length_minus_1=length(thresholds)-1
	best_split_point=0;
	best_gold_above=0
	best_neg_above=0;
	best_gold_below=0;
	best_neg_below=0;
	#This is really baseline entropy
	base_line_information=(length_gold/(length_gold+length_negative))*log2((length_gold/(length_gold+length_negative)))+(length_negative/(length_negative+length_gold))*log2((length_negative/(length_negative+length_gold)))
	#message("base_line_information: ",base_line_information)
		for (i in 1:length_minus_1) {
			#get threshold
			threshold=thresholds[i]
			#num gold above threshold
			num_gold_above=length(gold_standard[vector[gold_standard]>=threshold])
			num_gold_below=length(gold_standard[vector[gold_standard]<threshold])
			num_neg_above=length(negative[vector[negative]>=threshold])
			num_neg_below=length(negative[vector[negative]<threshold])
			#above_ratio=num_gold_above/num_neg_above
			#below_ratio=num_gold_below/num_neg_below
			#print("Above Ratio ",above_ratio)
			#print("Below Ratio ",below_ratio)
			#add .000000000001 so that you dont devide by zero
			prob_gold_above=(num_gold_above/(num_gold_above+num_neg_above))+.000000000001
			prob_neg_above=(num_neg_above/(num_gold_above+num_neg_above))+.000000000001
			prob_gold_below=(num_gold_below)/(num_gold_below+num_neg_below)+.000000000001
			prob_neg_below=(num_neg_below)/(num_gold_below+num_neg_below)+.000000000001

			#This is really the information gain
			my_igain=base_line_information-((prob_gold_above*log2(prob_gold_above) + prob_neg_above*log2(prob_neg_above)+prob_gold_below*log2(prob_gold_below)+prob_neg_below*log2(prob_neg_below))/2)
			if(exists("my_igain")=="FALSE"){my_igain=-100} #if it does not exist, set information to very low negative
			if(is.nan(my_igain)){my_igain=-100} #if it nan, set information to very low negative
			#message(my_igain)
			#message(my_igain)
			#print(my_entropy)
			if (my_igain>=max_igain){
				max_igain=my_igain
				best_split_point=thresholds[i]
				best_gold_above=num_gold_above
				best_neg_above=num_neg_above
				best_gold_below=num_gold_below
				best_neg_below=num_neg_below

			}
		}
		
			message("Best Gold ABove: ",best_gold_above)
			message("Best Neg above: ",best_neg_above)
			message("Best Gold Below: ",best_gold_below)
			message("Best Neg Below: ", best_neg_below)
			message("Best Split Point: ", best_split_point)
			message("Max igain: ", max_igain)
		return(max_igain)
}
#Integrate pvalues using fisher's method, given a list of pvalue as a list. Make sure you exclude NAs yourelf first:
fisherIntegration  <- function (vector){
	my_length=length(vector)
	deg_free=my_length*2
	y=-2*sum(log(vector))
	p.val <- 1-pchisq(y, df = deg_free);
	p.val=as.numeric(p.val);
	return(p.val)
}
#Integrate pvalues using stouffer's method, given a list of pvalue as a list. Make sure you exclude NAs yourelf first:
#Stouffers method takes a z-score, note the raw pvalue, so remember to convert to z-score first
stoufferIntegration <- function (vector,lower.tail=NULL){
	if(is.null(lower.tail)) {lower.tail=TRUE}
	my_length=length(vector)
	#zscores=scale(vector)
	p.val=sum(vector)/sqrt(my_length)
	p.val=pnorm(p.val,lower.tail=lower.tail)
	return(p.val)
}
#list_1 is a list, num_1 is how many fold validation you want to do (e.g. 10 fold validation)
#Returns 2 lists of lists, where the first one is a lists of all the training sets and the second one is the list of all the testing sets. These lists have the same length obviously
#A particular training set list can be retrived as   as: training_and_testing_folds[1][1][[1]][[1]]
#A particular testing set can be retrieved as: training_and_testing_folds[2][1][[1]][[1]]
kfolds <-function (list_1,num_1){
	list_1=sample(list_1)
	training_list_of_lists=list()
	testing_lists_of_lists=list()
	folds <- cut(seq(1,length(list_1)),breaks=num_1,labels=FALSE)
	folds_indices=unique(sort(folds))
	training_matrix=combn(folds_indices,num_1-1)
	for (i in 1:ncol(training_matrix)){
		training_folds=training_matrix[,i]
		testing_fold=setdiff(folds_indices,training_matrix[,i])
		training_folds_2=list_1[folds %in% training_folds]
		testing_fold_2=list_1[folds %in% testing_fold]
		training_list_of_lists[[i]]=training_folds_2
		testing_lists_of_lists[[i]]=testing_fold_2

	}

	return(list(training_list_of_lists,testing_lists_of_lists))

}



