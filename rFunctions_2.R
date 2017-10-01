#Convert dollars amounts to numbers 
#list_1 is a list of amounts in dollars list_1=c($500,$600, etc)
dollarsTonumber <- function(list_1){
	x=factor(c(as.character(list_1)))
	x=as.numeric(sub('\\$','',as.character(x)))
	return(x)
}