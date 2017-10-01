

#This script performs analysis of airbnb listings in NYC
#get utility functions
df=read.csv("/Users/joshuabroyde/Documents/Airbnb_stuff/data/listings_05_02_2017.csv")
source("~/Scripts/Utils/rFunctions.R")
source("~/Scripts/Utils/rFunctions_2.R")

rownames(df)=df$id
#Convert dollars to numbers
df$price=dollarsTonumber(df$price)
#Get raw occupancy for the next 30 days. This is simply 30 minus the number of days listed divided by 30, (i.e. how many days within the next 30 are not availible)
df$raw_occupancy=(30-df$availability_30)/30
df$occupied_30=30-df$availability_30
just_manhattan_listings=df[which(df$neighbourhood_group_cleansed=="Manhattan"),]
#Calculate normalized occupancy for next 30 days. In this calculation, the assumption is that no one makes reservations more than 2 months in advance, and that the owner's schedule 60-90 days from now is the same as 1-30 days from now. Thus, we assume that all availve days 60-90 days from now are all the maximum days availible. 
total_occupied_90=90-df$availability_90
total_occupied_60=60-df$availability_60
total_occupied_60_through_90=total_occupied_90-total_occupied_60
df$total_occupied_60_through_90=total_occupied_60_through_90
df$total_availible_60_through_90=30-df$total_occupied_60_through_90
df$normalized_occupancy_30=(30-df$availability_30)/(df$total_availible_60_through_90)

#Normalize differently. Assume that number of days availible in first 30 days is same as proportion of days availilbe in 365
df$occupancy_rate_365_normalized=df$occupied_30/((df$availability_365/365)*30)

print(df$occupancy_rate_365_normalized)
#Just listings with 3 bedrooms or more in manhattan with whole apartment or house availible:
just_manhttan_listings_just_Entire_home_apt=just_manhattan_listings[which(just_manhattan_listings$room_type=="Entire home/apt"),]
three_or_more_bedrooms=just_manhttan_listings_just_Entire_home_apt[which(just_manhttan_listings_just_Entire_home_apt$bedrooms>=3),]
#convert dollars to numbers
x=factor(c(as.character(three_or_more_bedrooms$price)))
three_or_more_bedrooms$price=as.numeric(sub('\\$','',as.character(x)))
#pdf("Histogram_of_prices_3_bed_or_more.pdf",width=5,height=5)
hist(three_or_more_bedrooms$price)

#Just listings with 5 bedrooms or more in manhattan with whole apartment or house availible:
five_or_more_bedrooms=just_manhttan_listings_just_Entire_home_apt[which(just_manhttan_listings_just_Entire_home_apt$bedrooms>=5),]
#convert dollars to numbers
x=factor(c(as.character(five_or_more_bedrooms$price)))
five_or_more_bedrooms$price=as.numeric(sub('\\$','',as.character(x)))
hist(five_or_more_bedrooms$price)


#Just manhattan listings three or more bedrooms with private room
just_manhattan_listings_private_room=just_manhattan_listings[which(just_manhattan_listings$room_type=="Private room"),]
just_manhattan_listings_private_room_3_or_more=just_manhattan_listings_private_room[which(just_manhattan_listings_private_room$bedrooms>=3),]
just_manhattan_listings_private_room_3_or_more$price=dollarsTonumber(just_manhattan_listings_private_room_3_or_more$price)
hist(just_manhattan_listings_private_room_3_or_more$price)
#Brooklyn listings with 3 bedrooms or more with whole apartments or house

just_brooklyn_listings=df[which(df$neighbourhood_group_cleansed=="Brooklyn"),]
just_brooklyn_listings_just_Entire_home_apt=just_brooklyn_listings[which(just_brooklyn_listings$room_type=="Entire home/apt"),]
three_or_more_bedrooms_brooklyn=just_brooklyn_listings_just_Entire_home_apt[which(just_brooklyn_listings_just_Entire_home_apt$bedrooms>=3),]
three_or_more_bedrooms_brooklyn$price=dollarsTonumber(three_or_more_bedrooms_brooklyn$price)
hist(three_or_more_bedrooms_brooklyn$price)

#Plot occupancy vs price for high and low priced 
over_500=just_manhattan_listings[which(just_manhattan_listings$price>=500),] 
less_than_200=just_manhattan_listings[which(just_manhattan_listings$price<200),]
less_than_100=just_manhattan_listings[which(just_manhattan_listings$price<100),]
mean(less_than_200$raw_occupancy) #this mean is .80
mean(over_500$raw_occupancy)


#Calculate the 30 day occupancy rate assuming that there are no bookings within the last 30 days (i.e. no one books more than 60 days in advance)
#This means that within the availability_90 column we assume that all occupied days are occupied because the owner does not want them on the markent