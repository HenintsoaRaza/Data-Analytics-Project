# Analysis 1
# Find the "average availability over 30 days" of listings per each city. 
listings %>%
  group_by(city) %>%
  summarise(average_availability_30 = mean(availability_30))

# Find the "average revenue of over 30 days" of listings per each city. 
listings %>%
  group_by(city) %>%
  summarise(Mean_revenue_30 = mean(revenue_30))


## Comparing the distribution of estimated availability for the next 30 days of listings
## per each city.
avgAvai30 <- ggplot(listings, aes(city, availability_30, fill=city))
avgAvai30 + geom_boxplot(aes(colour = "red"), outlier.shape = NA) +
  stat_summary(fun="mean") +
  scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") + 
  stat_summary(fun=mean, colour="black", geom="text",
               vjust=-0.8, aes(label=round(..y.., digits=2)))

## Comparing the distribution of estimated revenue for the next 30 days of listings
## per each city.

avgRev30 <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30 + geom_boxplot(aes(colour = "red"),  outlier.shape = NA) +
  stat_summary(fun="mean") +
  scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") +
  stat_summary(fun=mean, colour="black", geom="text",
               vjust=-0.8, aes(label=round(..y.., digits=2)))


# Compare the distribution of estimated revenue for the next 30 days of listings
# per each city & for each house size (# of bedrooms).

avgRev30_city_bed <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30_city_bed + geom_boxplot(aes(colour = bedrooms), lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
  scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") 


# Compare the distribution of estimated revenue for the next 30 days of listings
# per each city & for each room type (room_type).

avgRev30_city_roomType <- ggplot(listings, aes(city, revenue_30, fill=city))

avgRev30_city_roomType + geom_boxplot(aes(colour = room_type),  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
  scale_y_continuous(limits = quantile(listings$revenue_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") 


# -----------------------------------------------------------------------------------------------------------------

# Analysis 2 : 
# What is the proportion of each room type?
proportion_RoomType <- ggplot(listings, aes(room_type))
proportion_RoomType + geom_bar(color="blue", width=0.8 , fill="darkgreen") +
  geom_text(aes(label=stat(count)), stat='count', vjust = -0.3) + 
  ylab("Percentage") 

proportion_RoomType2<- ggplot(listings, aes(room_type))
proportion_RoomType2 + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) + theme(legend.position = "none")

# ylab("Percentage ") + scale_fill_discrete(name = "room_type")


# What is the proportion of each house size (# of bedroom)?
proportion_bedrooms<- ggplot(listings, aes(bedrooms))
proportion_bedrooms + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),4)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) + 
  ylab("Percentage ") + theme(legend.position = "none")


# What is the proportion of each neighborhood?
proportion_neighborhood<- ggplot(listings, aes(neighbourhood_cleansed))
proportion_neighborhood + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(..x..)), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) + 
  
  #scale_y_discrete(guide = guide_axis(check.overlap = TRUE))+
  ylab("Percentage ") + theme(legend.position = "none") + coord_flip()

# pas beau du tout
# du coup on va plot les top 30 neghbourhood !


# What is the average availability over the next 30 days for each room type /
# house size / neighborhood?
listings %>%
  group_by(city, room_type, bedrooms, neighbourhood_cleansed) %>%
  summarise(average_availability_30 = mean(availability_30))


# What is the average revenue over the next 30 days for each room type /
# house size / neighborhood?
listings %>%
  group_by(city, room_type, bedrooms, neighbourhood_cleansed) %>%
  summarise(average_revenue_30 = mean(revenue_30))


# What is the distribution of availability over the next 30 days for each room type
# house size / neighborhood?

# on est pas censé plot toutes les dimensiosns dans un mm plot
# mais plutot comme l'analysis 1 : 
# - distribution of availability for the next 30 days per each city and room type
# - distribution of availability for the next 30 days per each city and house size
# - distribution of availability for the next 30 days per each city and neighborhood
avgAvai30_city_roomType <- ggplot(listings, aes(city, availability_30, fill=city))
avgAvai30_city_roomType + geom_boxplot(aes(colour = room_type),  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
  scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") 

avgAvai30_city_bedrooms <- ggplot(listings, aes(city, availability_30, fill=city))
avgAvai30_city_bedrooms + geom_boxplot(aes(colour = bedrooms),  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
  scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") 

avgAvai30_city_neighbourhood_cleansed <- ggplot(listings, aes(city, availability_30, fill=city))
avgAvai30_city_neighbourhood_cleansed + geom_boxplot(aes(colour = neighbourhood_cleansed),  lwd = 0.8, position = position_dodge(0.9), na.rm = T) +
  scale_y_continuous(limits = quantile(listings$availability_30, c(0.1, 0.9), na.rm = T)) +
  scale_fill_brewer(palette="Set1") 
# faut faire un top 10 maybe 
# What is the distribution of revenue over the next 30 days for each room type /
# house size / neighborhood?
