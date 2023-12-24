House_Rent_Dataset <- read.csv("D:/PDFA/House_Rent_Dataset.csv")
View(House_Rent_Dataset)
attach(House_Rent_Dataset)

#correlation
cor(House_Rent_Dataset[3:4])
#covariation
cov(House_Rent_Dataset[3:4])
#check data frame
House_Rent_Dataset[3:4]
House_Rent_Dataset[3:4,]


#Analysis
library(ggplot2)
House_Rent_Dataset<-subset(House_Rent_Dataset,Rent<3500000)

#bar plot of count of rent house and furnishing status
ggplot(House_Rent_Dataset, aes(x=Furnishing.Status)) +geom_bar(col="black",fill="blue") +
  labs(x="Furnishing.Status", y = "Number of House sale", title = "Num of House Sale by Furnishing Status")

#barplot of mean_rent_by_status
mean_rent_by_status <- aggregate(Rent ~ Furnishing.Status, data = House_Rent_Dataset, FUN = mean)
View(mean_rent_by_status)
print(mean_rent_by_status)
ggplot(mean_rent_by_status, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
  geom_bar(stat = "identity") +
  labs(x = "Furnishing Status", y = "Mean Rent", fill = "Furnishing Status") +
  ggtitle("Mean Rent by Furnishing Status")+
  scale_fill_manual(values=c("Furnished"="yellow","Semi-Furnished"="lightyellow","Unfurnished"="white"))

#barplot of mean_rent_by_status_city
mean_rent_by_status_city <- aggregate(Rent ~ Furnishing.Status+City, data = House_Rent_Dataset, FUN = mean)
ggplot(mean_rent_by_status_city, aes(x = City, y = Rent, fill = Furnishing.Status)) +
  geom_bar(stat = "identity",position="dodge") +
  labs(x = "City", y = "Mean Rent", fill = "Furnishing Status") +
  ggtitle("Mean Rent by Furnishing Status and City")+
  scale_fill_manual(values=c("Furnished"="darkblue","Semi-Furnished"="blue","Unfurnished"="lightblue"))

#line and point plot of mean_rent_by_status_city
ggplot(mean_rent_by_status_city, aes(x = City, y = Rent,color=Furnishing.Status)) +
  geom_point()+geom_line(color="black")+
  labs(x = "City", y = "Mean Rent") +
  ggtitle("Mean Rent by Furnishing Status and City")+
  theme_minimal()

#histogram of mean_rent_by_factors
mean_rent_by_factors <- aggregate(Rent ~ Furnishing.Status+Tenant.Preferred+City, data = House_Rent_Dataset, FUN = mean)
ggplot(mean_rent_by_factors, aes(x=Rent, y=Furnishing.Status, fill = Tenant.Preferred)) +
  geom_histogram(stat = "summary",fun="mean",position="dodge") +
  facet_grid(rows=vars(City))+
  labs(x = "Mean Rent", y = "Furnishing Status", fill = "Tenant Preferred") +
  ggtitle("Mean Rent by Furnishing Status, City and Tenant Preferred")+
  theme_minimal()+
  scale_fill_manual(values=c("Bachelors"="darkgreen","Bachelors/Family"="green","Family"="grey"))

#histogram of mean_rent_by_status_tenant
mean_rent_by_status_tenant <- aggregate(Rent ~ Furnishing.Status+Tenant.Preferred, data = House_Rent_Dataset, FUN = mean)
ggplot(mean_rent_by_status_tenant, aes(x = Rent, y = Tenant.Preferred, fill = Furnishing.Status)) +
  geom_histogram(stat = "identity",position="dodge") +
  labs(x = "Mean Rent", y = "Tenant Preferred", fill = "Furnishing Status") +
  ggtitle("Mean Rent by Furnishing Status and Tenant Preferred")+
  scale_fill_manual(values=c("Furnished"="darkgreen","Semi-Furnished"="lightgreen","Unfurnished"="green"))
summary(mean_rent_by_status_tenant)
mean(Furnishing.Status)