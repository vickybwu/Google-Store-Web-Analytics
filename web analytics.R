##### Web Analytics Mockup Data (Source: Google store Dec. 2018 data)

library(gdata)
traffic_source=read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
                  sheet=1)
traffic_source
str(traffic_source)

#convert percenategs into numerics by deleting "%" then devide by 100
traffic_source$Bounce.Rate <- as.numeric(sub("%","",traffic_source$Bounce.Rate))/100
traffic_source$Ecommerce.Conversion.Rate <- as.numeric(sub("%","",traffic_source$Ecommerce.Conversion.Rate))/100
str(traffic_source)

## Users by traffic source
my_vector=traffic_source$Users[1:7]
names(my_vector)=traffic_source$Users[1:7]
User_plot<-barplot(traffic_source$Users[1:7], names.arg=traffic_source$Default.Channel.Grouping[1:7],
                   main="website traffic by channel", ylab="Users per month", 
                   ylim=c(0,10000), col="blue")
text(x=User_plot, y=traffic_source$Users[1:7], labels=names(my_vector), pos=3, col="red")
text(x=User_plot, y=traffic_source$Users[1:7], labels=names(my_vector)/*100, pos=3, col="yellow")

### Users vs New Users by traffic source
counts <- t(as.matrix(traffic_source[,2:3], row.names=traffic_source$Default.Channel.Grouping))
counts
Com_plot <- barplot(counts, beside=TRUE, names.arg = traffic_source$Default.Channel.Grouping,
                    col=c("blue", "green"), main="Users vs New Users by channel",
                    ylab="traffic per month", ylim=c(0,18000))
my_vector_2 <- c(8876,7490,2646,2201,2592,1418,1309,1003,152,135,63,26,59,44,15697,12317)
text(x=Com_plot, y=counts, labels=my_vector_2, pos=3, col="red")
cols=c("blue", "green")
legend(1,15000, legend=c("Users", "New Users"), fill=cols)

####Bounce Rate Pie Chart
install.packages("plotrix")
library(plotrix)
slices <- traffic_source$Bounce.Rate[1:7]
slices
lables_pie<- traffic_source$Default.Channel.Grouping[1:7]
br_pie<-pie3D(slices, labels=sprintf("%s = %.0f%%", lables_pie,
                                     100*traffic_source$Bounce.Rate[1:7]), col=c("mistyrose1","mistyrose2", "mistyrose3", "mistyrose4","moccasin",
                                     "lightsteelblue2","lightsteelblue3"),
    main="Bounce Rate of Each Traffic Source Channel", explode=0.1,cex=0.5)

#####Transactions and Revenues by channels
qqplot(traffic_source$Transactions[1:7], traffic_source$Revenue[1:7], ylim=c(0,2000),xlim=c(0,30),
       xlab="Number of Transactions", ylab="Revenues", main="Transactions and Revenus by channels", add.line=TRUE,
       col=c("red","yellow","black","green", "purple","orange","blue"))
text(x=traffic_source$Transactions[1:7], y=traffic_source$Revenue[1:7], 
     labels=lables_pie, pos=3, col="red")
qqline(x=traffic_source$Transactions[1:7], y=traffic_source$Revenue[1:7])

#(Method 2)
Revenue_plot<-barplot(traffic_source$Revenue[1:7], names.arg=traffic_source$Default.Channel.Grouping[1:7],
                   main="Revenues by channels", ylab="Revenue per month (US dollars)", 
                   ylim=c(0,2000), col="lavender",cex.names=0.5)
text(x=Revenue_plot, y=traffic_source$Revenue[1:7], labels=traffic_source$Revenue[1:7],
     pos=3, col="maroon")

#####Pages per session
pps_traffic_source <- qqplot(traffic_source$Default.Channel.Grouping[1:7],
                             traffic_source$Pages...Session[1:7])
qqline(x=c(12,3,4,5,6,7,8),y=traffic_source$Pages...Session[1:7], col="red")
text(x=traffic_source$Default.Channel.Grouping[1:7],y=traffic_source$Pages...Session[1:7],
     labels = traffic_source$Pages...Session[1:7], col='red', pos=2)
qqnorm(traffic_source$Pages...Session, ylab="pages per session", main="Pages per session data distribution")
qqline(traffic_source$Pages...Session, col="red")
##################User Age####################
user_age <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
         sheet=3)
user_age
str(user_age)

#Users by age
slices_2 <- user_age$Users[1:6]
labels_2 <- user_age$Age[1:6]
pie(slices_2, labels=sprintf("%s = %.0f%%",labels_2,100*slices_2/7600), col=c("peachpuff1","peachpuff2","peachpuff3","peachpuff4","peru","pink"),
    main="Users by age")

##################User Gender####################
user_gender <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
                     sheet=4)
user_gender
str(user_gender)
Gender_plot <- barplot(user_gender$Users[1:2], names.arg=user_gender$Gender[1:2], col=c("blue1", "pink2"), ylim=c(0,6000)
                       ,main="Users by gender", ylab="Number of users/visitors")
text(Gender_plot, y=user_gender$Users[1:2], labels=user_gender$Users[1:2],
     col="red", pos=3)

##################Device Usage####################
device_usage <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
                         sheet=5)
str(device_usage)
as.matrix(device_usage[,1:3])
slices_3 <- device_usage$Users[1:3]
labels_users <- device_usage$Device.Category[1:3]
device_pie<-pie3D(slices_3, labels=sprintf("%s = %.0f%%", labels_users,
                                     100*slices_3/14981), col=c("grey", "blue2","white"),
                  main="Traffic by devices")

#################Referral website#############
referral <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", sheet=2)
str(referral)
referral$Bounce.Rate <- as.numeric(sub("%","",referral$Bounce.Rate))/100
ref_bounce_rate
qqplot(x=referral$Source...Medium[1:10], y=ref_bounce_rate)
library(car)
scatterplot(referral$Bounce.Rate~referral$Pages...Session, data=referral[1:10],
            smooth=FALSE,xlab="pages per session", ylab="bounce rate", main="Bounce rate ~ Pages per session",
            col=heat.colors(11))
?scatterplot()

###############Mobile Devices################
mobile <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
                   sheet=6)
str(mobile)
counts_mobile <- t(as.matrix(mobile[,2:3], row.names=mobile$Mobile.Device.Info))
counts
Com_plot_mobile <- barplot(counts_mobile, beside=TRUE, names.arg = mobile$Mobile.Device.Info,
                    col=c("lightcyan2", "lemonchiffon"), main="Users vs New Users by mobile devices",
                    ylab="users per month",ylim=c(0,4500), cex.names = 0.5)
my_vector_3 <- c(8876,7490, 2646,2201, 2592,1418, 1309,1003,  152,135,63,26,59,44,15697,12317)
text(x=Com_plot_mobile, y=counts_mobile, labels=my_vector_3, pos=3, col="lightpink3")

##############Geo##############
geo <- read.xls("/Users/VickyWu/Desktop/R datasets/Web Analytics Mockup Raw Data .xlsx", 
                sheet=7)
str(geo)
library(maptools)
data(wrld_simpl)
Countries = as.character(geo$Country)[1:10]
Countries
myCountries = wrld_simpl@data$NAME %in% Countries
plot(wrld_simpl, col = c("olivedrab3", "red")[myCountries+1], main="User Location")
