#Renaming the outcome column

colnames(bank)[21] <- "Success"

#Data Wrangling

#Checking data
bank_NA_count <- apply(is.na(bank), 2, sum)
bank_NA_count

#summary statistics
dim(bank)
summary(bank)



#Age

# Overlaid histograms
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
breaks=c(20:90)
p <- ggplot(bank, aes(x=age, color=Success)) +
  ggtitle("Age Distribution")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 2,breaks=breaks)



#Adding the medina dashed line
p+ geom_vline(aes(xintercept=mean(age)),
              color="blue", linetype="dashed", size=1)



#marital
plot(bank$marital)
bank$marital <- as.numeric(bank$marital)
#ggplot

theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
mr <- ggplot(bank, aes(x=marital, color=Success)) +
  ggtitle("Success per Marital Status")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
mr + scale_x_discrete(name ="Marital Status", 
                      limits=c("Divorced","Married","Signle","Unknown"))

#job
plot(bank$job)
bank$job <- as.numeric(bank$job)
#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
jb <- ggplot(bank, aes(x=job, color=Success)) +
  ggtitle("Success per Job Title")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
jb + scale_x_discrete(name ="Job Title", 
                      limits=c("Adimn","Blue-Collar","Enterpreneur","Housemaid","Management","Retired","Self-employed","Services", "Student", "Tehcnician", "Unemployed", "Unknown" ))



#education
plot(bank$education)
bank$education <- as.numeric(bank$education)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
ed <- ggplot(bank, aes(x=education, color=Success)) +
  ggtitle("Success per Educational Level")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
ed + scale_x_discrete(name ="Education", 
                      limits=c("basic 4y","basic 6y","basic 9y","High School","Illiterate","Course","University","Unknown"))


#default
levels(bank$default)
plot(bank$default)
bank$default <- as.numeric(bank$default)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
df <- ggplot(bank, aes(x=default, color=Success)) +
  ggtitle("Success per Payment Default")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
df + scale_x_discrete(name ="Payment Default", 
                      limits=c("No","Unknown","Yes"))

#housing
levels(bank$housing)
plot(bank$housing)
bank$housing <- as.numeric(bank$housing)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
hs <- ggplot(bank, aes(x=housing, color=Success)) +
  ggtitle("House Loan and Success")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
hs + scale_x_discrete(name ="House Loan", 
                      limits=c("No","Unknown","Yes"))

#loan
levels(bank$loan)
plot(bank$loan)
bank$loan <- as.numeric(bank$loan)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
ln <- ggplot(bank, aes(x=loan, color=Success)) +
  ggtitle("Personal Loan and Success")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
ln + scale_x_discrete(name ="Personal Loan", 
                      limits=c("No","Unknown","Yes"))


#contact
levels(bank$contact)
plot(bank$contact)
bank$contact <- as.numeric(bank$contact)


#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
ct <- ggplot(bank, aes(x=contact, color=Success)) +
  ggtitle("Way of Contact and Success")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
ct + scale_x_discrete(name ="Way of Contact", 
                      limits=c("Cellular","Telephone"))


#month
levels(bank$month)
plot(bank$month)
bank$month <- as.numeric(bank$month)
table(bank$month)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
mth <- ggplot(bank, aes(x=month, color=Success)) +
  ggtitle("Month and Success")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
mth + scale_x_discrete(name ="Month", 
                       limits=c("Apr","Aug","Dec","Jul","Jun","Mar","May","Nov","Oct","Sep"))


#day_of_week
levels(bank$day_of_week)
plot(bank$day_of_week)
bank$day_of_week <- as.numeric(bank$day_of_week)

#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
dd <- ggplot(bank, aes(x=day_of_week, color=Success)) +
  ggtitle("Days of the week and Success")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
dd + scale_x_discrete(name ="Days of the week", 
                      limits=c("Fri","Mon","Thu","Tue","Wed"))

#Duration remains unclear if it will be used

#campaing

#campaign
levels(bank$campaign)
hist(bank$campaign)
table(bank$campaign)
boxplot(bank$campaign)
bank$campaign <- as.numeric(bank$campaign)




cp <- qplot(bank$campaign, geom="histogram",binwidth = 0.5) 
cp + scale_x_discrete(name ="Days of the week", 
                      limits=c(1:30))





theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
dd <- ggplot(bank, aes(x=campaign, color=Success)) +
  ggtitle("Number of contacts during this campaign")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
dd + scale_x_discrete(name ="Number of contacts", 
                      limits=c(1:30))


#Getting a table to check the frequencies
campaign <- data.frame(table(bank$campaign))


#pdays
hist(bank$pdays)
pdays <- data.frame(table(bank$pdays))
days <- filter(bank, bank$pdays > 1 & bank$pdays < 400)

#previous
hist(bank$previous)
table(bank$previous)
boxplot(bank$previous)

days <- filter(bank, bank$previous > 1)


#poutcome
levels(bank$poutcome)
table(bank$poutcome)
plot(bank$poutcome)
bank$poutcome <- as.numeric(bank$poutcome)
#ggplot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
po <- ggplot(bank, aes(x=poutcome, color=Success)) +
  ggtitle("Outcome of the previous marketing campaign")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
po + scale_x_discrete(name ="Days of the week", 
                      limits=c("Failure","Non existent","Success"))

#emp.var.rate
hist(bank$emp.var.rate)

theme_update(plot.title = element_text(hjust = 0.5))
ggplot() + ggtitle("Default is now set to centered")
vr <- ggplot(bank, aes(x=emp.var.rate, color=Success)) +
  ggtitle("Employment variation rate")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 1)
vr

#cons.price.idx
hist(bank$cons.price.idx)

pi <- ggplot(bank, aes(x=cons.price.idx, color=Success)) +
  ggtitle("Consumer price index")+
  geom_histogram(fill="black", alpha=0.5, position="identity",binwidth = 0.7)
pi

#Success
plot(bank$Success)
bank$Success <- as.numeric(bank$Success)
table(v)
bank$Success <- as.numeric(bank$Success)

#Correlation
library(ggcorrplot)
corr <- round(cor(bank), 1)
p.mat <- cor_pmat(bank)
ggcorrplot(corr, method = "circle")

library(corrplot)
corrplot(corr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




#not usefull
library("PerformanceAnalytics")
chart.Correlation(corr, histogram=TRUE, pch=19)

#best vis
library(corrplot)
corrplot(corr, method="circle")

#pca
PCAs_Variance <- prcomp(bank, center = T, scale. = T)
pc__var <- pc_price$sdev^2
pc__var
pc__PEV <- pc__var / sum(pc__var)
pc__PEV
plot(PCAs_Variance)
plot(
  cumsum(pc__PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'Cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
biplot(
  PCAs_Variance,
  scale = 0,
  col = c('grey40','orange')
)
biplot(PCAs_Variance, expand=5, xlim=c(-0.3, 0.1), ylim=c(-0.1, 0.1))
