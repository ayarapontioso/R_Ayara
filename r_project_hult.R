1+1
install.packages('readxl')
library(readxl)
sheet2 <- read_excel('final.xls',sheet=2)
# sheet2 
sheet3 <- read_excel('final.xls',sheet=3)
sheet4 <- read_excel('final.xls',sheet=4)
sheet5 <- read_excel('final.xls',sheet=5)
summary(sheet3)
barplot(sheet3)
install.packages('ggplot2')
library(ggplot2)
install.packages('corrplot')
install.packages('RColorBrewer')
install.packages('dplyr')
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(data.table)

# Unique Visits
chart1 <- ggplot(sheet2, aes(`Week (2008-2009)`, `Unique Visits`)) +
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Unique Per Week") +
  arrange('Week (2008-2009)')
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart1

# Revenue
chart2 <- ggplot(sheet3, aes(`Week (2008-2009)`, `Revenue`))+ 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Revenue Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2
# Profit
chart3 <- ggplot(sheet3, aes(`Week (2008-2009)`, `Profit`)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  labs(title="Profit Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart3
#Lbs. Sold
chart4 <- ggplot(sheet3, aes(`Week (2008-2009)`, `Lbs. Sold`)) + 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="Revenue Per Week") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart4

#### Question 2
merged_frame <-merge(sheet2,sheet3)
initial_period <- merged_frame[1:14,]
Pre_Promotion <- merged_frame[15:35,]
Promotion_Period <- merged_frame[36:52,]
Post_Promotion_Period <- merged_frame[53:66,]

metric_cal <- function (x) {
  y1 <- mean(x)
  y2 <- median(x)
  y3 <- sd(x)
  y4 <- min(x)
  y5 <- max(x)
  return(c(y1, y2, y3, y4, y5))
}

install.packages("data.table", type="source", dependencies=TRUE)
library(data.table)
## initial period values 
Initial_Period_values <- data.table(Metrics = c("Mean","Median","Std. Dev","Minimum","Maximum"),
                         Visits = metric_cal(initial_period$Visits),
                         Unique_Visits = metric_cal(initial_period$`Unique Visits`),
                         Revenue = metric_cal(initial_period$Revenue),
                         Profit = metric_cal(initial_period$Profit),
                         Lbs_Sold = metric_cal(initial_period$`Lbs. Sold`))
Initial_Period_values
# Pre Promotion Period
Pre_Prom_values <- data.table(Metrics = c("Mean","Median","Std. Dev","Minimum","Maximum"),
                          Visits = metric_cal(Pre_Promotion$Visits),
                          Unique_Visits = metric_cal(Pre_Promotion$`Unique Visits`),
                          Revenue = metric_cal(Pre_Promotion$Revenue),
                          Profit = metric_cal(Pre_Promotion$Profit),
                          Lbs_Sold = metric_cal(Pre_Promotion$`Lbs. Sold`))

Pre_Prom_values
# Promotion Period
Prom_values <- data.table(Metrics = c("Mean","Median","Std. Dev","Minimum","Maximum"),
                      Visits = metric_cal(Promotion_Period$Visits),
                      Unique_Visits = metric_cal(Promotion_Period$`Unique Visits`),
                      Revenue = metric_cal(Promotion_Period$Revenue),
                      Profit = metric_cal(Promotion_Period$Profit),
                      Lbs_Sold = metric_cal(Promotion_Period$`Lbs. Sold`))

Prom_values
## post_promotion 
Post_Prom_values <- data.table(Metrics = c("Mean","Median","Std. Dev","Minimum","Maximum"),
                           Visits = metric_cal(Post_Promotion_Period$Visits),
                           Unique_Visits = metric_cal(Post_Promotion_Period$`Unique Visits`),
                           Revenue = metric_cal(Post_Promotion_Period$Revenue),
                           Profit = metric_cal(Post_Promotion_Period$Profit),
                           Lbs_Sold = metric_cal(Post_Promotion_Period$`Lbs. Sold`))
Post_Prom_values

## corealtion heat maps for diffrent period 
merged_frame_corr<-data.frame(merged_frame$Visits,merged_frame$Profit,merged_frame$`Lbs. Sold`,merged_frame$Inquiries,merged_frame$`Bounce Rate`)
# promotion period 
corr<-cor(merged_frame_corr[36:52,])
corrplot(corr)
# initial peroid 
corr1<-cor(merged_frame_corr[1:14,])
corrplot(corr1)
# Pre_Promotion period 
corr1<-cor(merged_frame_corr[15:35,])
corrplot(corr1)
# post_promo
corr<-cor(merged_frame_corr[53:66,])
corrplot(corr)

# Revenue
Q3_Revenue <- c(mean(initial_period$Revenue),
                mean(Pre_Promotion$Revenue),
                mean(Promotion_Period$Revenue),
                mean(Post_Promotion_Period$Revenue))

# Profit
Q3_Profit <- c(mean(initial_period$Profit),
               mean(Pre_Promotion$Profit),
               mean(Promotion_Period$Profit),
               mean(Post_Promotion_Period$Profit))

# Lbs. Sold
Q3_Lbs_Sold <- c(mean(initial_period$`Lbs. Sold`),
                 mean(Pre_Promotion$`Lbs. Sold`),
                 mean(Promotion_Period$`Lbs. Sold`),
                 mean(Post_Promotion_Period$`Lbs. Sold`))

Q3_Visits <- c(mean(initial_period$`Visits`),
                 mean(Pre_Promotion$`Visits`),
                 mean(Promotion_Period$`Visits`),
                 mean(Post_Promotion_Period$`Visits`))
Q3_Unique_Visits <- c(mean(initial_period$`Unique Visits`),
                       mean(Pre_Promotion$`Unique Visits`),
                       mean(Promotion_Period$`Unique Visits`),
                        mean(Post_Promotion_Period$`Unique Visits`))




# Data Table for comparing each mean value for different periods
Q3_table <- data.table(Metrics = c("Initial", "Pre-Promo", "Promotion", "Post-Promo"),
                       Visits = Q3_Visits,
                       Unique_Visits = Q3_Unique_Visits,
                       Revenue = Q3_Revenue,
                       Profit = Q3_Profit,
                       Lbs_Sold = Q3_Lbs_Sold)

Q3_table


plot.ts(Q3_table)
install.packages('gridExtra')
library(gridExtra)
ss <- tableGrob(Q3_table)
k <- ggplot(Q3_table,aes(x=Q3_table$"Visits",y=Q3_table$"Unique_Visits")) + 
  geom_point()
grid.arrange(k,ss)

### q4 + additional EDA
# box plot for unique visits
boxplot(sheet2$'Unique Visits', main="Unique_Visits", sub=paste("Outlier rows: ",
boxplot.stats(sheet2$'Unique Visits')$out))
## box plot for pageviews 
boxplot(sheet2$'Pageviews', main="Pageviews", sub=paste("Outlier rows: ",
boxplot.stats(sheet2$'Pageviews')$out))
## box plot for visits 
boxplot(sheet2$"Pages/Visit", main="Visits", sub=paste("Outlier rows: ",
boxplot.stats(sheet2$'Pages/Visit')$out))

## relationship b/w variables 
scatter.smooth(sheet3$`Lbs. Sold`,sheet3$'Revenue')
## clearly a strong positive co realtion b/w revenue and lbs sold 
corr<-cor(sheet3$`Lbs. Sold`,sheet3$'Revenue')
corr
## 0.86 very strong positive corealtion 

## scatter plot for revenue versus visits

merged_frame
scatter.smooth(sheet3$`Revenue`,sheet2$'Visits')
## not a strong corealtion 


sheet6 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=7)

chart2 <- ggplot(sheet6, aes(`All Traffic Sources`, `Visits`))+ 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="diffrent traffic sources vs visits") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2

sheet6

sheet7 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=8)

chart2 <- ggplot(sheet7, aes(`Top Ten Referring Sites`, `Visits`))+ 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="referring sites vs visits") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2
sheet7
sheet6

sheet8 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=9)
chart2 <- ggplot(sheet8, aes(`Top Ten Search Engine Sources of Visits`, `Visits`))+ 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="search engine vs visits") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2
sheet8
sheet9 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=10)
sheet9
chart2 <- ggplot(sheet9, aes(`Top Ten Geographic Sources by Sub Continent Region`, `Visits`))+ 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="Top Ten Geographic Sources vs visits") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2
sheet10 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=11)
chart2 <- ggplot(sheet9, aes(`Top Ten Geographic Sources by Sub Continent Region`, `Visits`))+ 
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="Top Ten Geographic Sources by Sub Continent Region vs visits") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart2
install.packages('anytime')
library(flipTime)
AsDate("Jan. 10, 2016")
require(devtools)
install_github("Displayr/flipTime")
library(anytime)
anytime("Jan. 10, 2016")
sheet10 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=2)
sheet10
chart1 <- ggplot(sheet10, aes(`Week`, `Lbs_Sold`)) +
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="poundsold") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart1

sheet10

sheet11 <- read_excel('Web Analytics Case Student Spreadsheet-2 copy.xls',sheet=3)
sheet11
chart1 <- ggplot(sheet11, aes(`week`, `Lbs. Sold`)) +
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="poundsold") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart1
chart1 <- ggplot(sheet11, aes(`week`, `Profit`)) +
  geom_bar(stat="identity", width = 0.5, fill="darkgrey") +
  labs(title="profits vs weeks ") +
  theme(axis.text.x = element_text(angle=90, vjust=0.8))
chart1

head(sheet2)

lmvisits = lm(Visits~`Pages/Visit`+ `Bounce Rate` + `% New Visits`+Pageviews + `Unique Visits`, data = sheet2) #Create the linear regression
summary(lmvisits) #Review the results