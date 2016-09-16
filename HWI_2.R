#install.packages("HSAUR3")
#install.packages("RODBC")
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)

#####################################################################################
################################ HOMEWORK 1 #########################################
#####################################################################################

# Richard Blankenhorn

################################### Q1 ##############################################

data("Forbes2000", package="HSAUR3")
summary(Forbes2000)
head(Forbes2000, n=10)

#1.1 Calculate the median profit for US companies and for UK, France & Germany

# Get Companies by Country
US_Comp <- subset(Forbes2000, country=="United States")
UK_Comp <- subset(Forbes2000, country=="United Kingdom")
French_Comp <- subset(Forbes2000, country=="France")
German_Comp <- subset(Forbes2000, country=="Germany")

# Calculate Median Profits
US_Profit_Med <- median(US_Comp$profits, na.rm=TRUE)
UK_Profit_Med <- median(UK_Comp$profits, na.rm=TRUE)
French_Profit_Med <- median(French_Comp$profits, na.rm=TRUE)
German_Profit_Med <- median(German_Comp$profits, na.rm=TRUE)

# Print Medians by Country
US_Profit_Med
UK_Profit_Med
French_Profit_Med
German_Profit_Med

#1.2 Find all German companies with a negative profit

German_Neg_Profit <- subset(German_Comp, profits<0, c("name"))
German_Neg_Profit

#1.3 To which business category do most of the Bermuda island comps belong?

Bermuda_Comp <- subset(Forbes2000, country=="Bermuda")
Bermuda_Cat <- subset(as.data.frame(table(Bermuda_Comp$category)), Freq>0)
colnames(Bermuda_Cat) <- c("Category", "Frequency")
Maximum <- subset(Bermuda_Cat, Frequency==max(Bermuda_Cat$Frequency))
Maximum # Insurance

#1.4 For the 50 companies in the Forbes DSN with highest profits, plot sales
# against assets, labeling each point with the appropriate country name abbreviated

Top_50_Profits <- arrange(Forbes2000, desc(profits))[1:50,]
Co_Abrev_Names <- abbreviate(Top_50_Profits$name)

plot <- ggplot(Top_50_Profits, aes(x=profits, y=assets)) + geom_point()
plot + geom_text(label=Co_Abrev_Names, size=2, vjust=2)

#1.5 Find the avg value of sales for companies in each country and find the number
# of companies in each country with profits above 5 billion US Dollars

# Average Sales by Country
Avg_Sales_By_Country <- aggregate(sales ~ country, Forbes2000, mean)
colnames(Avg_Sales_By_Country) <- c("Country", "Avg_Sales")
Avg_Sales_By_Country

# Function to calculate the number of companies in each country with profits above
# 5 billion US dollars
profits_by_country <- function(df, li)
{
  count <- 0
  for (i in li) {
    t <- subset(df, country==i & profits>5)
    if (nrow(t) > 0) {
      if (count == 0) {
        new_df <- data.frame(Country=i, Total=nrow(t))
        count <- count + 1 }
      else {
        frame <- data.frame(Country=i, Total=nrow(t))
        new_df <- rbind(new_df, frame)
        count <- count + 1 } } }
  return(new_df)
}

# Print number of companies in each country with profits above 5 billion US dollars
countries <- unique(Forbes2000$country)
print(profits_by_country(Forbes2000, countries))

#################################### Q2 ###############################################

# 2.1 - The aim of the household survey was to investigate how the division of household
# expenditure between the four commodity groups depends on total expenditure and to find
# out whether this relationship differs for men and women.

data("household", package = "HSAUR3")
head(household)
summary(household)

# Graph expenditures for each commodity group by gender
house_spending <- melt(household, id.vars = "gender")
house_spending
ggplot(house_spending, aes(x=factor(variable),y=value,fill=factor(gender))) + geom_bar(stat='identity') +
  labs(title="Commodity Group Expenditures by Gender", x="Commodity Group",y="Expenditure")

################################### Q3 ################################################

# Use R to calculate the median absolute deviation on the data set chickwts.
# (1.4826*median|x-µ|)Verify using MAD function

data("chickwts")
head("chickwts")

d <- unlist(lapply(list(chickwts$weight), MAD, m=median(chickwts$weight)))
MAD <- 1.4826*median(d)

median(d)
MAD <- 1.4826*median(d)
MAD
mad(chickwts$weight)

################################### Q4 ###############################################

# Using the stat.x77 data matrix, obtain a side by side boxplots of the per capita
# income variable for the nine different divisions defined by the variable
# state.division

data(state)
head(state.x77)

State_Info <- as.data.frame(state.x77)
Divs <- as.data.frame(state.division)

State_and_Div <- cbind(State_Info, Divs)
unique(state.division)

ggplot(State_and_Div, aes(x=factor(state.division), y=Income)) + geom_boxplot() + scale_x_discrete(label=abbreviate) +
  labs(x="State Division")

##################################### Q5 ############################################

# Using the state.x77 data matrix, find the state with the minimum per capita income
# in the New England region as defined by the factor state.division.

New_England_States <- as.data.frame(subset(state.x77, state.division=="New England"))
New_England_State_Names <- as.data.frame(subset(state.name, state.division=="New England"))
All_New_England <- cbind(New_England_States, New_England_State_Names)

Lowest_Income <- rownames(subset(cbind(New_England_States, New_England_State_Names), Income==min(New_England_States$Income)))
Lowest_Income

################################### Q6 ##############################################

# Form a matrix object named mycars from the variables Min.Price, Max.Price, MPG.City
# MPG.highway, EngineSize, Length and Weight of the data frame Cars93.

data("Cars93",package = "MASS")
head(Cars93)

# vector of means named Cars.Means
new_DF <- Cars93[,c(4,6,7,8,12,19,21)]
mycars <- data.matrix(new_DF)
head(mycars)
Cars.Means <- apply(mycars, 2, mean)
Cars.Means

# vector of std errors named Cars.Std.Errors
std_error <- function(x) sd(x)/sqrt(length(x))
Cars.Std.Errors <- apply(mycars,2,std_error)
Cars.Std.Errors

# matrix with 2 rows containing the lower and upper limits of 99% confidence
# intervals for the means named Cars.CI.99

me_function <- function(y) qnorm(.99)*(y)
me <- sapply(Cars.Std.Errors, me_function)
me

lower <- function(y,x) x - y
upper <- function(y,x) x + y
lower_bounds <- mapply(lower, y=me, x=Cars.Means)
upper_bounds <- mapply(upper, y=me, x=Cars.Means)
lower_bounds
Cars.CI.99 <- matrix(c(lower_bounds, upper_bounds), ncol=2)
rownames(Cars.CI.99) <- c("Min.Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize", "Length", "Width")
colnames(Cars.CI.99) <- c("Lower.Bound", "Upper.Bound")
Cars.CI.99

cars.stats <- list(Cars.Means, Cars.Std.Errors, Cars.CI.99)
cars.stats

############################### Q7 ##############################################

# Use the apply() function on the 3-D array iris3 to compute the following:

data("iris3")
summary(iris3)

b <- c("Sepal L.", "Sepal W.", "Petal L.", "Petal W.")
Setosa <- matrix(iris3[, b,"Setosa"], ncol=4)
Versicolor <- matrix(iris3[, b, "Versicolor"], ncol=4)
Virginica <- matrix(iris3[, b, "Virginica"], ncol=4)

colnames(Setosa) <- b
colnames(Versicolor) <- b
colnames(Virginica) <- b

Setosa_Means <- apply(Setosa, 2, mean)
Versicolor_Means <- apply(Versicolor, 2, mean)
Virginica_Means <- apply(Virginica, 2, mean)
Setosa_Means
Versicolor_Means
Virginica_Means

Data_Set_Mean <- apply(iris3, 2, mean)
Data_Set_Mean

####################################### Q8 ########################################

# Use the state.x77 data matrix and the tapply() function to obtain the following:

# A - the mean per capita income of the states in each of the four regions def by
# state.region

Mean_Income_By_Region <- tapply(state.x77[,'Income'], state.region, mean)
Mean_Income_By_Region

# B - the maximum illiteracy rates for states in each of the nine divisions

Max_Illiteracy_Rates_By_Division <- tapply(state.x77[,'Illiteracy'], state.division, max)
Max_Illiteracy_Rates_By_Division

# C - the number of states in each region

States_By_Region <- tapply(state.name, state.region, length)
States_By_Region

# D - the median high school graduation rates for groups of states defined by
# state.region and state.size

state.size <- cut(state.x77[,'Population'], breaks=c(0,200,10000, Inf), labels = c("Small", "Medium", "Large"))
state.size
Median_HS_Grad_By_Region_Size <- tapply(state.x77[,'HS Grad'], list(state.region, state.size), median, na.rm=TRUE)
Median_HS_Grad_By_Region_Size

########################################### Q9 ###################################

# Use the function aov() to perform a one way analysis of variance on the chickwts data
# with feed as the treatment factor. Assign the result to an object names chick.aov.
# Print anova table

attach(chickwts)
head(chickwts)
chick.aov <- aov(weight ~ feed, data=chickwts)
chick.aov
chick.aov$residuals
ggplot(chick.aov, aes(x=factor(feed), y=weight)) + geom_boxplot()

#################################### Q10 ########################################

# Write an R function for conducting a one sample t-test. Return a list containing
# the two components t-stat and two sided p value
# Use this function to test the hypothesis that the mean of the weight variable is
# equal to 240 against the two sided alternative.

# t_test function
t_test <- function(dsn, m) {
  return(t.test(dsn, mu=m))
}

# obtain one sample t-test
test_ttest <- t_test(chickwts$weight, 240) # Ho: mu=240
test_ttest
P <- test_ttest$p.value
T <- test_ttest$statistic
t_test_stats <- list(T,P)

# t_stats
t_test_stats
