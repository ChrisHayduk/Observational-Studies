library(tidyverse)
library(stargazer)

#Load data
gdp_per_capita <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\GDP per Capita (2005 - 2017).csv")

fdi <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\FDI as Percent of GDP (2005 - 2017).csv")

fdi <- fdi[,1:7]

journal_articles <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Scientific and technical journal articles.csv")

#Create data frame
data <- data.frame(country = gdp_per_capita$`Country Code`, gdp_2005 = gdp_per_capita$`2005`, gdp_2017 = gdp_per_capita$`2017`)

#Remove rows with NAs
#data <- data[complete.cases(data), ]

#fdi <- fdi[complete.cases(fdi), ]

#Get mean FDI over period of interest
mean_fdi <- aggregate(fdi[,7], list(country = fdi$LOCATION), mean)

#Get mean number of journal articles over period of interest
journal_articles$total <- rowSums(journal_articles[,3:14], na.rm=TRUE)

mean_articles <- data.frame(country = journal_articles$`Country Code`, articles = (journal_articles$total)/12.0)

names(mean_fdi) <- c("country", "fdi")

#Load population data so we can adjust number of journal articles
pop_data <-  read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Population by Country.csv")

pop_data <- pop_data[complete.cases(pop_data),]

pop_data$total <- rowSums(pop_data[,3:15], na.rm=TRUE)

pop_data$total <- pop_data$total/100000.0

mean_pop <- data.frame(country = pop_data$`Country Code`, pop = (pop_data$total)/13.0)

article_data <- merge(mean_articles, mean_pop, by = "country")

mean_articles <- data.frame(country = article_data$country, articles = (article_data$articles)/(article_data$pop))

#Create final data frame
#Includes the following:
# GDP per capita in 2005
# GDP per capita in 2017
# Mean Gross FDI from 2005-2017
# Mean number of scientific journal articles published per 1000 inhabitants from 2005-2016
data <- merge(data, mean_fdi, by="country")

data <- merge(data, mean_articles, by = "country")

names(data) <- c("country", "gdp_per_capita_2005", "gdp_per_capita_2017", "mean_fdi", "journal_articles_per_100k_inhabitants")

#Run model
model <- lm(gdp_per_capita_2017 ~ gdp_per_capita_2005 + mean_fdi + journal_articles_per_100k_inhabitants, data)

summary(model)

#Now let's group countries by income level
income_levels <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Metadata for GDP per Capita Table.csv")
income_levels <- income_levels[,c("Country Code", "IncomeGroup")]
names(income_levels) <- c("country", "income_level")

data <- merge(data, income_levels, by="country")

#Create change in GDP and log GDP columns
data$change_in_gdp <- data$gdp_per_capita_2017 - data$gdp_per_capita_2005

data$log_starting_gdp <- log(data$gdp_per_capita_2005)

data$log_ending_gdp <- log(data$gdp_per_capita_2017)

data$change_in_log_gdp <- data$log_ending_gdp - data$log_starting_gdp

#Separate high income countries
high_income <- data[data$income_level == "High income",]
high_income <- high_income[complete.cases(high_income),]
middle_income <- data[data$income_level == "Upper middle income" | data$income_level == "Lower middle income",]
middle_income <- middle_income[complete.cases(middle_income),]

#Create percent increase in GDP column
perc_increase_in_gdp_per_capita <- (high_income$gdp_per_capita_2017 - high_income$gdp_per_capita_2005)/(high_income$gdp_per_capita_2005)*100
high_income$perc_increase_in_gdp_per_capita <- perc_increase_in_gdp_per_capita

#High income model
high_income_model1 <- lm(change_in_log_gdp ~ mean_fdi, high_income)
high_income_model2 <- lm(change_in_log_gdp ~ mean_fdi + journal_articles_per_100k_inhabitants, high_income)
high_income_model3 <- lm(change_in_log_gdp ~ log_starting_gdp + mean_fdi + journal_articles_per_100k_inhabitants, high_income)
high_income_model4 <- lm(change_in_log_gdp ~ log_starting_gdp*mean_fdi + journal_articles_per_100k_inhabitants, high_income)
stargazer(high_income_model1, high_income_model2, high_income_model3, high_income_model4, title="Results")

colnames(high_income)[5] <- "journal_articles"
pairs(high_income[,c("log_starting_gdp", "change_in_log_gdp", "mean_fdi", "journal_articles")], 
      las=TRUE, pch=19, col="firebrick", cex.labels = 1)

stargazer(high_income)
