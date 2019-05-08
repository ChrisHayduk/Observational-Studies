library(tidyverse)
library(stargazer)
library(bsts)
library(reshape2)

#Load data
gdp_per_capita <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\GDP per Capita (2005 - 2017).csv")
gdp_per_capita <- melt(gdp_per_capita, id.vars=1:2, value.name="GDP_Per_Capita", variable.name="Year")

gdp_per_capita <- gdp_per_capita[order(gdp_per_capita$`Country Code`),]

fdi <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\FDI as Percent of GDP (2005 - 2017).csv")

fdi <- fdi[,1:7]

fdi <- data.frame(`Country Code` = fdi$LOCATION, Year = fdi$TIME, FDI = fdi$Value)
colnames(fdi) <- c("Country Code", "Year", "FDI")

tertiary_school <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Tertiary_School_Completion.csv")

tertiary_school <- tertiary_school[,1:7]

tertiary_school <- data.frame(`Country Code` = tertiary_school$LOCATION, Year = tertiary_school$TIME, FDI = tertiary_school$Value)
colnames(tertiary_school) <- c("Country Code", "Year", "Completion_Rate")

journal_articles <- read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Scientific and technical journal articles.csv")
journal_articles <- melt(journal_articles, id.vars=1:2, value.name="Journal Articles", variable.name="Year")
journal_articles <- journal_articles[order(journal_articles$`Country Code`),]

#Load population data so we can adjust number of journal articles
pop_data <-  read_csv("C:\\Users\\Chris\\Desktop\\Observational-Studies\\Data\\Population by Country.csv")
pop_data <- melt(pop_data, id.vars=1:2, value.name="Pop", variable.name="Year")
pop_data <- pop_data[order(pop_data$`Country Code`),]

pop_data$Pop <- pop_data$Pop/100000.0

article_data <- merge(journal_articles, pop_data, by = c("Country Code", "Year"))

articles_per_100000 <- data.frame(`Country Code` = article_data$`Country Code`, 
                                  articles = (article_data$`Journal Articles`)/(article_data$Pop), year = article_data$Year)
colnames(articles_per_100000) <- c("Country Code", "Articles", "Year")
#Create final data frame
#Includes the following:
# GDP per capita in 2005
# GDP per capita in 2017
# Mean Gross FDI from 2005-2017
# Mean number of scientific journal articles published per 1000 inhabitants from 2005-2016
data <- merge(gdp_per_capita, fdi, by=c("Country Code", "Year"))

data <- merge(data, articles_per_100000, by = c("Country Code", "Year"))

data <- merge(data, tertiary_school, by = c("Country Code", "Year"))

data <- data[complete.cases(data),]

new_data <- data[data$`Country Code` == 'USA',]
#Run model
ss <- AddLocalLinearTrend(list(), new_data$GDP_Per_Capita)

model1 <- bsts(new_data$GDP_Per_Capita,
               state.specification = ss,
               niter = 10000)

plot(model1)
plot(model1, "comp")

pred1 <- predict(model1, horizon = 12)
plot(pred1, plot.original = 156)

model2 <- bsts(new_data$GDP_Per_Capita ~ new_data$FDI + new_data$Articles + new_data$Completion_Rate,
               state.specification = ss,
               niter = 10000,
               data = new_data, 
               expected.model.size = 1)

model3 <- bsts(new_data$GDP_Per_Capita ~ new_data$FDI + new_data$Articles + new_data$Completion_Rate,
               state.specification = ss,
               niter = 10000,
               data = new_data,
               expected.model.size = 3)

CompareBstsModels(list("Model 1" = model1,
                      "Model 2" = model2,
                      "Model 3" = model3),
                 colors = c("black", "red", "blue"))




