rm(list=ls())

#reloading the existing libraries
library("rio")
library("moments")
library(dplyr)
#library(ggplot2)
library("corrplot")
library(car)
library(tidyr)
#library(GGally)
library(MASS)
library(lmtest)
library(PerformanceAnalytics)
library(pscl)
library(AER)
library(survival)
library(stargazer)
library(readxl)

setwd("C:/Users/himan/Downloads")

sheet1 <- read_excel("SnackChain.xlsx", sheet = "stores")
sheet2 <- read_excel("SnackChain.xlsx", sheet = "products")
sheet3 <- read_excel("SnackChain.xlsx", sheet = "transactions")

# identify duplicated values for primary key: Stores
sheet1$dup_ids <- duplicated(sheet1$STORE_ID)
##-- we can see that store store ID 4503 and 17627 have duplicates. 

mean(subset(sheet1, SEGMENT == "UPSCALE")$AVG_WEEKLY_BASKETS)
mean(subset(sheet1, SEGMENT == "MAINSTREAM")$AVG_WEEKLY_BASKETS)
##--- Value matches nore closly to Mainstream 

sheet1 <- sheet1[!(sheet1$STORE_ID == 17627 & sheet1$SEGMENT == 'UPSCALE'), ]
sheet1 <- sheet1[!(sheet1$STORE_ID == 4503 & sheet1$SEGMENT == 'UPSCALE'), ]
sheet1$dup_ids <- NULL

# Join the first two data frames based on a common column
joined_table1 <- merge(sheet3, sheet2, by = "UPC")

# Join the third data frame with the joined table based on a common column
df <- merge(joined_table1, sheet1, by = "STORE_ID")
df <- df[df$CATEGORY != "ORAL HYGIENE PRODUCTS", ] # Not Used

# Check for missing values
colSums(is.na(df))  #price - 10, base_price - 173, parking - 277348 

df[is.na(df)] <- NA

df$PRICE <- ifelse(is.na(df$PRICE) & df$TPR_ONLY == 0, df$BASE_PRICE, df$PRICE)
df$BASE_PRICE <- ifelse(is.na(df$BASE_PRICE) & df$TPR_ONLY == 0, df$PRICE, df$BASE_PRICE)

df$PARKING <- NULL
colSums(is.na(df)) # Check for missing values


#library(openxlsx)
#write.xlsx(df, file = "SnackChain_merge.xlsx", sheetName = "Sheet1", row.names = FALSE)

# Extract numeric values and units for product size
df$PRODUCT_SIZE <- as.numeric(gsub("[^0-9.]", "", df$PRODUCT_SIZE))

#Rename Columns:
colnames(df)[which(names(df) == "WEEK_END_DATE")] <- "WEEKEND"
colnames(df)[which(names(df) == "AVG_WEEKLY_BASKETS")] <- "AVG_BSKT"

#convert to as factor :
df$DISPLAY <- as.factor(df$DISPLAY)
df$FEATURE <- as.factor(df$FEATURE)
df$TPR_ONLY <- as.factor(df$TPR_ONLY)
df$WEEK_END_DATE <- as.factor(df$WEEKEND)
df$UPC <- as.factor(df$UPC)
df$STORE_ID <- as.factor(df$STORE_ID)
df$CATEGORY <- as.factor(df$CATEGORY)
df$DESCRIPTION <- as.factor(df$DESCRIPTION)
df$MANUFACTURER <- as.factor(df$MANUFACTURER)
df$SUB_CATEGORY <- as.factor(df$SUB_CATEGORY)
df$CITY <- as.factor(df$CITY)
df$STATE <- as.factor(df$STATE)
df$MSA <- as.factor(df$MSA)
df$SEGMENT <- as.factor(df$SEGMENT)
df$STORE_NAME <- as.factor(df$STORE_NAME)

##------------------- Corrplot---------------------------------
# select only numeric columns from df
df_corr <- df[, sapply(df, is.numeric)]

# Compute the correlation matrix
correlations <- cor(df_corr)

# create a visually-pleasing correlation plot
correlations <- cor(df_corr)
corrplot(correlations, method = "number", type = "upper", tl.col = "black")
##price and base price consider base price
##Units , visits ,hhs, spend corelate 

##-----------------------Data Visualization---------------
#histogram of target variable : SPEND
hist(df$SPEND, col = "lightblue", main = "Distribution of SPEND", xlab = "SPEND", ylab = "Frequency")
hist(log(df$SPEND), col = "lightblue", main = "Distribution of SPEND", xlab = "SPEND", ylab = "Frequency")

#histogram of target variable : UNITS
hist(df$UNITS, col = "lightblue", main = "Distribution of UNITS", xlab = "UNITS", ylab = "Frequency")
hist(log(df$UNITS), col = "lightblue", main = "Distribution of UNITS", xlab = "UNITS", ylab = "Frequency")

#histogram of target variable : HHS
hist(df$HHS, col = "lightblue", main = "Distribution of HHS", xlab = "HHS", ylab = "Frequency")
hist(log(df$HHS), col = "lightblue", main = "Distribution of HHS", xlab = "HHS", ylab = "Frequency")

#Box Plot for : SEGMENT 

plot(df$SPEND ~ df$SEGMENT)

# Box Plot for : CATEGORY 
plot(df$SPEND ~ df$CATEGORY)
##---------------------Model for UNITS, HHS, SPEND------------------
#1.1 Model for Spend 
df$SPEND_I <- as.integer(df$SPEND)
df <- df[df$SPEND_I != 0, ]
m1_spend = lmer(log(SPEND_I) ~  DISPLAY*CATEGORY + FEATURE*CATEGORY + TPR_ONLY*CATEGORY +
                   DISPLAY*SEGMENT + FEATURE*SEGMENT + TPR_ONLY*SEGMENT +
                   BASE_PRICE + PRODUCT_SIZE  + AVG_BSKT  +( 1 | STORE_ID ) + (1 | MSA), data = df)

summary(m1_spend)

ranef(m1_spend)
vif(m1_spend)
#durbinWatsonTest(resid(m1_spend))

#1.2 Model for Unit
m1_unit = lmer(log(UNITS) ~ DISPLAY + FEATURE + TPR_ONLY + BASE_PRICE+ CATEGORY +
                PRODUCT_SIZE  + SEGMENT +  AVG_BSKT + ( 1 | STORE_ID ) + (1 | MSA), data = df)

summary(m1_unit)

ranef(m1_unit)
vif(m1_unit)
durbinWatsonTest(resid(m1_unit))

#1.3 Model for HHS
m1_HHS = lmer(log(HHS) ~ DISPLAY + FEATURE + TPR_ONLY + BASE_PRICE+ CATEGORY +
                  PRODUCT_SIZE  + SEGMENT +  AVG_BSKT + ( 1 | STORE_ID ) + (1 | MSA), data = df)

summary(m1_HHS)
ranef(m1_HHS)
vif(m1_HHS)
durbinWatsonTest(resid(m1_HHS ))
#Stargazer
stargazer(m1_spend , m1_unit ,m1_HHS , title="Different Variables", type="text")

##-----------------------------------
m_spend <- glm( SPEND ~ DISPLAY*CATEGORY + FEATURE*CATEGORY + TPR_ONLY*CATEGORY +
                  DISPLAY*SEGMENT + FEATURE*SEGMENT + TPR_ONLY*SEGMENT +
                  BASE_PRICE + PRODUCT_SIZE  + AVG_BSKT + STORE_ID + MSA , data = df , family=poisson(link = log))

m_spend <- glm( SPEND ~ DISPLAY*CATEGORY + FEATURE*CATEGORY + TPR_ONLY*CATEGORY +
                  DISPLAY*SEGMENT + FEATURE*SEGMENT + TPR_ONLY*SEGMENT +
                  BASE_PRICE + PRODUCT_SIZE  + AVG_BSKT + STORE_ID + MSA , data = df , family=poisson(link = log))


##---------------------Price Elasticity-----------------------------
#Plot to see the relation
plot(UNITS ~ PRICE, data = df, pch = 16, cex = 0.5, col = "darkblue",
     xlab = "Price", ylab = "Units Sold", main = "Price Elasticity Scatter Plot")

# create empty data frame to store results
elasticity_data <- data.frame(UPC = character(),
                              Elasticity = numeric(),
                              Description = character(),
                              Category = character(),
                              stringsAsFactors = FALSE)

# function to estimate regression and return the elasticity coefficient
get_elasticity <- function(data) {
  lm_model <- glm(UNITS ~ PRICE, data = data,family = poisson(link = log))
  coef <- summary(lm_model)$coefficients[2, 1]
  return(abs(coef * mean(data$PRICE) / mean(data$UNITS)))
}


# loop through each unique UPC
for (upc in unique(df$UPC)) {
  # subset data for current UPC
  df_sub <- subset(df, UPC == upc)
  
  # estimate elasticity using function
  elasticity <- get_elasticity(df_sub)
  
  # extract product description and category
  description <- unique(df$DESCRIPTION[df$UPC == upc])
  category <- unique(df$CATEGORY[df$UPC == upc])
  
  # add results to data frame
  elasticity_data <- rbind(elasticity_data, data.frame(UPC = upc,
                                                       Elasticity = elasticity,
                                                       Description = description,
                                                       Category = category,
                                                       stringsAsFactors = FALSE))
}

# sort data frame by elasticity in descending order
elasticity_data <- elasticity_data[order(-elasticity_data$Elasticity), ]
# rank the products based on their elasticity coefficient and identify the top five most price elastic and least price elastic products
#5 most elastic products 
most_elastic <- head(elasticity_data[order(elasticity_data$Elasticity, decreasing = TRUE), ], 5)
#5 least elastic products 
least_elastic <- head(elasticity_data[order(elasticity_data$Elasticity), ], 5)
most_elastic
least_elastic
mean(subset(df, UPC == "7218063052")$BASE_PRICE)
mean(subset(df, UPC == "2066200532")$BASE_PRICE)
mean(subset(df, UPC == "7218063979")$BASE_PRICE)
mean(subset(df, UPC == "7218063983")$BASE_PRICE)
