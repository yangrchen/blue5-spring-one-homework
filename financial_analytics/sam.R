#libraries 
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

#load in datasets
accepts <- read.csv("/Users/samdotson/Downloads/Homework1_FA/accepted_customers.csv")
rejects <- read.csv("/Users/samdotson/Downloads/Homework1_FA/rejected_customers.csv")

#switching target (good = 1)
table(accepts$GB)
accepts$GB <- abs(accepts$GB - 1)
table(accepts$GB)

#factor categorical variables 
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$CAR <- as.factor(accepts$CAR)
accepts$CARDS <- as.factor(accepts$CARDS)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$REGN <- as.factor(accepts$REGN)
accepts$TEL <- as.factor(accepts$TEL)
accepts$DIV <- as.factor(accepts$DIV)
accepts$EC_CARD <- as.factor(accepts$EC_CARD)
accepts$FINLOAN <- as.factor(accepts$FINLOAN)
accepts$LOCATION <- as.factor(accepts$LOCATION)
accepts$RESID <- as.factor(accepts$RESID)
accepts$NMBLOAN <- as.factor(accepts$NMBLOAN)

#creating training and test datasets
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$GB)
table(test$GB)

#binning AGE continuous variables
result <- smbinning(df = train, y = "GB", x = "AGE")
result$ivtable
result$cut
result$iv

#plotting AGE as an examples
smbinning.plot(result,option="dist",sub="AGE")
smbinning.plot(result,option="goodrate",sub="AGE")
smbinning.plot(result,option="badrate",sub="AGE")
smbinning.plot(result,option="WoE",sub="=AGE")

#doing the rest of the continuous variables 
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() 

for(i in 1:length(num_names)){
  result_all[[num_names[i]]] <- smbinning(df = train, y = "GB", x = num_names[i])
 }
 
#show results for each continuous variable
result_all$AGE$ivtable
result_all$CASH$ivtable
result_all$CHILDREN$ivtable
result_all$INCOME$ivtable
result_all$LOANS #no significant splits
#result_all$NMBLOAN (changed to categorical since unique values < 5)
result_all$PERS_H$ivtable
result_all$TMADD$ivtable
result_all$TMJOB1$ivtable

# Binning of Factor Variables #
result <- smbinning.factor(df = train, y = "GB", x = "CARDS")
result$ivtable
result$cut
result$iv

# Binning of Factor Variables #
result <- smbinning.factor(df = train, y = "GB", x = "EC_CARD")
result$ivtable
result$cut
result$iv

#Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "GB")

smbinning.sumiv.plot(iv_summary)
iv_summary
#ONLY keeping AGE, TMJOB1, INCOME, PERS_H, CARDS, EC_CARD because IV > 0.1

# Generating Variables of Bins and WOE Values #
#apply bins to variables
train = smbinning.gen(df = train, ivout = result_all$PERS_H, chrname = 'PERS_Hbin')
train = smbinning.gen(df = train, ivout = result_all$AGE, chrname = 'AGEbin')
train = smbinning.gen(df = train, ivout = result_all$TMJOB1, chrname = 'TMJOB1bin')
train = smbinning.gen(df = train, ivout = result_all$INCOME, chrname = 'INCOMEbin')
#train = smbinning.gen(df = train, ivout = result$CARDS, chrname = 'CARDSbin')
#train = smbinning.gen(df = train, ivout = result$EC_CARDS, chrname = 'EC_CARDSbin')


#INITIAL MODEL
initial_model <- glm(data = train, GB ~  PERS_Hbin +
                           AGEbin + 
                           TMJOB1bin +
                           INCOMEbin +
                          CARDS +
                          EC_CARD, 
                         weights = train$X_freq_, family = "binomial")

summary(initial_model)

