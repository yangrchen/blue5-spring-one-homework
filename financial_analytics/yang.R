library(tidyverse)
library(smbinning)
library(car)

accepted <- read.csv("accepted_customers.csv")

# Flip the target variable for the smbinning package and rename the weights
accepted <- accepted %>% mutate(GB = ifelse(GB == 1, 0, 1)) %>% rename(weight = X_freq_)

# Check for missing values in the columns
accepted %>% summarise(across(everything(), ~ sum(is.na(.))))

set.seed(12345)
train_id <- sample(seq_len(nrow(accepted)), size = floor(0.7 * nrow(accepted)))
train <- accepted[train_id, ]
valid <- accepted[-train_id, ]

# Factoring the categorical variables if unique values < 3
categorical <- c("BUREAU", "CAR", "CARDS", "DIV", "EC_CARD", "FINLOAN", "LOCATION", "NAT", "PRODUCT", "PROF", "REGN", "RESID", "TEL")
train <- train %>% mutate_at(categorical, as.factor)

# Calculate the information value for every variable with good-bad target
iv_summary <- smbinning.sumiv(df = train, y = "GB")
smbinning.sumiv.plot(iv_summary)

# Filter out EC_CARD because the information is already contained in CARDS
table(train$CARDS, train$EC_CARD)

# Selecting variables with IV >= 0.1
sig_vars <- iv_summary %>%
    filter(IV >= 0.1 & Char != "EC_CARD") %>%
    arrange(desc(IV))
sig_vars

result_all_sig <- list()

for (i in 1:nrow(sig_vars)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        result <- smbinning.factor(df = train, y = "GB", x = var)
    } else {
        result <- smbinning(df = train, y = "GB", x = var)
    }
    result_all_sig[[var]] <- result
}
result_all_sig

for (i in 1:length(result_all_sig)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        train <- smbinning.factor.gen(df = train, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    } else {
        train <- smbinning.gen(df = train, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    }
}
train %>% head()

# Fitting the initial model with binned variables
initial_model <- glm(GB ~ AGE_bin + TMJOB1_bin + INCOME_bin + PERS_H_bin + CARDS_bin, data = train, family = "binomial", weights = train$weight)
summary(initial_model)

train <- train %>% mutate(pred = initial_model$fitted.values)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)

bin_model <- smbinning.scaling(initial_model, pdo = 50, score = 500, odds = 20)
train_bin <- smbinning.scoring.gen(bin_model, dataset = train)
train_bin %>% head()
