library(tidyverse)
library(smbinning)

accepted <- read.csv("accepted_customers.csv")

# Flip the target variable for the smbinning package
accepted <- accepted %>% mutate(GB = ifelse(GB == 1, 0, 1))

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

# Selecting variables with IV >= 0.1
sig_vars <- iv_summary %>%
    filter(IV >= 0.1) %>%
    arrange(desc(IV))
sig_vars

result_all_sig <- list()

for (i in 1:nrow(sig_vars)) {
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        result <- smbinning.factor(df = train, y = "GB", x = sig_vars$Char[i])
    } else {
        result <- smbinning(df = train, y = "GB", x = sig_vars$Char[i])
    }
    result_all_sig[[var]] <- result
}
result_all_sig


# smbinning.gen(df = train, ivout = result_all_sig[["AGE"]], chrname = "AGE_bin")
