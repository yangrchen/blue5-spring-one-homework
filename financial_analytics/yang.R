library(tidyverse)
library(gmodels)
library(vcd)
library(shades)
library(latticeExtra)
library(plotly)
library(smbinning)
library(car)

accepted <- read.csv("accepted_customers.csv")
rejected <- read.csv("rejected_customers.csv")

# Flip the target variable for the smbinning package and rename the weights
accepted <- accepted %>%
    mutate(good = ifelse(GB == 1, 0, 1)) %>%
    rename(weight = X_freq_, bad = GB)

# Check for missing values in the columns
accepted %>% summarise(across(everything(), ~ sum(is.na(.))))

# Factoring the categorical variables
categorical <- c("BUREAU", "CAR", "CARDS", "DIV", "EC_CARD", "FINLOAN", "LOCATION", "NAT", "PRODUCT", "PROF", "REGN", "RESID", "TEL")
accepted <- accepted %>% mutate_at(categorical, as.factor)

set.seed(12345)
train_id <- sample(seq_len(nrow(accepted)), size = floor(0.7 * nrow(accepted)))
train <- accepted[train_id, ]
test <- accepted[-train_id, ]

# Sample 1000 observations from rejects to reduce weight calculations
set.seed(12345)
rejected_id <- sample(seq_len(nrow(rejected)), size = 1000)
rejected <- rejected[rejected_id, ]

# Calculate the information value for every variable with good-bad target
iv_summary <- smbinning.sumiv(df = train, y = "good")
smbinning.sumiv.plot(iv_summary)
iv_summary

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
        result <- smbinning.factor(df = train, y = "good", x = var)
    } else {
        result <- smbinning(df = train, y = "good", x = var)
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

# Checking for quasi-complete separation
table(train$good, train$AGE_bin)
table(train$good, train$TMJOB1_bin)
table(train$good, train$INCOME_bin)
table(train$good, train$PERS_H_bin)
table(train$good, train$CARDS_bin)

# Combine categories to address quasi-complete separation in CARDS_bin
train$CARDS <- fct_collapse(train$CARDS, AMEXVISA = c("American Express", "VISA Others", "VISA mybank"))
test$CARDS <- fct_collapse(test$CARDS, AMEXVISA = c("American Express", "VISA Others", "VISA mybank"))
accepted$CARDS <- fct_collapse(accepted$CARDS, AMEXVISA = c("American Express", "VISA Others", "VISA mybank"))
train <- train %>% select(-CARDS_bin)

result_all_sig[["CARDS"]] <- smbinning.factor(df = train, y = "good", x = "CARDS")
train <- smbinning.factor.gen(df = train, ivout = result_all_sig[["CARDS"]], chrname = "CARDS_bin")

# Fitting the initial model with binned variables
initial_model <- glm(good ~ AGE_bin + TMJOB1_bin + INCOME_bin + PERS_H_bin + CARDS_bin, data = train, family = "binomial", weights = train$weight)
summary(initial_model)

# Evaluate the initial model on the training data
train <- train %>% mutate(pred = initial_model$fitted.values)

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "good", report = 0, plot = "auc")

# Evaluate the initial model on the test data
for (i in 1:length(result_all_sig)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        test <- smbinning.factor.gen(df = test, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    } else {
        test <- smbinning.gen(df = test, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    }
}

test <- test %>% mutate(pred = predict(initial_model, newdata = test, type = "response"))

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "good", report = 0, plot = "auc")

# Creating the scorecard model on training data
model_scaled <- smbinning.scaling(initial_model, pdo = 50, score = 500, odds = 20)
model_scaled

# Score the model on training data
train <- smbinning.scoring.gen(model_scaled, dataset = train)

# Plot the distribution of train scores
hist(train$Score, breaks = 50, xlim = c(400, 720), main = "Distribution of Train Scores", xlab = "Score")

# Create the scorecard model on test data
test <- smbinning.scoring.gen(model_scaled, dataset = test)
test %>% head()

hist(test$Score, breaks = 50, xlim = c(400, 720), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(400, 720), main = "Distribution of Scores", xlab = "Score")

# Plotting default by score in training data

# Create deciles of score at each 10% interval
cutpoints <- quantile(train$Score, probs = seq(0, 1, 0.1))
train$Score.QBin <- cut(train$Score, breaks = cutpoints, include.lowest = TRUE)

# Calculate the sample default rate for each interval
Default.QBin.train <- round(table(train$Score.QBin, train$bad)[, 2] / rowSums(table(train$Score.QBin, train$bad)) * 100, 2)

Default.QBin.train

# Calculate the population default rate for each interval where our goods are multiplied by the proportional weight
Default.QBin.train.pop <- round(table(train$Score.QBin, train$bad)[, 2] / (table(train$Score.QBin, train$bad)[, 2] + table(train$Score.QBin, train$bad)[, 1] * 30.0) * 100, 2)

Default.QBin.train.pop

barplot(Default.QBin.train.pop,
    main = "Default Decile Plot",
    xlab = "Deciles of Scorecard",
    ylab = "Default Rate (%)", ylim = c(0, 20),
)
abline(h = 5.00, lwd = 2, lty = "dashed")

# Plotting default by score in test data
cutpoints <- quantile(test$Score, probs = seq(0, 1, 0.1))
test$Score.QBin <- cut(test$Score, breaks = cutpoints, include.lowest = TRUE)
Default.QBin.test <- round(table(test$Score.QBin, test$bad)[, 2] / rowSums(table(test$Score.QBin, test$bad)) * 100.0, 2)

Default.QBin.test

Default.QBin.test.pop <- round(table(test$Score.QBin, test$bad)[, 2] / (table(test$Score.QBin, test$bad)[, 2] + table(train$Score.QBin, train$bad)[, 1] * 30.0) * 100, 2)

Default.QBin.test.pop

barplot(Default.QBin.test.pop,
    main = "Default Decile Plot",
    xlab = "Deciles of Scorecard",
    ylab = "Default Rate (%)", ylim = c(0, 20),
    col = saturation(heat.colors, scalefac(0.8))(10)
)
abline(h = 5.00, lwd = 2, lty = "dashed")
text(11.5, 6, "Current = 5.00%")

# Reject Inference

# Cleaning and preparing reject data

# Binning the CARDS variable the same way as the accepted data
rejected$CARDS <- fct_collapse(rejected$CARDS, AMEXVISA = c("American Express", "VISA Others", "VISA Citibank"))
rejected <- rejected %>% mutate_at(categorical, as.factor)

# for (var in names(result_all_sig)) {
#     result_all_sig[[var]]$bands[1] <- min(c(accepted[[var]], rejected[[var]]), na.rm = TRUE)
#     result_all_sig[[var]]$bands[length(result_all_sig[[var]]$bands)] <- max(c(accepted[[var]], rejected[[var]]), na.rm = TRUE)
# }

rejects_scored <- rejected
for (i in 1:length(result_all_sig)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        rejects_scored <- smbinning.factor.gen(df = rejects_scored, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    } else {
        rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    }
}

# Fuzzy augmentation for reject inference

# Set the reject-bad weight to 1 and the reject-good weight to 30 since we downsampled the rejects
weight_rb <- 1
weight_rg <- 30
rejects_scored <- rejects_scored %>% mutate(pred = predict(initial_model, newdata = rejects_scored, type = "response"))

rejects_g <- rejected
rejects_b <- rejected

# Create the weighted cases for each rejected applicant using probability of default and non-default--keep in mind the initial model is predicting the goods
rejects_g$bad <- 0
rejects_g$weight_ar <- rejects_scored$pred * weight_rg
rejects_g$good <- 1

rejects_b$bad <- 1
rejects_b$weight_ar <- (1 - rejects_scored$pred) * weight_rb
rejects_b$good <- 0

# Don't adjust the weights for the accepted applicants due to downsampling
accepted <- accepted %>% rename(weight_ar = weight)

comb <- rbind(accepted, rejects_g, rejects_b)

# Build final scorecard model!

set.seed(12345)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.7 * nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

# Since we are including rejects, may want to keep more variables other than just 0.1 IV. Keep the weak predictors > 0.02 IV for now
iv_summary <- smbinning.sumiv(df = train_comb, y = "good")
smbinning.sumiv.plot(iv_summary)
iv_summary

sig_vars <- iv_summary %>%
    filter(IV >= 0.02 & Char != "EC_CARD") %>%
    arrange(desc(IV))
sig_vars

result_all_sig <- list()

for (i in 1:nrow(sig_vars)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        result <- smbinning.factor(df = train_comb, y = "good", x = var)
    } else {
        result <- smbinning(df = train_comb, y = "good", x = var)
    }
    result_all_sig[[var]] <- result
}
result_all_sig

for (i in 1:length(result_all_sig)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        train_comb <- smbinning.factor.gen(df = train_comb, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    } else {
        train_comb <- smbinning.gen(df = train_comb, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    }
}

# Validating that there are no missing values
train_comb %>% summarise(across(everything(), ~ sum(is.na(.))))

# Build the final model
final_score <- glm(good ~ AGE_bin + TMJOB1_bin + CARDS_bin + PERS_H_bin + TEL_bin, data = train_comb, family = "binomial", weights = train_comb$weight_ar)
summary(final_score)

train_comb <- train_comb %>% mutate(pred = final_score$fitted.values)

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "good", report = 0, plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "good", report = 0, plot = "auc")

# Evaluate the final model on the test data
for (i in 1:length(result_all_sig)) {
    var <- sig_vars$Char[i]
    if (str_split(sig_vars$Process[i], " ")[[1]][1] == "Factor") {
        test_comb <- smbinning.factor.gen(df = test_comb, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    } else {
        test_comb <- smbinning.gen(df = test_comb, ivout = result_all_sig[[var]], chrname = paste0(var, "_bin"))
    }
}

test_comb <- test_comb %>% mutate(pred = predict(final_score, newdata = test_comb, type = "response"))

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "good", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "good", report = 0, plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "good", report = 0, plot = "auc")

# Create the final scorecard model
final_model_scaled <- smbinning.scaling(final_score, pdo = 50, score = 500, odds = 20)
final_model_scaled

# Score the final model on training data
train_comb <- smbinning.scoring.gen(final_model_scaled, dataset = train_comb)

hist(train_comb$Score, breaks = 50, xlim = c(400, 820), main = "Distribution of Train Scores", xlab = "Score")

# Score the final model on test data
test_comb <- smbinning.scoring.gen(final_model_scaled, dataset = test_comb)

hist(test_comb$Score, breaks = 50, xlim = c(400, 820), main = "Distribution of Train Scores", xlab = "Score")

accepts_scored_comb <- rbind(train_comb, test_comb)

cutpoints <- quantile(accepts_scored_comb$Score, probs = seq(0, 1, 0.1))
accepts_scored_comb$Score.QBin <- cut(accepts_scored_comb$Score, breaks = cutpoints, include.lowest = TRUE)
Default.QBin.pop <- round(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[, 2] / (table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[, 2] + table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[, 1] * 30.0) * 100, 2)

Default.QBin.pop

barplot(Default.QBin.pop,
    main = "Default Decile Plot",
    xlab = "Deciles of Scorecard",
    ylab = "Default Rate (%)", ylim = c(0, 20),
    col = saturation(heat.colors, scalefac(0.8))(10)
)
abline(h = 3.23, lwd = 2, lty = "dashed")
text(11.5, 5, "Current = 3.23%")

# Plotting Default, Acceptance, & Profit By Score #
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL

cost <- 52000
profit <- 2000
for (i in min(floor(accepts_scored_comb$Score)):max(floor(accepts_scored_comb$Score))) {
    score[i - min(floor(accepts_scored_comb$Score)) + 1] <- i
    def[i - min(floor(accepts_scored_comb$Score)) + 1] <- 100 * sum(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i)]) / (length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 1)]) + 30 * length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 0)]))
    acc[i - min(floor(accepts_scored_comb$Score)) + 1] <- 100 * (length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 1)]) + 30 * length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 0)])) / (length(accepts_scored_comb$bad[which(accepts_scored_comb$bad == 1)]) + 30 * length(accepts_scored_comb$bad[which(accepts_scored_comb$bad == 0)]))
    prof[i - min(floor(accepts_scored_comb$Score)) + 1] <- length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 1)]) * (-cost) + 30 * length(accepts_scored_comb$bad[which(accepts_scored_comb$Score >= i & accepts_scored_comb$bad == 0)]) * profit
}

plot_data <- data.frame(def, acc, prof, score)

def_plot <- xyplot(def ~ score, plot_data,
    type = "l", lwd = 2, col = "red",
    ylab = "Default Rate (%)",
    xlab = "Score",
    main = "Default Rate by Acceptance Across Score",
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(h = 5.00, col = "red")
    }
)
acc_plot <- xyplot(acc ~ score, plot_data,
    type = "l", lwd = 2, col = "blue",
    ylab = "Acceptance Rate (%)",
    panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(h = 70, col = "blue")
    }
)
prof_plot <- xyplot(prof / 1000 ~ score, plot_data,
    type = "l", lwd = 2, col = "green",
    ylab = "Profit (Thousands $)",
    xlab = "Score",
    main = "Profit by Acceptance Across Score"
)

doubleYScale(def_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)
doubleYScale(prof_plot, acc_plot, add.ylab2 = TRUE, use.style = FALSE)

ay1 <- list(
    title = "Default Rate (%)",
    range = c(0, 10)
)
ay2 <- list(
    tickfont = list(),
    range = c(0, 100),
    overlaying = "y",
    side = "right",
    title = "Acceptance Rate (%)"
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~def, name = "Default Rate (%)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(
    title = "Default Rate by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
    xaxis = list(title = "Scorecard Value"),
    legend = list(x = 1.2, y = 0.8)
)

fig

ay1 <- list(
    title = "Profit ($)",
    showline = FALSE,
    showgrid = FALSE
)
ay2 <- list(
    tickfont = list(),
    range = c(0, 100),
    overlaying = "y",
    side = "right",
    title = "Acceptance Rate (%)"
)
fig <- plot_ly()
fig <- fig %>% add_lines(x = ~score, y = ~prof, name = "Profit ($)")
fig <- fig %>% add_lines(x = ~score, y = ~acc, name = "Acceptance Rate (%)", yaxis = "y2")
fig <- fig %>% layout(
    title = "Profit by Acceptance Across Score", yaxis = ay1, yaxis2 = ay2,
    xaxis = list(title = "Scorecard Value"),
    legend = list(x = 1.2, y = 0.8)
)

fig