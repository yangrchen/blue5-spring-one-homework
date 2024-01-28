library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

# reading in the dataset and getting all of the varaiables correct
accepts <- read.csv("accepted_customers.csv")

accepts <- as.data.frame(accepts)
accepts <- accepts %>% mutate(GB = ifelse(GB == 1, 0, 1))

accepts$CHILDREN <- as.factor(accepts$CHILDREN)
accepts$TEL <- as.factor(accepts$TEL)
accepts$NMBLOAN <- as.factor(accepts$NMBLOAN)
accepts$FINLOAN <- as.factor(accepts$FINLOAN)
accepts$EC_CARD <- as.factor(accepts$EC_CARD)
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$LOCATION <- as.factor(accepts$LOCATION)
accepts$REGN <- as.factor(accepts$REGN)
accepts$DIV <- as.factor(accepts$DIV)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$RESID <- as.factor(accepts$RESID)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$CAR <- as.factor(accepts$CAR)
accepts$CARDS <- as.factor(accepts$CARDS)
accepts$GB <- as.numeric(accepts$GB)

# Collapsing categories to address quasi-complete separation
accepts <- accepts %>%
  mutate(CARDS = fct_collapse(CARDS, "Other credit car" = c("American Express")))

accepts <- accepts %>%
  mutate(CARDS = fct_collapse(CARDS, "VISA Others" = c("VISA mybank")))

unique(accepts$CARDS)

set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70 * nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

iv_summary <- smbinning.sumiv(df = train, y = "GB")
smbinning.sumiv.plot(iv_summary)

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list()

for (i in 1:length(num_names)) {
  result_all[[num_names[i]]] <- smbinning(df = train, y = "GB", x = num_names[i])
}

# all variables with IV above 0.1
train <- smbinning.gen(df = train, ivout = result_all$AGE, chrname = "AGE_bin")
train <- smbinning.gen(df = train, ivout = result_all$INCOME, chrname = "INCOME_bin")
train <- smbinning.gen(df = train, ivout = result_all$TMJOB1, chrname = "TMJOB1_bin")
train <- smbinning.gen(df = train, ivout = result_all$PERS_H, chrname = "PERS_H_bin")
train <- smbinning.factor.gen(df = train, ivout = smbinning.factor(df = train, y = "GB", x = "CARDS"), chrname = "CARDS_bin")

# Not including EC_CARD as CARDS contains its info
initial_score <- glm(
  data = train, GB ~ PERS_H_bin +
    AGE_bin +
    TMJOB1_bin +
    INCOME_bin +
    CARDS_bin,
  weights = train$X_freq_, family = "binomial"
)

summary(initial_score)

train <- train %>%
  mutate(pred = initial_score$fitted.values)

smbinning.metrics(
  dataset = train, prediction = "pred",
  actualclass = "GB", report = 1
)
smbinning.metrics(
  dataset = train, prediction = "pred",
  actualclass = "GB", plot = "ks"
)
smbinning.metrics(
  dataset = train, prediction = "pred",
  actualclass = "GB", plot = "auc"
) # AUC on train is 70.24%

bin_model <- smbinning.scaling(initial_score, pdo = 50, score = 500, odds = 20)
train <- smbinning.scoring.gen(bin_model, dataset = train)
train %>% head()

test <- smbinning.gen(df = test, ivout = result_all$AGE, chrname = "AGE_bin")
test <- smbinning.gen(df = test, ivout = result_all$INCOME, chrname = "INCOME_bin")
test <- smbinning.gen(df = test, ivout = result_all$TMJOB1, chrname = "TMJOB1_bin")
test <- smbinning.gen(df = test, ivout = result_all$PERS_H, chrname = "PERS_H_bin")
test <- smbinning.factor.gen(
  df = test, ivout = smbinning.factor(
    df = train, y = "GB",
    x = "CARDS"
  ),
  chrname = "CARDS_bin"
)

test <- smbinning.scoring.gen(bin_model, dataset = test)
head(test)

test <- test %>%
  mutate(pred = predict(initial_score, newdata = test, type = "response"))

smbinning.metrics(
  dataset = test, prediction = "pred",
  actualclass = "GB", report = 1
)
smbinning.metrics(
  dataset = test, prediction = "pred",
  actualclass = "GB", plot = "ks"
)
smbinning.metrics(
  dataset = test, prediction = "pred",
  actualclass = "GB", plot = "auc"
) # AUC on test is 72.58%

accepts_scored <- rbind(train, test)

# reading in the rejects dataset and getting all of the varaiables correct
rejects <- read.csv("rejected_customers.csv")
rejects <- as.data.frame(rejects)

set.seed(1234)
train_id <- sample(seq_len(nrow(rejects)), size = 1000)
rejects <- rejects[train_id, ]

rejects$CHILDREN <- as.factor(rejects$CHILDREN)
rejects$PERS_H <- as.integer(rejects$PERS_H)
rejects$AGE <- as.integer(rejects$AGE)
rejects$TMADD <- as.integer(rejects$TMADD)
rejects$TMJOB1 <- as.integer(rejects$TMJOB1)
rejects$TEL <- as.factor(rejects$TEL)
rejects$NMBLOAN <- as.factor(rejects$NMBLOAN)
rejects$FINLOAN <- as.factor(rejects$FINLOAN)
rejects$INCOME <- as.integer(rejects$INCOME)
rejects$EC_CARD <- as.factor(rejects$EC_CARD)
rejects$BUREAU <- as.factor(rejects$BUREAU)
rejects$LOCATION <- as.factor(rejects$LOCATION)
rejects$LOANS <- as.integer(rejects$LOANS)
rejects$REGN <- as.factor(rejects$REGN)
rejects$DIV <- as.factor(rejects$DIV)
rejects$CASH <- as.integer(rejects$CASH)
rejects$PRODUCT <- as.factor(rejects$PRODUCT)
rejects$RESID <- as.factor(rejects$RESID)
rejects$NAT <- as.factor(rejects$NAT)
rejects$PROF <- as.factor(rejects$PROF)
rejects$CAR <- as.factor(rejects$CAR)
rejects$CARDS <- as.factor(rejects$CARDS)

rejects <- rejects %>%
  mutate(CARDS = fct_collapse(CARDS, "VISA Others" = c("VISA Citibank")))

rejects <- rejects %>%
  mutate(CARDS = fct_collapse(CARDS, "Other credit car" = c("American Express")))

unique(rejects$CARDS)

num_names <- names(rejects)[sapply(accepts, is.numeric)] # Gathering the names of numeric variables in data #

rejects_scored <- rejects
rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all$AGE, chrname = "AGE_bin")
rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all$INCOME, chrname = "INCOME_bin")
rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all$TMJOB1, chrname = "TMJOB1_bin")
rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all$PERS_H, chrname = "PERS_H_bin")
rejects_scored <- smbinning.factor.gen(df = rejects_scored, ivout = smbinning.factor(df = accepts, y = "GB", x = "CARDS"), chrname = "CARDS_bin")

rejects_scored <- smbinning.scoring.gen(bin_model, dataset = rejects_scored)

rejects_scored$pred <- predict(initial_score, newdata = rejects_scored, type = "response")
rejects$GB <- as.numeric(rejects_scored$pred > 0.9643)
rejects$bad <- abs(rejects$GB - 1)
accepts$bad <- abs(accepts$GB - 1)
rejects$X_freq_ <- ifelse(rejects$bad == 1, 1, 30)

comb_hard <- rbind(accepts, rejects)

final <- comb_hard

set.seed(12345)
final_id <- sample(seq_len(nrow(final)), size = floor(0.70 * nrow(final)))
final_train <- final[final_id, ]
final_test <- final[-final_id, ]

iv_summary <- smbinning.sumiv(df = final_train, y = "GB")
smbinning.sumiv.plot(iv_summary)

num_names2 <- names(final_train)[sapply(final_train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list()

for (i in 1:length(num_names2)) {
  result_all[[num_names2[i]]] <- smbinning(df = final_train, y = "GB", x = num_names2[i])
}

final_train <- smbinning.gen(df = final_train, ivout = result_all$AGE, chrname = "AGE_bin")
final_train <- smbinning.gen(df = final_train, ivout = result_all$TMJOB1, chrname = "TMJOB1_bin")
final_train <- smbinning.gen(df = final_train, ivout = result_all$PERS_H, chrname = "PERS_H_bin")
final_train <- smbinning.factor.gen(df = final_train, ivout = smbinning.factor(df = final, y = "GB", x = "CARDS"), chrname = "CARDS_bin")

final_model <- glm(
  data = final_train, GB ~ PERS_H_bin +
    AGE_bin +
    TMJOB1_bin +
    CARDS_bin,
  weights = final_train$X_freq_, family = "binomial"
)
summary(final_model)

final_train$pred <- final_model$fitted.values

smbinning.metrics(
  dataset = final_train, prediction = "pred",
  actualclass = "GB", report = 1
)
smbinning.metrics(
  dataset = final_train, prediction = "pred",
  actualclass = "GB", plot = "ks"
)
smbinning.metrics(
  dataset = final_train, prediction = "pred",
  actualclass = "GB", plot = "auc"
) # AUC is 78.80%

final_bin_model <- smbinning.scaling(final_model, pdo = 50, score = 500, odds = 20)
final_train <- smbinning.scoring.gen(final_bin_model, dataset = final_train)
final_train %>% head()

final_test <- smbinning.gen(df = final_test, ivout = result_all$AGE, chrname = "AGE_bin")
final_test <- smbinning.gen(df = final_test, ivout = result_all$TMJOB1, chrname = "TMJOB1_bin")
final_test <- smbinning.gen(df = final_test, ivout = result_all$PERS_H, chrname = "PERS_H_bin")
final_test <- smbinning.factor.gen(df = final_test, ivout = smbinning.factor(df = final, y = "GB", x = "CARDS"), chrname = "CARDS_bin")

final_test <- smbinning.scoring.gen(final_bin_model, dataset = final_test)
head(final_test)

final_test <- final_test %>%
  mutate(pred = predict(final_model, newdata = final_test, type = "response"))

smbinning.metrics(
  dataset = final_test, prediction = "pred",
  actualclass = "GB", report = 1
)
smbinning.metrics(
  dataset = final_test, prediction = "pred",
  actualclass = "GB", plot = "ks"
)
smbinning.metrics(
  dataset = final_test, prediction = "pred",
  actualclass = "GB", plot = "auc"
) # AUC is 77.59%

hist(final_test$Score, breaks = 50, xlim = c(400, 820), main = "Distribution of Train Scores", xlab = "Score")

accepts_scored_comb <- rbind(final_train, final_test)

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
for (i in min(floor(final_train$Score)):max(floor(final_train$Score))) {
  score[i - min(floor(final_train$Score)) + 1] <- i
  def[i - min(floor(final_train$Score)) + 1] <- 100 * sum(final_train$bad[which(final_train$Score >= i)]) / (length(final_train$bad[which(final_train$Score >= i & final_train$bad == 1)]) + 30 * length(final_train$bad[which(final_train$Score >= i & final_train$bad == 0)]))
  acc[i - min(floor(final_train$Score)) + 1] <- 100 * (length(final_train$bad[which(final_train$Score >= i & final_train$bad == 1)]) + 30 * length(final_train$bad[which(final_train$Score >= i & final_train$bad == 0)])) / (length(final_train$bad[which(final_train$bad == 1)]) + 30 * length(final_train$bad[which(final_train$bad == 0)]))
  prof[i - min(floor(final_train$Score)) + 1] <- length(final_train$bad[which(final_train$Score >= i & final_train$bad == 1)]) * (-cost) + 30 * length(final_train$bad[which(final_train$Score >= i & final_train$bad == 0)]) * profit
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