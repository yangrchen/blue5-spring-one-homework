library(readr)
library(smbinning)
library(gmodels)
library(vcd)
library(dplyr)
library(forcats)


#reading in the dataset and getting all of the varaiables correct
whole <- read.csv("/Users/samdotson/Downloads/Homework1_FA/accepted_customers.csv")
accepts <- read.csv("/Users/samdotson/Downloads/Homework1_FA/accepted_customers.csv")

whole <- as.data.frame(whole)
whole <- whole %>% mutate(GB = ifelse(GB == 1, 0, 1))

whole$CHILDREN <- as.factor(whole$CHILDREN)
whole$TEL <- as.factor(whole$TEL)
whole$NMBLOAN <- as.factor(whole$NMBLOAN)
whole$FINLOAN <- as.factor(whole$FINLOAN)
whole$EC_CARD <- as.factor(whole$EC_CARD)
whole$BUREAU <- as.factor(whole$BUREAU)
whole$LOCATION <- as.factor(whole$LOCATION)
whole$REGN <- as.factor(whole$REGN)
whole$DIV <- as.factor(whole$DIV)
whole$PRODUCT <- as.factor(whole$PRODUCT)
whole$RESID <- as.factor(whole$RESID)
whole$NAT <- as.factor(whole$NAT)
whole$PROF <- as.factor(whole$PROF)
whole$CAR <- as.factor(whole$CAR)
whole$CARDS <- as.factor(whole$CARDS)
whole$GB <- as.numeric(whole$GB)

whole <- whole %>%
  mutate(CARDS = fct_collapse(CARDS, "Other credit car" = c("American Express")))

whole <- whole %>%
  mutate(CARDS = fct_collapse(CARDS, "VISA Others" = c("VISA mybank")))

unique(whole$CARDS)

accepts <- whole

unique(accepts$CARDS)

whole$CARDS <- as.factor(whole$CARDS)

set.seed(12345)
train_id <- sample(seq_len(nrow(whole)), size = floor(0.70*nrow(whole)))

train <- whole[train_id, ]
test <- whole[-train_id, ]

result <- smbinning(df = train, y = "GB", x = "INCOME")
result$ivtable


iv_summary <- smbinning.sumiv(df = train, y = "GB")
smbinning.sumiv.plot(iv_summary)

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() 

for(i in 1:length(num_names)){
  result_all[[num_names[i]]] <- smbinning(df = train, y = "GB", x = num_names[i])
}

#all variables with IV above 0.1
train <- smbinning.gen(df = train, ivout=result_all$AGE, chrname = "AGE_bin")
train <- smbinning.gen(df = train, ivout=result_all$INCOME, chrname = "INCOME_bin")
train <- smbinning.gen(df = train, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
train <- smbinning.gen(df = train, ivout=result_all$PERS_H, chrname = "PERS_H_bin")
train <- smbinning.factor.gen(df = train, ivout=smbinning.factor(df = train, y = "GB", x = "CARDS"), chrname = "CARDS_bin")


initial_model <- glm(data = train, GB ~  PERS_H_bin +
                       AGE_bin + 
                       TMJOB1_bin +
                       INCOME_bin +
                       CARDS_bin, 
                     weights = train$X_freq_, family = "binomial")

summary(initial_model)




train <- train %>%
  mutate(pred = initial_model$fitted.values)


smbinning.metrics(dataset = train, prediction = "pred",
                  actualclass = "GB", report = 1)
smbinning.metrics(dataset = train, prediction = "pred",
                  actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred",
                  actualclass = "GB", plot = "auc") #AUC on train is 70.24%

bin_model <- smbinning.scaling(initial_model, pdo = 50, score = 500, odds = 20)
train_bin <- smbinning.scoring.gen(bin_model, dataset = train)
train_bin %>% head()


test <- smbinning.gen(df = test, ivout=result_all$AGE, chrname = "AGE_bin")
test <- smbinning.gen(df = test, ivout=result_all$INCOME, chrname = "INCOME_bin")
test <- smbinning.gen(df = test, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
test <- smbinning.gen(df = test, ivout=result_all$PERS_H, chrname = "PERS_H_bin")

test <- smbinning.factor.gen(df = test, ivout=smbinning.factor(df = train, y = "GB", 
                                                               x = "CARDS"), 
                             chrname = "CARDS_bin")

test_bin <- smbinning.scoring.gen(bin_model, dataset = test)
head(test_bin)

test <- test %>%
  mutate(pred = predict(initial_model, test))

smbinning.metrics(dataset = test, prediction = "pred",
                  actualclass = "GB", report = 1)
smbinning.metrics(dataset = test, prediction = "pred",
                  actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred",
                  actualclass = "GB", plot = "auc") #AUC on test is 72.58%

#reading in the rejects dataset and getting all of the varaiables correct
rejects <- read.csv("/Users/samdotson/Downloads/Homework1_FA/rejected_customers.csv")
reject <- read.csv("/Users/samdotson/Downloads/Homework1_FA/rejected_customers.csv")
rejects <- as.data.frame(rejects)

set.seed(1234)
train_id <- sample(seq_len(nrow(rejects)), size = 1000)
rejects <- rejects[train_id, ]
dim(rejects)

 

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

unique(rejects$CARDS)

rejects$CARDS <- as.factor(rejects$CARDS)
reject <- rejects
unique(reject$CARDS)

num_names <- names(rejects)[sapply(whole, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() 

for(i in 1:length(num_names)){
  result_all[[num_names[i]]] <- smbinning(df = whole, y = "GB", x = num_names[i])
}


whole <- smbinning.gen(df = whole, ivout=result_all$AGE, chrname = "AGE_bin")
whole <- smbinning.gen(df = whole, ivout=result_all$INCOME, chrname = "INCOME_bin")
whole <- smbinning.gen(df = whole, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
whole <- smbinning.gen(df = whole, ivout=result_all$PERS_H, chrname = "PERS_H_bin")
whole <- smbinning.factor.gen(df = whole, ivout=smbinning.factor(df = whole, y = "GB", x = "CARDS"), chrname = "CARDS_bin")


rejects <- smbinning.gen(df = rejects, ivout=result_all$AGE, chrname = "AGE_bin")
rejects <- smbinning.gen(df = rejects, ivout=result_all$INCOME, chrname = "INCOME_bin")
rejects <- smbinning.gen(df = rejects, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
rejects <- smbinning.gen(df = rejects, ivout=result_all$PERS_H, chrname = "PERS_H_bin")
rejects <- smbinning.factor.gen(df = rejects, ivout=smbinning.factor(df = whole, y = "GB", x = "CARDS"), chrname = "CARDS_bin")


new_model <- glm(data = whole, GB ~  PERS_H_bin +
                       AGE_bin + 
                       TMJOB1_bin +
                       INCOME_bin +
                       CARDS_bin, 
                     weights = whole$X_freq_, family = "binomial")

summary(new_model)



rejects$pred <- predict(new_model, newdata = rejects, type = 'response')
reject$GB <- as.numeric(rejects$pred > 0.9643) 
reject$X_freq_ <- ifelse(reject$GB == 1, 30, 1) 

comb_hard <- rbind(accepts, reject)

final <- comb_hard

set.seed(12345)
final_id <- sample(seq_len(nrow(final)), size = floor(0.70*nrow(final)))
final_train <- final[final_id, ]
final_test <- final[-final_id, ]

iv_summary <- smbinning.sumiv(df = final_train, y = "GB")
smbinning.sumiv.plot(iv_summary)

num_names2 <- names(final_train)[sapply(final_train, is.numeric)] # Gathering the names of numeric variables in data #

result_all <- list() 

for(i in 1:length(num_names2)){
  result_all[[num_names2[i]]] <- smbinning(df = final_train, y = "GB", x = num_names2[i])
}

final_train <- smbinning.gen(df = final_train, ivout=result_all$AGE, chrname = "AGE_bin")
#final_train <- smbinning.gen(df = final_train, ivout=result_all$INCOME, chrname = "INCOME_bin")
final_train <- smbinning.gen(df = final_train, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
final_train <- smbinning.gen(df = final_train, ivout=result_all$PERS_H, chrname = "PERS_H_bin")
final_train <- smbinning.factor.gen(df = final_train, ivout=smbinning.factor(df = final, y = "GB", x = "CARDS"), chrname = "CARDS_bin")


final_model <- glm(data = final_train, GB ~  PERS_H_bin +
                       AGE_bin + 
                       TMJOB1_bin +
                       #INCOME_bin +
                       CARDS_bin, 
                     weights = final_train$X_freq_, family = "binomial")

summary(final_model)

final_bin_model <- smbinning.scaling(final_model, pdo = 50, score = 500, odds = 20)
final_train_bin <- smbinning.scoring.gen(final_bin_model, dataset = final_train)
final_train_bin %>% head()




final_test <- smbinning.gen(df = final_test, ivout=result_all$AGE, chrname = "AGE_bin")
#final_test <- smbinning.gen(df = final_test, ivout=result_all$INCOME, chrname = "INCOME_bin")
final_test <- smbinning.gen(df = final_test, ivout=result_all$TMJOB1, chrname = "TMJOB1_bin")
final_test <- smbinning.gen(df = final_test, ivout=result_all$PERS_H, chrname = "PERS_H_bin")
final_test <- smbinning.factor.gen(df = final_test, ivout=smbinning.factor(df = final, y = "GB", x = "CARDS"), chrname = "CARDS_bin")



final_test_bin <- smbinning.scoring.gen(final_bin_model, dataset = final_test)
head(final_test_bin)

final_test <- final_test %>%
  mutate(pred = predict(final_model, final_test))

smbinning.metrics(dataset = final_test, prediction = "pred",
                  actualclass = "GB", report = 1)
smbinning.metrics(dataset = final_test, prediction = "pred",
                  actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = final_test, prediction = "pred",
                  actualclass = "GB", plot = "auc") #AUC is 77.78%

hist(final_test_bin$Score, breaks = 50, xlim = c(400, 820), main = "Distribution of Train Scores", xlab = "Score")

accepts_scored_comb <- rbind(final_train_bin, final_test_bin)
accepts_scored_comb$bad <- abs(accepts_scored_comb$GB-1)

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

final_train_bin$bad <- abs(final_train_bin$GB-1)

library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

def <- NULL
acc <- NULL
prof <- NULL
score <- NULL

cost <- 50000
profit <- 1200
for(i in min(floor(final_train_bin$Score)):max(floor(final_train_bin$Score))){
  score[i - min(floor(final_train_bin$Score)) + 1] <- i
  def[i - min(floor(final_train_bin$Score)) + 1] <- 100*sum(final_train_bin$bad[which(final_train_bin$Score >= i)])/(length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 1)]) + 30*length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 0)]))
  acc[i - min(floor(final_train_bin$Score)) + 1] <- 100*(length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 1)]) + 30*length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 0)]))/(length(final_train_bin$bad[which(final_train_bin$bad == 1)]) + 30*length(final_train_bin$bad[which(final_train_bin$bad == 0)]))
  prof[i - min(floor(final_train_bin$Score)) + 1] <- length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 1)])*(-cost) + 30*length(final_train_bin$bad[which(final_train_bin$Score >= i & final_train_bin$bad == 0)])*profit
}

plot_data <- data.frame(def, acc, prof, score)

def_plot <- xyplot(def ~ score, plot_data, 
                   type = "l" , lwd=2, col="red",
                   ylab = "Default Rate (%)",
                   xlab = "Score",
                   main = "Default Rate by Acceptance Across Score",
                   panel = function(x, y,...) {
                     panel.xyplot(x, y, ...)
                     panel.abline(h = 5.00, col = "red")
                   })
acc_plot <- xyplot(acc ~ score, plot_data, 
                   type = "l", lwd=2, col="blue",
                   ylab = "Acceptance Rate (%)",
                   panel = function(x, y,...) {
                     panel.xyplot(x, y, ...)
                     panel.abline(h = 70, col = "blue")
                   })
prof_plot <- xyplot(prof/1000 ~ score, plot_data, 
                    type = "l" , lwd=2, col="green",
                    ylab = "Profit (Thousands $)",
                    xlab = "Score",
                    main = "Profit by Acceptance Across Score"
)
doubleYScale(def_plot, acc_plot, add.ylab2 = TRUE, use.style=FALSE)
doubleYScale(prof_plot, acc_plot, add.ylab2 = TRUE, use.style=FALSE)



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
  xaxis = list(title="Scorecard Value"),
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
  xaxis = list(title="Scorecard Value"),
  legend = list(x = 1.2, y = 0.8)
)

fig



