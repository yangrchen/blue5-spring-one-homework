###############################
#                             #
#     Financial Analytics:    #
#     Scorecard Creation &    #
#       Reject Inference      #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("gmodels")
install.packages("vcd")
install.packages("smbinning")
install.packages("dplyr")
install.packages("stringr")
install.packages("shades")
install.packages("latticeExtra")
install.packages("plotly")

library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(shades)
library(latticeExtra)
library(plotly)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

accepts <- read.csv(file = "accepts.csv", header = TRUE)
rejects <- read.csv(file = "rejects.csv", header = TRUE)

# Understand Target Variable #
table(accepts$bad)

accepts$good <- abs(accepts$bad - 1)
table(accepts$good)

# Setting Categorical Variables as Factors #
accepts$bankruptcy <- as.factor(accepts$bankruptcy)
accepts$used_ind <- as.factor(accepts$used_ind)
accepts$purpose <- as.factor(accepts$purpose)

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.75*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$good)
table(test$good)

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary # Only Continuous Variables >= 0.1 IV #

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
  result_all_sig[[num_names[i]]] <- check_res
  }
}

# Generating Variables of Bins and WOE Values #
for(i in 1:length(result_all_sig)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)

    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

# Build Initial Logistic Regression #
initial_score <- glm(data = train, bad ~ tot_derog_WOE + 
                                         tot_tr_WOE + 
                                         age_oldest_tr_WOE + 
                                         tot_rev_line_WOE +
                                         rev_util_WOE +
                                         bureau_score_WOE +
                                         #down_pyt_WOE +
                                         ltv_WOE, 
                     weights = train$weight, family = "binomial")

summary(initial_score)

# Variable Selected Logistic Regression #
initial_score_red <- glm(data = train, bad ~ #tot_derog_WOE + 
                                         #tot_tr_WOE + 
                                         age_oldest_tr_WOE + 
                                         tot_rev_line_WOE +
                                         rev_util_WOE +
                                         bureau_score_WOE +
                                         #down_pyt_WOE +
                                         ltv_WOE, 
                     weights = train$weight, family = "binomial")

summary(initial_score_red)

# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

#Evaluate the Initial Model - Testing Data #
for(i in 1:length(result_all_sig)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

# Add Scores to Initial Model #
pdo <- 20
score <- 600
odds <- 50
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")

  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(475,725), main = "Distribution of Scores", xlab = "Score")

# Plotting Default by Score in Train Data #
cutpoints <- quantile(train$Score, probs = seq(0,1,0.10))
train$Score.QBin <- cut(train$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.train <- round(table(train$Score.QBin, train$bad)[,2]/rowSums(table(train$Score.QBin, train$bad))*100,2)

print(Default.QBin.train)

Default.QBin.train.pop <- round(table(train$Score.QBin, train$bad)[,2]/(table(train$Score.QBin, train$bad)[,2] + table(train$Score.QBin, train$bad)[,1]*4.75)*100,2)

print(Default.QBin.train.pop)

# Plotting Default by Score in Test Data #
cutpoints <- quantile(test$Score, probs = seq(0,1,0.10))
test$Score.QBin <- cut(test$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.test <- round(table(test$Score.QBin, test$bad)[,2]/rowSums(table(test$Score.QBin, test$bad))*100,2)

print(Default.QBin.test)

barplot(Default.QBin.test, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,60),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 20.5, lwd = 2, lty = "dashed")
text(11.5, 23, "Current = 20.5%")

Default.QBin.test.pop <- round(table(test$Score.QBin, test$bad)[,2]/(table(test$Score.QBin, test$bad)[,2] + table(test$Score.QBin, test$bad)[,1]*4.75)*100,2)

print(Default.QBin.test.pop)

barplot(Default.QBin.test.pop, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,20),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 5.00, lwd = 2, lty = "dashed")
text(11.5, 6, "Current = 5.00%")

# Plotting Default, Acceptance, & Profit By Score #
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL

cost <- 50000
profit <- 1200
for(i in min(floor(train$Score)):max(floor(train$Score))){
  score[i - min(floor(train$Score)) + 1] <- i
  def[i - min(floor(train$Score)) + 1] <- 100*sum(train$bad[which(train$Score >= i)])/(length(train$bad[which(train$Score >= i & train$bad == 1)]) + 4.75*length(train$bad[which(train$Score >= i & train$bad == 0)]))
  acc[i - min(floor(train$Score)) + 1] <- 100*(length(train$bad[which(train$Score >= i & train$bad == 1)]) + 4.75*length(train$bad[which(train$Score >= i & train$bad == 0)]))/(length(train$bad[which(train$bad == 1)]) + 4.75*length(train$bad[which(train$bad == 0)]))
  prof[i - min(floor(train$Score)) + 1] <- length(train$bad[which(train$Score >= i & train$bad == 1)])*(-cost) + 4.75*length(train$bad[which(train$Score >= i & train$bad == 0)])*profit
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
  range = c(0, 5)
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

# Reject Inference - Clean & Prepare Reject Data #
for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
}

for(i in 1:length(rejects[["ltv"]])){
  rejects[["ltv"]][is.na(rejects[["ltv"]])] <- floor(mean(rejects[["ltv"]], na.rm = TRUE))
}

rejects_scored <- rejects
for(i in 1:length(result_all_sig)) {
  rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

pdo <- 20
score <- 600
odds <- 50
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

# Reject Inference - Hard Cut-off #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')
rejects$bad <- as.numeric(rejects_scored$pred > 0.0617)
rejects$good <- abs(rejects$bad - 1)

pop_g <- 22045
pop_b <- 1196

sam_g <- 4641
sam_b <- 1196

pop_sam_gb_ratio <- (pop_g/pop_b)/(sam_g/sam_b)

pop_a <- 0.7
pop_r <- 0.3

sam_a <- 5837
sam_r <- 4233

pop_sam_ar_ratio <- (pop_a/pop_r)/(sam_a/sam_r)

weight_rb <- 1
weight_rg <- pop_sam_gb_ratio

weight_ab <- pop_sam_ar_ratio
weight_ag <- pop_sam_ar_ratio*pop_sam_gb_ratio

accepts$weight_ar <- ifelse(accepts$bad == 1, weight_ab, weight_ag)
rejects$weight_ar <- ifelse(rejects$bad == 1, weight_rb, weight_rg)

comb_hard <- rbind(accepts[, !(names(accepts) == 'weight')], rejects) # New Combined Data Set #

# Reject Inference - Parcelling #
parc <- seq(500, 725, 25)

accepts_scored$Score_parc <- cut(accepts_scored$Score, breaks = parc)
rejects_scored$Score_parc <- cut(rejects_scored$Score, breaks = parc)

table(accepts_scored$Score_parc, accepts_scored$bad)

parc_perc <- table(accepts_scored$Score_parc, accepts_scored$bad)[,2]/rowSums(table(accepts_scored$Score_parc, accepts_scored$bad))

rejects$bad <- 0

rej_bump <- 1.25

for(i in 1:(length(parc)-1)) {
  for(j in 1:length(rejects_scored$Score)) {
    if((rejects_scored$Score[j] > parc[i]) & 
       (rejects_scored$Score[j] <= parc[i+1]) & 
       (runif(n = 1, min = 0, max = 1) < (rej_bump*parc_perc[i]))) {
      rejects$bad[j] <- 1
    }
  }
}

table(rejects_scored$Score_parc, rejects$bad)

rejects$good <- abs(rejects$bad - 1)

accepts$weight_ar <- ifelse(accepts$bad == 1, weight_ab, weight_ag)
rejects$weight_ar <- ifelse(rejects$bad == 1, weight_rb, weight_rg)

comb_parc <- rbind(accepts[, !(names(accepts) == 'weight')], rejects) # New Combined Data Set #

# Reject Inference - Fuzzy Augmentation #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects_g <- rejects
rejects_b <- rejects

rejects_g$bad <- 0
rejects_g$weight_ar <- (1-rejects_scored$pred)*weight_rg
rejects_g$good <- 1

rejects_b$bad <- 1
rejects_b$weight_ar <- (rejects_scored$pred)*weight_rb
rejects_b$good <- 0

accepts$weight_ar <- ifelse(accepts$bad == 1, weight_ab, weight_ag)
comb_fuzz <- rbind(accepts[, !(names(accepts) == 'weight')], rejects_g, rejects_b)

# Build Final Scorecard Model - Basically Repeating ALL Steps with New Data #
comb <- comb_parc # Select which data set you want to use from above techniques #

set.seed(12345)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.75*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

iv_summary <- smbinning.sumiv(df = train_comb, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary

num_names <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train_comb, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}

for(i in 1:length(result_all_sig)) {
  train_comb <- smbinning.gen(df = train_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train_comb)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

final_score <- glm(data = train_comb, bad ~ tot_tr_WOE +
                                            tot_derog_WOE +
                                            age_oldest_tr_WOE +
                                            tot_rev_line_WOE +
                                            rev_util_WOE +
                                            bureau_score_WOE +
                                            ltv_WOE
                     , weights = train_comb$weight_ar, family = "binomial")

summary(final_score)

train_comb$pred <- final_score$fitted.values

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

for(i in 1:length(result_all_sig)) {
  test_comb <- smbinning.gen(df = test_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test_comb)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test_comb$pred <- predict(final_score, newdata=test_comb, type='response')

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 0, plot = "auc")

pdo <- 20
score <- 600
odds <- 50
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(final_score$coefficients[-1])

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  train_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train_comb)-nvar + 1)
colend <- ncol(train_comb)
train_comb$Score <- rowSums(train_comb[, colini:colend])

hist(train_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test_comb)-nvar + 1)
colend <- ncol(test_comb)
test_comb$Score <- rowSums(test_comb[, colini:colend])

hist(test_comb$Score, breaks = 50, main = "Distribution of Test Scores", xlab = "Score")

accepts_scored_comb <- rbind(train_comb, test_comb)
hist(accepts_scored_comb$Score, breaks = 50, xlim = c(450,650), main = "Distribution of Scores", xlab = "Score")

cutpoints <- quantile(accepts_scored_comb$Score, probs = seq(0,1,0.10))
accepts_scored_comb$Score.QBin <- cut(accepts_scored_comb$Score, breaks=cutpoints, include.lowest=TRUE)
Default.QBin.pop <- round(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,2]/(table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,2] + table(accepts_scored_comb$Score.QBin, accepts_scored_comb$bad)[,1]*4.75)*100,2)

print(Default.QBin.pop)

barplot(Default.QBin.pop, 
        main = "Default Decile Plot", 
        xlab = "Deciles of Scorecard",
        ylab = "Default Rate (%)", ylim = c(0,20),
        col = saturation(heat.colors, scalefac(0.8))(10))
abline(h = 5, lwd = 2, lty = "dashed")
text(11.5, 5, "Current = 5.00%")

# Plotting Default, Acceptance, & Profit By Score #
def <- NULL
acc <- NULL
prof <- NULL
score <- NULL

cost <- 50000
profit <- 1200
for(i in min(floor(train_comb$Score)):max(floor(train_comb$Score))){
  score[i - min(floor(train_comb$Score)) + 1] <- i
  def[i - min(floor(train_comb$Score)) + 1] <- 100*sum(train_comb$bad[which(train_comb$Score >= i)])/(length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)]) + 4.75*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)]))
  acc[i - min(floor(train_comb$Score)) + 1] <- 100*(length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)]) + 4.75*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)]))/(length(train_comb$bad[which(train_comb$bad == 1)]) + 4.75*length(train_comb$bad[which(train_comb$bad == 0)]))
  prof[i - min(floor(train_comb$Score)) + 1] <- length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 1)])*(-cost) + 4.75*length(train_comb$bad[which(train_comb$Score >= i & train_comb$bad == 0)])*profit
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
