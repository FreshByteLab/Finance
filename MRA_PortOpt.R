library(tseries)
library(lubridate)
library(graphics)
library(readit)
library(PerformanceAnalytics)
library(openxlsx)

riskless_ind = TRUE
w_max = c(1,1,1)

len_tot <- length(r_import$date)

date <- r_import$date
ret <- r_import[,5:7]
ret <- xts(x = ret,order.by = as.Date(date))

len_sa <- (length(endpoints(ret,on="quarters"))+1)/2
t_sa <- endpoints(ret,on="quarters")[c(TRUE,FALSE)]
t_sa[1] <- 1
m <- diff(t_sa)
m[1] <- m[1]+1

# extending window Wavelet portfolio

er_W1_ext <- NULL
er_W2_ext <- NULL
er_W3_ext <- NULL
er_W4_ext <- NULL
er_W5_ext <- NULL
er_W6_ext <- NULL
er_W7_ext <- NULL

for (i in 1:(len_sa-2)) {
  er_W1_ext[[i]] <- colMeans(W1[[i]][t_sa[1]:t_sa[i+1],])
  er_W2_ext[[i]] <- colMeans(W2[[i]][t_sa[1]:t_sa[i+1],])
  er_W3_ext[[i]] <- colMeans(W3[[i]][t_sa[1]:t_sa[i+1],])
  er_W4_ext[[i]] <- colMeans(W4[[i]][t_sa[1]:t_sa[i+1],])
  er_W5_ext[[i]] <- colMeans(W5[[i]][t_sa[1]:t_sa[i+1],])
  er_W6_ext[[i]] <- colMeans(W6[[i]][t_sa[1]:t_sa[i+1],])
  er_W7_ext[[i]] <- colMeans(W7[[i]][t_sa[1]:t_sa[i+1],])
}

rvec_W1_ext <- NULL
rvec_W2_ext <- NULL
rvec_W3_ext <- NULL
rvec_W4_ext <- NULL
rvec_W5_ext <- NULL
rvec_W6_ext <- NULL
rvec_W7_ext <- NULL

# rvec_W1[[1]] <- matrix(rep(er_W1[[1]],each=m[1]),nrow=m[1])
# rvec_W2[[1]] <- matrix(rep(er_W2[[1]],each=m[1]),nrow=m[1])
# rvec_W3[[1]] <- matrix(rep(er_W3[[1]],each=m[1]),nrow=m[1])
# rvec_W4[[1]] <- matrix(rep(er_W4[[1]],each=m[1]),nrow=m[1])
# rvec_W5[[1]] <- matrix(rep(er_W5[[1]],each=m[1]),nrow=m[1])
# rvec_W6[[1]] <- matrix(rep(er_W6[[1]],each=m[1]),nrow=m[1])
# rvec_W7[[1]] <- matrix(rep(er_W7[[1]],each=m[1]),nrow=m[1])

# for (i in 2:(len_sa-1)) {
#   rvec_W1[[i]] <- rbind(rvec_W1[[i-1]],matrix(rep(er_W1[[i]],each=m[i]),nrow=m[i]))
#   rvec_W2[[i]] <- rbind(rvec_W2[[i-1]],matrix(rep(er_W2[[i]],each=m[i]),nrow=m[i]))
#   rvec_W3[[i]] <- rbind(rvec_W3[[i-1]],matrix(rep(er_W3[[i]],each=m[i]),nrow=m[i]))
#   rvec_W4[[i]] <- rbind(rvec_W4[[i-1]],matrix(rep(er_W4[[i]],each=m[i]),nrow=m[i]))
#   rvec_W5[[i]] <- rbind(rvec_W5[[i-1]],matrix(rep(er_W5[[i]],each=m[i]),nrow=m[i]))
#   rvec_W6[[i]] <- rbind(rvec_W6[[i-1]],matrix(rep(er_W6[[i]],each=m[i]),nrow=m[i]))
#   rvec_W7[[i]] <- rbind(rvec_W7[[i-1]],matrix(rep(er_W7[[i]],each=m[i]),nrow=m[i]))
# }

for (i in 1:(len_sa-2)) {
  rvec_W1_ext[[i]] <- matrix(rep(er_W1_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W2_ext[[i]] <- matrix(rep(er_W2_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W3_ext[[i]] <- matrix(rep(er_W3_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W4_ext[[i]] <- matrix(rep(er_W4_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W5_ext[[i]] <- matrix(rep(er_W5_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W6_ext[[i]] <- matrix(rep(er_W6_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W7_ext[[i]] <- matrix(rep(er_W7_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
}

for (i in 1:(len_sa-2)) {
  rvec_W1_ext[[i]] <- xts(x = rvec_W1_ext[[i]],order.by = as.Date(date[1:(length(rvec_W1_ext[[i]])/3)]))
  rvec_W2_ext[[i]] <- xts(x = rvec_W2_ext[[i]],order.by = as.Date(date[1:(length(rvec_W2_ext[[i]])/3)]))
  rvec_W3_ext[[i]] <- xts(x = rvec_W3_ext[[i]],order.by = as.Date(date[1:(length(rvec_W3_ext[[i]])/3)]))
  rvec_W4_ext[[i]] <- xts(x = rvec_W4_ext[[i]],order.by = as.Date(date[1:(length(rvec_W4_ext[[i]])/3)]))
  rvec_W5_ext[[i]] <- xts(x = rvec_W5_ext[[i]],order.by = as.Date(date[1:(length(rvec_W5_ext[[i]])/3)]))
  rvec_W6_ext[[i]] <- xts(x = rvec_W6_ext[[i]],order.by = as.Date(date[1:(length(rvec_W6_ext[[i]])/3)]))
  rvec_W7_ext[[i]] <- xts(x = rvec_W7_ext[[i]],order.by = as.Date(date[1:(length(rvec_W7_ext[[i]])/3)]))
}

W1_ext <- NULL
W2_ext <- NULL
W3_ext <- NULL
W4_ext <- NULL
W5_ext <- NULL
W6_ext <- NULL
W7_ext <- NULL

for (i in 1:(len_sa-2)) {
  W1_ext[[i]] <- portfolio.optim(rvec_W1_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W1_ext[[i]])
  W2_ext[[i]] <- portfolio.optim(rvec_W2_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W2_ext[[i]])
  W3_ext[[i]] <- portfolio.optim(rvec_W3_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W3_ext[[i]])
  W4_ext[[i]] <- portfolio.optim(rvec_W4_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W4_ext[[i]])
  W5_ext[[i]] <- portfolio.optim(rvec_W5_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W5_ext[[i]])
  W6_ext[[i]] <- portfolio.optim(rvec_W6_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W6_ext[[i]])
  W7_ext[[i]] <- portfolio.optim(rvec_W7_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W7_ext[[i]])
}

for (i in 1:(len_sa-2)) {
  W1_ext[[i]]$px <- c(tcrossprod(W1_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W2_ext[[i]]$px <- c(tcrossprod(W2_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W3_ext[[i]]$px <- c(tcrossprod(W3_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W4_ext[[i]]$px <- c(tcrossprod(W4_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W5_ext[[i]]$px <- c(tcrossprod(W5_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W6_ext[[i]]$px <- c(tcrossprod(W6_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W7_ext[[i]]$px <- c(tcrossprod(W7_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

W1_ext_con <- NULL
W1_ext_ann <- NULL
W1_ext_cum <- NULL

W1_ext_con <- c(W1_ext[[1]]$px,head(W1_ext[[2]]$px,-1),head(W1_ext[[3]]$px,-1),head(W1_ext[[4]]$px,-1),head(W1_ext[[5]]$px,-1),head(W1_ext[[6]]$px,-1),head(W1_ext[[7]]$px,-1),head(W1_ext[[8]]$px,-1),head(W1_ext[[9]]$px,-1),head(W1_ext[[10]]$px,-1))

W1_ext_cum_sca <- Return.cumulative(W1_ext_con,geometric = TRUE)
W1_ext_cum_sca

len <- length(W1_ext_con)

for (i in 1:(len)) {
  W1_ext_cum[i] = Return.cumulative(W1_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W1_ext_ann[i] = Return.annualized(W1_ext_con[1:i], scale = 252,geometric = TRUE)
}

W1_ext_ann_sca = Return.annualized(W1_ext_con, scale = 252,geometric = TRUE)
W1_ext_ann_sca

ts.plot(as.ts(W1_ext_cum),as.ts(dcc_cum))

W1_ext_w <- NULL
W1_ext_w <- c(W1_ext[[1]]$pw,W1_ext[[2]]$pw,W1_ext[[3]]$pw,W1_ext[[4]]$pw,W1_ext[[5]]$pw,W1_ext[[6]]$pw,W1_ext[[7]]$pw,W1_ext[[8]]$pw,W1_ext[[9]]$pw,W1_ext[[10]]$pw)
W1_ext_w <- matrix(W1_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W1_ext_w

W1_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W1_ext_w_excel <- c(W1_ext_w_excel,replicate(m[i],W1_ext[[i-1]]$pw))
}

W1_ext_w_excel <- matrix(W1_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W1_ext_perf <- W1_ext_cum

W2_ext_con <- NULL
W2_ext_ann <- NULL
W2_ext_cum <- NULL

W2_ext_con <- c(W2_ext[[1]]$px,head(W2_ext[[2]]$px,-1),head(W2_ext[[3]]$px,-1),head(W2_ext[[4]]$px,-1),head(W2_ext[[5]]$px,-1),head(W2_ext[[6]]$px,-1),head(W2_ext[[7]]$px,-1),head(W2_ext[[8]]$px,-1),head(W2_ext[[9]]$px,-1),head(W2_ext[[10]]$px,-1))

W2_ext_cum_sca <- Return.cumulative(W2_ext_con,geometric = TRUE)
W2_ext_cum_sca

len <- length(W2_ext_con)

for (i in 1:(len)) {
  W2_ext_cum[i] = Return.cumulative(W2_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W2_ext_ann[i] = Return.annualized(W2_ext_con[1:i], scale = 252,geometric = TRUE)
}

W2_ext_ann_sca = Return.annualized(W2_ext_con, scale = 252,geometric = TRUE)
W2_ext_ann_sca

ts.plot(as.ts(W2_ext_cum))

W2_ext_w <- NULL
W2_ext_w <- c(W2_ext[[1]]$pw,W2_ext[[2]]$pw,W2_ext[[3]]$pw,W2_ext[[4]]$pw,W2_ext[[5]]$pw,W2_ext[[6]]$pw,W2_ext[[7]]$pw,W2_ext[[8]]$pw,W2_ext[[9]]$pw,W2_ext[[10]]$pw)
W2_ext_w <- matrix(W2_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W2_ext_w

W2_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W2_ext_w_excel <- c(W2_ext_w_excel,replicate(m[i],W2_ext[[i-1]]$pw))
}

W2_ext_w_excel <- matrix(W2_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W2_ext_perf <- W2_ext_cum

W2_ext_sd <- StdDev.annualized(W2_ext_con, scale = 252,geometric = TRUE)
W2_ext_sk <- skewness(W2_ext_con, method = "sample")
W2_ext_ku <- kurtosis(W2_ext_con, method = "sample")
W2_ext_CVaR <- CVaR(W2_ext_con,p = 0.95,method = "historical")[1]
W2_ext_GtP <- sum(W2_ext_con[W2_ext_con>0])/abs(sum(W2_ext_con[W2_ext_con<0]))
W2_ext_PR <- PainRatio(as.xts(x = W2_ext_con,order.by = date[-(1:127)]))[1]
W2_ext_ShR <- SharpeRatio(as.xts(x = W2_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W2_ext_SoR <- SortinoRatio(W2_ext_con,MAR = 0)[1]
W2_ext_SemiD <- SemiDeviation(W2_ext_con)[1]
W2_ext_om <- Omega(W2_ext_con,L = 0,method = "simple",output = "point")
W2_ext_ka <- Kappa(W2_ext_con,MAR = 0,l=3)
W2_ext_MD <- maxDrawdown(as.xts(x = W2_ext_con,order.by = date[-(1:127)]))
W2_ext_awchg <- mean(abs(diff(W2_ext_w)))
W2_ext_alev <- mean((1-rowSums(W2_ext_w))*-1)
W2_ext_maxlev <- max((1-rowSums(W2_ext_w))*-1)
W2_ext_minlev <- min((1-rowSums(W2_ext_w))*-1)

W3_ext_con <- NULL
W3_ext_ann <- NULL
W3_ext_cum <- NULL

W3_ext_con <- c(W3_ext[[1]]$px,head(W3_ext[[2]]$px,-1),head(W3_ext[[3]]$px,-1),head(W3_ext[[4]]$px,-1),head(W3_ext[[5]]$px,-1),head(W3_ext[[6]]$px,-1),head(W3_ext[[7]]$px,-1),head(W3_ext[[8]]$px,-1),head(W3_ext[[9]]$px,-1),head(W3_ext[[10]]$px,-1))

W3_ext_cum_sca <- Return.cumulative(W3_ext_con,geometric = TRUE)
W3_ext_cum_sca

len <- length(W3_ext_con)

for (i in 1:(len)) {
  W3_ext_cum[i] = Return.cumulative(W3_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W3_ext_ann[i] = Return.annualized(W3_ext_con[1:i], scale = 252,geometric = TRUE)
}

W3_ext_ann_sca = Return.annualized(W3_ext_con, scale = 252,geometric = TRUE)
W3_ext_ann_sca

ts.plot(as.ts(W3_ext_cum))

W3_ext_w <- NULL
W3_ext_w <- c(W3_ext[[1]]$pw,W3_ext[[2]]$pw,W3_ext[[3]]$pw,W3_ext[[4]]$pw,W3_ext[[5]]$pw,W3_ext[[6]]$pw,W3_ext[[7]]$pw,W3_ext[[8]]$pw,W3_ext[[9]]$pw,W3_ext[[10]]$pw)
W3_ext_w <- matrix(W3_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W3_ext_w

W3_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W3_ext_w_excel <- c(W3_ext_w_excel,replicate(m[i],W3_ext[[i-1]]$pw))
}

W3_ext_w_excel <- matrix(W3_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W3_ext_perf <- W3_ext_cum

W3_ext_sd <- StdDev.annualized(W3_ext_con, scale = 252,geometric = TRUE)
W3_ext_sk <- skewness(W3_ext_con, method = "sample")
W3_ext_ku <- kurtosis(W3_ext_con, method = "sample")
W3_ext_CVaR <- CVaR(W3_ext_con,p = 0.95,method = "historical")[1]
W3_ext_GtP <- sum(W3_ext_con[W3_ext_con>0])/abs(sum(W3_ext_con[W3_ext_con<0]))
W3_ext_PR <- PainRatio(as.xts(x = W3_ext_con,order.by = date[-(1:127)]))[1]
W3_ext_ShR <- SharpeRatio(as.xts(x = W3_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W3_ext_SoR <- SortinoRatio(W3_ext_con,MAR = 0)[1]
W3_ext_SemiD <- SemiDeviation(W3_ext_con)[1]
W3_ext_om <- Omega(W3_ext_con,L = 0,method = "simple",output = "point")
W3_ext_ka <- Kappa(W3_ext_con,MAR = 0,l=3)
W3_ext_MD <- maxDrawdown(as.xts(x = W3_ext_con,order.by = date[-(1:127)]))
W3_ext_awchg <- mean(abs(diff(W3_ext_w)))
W3_ext_alev <- mean((1-rowSums(W3_ext_w))*-1)
W3_ext_maxlev <- max((1-rowSums(W3_ext_w))*-1)
W3_ext_minlev <- min((1-rowSums(W3_ext_w))*-1)

W4_ext_con <- NULL
W4_ext_ann <- NULL
W4_ext_cum <- NULL

W4_ext_con <- c(W4_ext[[1]]$px,head(W4_ext[[2]]$px,-1),head(W4_ext[[3]]$px,-1),head(W4_ext[[4]]$px,-1),head(W4_ext[[5]]$px,-1),head(W4_ext[[6]]$px,-1),head(W4_ext[[7]]$px,-1),head(W4_ext[[8]]$px,-1),head(W4_ext[[9]]$px,-1),head(W4_ext[[10]]$px,-1))

W4_ext_cum_sca <- Return.cumulative(W4_ext_con,geometric = TRUE)
W4_ext_cum_sca

len <- length(W4_ext_con)

for (i in 1:(len)) {
  W4_ext_cum[i] = Return.cumulative(W4_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W4_ext_ann[i] = Return.annualized(W4_ext_con[1:i], scale = 252,geometric = TRUE)
}

W4_ext_ann_sca = Return.annualized(W4_ext_con, scale = 252,geometric = TRUE)
W4_ext_ann_sca

ts.plot(as.ts(W4_ext_cum))

W4_ext_w <- NULL
W4_ext_w <- c(W4_ext[[1]]$pw,W4_ext[[2]]$pw,W4_ext[[3]]$pw,W4_ext[[4]]$pw,W4_ext[[5]]$pw,W4_ext[[6]]$pw,W4_ext[[7]]$pw,W4_ext[[8]]$pw,W4_ext[[9]]$pw,W4_ext[[10]]$pw)
W4_ext_w <- matrix(W4_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W4_ext_w

W4_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W4_ext_w_excel <- c(W4_ext_w_excel,replicate(m[i],W4_ext[[i-1]]$pw))
}

W4_ext_w_excel <- matrix(W4_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W4_ext_perf <- W4_ext_cum

W4_ext_sd <- StdDev.annualized(W4_ext_con, scale = 252,geometric = TRUE)
W4_ext_sk <- skewness(W4_ext_con, method = "sample")
W4_ext_ku <- kurtosis(W4_ext_con, method = "sample")
W4_ext_CVaR <- CVaR(W4_ext_con,p = 0.95,method = "historical")[1]
W4_ext_GtP <- sum(W4_ext_con[W4_ext_con>0])/abs(sum(W4_ext_con[W4_ext_con<0]))
W4_ext_PR <- PainRatio(as.xts(x = W4_ext_con,order.by = date[-(1:127)]))[1]
W4_ext_ShR <- SharpeRatio(as.xts(x = W4_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W4_ext_SoR <- SortinoRatio(W4_ext_con,MAR = 0)[1]
W4_ext_SemiD <- SemiDeviation(W4_ext_con)[1]
W4_ext_om <- Omega(W4_ext_con,L = 0,method = "simple",output = "point")
W4_ext_ka <- Kappa(W4_ext_con,MAR = 0,l=3)
W4_ext_MD <- maxDrawdown(as.xts(x = W4_ext_con,order.by = date[-(1:127)]))
W4_ext_awchg <- mean(abs(diff(W4_ext_w)))
W4_ext_alev <- mean((1-rowSums(W4_ext_w))*-1)
W4_ext_maxlev <- max((1-rowSums(W4_ext_w))*-1)
W4_ext_minlev <- min((1-rowSums(W4_ext_w))*-1)

W5_ext_con <- NULL
W5_ext_ann <- NULL
W5_ext_cum <- NULL

W5_ext_con <- c(W5_ext[[1]]$px,head(W5_ext[[2]]$px,-1),head(W5_ext[[3]]$px,-1),head(W5_ext[[4]]$px,-1),head(W5_ext[[5]]$px,-1),head(W5_ext[[6]]$px,-1),head(W5_ext[[7]]$px,-1),head(W5_ext[[8]]$px,-1),head(W5_ext[[9]]$px,-1),head(W5_ext[[10]]$px,-1))

W5_ext_cum_sca <- Return.cumulative(W5_ext_con,geometric = TRUE)
W5_ext_cum_sca

len <- length(W5_ext_con)

for (i in 1:(len)) {
  W5_ext_cum[i] = Return.cumulative(W5_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W5_ext_ann[i] = Return.annualized(W5_ext_con[1:i], scale = 252,geometric = TRUE)
}

W5_ext_ann_sca = Return.annualized(W5_ext_con, scale = 252,geometric = TRUE)
W5_ext_ann_sca

ts.plot(as.ts(W5_ext_cum))

W5_ext_w <- NULL
W5_ext_w <- c(W5_ext[[1]]$pw,W5_ext[[2]]$pw,W5_ext[[3]]$pw,W5_ext[[4]]$pw,W5_ext[[5]]$pw,W5_ext[[6]]$pw,W5_ext[[7]]$pw,W5_ext[[8]]$pw,W5_ext[[9]]$pw,W5_ext[[10]]$pw)
W5_ext_w <- matrix(W5_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W5_ext_w

W5_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W5_ext_w_excel <- c(W5_ext_w_excel,replicate(m[i],W5_ext[[i-1]]$pw))
}

W5_ext_w_excel <- matrix(W5_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W5_ext_perf <- W5_ext_cum

W5_ext_sd <- StdDev.annualized(W5_ext_con, scale = 252,geometric = TRUE)
W5_ext_sk <- skewness(W5_ext_con, method = "sample")
W5_ext_ku <- kurtosis(W5_ext_con, method = "sample")
W5_ext_CVaR <- CVaR(W5_ext_con,p = 0.95,method = "historical")[1]
W5_ext_GtP <- sum(W5_ext_con[W5_ext_con>0])/abs(sum(W5_ext_con[W5_ext_con<0]))
W5_ext_PR <- PainRatio(as.xts(x = W5_ext_con,order.by = date[-(1:127)]))[1]
W5_ext_ShR <- SharpeRatio(as.xts(x = W5_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W5_ext_SoR <- SortinoRatio(W5_ext_con,MAR = 0)[1]
W5_ext_SemiD <- SemiDeviation(W5_ext_con)[1]
W5_ext_om <- Omega(W5_ext_con,L = 0,method = "simple",output = "point")
W5_ext_ka <- Kappa(W5_ext_con,MAR = 0,l=3)
W5_ext_MD <- maxDrawdown(as.xts(x = W5_ext_con,order.by = date[-(1:127)]))
W5_ext_awchg <- mean(abs(diff(W5_ext_w)))
W5_ext_alev <- mean((1-rowSums(W5_ext_w))*-1)
W5_ext_maxlev <- max((1-rowSums(W5_ext_w))*-1)
W5_ext_minlev <- min((1-rowSums(W5_ext_w))*-1)

W6_ext_con <- NULL
W6_ext_ann <- NULL
W6_ext_cum <- NULL

W6_ext_con <- c(W6_ext[[1]]$px,head(W6_ext[[2]]$px,-1),head(W6_ext[[3]]$px,-1),head(W6_ext[[4]]$px,-1),head(W6_ext[[5]]$px,-1),head(W6_ext[[6]]$px,-1),head(W6_ext[[7]]$px,-1),head(W6_ext[[8]]$px,-1),head(W6_ext[[9]]$px,-1),head(W6_ext[[10]]$px,-1))

W6_ext_cum_sca <- Return.cumulative(W6_ext_con,geometric = TRUE)
W6_ext_cum_sca

len <- length(W6_ext_con)

for (i in 1:(len)) {
  W6_ext_cum[i] = Return.cumulative(W6_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W6_ext_ann[i] = Return.annualized(W6_ext_con[1:i], scale = 252,geometric = TRUE)
}

W6_ext_ann_sca = Return.annualized(W6_ext_con, scale = 252,geometric = TRUE)
W6_ext_ann_sca

ts.plot(as.ts(W6_ext_cum))

W6_ext_w <- NULL
W6_ext_w <- c(W6_ext[[1]]$pw,W6_ext[[2]]$pw,W6_ext[[3]]$pw,W6_ext[[4]]$pw,W6_ext[[5]]$pw,W6_ext[[6]]$pw,W6_ext[[7]]$pw,W6_ext[[8]]$pw,W6_ext[[9]]$pw,W6_ext[[10]]$pw)
W6_ext_w <- matrix(W6_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W6_ext_w

W6_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W6_ext_w_excel <- c(W6_ext_w_excel,replicate(m[i],W6_ext[[i-1]]$pw))
}

W6_ext_w_excel <- matrix(W6_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W6_ext_perf <- W6_ext_cum

W6_ext_sd <- StdDev.annualized(W6_ext_con, scale = 252,geometric = TRUE)
W6_ext_sk <- skewness(W6_ext_con, method = "sample")
W6_ext_ku <- kurtosis(W6_ext_con, method = "sample")
W6_ext_CVaR <- CVaR(W6_ext_con,p = 0.95,method = "historical")[1]
W6_ext_GtP <- sum(W6_ext_con[W6_ext_con>0])/abs(sum(W6_ext_con[W6_ext_con<0]))
W6_ext_PR <- PainRatio(as.xts(x = W6_ext_con,order.by = date[-(1:127)]))[1]
W6_ext_ShR <- SharpeRatio(as.xts(x = W6_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W6_ext_SoR <- SortinoRatio(W6_ext_con,MAR = 0)[1]
W6_ext_SemiD <- SemiDeviation(W6_ext_con)[1]
W6_ext_om <- Omega(W6_ext_con,L = 0,method = "simple",output = "point")
W6_ext_ka <- Kappa(W6_ext_con,MAR = 0,l=3)
W6_ext_MD <- maxDrawdown(as.xts(x = W6_ext_con,order.by = date[-(1:127)]))
W6_ext_awchg <- mean(abs(diff(W6_ext_w)))
W6_ext_alev <- mean((1-rowSums(W6_ext_w))*-1)
W6_ext_maxlev <- max((1-rowSums(W6_ext_w))*-1)
W6_ext_minlev <- min((1-rowSums(W6_ext_w))*-1)

W7_ext_con <- NULL
W7_ext_ann <- NULL
W7_ext_cum <- NULL

W7_ext_con <- c(W7_ext[[1]]$px,head(W7_ext[[2]]$px,-1),head(W7_ext[[3]]$px,-1),head(W7_ext[[4]]$px,-1),head(W7_ext[[5]]$px,-1),head(W7_ext[[6]]$px,-1),head(W7_ext[[7]]$px,-1),head(W7_ext[[8]]$px,-1),head(W7_ext[[9]]$px,-1),head(W7_ext[[10]]$px,-1))

W7_ext_cum_sca <- Return.cumulative(W7_ext_con,geometric = TRUE)
W7_ext_cum_sca

len <- length(W7_ext_con)

for (i in 1:(len)) {
  W7_ext_cum[i] = Return.cumulative(W7_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W7_ext_ann[i] = Return.annualized(W7_ext_con[1:i], scale = 252,geometric = TRUE)
}

W7_ext_ann_sca = Return.annualized(W7_ext_con, scale = 252,geometric = TRUE)
W7_ext_ann_sca

ts.plot(as.ts(W7_ext_cum))

W7_ext_w <- NULL
W7_ext_w <- c(W7_ext[[1]]$pw,W7_ext[[2]]$pw,W7_ext[[3]]$pw,W7_ext[[4]]$pw,W7_ext[[5]]$pw,W7_ext[[6]]$pw,W7_ext[[7]]$pw,W7_ext[[8]]$pw,W7_ext[[9]]$pw,W7_ext[[10]]$pw)
W7_ext_w <- matrix(W7_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W7_ext_w

W7_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W7_ext_w_excel <- c(W7_ext_w_excel,replicate(m[i],W7_ext[[i-1]]$pw))
}

W7_ext_w_excel <- matrix(W7_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W7_ext_perf <- W7_ext_cum

W7_ext_sd <- StdDev.annualized(W7_ext_con, scale = 252,geometric = TRUE)
W7_ext_sk <- skewness(W7_ext_con, method = "sample")
W7_ext_ku <- kurtosis(W7_ext_con, method = "sample")
W7_ext_CVaR <- CVaR(W7_ext_con,p = 0.95,method = "historical")[1]
W7_ext_GtP <- sum(W7_ext_con[W7_ext_con>0])/abs(sum(W7_ext_con[W7_ext_con<0]))
W7_ext_PR <- PainRatio(as.xts(x = W7_ext_con,order.by = date[-(1:127)]))[1]
W7_ext_ShR <- SharpeRatio(as.xts(x = W7_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W7_ext_SoR <- SortinoRatio(W7_ext_con,MAR = 0)[1]
W7_ext_SemiD <- SemiDeviation(W7_ext_con)[1]
W7_ext_om <- Omega(W7_ext_con,L = 0,method = "simple",output = "point")
W7_ext_ka <- Kappa(W7_ext_con,MAR = 0,l=3)
W7_ext_MD <- maxDrawdown(as.xts(x = W7_ext_con,order.by = date[-(1:127)]))
W7_ext_awchg <- mean(abs(diff(W7_ext_w)))
W7_ext_alev <- mean((1-rowSums(W7_ext_w))*-1)
W7_ext_maxlev <- max((1-rowSums(W7_ext_w))*-1)
W7_ext_minlev <- min((1-rowSums(W7_ext_w))*-1)

# rolling window Wavelet portfolio

er_W1_roll <- NULL
er_W2_roll <- NULL
er_W3_roll <- NULL
er_W4_roll <- NULL
er_W5_roll <- NULL
er_W6_roll <- NULL
er_W7_roll <- NULL

for (i in 1:(len_sa-2)) {
  er_W1_roll[[i]] <- colMeans(W1[[i]][t_sa[i]:t_sa[i+1],])
  er_W2_roll[[i]] <- colMeans(W2[[i]][t_sa[i]:t_sa[i+1],])
  er_W3_roll[[i]] <- colMeans(W3[[i]][t_sa[i]:t_sa[i+1],])
  er_W4_roll[[i]] <- colMeans(W4[[i]][t_sa[i]:t_sa[i+1],])
  er_W5_roll[[i]] <- colMeans(W5[[i]][t_sa[i]:t_sa[i+1],])
  er_W6_roll[[i]] <- colMeans(W6[[i]][t_sa[i]:t_sa[i+1],])
  er_W7_roll[[i]] <- colMeans(W7[[i]][t_sa[i]:t_sa[i+1],])
}

rvec_W1_roll <- NULL
rvec_W2_roll <- NULL
rvec_W3_roll <- NULL
rvec_W4_roll <- NULL
rvec_W5_roll <- NULL
rvec_W6_roll <- NULL
rvec_W7_roll <- NULL

# rvec_W1[[1]] <- matrix(rep(er_W1[[1]],each=m[1]),nrow=m[1])
# rvec_W2[[1]] <- matrix(rep(er_W2[[1]],each=m[1]),nrow=m[1])
# rvec_W3[[1]] <- matrix(rep(er_W3[[1]],each=m[1]),nrow=m[1])
# rvec_W4[[1]] <- matrix(rep(er_W4[[1]],each=m[1]),nrow=m[1])
# rvec_W5[[1]] <- matrix(rep(er_W5[[1]],each=m[1]),nrow=m[1])
# rvec_W6[[1]] <- matrix(rep(er_W6[[1]],each=m[1]),nrow=m[1])
# rvec_W7[[1]] <- matrix(rep(er_W7[[1]],each=m[1]),nrow=m[1])

# for (i in 2:(len_sa-1)) {
#   rvec_W1[[i]] <- rbind(rvec_W1[[i-1]],matrix(rep(er_W1[[i]],each=m[i]),nrow=m[i]))
#   rvec_W2[[i]] <- rbind(rvec_W2[[i-1]],matrix(rep(er_W2[[i]],each=m[i]),nrow=m[i]))
#   rvec_W3[[i]] <- rbind(rvec_W3[[i-1]],matrix(rep(er_W3[[i]],each=m[i]),nrow=m[i]))
#   rvec_W4[[i]] <- rbind(rvec_W4[[i-1]],matrix(rep(er_W4[[i]],each=m[i]),nrow=m[i]))
#   rvec_W5[[i]] <- rbind(rvec_W5[[i-1]],matrix(rep(er_W5[[i]],each=m[i]),nrow=m[i]))
#   rvec_W6[[i]] <- rbind(rvec_W6[[i-1]],matrix(rep(er_W6[[i]],each=m[i]),nrow=m[i]))
#   rvec_W7[[i]] <- rbind(rvec_W7[[i-1]],matrix(rep(er_W7[[i]],each=m[i]),nrow=m[i]))
# }

for (i in 1:(len_sa-2)) {
  rvec_W1_roll[[i]] <- matrix(rep(er_W1_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W2_roll[[i]] <- matrix(rep(er_W2_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W3_roll[[i]] <- matrix(rep(er_W3_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W4_roll[[i]] <- matrix(rep(er_W4_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W5_roll[[i]] <- matrix(rep(er_W5_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W6_roll[[i]] <- matrix(rep(er_W6_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W7_roll[[i]] <- matrix(rep(er_W7_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
}

for (i in 1:(len_sa-2)) {
  rvec_W1_roll[[i]] <- xts(x = rvec_W1_roll[[i]],order.by = as.Date(date[1:(length(rvec_W1_roll[[i]])/3)]))
  rvec_W2_roll[[i]] <- xts(x = rvec_W2_roll[[i]],order.by = as.Date(date[1:(length(rvec_W2_roll[[i]])/3)]))
  rvec_W3_roll[[i]] <- xts(x = rvec_W3_roll[[i]],order.by = as.Date(date[1:(length(rvec_W3_roll[[i]])/3)]))
  rvec_W4_roll[[i]] <- xts(x = rvec_W4_roll[[i]],order.by = as.Date(date[1:(length(rvec_W4_roll[[i]])/3)]))
  rvec_W5_roll[[i]] <- xts(x = rvec_W5_roll[[i]],order.by = as.Date(date[1:(length(rvec_W5_roll[[i]])/3)]))
  rvec_W6_roll[[i]] <- xts(x = rvec_W6_roll[[i]],order.by = as.Date(date[1:(length(rvec_W6_roll[[i]])/3)]))
  rvec_W7_roll[[i]] <- xts(x = rvec_W7_roll[[i]],order.by = as.Date(date[1:(length(rvec_W7_roll[[i]])/3)]))
}

W1_roll <- NULL
W2_roll <- NULL
W3_roll <- NULL
W4_roll <- NULL
W5_roll <- NULL
W6_roll <- NULL
W7_roll <- NULL

for (i in 1:(len_sa-2)) {
  W1_roll[[i]] <- portfolio.optim(rvec_W1_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W1_roll[[i]])
  W2_roll[[i]] <- portfolio.optim(rvec_W2_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W2_roll[[i]])
  W3_roll[[i]] <- portfolio.optim(rvec_W3_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W3_roll[[i]])
  W4_roll[[i]] <- portfolio.optim(rvec_W4_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W4_roll[[i]])
  W5_roll[[i]] <- portfolio.optim(rvec_W5_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W5_roll[[i]])
  W6_roll[[i]] <- portfolio.optim(rvec_W6_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W6_roll[[i]])
  W7_roll[[i]] <- portfolio.optim(rvec_W7_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W7_roll[[i]])
}

for (i in 1:(len_sa-2)) {
  W1_roll[[i]]$px <- c(tcrossprod(W1_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W2_roll[[i]]$px <- c(tcrossprod(W2_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W3_roll[[i]]$px <- c(tcrossprod(W3_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W4_roll[[i]]$px <- c(tcrossprod(W4_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W5_roll[[i]]$px <- c(tcrossprod(W5_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W6_roll[[i]]$px <- c(tcrossprod(W6_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W7_roll[[i]]$px <- c(tcrossprod(W7_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

W1_roll_con <- NULL
W1_roll_ann <- NULL
W1_roll_cum <- NULL

W1_roll_con <- c(W1_roll[[1]]$px,head(W1_roll[[2]]$px,-1),head(W1_roll[[3]]$px,-1),head(W1_roll[[4]]$px,-1),head(W1_roll[[5]]$px,-1),head(W1_roll[[6]]$px,-1),head(W1_roll[[7]]$px,-1),head(W1_roll[[8]]$px,-1),head(W1_roll[[9]]$px,-1),head(W1_roll[[10]]$px,-1))

W1_roll_cum_sca <- Return.cumulative(W1_roll_con,geometric = TRUE)
W1_roll_cum_sca

len <- length(W1_roll_con)

for (i in 1:(len)) {
  W1_roll_cum[i] = Return.cumulative(W1_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W1_roll_ann[i] = Return.annualized(W1_roll_con[1:i], scale = 252,geometric = TRUE)
}

W1_roll_ann_sca = Return.annualized(W1_roll_con, scale = 252,geometric = TRUE)
W1_roll_ann_sca

ts.plot(as.ts(W1_roll_cum),as.ts(dcc_cum))

W1_roll_w <- NULL
W1_roll_w <- c(W1_roll[[1]]$pw,W1_roll[[2]]$pw,W1_roll[[3]]$pw,W1_roll[[4]]$pw,W1_roll[[5]]$pw,W1_roll[[6]]$pw,W1_roll[[7]]$pw,W1_roll[[8]]$pw,W1_roll[[9]]$pw,W1_roll[[10]]$pw)
W1_roll_w <- matrix(W1_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W1_roll_w

W1_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W1_roll_w_excel <- c(W1_roll_w_excel,replicate(m[i],W1_roll[[i-1]]$pw))
}

W1_roll_w_excel <- matrix(W1_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W1_roll_perf <- W1_roll_cum

W1_roll_sd <- StdDev.annualized(W1_roll_con, scale = 252,geometric = TRUE)
W1_roll_sk <- skewness(W1_roll_con, method = "sample")
W1_roll_ku <- kurtosis(W1_roll_con, method = "sample")
W1_roll_CVaR <- CVaR(W1_roll_con,p = 0.95,method = "historical")[1]
W1_roll_GtP <- sum(W1_roll_con[W1_roll_con>0])/abs(sum(W1_roll_con[W1_roll_con<0]))
W1_roll_PR <- PainRatio(as.xts(x = W1_roll_con,order.by = date[-(1:127)]))[1]
W1_roll_ShR <- SharpeRatio(as.xts(x = W1_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W1_roll_SoR <- SortinoRatio(W1_roll_con,MAR = 0)[1]
W1_roll_SemiD <- SemiDeviation(W1_roll_con)[1]
W1_roll_om <- Omega(W1_roll_con,L = 0,method = "simple",output = "point")
W1_roll_ka <- Kappa(W1_roll_con,MAR = 0,l=3)
W1_roll_MD <- maxDrawdown(as.xts(x = W1_roll_con,order.by = date[-(1:127)]))
W1_roll_awchg <- mean(abs(diff(W1_roll_w)))
W1_roll_alev <- mean((1-rowSums(W1_roll_w))*-1)
W1_roll_maxlev <- max((1-rowSums(W1_roll_w))*-1)
W1_roll_minlev <- min((1-rowSums(W1_roll_w))*-1)

W2_roll_con <- NULL
W2_roll_ann <- NULL
W2_roll_cum <- NULL

W2_roll_con <- c(W2_roll[[1]]$px,head(W2_roll[[2]]$px,-1),head(W2_roll[[3]]$px,-1),head(W2_roll[[4]]$px,-1),head(W2_roll[[5]]$px,-1),head(W2_roll[[6]]$px,-1),head(W2_roll[[7]]$px,-1),head(W2_roll[[8]]$px,-1),head(W2_roll[[9]]$px,-1),head(W2_roll[[10]]$px,-1))

W2_roll_cum_sca <- Return.cumulative(W2_roll_con,geometric = TRUE)
W2_roll_cum_sca

len <- length(W2_roll_con)

for (i in 1:(len)) {
  W2_roll_cum[i] = Return.cumulative(W2_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W2_roll_ann[i] = Return.annualized(W2_roll_con[1:i], scale = 252,geometric = TRUE)
}

W2_roll_ann_sca = Return.annualized(W2_roll_con, scale = 252,geometric = TRUE)
W2_roll_ann_sca

ts.plot(as.ts(W2_roll_cum))

W2_roll_w <- NULL
W2_roll_w <- c(W2_roll[[1]]$pw,W2_roll[[2]]$pw,W2_roll[[3]]$pw,W2_roll[[4]]$pw,W2_roll[[5]]$pw,W2_roll[[6]]$pw,W2_roll[[7]]$pw,W2_roll[[8]]$pw,W2_roll[[9]]$pw,W2_roll[[10]]$pw)
W2_roll_w <- matrix(W2_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W2_roll_w

W2_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W2_roll_w_excel <- c(W2_roll_w_excel,replicate(m[i],W2_roll[[i-1]]$pw))
}

W2_roll_w_excel <- matrix(W2_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W2_roll_perf <- W2_roll_cum

W2_roll_sd <- StdDev.annualized(W2_roll_con, scale = 252,geometric = TRUE)
W2_roll_sk <- skewness(W2_roll_con, method = "sample")
W2_roll_ku <- kurtosis(W2_roll_con, method = "sample")
W2_roll_CVaR <- CVaR(W2_roll_con,p = 0.95,method = "historical")[1]
W2_roll_GtP <- sum(W2_roll_con[W2_roll_con>0])/abs(sum(W2_roll_con[W2_roll_con<0]))
W2_roll_PR <- PainRatio(as.xts(x = W2_roll_con,order.by = date[-(1:127)]))[1]
W2_roll_ShR <- SharpeRatio(as.xts(x = W2_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W2_roll_SoR <- SortinoRatio(W2_roll_con,MAR = 0)[1]
W2_roll_SemiD <- SemiDeviation(W2_roll_con)[1]
W2_roll_om <- Omega(W2_roll_con,L = 0,method = "simple",output = "point")
W2_roll_ka <- Kappa(W2_roll_con,MAR = 0,l=3)
W2_roll_MD <- maxDrawdown(as.xts(x = W2_roll_con,order.by = date[-(1:127)]))
W2_roll_awchg <- mean(abs(diff(W2_roll_w)))
W2_roll_alev <- mean((1-rowSums(W2_roll_w))*-1)
W2_roll_maxlev <- max((1-rowSums(W2_roll_w))*-1)
W2_roll_minlev <- min((1-rowSums(W2_roll_w))*-1)

W3_roll_con <- NULL
W3_roll_ann <- NULL
W3_roll_cum <- NULL

W3_roll_con <- c(W3_roll[[1]]$px,head(W3_roll[[2]]$px,-1),head(W3_roll[[3]]$px,-1),head(W3_roll[[4]]$px,-1),head(W3_roll[[5]]$px,-1),head(W3_roll[[6]]$px,-1),head(W3_roll[[7]]$px,-1),head(W3_roll[[8]]$px,-1),head(W3_roll[[9]]$px,-1),head(W3_roll[[10]]$px,-1))

W3_roll_cum_sca <- Return.cumulative(W3_roll_con,geometric = TRUE)
W3_roll_cum_sca

len <- length(W3_roll_con)

for (i in 1:(len)) {
  W3_roll_cum[i] = Return.cumulative(W3_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W3_roll_ann[i] = Return.annualized(W3_roll_con[1:i], scale = 252,geometric = TRUE)
}

W3_roll_ann_sca = Return.annualized(W3_roll_con, scale = 252,geometric = TRUE)
W3_roll_ann_sca

ts.plot(as.ts(W3_roll_cum))

W3_roll_w <- NULL
W3_roll_w <- c(W3_roll[[1]]$pw,W3_roll[[2]]$pw,W3_roll[[3]]$pw,W3_roll[[4]]$pw,W3_roll[[5]]$pw,W3_roll[[6]]$pw,W3_roll[[7]]$pw,W3_roll[[8]]$pw,W3_roll[[9]]$pw,W3_roll[[10]]$pw)
W3_roll_w <- matrix(W3_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W3_roll_w

W3_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W3_roll_w_excel <- c(W3_roll_w_excel,replicate(m[i],W3_roll[[i-1]]$pw))
}

W3_roll_w_excel <- matrix(W3_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W3_roll_perf <- W3_roll_cum

W3_roll_sd <- StdDev.annualized(W3_roll_con, scale = 252,geometric = TRUE)
W3_roll_sk <- skewness(W3_roll_con, method = "sample")
W3_roll_ku <- kurtosis(W3_roll_con, method = "sample")
W3_roll_CVaR <- CVaR(W3_roll_con,p = 0.95,method = "historical")[1]
W3_roll_GtP <- sum(W3_roll_con[W3_roll_con>0])/abs(sum(W3_roll_con[W3_roll_con<0]))
W3_roll_PR <- PainRatio(as.xts(x = W3_roll_con,order.by = date[-(1:127)]))[1]
W3_roll_ShR <- SharpeRatio(as.xts(x = W3_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W3_roll_SoR <- SortinoRatio(W3_roll_con,MAR = 0)[1]
W3_roll_SemiD <- SemiDeviation(W3_roll_con)[1]
W3_roll_om <- Omega(W3_roll_con,L = 0,method = "simple",output = "point")
W3_roll_ka <- Kappa(W3_roll_con,MAR = 0,l=3)
W3_roll_MD <- maxDrawdown(as.xts(x = W3_roll_con,order.by = date[-(1:127)]))
W3_roll_awchg <- mean(abs(diff(W3_roll_w)))
W3_roll_alev <- mean((1-rowSums(W3_roll_w))*-1)
W3_roll_maxlev <- max((1-rowSums(W3_roll_w))*-1)
W3_roll_minlev <- min((1-rowSums(W3_roll_w))*-1)

W4_roll_con <- NULL
W4_roll_ann <- NULL
W4_roll_cum <- NULL

W4_roll_con <- c(W4_roll[[1]]$px,head(W4_roll[[2]]$px,-1),head(W4_roll[[3]]$px,-1),head(W4_roll[[4]]$px,-1),head(W4_roll[[5]]$px,-1),head(W4_roll[[6]]$px,-1),head(W4_roll[[7]]$px,-1),head(W4_roll[[8]]$px,-1),head(W4_roll[[9]]$px,-1),head(W4_roll[[10]]$px,-1))

W4_roll_cum_sca <- Return.cumulative(W4_roll_con,geometric = TRUE)
W4_roll_cum_sca

len <- length(W4_roll_con)

for (i in 1:(len)) {
  W4_roll_cum[i] = Return.cumulative(W4_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W4_roll_ann[i] = Return.annualized(W4_roll_con[1:i], scale = 252,geometric = TRUE)
}

W4_roll_ann_sca = Return.annualized(W4_roll_con, scale = 252,geometric = TRUE)
W4_roll_ann_sca

ts.plot(as.ts(W4_roll_cum))

W4_roll_w <- NULL
W4_roll_w <- c(W4_roll[[1]]$pw,W4_roll[[2]]$pw,W4_roll[[3]]$pw,W4_roll[[4]]$pw,W4_roll[[5]]$pw,W4_roll[[6]]$pw,W4_roll[[7]]$pw,W4_roll[[8]]$pw,W4_roll[[9]]$pw,W4_roll[[10]]$pw)
W4_roll_w <- matrix(W4_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W4_roll_w

W4_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W4_roll_w_excel <- c(W4_roll_w_excel,replicate(m[i],W4_roll[[i-1]]$pw))
}

W4_roll_w_excel <- matrix(W4_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W4_roll_perf <- W4_roll_cum

W4_roll_sd <- StdDev.annualized(W4_roll_con, scale = 252,geometric = TRUE)
W4_roll_sk <- skewness(W4_roll_con, method = "sample")
W4_roll_ku <- kurtosis(W4_roll_con, method = "sample")
W4_roll_CVaR <- CVaR(W4_roll_con,p = 0.95,method = "historical")[1]
W4_roll_GtP <- sum(W4_roll_con[W4_roll_con>0])/abs(sum(W4_roll_con[W4_roll_con<0]))
W4_roll_PR <- PainRatio(as.xts(x = W4_roll_con,order.by = date[-(1:127)]))[1]
W4_roll_ShR <- SharpeRatio(as.xts(x = W4_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W4_roll_SoR <- SortinoRatio(W4_roll_con,MAR = 0)[1]
W4_roll_SemiD <- SemiDeviation(W4_roll_con)[1]
W4_roll_om <- Omega(W4_roll_con,L = 0,method = "simple",output = "point")
W4_roll_ka <- Kappa(W4_roll_con,MAR = 0,l=3)
W4_roll_MD <- maxDrawdown(as.xts(x = W4_roll_con,order.by = date[-(1:127)]))
W4_roll_awchg <- mean(abs(diff(W4_roll_w)))
W4_roll_alev <- mean((1-rowSums(W4_roll_w))*-1)
W4_roll_maxlev <- max((1-rowSums(W4_roll_w))*-1)
W4_roll_minlev <- min((1-rowSums(W4_roll_w))*-1)

W5_roll_con <- NULL
W5_roll_ann <- NULL
W5_roll_cum <- NULL

W5_roll_con <- c(W5_roll[[1]]$px,head(W5_roll[[2]]$px,-1),head(W5_roll[[3]]$px,-1),head(W5_roll[[4]]$px,-1),head(W5_roll[[5]]$px,-1),head(W5_roll[[6]]$px,-1),head(W5_roll[[7]]$px,-1),head(W5_roll[[8]]$px,-1),head(W5_roll[[9]]$px,-1),head(W5_roll[[10]]$px,-1))

W5_roll_cum_sca <- Return.cumulative(W5_roll_con,geometric = TRUE)
W5_roll_cum_sca

len <- length(W5_roll_con)

for (i in 1:(len)) {
  W5_roll_cum[i] = Return.cumulative(W5_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W5_roll_ann[i] = Return.annualized(W5_roll_con[1:i], scale = 252,geometric = TRUE)
}

W5_roll_ann_sca = Return.annualized(W5_roll_con, scale = 252,geometric = TRUE)
W5_roll_ann_sca

ts.plot(as.ts(W5_roll_cum))

W5_roll_w <- NULL
W5_roll_w <- c(W5_roll[[1]]$pw,W5_roll[[2]]$pw,W5_roll[[3]]$pw,W5_roll[[4]]$pw,W5_roll[[5]]$pw,W5_roll[[6]]$pw,W5_roll[[7]]$pw,W5_roll[[8]]$pw,W5_roll[[9]]$pw,W5_roll[[10]]$pw)
W5_roll_w <- matrix(W5_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W5_roll_w

W5_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W5_roll_w_excel <- c(W5_roll_w_excel,replicate(m[i],W5_roll[[i-1]]$pw))
}

W5_roll_w_excel <- matrix(W5_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W5_roll_perf <- W5_roll_cum

W5_roll_sd <- StdDev.annualized(W5_roll_con, scale = 252,geometric = TRUE)
W5_roll_sk <- skewness(W5_roll_con, method = "sample")
W5_roll_ku <- kurtosis(W5_roll_con, method = "sample")
W5_roll_CVaR <- CVaR(W5_roll_con,p = 0.95,method = "historical")[1]
W5_roll_GtP <- sum(W5_roll_con[W5_roll_con>0])/abs(sum(W5_roll_con[W5_roll_con<0]))
W5_roll_PR <- PainRatio(as.xts(x = W5_roll_con,order.by = date[-(1:127)]))[1]
W5_roll_ShR <- SharpeRatio(as.xts(x = W5_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W5_roll_SoR <- SortinoRatio(W5_roll_con,MAR = 0)[1]
W5_roll_SemiD <- SemiDeviation(W5_roll_con)[1]
W5_roll_om <- Omega(W5_roll_con,L = 0,method = "simple",output = "point")
W5_roll_ka <- Kappa(W5_roll_con,MAR = 0,l=3)
W5_roll_MD <- maxDrawdown(as.xts(x = W5_roll_con,order.by = date[-(1:127)]))
W5_roll_awchg <- mean(abs(diff(W5_roll_w)))
W5_roll_alev <- mean((1-rowSums(W5_roll_w))*-1)
W5_roll_maxlev <- max((1-rowSums(W5_roll_w))*-1)
W5_roll_minlev <- min((1-rowSums(W5_roll_w))*-1)

W6_roll_con <- NULL
W6_roll_ann <- NULL
W6_roll_cum <- NULL

W6_roll_con <- c(W6_roll[[1]]$px,head(W6_roll[[2]]$px,-1),head(W6_roll[[3]]$px,-1),head(W6_roll[[4]]$px,-1),head(W6_roll[[5]]$px,-1),head(W6_roll[[6]]$px,-1),head(W6_roll[[7]]$px,-1),head(W6_roll[[8]]$px,-1),head(W6_roll[[9]]$px,-1),head(W6_roll[[10]]$px,-1))

W6_roll_cum_sca <- Return.cumulative(W6_roll_con,geometric = TRUE)
W6_roll_cum_sca

len <- length(W6_roll_con)

for (i in 1:(len)) {
  W6_roll_cum[i] = Return.cumulative(W6_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W6_roll_ann[i] = Return.annualized(W6_roll_con[1:i], scale = 252,geometric = TRUE)
}

W6_roll_ann_sca = Return.annualized(W6_roll_con, scale = 252,geometric = TRUE)
W6_roll_ann_sca

ts.plot(as.ts(W6_roll_cum))

W6_roll_w <- NULL
W6_roll_w <- c(W6_roll[[1]]$pw,W6_roll[[2]]$pw,W6_roll[[3]]$pw,W6_roll[[4]]$pw,W6_roll[[5]]$pw,W6_roll[[6]]$pw,W6_roll[[7]]$pw,W6_roll[[8]]$pw,W6_roll[[9]]$pw,W6_roll[[10]]$pw)
W6_roll_w <- matrix(W6_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W6_roll_w

W6_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W6_roll_w_excel <- c(W6_roll_w_excel,replicate(m[i],W6_roll[[i-1]]$pw))
}

W6_roll_w_excel <- matrix(W6_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W6_roll_perf <- W6_roll_cum

W6_roll_sd <- StdDev.annualized(W6_roll_con, scale = 252,geometric = TRUE)
W6_roll_sk <- skewness(W6_roll_con, method = "sample")
W6_roll_ku <- kurtosis(W6_roll_con, method = "sample")
W6_roll_CVaR <- CVaR(W6_roll_con,p = 0.95,method = "historical")[1]
W6_roll_GtP <- sum(W6_roll_con[W6_roll_con>0])/abs(sum(W6_roll_con[W6_roll_con<0]))
W6_roll_PR <- PainRatio(as.xts(x = W6_roll_con,order.by = date[-(1:127)]))[1]
W6_roll_ShR <- SharpeRatio(as.xts(x = W6_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE)[1]
W6_roll_SoR <- SortinoRatio(W6_roll_con,MAR = 0)[1]
W6_roll_SemiD <- SemiDeviation(W6_roll_con)[1]
W6_roll_om <- Omega(W6_roll_con,L = 0,method = "simple",output = "point")
W6_roll_ka <- Kappa(W6_roll_con,MAR = 0,l=3)
W6_roll_MD <- maxDrawdown(as.xts(x = W6_roll_con,order.by = date[-(1:127)]))
W6_roll_awchg <- mean(abs(diff(W6_roll_w)))
W6_roll_alev <- mean((1-rowSums(W6_roll_w))*-1)
W6_roll_maxlev <- max((1-rowSums(W6_roll_w))*-1)
W6_roll_minlev <- min((1-rowSums(W6_roll_w))*-1)

W7_roll_con <- NULL
W7_roll_ann <- NULL
W7_roll_cum <- NULL

W7_roll_con <- c(W7_roll[[1]]$px,head(W7_roll[[2]]$px,-1),head(W7_roll[[3]]$px,-1),head(W7_roll[[4]]$px,-1),head(W7_roll[[5]]$px,-1),head(W7_roll[[6]]$px,-1),head(W7_roll[[7]]$px,-1),head(W7_roll[[8]]$px,-1),head(W7_roll[[9]]$px,-1),head(W7_roll[[10]]$px,-1))

W7_roll_cum_sca <- Return.cumulative(W7_roll_con,geometric = TRUE)
W7_roll_cum_sca

len <- length(W7_roll_con)

for (i in 1:(len)) {
  W7_roll_cum[i] = Return.cumulative(W7_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W7_roll_ann[i] = Return.annualized(W7_roll_con[1:i], scale = 252,geometric = TRUE)
}

W7_roll_ann_sca = Return.annualized(W7_roll_con, scale = 252,geometric = TRUE)
W7_roll_ann_sca

ts.plot(as.ts(W7_roll_cum))

W7_roll_w <- NULL
W7_roll_w <- c(W7_roll[[1]]$pw,W7_roll[[2]]$pw,W7_roll[[3]]$pw,W7_roll[[4]]$pw,W7_roll[[5]]$pw,W7_roll[[6]]$pw,W7_roll[[7]]$pw,W7_roll[[8]]$pw,W7_roll[[9]]$pw,W7_roll[[10]]$pw)
W7_roll_w <- matrix(W7_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W7_roll_w

W7_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W7_roll_w_excel <- c(W7_roll_w_excel,replicate(m[i],W7_roll[[i-1]]$pw))
}

W7_roll_w_excel <- matrix(W7_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W7_roll_perf <- W7_roll_cum

W1_ext_sd <- StdDev.annualized(W1_ext_con, scale = 252,geometric = TRUE)
W1_ext_sk <- skewness(W1_ext_con, method = "sample")
W1_ext_ku <- kurtosis(W1_ext_con, method = "sample")
W1_ext_CVaR <- CVaR(W1_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W1_ext_GtP <- sum(W1_ext_con[W1_ext_con>0])/abs(sum(W1_ext_con[W1_ext_con<0]))
W1_ext_PR <- PainRatio(as.xts(x = W1_ext_con,order.by = date[-(1:127)]))[1]
W1_ext_ShR <- SharpeRatio(as.xts(x = W1_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W1_ext_SoR <- SortinoRatio(W1_ext_con,MAR = 0)[1]*sqrt(252)
W1_ext_SemiD <- SemiDeviation(W1_ext_con)[1]*sqrt(252)
W1_ext_om <- Omega(W1_ext_con,L = 0,method = "simple",output = "point")-1
W1_ext_ka <- Kappa(W1_ext_con,MAR = 0,l=3)
W1_ext_MD <- maxDrawdown(as.xts(x = W1_ext_con,order.by = date[-(1:127)]))*-1
W1_ext_awchg <- mean(abs(diff(W1_ext_w)))
W1_ext_alev <- mean((1-rowSums(W1_ext_w))[(1-rowSums(W1_ext_w))<0])*-1
W1_ext_maxlev <- max((1-rowSums(W1_ext_w))*-1)
W1_ext_minlev <- min((1-rowSums(W1_ext_w))*-1)

W2_ext_sd <- StdDev.annualized(W2_ext_con, scale = 252,geometric = TRUE)
W2_ext_sk <- skewness(W2_ext_con, method = "sample")
W2_ext_ku <- kurtosis(W2_ext_con, method = "sample")
W2_ext_CVaR <- CVaR(W2_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W2_ext_GtP <- sum(W2_ext_con[W2_ext_con>0])/abs(sum(W2_ext_con[W2_ext_con<0]))
W2_ext_PR <- PainRatio(as.xts(x = W2_ext_con,order.by = date[-(1:127)]))[1]
W2_ext_ShR <- SharpeRatio(as.xts(x = W2_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W2_ext_SoR <- SortinoRatio(W2_ext_con,MAR = 0)[1]*sqrt(252)
W2_ext_SemiD <- SemiDeviation(W2_ext_con)[1]*sqrt(252)
W2_ext_om <- Omega(W2_ext_con,L = 0,method = "simple",output = "point")-1
W2_ext_ka <- Kappa(W2_ext_con,MAR = 0,l=3)
W2_ext_MD <- maxDrawdown(as.xts(x = W2_ext_con,order.by = date[-(1:127)]))*-1
W2_ext_awchg <- mean(abs(diff(W2_ext_w)))
W2_ext_alev <- mean((1-rowSums(W2_ext_w))[(1-rowSums(W2_ext_w))<0])*-1
W2_ext_maxlev <- max((1-rowSums(W2_ext_w))*-1)
W2_ext_minlev <- min((1-rowSums(W2_ext_w))*-1)

W3_ext_sd <- StdDev.annualized(W3_ext_con, scale = 252,geometric = TRUE)
W3_ext_sk <- skewness(W3_ext_con, method = "sample")
W3_ext_ku <- kurtosis(W3_ext_con, method = "sample")
W3_ext_CVaR <- CVaR(W3_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W3_ext_GtP <- sum(W3_ext_con[W3_ext_con>0])/abs(sum(W3_ext_con[W3_ext_con<0]))
W3_ext_PR <- PainRatio(as.xts(x = W3_ext_con,order.by = date[-(1:127)]))[1]
W3_ext_ShR <- SharpeRatio(as.xts(x = W3_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W3_ext_SoR <- SortinoRatio(W3_ext_con,MAR = 0)[1]*sqrt(252)
W3_ext_SemiD <- SemiDeviation(W3_ext_con)[1]*sqrt(252)
W3_ext_om <- Omega(W3_ext_con,L = 0,method = "simple",output = "point")-1
W3_ext_ka <- Kappa(W3_ext_con,MAR = 0,l=3)
W3_ext_MD <- maxDrawdown(as.xts(x = W3_ext_con,order.by = date[-(1:127)]))*-1
W3_ext_awchg <- mean(abs(diff(W3_ext_w)))
W3_ext_alev <- mean((1-rowSums(W3_ext_w))[(1-rowSums(W3_ext_w))<0])*-1
W3_ext_maxlev <- max((1-rowSums(W3_ext_w))*-1)
W3_ext_minlev <- min((1-rowSums(W3_ext_w))*-1)

W4_ext_sd <- StdDev.annualized(W4_ext_con, scale = 252,geometric = TRUE)
W4_ext_sk <- skewness(W4_ext_con, method = "sample")
W4_ext_ku <- kurtosis(W4_ext_con, method = "sample")
W4_ext_CVaR <- CVaR(W4_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W4_ext_GtP <- sum(W4_ext_con[W4_ext_con>0])/abs(sum(W4_ext_con[W4_ext_con<0]))
W4_ext_PR <- PainRatio(as.xts(x = W4_ext_con,order.by = date[-(1:127)]))[1]
W4_ext_ShR <- SharpeRatio(as.xts(x = W4_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W4_ext_SoR <- SortinoRatio(W4_ext_con,MAR = 0)[1]*sqrt(252)
W4_ext_SemiD <- SemiDeviation(W4_ext_con)[1]*sqrt(252)
W4_ext_om <- Omega(W4_ext_con,L = 0,method = "simple",output = "point")-1
W4_ext_ka <- Kappa(W4_ext_con,MAR = 0,l=3)
W4_ext_MD <- maxDrawdown(as.xts(x = W4_ext_con,order.by = date[-(1:127)]))*-1
W4_ext_awchg <- mean(abs(diff(W4_ext_w)))
W4_ext_alev <- mean((1-rowSums(W4_ext_w))[(1-rowSums(W4_ext_w))<0])*-1
W4_ext_maxlev <- max((1-rowSums(W4_ext_w))*-1)
W4_ext_minlev <- min((1-rowSums(W4_ext_w))*-1)

W5_ext_sd <- StdDev.annualized(W5_ext_con, scale = 252,geometric = TRUE)
W5_ext_sk <- skewness(W5_ext_con, method = "sample")
W5_ext_ku <- kurtosis(W5_ext_con, method = "sample")
W5_ext_CVaR <- CVaR(W5_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W5_ext_GtP <- sum(W5_ext_con[W5_ext_con>0])/abs(sum(W5_ext_con[W5_ext_con<0]))
W5_ext_PR <- PainRatio(as.xts(x = W5_ext_con,order.by = date[-(1:127)]))[1]
W5_ext_ShR <- SharpeRatio(as.xts(x = W5_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W5_ext_SoR <- SortinoRatio(W5_ext_con,MAR = 0)[1]*sqrt(252)
W5_ext_SemiD <- SemiDeviation(W5_ext_con)[1]*sqrt(252)
W5_ext_om <- Omega(W5_ext_con,L = 0,method = "simple",output = "point")-1
W5_ext_ka <- Kappa(W5_ext_con,MAR = 0,l=3)
W5_ext_MD <- maxDrawdown(as.xts(x = W5_ext_con,order.by = date[-(1:127)]))*-1
W5_ext_awchg <- mean(abs(diff(W5_ext_w)))
W5_ext_alev <- mean((1-rowSums(W5_ext_w))[(1-rowSums(W5_ext_w))<0])*-1
W5_ext_maxlev <- max((1-rowSums(W5_ext_w))*-1)
W5_ext_minlev <- min((1-rowSums(W5_ext_w))*-1)

W6_ext_sd <- StdDev.annualized(W6_ext_con, scale = 252,geometric = TRUE)
W6_ext_sk <- skewness(W6_ext_con, method = "sample")
W6_ext_ku <- kurtosis(W6_ext_con, method = "sample")
W6_ext_CVaR <- CVaR(W6_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W6_ext_GtP <- sum(W6_ext_con[W6_ext_con>0])/abs(sum(W6_ext_con[W6_ext_con<0]))
W6_ext_PR <- PainRatio(as.xts(x = W6_ext_con,order.by = date[-(1:127)]))[1]
W6_ext_ShR <- SharpeRatio(as.xts(x = W6_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W6_ext_SoR <- SortinoRatio(W6_ext_con,MAR = 0)[1]*sqrt(252)
W6_ext_SemiD <- SemiDeviation(W6_ext_con)[1]*sqrt(252)
W6_ext_om <- Omega(W6_ext_con,L = 0,method = "simple",output = "point")-1
W6_ext_ka <- Kappa(W6_ext_con,MAR = 0,l=3)
W6_ext_MD <- maxDrawdown(as.xts(x = W6_ext_con,order.by = date[-(1:127)]))*-1
W6_ext_awchg <- mean(abs(diff(W6_ext_w)))
W6_ext_alev <- mean((1-rowSums(W6_ext_w))[(1-rowSums(W6_ext_w))<0])*-1
W6_ext_maxlev <- max((1-rowSums(W6_ext_w))*-1)
W6_ext_minlev <- min((1-rowSums(W6_ext_w))*-1)

W7_ext_sd <- StdDev.annualized(W7_ext_con, scale = 252,geometric = TRUE)
W7_ext_sk <- skewness(W7_ext_con, method = "sample")
W7_ext_ku <- kurtosis(W7_ext_con, method = "sample")
W7_ext_CVaR <- CVaR(W7_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W7_ext_GtP <- sum(W7_ext_con[W7_ext_con>0])/abs(sum(W7_ext_con[W7_ext_con<0]))
W7_ext_PR <- PainRatio(as.xts(x = W7_ext_con,order.by = date[-(1:127)]))[1]
W7_ext_ShR <- SharpeRatio(as.xts(x = W7_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W7_ext_SoR <- SortinoRatio(W7_ext_con,MAR = 0)[1]*sqrt(252)
W7_ext_SemiD <- SemiDeviation(W7_ext_con)[1]*sqrt(252)
W7_ext_om <- Omega(W7_ext_con,L = 0,method = "simple",output = "point")-1
W7_ext_ka <- Kappa(W7_ext_con,MAR = 0,l=3)
W7_ext_MD <- maxDrawdown(as.xts(x = W7_ext_con,order.by = date[-(1:127)]))*-1
W7_ext_awchg <- mean(abs(diff(W7_ext_w)))
W7_ext_alev <- mean((1-rowSums(W7_ext_w))[(1-rowSums(W7_ext_w))<0])*-1
W7_ext_maxlev <- max((1-rowSums(W7_ext_w))*-1)
W7_ext_minlev <- min((1-rowSums(W7_ext_w))*-1)

W1_roll_sd <- StdDev.annualized(W1_roll_con, scale = 252,geometric = TRUE)
W1_roll_sk <- skewness(W1_roll_con, method = "sample")
W1_roll_ku <- kurtosis(W1_roll_con, method = "sample")
W1_roll_CVaR <- CVaR(W1_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W1_roll_GtP <- sum(W1_roll_con[W1_roll_con>0])/abs(sum(W1_roll_con[W1_roll_con<0]))
W1_roll_PR <- PainRatio(as.xts(x = W1_roll_con,order.by = date[-(1:127)]))[1]
W1_roll_ShR <- SharpeRatio(as.xts(x = W1_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W1_roll_SoR <- SortinoRatio(W1_roll_con,MAR = 0)[1]*sqrt(252)
W1_roll_SemiD <- SemiDeviation(W1_roll_con)[1]*sqrt(252)
W1_roll_om <- Omega(W1_roll_con,L = 0,method = "simple",output = "point")-1
W1_roll_ka <- Kappa(W1_roll_con,MAR = 0,l=3)
W1_roll_MD <- maxDrawdown(as.xts(x = W1_roll_con,order.by = date[-(1:127)]))*-1
W1_roll_awchg <- mean(abs(diff(W1_roll_w)))
W1_roll_alev <- mean((1-rowSums(W1_roll_w))[(1-rowSums(W1_roll_w))<0])*-1
W1_roll_maxlev <- max((1-rowSums(W1_roll_w))*-1)
W1_roll_minlev <- min((1-rowSums(W1_roll_w))*-1)

W2_roll_sd <- StdDev.annualized(W2_roll_con, scale = 252,geometric = TRUE)
W2_roll_sk <- skewness(W2_roll_con, method = "sample")
W2_roll_ku <- kurtosis(W2_roll_con, method = "sample")
W2_roll_CVaR <- CVaR(W2_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W2_roll_GtP <- sum(W2_roll_con[W2_roll_con>0])/abs(sum(W2_roll_con[W2_roll_con<0]))
W2_roll_PR <- PainRatio(as.xts(x = W2_roll_con,order.by = date[-(1:127)]))[1]
W2_roll_ShR <- SharpeRatio(as.xts(x = W2_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W2_roll_SoR <- SortinoRatio(W2_roll_con,MAR = 0)[1]*sqrt(252)
W2_roll_SemiD <- SemiDeviation(W2_roll_con)[1]*sqrt(252)
W2_roll_om <- Omega(W2_roll_con,L = 0,method = "simple",output = "point")-1
W2_roll_ka <- Kappa(W2_roll_con,MAR = 0,l=3)
W2_roll_MD <- maxDrawdown(as.xts(x = W2_roll_con,order.by = date[-(1:127)]))*-1
W2_roll_awchg <- mean(abs(diff(W2_roll_w)))
W2_roll_alev <- mean((1-rowSums(W2_roll_w))[(1-rowSums(W2_roll_w))<0])*-1
W2_roll_maxlev <- max((1-rowSums(W2_roll_w))*-1)
W2_roll_minlev <- min((1-rowSums(W2_roll_w))*-1)

W3_roll_sd <- StdDev.annualized(W3_roll_con, scale = 252,geometric = TRUE)
W3_roll_sk <- skewness(W3_roll_con, method = "sample")
W3_roll_ku <- kurtosis(W3_roll_con, method = "sample")
W3_roll_CVaR <- CVaR(W3_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W3_roll_GtP <- sum(W3_roll_con[W3_roll_con>0])/abs(sum(W3_roll_con[W3_roll_con<0]))
W3_roll_PR <- PainRatio(as.xts(x = W3_roll_con,order.by = date[-(1:127)]))[1]
W3_roll_ShR <- SharpeRatio(as.xts(x = W3_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W3_roll_SoR <- SortinoRatio(W3_roll_con,MAR = 0)[1]*sqrt(252)
W3_roll_SemiD <- SemiDeviation(W3_roll_con)[1]*sqrt(252)
W3_roll_om <- Omega(W3_roll_con,L = 0,method = "simple",output = "point")-1
W3_roll_ka <- Kappa(W3_roll_con,MAR = 0,l=3)
W3_roll_MD <- maxDrawdown(as.xts(x = W3_roll_con,order.by = date[-(1:127)]))*-1
W3_roll_awchg <- mean(abs(diff(W3_roll_w)))
W3_roll_alev <- mean((1-rowSums(W3_roll_w))[(1-rowSums(W3_roll_w))<0])*-1
W3_roll_maxlev <- max((1-rowSums(W3_roll_w))*-1)
W3_roll_minlev <- min((1-rowSums(W3_roll_w))*-1)

W4_roll_sd <- StdDev.annualized(W4_roll_con, scale = 252,geometric = TRUE)
W4_roll_sk <- skewness(W4_roll_con, method = "sample")
W4_roll_ku <- kurtosis(W4_roll_con, method = "sample")
W4_roll_CVaR <- CVaR(W4_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W4_roll_GtP <- sum(W4_roll_con[W4_roll_con>0])/abs(sum(W4_roll_con[W4_roll_con<0]))
W4_roll_PR <- PainRatio(as.xts(x = W4_roll_con,order.by = date[-(1:127)]))[1]
W4_roll_ShR <- SharpeRatio(as.xts(x = W4_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W4_roll_SoR <- SortinoRatio(W4_roll_con,MAR = 0)[1]*sqrt(252)
W4_roll_SemiD <- SemiDeviation(W4_roll_con)[1]*sqrt(252)
W4_roll_om <- Omega(W4_roll_con,L = 0,method = "simple",output = "point")-1
W4_roll_ka <- Kappa(W4_roll_con,MAR = 0,l=3)
W4_roll_MD <- maxDrawdown(as.xts(x = W4_roll_con,order.by = date[-(1:127)]))*-1
W4_roll_awchg <- mean(abs(diff(W4_roll_w)))
W4_roll_alev <- mean((1-rowSums(W4_roll_w))[(1-rowSums(W4_roll_w))<0])*-1
W4_roll_maxlev <- max((1-rowSums(W4_roll_w))*-1)
W4_roll_minlev <- min((1-rowSums(W4_roll_w))*-1)

W5_roll_sd <- StdDev.annualized(W5_roll_con, scale = 252,geometric = TRUE)
W5_roll_sk <- skewness(W5_roll_con, method = "sample")
W5_roll_ku <- kurtosis(W5_roll_con, method = "sample")
W5_roll_CVaR <- CVaR(W5_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W5_roll_GtP <- sum(W5_roll_con[W5_roll_con>0])/abs(sum(W5_roll_con[W5_roll_con<0]))
W5_roll_PR <- PainRatio(as.xts(x = W5_roll_con,order.by = date[-(1:127)]))[1]
W5_roll_ShR <- SharpeRatio(as.xts(x = W5_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W5_roll_SoR <- SortinoRatio(W5_roll_con,MAR = 0)[1]*sqrt(252)
W5_roll_SemiD <- SemiDeviation(W5_roll_con)[1]*sqrt(252)
W5_roll_om <- Omega(W5_roll_con,L = 0,method = "simple",output = "point")-1
W5_roll_ka <- Kappa(W5_roll_con,MAR = 0,l=3)
W5_roll_MD <- maxDrawdown(as.xts(x = W5_roll_con,order.by = date[-(1:127)]))*-1
W5_roll_awchg <- mean(abs(diff(W5_roll_w)))
W5_roll_alev <- mean((1-rowSums(W5_roll_w))[(1-rowSums(W5_roll_w))<0])*-1
W5_roll_maxlev <- max((1-rowSums(W5_roll_w))*-1)
W5_roll_minlev <- min((1-rowSums(W5_roll_w))*-1)

W6_roll_sd <- StdDev.annualized(W6_roll_con, scale = 252,geometric = TRUE)
W6_roll_sk <- skewness(W6_roll_con, method = "sample")
W6_roll_ku <- kurtosis(W6_roll_con, method = "sample")
W6_roll_CVaR <- CVaR(W6_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W6_roll_GtP <- sum(W6_roll_con[W6_roll_con>0])/abs(sum(W6_roll_con[W6_roll_con<0]))
W6_roll_PR <- PainRatio(as.xts(x = W6_roll_con,order.by = date[-(1:127)]))[1]
W6_roll_ShR <- SharpeRatio(as.xts(x = W6_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W6_roll_SoR <- SortinoRatio(W6_roll_con,MAR = 0)[1]*sqrt(252)
W6_roll_SemiD <- SemiDeviation(W6_roll_con)[1]*sqrt(252)
W6_roll_om <- Omega(W6_roll_con,L = 0,method = "simple",output = "point")-1
W6_roll_ka <- Kappa(W6_roll_con,MAR = 0,l=3)
W6_roll_MD <- maxDrawdown(as.xts(x = W6_roll_con,order.by = date[-(1:127)]))*-1
W6_roll_awchg <- mean(abs(diff(W6_roll_w)))
W6_roll_alev <- mean((1-rowSums(W6_roll_w))[(1-rowSums(W6_roll_w))<0])*-1
W6_roll_maxlev <- max((1-rowSums(W6_roll_w))*-1)
W6_roll_minlev <- min((1-rowSums(W6_roll_w))*-1)

W7_roll_sd <- StdDev.annualized(W7_roll_con, scale = 252,geometric = TRUE)
W7_roll_sk <- skewness(W7_roll_con, method = "sample")
W7_roll_ku <- kurtosis(W7_roll_con, method = "sample")
W7_roll_CVaR <- CVaR(W7_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W7_roll_GtP <- sum(W7_roll_con[W7_roll_con>0])/abs(sum(W7_roll_con[W7_roll_con<0]))
W7_roll_PR <- PainRatio(as.xts(x = W7_roll_con,order.by = date[-(1:127)]))[1]
W7_roll_ShR <- SharpeRatio(as.xts(x = W7_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W7_roll_SoR <- SortinoRatio(W7_roll_con,MAR = 0)[1]*sqrt(252)
W7_roll_SemiD <- SemiDeviation(W7_roll_con)[1]*sqrt(252)
W7_roll_om <- Omega(W7_roll_con,L = 0,method = "simple",output = "point")-1
W7_roll_ka <- Kappa(W7_roll_con,MAR = 0,l=3)
W7_roll_MD <- maxDrawdown(as.xts(x = W7_roll_con,order.by = date[-(1:127)]))*-1
W7_roll_awchg <- mean(abs(diff(W7_roll_w)))
W7_roll_alev <- mean((1-rowSums(W7_roll_w))[(1-rowSums(W7_roll_w))<0])*-1
W7_roll_maxlev <- max((1-rowSums(W7_roll_w))*-1)
W7_roll_minlev <- min((1-rowSums(W7_roll_w))*-1)
