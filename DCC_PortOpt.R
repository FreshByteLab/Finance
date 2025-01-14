library(tseries)
library(PerformanceAnalytics)

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

# rolling window DCC portfolio

len_roll <- length(r_import$date)-128

er_dcc_roll <- NULL

for (i in 1:(len_sa-2)) {
  er_dcc_roll[[i]] <- colMeans(ret[t_sa[i]:t_sa[i+1]])
}

rvec_dcc_roll <- NULL

for (i in 1:(len_sa-2)) {
  rvec_dcc_roll <- rbind(rvec_dcc_roll,matrix(rep(er_dcc_roll[[i]],each=m[i]),nrow=m[i]))
}

rvec_dcc_roll <- xts(x = rvec_dcc_roll,order.by = as.Date(date[1:(length(rvec_dcc_roll)/3)]))

dcc_roll <- NULL

for (i in 1:(len_sa-2)) {
  dcc_roll[[i]] <- portfolio.optim(rvec_dcc_roll[t_sa[i]:t_sa[i+1]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_dcc_roll[[i]])
}

for (i in 1:(len_sa-2)) {
  dcc_roll[[i]]$px <- c(tcrossprod(dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

dcc_roll_con <- NULL
dcc_roll_ann <- NULL
dcc_roll_cum <- NULL

dcc_roll_con <- c(dcc_roll[[1]]$px,head(dcc_roll[[2]]$px,-1),head(dcc_roll[[3]]$px,-1),head(dcc_roll[[4]]$px,-1),head(dcc_roll[[5]]$px,-1),head(dcc_roll[[6]]$px,-1),head(dcc_roll[[7]]$px,-1),head(dcc_roll[[8]]$px,-1),head(dcc_roll[[9]]$px,-1),head(dcc_roll[[10]]$px,-1))

dcc_roll_cum_sca <- Return.cumulative(dcc_roll_con,geometric = TRUE)
dcc_roll_cum_sca

len <- length(dcc_roll_con)

for (i in 1:(len)) {
  dcc_roll_cum[i] = Return.cumulative(dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  dcc_roll_ann[i] = Return.annualized(dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

dcc_roll_ann_sca = Return.annualized(dcc_roll_con, scale = 252,geometric = TRUE)
dcc_roll_ann_sca

ts.plot(as.ts(dcc_roll_cum))

dcc_roll_w <- NULL
dcc_roll_w <- c(dcc_roll[[1]]$pw,dcc_roll[[2]]$pw,dcc_roll[[3]]$pw,dcc_roll[[4]]$pw,dcc_roll[[5]]$pw,dcc_roll[[6]]$pw,dcc_roll[[7]]$pw,dcc_roll[[8]]$pw,dcc_roll[[9]]$pw,dcc_roll[[10]]$pw)
dcc_roll_w <- matrix(dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
dcc_roll_w

dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  dcc_roll_w_excel <- c(dcc_roll_w_excel,replicate(m[i],dcc_roll[[i-1]]$pw))
}

dcc_roll_w_excel <- matrix(dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

dcc_roll_perf <- dcc_cum

# extending window DCC portfolio

len_roll <- length(r_import$date)-128

er_dcc_ext <- NULL

for (i in 1:(len_sa-2)) {
  er_dcc_ext[[i]] <- colMeans(ret[t_sa[1]:t_sa[i+1]])
}

rvec_dcc_ext <- NULL

for (i in 1:(len_sa-2)) {
  rvec_dcc_ext <- rbind(rvec_dcc_ext,matrix(rep(er_dcc_ext[[i]],each=m[i]),nrow=m[i]))
}

rvec_dcc_ext <- xts(x = rvec_dcc_ext,order.by = as.Date(date[1:(length(rvec_dcc_ext)/3)]))

dcc_ext <- NULL

for (i in 1:(len_sa-2)) {
  dcc_ext[[i]] <- portfolio.optim(rvec_dcc_ext[t_sa[i]:t_sa[i+1]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_dcc_ext[[i]])
}

for (i in 1:(len_sa-2)) {
  dcc_ext[[i]]$px <- c(tcrossprod(dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

dcc_ext_con <- NULL
dcc_ext_ann <- NULL
dcc_ext_cum <- NULL

dcc_ext_con <- c(dcc_ext[[1]]$px,head(dcc_ext[[2]]$px,-1),head(dcc_ext[[3]]$px,-1),head(dcc_ext[[4]]$px,-1),head(dcc_ext[[5]]$px,-1),head(dcc_ext[[6]]$px,-1),head(dcc_ext[[7]]$px,-1),head(dcc_ext[[8]]$px,-1),head(dcc_ext[[9]]$px,-1),head(dcc_ext[[10]]$px,-1))

dcc_ext_cum_sca <- Return.cumulative(dcc_ext_con,geometric = TRUE)
dcc_ext_cum_sca

len <- length(dcc_ext_con)

for (i in 1:(len)) {
  dcc_ext_cum[i] = Return.cumulative(dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  dcc_ext_ann[i] = Return.annualized(dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

dcc_ext_ann_sca = Return.annualized(dcc_ext_con, scale = 252,geometric = TRUE)
dcc_ext_ann_sca

ts.plot(as.ts(dcc_ext_cum))

dcc_ext_w <- NULL
dcc_ext_w <- c(dcc_ext[[1]]$pw,dcc_ext[[2]]$pw,dcc_ext[[3]]$pw,dcc_ext[[4]]$pw,dcc_ext[[5]]$pw,dcc_ext[[6]]$pw,dcc_ext[[7]]$pw,dcc_ext[[8]]$pw,dcc_ext[[9]]$pw,dcc_ext[[10]]$pw)
dcc_ext_w <- matrix(dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
dcc_ext_w

dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  dcc_ext_w_excel <- c(dcc_ext_w_excel,replicate(m[i],dcc_ext[[i-1]]$pw))
}

dcc_ext_w_excel <- matrix(dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

dcc_ext_perf <- dcc_ext_cum

dcc_ext_sd <- StdDev.annualized(dcc_ext_con, scale = 252,geometric = TRUE)
dcc_ext_sk <- skewness(dcc_ext_con, method = "sample")
dcc_ext_ku <- kurtosis(dcc_ext_con, method = "sample")
dcc_ext_CVaR <- CVaR(dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
dcc_ext_GtP <- sum(dcc_ext_con[dcc_ext_con>0])/abs(sum(dcc_ext_con[dcc_ext_con<0]))
dcc_ext_PR <- PainRatio(as.xts(x = dcc_ext_con,order.by = date[-(1:127)]))[1]
dcc_ext_ShR <- SharpeRatio(as.xts(x = dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
dcc_ext_SoR <- SortinoRatio(dcc_ext_con,MAR = 0)[1]*sqrt(252)
dcc_ext_SemiD <- SemiDeviation(dcc_ext_con)[1]*sqrt(252)
dcc_ext_om <- Omega(dcc_ext_con,L = 0,method = "simple",output = "point")-1
dcc_ext_ka <- Kappa(dcc_ext_con,MAR = 0,l=3)
dcc_ext_MD <- maxDrawdown(as.xts(x = dcc_ext_con,order.by = date[-(1:127)]))*-1
dcc_ext_awchg <- mean(abs(diff(dcc_ext_w)))
dcc_ext_alev <- mean((1-rowSums(dcc_ext_w))[(1-rowSums(dcc_ext_w))<0])*-1
dcc_ext_maxlev <- max((1-rowSums(dcc_ext_w))*-1)
dcc_ext_minlev <- min((1-rowSums(dcc_ext_w))*-1)

dcc_roll_sd <- StdDev.annualized(dcc_roll_con, scale = 252,geometric = TRUE)
dcc_roll_sk <- skewness(dcc_roll_con, method = "sample")
dcc_roll_ku <- kurtosis(dcc_roll_con, method = "sample")
dcc_roll_CVaR <- CVaR(dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
dcc_roll_GtP <- sum(dcc_roll_con[dcc_roll_con>0])/abs(sum(dcc_roll_con[dcc_roll_con<0]))
dcc_roll_PR <- PainRatio(as.xts(x = dcc_roll_con,order.by = date[-(1:127)]))[1]
dcc_roll_ShR <- SharpeRatio(as.xts(x = dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
dcc_roll_SoR <- SortinoRatio(dcc_roll_con,MAR = 0)[1]*sqrt(252)
dcc_roll_SemiD <- SemiDeviation(dcc_roll_con)[1]*sqrt(252)
dcc_roll_om <- Omega(dcc_roll_con,L = 0,method = "simple",output = "point")-1
dcc_roll_ka <- Kappa(dcc_roll_con,MAR = 0,l=3)
dcc_roll_MD <- maxDrawdown(as.xts(x = dcc_roll_con,order.by = date[-(1:127)]))*-1
dcc_roll_awchg <- mean(abs(diff(dcc_roll_w)))
dcc_roll_alev <- mean((1-rowSums(dcc_roll_w))[(1-rowSums(dcc_roll_w))<0])*-1
dcc_roll_maxlev <- max((1-rowSums(dcc_roll_w))*-1)
dcc_roll_minlev <- min((1-rowSums(dcc_roll_w))*-1)

bn_dcc_roll_cum_sca <- Return.cumulative(dcc_roll_con[(521-127):(782-127)],geometric = TRUE)
bn_dcc_roll_ann_sca = Return.annualized(dcc_roll_con[(521-127):(782-127)], scale = 252,geometric = TRUE)
bn_dcc_roll_sd <- StdDev.annualized(dcc_roll_con[(521-127):(782-127)], scale = 252,geometric = TRUE)
bn_dcc_roll_sk <- skewness(dcc_roll_con[(521-127):(782-127)], method = "sample")
bn_dcc_roll_ku <- kurtosis(dcc_roll_con[(521-127):(782-127)], method = "sample")
bn_dcc_roll_CVaR <- CVaR(dcc_roll_con[(521-127):(782-127)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_dcc_roll_GtP <- sum(dcc_roll_con[(521-127):(782-127)][dcc_roll_con[(521-127):(782-127)]>0])/abs(sum(dcc_roll_con[(521-127):(782-127)][dcc_roll_con[(521-127):(782-127)]<0]))
bn_dcc_roll_PR <- PainRatio(as.xts(x = dcc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]))[1]
bn_dcc_roll_ShR <- SharpeRatio(as.xts(x = dcc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_dcc_roll_SoR <- SortinoRatio(dcc_roll_con[(521-127):(782-127)],MAR = 0)[1]*sqrt(252)
bn_dcc_roll_SemiD <- SemiDeviation(dcc_roll_con[(521-127):(782-127)])[1]*sqrt(252)
bn_dcc_roll_om <- Omega(dcc_roll_con[(521-127):(782-127)],L = 0,method = "simple",output = "point")-1
bn_dcc_roll_ka <- Kappa(dcc_roll_con[(521-127):(782-127)],MAR = 0,l=3)
bn_dcc_roll_MD <- maxDrawdown(as.xts(x = dcc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]))*-1
bn_dcc_roll_awchg <- mean(abs(diff(dcc_roll_w[4:5,])))
bn_dcc_roll_alev <- mean((1-rowSums(dcc_roll_w[4:5,]))[(1-rowSums(dcc_roll_w[4:5,]))<0])*-1
bn_dcc_roll_maxlev <- max((1-rowSums(dcc_roll_w[4:5,]))*-1)
bn_dcc_roll_minlev <- min((1-rowSums(dcc_roll_w[4:5,]))*-1)
