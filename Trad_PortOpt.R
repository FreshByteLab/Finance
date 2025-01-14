library(tseries)
library(lubridate)
library(graphics)
library(readit)
library(PerformanceAnalytics)
library(pracma)

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

# extending window pearson correlation portfolio

# len_win <- length(r_import$date)-128

er_pc_ext <- NULL

# er_pc <- c(0.03,0.00035,0.0003)

for (i in 1:(len_sa-2)) {
  er_pc_ext[[i]] <- colMeans(ret[t_sa[1]:t_sa[i+1]])
}

rvec_pc_ext <- NULL

for (i in 1:(len_sa-2)) {
  rvec_pc_ext <- rbind(rvec_pc_ext,matrix(rep(er_pc_ext[[i]],each=m[i]),nrow=m[i]))
}

rvec_pc_ext <- xts(x = rvec_pc_ext,order.by = as.Date(date[1:(length(rvec_pc_ext)/3)]))

covmat_pc_ext <- NULL

for (i in 1:(len_sa-2)) {
  covmat_pc_ext[[i]] <- matrix(c(CovAA_ext[t_sa[i]],CovAE_ext[t_sa[i]],CovAB_ext[t_sa[i]],CovAE_ext[t_sa[i]],CovEE_ext[t_sa[i]],CovEB_ext[t_sa[i]],CovAB_ext[t_sa[i]],CovEB_ext[t_sa[i]],
                             CovBB_ext[t_sa[i]]), nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(c("AT1", "BEQ", "BBD"),c("AT1", "BEQ", "BBD")))
}

pc_ext <- NULL

for (i in 1:(len_sa-2)) {
  pc_ext[[i]] <- portfolio.optim(rvec_pc_ext[t_sa[i]:t_sa[i+1]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_pc_ext[[i]])
}

for (i in 1:(len_sa-2)) {
  pc_ext[[i]]$px <- c(tcrossprod(pc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

pc_ext_con <- NULL
pc_ext_ann <- NULL
pc_ext_cum <- NULL

pc_ext_con <- c(pc_ext[[1]]$px,head(pc_ext[[2]]$px,-1),head(pc_ext[[3]]$px,-1),head(pc_ext[[4]]$px,-1),head(pc_ext[[5]]$px,-1),head(pc_ext[[6]]$px,-1),head(pc_ext[[7]]$px,-1),head(pc_ext[[8]]$px,-1),head(pc_ext[[9]]$px,-1),head(pc_ext[[10]]$px,-1))

pc_ext_cum_sca <- Return.cumulative(pc_ext_con,geometric = TRUE)
pc_ext_cum_sca

c('cum_perf','ann_perf','vola','skew','kurt','CVaR','GtP','ShR','SoR','Omega','Kappa','MD','dWChg','dLev')

len <- length(pc_ext_con)

for (i in 1:(len)) {
  pc_ext_cum[i] = Return.cumulative(pc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  pc_ext_ann[i] = Return.annualized(pc_ext_con[1:i], scale = 252,geometric = TRUE)
}

pc_ext_ann_sca = Return.annualized(pc_ext_con, scale = 252,geometric = TRUE)
pc_ext_ann_sca

pc_ext_w <- NULL
pc_ext_w <- c(pc_ext[[1]]$pw,pc_ext[[2]]$pw,pc_ext[[3]]$pw,pc_ext[[4]]$pw,pc_ext[[5]]$pw,pc_ext[[6]]$pw,pc_ext[[7]]$pw,pc_ext[[8]]$pw,pc_ext[[9]]$pw,pc_ext[[10]]$pw)
pc_ext_w <- matrix(pc_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
pc_ext_w

pc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  pc_ext_w_excel <- c(pc_ext_w_excel,replicate(m[i],pc_ext[[i-1]]$pw))
}

pc_ext_w_excel <- matrix(pc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)
pc_ext_perf <- pc_ext_cum

# rolling window pearson correlation portfolio

len_roll <- length(r_import$date)-128

er_pc_roll <- NULL

# er_pc_roll <- c(0.03,0.00035,0.0003)

for (i in 1:(len_sa-2)) {
  er_pc_roll[[i]] <- colMeans(ret[t_sa[i]:t_sa[i+1]])
}

rvec_pc_roll <- NULL

for (i in 1:(len_sa-2)) {
  rvec_pc_roll <- rbind(rvec_pc_roll,matrix(rep(er_pc_roll[[i]],each=m[i]),nrow=m[i]))
}

rvec_pc_roll <- xts(x = rvec_pc_roll,order.by = as.Date(date[1:(length(rvec_pc_roll)/3)]))

covmat_pc_roll <- NULL

for (i in 1:(len_sa-2)) {
  covmat_pc_roll[[i]] <- matrix(c(CovAA_roll[t_sa[i]],CovAE_roll[t_sa[i]],CovAB_roll[t_sa[i]],CovAE_roll[t_sa[i]],CovEE_roll[t_sa[i]],CovEB_roll[t_sa[i]],CovAB_roll[t_sa[i]],CovEB_roll[t_sa[i]],
                             CovBB_roll[t_sa[i]]), nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(c("AT1", "BEQ", "BBD"),c("AT1", "BEQ", "BBD")))
}

pc_roll <- NULL

for (i in 1:(len_sa-2)) {
  pc_roll[[i]] <- portfolio.optim(rvec_pc_roll[t_sa[i]:t_sa[i+1]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_pc_roll[[i]])
}

for (i in 1:(len_sa-2)) {
  pc_roll[[i]]$px <- c(tcrossprod(pc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

pc_roll_con <- NULL
pc_roll_ann <- NULL
pc_roll_cum <- NULL

pc_roll_con <- c(pc_roll[[1]]$px,head(pc_roll[[2]]$px,-1),head(pc_roll[[3]]$px,-1),head(pc_roll[[4]]$px,-1),head(pc_roll[[5]]$px,-1),head(pc_roll[[6]]$px,-1),head(pc_roll[[7]]$px,-1),head(pc_roll[[8]]$px,-1),head(pc_roll[[9]]$px,-1),head(pc_roll[[10]]$px,-1))

pc_roll_cum_sca <- Return.cumulative(pc_roll_con,geometric = TRUE)
pc_roll_cum_sca

pc_roll_sd <- StdDev.annualized(pc_roll_con, scale = 252,geometric = TRUE)

len <- length(pc_roll_con)

for (i in 1:(len)) {
  pc_roll_cum[i] = Return.cumulative(pc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  pc_roll_ann[i] = Return.annualized(pc_roll_con[1:i], scale = 252,geometric = TRUE)
}

pc_roll_ann_sca = Return.annualized(pc_roll_con, scale = 252,geometric = TRUE)
pc_roll_ann_sca

pc_roll_w <- NULL
pc_roll_w <- c(pc_roll[[1]]$pw,pc_roll[[2]]$pw,pc_roll[[3]]$pw,pc_roll[[4]]$pw,pc_roll[[5]]$pw,pc_roll[[6]]$pw,pc_roll[[7]]$pw,pc_roll[[8]]$pw,pc_roll[[9]]$pw,pc_roll[[10]]$pw)
pc_roll_w <- matrix(pc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
pc_roll_w

pc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  pc_roll_w_excel <- c(pc_roll_w_excel,replicate(m[i],pc_roll[[i-1]]$pw))
}

pc_roll_w_excel <- matrix(pc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)
pc_roll_perf <- pc_roll_cum

#gleichgewichtetes Pf und 100% portfolios

ggw_port <- rowSums((1/3)*ret[128:len_tot])
ggw_perf <- NULL

for (i in 1:(len_tot-128)) {
  ggw_perf[i] <- Return.cumulative(ggw_port[1:i],geometric = TRUE)
}

ggw_cum_sca <- Return.cumulative(ggw_port,geometric = TRUE)
ggw_ann_sca <- Return.annualized(ggw_port, scale = 252,geometric = TRUE)

at1_port <- rowSums(ret$AT1[128:len_tot])
at1_perf <- NULL

for (i in 1:(len_tot-128)) {
  at1_perf[i] <- Return.cumulative(at1_port[1:i],geometric = TRUE)
}

at1_cum_sca <- Return.cumulative(at1_port,geometric = TRUE)
at1_ann_sca <- Return.annualized(at1_port, scale = 252,geometric = TRUE)

eq_port <- rowSums(ret$EQ[128:len_tot])
eq_perf <- NULL

for (i in 1:(len_tot-128)) {
  eq_perf[i] <- Return.cumulative(eq_port[1:i],geometric = TRUE)
}

eq_cum_sca <- Return.cumulative(eq_port,geometric = TRUE)
eq_ann_sca <- Return.annualized(eq_port, scale = 252,geometric = TRUE)

bd_port <- rowSums(ret$BD[128:len_tot])
bd_perf <- NULL

for (i in 1:(len_tot-128)) {
  bd_perf[i] <- Return.cumulative(bd_port[1:i],geometric = TRUE)
}

c(length(ggw_perf),length(at1_perf),length(eq_perf),length(bd_perf))


bd_cum_sca <- Return.cumulative(bd_port,geometric = TRUE)
bd_ann_sca <- Return.annualized(bd_port, scale = 252,geometric = TRUE)

pc_ext_sd <- StdDev.annualized(pc_ext_con, scale = 252,geometric = TRUE)
pc_ext_sk <- skewness(pc_ext_con, method = "sample")
pc_ext_ku <- kurtosis(pc_ext_con, method = "sample")
pc_ext_CVaR <- CVaR(pc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
pc_ext_GtP <- sum(pc_ext_con[pc_ext_con>0])/abs(sum(pc_ext_con[pc_ext_con<0]))
pc_ext_PR <- PainRatio(as.xts(x = pc_ext_con,order.by = date[-(1:127)]))[1]
pc_ext_ShR <- SharpeRatio(as.xts(x = pc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
pc_ext_SoR <- SortinoRatio(pc_ext_con,MAR = 0)[1]*sqrt(252)
pc_ext_SemiD <- SemiDeviation(pc_ext_con)[1]*sqrt(252)
pc_ext_om <- Omega(pc_ext_con,L = 0,method = "simple",output = "point")-1
pc_ext_ka <- Kappa(pc_ext_con,MAR = 0,l=3)
pc_ext_MD <- maxDrawdown(as.xts(x = pc_ext_con,order.by = date[-(1:127)]))*-1
pc_ext_awchg <- mean(abs(diff(pc_ext_w)))
pc_ext_alev <- mean((1-rowSums(pc_ext_w))[(1-rowSums(pc_ext_w))<0])*-1
pc_ext_maxlev <- max((1-rowSums(pc_ext_w))*-1)
pc_ext_minlev <- min((1-rowSums(pc_ext_w))*-1)

pc_roll_sd <- StdDev.annualized(pc_roll_con, scale = 252,geometric = TRUE)
pc_roll_sk <- skewness(pc_roll_con, method = "sample")
pc_roll_ku <- kurtosis(pc_roll_con, method = "sample")
pc_roll_CVaR <- CVaR(pc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
pc_roll_GtP <- sum(pc_roll_con[pc_roll_con>0])/abs(sum(pc_roll_con[pc_roll_con<0]))
pc_roll_PR <- PainRatio(as.xts(x = pc_roll_con,order.by = date[-(1:127)]))[1]
pc_roll_ShR <- SharpeRatio(as.xts(x = pc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
pc_roll_SoR <- SortinoRatio(pc_roll_con,MAR = 0)[1]*sqrt(252)
pc_roll_SemiD <- SemiDeviation(pc_roll_con)[1]*sqrt(252)
pc_roll_om <- Omega(pc_roll_con,L = 0,method = "simple",output = "point")-1
pc_roll_ka <- Kappa(pc_roll_con,MAR = 0,l=3)
pc_roll_MD <- maxDrawdown(as.xts(x = pc_roll_con,order.by = date[-(1:127)]))*-1
pc_roll_awchg <- mean(abs(diff(pc_roll_w)))
pc_roll_alev <- mean((1-rowSums(pc_roll_w))[(1-rowSums(pc_roll_w))<0])*-1
pc_roll_maxlev <- max((1-rowSums(pc_roll_w))*-1)
pc_roll_minlev <- min((1-rowSums(pc_roll_w))*-1)

ggw_sd <- StdDev.annualized(ggw_port, scale = 252,geometric = TRUE)
ggw_sk <- skewness(ggw_port, method = "sample")
ggw_ku <- kurtosis(ggw_port, method = "sample")
ggw_CVaR <- CVaR(ggw_port,p = 0.95,method = "gaussian")[1]*sqrt(252)
ggw_GtP <- sum(ggw_port[ggw_port>0])/abs(sum(ggw_port[ggw_port<0]))
ggw_PR <- PainRatio(as.xts(x = ggw_port,order.by = date[-(1:127)]))[1]
ggw_ShR <- SharpeRatio(as.xts(x = ggw_port,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
ggw_SoR <- SortinoRatio(ggw_port,MAR = 0)[1]*sqrt(252)
ggw_SemiD <- SemiDeviation(ggw_port)[1]*sqrt(252)
ggw_om <- Omega(ggw_port,L = 0,method = "simple",output = "point")-1
ggw_ka <- Kappa(ggw_port,MAR = 0,l=3)
ggw_MD <- maxDrawdown(as.xts(x = ggw_port,order.by = date[-(1:127)]))*-1
ggw_awchg <- 0
ggw_alev <- 0
ggw_maxlev <- 0
ggw_minlev <- 0

at1_sd <- StdDev.annualized(at1_port, scale = 252,geometric = TRUE)
at1_sk <- skewness(at1_port, method = "sample")
at1_ku <- kurtosis(at1_port, method = "sample")
at1_CVaR <- CVaR(at1_port,p = 0.95,method = "gaussian")[1]*sqrt(252)
at1_GtP <- sum(at1_port[at1_port>0])/abs(sum(at1_port[at1_port<0]))
at1_PR <- PainRatio(as.xts(x = at1_port,order.by = date[-(1:127)]))[1]
at1_ShR <- SharpeRatio(as.xts(x = at1_port,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
at1_SoR <- SortinoRatio(at1_port,MAR = 0)[1]*sqrt(252)
at1_SemiD <- SemiDeviation(at1_port)[1]*sqrt(252)
at1_om <- Omega(at1_port,L = 0,method = "simple",output = "point")-1
at1_ka <- Kappa(at1_port,MAR = 0,l=3)
at1_MD <- maxDrawdown(as.xts(x = at1_port,order.by = date[-(1:127)]))*-1
at1_awchg <- 0
at1_alev <- 0
at1_maxlev <- 0
at1_minlev <- 0

eq_sd <- StdDev.annualized(eq_port, scale = 252,geometric = TRUE)
eq_sk <- skewness(eq_port, method = "sample")
eq_ku <- kurtosis(eq_port, method = "sample")
eq_CVaR <- CVaR(eq_port,p = 0.95,method = "gaussian")[1]*sqrt(252)
eq_GtP <- sum(eq_port[eq_port>0])/abs(sum(eq_port[eq_port<0]))
eq_PR <- PainRatio(as.xts(x = eq_port,order.by = date[-(1:127)]))[1]
eq_ShR <- SharpeRatio(as.xts(x = eq_port,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
eq_SoR <- SortinoRatio(eq_port,MAR = 0)[1]*sqrt(252)
eq_SemiD <- SemiDeviation(eq_port)[1]*sqrt(252)
eq_om <- Omega(eq_port,L = 0,method = "simple",output = "point")-1
eq_ka <- Kappa(eq_port,MAR = 0,l=3)
eq_MD <- maxDrawdown(as.xts(x = eq_port,order.by = date[-(1:127)]))*-1
eq_awchg <- 0
eq_alev <- 0
eq_maxlev <- 0
eq_minlev <- 0

bd_sd <- StdDev.annualized(bd_port, scale = 252,geometric = TRUE)
bd_sk <- skewness(bd_port, method = "sample")
bd_ku <- kurtosis(bd_port, method = "sample")
bd_CVaR <- CVaR(bd_port,p = 0.95,method = "gaussian")[1]*sqrt(252)
bd_GtP <- sum(bd_port[bd_port>0])/abs(sum(bd_port[bd_port<0]))
bd_PR <- PainRatio(as.xts(x = bd_port,order.by = date[-(1:127)]))[1]
bd_ShR <- SharpeRatio(as.xts(x = bd_port,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bd_SoR <- SortinoRatio(bd_port,MAR = 0)[1]*sqrt(252)
bd_SemiD <- SemiDeviation(bd_port)[1]*sqrt(252)
bd_om <- Omega(bd_port,L = 0,method = "simple",output = "point")-1
bd_ka <- Kappa(bd_port,MAR = 0,l=3)
bd_MD <- maxDrawdown(as.xts(x = bd_port,order.by = date[-(1:127)]))*-1
bd_awchg <- 0
bd_alev <- 0
bd_maxlev <- 0
bd_minlev <- 0

bn_pc_roll_cum_sca <- Return.cumulative(pc_roll_con[(521-127):(782-127)],geometric = TRUE)
bn_pc_roll_ann_sca = Return.annualized(pc_roll_con[(521-127):(782-127)], scale = 252,geometric = TRUE)
bn_pc_roll_sd <- StdDev.annualized(pc_roll_con[(521-127):(782-127)], scale = 252,geometric = TRUE)
bn_pc_roll_sk <- skewness(pc_roll_con[(521-127):(782-127)], method = "sample")
bn_pc_roll_ku <- kurtosis(pc_roll_con[(521-127):(782-127)], method = "sample")
bn_pc_roll_CVaR <- CVaR(pc_roll_con[(521-127):(782-127)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_pc_roll_GtP <- sum(pc_roll_con[(521-127):(782-127)][pc_roll_con[(521-127):(782-127)]>0])/abs(sum(pc_roll_con[(521-127):(782-127)][pc_roll_con[(521-127):(782-127)]<0]))
bn_pc_roll_PR <- PainRatio(as.xts(x = pc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]))[1]
bn_pc_roll_ShR <- SharpeRatio(as.xts(x = pc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_pc_roll_SoR <- SortinoRatio(pc_roll_con[(521-127):(782-127)],MAR = 0)[1]*sqrt(252)
bn_pc_roll_SemiD <- SemiDeviation(pc_roll_con[(521-127):(782-127)])[1]*sqrt(252)
bn_pc_roll_om <- Omega(pc_roll_con[(521-127):(782-127)],L = 0,method = "simple",output = "point")-1
bn_pc_roll_ka <- Kappa(pc_roll_con[(521-127):(782-127)],MAR = 0,l=3)
bn_pc_roll_MD <- maxDrawdown(as.xts(x = pc_roll_con[(521-127):(782-127)],order.by = date[(521):(782)]))*-1
bn_pc_roll_awchg <- mean(abs(diff(pc_roll_w[4:5,])))
bn_pc_roll_alev <- mean((1-rowSums(pc_roll_w[4:5,]))[(1-rowSums(pc_roll_w[4:5,]))<0])*-1
bn_pc_roll_maxlev <- max((1-rowSums(pc_roll_w[4:5,]))*-1)
bn_pc_roll_minlev <- min((1-rowSums(pc_roll_w[4:5,]))*-1)

bn_ggw_cum_sca <- Return.cumulative(ggw_port[(521-128):(782-128)],geometric = TRUE)
bn_ggw_ann_sca = Return.annualized(ggw_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_ggw_sd <- StdDev.annualized(ggw_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_ggw_sk <- skewness(ggw_port[(521-128):(782-128)], method = "sample")
bn_ggw_ku <- kurtosis(ggw_port[(521-128):(782-128)], method = "sample")
bn_ggw_CVaR <- CVaR(ggw_port[(521-128):(782-128)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_ggw_GtP <- sum(ggw_port[(521-128):(782-128)][ggw_port[(521-128):(782-128)]>0])/abs(sum(ggw_port[(521-128):(782-128)][ggw_port[(521-128):(782-128)]<0]))
bn_ggw_PR <- PainRatio(as.xts(x = ggw_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))[1]
bn_ggw_ShR <- SharpeRatio(as.xts(x = ggw_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_ggw_SoR <- SortinoRatio(ggw_port[(521-128):(782-128)],MAR = 0)[1]*sqrt(252)
bn_ggw_SemiD <- SemiDeviation(ggw_port[(521-128):(782-128)])[1]*sqrt(252)
bn_ggw_om <- Omega(ggw_port[(521-128):(782-128)],L = 0,method = "simple",output = "point")-1
bn_ggw_ka <- Kappa(ggw_port[(521-128):(782-128)],MAR = 0,l=3)
bn_ggw_MD <- maxDrawdown(as.xts(x = ggw_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))*-1
bn_ggw_awchg <- 0
bn_ggw_alev <- 0
bn_ggw_maxlev <- 0
bn_ggw_minlev <- 0

bn_at1_cum_sca <- Return.cumulative(at1_port[(521-128):(782-128)],geometric = TRUE)
bn_at1_ann_sca = Return.annualized(at1_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_at1_sd <- StdDev.annualized(at1_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_at1_sk <- skewness(at1_port[(521-128):(782-128)], method = "sample")
bn_at1_ku <- kurtosis(at1_port[(521-128):(782-128)], method = "sample")
bn_at1_CVaR <- CVaR(at1_port[(521-128):(782-128)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_at1_GtP <- sum(at1_port[(521-128):(782-128)][at1_port[(521-128):(782-128)]>0])/abs(sum(at1_port[(521-128):(782-128)][at1_port[(521-128):(782-128)]<0]))
bn_at1_PR <- PainRatio(as.xts(x = at1_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))[1]
bn_at1_ShR <- SharpeRatio(as.xts(x = at1_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_at1_SoR <- SortinoRatio(at1_port[(521-128):(782-128)],MAR = 0)[1]*sqrt(252)
bn_at1_SemiD <- SemiDeviation(at1_port[(521-128):(782-128)])[1]*sqrt(252)
bn_at1_om <- Omega(at1_port[(521-128):(782-128)],L = 0,method = "simple",output = "point")-1
bn_at1_ka <- Kappa(at1_port[(521-128):(782-128)],MAR = 0,l=3)
bn_at1_MD <- maxDrawdown(as.xts(x = at1_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))*-1
bn_at1_awchg <- 0
bn_at1_alev <- 0
bn_at1_maxlev <- 0
bn_at1_minlev <- 0

bn_eq_cum_sca <- Return.cumulative(eq_port[(521-128):(782-128)],geometric = TRUE)
bn_eq_ann_sca = Return.annualized(eq_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_eq_sd <- StdDev.annualized(eq_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_eq_sk <- skewness(eq_port[(521-128):(782-128)], method = "sample")
bn_eq_ku <- kurtosis(eq_port[(521-128):(782-128)], method = "sample")
bn_eq_CVaR <- CVaR(eq_port[(521-128):(782-128)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_eq_GtP <- sum(eq_port[(521-128):(782-128)][eq_port[(521-128):(782-128)]>0])/abs(sum(eq_port[(521-128):(782-128)][eq_port[(521-128):(782-128)]<0]))
bn_eq_PR <- PainRatio(as.xts(x = eq_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))[1]
bn_eq_ShR <- SharpeRatio(as.xts(x = eq_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_eq_SoR <- SortinoRatio(eq_port[(521-128):(782-128)],MAR = 0)[1]*sqrt(252)
bn_eq_SemiD <- SemiDeviation(eq_port[(521-128):(782-128)])[1]*sqrt(252)
bn_eq_om <- Omega(eq_port[(521-128):(782-128)],L = 0,method = "simple",output = "point")-1
bn_eq_ka <- Kappa(eq_port[(521-128):(782-128)],MAR = 0,l=3)
bn_eq_MD <- maxDrawdown(as.xts(x = eq_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))*-1
bn_eq_awchg <- 0
bn_eq_alev <- 0
bn_eq_maxlev <- 0
bn_eq_minlev <- 0

bn_bd_cum_sca <- Return.cumulative(bd_port[(521-128):(782-128)],geometric = TRUE)
bn_bd_ann_sca = Return.annualized(bd_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_bd_sd <- StdDev.annualized(bd_port[(521-128):(782-128)], scale = 252,geometric = TRUE)
bn_bd_sk <- skewness(bd_port[(521-128):(782-128)], method = "sample")
bn_bd_ku <- kurtosis(bd_port[(521-128):(782-128)], method = "sample")
bn_bd_CVaR <- CVaR(bd_port[(521-128):(782-128)],p = 0.95,method = "gaussian")[1]*sqrt(252)
bn_bd_GtP <- sum(bd_port[(521-128):(782-128)][bd_port[(521-128):(782-128)]>0])/abs(sum(bd_port[(521-128):(782-128)][bd_port[(521-128):(782-128)]<0]))
bn_bd_PR <- PainRatio(as.xts(x = bd_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))[1]
bn_bd_ShR <- SharpeRatio(as.xts(x = bd_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bn_bd_SoR <- SortinoRatio(bd_port[(521-128):(782-128)],MAR = 0)[1]*sqrt(252)
bn_bd_SemiD <- SemiDeviation(bd_port[(521-128):(782-128)])[1]*sqrt(252)
bn_bd_om <- Omega(bd_port[(521-128):(782-128)],L = 0,method = "simple",output = "point")-1
bn_bd_ka <- Kappa(bd_port[(521-128):(782-128)],MAR = 0,l=3)
bn_bd_MD <- maxDrawdown(as.xts(x = bd_port[(521-128):(782-128)],order.by = date[(521-128):(782-128)]))*-1
bn_bd_awchg <- 0
bn_bd_alev <- 0
bn_bd_maxlev <- 0
bn_bd_minlev <- 0
