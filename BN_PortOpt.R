library(xts)
library(openxlsx)
library(tseries)
library(lubridate)
library(graphics)
library(readit)
library(PerformanceAnalytics)
library(pracma)

#Correlation

# BN_SCorAE <- 
# BN_SCorAB <- cor(r_import$AT1,r_import$BD,method = "pearson")
# BN_SCorEB <- cor(r_import$EQ,r_import$BD,method = "pearson")

#Covariance

BN_SCovAA[[1]] <- 0.00100569809958196
BN_SCovAE[[1]] <- 0.000728198488244614
BN_SCovAB[[1]] <- 0.000101873708787394
BN_SCovEE[[1]] <- 0.017569958781213
BN_SCovEB[[1]] <- 0.000175228272585438
BN_SCovBB[[1]] <- 9.34997449030352E-05

BN_SCovAA[[2]] <- 0.0018831615418127
BN_SCovAE[[2]] <- 0.000722490487838569
BN_SCovAB[[2]] <- 0.000100687232343157
BN_SCovEE[[2]] <- 0.0183693047721011
BN_SCovEB[[2]] <- 0.000186279150631764
BN_SCovBB[[2]] <- 9.54746808583945E-05

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

# bn static correlation portfolio

len_roll <- length(r_import$date)-128

er_bnpc <- NULL
er_bnpc[[1]] <- c(-0.00937298627794426,-0.0671326145145166,-0.00235836674973589)
er_bnpc[[2]] <- c(-0.0203949035219389,-0.0719728128091615,-0.00242483889725299)

rvec_bnpc <- NULL
rvec_bnpc[[1]] <- rbind(rvec_bnpc,matrix(rep(er_bnpc[[1]],each=m[4]),nrow=m[4]))
rvec_bnpc[[1]] <- xts(x = rvec_bnpc[[1]],order.by = as.Date(date[(sum(m[1:3])+1):(sum(m[1:3])+(length(rvec_bnpc[[1]])/3))]))
rvec_bnpc[[2]] <- matrix(rep(er_bnpc[[2]],each=m[5]),nrow=m[5])
rvec_bnpc[[2]] <- xts(x = rvec_bnpc[[2]],order.by = as.Date(date[(sum(m[1:4])+1):(sum(m[1:4])+(length(rvec_bnpc[[2]])/3))]))

covmat_bnpc <- NULL
covmat_bnpc[[1]] <- matrix(c(BN_SCovAA[[1]],BN_SCovAE[[1]],BN_SCovAB[[1]],BN_SCovAE[[1]],BN_SCovEE[[1]],BN_SCovEB[[1]],BN_SCovAB[[1]],BN_SCovEB[[1]],
                             BN_SCovBB[[1]]), nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(c("AT1", "BEQ", "BBD"),c("AT1", "BEQ", "BBD")))
covmat_bnpc[[2]] <- matrix(c(BN_SCovAA[[2]],BN_SCovAE[[2]],BN_SCovAB[[2]],BN_SCovAE[[2]],BN_SCovEE[[2]],BN_SCovEB[[2]],BN_SCovAB[[2]],BN_SCovEB[[2]],
                             BN_SCovBB[[2]]), nrow = 3, ncol = 3, byrow = TRUE,dimnames = list(c("AT1", "BEQ", "BBD"),c("AT1", "BEQ", "BBD")))

bnpc <- NULL

for (i in 1:2) {
  bnpc[[i]] <- portfolio.optim(rvec_bnpc[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i+3]],reshigh = w_max,covmat=covmat_bnpc[[i]])
}

for (i in 1:2) {
  bnpc[[i]]$px <- c(tcrossprod(bnpc[[i]]$pw, ret[t_sa[i+1+3]:t_sa[i+2+3]]))
}

bnpc_con <- NULL
bnpc_ann <- NULL
bnpc_cum <- NULL

bnpc_con <- c(bnpc[[1]]$px,bnpc[[2]]$px[-1])

bnpc_cum_sca <- Return.cumulative(bnpc_con,geometric = TRUE)
bnpc_cum_sca

len <- length(bnpc_con)

for (i in 1:(len)) {
  bnpc_cum[i] = Return.cumulative(bnpc_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  bnpc_ann[i] = Return.annualized(bnpc_con[1:i], scale = 252,geometric = TRUE)
}

bnpc_ann_sca = Return.annualized(bnpc_con, scale = 252,geometric = TRUE)
bnpc_ann_sca

ts.plot(as.ts(bnpc_cum),as.ts(bnpc_cum))

bnpc_w <- NULL
bnpc_w <- c(bnpc[[1]]$pw,bnpc[[2]]$pw)
bnpc_w <- matrix(bnpc_w, nrow = 2, ncol = 3, byrow = TRUE)
bnpc_w

bnpc_w_excel <- NULL

for (i in 2:3) {
  bnpc_w_excel <- c(bnpc_w_excel,replicate(m[i],bnpc[[i-1]]$pw))
}

bnpc_w_excel <- matrix(bnpc_w_excel, nrow = 261, ncol = 3, byrow = TRUE)
bnpc_perf <- bnpc_cum

bnpc_cum_sca <- Return.cumulative(bnpc_con,geometric = TRUE)
bnpc_ann_sca = Return.annualized(bnpc_con, scale = 252,geometric = TRUE)
bnpc_sd <- StdDev.annualized(bnpc_con, scale = 252,geometric = TRUE)
bnpc_sk <- skewness(bnpc_con, method = "sample")
bnpc_ku <- kurtosis(bnpc_con, method = "sample")
bnpc_CVaR <- CVaR(bnpc_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
bnpc_GtP <- sum(bnpc_con[bnpc_con>0])/abs(sum(bnpc_con[bnpc_con<0]))
bnpc_PR <- PainRatio(as.xts(x = bnpc_con,order.by = date[(521):(782)]))[1]
bnpc_ShR <- SharpeRatio(as.xts(x = bnpc_con,order.by = date[(521):(782)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
bnpc_SoR <- SortinoRatio(bnpc_con,MAR = 0)[1]*sqrt(252)
bnpc_SemiD <- SemiDeviation(bnpc_con)[1]*sqrt(252)
bnpc_om <- Omega(bnpc_con,L = 0,method = "simple",output = "point")-1
bnpc_ka <- Kappa(bnpc_con,MAR = 0,l=3)
bnpc_MD <- maxDrawdown(as.xts(x = bnpc_con,order.by = date[(521):(782)]))*-1
bnpc_awchg <- mean(abs(diff(bnpc_w)))
bnpc_alev <- mean((1-rowSums(bnpc_w))[(1-rowSums(bnpc_w))<0])*-1
bnpc_maxlev <- max((1-rowSums(bnpc_w))*-1)
bnpc_minlev <- min((1-rowSums(bnpc_w))*-1)

write.xlsx(bnpc_perf,"results/BN_performance.xlsx")


