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

er_W1_dcc_ext <- NULL
er_W2_dcc_ext <- NULL
er_W3_dcc_ext <- NULL
er_W4_dcc_ext <- NULL
er_W5_dcc_ext <- NULL
er_W6_dcc_ext <- NULL
er_W7_dcc_ext <- NULL

for (i in 1:(len_sa-2)) {
  er_W1_dcc_ext[[i]] <- colMeans(W1.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W2_dcc_ext[[i]] <- colMeans(W2.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W3_dcc_ext[[i]] <- colMeans(W3.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W4_dcc_ext[[i]] <- colMeans(W4.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W5_dcc_ext[[i]] <- colMeans(W5.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W6_dcc_ext[[i]] <- colMeans(W6.dcc[[i]][t_sa[1]:t_sa[i+1],])
  er_W7_dcc_ext[[i]] <- colMeans(W7.dcc[[i]][t_sa[1]:t_sa[i+1],])
}

rvec_W1_dcc_ext <- NULL
rvec_W2_dcc_ext <- NULL
rvec_W3_dcc_ext <- NULL
rvec_W4_dcc_ext <- NULL
rvec_W5_dcc_ext <- NULL
rvec_W6_dcc_ext <- NULL
rvec_W7_dcc_ext <- NULL

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
  rvec_W1_dcc_ext[[i]] <- matrix(rep(er_W1_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W2_dcc_ext[[i]] <- matrix(rep(er_W2_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W3_dcc_ext[[i]] <- matrix(rep(er_W3_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W4_dcc_ext[[i]] <- matrix(rep(er_W4_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W5_dcc_ext[[i]] <- matrix(rep(er_W5_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W6_dcc_ext[[i]] <- matrix(rep(er_W6_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W7_dcc_ext[[i]] <- matrix(rep(er_W7_dcc_ext[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
}

for (i in 1:(len_sa-2)) {
  rvec_W1_dcc_ext[[i]] <- xts(x = rvec_W1_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W1_dcc_ext[[i]])/3)]))
  rvec_W2_dcc_ext[[i]] <- xts(x = rvec_W2_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W2_dcc_ext[[i]])/3)]))
  rvec_W3_dcc_ext[[i]] <- xts(x = rvec_W3_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W3_dcc_ext[[i]])/3)]))
  rvec_W4_dcc_ext[[i]] <- xts(x = rvec_W4_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W4_dcc_ext[[i]])/3)]))
  rvec_W5_dcc_ext[[i]] <- xts(x = rvec_W5_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W5_dcc_ext[[i]])/3)]))
  rvec_W6_dcc_ext[[i]] <- xts(x = rvec_W6_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W6_dcc_ext[[i]])/3)]))
  rvec_W7_dcc_ext[[i]] <- xts(x = rvec_W7_dcc_ext[[i]],order.by = as.Date(date[1:(length(rvec_W7_dcc_ext[[i]])/3)]))
}

W1_dcc_ext <- NULL
W2_dcc_ext <- NULL
W3_dcc_ext <- NULL
W4_dcc_ext <- NULL
W5_dcc_ext <- NULL
W6_dcc_ext <- NULL
W7_dcc_ext <- NULL

for (i in 1:(len_sa-2)) {
  W1_dcc_ext[[i]] <- portfolio.optim(rvec_W1_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W1_dcc_ext[[i]])
  W2_dcc_ext[[i]] <- portfolio.optim(rvec_W2_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W2_dcc_ext[[i]])
  W3_dcc_ext[[i]] <- portfolio.optim(rvec_W3_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W3_dcc_ext[[i]])
  W4_dcc_ext[[i]] <- portfolio.optim(rvec_W4_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W4_dcc_ext[[i]])
  W5_dcc_ext[[i]] <- portfolio.optim(rvec_W5_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W5_dcc_ext[[i]])
  W6_dcc_ext[[i]] <- portfolio.optim(rvec_W6_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W6_dcc_ext[[i]])
  W7_dcc_ext[[i]] <- portfolio.optim(rvec_W7_dcc_ext[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W7_dcc_ext[[i]])
}

for (i in 1:(len_sa-2)) {
  W1_dcc_ext[[i]]$px <- c(tcrossprod(W1_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W2_dcc_ext[[i]]$px <- c(tcrossprod(W2_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W3_dcc_ext[[i]]$px <- c(tcrossprod(W3_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W4_dcc_ext[[i]]$px <- c(tcrossprod(W4_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W5_dcc_ext[[i]]$px <- c(tcrossprod(W5_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W6_dcc_ext[[i]]$px <- c(tcrossprod(W6_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W7_dcc_ext[[i]]$px <- c(tcrossprod(W7_dcc_ext[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

W1_dcc_ext_con <- NULL
W1_dcc_ext_ann <- NULL
W1_dcc_ext_cum <- NULL

W1_dcc_ext_con <- c(W1_dcc_ext[[1]]$px,head(W1_dcc_ext[[2]]$px,-1),head(W1_dcc_ext[[3]]$px,-1),head(W1_dcc_ext[[4]]$px,-1),head(W1_dcc_ext[[5]]$px,-1),head(W1_dcc_ext[[6]]$px,-1),head(W1_dcc_ext[[7]]$px,-1),head(W1_dcc_ext[[8]]$px,-1),head(W1_dcc_ext[[9]]$px,-1),head(W1_dcc_ext[[10]]$px,-1))

W1_dcc_ext_cum_sca <- Return.cumulative(W1_dcc_ext_con,geometric = TRUE)
W1_dcc_ext_cum_sca

len <- length(W1_dcc_ext_con)

for (i in 1:(len)) {
  W1_dcc_ext_cum[i] = Return.cumulative(W1_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W1_dcc_ext_ann[i] = Return.annualized(W1_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W1_dcc_ext_ann_sca = Return.annualized(W1_dcc_ext_con, scale = 252,geometric = TRUE)
W1_dcc_ext_ann_sca

ts.plot(as.ts(W1_dcc_ext_cum),as.ts(dcc_cum))

W1_dcc_ext_w <- NULL
W1_dcc_ext_w <- c(W1_dcc_ext[[1]]$pw,W1_dcc_ext[[2]]$pw,W1_dcc_ext[[3]]$pw,W1_dcc_ext[[4]]$pw,W1_dcc_ext[[5]]$pw,W1_dcc_ext[[6]]$pw,W1_dcc_ext[[7]]$pw,W1_dcc_ext[[8]]$pw,W1_dcc_ext[[9]]$pw,W1_dcc_ext[[10]]$pw)
W1_dcc_ext_w <- matrix(W1_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W1_dcc_ext_w

W1_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W1_dcc_ext_w_excel <- c(W1_dcc_ext_w_excel,replicate(m[i],W1_dcc_ext[[i-1]]$pw))
}

W1_dcc_ext_w_excel <- matrix(W1_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W1_dcc_ext_perf <- W1_dcc_ext_cum

W2_dcc_ext_con <- NULL
W2_dcc_ext_ann <- NULL
W2_dcc_ext_cum <- NULL

W2_dcc_ext_con <- c(W2_dcc_ext[[1]]$px,head(W2_dcc_ext[[2]]$px,-1),head(W2_dcc_ext[[3]]$px,-1),head(W2_dcc_ext[[4]]$px,-1),head(W2_dcc_ext[[5]]$px,-1),head(W2_dcc_ext[[6]]$px,-1),head(W2_dcc_ext[[7]]$px,-1),head(W2_dcc_ext[[8]]$px,-1),head(W2_dcc_ext[[9]]$px,-1),head(W2_dcc_ext[[10]]$px,-1))

W2_dcc_ext_cum_sca <- Return.cumulative(W2_dcc_ext_con,geometric = TRUE)
W2_dcc_ext_cum_sca

len <- length(W2_dcc_ext_con)

for (i in 1:(len)) {
  W2_dcc_ext_cum[i] = Return.cumulative(W2_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W2_dcc_ext_ann[i] = Return.annualized(W2_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W2_dcc_ext_ann_sca = Return.annualized(W2_dcc_ext_con, scale = 252,geometric = TRUE)
W2_dcc_ext_ann_sca

ts.plot(as.ts(W2_dcc_ext_cum))

W2_dcc_ext_w <- NULL
W2_dcc_ext_w <- c(W2_dcc_ext[[1]]$pw,W2_dcc_ext[[2]]$pw,W2_dcc_ext[[3]]$pw,W2_dcc_ext[[4]]$pw,W2_dcc_ext[[5]]$pw,W2_dcc_ext[[6]]$pw,W2_dcc_ext[[7]]$pw,W2_dcc_ext[[8]]$pw,W2_dcc_ext[[9]]$pw,W2_dcc_ext[[10]]$pw)
W2_dcc_ext_w <- matrix(W2_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W2_dcc_ext_w

W2_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W2_dcc_ext_w_excel <- c(W2_dcc_ext_w_excel,replicate(m[i],W2_dcc_ext[[i-1]]$pw))
}

W2_dcc_ext_w_excel <- matrix(W2_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W2_dcc_ext_perf <- W2_dcc_ext_cum

W3_dcc_ext_con <- NULL
W3_dcc_ext_ann <- NULL
W3_dcc_ext_cum <- NULL

W3_dcc_ext_con <- c(W3_dcc_ext[[1]]$px,head(W3_dcc_ext[[2]]$px,-1),head(W3_dcc_ext[[3]]$px,-1),head(W3_dcc_ext[[4]]$px,-1),head(W3_dcc_ext[[5]]$px,-1),head(W3_dcc_ext[[6]]$px,-1),head(W3_dcc_ext[[7]]$px,-1),head(W3_dcc_ext[[8]]$px,-1),head(W3_dcc_ext[[9]]$px,-1),head(W3_dcc_ext[[10]]$px,-1))

W3_dcc_ext_cum_sca <- Return.cumulative(W3_dcc_ext_con,geometric = TRUE)
W3_dcc_ext_cum_sca

len <- length(W3_dcc_ext_con)

for (i in 1:(len)) {
  W3_dcc_ext_cum[i] = Return.cumulative(W3_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W3_dcc_ext_ann[i] = Return.annualized(W3_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W3_dcc_ext_ann_sca = Return.annualized(W3_dcc_ext_con, scale = 252,geometric = TRUE)
W3_dcc_ext_ann_sca

ts.plot(as.ts(W3_dcc_ext_cum))

W3_dcc_ext_w <- NULL
W3_dcc_ext_w <- c(W3_dcc_ext[[1]]$pw,W3_dcc_ext[[2]]$pw,W3_dcc_ext[[3]]$pw,W3_dcc_ext[[4]]$pw,W3_dcc_ext[[5]]$pw,W3_dcc_ext[[6]]$pw,W3_dcc_ext[[7]]$pw,W3_dcc_ext[[8]]$pw,W3_dcc_ext[[9]]$pw,W3_dcc_ext[[10]]$pw)
W3_dcc_ext_w <- matrix(W3_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W3_dcc_ext_w

W3_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W3_dcc_ext_w_excel <- c(W3_dcc_ext_w_excel,replicate(m[i],W3_dcc_ext[[i-1]]$pw))
}

W3_dcc_ext_w_excel <- matrix(W3_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W3_dcc_ext_perf <- W3_dcc_ext_cum

W4_dcc_ext_con <- NULL
W4_dcc_ext_ann <- NULL
W4_dcc_ext_cum <- NULL

W4_dcc_ext_con <- c(W4_dcc_ext[[1]]$px,head(W4_dcc_ext[[2]]$px,-1),head(W4_dcc_ext[[3]]$px,-1),head(W4_dcc_ext[[4]]$px,-1),head(W4_dcc_ext[[5]]$px,-1),head(W4_dcc_ext[[6]]$px,-1),head(W4_dcc_ext[[7]]$px,-1),head(W4_dcc_ext[[8]]$px,-1),head(W4_dcc_ext[[9]]$px,-1),head(W4_dcc_ext[[10]]$px,-1))

W4_dcc_ext_cum_sca <- Return.cumulative(W4_dcc_ext_con,geometric = TRUE)
W4_dcc_ext_cum_sca

len <- length(W4_dcc_ext_con)

for (i in 1:(len)) {
  W4_dcc_ext_cum[i] = Return.cumulative(W4_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W4_dcc_ext_ann[i] = Return.annualized(W4_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W4_dcc_ext_ann_sca = Return.annualized(W4_dcc_ext_con, scale = 252,geometric = TRUE)
W4_dcc_ext_ann_sca

ts.plot(as.ts(W4_dcc_ext_cum))

W4_dcc_ext_w <- NULL
W4_dcc_ext_w <- c(W4_dcc_ext[[1]]$pw,W4_dcc_ext[[2]]$pw,W4_dcc_ext[[3]]$pw,W4_dcc_ext[[4]]$pw,W4_dcc_ext[[5]]$pw,W4_dcc_ext[[6]]$pw,W4_dcc_ext[[7]]$pw,W4_dcc_ext[[8]]$pw,W4_dcc_ext[[9]]$pw,W4_dcc_ext[[10]]$pw)
W4_dcc_ext_w <- matrix(W4_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W4_dcc_ext_w

W4_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W4_dcc_ext_w_excel <- c(W4_dcc_ext_w_excel,replicate(m[i],W4_dcc_ext[[i-1]]$pw))
}

W4_dcc_ext_w_excel <- matrix(W4_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W4_dcc_ext_perf <- W4_dcc_ext_cum

W5_dcc_ext_con <- NULL
W5_dcc_ext_ann <- NULL
W5_dcc_ext_cum <- NULL

W5_dcc_ext_con <- c(W5_dcc_ext[[1]]$px,head(W5_dcc_ext[[2]]$px,-1),head(W5_dcc_ext[[3]]$px,-1),head(W5_dcc_ext[[4]]$px,-1),head(W5_dcc_ext[[5]]$px,-1),head(W5_dcc_ext[[6]]$px,-1),head(W5_dcc_ext[[7]]$px,-1),head(W5_dcc_ext[[8]]$px,-1),head(W5_dcc_ext[[9]]$px,-1),head(W5_dcc_ext[[10]]$px,-1))

W5_dcc_ext_cum_sca <- Return.cumulative(W5_dcc_ext_con,geometric = TRUE)
W5_dcc_ext_cum_sca

len <- length(W5_dcc_ext_con)

for (i in 1:(len)) {
  W5_dcc_ext_cum[i] = Return.cumulative(W5_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W5_dcc_ext_ann[i] = Return.annualized(W5_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W5_dcc_ext_ann_sca = Return.annualized(W5_dcc_ext_con, scale = 252,geometric = TRUE)
W5_dcc_ext_ann_sca

ts.plot(as.ts(W5_dcc_ext_cum))

W5_dcc_ext_w <- NULL
W5_dcc_ext_w <- c(W5_dcc_ext[[1]]$pw,W5_dcc_ext[[2]]$pw,W5_dcc_ext[[3]]$pw,W5_dcc_ext[[4]]$pw,W5_dcc_ext[[5]]$pw,W5_dcc_ext[[6]]$pw,W5_dcc_ext[[7]]$pw,W5_dcc_ext[[8]]$pw,W5_dcc_ext[[9]]$pw,W5_dcc_ext[[10]]$pw)
W5_dcc_ext_w <- matrix(W5_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W5_dcc_ext_w

W5_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W5_dcc_ext_w_excel <- c(W5_dcc_ext_w_excel,replicate(m[i],W5_dcc_ext[[i-1]]$pw))
}

W5_dcc_ext_w_excel <- matrix(W5_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W5_dcc_ext_perf <- W5_dcc_ext_cum

W6_dcc_ext_con <- NULL
W6_dcc_ext_ann <- NULL
W6_dcc_ext_cum <- NULL

W6_dcc_ext_con <- c(W6_dcc_ext[[1]]$px,head(W6_dcc_ext[[2]]$px,-1),head(W6_dcc_ext[[3]]$px,-1),head(W6_dcc_ext[[4]]$px,-1),head(W6_dcc_ext[[5]]$px,-1),head(W6_dcc_ext[[6]]$px,-1),head(W6_dcc_ext[[7]]$px,-1),head(W6_dcc_ext[[8]]$px,-1),head(W6_dcc_ext[[9]]$px,-1),head(W6_dcc_ext[[10]]$px,-1))

W6_dcc_ext_cum_sca <- Return.cumulative(W6_dcc_ext_con,geometric = TRUE)
W6_dcc_ext_cum_sca

len <- length(W6_dcc_ext_con)

for (i in 1:(len)) {
  W6_dcc_ext_cum[i] = Return.cumulative(W6_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W6_dcc_ext_ann[i] = Return.annualized(W6_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W6_dcc_ext_ann_sca = Return.annualized(W6_dcc_ext_con, scale = 252,geometric = TRUE)
W6_dcc_ext_ann_sca

ts.plot(as.ts(W6_dcc_ext_cum))

W6_dcc_ext_w <- NULL
W6_dcc_ext_w <- c(W6_dcc_ext[[1]]$pw,W6_dcc_ext[[2]]$pw,W6_dcc_ext[[3]]$pw,W6_dcc_ext[[4]]$pw,W6_dcc_ext[[5]]$pw,W6_dcc_ext[[6]]$pw,W6_dcc_ext[[7]]$pw,W6_dcc_ext[[8]]$pw,W6_dcc_ext[[9]]$pw,W6_dcc_ext[[10]]$pw)
W6_dcc_ext_w <- matrix(W6_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W6_dcc_ext_w

W6_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W6_dcc_ext_w_excel <- c(W6_dcc_ext_w_excel,replicate(m[i],W6_dcc_ext[[i-1]]$pw))
}

W6_dcc_ext_w_excel <- matrix(W6_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W6_dcc_ext_perf <- W6_dcc_ext_cum

W7_dcc_ext_con <- NULL
W7_dcc_ext_ann <- NULL
W7_dcc_ext_cum <- NULL

W7_dcc_ext_con <- c(W7_dcc_ext[[1]]$px,head(W7_dcc_ext[[2]]$px,-1),head(W7_dcc_ext[[3]]$px,-1),head(W7_dcc_ext[[4]]$px,-1),head(W7_dcc_ext[[5]]$px,-1),head(W7_dcc_ext[[6]]$px,-1),head(W7_dcc_ext[[7]]$px,-1),head(W7_dcc_ext[[8]]$px,-1),head(W7_dcc_ext[[9]]$px,-1),head(W7_dcc_ext[[10]]$px,-1))

W7_dcc_ext_cum_sca <- Return.cumulative(W7_dcc_ext_con,geometric = TRUE)
W7_dcc_ext_cum_sca

len <- length(W7_dcc_ext_con)

for (i in 1:(len)) {
  W7_dcc_ext_cum[i] = Return.cumulative(W7_dcc_ext_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W7_dcc_ext_ann[i] = Return.annualized(W7_dcc_ext_con[1:i], scale = 252,geometric = TRUE)
}

W7_dcc_ext_ann_sca = Return.annualized(W7_dcc_ext_con, scale = 252,geometric = TRUE)
W7_dcc_ext_ann_sca

ts.plot(as.ts(W7_dcc_ext_cum))

W7_dcc_ext_w <- NULL
W7_dcc_ext_w <- c(W7_dcc_ext[[1]]$pw,W7_dcc_ext[[2]]$pw,W7_dcc_ext[[3]]$pw,W7_dcc_ext[[4]]$pw,W7_dcc_ext[[5]]$pw,W7_dcc_ext[[6]]$pw,W7_dcc_ext[[7]]$pw,W7_dcc_ext[[8]]$pw,W7_dcc_ext[[9]]$pw,W7_dcc_ext[[10]]$pw)
W7_dcc_ext_w <- matrix(W7_dcc_ext_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W7_dcc_ext_w

W7_dcc_ext_w_excel <- NULL

for (i in 2:(length(m))) {
  W7_dcc_ext_w_excel <- c(W7_dcc_ext_w_excel,replicate(m[i],W7_dcc_ext[[i-1]]$pw))
}

W7_dcc_ext_w_excel <- matrix(W7_dcc_ext_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W7_dcc_ext_perf <- W7_dcc_ext_cum

# rolling window Wavelet portfolio

er_W1_dcc_roll <- NULL
er_W2_dcc_roll <- NULL
er_W3_dcc_roll <- NULL
er_W4_dcc_roll <- NULL
er_W5_dcc_roll <- NULL
er_W6_dcc_roll <- NULL
er_W7_dcc_roll <- NULL

for (i in 1:(len_sa-2)) {
  er_W1_dcc_roll[[i]] <- colMeans(W1.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W2_dcc_roll[[i]] <- colMeans(W2.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W3_dcc_roll[[i]] <- colMeans(W3.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W4_dcc_roll[[i]] <- colMeans(W4.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W5_dcc_roll[[i]] <- colMeans(W5.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W6_dcc_roll[[i]] <- colMeans(W6.dcc[[i]][t_sa[i]:t_sa[i+1],])
  er_W7_dcc_roll[[i]] <- colMeans(W7.dcc[[i]][t_sa[i]:t_sa[i+1],])
}

rvec_W1_dcc_roll <- NULL
rvec_W2_dcc_roll <- NULL
rvec_W3_dcc_roll <- NULL
rvec_W4_dcc_roll <- NULL
rvec_W5_dcc_roll <- NULL
rvec_W6_dcc_roll <- NULL
rvec_W7_dcc_roll <- NULL

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
  rvec_W1_dcc_roll[[i]] <- matrix(rep(er_W1_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W2_dcc_roll[[i]] <- matrix(rep(er_W2_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W3_dcc_roll[[i]] <- matrix(rep(er_W3_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W4_dcc_roll[[i]] <- matrix(rep(er_W4_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W5_dcc_roll[[i]] <- matrix(rep(er_W5_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W6_dcc_roll[[i]] <- matrix(rep(er_W6_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
  rvec_W7_dcc_roll[[i]] <- matrix(rep(er_W7_dcc_roll[[i]],each=t_sa[i+1]),nrow=t_sa[i+1])
}

for (i in 1:(len_sa-2)) {
  rvec_W1_dcc_roll[[i]] <- xts(x = rvec_W1_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W1_dcc_roll[[i]])/3)]))
  rvec_W2_dcc_roll[[i]] <- xts(x = rvec_W2_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W2_dcc_roll[[i]])/3)]))
  rvec_W3_dcc_roll[[i]] <- xts(x = rvec_W3_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W3_dcc_roll[[i]])/3)]))
  rvec_W4_dcc_roll[[i]] <- xts(x = rvec_W4_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W4_dcc_roll[[i]])/3)]))
  rvec_W5_dcc_roll[[i]] <- xts(x = rvec_W5_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W5_dcc_roll[[i]])/3)]))
  rvec_W6_dcc_roll[[i]] <- xts(x = rvec_W6_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W6_dcc_roll[[i]])/3)]))
  rvec_W7_dcc_roll[[i]] <- xts(x = rvec_W7_dcc_roll[[i]],order.by = as.Date(date[1:(length(rvec_W7_dcc_roll[[i]])/3)]))
}

W1_dcc_roll <- NULL
W2_dcc_roll <- NULL
W3_dcc_roll <- NULL
W4_dcc_roll <- NULL
W5_dcc_roll <- NULL
W6_dcc_roll <- NULL
W7_dcc_roll <- NULL

for (i in 1:(len_sa-2)) {
  W1_dcc_roll[[i]] <- portfolio.optim(rvec_W1_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W1_dcc_roll[[i]])
  W2_dcc_roll[[i]] <- portfolio.optim(rvec_W2_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W2_dcc_roll[[i]])
  W3_dcc_roll[[i]] <- portfolio.optim(rvec_W3_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W3_dcc_roll[[i]])
  W4_dcc_roll[[i]] <- portfolio.optim(rvec_W4_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W4_dcc_roll[[i]])
  W5_dcc_roll[[i]] <- portfolio.optim(rvec_W5_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W5_dcc_roll[[i]])
  W6_dcc_roll[[i]] <- portfolio.optim(rvec_W6_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W6_dcc_roll[[i]])
  W7_dcc_roll[[i]] <- portfolio.optim(rvec_W7_dcc_roll[[i]],riskless=riskless_ind,shorts=FALSE,rf = rf[t_sa[i]],reshigh = w_max,covmat=covmat_W7_dcc_roll[[i]])
}

for (i in 1:(len_sa-2)) {
  W1_dcc_roll[[i]]$px <- c(tcrossprod(W1_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W2_dcc_roll[[i]]$px <- c(tcrossprod(W2_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W3_dcc_roll[[i]]$px <- c(tcrossprod(W3_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W4_dcc_roll[[i]]$px <- c(tcrossprod(W4_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W5_dcc_roll[[i]]$px <- c(tcrossprod(W5_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W6_dcc_roll[[i]]$px <- c(tcrossprod(W6_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
  W7_dcc_roll[[i]]$px <- c(tcrossprod(W7_dcc_roll[[i]]$pw, ret[t_sa[i+1]:t_sa[i+2]]))
}

W1_dcc_roll_con <- NULL
W1_dcc_roll_ann <- NULL
W1_dcc_roll_cum <- NULL

W1_dcc_roll_con <- c(W1_dcc_roll[[1]]$px,head(W1_dcc_roll[[2]]$px,-1),head(W1_dcc_roll[[3]]$px,-1),head(W1_dcc_roll[[4]]$px,-1),head(W1_dcc_roll[[5]]$px,-1),head(W1_dcc_roll[[6]]$px,-1),head(W1_dcc_roll[[7]]$px,-1),head(W1_dcc_roll[[8]]$px,-1),head(W1_dcc_roll[[9]]$px,-1),head(W1_dcc_roll[[10]]$px,-1))

W1_dcc_roll_cum_sca <- Return.cumulative(W1_dcc_roll_con,geometric = TRUE)
W1_dcc_roll_cum_sca

len <- length(W1_dcc_roll_con)

for (i in 1:(len)) {
  W1_dcc_roll_cum[i] = Return.cumulative(W1_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W1_dcc_roll_ann[i] = Return.annualized(W1_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W1_dcc_roll_ann_sca = Return.annualized(W1_dcc_roll_con, scale = 252,geometric = TRUE)
W1_dcc_roll_ann_sca

ts.plot(as.ts(W1_dcc_roll_cum),as.ts(dcc_cum))

W1_dcc_roll_w <- NULL
W1_dcc_roll_w <- c(W1_dcc_roll[[1]]$pw,W1_dcc_roll[[2]]$pw,W1_dcc_roll[[3]]$pw,W1_dcc_roll[[4]]$pw,W1_dcc_roll[[5]]$pw,W1_dcc_roll[[6]]$pw,W1_dcc_roll[[7]]$pw,W1_dcc_roll[[8]]$pw,W1_dcc_roll[[9]]$pw,W1_dcc_roll[[10]]$pw)
W1_dcc_roll_w <- matrix(W1_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W1_dcc_roll_w

W1_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W1_dcc_roll_w_excel <- c(W1_dcc_roll_w_excel,replicate(m[i],W1_dcc_roll[[i-1]]$pw))
}

W1_dcc_roll_w_excel <- matrix(W1_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W1_dcc_roll_perf <- W1_dcc_roll_cum

W2_dcc_roll_con <- NULL
W2_dcc_roll_ann <- NULL
W2_dcc_roll_cum <- NULL

W2_dcc_roll_con <- c(W2_dcc_roll[[1]]$px,head(W2_dcc_roll[[2]]$px,-1),head(W2_dcc_roll[[3]]$px,-1),head(W2_dcc_roll[[4]]$px,-1),head(W2_dcc_roll[[5]]$px,-1),head(W2_dcc_roll[[6]]$px,-1),head(W2_dcc_roll[[7]]$px,-1),head(W2_dcc_roll[[8]]$px,-1),head(W2_dcc_roll[[9]]$px,-1),head(W2_dcc_roll[[10]]$px,-1))

W2_dcc_roll_cum_sca <- Return.cumulative(W2_dcc_roll_con,geometric = TRUE)
W2_dcc_roll_cum_sca

len <- length(W2_dcc_roll_con)

for (i in 1:(len)) {
  W2_dcc_roll_cum[i] = Return.cumulative(W2_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W2_dcc_roll_ann[i] = Return.annualized(W2_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W2_dcc_roll_ann_sca = Return.annualized(W2_dcc_roll_con, scale = 252,geometric = TRUE)
W2_dcc_roll_ann_sca

ts.plot(as.ts(W2_dcc_roll_cum))

W2_dcc_roll_w <- NULL
W2_dcc_roll_w <- c(W2_dcc_roll[[1]]$pw,W2_dcc_roll[[2]]$pw,W2_dcc_roll[[3]]$pw,W2_dcc_roll[[4]]$pw,W2_dcc_roll[[5]]$pw,W2_dcc_roll[[6]]$pw,W2_dcc_roll[[7]]$pw,W2_dcc_roll[[8]]$pw,W2_dcc_roll[[9]]$pw,W2_dcc_roll[[10]]$pw)
W2_dcc_roll_w <- matrix(W2_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W2_dcc_roll_w

W2_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W2_dcc_roll_w_excel <- c(W2_dcc_roll_w_excel,replicate(m[i],W2_dcc_roll[[i-1]]$pw))
}

W2_dcc_roll_w_excel <- matrix(W2_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W2_dcc_roll_perf <- W2_dcc_roll_cum

W3_dcc_roll_con <- NULL
W3_dcc_roll_ann <- NULL
W3_dcc_roll_cum <- NULL

W3_dcc_roll_con <- c(W3_dcc_roll[[1]]$px,head(W3_dcc_roll[[2]]$px,-1),head(W3_dcc_roll[[3]]$px,-1),head(W3_dcc_roll[[4]]$px,-1),head(W3_dcc_roll[[5]]$px,-1),head(W3_dcc_roll[[6]]$px,-1),head(W3_dcc_roll[[7]]$px,-1),head(W3_dcc_roll[[8]]$px,-1),head(W3_dcc_roll[[9]]$px,-1),head(W3_dcc_roll[[10]]$px,-1))

W3_dcc_roll_cum_sca <- Return.cumulative(W3_dcc_roll_con,geometric = TRUE)
W3_dcc_roll_cum_sca

len <- length(W3_dcc_roll_con)

for (i in 1:(len)) {
  W3_dcc_roll_cum[i] = Return.cumulative(W3_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W3_dcc_roll_ann[i] = Return.annualized(W3_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W3_dcc_roll_ann_sca = Return.annualized(W3_dcc_roll_con, scale = 252,geometric = TRUE)
W3_dcc_roll_ann_sca

ts.plot(as.ts(W3_dcc_roll_cum))

W3_dcc_roll_w <- NULL
W3_dcc_roll_w <- c(W3_dcc_roll[[1]]$pw,W3_dcc_roll[[2]]$pw,W3_dcc_roll[[3]]$pw,W3_dcc_roll[[4]]$pw,W3_dcc_roll[[5]]$pw,W3_dcc_roll[[6]]$pw,W3_dcc_roll[[7]]$pw,W3_dcc_roll[[8]]$pw,W3_dcc_roll[[9]]$pw,W3_dcc_roll[[10]]$pw)
W3_dcc_roll_w <- matrix(W3_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W3_dcc_roll_w

W3_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W3_dcc_roll_w_excel <- c(W3_dcc_roll_w_excel,replicate(m[i],W3_dcc_roll[[i-1]]$pw))
}

W3_dcc_roll_w_excel <- matrix(W3_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W3_dcc_roll_perf <- W3_dcc_roll_cum

W4_dcc_roll_con <- NULL
W4_dcc_roll_ann <- NULL
W4_dcc_roll_cum <- NULL

W4_dcc_roll_con <- c(W4_dcc_roll[[1]]$px,head(W4_dcc_roll[[2]]$px,-1),head(W4_dcc_roll[[3]]$px,-1),head(W4_dcc_roll[[4]]$px,-1),head(W4_dcc_roll[[5]]$px,-1),head(W4_dcc_roll[[6]]$px,-1),head(W4_dcc_roll[[7]]$px,-1),head(W4_dcc_roll[[8]]$px,-1),head(W4_dcc_roll[[9]]$px,-1),head(W4_dcc_roll[[10]]$px,-1))

W4_dcc_roll_cum_sca <- Return.cumulative(W4_dcc_roll_con,geometric = TRUE)
W4_dcc_roll_cum_sca

len <- length(W4_dcc_roll_con)

for (i in 1:(len)) {
  W4_dcc_roll_cum[i] = Return.cumulative(W4_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W4_dcc_roll_ann[i] = Return.annualized(W4_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W4_dcc_roll_ann_sca = Return.annualized(W4_dcc_roll_con, scale = 252,geometric = TRUE)
W4_dcc_roll_ann_sca

ts.plot(as.ts(W4_dcc_roll_cum))

W4_dcc_roll_w <- NULL
W4_dcc_roll_w <- c(W4_dcc_roll[[1]]$pw,W4_dcc_roll[[2]]$pw,W4_dcc_roll[[3]]$pw,W4_dcc_roll[[4]]$pw,W4_dcc_roll[[5]]$pw,W4_dcc_roll[[6]]$pw,W4_dcc_roll[[7]]$pw,W4_dcc_roll[[8]]$pw,W4_dcc_roll[[9]]$pw,W4_dcc_roll[[10]]$pw)
W4_dcc_roll_w <- matrix(W4_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W4_dcc_roll_w

W4_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W4_dcc_roll_w_excel <- c(W4_dcc_roll_w_excel,replicate(m[i],W4_dcc_roll[[i-1]]$pw))
}

W4_dcc_roll_w_excel <- matrix(W4_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W4_dcc_roll_perf <- W4_dcc_roll_cum

W5_dcc_roll_con <- NULL
W5_dcc_roll_ann <- NULL
W5_dcc_roll_cum <- NULL

W5_dcc_roll_con <- c(W5_dcc_roll[[1]]$px,head(W5_dcc_roll[[2]]$px,-1),head(W5_dcc_roll[[3]]$px,-1),head(W5_dcc_roll[[4]]$px,-1),head(W5_dcc_roll[[5]]$px,-1),head(W5_dcc_roll[[6]]$px,-1),head(W5_dcc_roll[[7]]$px,-1),head(W5_dcc_roll[[8]]$px,-1),head(W5_dcc_roll[[9]]$px,-1),head(W5_dcc_roll[[10]]$px,-1))

W5_dcc_roll_cum_sca <- Return.cumulative(W5_dcc_roll_con,geometric = TRUE)
W5_dcc_roll_cum_sca

len <- length(W5_dcc_roll_con)

for (i in 1:(len)) {
  W5_dcc_roll_cum[i] = Return.cumulative(W5_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W5_dcc_roll_ann[i] = Return.annualized(W5_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W5_dcc_roll_ann_sca = Return.annualized(W5_dcc_roll_con, scale = 252,geometric = TRUE)
W5_dcc_roll_ann_sca

ts.plot(as.ts(W5_dcc_roll_cum))

W5_dcc_roll_w <- NULL
W5_dcc_roll_w <- c(W5_dcc_roll[[1]]$pw,W5_dcc_roll[[2]]$pw,W5_dcc_roll[[3]]$pw,W5_dcc_roll[[4]]$pw,W5_dcc_roll[[5]]$pw,W5_dcc_roll[[6]]$pw,W5_dcc_roll[[7]]$pw,W5_dcc_roll[[8]]$pw,W5_dcc_roll[[9]]$pw,W5_dcc_roll[[10]]$pw)
W5_dcc_roll_w <- matrix(W5_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W5_dcc_roll_w

W5_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W5_dcc_roll_w_excel <- c(W5_dcc_roll_w_excel,replicate(m[i],W5_dcc_roll[[i-1]]$pw))
}

W5_dcc_roll_w_excel <- matrix(W5_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W5_dcc_roll_perf <- W5_dcc_roll_cum

W6_dcc_roll_con <- NULL
W6_dcc_roll_ann <- NULL
W6_dcc_roll_cum <- NULL

W6_dcc_roll_con <- c(W6_dcc_roll[[1]]$px,head(W6_dcc_roll[[2]]$px,-1),head(W6_dcc_roll[[3]]$px,-1),head(W6_dcc_roll[[4]]$px,-1),head(W6_dcc_roll[[5]]$px,-1),head(W6_dcc_roll[[6]]$px,-1),head(W6_dcc_roll[[7]]$px,-1),head(W6_dcc_roll[[8]]$px,-1),head(W6_dcc_roll[[9]]$px,-1),head(W6_dcc_roll[[10]]$px,-1))

W6_dcc_roll_cum_sca <- Return.cumulative(W6_dcc_roll_con,geometric = TRUE)
W6_dcc_roll_cum_sca

len <- length(W6_dcc_roll_con)

for (i in 1:(len)) {
  W6_dcc_roll_cum[i] = Return.cumulative(W6_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W6_dcc_roll_ann[i] = Return.annualized(W6_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W6_dcc_roll_ann_sca = Return.annualized(W6_dcc_roll_con, scale = 252,geometric = TRUE)
W6_dcc_roll_ann_sca

ts.plot(as.ts(W6_dcc_roll_cum))

W6_dcc_roll_w <- NULL
W6_dcc_roll_w <- c(W6_dcc_roll[[1]]$pw,W6_dcc_roll[[2]]$pw,W6_dcc_roll[[3]]$pw,W6_dcc_roll[[4]]$pw,W6_dcc_roll[[5]]$pw,W6_dcc_roll[[6]]$pw,W6_dcc_roll[[7]]$pw,W6_dcc_roll[[8]]$pw,W6_dcc_roll[[9]]$pw,W6_dcc_roll[[10]]$pw)
W6_dcc_roll_w <- matrix(W6_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W6_dcc_roll_w

W6_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W6_dcc_roll_w_excel <- c(W6_dcc_roll_w_excel,replicate(m[i],W6_dcc_roll[[i-1]]$pw))
}

W6_dcc_roll_w_excel <- matrix(W6_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W6_dcc_roll_perf <- W6_dcc_roll_cum

W7_dcc_roll_con <- NULL
W7_dcc_roll_ann <- NULL
W7_dcc_roll_cum <- NULL

W7_dcc_roll_con <- c(W7_dcc_roll[[1]]$px,head(W7_dcc_roll[[2]]$px,-1),head(W7_dcc_roll[[3]]$px,-1),head(W7_dcc_roll[[4]]$px,-1),head(W7_dcc_roll[[5]]$px,-1),head(W7_dcc_roll[[6]]$px,-1),head(W7_dcc_roll[[7]]$px,-1),head(W7_dcc_roll[[8]]$px,-1),head(W7_dcc_roll[[9]]$px,-1),head(W7_dcc_roll[[10]]$px,-1))

W7_dcc_roll_cum_sca <- Return.cumulative(W7_dcc_roll_con,geometric = TRUE)
W7_dcc_roll_cum_sca

len <- length(W7_dcc_roll_con)

for (i in 1:(len)) {
  W7_dcc_roll_cum[i] = Return.cumulative(W7_dcc_roll_con[1:i],geometric = TRUE)
}

for (i in 1:(len)) {
  W7_dcc_roll_ann[i] = Return.annualized(W7_dcc_roll_con[1:i], scale = 252,geometric = TRUE)
}

W7_dcc_roll_ann_sca = Return.annualized(W7_dcc_roll_con, scale = 252,geometric = TRUE)
W7_dcc_roll_ann_sca

ts.plot(as.ts(W7_dcc_roll_cum))

W7_dcc_roll_w <- NULL
W7_dcc_roll_w <- c(W7_dcc_roll[[1]]$pw,W7_dcc_roll[[2]]$pw,W7_dcc_roll[[3]]$pw,W7_dcc_roll[[4]]$pw,W7_dcc_roll[[5]]$pw,W7_dcc_roll[[6]]$pw,W7_dcc_roll[[7]]$pw,W7_dcc_roll[[8]]$pw,W7_dcc_roll[[9]]$pw,W7_dcc_roll[[10]]$pw)
W7_dcc_roll_w <- matrix(W7_dcc_roll_w, nrow = length(m)-1, ncol = 3, byrow = TRUE)
W7_dcc_roll_w

W7_dcc_roll_w_excel <- NULL

for (i in 2:(length(m))) {
  W7_dcc_roll_w_excel <- c(W7_dcc_roll_w_excel,replicate(m[i],W7_dcc_roll[[i-1]]$pw))
}

W7_dcc_roll_w_excel <- matrix(W7_dcc_roll_w_excel, nrow = sum(m[-1]), ncol = 3, byrow = TRUE)

W7_dcc_roll_perf <- W7_dcc_roll_cum

W1_dcc_ext_sd <- StdDev.annualized(W1_dcc_ext_con, scale = 252,geometric = TRUE)
W1_dcc_ext_sk <- skewness(W1_dcc_ext_con, method = "sample")
W1_dcc_ext_ku <- kurtosis(W1_dcc_ext_con, method = "sample")
W1_dcc_ext_CVaR <- CVaR(W1_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W1_dcc_ext_GtP <- sum(W1_dcc_ext_con[W1_dcc_ext_con>0])/abs(sum(W1_dcc_ext_con[W1_dcc_ext_con<0]))
W1_dcc_ext_PR <- PainRatio(as.xts(x = W1_dcc_ext_con,order.by = date[-(1:127)]))[1]
W1_dcc_ext_ShR <- SharpeRatio(as.xts(x = W1_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W1_dcc_ext_SoR <- SortinoRatio(W1_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W1_dcc_ext_SemiD <- SemiDeviation(W1_dcc_ext_con)[1]*sqrt(252)
W1_dcc_ext_om <- Omega(W1_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W1_dcc_ext_ka <- Kappa(W1_dcc_ext_con,MAR = 0,l=3)
W1_dcc_ext_MD <- maxDrawdown(as.xts(x = W1_dcc_ext_con,order.by = date[-(1:127)]))*-1
W1_dcc_ext_awchg <- mean(abs(diff(W1_dcc_ext_w)))
W1_dcc_ext_alev <- mean((1-rowSums(W1_dcc_ext_w))[(1-rowSums(W1_dcc_ext_w))<0])*-1
W1_dcc_ext_maxlev <- max((1-rowSums(W1_dcc_ext_w))*-1)
W1_dcc_ext_minlev <- min((1-rowSums(W1_dcc_ext_w))*-1)

W2_dcc_ext_sd <- StdDev.annualized(W2_dcc_ext_con, scale = 252,geometric = TRUE)
W2_dcc_ext_sk <- skewness(W2_dcc_ext_con, method = "sample")
W2_dcc_ext_ku <- kurtosis(W2_dcc_ext_con, method = "sample")
W2_dcc_ext_CVaR <- CVaR(W2_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W2_dcc_ext_GtP <- sum(W2_dcc_ext_con[W2_dcc_ext_con>0])/abs(sum(W2_dcc_ext_con[W2_dcc_ext_con<0]))
W2_dcc_ext_PR <- PainRatio(as.xts(x = W2_dcc_ext_con,order.by = date[-(1:127)]))[1]
W2_dcc_ext_ShR <- SharpeRatio(as.xts(x = W2_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W2_dcc_ext_SoR <- SortinoRatio(W2_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W2_dcc_ext_SemiD <- SemiDeviation(W2_dcc_ext_con)[1]*sqrt(252)
W2_dcc_ext_om <- Omega(W2_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W2_dcc_ext_ka <- Kappa(W2_dcc_ext_con,MAR = 0,l=3)
W2_dcc_ext_MD <- maxDrawdown(as.xts(x = W2_dcc_ext_con,order.by = date[-(1:127)]))*-1
W2_dcc_ext_awchg <- mean(abs(diff(W2_dcc_ext_w)))
W2_dcc_ext_alev <- mean((1-rowSums(W2_dcc_ext_w))[(1-rowSums(W2_dcc_ext_w))<0])*-1
W2_dcc_ext_maxlev <- max((1-rowSums(W2_dcc_ext_w))*-1)
W2_dcc_ext_minlev <- min((1-rowSums(W2_dcc_ext_w))*-1)

W3_dcc_ext_sd <- StdDev.annualized(W3_dcc_ext_con, scale = 252,geometric = TRUE)
W3_dcc_ext_sk <- skewness(W3_dcc_ext_con, method = "sample")
W3_dcc_ext_ku <- kurtosis(W3_dcc_ext_con, method = "sample")
W3_dcc_ext_CVaR <- CVaR(W3_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W3_dcc_ext_GtP <- sum(W3_dcc_ext_con[W3_dcc_ext_con>0])/abs(sum(W3_dcc_ext_con[W3_dcc_ext_con<0]))
W3_dcc_ext_PR <- PainRatio(as.xts(x = W3_dcc_ext_con,order.by = date[-(1:127)]))[1]
W3_dcc_ext_ShR <- SharpeRatio(as.xts(x = W3_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W3_dcc_ext_SoR <- SortinoRatio(W3_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W3_dcc_ext_SemiD <- SemiDeviation(W3_dcc_ext_con)[1]*sqrt(252)
W3_dcc_ext_om <- Omega(W3_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W3_dcc_ext_ka <- Kappa(W3_dcc_ext_con,MAR = 0,l=3)
W3_dcc_ext_MD <- maxDrawdown(as.xts(x = W3_dcc_ext_con,order.by = date[-(1:127)]))*-1
W3_dcc_ext_awchg <- mean(abs(diff(W3_dcc_ext_w)))
W3_dcc_ext_alev <- mean((1-rowSums(W3_dcc_ext_w))[(1-rowSums(W3_dcc_ext_w))<0])*-1
W3_dcc_ext_maxlev <- max((1-rowSums(W3_dcc_ext_w))*-1)
W3_dcc_ext_minlev <- min((1-rowSums(W3_dcc_ext_w))*-1)

W4_dcc_ext_sd <- StdDev.annualized(W4_dcc_ext_con, scale = 252,geometric = TRUE)
W4_dcc_ext_sk <- skewness(W4_dcc_ext_con, method = "sample")
W4_dcc_ext_ku <- kurtosis(W4_dcc_ext_con, method = "sample")
W4_dcc_ext_CVaR <- CVaR(W4_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W4_dcc_ext_GtP <- sum(W4_dcc_ext_con[W4_dcc_ext_con>0])/abs(sum(W4_dcc_ext_con[W4_dcc_ext_con<0]))
W4_dcc_ext_PR <- PainRatio(as.xts(x = W4_dcc_ext_con,order.by = date[-(1:127)]))[1]
W4_dcc_ext_ShR <- SharpeRatio(as.xts(x = W4_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W4_dcc_ext_SoR <- SortinoRatio(W4_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W4_dcc_ext_SemiD <- SemiDeviation(W4_dcc_ext_con)[1]*sqrt(252)
W4_dcc_ext_om <- Omega(W4_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W4_dcc_ext_ka <- Kappa(W4_dcc_ext_con,MAR = 0,l=3)
W4_dcc_ext_MD <- maxDrawdown(as.xts(x = W4_dcc_ext_con,order.by = date[-(1:127)]))*-1
W4_dcc_ext_awchg <- mean(abs(diff(W4_dcc_ext_w)))
W4_dcc_ext_alev <- mean((1-rowSums(W4_dcc_ext_w))[(1-rowSums(W4_dcc_ext_w))<0])*-1
W4_dcc_ext_maxlev <- max((1-rowSums(W4_dcc_ext_w))*-1)
W4_dcc_ext_minlev <- min((1-rowSums(W4_dcc_ext_w))*-1)

W5_dcc_ext_sd <- StdDev.annualized(W5_dcc_ext_con, scale = 252,geometric = TRUE)
W5_dcc_ext_sk <- skewness(W5_dcc_ext_con, method = "sample")
W5_dcc_ext_ku <- kurtosis(W5_dcc_ext_con, method = "sample")
W5_dcc_ext_CVaR <- CVaR(W5_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W5_dcc_ext_GtP <- sum(W5_dcc_ext_con[W5_dcc_ext_con>0])/abs(sum(W5_dcc_ext_con[W5_dcc_ext_con<0]))
W5_dcc_ext_PR <- PainRatio(as.xts(x = W5_dcc_ext_con,order.by = date[-(1:127)]))[1]
W5_dcc_ext_ShR <- SharpeRatio(as.xts(x = W5_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W5_dcc_ext_SoR <- SortinoRatio(W5_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W5_dcc_ext_SemiD <- SemiDeviation(W5_dcc_ext_con)[1]*sqrt(252)
W5_dcc_ext_om <- Omega(W5_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W5_dcc_ext_ka <- Kappa(W5_dcc_ext_con,MAR = 0,l=3)
W5_dcc_ext_MD <- maxDrawdown(as.xts(x = W5_dcc_ext_con,order.by = date[-(1:127)]))*-1
W5_dcc_ext_awchg <- mean(abs(diff(W5_dcc_ext_w)))
W5_dcc_ext_alev <- mean((1-rowSums(W5_dcc_ext_w))[(1-rowSums(W5_dcc_ext_w))<0])*-1
W5_dcc_ext_maxlev <- max((1-rowSums(W5_dcc_ext_w))*-1)
W5_dcc_ext_minlev <- min((1-rowSums(W5_dcc_ext_w))*-1)

W6_dcc_ext_sd <- StdDev.annualized(W6_dcc_ext_con, scale = 252,geometric = TRUE)
W6_dcc_ext_sk <- skewness(W6_dcc_ext_con, method = "sample")
W6_dcc_ext_ku <- kurtosis(W6_dcc_ext_con, method = "sample")
W6_dcc_ext_CVaR <- CVaR(W6_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W6_dcc_ext_GtP <- sum(W6_dcc_ext_con[W6_dcc_ext_con>0])/abs(sum(W6_dcc_ext_con[W6_dcc_ext_con<0]))
W6_dcc_ext_PR <- PainRatio(as.xts(x = W6_dcc_ext_con,order.by = date[-(1:127)]))[1]
W6_dcc_ext_ShR <- SharpeRatio(as.xts(x = W6_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W6_dcc_ext_SoR <- SortinoRatio(W6_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W6_dcc_ext_SemiD <- SemiDeviation(W6_dcc_ext_con)[1]*sqrt(252)
W6_dcc_ext_om <- Omega(W6_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W6_dcc_ext_ka <- Kappa(W6_dcc_ext_con,MAR = 0,l=3)
W6_dcc_ext_MD <- maxDrawdown(as.xts(x = W6_dcc_ext_con,order.by = date[-(1:127)]))*-1
W6_dcc_ext_awchg <- mean(abs(diff(W6_dcc_ext_w)))
W6_dcc_ext_alev <- mean((1-rowSums(W6_dcc_ext_w))[(1-rowSums(W6_dcc_ext_w))<0])*-1
W6_dcc_ext_maxlev <- max((1-rowSums(W6_dcc_ext_w))*-1)
W6_dcc_ext_minlev <- min((1-rowSums(W6_dcc_ext_w))*-1)

W7_dcc_ext_sd <- StdDev.annualized(W7_dcc_ext_con, scale = 252,geometric = TRUE)
W7_dcc_ext_sk <- skewness(W7_dcc_ext_con, method = "sample")
W7_dcc_ext_ku <- kurtosis(W7_dcc_ext_con, method = "sample")
W7_dcc_ext_CVaR <- CVaR(W7_dcc_ext_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W7_dcc_ext_GtP <- sum(W7_dcc_ext_con[W7_dcc_ext_con>0])/abs(sum(W7_dcc_ext_con[W7_dcc_ext_con<0]))
W7_dcc_ext_PR <- PainRatio(as.xts(x = W7_dcc_ext_con,order.by = date[-(1:127)]))[1]
W7_dcc_ext_ShR <- SharpeRatio(as.xts(x = W7_dcc_ext_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W7_dcc_ext_SoR <- SortinoRatio(W7_dcc_ext_con,MAR = 0)[1]*sqrt(252)
W7_dcc_ext_SemiD <- SemiDeviation(W7_dcc_ext_con)[1]*sqrt(252)
W7_dcc_ext_om <- Omega(W7_dcc_ext_con,L = 0,method = "simple",output = "point")-1
W7_dcc_ext_ka <- Kappa(W7_dcc_ext_con,MAR = 0,l=3)
W7_dcc_ext_MD <- maxDrawdown(as.xts(x = W7_dcc_ext_con,order.by = date[-(1:127)]))*-1
W7_dcc_ext_awchg <- mean(abs(diff(W7_dcc_ext_w)))
W7_dcc_ext_alev <- mean((1-rowSums(W7_dcc_ext_w))[(1-rowSums(W7_dcc_ext_w))<0])*-1
W7_dcc_ext_maxlev <- max((1-rowSums(W7_dcc_ext_w))*-1)
W7_dcc_ext_minlev <- min((1-rowSums(W7_dcc_ext_w))*-1)

W1_dcc_roll_sd <- StdDev.annualized(W1_dcc_roll_con, scale = 252,geometric = TRUE)
W1_dcc_roll_sk <- skewness(W1_dcc_roll_con, method = "sample")
W1_dcc_roll_ku <- kurtosis(W1_dcc_roll_con, method = "sample")
W1_dcc_roll_CVaR <- CVaR(W1_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W1_dcc_roll_GtP <- sum(W1_dcc_roll_con[W1_dcc_roll_con>0])/abs(sum(W1_dcc_roll_con[W1_dcc_roll_con<0]))
W1_dcc_roll_PR <- PainRatio(as.xts(x = W1_dcc_roll_con,order.by = date[-(1:127)]))[1]
W1_dcc_roll_ShR <- SharpeRatio(as.xts(x = W1_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W1_dcc_roll_SoR <- SortinoRatio(W1_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W1_dcc_roll_SemiD <- SemiDeviation(W1_dcc_roll_con)[1]*sqrt(252)
W1_dcc_roll_om <- Omega(W1_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W1_dcc_roll_ka <- Kappa(W1_dcc_roll_con,MAR = 0,l=3)
W1_dcc_roll_MD <- maxDrawdown(as.xts(x = W1_dcc_roll_con,order.by = date[-(1:127)]))*-1
W1_dcc_roll_awchg <- mean(abs(diff(W1_dcc_roll_w)))
W1_dcc_roll_alev <- mean((1-rowSums(W1_dcc_roll_w))[(1-rowSums(W1_dcc_roll_w))<0])*-1
W1_dcc_roll_maxlev <- max((1-rowSums(W1_dcc_roll_w))*-1)
W1_dcc_roll_minlev <- min((1-rowSums(W1_dcc_roll_w))*-1)

W2_dcc_roll_sd <- StdDev.annualized(W2_dcc_roll_con, scale = 252,geometric = TRUE)
W2_dcc_roll_sk <- skewness(W2_dcc_roll_con, method = "sample")
W2_dcc_roll_ku <- kurtosis(W2_dcc_roll_con, method = "sample")
W2_dcc_roll_CVaR <- CVaR(W2_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W2_dcc_roll_GtP <- sum(W2_dcc_roll_con[W2_dcc_roll_con>0])/abs(sum(W2_dcc_roll_con[W2_dcc_roll_con<0]))
W2_dcc_roll_PR <- PainRatio(as.xts(x = W2_dcc_roll_con,order.by = date[-(1:127)]))[1]
W2_dcc_roll_ShR <- SharpeRatio(as.xts(x = W2_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W2_dcc_roll_SoR <- SortinoRatio(W2_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W2_dcc_roll_SemiD <- SemiDeviation(W2_dcc_roll_con)[1]*sqrt(252)
W2_dcc_roll_om <- Omega(W2_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W2_dcc_roll_ka <- Kappa(W2_dcc_roll_con,MAR = 0,l=3)
W2_dcc_roll_MD <- maxDrawdown(as.xts(x = W2_dcc_roll_con,order.by = date[-(1:127)]))*-1
W2_dcc_roll_awchg <- mean(abs(diff(W2_dcc_roll_w)))
W2_dcc_roll_alev <- mean((1-rowSums(W2_dcc_roll_w))[(1-rowSums(W2_dcc_roll_w))<0])*-1
W2_dcc_roll_maxlev <- max((1-rowSums(W2_dcc_roll_w))*-1)
W2_dcc_roll_minlev <- min((1-rowSums(W2_dcc_roll_w))*-1)

W3_dcc_roll_sd <- StdDev.annualized(W3_dcc_roll_con, scale = 252,geometric = TRUE)
W3_dcc_roll_sk <- skewness(W3_dcc_roll_con, method = "sample")
W3_dcc_roll_ku <- kurtosis(W3_dcc_roll_con, method = "sample")
W3_dcc_roll_CVaR <- CVaR(W3_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W3_dcc_roll_GtP <- sum(W3_dcc_roll_con[W3_dcc_roll_con>0])/abs(sum(W3_dcc_roll_con[W3_dcc_roll_con<0]))
W3_dcc_roll_PR <- PainRatio(as.xts(x = W3_dcc_roll_con,order.by = date[-(1:127)]))[1]
W3_dcc_roll_ShR <- SharpeRatio(as.xts(x = W3_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W3_dcc_roll_SoR <- SortinoRatio(W3_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W3_dcc_roll_SemiD <- SemiDeviation(W3_dcc_roll_con)[1]*sqrt(252)
W3_dcc_roll_om <- Omega(W3_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W3_dcc_roll_ka <- Kappa(W3_dcc_roll_con,MAR = 0,l=3)
W3_dcc_roll_MD <- maxDrawdown(as.xts(x = W3_dcc_roll_con,order.by = date[-(1:127)]))*-1
W3_dcc_roll_awchg <- mean(abs(diff(W3_dcc_roll_w)))
W3_dcc_roll_alev <- mean((1-rowSums(W3_dcc_roll_w))[(1-rowSums(W3_dcc_roll_w))<0])*-1
W3_dcc_roll_maxlev <- max((1-rowSums(W3_dcc_roll_w))*-1)
W3_dcc_roll_minlev <- min((1-rowSums(W3_dcc_roll_w))*-1)

W4_dcc_roll_sd <- StdDev.annualized(W4_dcc_roll_con, scale = 252,geometric = TRUE)
W4_dcc_roll_sk <- skewness(W4_dcc_roll_con, method = "sample")
W4_dcc_roll_ku <- kurtosis(W4_dcc_roll_con, method = "sample")
W4_dcc_roll_CVaR <- CVaR(W4_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W4_dcc_roll_GtP <- sum(W4_dcc_roll_con[W4_dcc_roll_con>0])/abs(sum(W4_dcc_roll_con[W4_dcc_roll_con<0]))
W4_dcc_roll_PR <- PainRatio(as.xts(x = W4_dcc_roll_con,order.by = date[-(1:127)]))[1]
W4_dcc_roll_ShR <- SharpeRatio(as.xts(x = W4_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W4_dcc_roll_SoR <- SortinoRatio(W4_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W4_dcc_roll_SemiD <- SemiDeviation(W4_dcc_roll_con)[1]*sqrt(252)
W4_dcc_roll_om <- Omega(W4_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W4_dcc_roll_ka <- Kappa(W4_dcc_roll_con,MAR = 0,l=3)
W4_dcc_roll_MD <- maxDrawdown(as.xts(x = W4_dcc_roll_con,order.by = date[-(1:127)]))*-1
W4_dcc_roll_awchg <- mean(abs(diff(W4_dcc_roll_w)))
W4_dcc_roll_alev <- mean((1-rowSums(W4_dcc_roll_w))[(1-rowSums(W4_dcc_roll_w))<0])*-1
W4_dcc_roll_maxlev <- max((1-rowSums(W4_dcc_roll_w))*-1)
W4_dcc_roll_minlev <- min((1-rowSums(W4_dcc_roll_w))*-1)

W5_dcc_roll_sd <- StdDev.annualized(W5_dcc_roll_con, scale = 252,geometric = TRUE)
W5_dcc_roll_sk <- skewness(W5_dcc_roll_con, method = "sample")
W5_dcc_roll_ku <- kurtosis(W5_dcc_roll_con, method = "sample")
W5_dcc_roll_CVaR <- CVaR(W5_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W5_dcc_roll_GtP <- sum(W5_dcc_roll_con[W5_dcc_roll_con>0])/abs(sum(W5_dcc_roll_con[W5_dcc_roll_con<0]))
W5_dcc_roll_PR <- PainRatio(as.xts(x = W5_dcc_roll_con,order.by = date[-(1:127)]))[1]
W5_dcc_roll_ShR <- SharpeRatio(as.xts(x = W5_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W5_dcc_roll_SoR <- SortinoRatio(W5_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W5_dcc_roll_SemiD <- SemiDeviation(W5_dcc_roll_con)[1]*sqrt(252)
W5_dcc_roll_om <- Omega(W5_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W5_dcc_roll_ka <- Kappa(W5_dcc_roll_con,MAR = 0,l=3)
W5_dcc_roll_MD <- maxDrawdown(as.xts(x = W5_dcc_roll_con,order.by = date[-(1:127)]))*-1
W5_dcc_roll_awchg <- mean(abs(diff(W5_dcc_roll_w)))
W5_dcc_roll_alev <- mean((1-rowSums(W5_dcc_roll_w))[(1-rowSums(W5_dcc_roll_w))<0])*-1
W5_dcc_roll_maxlev <- max((1-rowSums(W5_dcc_roll_w))*-1)
W5_dcc_roll_minlev <- min((1-rowSums(W5_dcc_roll_w))*-1)

W6_dcc_roll_sd <- StdDev.annualized(W6_dcc_roll_con, scale = 252,geometric = TRUE)
W6_dcc_roll_sk <- skewness(W6_dcc_roll_con, method = "sample")
W6_dcc_roll_ku <- kurtosis(W6_dcc_roll_con, method = "sample")
W6_dcc_roll_CVaR <- CVaR(W6_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W6_dcc_roll_GtP <- sum(W6_dcc_roll_con[W6_dcc_roll_con>0])/abs(sum(W6_dcc_roll_con[W6_dcc_roll_con<0]))
W6_dcc_roll_PR <- PainRatio(as.xts(x = W6_dcc_roll_con,order.by = date[-(1:127)]))[1]
W6_dcc_roll_ShR <- SharpeRatio(as.xts(x = W6_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W6_dcc_roll_SoR <- SortinoRatio(W6_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W6_dcc_roll_SemiD <- SemiDeviation(W6_dcc_roll_con)[1]*sqrt(252)
W6_dcc_roll_om <- Omega(W6_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W6_dcc_roll_ka <- Kappa(W6_dcc_roll_con,MAR = 0,l=3)
W6_dcc_roll_MD <- maxDrawdown(as.xts(x = W6_dcc_roll_con,order.by = date[-(1:127)]))*-1
W6_dcc_roll_awchg <- mean(abs(diff(W6_dcc_roll_w)))
W6_dcc_roll_alev <- mean((1-rowSums(W6_dcc_roll_w))[(1-rowSums(W6_dcc_roll_w))<0])*-1
W6_dcc_roll_maxlev <- max((1-rowSums(W6_dcc_roll_w))*-1)
W6_dcc_roll_minlev <- min((1-rowSums(W6_dcc_roll_w))*-1)

W7_dcc_roll_sd <- StdDev.annualized(W7_dcc_roll_con, scale = 252,geometric = TRUE)
W7_dcc_roll_sk <- skewness(W7_dcc_roll_con, method = "sample")
W7_dcc_roll_ku <- kurtosis(W7_dcc_roll_con, method = "sample")
W7_dcc_roll_CVaR <- CVaR(W7_dcc_roll_con,p = 0.95,method = "gaussian")[1]*sqrt(252)
W7_dcc_roll_GtP <- sum(W7_dcc_roll_con[W7_dcc_roll_con>0])/abs(sum(W7_dcc_roll_con[W7_dcc_roll_con<0]))
W7_dcc_roll_PR <- PainRatio(as.xts(x = W7_dcc_roll_con,order.by = date[-(1:127)]))[1]
W7_dcc_roll_ShR <- SharpeRatio(as.xts(x = W7_dcc_roll_con,order.by = date[-(1:127)]), scale = 252,geometric = TRUE,annualize = TRUE)[1]
W7_dcc_roll_SoR <- SortinoRatio(W7_dcc_roll_con,MAR = 0)[1]*sqrt(252)
W7_dcc_roll_SemiD <- SemiDeviation(W7_dcc_roll_con)[1]*sqrt(252)
W7_dcc_roll_om <- Omega(W7_dcc_roll_con,L = 0,method = "simple",output = "point")-1
W7_dcc_roll_ka <- Kappa(W7_dcc_roll_con,MAR = 0,l=3)
W7_dcc_roll_MD <- maxDrawdown(as.xts(x = W7_dcc_roll_con,order.by = date[-(1:127)]))*-1
W7_dcc_roll_awchg <- mean(abs(diff(W7_dcc_roll_w)))
W7_dcc_roll_alev <- mean((1-rowSums(W7_dcc_roll_w))[(1-rowSums(W7_dcc_roll_w))<0])*-1
W7_dcc_roll_maxlev <- max((1-rowSums(W7_dcc_roll_w))*-1)
W7_dcc_roll_minlev <- min((1-rowSums(W7_dcc_roll_w))*-1)
