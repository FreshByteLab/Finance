library(quantmod)
library(rugarch)
library(rmgarch)
library(openxlsx)

# Vorbereitung der Zeitreihen und erstellen der Programmparameter

date <- r_import$date
ret <- r_import[,5:7]
ret <- xts(x = ret,order.by = as.Date(date))

len_tot <- length(r_import$date)

len_sa <- (length(endpoints(ret,on="quarters"))+1)/2
t_sa <- endpoints(ret,on="quarters")[c(TRUE,FALSE)]
t_sa[1] <- 1
m <- diff(t_sa)
m[1] <- m[1]+1

# Spezifikation des univariaten Teils des DCC-Modells
gjrtspec = ugarchspec(mean.model=list(armaOrder=c(0,0)),
    variance.model= list(garchOrder= c(2,2), solver.control = list(tol = 1e-12), 
    model = "sGARCH"), distribution.model="norm") 
# Spezifikation des bivariaten Teils des DCC-Modells
dcc_spec = dccspec(uspec = multispec(replicate(3, gjrtspec)), dccOrder= c(2,2), model = c("DCC"),
    distribution = "mvnorm")

# halbjÃ¤hrlich-rollierendes/erweiterndes Zeitfenster der DCC-Korrelation anhand der untransformierten bzw. transformierten Daten

covmat_dcc_ext <- NULL
cormat_dcc_ext <- NULL
covmat_dcc_roll <- NULL
cormat_dcc_roll <- NULL
covmat_W1_dcc_ext <- NULL
cormat_W1_dcc_ext <- NULL
covmat_W1_dcc_roll <- NULL
cormat_W1_dcc_roll <- NULL
covmat_W2_dcc_ext <- NULL
cormat_W2_dcc_ext <- NULL
covmat_W2_dcc_roll <- NULL
cormat_W2_dcc_roll <- NULL
covmat_W3_dcc_ext <- NULL
cormat_W3_dcc_ext <- NULL
covmat_W3_dcc_roll <- NULL
cormat_W3_dcc_roll <- NULL
covmat_W4_dcc_ext <- NULL
cormat_W4_dcc_ext <- NULL
covmat_W4_dcc_roll <- NULL
cormat_W4_dcc_roll <- NULL
covmat_W5_dcc_ext <- NULL
cormat_W5_dcc_ext <- NULL
covmat_W5_dcc_roll <- NULL
cormat_W5_dcc_roll <- NULL
covmat_W6_dcc_ext <- NULL
cormat_W6_dcc_ext <- NULL
covmat_W6_dcc_roll <- NULL
cormat_W6_dcc_roll <- NULL
covmat_W7_dcc_ext <- NULL
cormat_W7_dcc_ext <- NULL
covmat_W7_dcc_roll <- NULL
cormat_W7_dcc_roll <- NULL

for (i in 1:(len_sa-1)) {
  covmat_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,ret[t_sa[1]:t_sa[i+1]]))[,,length(rcov(dccfit(dcc_spec,ret[t_sa[1]:t_sa[i+1]]))[1,1,])]
  cormat_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,ret[t_sa[1]:t_sa[i+1]]))[,,length(rcov(dccfit(dcc_spec,ret[t_sa[1]:t_sa[i+1]]))[1,1,])]
  covmat_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,ret[t_sa[i]:t_sa[i+1]]))[,,length(rcov(dccfit(dcc_spec,ret[t_sa[i]:t_sa[i+1]]))[1,1,])]
  cormat_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,ret[t_sa[i]:t_sa[i+1]]))[,,length(rcov(dccfit(dcc_spec,ret[t_sa[i]:t_sa[i+1]]))[1,1,])]
  covmat_W1_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W1_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W1.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W1_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W1_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W1.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W1.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W2_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W2_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W2.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W2_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W2_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W2.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W2.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W3_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W3_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W3.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W3_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W3_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W3.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W3.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W4_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W4_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W4.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W4_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W4_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W4.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W4.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W5_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W5_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W5.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W5_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W5_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W5.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W5.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W6_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W6_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W6.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W6_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W6_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W6.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W6.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  covmat_W7_dcc_ext[[i]] <- rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  cormat_W7_dcc_ext[[i]] <- rcor(dccfit(dcc_spec,W7.dcc[[i]][t_sa[1]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[1]:t_sa[i+1],]))[1,1,])]
  covmat_W7_dcc_roll[[i]] <- rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
  cormat_W7_dcc_roll[[i]] <- rcor(dccfit(dcc_spec,W7.dcc[[i]][t_sa[i]:t_sa[i+1],]))[,,length(rcov(dccfit(dcc_spec,W7.dcc[[i]][t_sa[i]:t_sa[i+1],]))[1,1,])]
}

dcc_AE_roll <- NULL
dcc_AB_roll <- NULL
dcc_EB_roll <- NULL
dcc_AE_ext <- NULL
dcc_AB_ext <- NULL
dcc_EB_ext <- NULL
W1_dcc_AE_ext <- NULL
W1_dcc_AB_ext <- NULL
W1_dcc_EB_ext <- NULL
W2_dcc_AE_ext <- NULL
W2_dcc_AB_ext <- NULL
W2_dcc_EB_ext <- NULL
W3_dcc_AE_ext <- NULL
W3_dcc_AB_ext <- NULL
W3_dcc_EB_ext <- NULL
W4_dcc_AE_ext <- NULL
W4_dcc_AB_ext <- NULL
W4_dcc_EB_ext <- NULL
W5_dcc_AE_ext <- NULL
W5_dcc_AB_ext <- NULL
W5_dcc_EB_ext <- NULL
W6_dcc_AE_ext <- NULL
W6_dcc_AB_ext <- NULL
W6_dcc_EB_ext <- NULL
W7_dcc_AE_ext <- NULL
W7_dcc_AB_ext <- NULL
W7_dcc_EB_ext <- NULL
W1_dcc_AE_roll <- NULL
W1_dcc_AB_roll <- NULL
W1_dcc_EB_roll <- NULL
W2_dcc_AE_roll <- NULL
W2_dcc_AB_roll <- NULL
W2_dcc_EB_roll <- NULL
W3_dcc_AE_roll <- NULL
W3_dcc_AB_roll <- NULL
W3_dcc_EB_roll <- NULL
W4_dcc_AE_roll <- NULL
W4_dcc_AB_roll <- NULL
W4_dcc_EB_roll <- NULL
W5_dcc_AE_roll <- NULL
W5_dcc_AB_roll <- NULL
W5_dcc_EB_roll <- NULL
W6_dcc_AE_roll <- NULL
W6_dcc_AB_roll <- NULL
W6_dcc_EB_roll <- NULL
W7_dcc_AE_roll <- NULL
W7_dcc_AB_roll <- NULL
W7_dcc_EB_roll <- NULL

for (i in 1:(length(m))) {
  dcc_AE_ext <- c(dcc_AE_ext,replicate(m[i],cormat_dcc_ext[[i]][1,2]))
  dcc_AB_ext <- c(dcc_AB_ext,replicate(m[i],cormat_dcc_ext[[i]][1,3]))
  dcc_EB_ext <- c(dcc_EB_ext,replicate(m[i],cormat_dcc_ext[[i]][2,3]))
  dcc_AE_roll <- c(dcc_AE_roll,replicate(m[i],cormat_dcc_roll[[i]][1,2]))
  dcc_AB_roll <- c(dcc_AB_roll,replicate(m[i],cormat_dcc_roll[[i]][1,3]))
  dcc_EB_roll <- c(dcc_EB_roll,replicate(m[i],cormat_dcc_roll[[i]][2,3]))
  W1_dcc_AE_ext <- c(W1_dcc_AE_ext,replicate(m[i],cormat_W1_dcc_ext[[i]][1,2]))
  W1_dcc_AB_ext <- c(W1_dcc_AB_ext,replicate(m[i],cormat_W1_dcc_ext[[i]][1,3]))
  W1_dcc_EB_ext <- c(W1_dcc_EB_ext,replicate(m[i],cormat_W1_dcc_ext[[i]][2,3]))
  W2_dcc_AE_ext <- c(W2_dcc_AE_ext,replicate(m[i],cormat_W2_dcc_ext[[i]][1,2]))
  W2_dcc_AB_ext <- c(W2_dcc_AB_ext,replicate(m[i],cormat_W2_dcc_ext[[i]][1,3]))
  W2_dcc_EB_ext <- c(W2_dcc_EB_ext,replicate(m[i],cormat_W2_dcc_ext[[i]][2,3]))
  W3_dcc_AE_ext <- c(W3_dcc_AE_ext,replicate(m[i],cormat_W3_dcc_ext[[i]][1,2]))
  W3_dcc_AB_ext <- c(W3_dcc_AB_ext,replicate(m[i],cormat_W3_dcc_ext[[i]][1,3]))
  W3_dcc_EB_ext <- c(W3_dcc_EB_ext,replicate(m[i],cormat_W3_dcc_ext[[i]][2,3]))
  W4_dcc_AE_ext <- c(W4_dcc_AE_ext,replicate(m[i],cormat_W4_dcc_ext[[i]][1,2]))
  W4_dcc_AB_ext <- c(W4_dcc_AB_ext,replicate(m[i],cormat_W4_dcc_ext[[i]][1,3]))
  W4_dcc_EB_ext <- c(W4_dcc_EB_ext,replicate(m[i],cormat_W4_dcc_ext[[i]][2,3]))
  W5_dcc_AE_ext <- c(W5_dcc_AE_ext,replicate(m[i],cormat_W5_dcc_ext[[i]][1,2]))
  W5_dcc_AB_ext <- c(W5_dcc_AB_ext,replicate(m[i],cormat_W5_dcc_ext[[i]][1,3]))
  W5_dcc_EB_ext <- c(W5_dcc_EB_ext,replicate(m[i],cormat_W5_dcc_ext[[i]][2,3]))
  W6_dcc_AE_ext <- c(W6_dcc_AE_ext,replicate(m[i],cormat_W6_dcc_ext[[i]][1,2]))
  W6_dcc_AB_ext <- c(W6_dcc_AB_ext,replicate(m[i],cormat_W6_dcc_ext[[i]][1,3]))
  W6_dcc_EB_ext <- c(W6_dcc_EB_ext,replicate(m[i],cormat_W6_dcc_ext[[i]][2,3]))
  W7_dcc_AE_ext <- c(W7_dcc_AE_ext,replicate(m[i],cormat_W7_dcc_ext[[i]][1,2]))
  W7_dcc_AB_ext <- c(W7_dcc_AB_ext,replicate(m[i],cormat_W7_dcc_ext[[i]][1,3]))
  W7_dcc_EB_ext <- c(W7_dcc_EB_ext,replicate(m[i],cormat_W7_dcc_ext[[i]][2,3]))
  W1_dcc_AE_roll <- c(W1_dcc_AE_roll,replicate(m[i],cormat_W1_dcc_roll[[i]][1,2]))
  W1_dcc_AB_roll <- c(W1_dcc_AB_roll,replicate(m[i],cormat_W1_dcc_roll[[i]][1,3]))
  W1_dcc_EB_roll <- c(W1_dcc_EB_roll,replicate(m[i],cormat_W1_dcc_roll[[i]][2,3]))
  W2_dcc_AE_roll <- c(W2_dcc_AE_roll,replicate(m[i],cormat_W2_dcc_roll[[i]][1,2]))
  W2_dcc_AB_roll <- c(W2_dcc_AB_roll,replicate(m[i],cormat_W2_dcc_roll[[i]][1,3]))
  W2_dcc_EB_roll <- c(W2_dcc_EB_roll,replicate(m[i],cormat_W2_dcc_roll[[i]][2,3]))
  W3_dcc_AE_roll <- c(W3_dcc_AE_roll,replicate(m[i],cormat_W3_dcc_roll[[i]][1,2]))
  W3_dcc_AB_roll <- c(W3_dcc_AB_roll,replicate(m[i],cormat_W3_dcc_roll[[i]][1,3]))
  W3_dcc_EB_roll <- c(W3_dcc_EB_roll,replicate(m[i],cormat_W3_dcc_roll[[i]][2,3]))
  W4_dcc_AE_roll <- c(W4_dcc_AE_roll,replicate(m[i],cormat_W4_dcc_roll[[i]][1,2]))
  W4_dcc_AB_roll <- c(W4_dcc_AB_roll,replicate(m[i],cormat_W4_dcc_roll[[i]][1,3]))
  W4_dcc_EB_roll <- c(W4_dcc_EB_roll,replicate(m[i],cormat_W4_dcc_roll[[i]][2,3]))
  W5_dcc_AE_roll <- c(W5_dcc_AE_roll,replicate(m[i],cormat_W5_dcc_roll[[i]][1,2]))
  W5_dcc_AB_roll <- c(W5_dcc_AB_roll,replicate(m[i],cormat_W5_dcc_roll[[i]][1,3]))
  W5_dcc_EB_roll <- c(W5_dcc_EB_roll,replicate(m[i],cormat_W5_dcc_roll[[i]][2,3]))
  W6_dcc_AE_roll <- c(W6_dcc_AE_roll,replicate(m[i],cormat_W6_dcc_roll[[i]][1,2]))
  W6_dcc_AB_roll <- c(W6_dcc_AB_roll,replicate(m[i],cormat_W6_dcc_roll[[i]][1,3]))
  W6_dcc_EB_roll <- c(W6_dcc_EB_roll,replicate(m[i],cormat_W6_dcc_roll[[i]][2,3]))
  W7_dcc_AE_roll <- c(W7_dcc_AE_roll,replicate(m[i],cormat_W7_dcc_roll[[i]][1,2]))
  W7_dcc_AB_roll <- c(W7_dcc_AB_roll,replicate(m[i],cormat_W7_dcc_roll[[i]][1,3]))
  W7_dcc_EB_roll <- c(W7_dcc_EB_roll,replicate(m[i],cormat_W7_dcc_roll[[i]][2,3]))
}
