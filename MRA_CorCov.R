library(wavelets)
library(xts)
library(openxlsx)

# Vorbereitung der Zeitreihen und erstellen der Programmparameter

date <- r_import$date
ret <- r_import[,5:7]
ret <- xts(x = ret,order.by = as.Date(date))

len_sa <- (length(endpoints(ret,on="quarters"))+1)/2
t_sa <- endpoints(ret,on="quarters")[c(TRUE,FALSE)]
t_sa[1] <- 1
m <- diff(t_sa)
m[1] <- m[1]+1

# MSA fÃ¼r das Pearson-Modell und das DCC-Modell

W1 <- NULL
W2 <- NULL
W3 <- NULL
W4 <- NULL
W5 <- NULL
W6 <- NULL
W7 <- NULL

W1.dcc <- NULL
W2.dcc <- NULL
W3.dcc <- NULL
W4.dcc <- NULL
W5.dcc <- NULL
W6.dcc <- NULL
W7.dcc <- NULL

for (i in 1:(len_sa-1)) {
  MRA <- mra(as.ts(ret[t_sa[1]:t_sa[i+1]]), filter = "d18", n.levels = 7, boundary = "periodic", method = "modwt")
  W1[[i]] <- MRA@D[[1]]
  W2[[i]] <- MRA@D[[2]]
  W3[[i]] <- MRA@D[[3]]
  W4[[i]] <- MRA@D[[4]]
  W5[[i]] <- MRA@D[[5]]
  W6[[i]] <- MRA@D[[6]]
  W7[[i]] <- MRA@D[[7]]
}

for (i in 1:(len_sa-1)) {
  MRA <- mra(as.ts(ret[t_sa[1]:t_sa[i+1]]), filter = "d18", n.levels = 7, boundary = "periodic", method = "modwt")
  W1.dcc[[i]] <- MRA@D[[1]] + MRA@S[[1]]
  W2.dcc[[i]] <- MRA@D[[2]] + MRA@S[[2]]
  W3.dcc[[i]] <- MRA@D[[3]] + MRA@S[[3]]
  W4.dcc[[i]] <- MRA@D[[4]] + MRA@S[[4]]
  W5.dcc[[i]] <- MRA@D[[5]] + MRA@S[[5]]
  W6.dcc[[i]] <- MRA@D[[6]] + MRA@S[[6]]
  W7.dcc[[i]] <- MRA@D[[7]] + MRA@S[[7]]
}

# halbjÃ¤hrlich-rollierendes/erweiterndes Zeitfenster der Pearson-Korrelation anhand der transformierten Daten

covmat_W1_ext <- NULL
cormat_W1_ext <- NULL
covmat_W1_roll <- NULL
cormat_W1_roll <- NULL
covmat_W2_ext <- NULL
cormat_W2_ext <- NULL
covmat_W2_roll <- NULL
cormat_W2_roll <- NULL
covmat_W3_ext <- NULL
cormat_W3_ext <- NULL
covmat_W3_roll <- NULL
cormat_W3_roll <- NULL
covmat_W4_ext <- NULL
cormat_W4_ext <- NULL
covmat_W4_roll <- NULL
cormat_W4_roll <- NULL
covmat_W5_ext <- NULL
cormat_W5_ext <- NULL
covmat_W5_roll <- NULL
cormat_W5_roll <- NULL
covmat_W6_ext <- NULL
cormat_W6_ext <- NULL
covmat_W6_roll <- NULL
cormat_W6_roll <- NULL
covmat_W7_ext <- NULL
cormat_W7_ext <- NULL
covmat_W7_roll <- NULL
cormat_W7_roll <- NULL


for (i in 1:(len_sa-1)) {
  covmat_W1_ext[[i]] <- cov(W1[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W1_ext[[i]] <- cor(W1[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W2_ext[[i]] <- cov(W2[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W2_ext[[i]] <- cor(W2[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W3_ext[[i]] <- cov(W3[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W3_ext[[i]] <- cor(W3[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W4_ext[[i]] <- cov(W4[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W4_ext[[i]] <- cor(W4[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W5_ext[[i]] <- cov(W5[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W5_ext[[i]] <- cor(W5[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W6_ext[[i]] <- cov(W6[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W6_ext[[i]] <- cor(W6[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  covmat_W7_ext[[i]] <- cov(W7[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  cormat_W7_ext[[i]] <- cor(W7[[i]][t_sa[1]:t_sa[i+1],],method = "pearson")
  
  covmat_W1_roll[[i]] <- cov(W1[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W1_roll[[i]] <- cor(W1[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W2_roll[[i]] <- cov(W2[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W2_roll[[i]] <- cor(W2[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W3_roll[[i]] <- cov(W3[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W3_roll[[i]] <- cor(W3[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W4_roll[[i]] <- cov(W4[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W4_roll[[i]] <- cor(W4[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W5_roll[[i]] <- cov(W5[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W5_roll[[i]] <- cor(W5[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W6_roll[[i]] <- cov(W6[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W6_roll[[i]] <- cor(W6[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  covmat_W7_roll[[i]] <- cov(W7[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
  cormat_W7_roll[[i]] <- cor(W7[[i]][t_sa[i]:t_sa[i+1],],method = "pearson")
}

W1_AE_ext <- NULL
W1_AB_ext <- NULL
W1_EB_ext <- NULL
W2_AE_ext <- NULL
W2_AB_ext <- NULL
W2_EB_ext <- NULL
W3_AE_ext <- NULL
W3_AB_ext <- NULL
W3_EB_ext <- NULL
W4_AE_ext <- NULL
W4_AB_ext <- NULL
W4_EB_ext <- NULL
W5_AE_ext <- NULL
W5_AB_ext <- NULL
W5_EB_ext <- NULL
W6_AE_ext <- NULL
W6_AB_ext <- NULL
W6_EB_ext <- NULL
W7_AE_ext <- NULL
W7_AB_ext <- NULL
W7_EB_ext <- NULL

W1_AE_roll <- NULL
W1_AB_roll <- NULL
W1_EB_roll <- NULL
W2_AE_roll <- NULL
W2_AB_roll <- NULL
W2_EB_roll <- NULL
W3_AE_roll <- NULL
W3_AB_roll <- NULL
W3_EB_roll <- NULL
W4_AE_roll <- NULL
W4_AB_roll <- NULL
W4_EB_roll <- NULL
W5_AE_roll <- NULL
W5_AB_roll <- NULL
W5_EB_roll <- NULL
W6_AE_roll <- NULL
W6_AB_roll <- NULL
W6_EB_roll <- NULL
W7_AE_roll <- NULL
W7_AB_roll <- NULL
W7_EB_roll <- NULL

for (i in 1:(length(m))) {
  W1_AE_ext <- c(W1_AE_ext,replicate(m[i],cormat_W1_ext[[i]][1,2]))
  W1_AB_ext <- c(W1_AB_ext,replicate(m[i],cormat_W1_ext[[i]][1,3]))
  W1_EB_ext <- c(W1_EB_ext,replicate(m[i],cormat_W1_ext[[i]][2,3]))
  W2_AE_ext <- c(W2_AE_ext,replicate(m[i],cormat_W2_ext[[i]][1,2]))
  W2_AB_ext <- c(W2_AB_ext,replicate(m[i],cormat_W2_ext[[i]][1,3]))
  W2_EB_ext <- c(W2_EB_ext,replicate(m[i],cormat_W2_ext[[i]][2,3]))
  W3_AE_ext <- c(W3_AE_ext,replicate(m[i],cormat_W3_ext[[i]][1,2]))
  W3_AB_ext <- c(W3_AB_ext,replicate(m[i],cormat_W3_ext[[i]][1,3]))
  W3_EB_ext <- c(W3_EB_ext,replicate(m[i],cormat_W3_ext[[i]][2,3]))
  W4_AE_ext <- c(W4_AE_ext,replicate(m[i],cormat_W4_ext[[i]][1,2]))
  W4_AB_ext <- c(W4_AB_ext,replicate(m[i],cormat_W4_ext[[i]][1,3]))
  W4_EB_ext <- c(W4_EB_ext,replicate(m[i],cormat_W4_ext[[i]][2,3]))
  W5_AE_ext <- c(W5_AE_ext,replicate(m[i],cormat_W5_ext[[i]][1,2]))
  W5_AB_ext <- c(W5_AB_ext,replicate(m[i],cormat_W5_ext[[i]][1,3]))
  W5_EB_ext <- c(W5_EB_ext,replicate(m[i],cormat_W5_ext[[i]][2,3]))
  W6_AE_ext <- c(W6_AE_ext,replicate(m[i],cormat_W6_ext[[i]][1,2]))
  W6_AB_ext <- c(W6_AB_ext,replicate(m[i],cormat_W6_ext[[i]][1,3]))
  W6_EB_ext <- c(W6_EB_ext,replicate(m[i],cormat_W6_ext[[i]][2,3]))
  W7_AE_ext <- c(W7_AE_ext,replicate(m[i],cormat_W7_ext[[i]][1,2]))
  W7_AB_ext <- c(W7_AB_ext,replicate(m[i],cormat_W7_ext[[i]][1,3]))
  W7_EB_ext <- c(W7_EB_ext,replicate(m[i],cormat_W7_ext[[i]][2,3]))
}

for (i in 1:(length(m))) {
  W1_AE_roll <- c(W1_AE_roll,replicate(m[i],cormat_W1_roll[[i]][1,2]))
  W1_AB_roll <- c(W1_AB_roll,replicate(m[i],cormat_W1_roll[[i]][1,3]))
  W1_EB_roll <- c(W1_EB_roll,replicate(m[i],cormat_W1_roll[[i]][2,3]))
  W2_AE_roll <- c(W2_AE_roll,replicate(m[i],cormat_W2_roll[[i]][1,2]))
  W2_AB_roll <- c(W2_AB_roll,replicate(m[i],cormat_W2_roll[[i]][1,3]))
  W2_EB_roll <- c(W2_EB_roll,replicate(m[i],cormat_W2_roll[[i]][2,3]))
  W3_AE_roll <- c(W3_AE_roll,replicate(m[i],cormat_W3_roll[[i]][1,2]))
  W3_AB_roll <- c(W3_AB_roll,replicate(m[i],cormat_W3_roll[[i]][1,3]))
  W3_EB_roll <- c(W3_EB_roll,replicate(m[i],cormat_W3_roll[[i]][2,3]))
  W4_AE_roll <- c(W4_AE_roll,replicate(m[i],cormat_W4_roll[[i]][1,2]))
  W4_AB_roll <- c(W4_AB_roll,replicate(m[i],cormat_W4_roll[[i]][1,3]))
  W4_EB_roll <- c(W4_EB_roll,replicate(m[i],cormat_W4_roll[[i]][2,3]))
  W5_AE_roll <- c(W5_AE_roll,replicate(m[i],cormat_W5_roll[[i]][1,2]))
  W5_AB_roll <- c(W5_AB_roll,replicate(m[i],cormat_W5_roll[[i]][1,3]))
  W5_EB_roll <- c(W5_EB_roll,replicate(m[i],cormat_W5_roll[[i]][2,3]))
  W6_AE_roll <- c(W6_AE_roll,replicate(m[i],cormat_W6_roll[[i]][1,2]))
  W6_AB_roll <- c(W6_AB_roll,replicate(m[i],cormat_W6_roll[[i]][1,3]))
  W6_EB_roll <- c(W6_EB_roll,replicate(m[i],cormat_W6_roll[[i]][2,3]))
  W7_AE_roll <- c(W7_AE_roll,replicate(m[i],cormat_W7_roll[[i]][1,2]))
  W7_AB_roll <- c(W7_AB_roll,replicate(m[i],cormat_W7_roll[[i]][1,3]))
  W7_EB_roll <- c(W7_EB_roll,replicate(m[i],cormat_W7_roll[[i]][2,3]))
}



