library(xts)
library(openxlsx)

# Vorbereitung der Zeitreihen und erstellen der Programmparameter

date <- r_import$date[129:end(r_import$date)[1]]
len <- length(r_import$date)-128

len_sa <- (length(endpoints(ret,on="quarters"))+1)/2
t_sa <- endpoints(ret,on="quarters")[c(TRUE,FALSE)]
t_sa[1] <- 1
m <- diff(t_sa)
m[1] <- m[1]+1

# halbjÃ¤hrlich-rollierendes/erweiterndes Zeitfenster der Pearson-Korrelation

CorAE_ext <- NULL
CorAB_ext <- NULL
CorEB_ext <- NULL

CorAE_roll <- NULL
CorAB_roll <- NULL
CorEB_roll <- NULL

for (i in 1:len) {
  CorAE_ext[i] <- cor(r_import$AT1[1:(i+128)],r_import$EQ[1:(i+128)],method = "pearson")
  CorAB_ext[i] <- cor(r_import$AT1[1:(i+128)],r_import$BD[1:(i+128)],method = "pearson")
  CorEB_ext[i] <- cor(r_import$EQ[1:(i+128)],r_import$BD[1:(i+128)],method = "pearson")
  
  CorAE_roll[i] <- cor(r_import$AT1[i:(i+128)],r_import$EQ[i:(i+128)],method = "pearson")
  CorAB_roll[i] <- cor(r_import$AT1[i:(i+128)],r_import$BD[i:(i+128)],method = "pearson")
  CorEB_roll[i] <- cor(r_import$EQ[i:(i+128)],r_import$BD[i:(i+128)],method = "pearson")
}

# halbjÃ¤hrlich-rollierendes/erweiterndes Zeitfenster der Pearson-Kovarianz

len <- length(r_import$date)-128

CovAA_ext <- NULL
CovAE_ext <- NULL
CovAB_ext <- NULL
CovEE_ext <- NULL
CovEB_ext <- NULL
CovBB_ext <- NULL

CovAA_roll <- NULL
CovAE_roll <- NULL
CovAB_roll <- NULL
CovEE_roll <- NULL
CovEB_roll <- NULL
CovBB_roll <- NULL

for (i in 1:len) {
  CovAA_ext[i] <- cov(r_import$AT1[1:(i+128)],r_import$AT1[1:(i+128)],method = "pearson")
  CovAE_ext[i] <- cov(r_import$AT1[1:(i+128)],r_import$EQ[1:(i+128)],method = "pearson")
  CovAB_ext[i] <- cov(r_import$AT1[1:(i+128)],r_import$BD[1:(i+128)],method = "pearson")
  CovEE_ext[i] <- cov(r_import$EQ[1:(i+128)],r_import$EQ[1:(i+128)],method = "pearson")
  CovEB_ext[i] <- cov(r_import$EQ[1:(i+128)],r_import$BD[1:(i+128)],method = "pearson")
  CovBB_ext[i] <- cov(r_import$BD[1:(i+128)],r_import$BD[1:(i+128)],method = "pearson")
  
  CovAA_roll[i] <- cov(r_import$AT1[i:(i+128)],r_import$AT1[i:(i+128)],method = "pearson")
  CovAE_roll[i] <- cov(r_import$AT1[i:(i+128)],r_import$EQ[i:(i+128)],method = "pearson")
  CovAB_roll[i] <- cov(r_import$AT1[i:(i+128)],r_import$BD[i:(i+128)],method = "pearson")
  CovEE_roll[i] <- cov(r_import$EQ[i:(i+128)],r_import$EQ[i:(i+128)],method = "pearson")
  CovEB_roll[i] <- cov(r_import$EQ[i:(i+128)],r_import$BD[i:(i+128)],method = "pearson")
  CovBB_roll[i] <- cov(r_import$BD[i:(i+128)],r_import$BD[i:(i+128)],method = "pearson")
}

# Frequenzmodifikation der Korrelation auf Halbjahresbasis

PCorAE_ext <- NULL
PCorAB_ext <- NULL
PCorEB_ext <- NULL

PCorAE_roll <- NULL
PCorAB_roll <- NULL
PCorEB_roll <- NULL

for (i in 1:(length(m))) {
  PCorAE_ext <- c(PCorAE_ext,replicate(m[i],CorAE_ext[t_sa[i]]))
  PCorAB_ext <- c(PCorAB_ext,replicate(m[i],CorAB_ext[t_sa[i]]))
  PCorEB_ext <- c(PCorEB_ext,replicate(m[i],CorEB_ext[t_sa[i]]))
  PCorAE_roll <- c(PCorAE_roll,replicate(m[i],CorAE_roll[t_sa[i]]))
  PCorAB_roll <- c(PCorAB_roll,replicate(m[i],CorAB_roll[t_sa[i]]))
  PCorEB_roll <- c(PCorEB_roll,replicate(m[i],CorEB_roll[t_sa[i]]))
}
