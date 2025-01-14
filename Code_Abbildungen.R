library(wavelets)

# whole sample MRA

modwtobj <- modwt(as.ts(ret[,1]), filter="d18", n.levels = 7, boundary="periodic")
plot.modwt(modwtobj, levels = 7,y.rlabs=FALSE,plot.V=TRUE,X.xlab="")
modwtobj <- modwt(as.ts(ret[,2]), filter="d18", n.levels = 7, boundary="periodic")
plot.modwt(modwtobj, levels = 7,y.rlabs=FALSE,plot.V=TRUE,X.xlab="")
modwtobj <- modwt(as.ts(ret[,3]), filter="d18", n.levels = 7, boundary="periodic")
plot.modwt(modwtobj, levels = 7,y.rlabs=FALSE,plot.V=TRUE,X.xlab="")

library(dplR)

# AT1
yrs <- time(date)
dat <- as.numeric(r_import[,5])
out.wave <- morlet(y1 = dat, x1 = yrs, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(out.wave, useRaster=TRUE, reverse.y = TRUE,  add.coi = TRUE, add.sig = TRUE, res = 150, crn.lab = gettext("Zeitreihe"), period.lab = gettext("Skala"),x.lab = gettext("Zeit"))

# EQ
yrs <- time(date)
dat <- as.numeric(r_import[,6])
out.wave <- morlet(y1 = dat, x1 = yrs, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(out.wave, useRaster=TRUE, reverse.y = TRUE,  add.coi = TRUE, add.sig = TRUE, res = 150, crn.lab = gettext("Zeitreihe"), period.lab = gettext("Skala"),x.lab = gettext("Zeit"))

# BD
yrs <- time(date)
dat <- as.numeric(r_import[,7])
out.wave <- morlet(y1 = dat, x1 = yrs, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(out.wave, useRaster=TRUE, reverse.y = TRUE,  add.coi = TRUE, add.sig = TRUE, res = 150, crn.lab = gettext("Zeitreihe"), period.lab = gettext("Skala"),x.lab = gettext("Zeit"))

library(WaveletComp)
library(xts)

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,5])
y = as.numeric(my.data[,6])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

#AT1 vs BD

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,5])
y = as.numeric(my.data[,7])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

#EQ vs BD

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,6])
y = as.numeric(my.data[,7])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

# cum_oerf

#AT1 vs EQ

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,2])
y = as.numeric(my.data[,3])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

#AT1 vs BD

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,2])
y = as.numeric(my.data[,4])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

#EQ vs BD

my.data <- as.data.frame(r_import)
date = as.POSIXct(my.data[,1])
x = as.numeric(my.data[,3])
y = as.numeric(my.data[,4])
my.data <- data.frame(date = date,x = x,y = y)

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           # lowerPeriod = 16, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)


wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")

my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
                           loess.span = 0, dt = 1, dj = 0.01,
                           window.type.t = 3, window.type.s = 3,
                           window.size.t = 5, window.size.s = 1,
                           lowerPeriod = 32, upperPeriod = 256,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, which.image = "wc", color.key = "interval", n.levels = 250,
         siglvl.contour = 0.1, siglvl.arrow = 0.05,
         legend.params = list(lab = "Wavelet-KohÃ¤renz Level"),
         periodlab = "Zeitskala (Tage)",
         timelab = "Zeit", label.time.axis = TRUE, show.date = TRUE, date.format = "%F %T")
