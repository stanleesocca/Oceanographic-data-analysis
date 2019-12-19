# HYDROGRAPHY

# Seawater calculation 
library(oce)
data(ctd, package = "oce")
head(swRho(ctd, eos = "gsw"))

data("section")
SAv <- section[["SA"]]
SA1 <- section[["SA", "byStation"]]

head(gsw_rho(ctd[["SA"]], ctd[["CT"]], ctd[["pressure"]]), 3)

cabbel <- gsw_cabbeling(ctd[["SA"]], ctd[["CT"]], ctd[["p"]])

# PROFILE AND SECTION ANALYSIS
# Reading CTD dATA

d <- read.oce("sta01.cnv")

# editing CTD Data
data(ctd, package = "oce")
ctd[["temperature"]] <- ctd[["temperature"]] + 0.1
ctd <- oce.edit(ctd, "temperature", ctd[["temperature"]] + 0.1)
ctd <- oce.edit(ctd, "waterDepth", 100)
ctd@processingLog
summary(ctd)

# Plotting CTD data
plot(ctd)

# Trimming CTD Data
data("ctdRaw", package = "oce")
summary(ctdRaw)

plotScan(ctdRaw, type = "o")

p <- ctdRaw[["pressure"]]
stem(p)
boxplot(p)
hist(p)

which(abs(diff(p)) > 1)
which(abs(p - smooth(p)) > 0.5)
which(abs(p - mean(p)) > 2*sd(p))

data("ctdRaw", package = "oce")
ctd2 <- ctdTrim(ctdRaw, "range", parameters = list(item = "scan", from = 130, to = 320))
ctd3 <- ctdDecimate(ctd2, p = seq(0, 50, 1), "boxcar")
plot(ctd3, which = "N2", col.N2 = "black")
lines(swN2(ctd3, df = 10), ctd3[["pressure"]], lty = "dashed")

plot(ctd3, which = "density")
dz = mean(diff(swZ(ctd3)), na.rm = TRUE)
rho0 <- mean(swRho(ctd3), na.rm = TRUE)
rhom <- min(swRho(ctd3), na.rm = TRUE)
g <- gravity()
N2 <- swN2(ctd3, df = 10)
N2[is.na(N2)] <- 0
rho <- rhom - dz * (cumsum(N2) - N2[1]) * rho0 / g
lines(rho, ctd3[["pressure"]], lty = "dashed")


# .Plotsalinityandtemperatureproﬁlesforthe ctd dataset within 3dbar of the pycnocline centre
N2 <- swN2(ctd)
p0 <- ctd[["pressure"]][which.max(N2)]
pynocline <- subset(ctd, p0 - 2 <= pressure & pressure <= p0 + 2)

par(mfrow = c(1, 2))
plot(pynocline, which = "salinity", type = "o", yaxs = "i", abline(h = p0, lty = 2))
plot(pynocline, which = "temperature", type = "o", yaxs = "i", 
     abline(h = p0, lty = 2))
mtext(sprintf("%.1f dbar", p0), side = 4, at = p0)

# Use ctdTrim() and plotScan() together, to trim ctdRaw to just the downcast portion
data("ctdRaw", package = "oce")
plotScan(ctdRaw)
x <- round(locator(1)$x); abline(v = x); mtext(x, at = x, side = 3)

plotScan(ctdTrim(ctdRaw, "range", parameters = list(item = "scan", 
                                                    from = 100, to = 400)))
plotScan(ctdTrim(ctdRaw, "range", 
                 parameters = list(item = "scan", 
                                   from = 150, 
                                   to = 400)))
plotScan(ctdTrim(ctdRaw, method = "sbe"))

# smoothing and decimating CTD
library(oce)
data("ctdRaw", package = "oce")
ctd2 <- ctdTrim(ctdRaw, method = "sbe")
summary(diff(ctd2[["pressure"]]))

data("ctdRaw", package = "oce")
ctd2 <- ctdTrim(ctdRaw, method = "sbe")
ctd3 <- ctdDecimate(ctd2)

par(mfrow = c(1, 2))
plot(ctd2, which = "CT", plim = c(15, 0))
lines(ctd3[["CT"]], ctd3[["pressure"]], lwd = 3, col = "gray")
plot(ctd2, which = "SA", plim = c(15, 0))
lines(ctd3[["SA"]], ctd3[["pressure"]], lwd = 3, col = "gray")

 # Mixed layer detection
data(ctd, package = "oce")
plotProfile(ctd, xtype = "temperature", ylim = c(15, 0), 
            col.temperature = "black")
temperature <- ctd[["temperature"]]
pressure <- ctd[["pressure"]]
for (criterion in c(0.1, 0.5)) {
  inMLD <- abs(temperature[1] - temperature) < criterion
  MLDindex <- which.min(inMLD)
  MLDpressure <- pressure[MLDindex]
  abline(h = pressure[MLDindex], lwd = 2, lty = "dashed")
}

plotProfile(ctd, xtype = "N2", ylim = c(15, 0))
mid <- which.max(swN2(ctd))
pstar <- pressure[mid]
abline(h = pstar, lwd = 2, lty = "dashed")


url <- "https://cchdo.ucsd.edu/data/7872/a03_hy1.csv"
section <- read.section(file=url, sectionId="a03", institute="SIO", ship="R/V Professor Multanovskiy", scientist="Vladimir Tereschenkov")

# plotting section 
data(section, package = "oce")
GS <- subset(section, longitude < -70)
GS <- sectionSort(GS, by = "longitude")
GSG <- sectionGrid(GS, p = seq(0, 1600, 25))
plot(GSG, which = c(1, 99))
