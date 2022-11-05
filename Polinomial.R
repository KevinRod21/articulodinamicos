library(dlm)
# Datos
library(readxl)
library(readr)
library(tseries)
library(MLmetrics)
cereales <- read_excel("Base nueva.xlsx", sheet = "Datos")
cereales=cereales[-c(1,59:62),]
y=cereales$`Consumo de fertilizantes`[1:57]
n=length(y)
y=ts(y)
plot(y)
adf.test(y)

## Pron?stico
expd.dlm <- dlm(m0 = rep(0,4), C0 = 1e8 * diag(4),
                FF = matrix(c(1, 1, 0, 0), nr = 1),
                V = 7,
                GG = bdiag(matrix(1),
                           matrix(c(-1, -1, -1, 1, 0, 0, 0, 1, 0),
                                  nr = 3, byrow = TRUE)),
                W = diag(c(100, 1, 0, 0), nr = 4))
expdFilt <- dlmFilter(y, expd.dlm)

## Gr?fico

set.seed(1)
expdFore <- dlmForecast(expdFilt, nAhead =1, sampleNew =10)

expdFore$newObs

plot(cereales$`Consumo de fertilizantes`, type = "l",col = c("cornflowerblue"), xlab = "Año", 
     ylab = "Kg",main="Consumo de fertilizante",xlim = c(0,58),xaxt = "n")
axis(1,                         # Define x-axis manually
     at = c(0,10,20,30,40,50,60),
     labels = c(1961,1961+9,1961+19,1961+29,1961+39,1961+49,1961+59))

names(expdFore)
attach(expdFore)
invisible(lapply(newObs, function(x)
  lines(x, col = "darkgrey",
        type = 'o', pch = 4)))
lines(f, type = 'o', lwd = 2, pch = 16)
abline(v = mean(c(time(f)[1], time(y)[length(y)])),
       lty = "dashed")
detach()
expdFore$f
expdFore$Q
mean(c(time(f)))

MAPE(351.1179,190.935990699219)

vec1=data.frame("i"=NA,"j"=NA,"m"=NA)
vec2=c()
w=1
reales=cereales$`Consumo de fertilizantes`[59:60]
for (i in 1:50) {
  for (j in 1:100) {
    for (m in 1:100) {
      expd.dlm <- dlm(m0 = rep(0,4), C0 = 1e8 * diag(4),
                      FF = matrix(c(1, 1, 0, 0), nr = 1),
                      V = i,
                      GG = bdiag(matrix(1),
                                 matrix(c(-1, -1, -1, 1, 0, 0, 0, 1, 0),
                                        nr = 3, byrow = TRUE)),
                      W = diag(c(j, m, 0, 0), nr = 4))
      expdFilt <- dlmFilter(y, expd.dlm)
      set.seed(1)
      expdFore <- dlmForecast(expdFilt, nAhead =5, sampleNew =10)
      sopas=data.frame("i"=i,"j"=j,"m"=m)
      vec1=rbind(vec1, sopas)
      vec2[w]= sum(abs(reales-as.vector(expdFore$f))/reales)/length(reales)
        w=w+1
    }
  }
}
vec1=na.omit(vec1)
vec1[which.min(vec2),]
vec2[which.min(vec2)]
