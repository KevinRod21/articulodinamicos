library(R2jags)
library(coda)
library(lattice)
library(R2WinBUGS)
library(rjags)
library(superdiag)
library(mcmcplots)
library(readr)
library(tseries)
library(dlm)
library(gtools)
library(readxl)

##Intento modelo jerarquico

m1.model <-function() {
  et[1]~dnorm(0,0.01)
  ut[1]~dnorm(0,0.01)
  v1[1]~dnorm(0,0.01)
  v2[1]~dnorm(0,0.01)
  al0[1]~dnorm(0,0.01)
  al1[1]~dnorm(0,0.01)
  betha[1] = al0[1] + al1[1]*x1[1]+ut[1]
  y[1]~dnorm(betha[1],et[1])
  for(i in 2:n){
    betha[i] = al0[i] + al1[i]*x1[i]+ut[i]
    y[i]~dnorm(mu[i],et[i])
    mu[i]<- betha[i]*y[i-1]+et[i]
    al0[i]=al0[i-1]+v1[i]
    al1[i]=al1[i-1]+v2[i]
    et[i]~dnorm(0,0.01)
    ut[i]~dnorm(0,0.01)
    v1[i]~dnorm(0,0.01)
    v2[i]~dnorm(0,0.01)
  }
}