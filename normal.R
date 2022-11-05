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


cereales <- read_excel("Base nueva.xlsx", sheet = "Datos")
cereales=cereales[-c(1,59:62),]
prod<-ts(cereales$`Producción de cereales (colombia)`, start = 1970, frequency=1)

y = ts(cereales$`Consumo de fertilizantes`)
x1 = cereales$`Producción de cereales (colombia)`/10000
x2 = cereales$Hectareas/10000
x3 = cereales$`Importaciones de bienes y servicios`
x4 = cereales$`Inflación, precios al consumidor`
x5 = cereales$`Población rural`
x6 = cereales$`Tierras cultivables`
x7 = cereales$`Produccion de cereal (EEUU)`/1000000
n = length(y)

## modelos univariados con gama variables

variables=c("x1","x2","x3","x4","x5","x6","x7")
parejas=combinations(7,2,variables)
trio=combinations(7,3,variables)
X= as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7))

dic_1=c()
dic_2=c()
dic_3=c()

#univariadas

{m1.model <-function() {
    #informacion a priori
    beta0[1]~dnorm(0,0.01)
    beta1[1]~dnorm(0,0.01)
    w1[1]~dnorm(0,0.01)
    w2[1]~dnorm(0,0.01)
    fi[1]~dgamma(1,1)
    for(i in 2:n)
    {
      y[i]~dnorm(mu[i],fi[i])
      mu[i]<- beta0[i]+beta1[i]*x1[i]
      beta0[i]<-beta0[i-1]+w1[i]
      beta1[i]<-beta1[i-1]+w2[i]
      fi[i]~dgamma(1,1)
      w1[i]~dnorm(0.01,0.01)
      w2[i]~dnorm(0.01,0.01)
    }
  }
  
  m1.data <- list("y","n","x1")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x3[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x3")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x4[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x4")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x5[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x5")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x6")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x7")
  m1.param <- c("mu","fi","beta0","beta1")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_1=c(dic_1,m1.fit$BUGSoutput$DIC)
}

#bivariadas

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x2[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x2")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x3[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x3")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x4[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x4")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x5[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x5")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x6")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x1[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x1","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]+beta2[i]*x3[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2","x3")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]+beta2[i]*x4[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2","x4")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]+beta2[i]*x5[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2","x5")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]+beta2[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2","x6")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x2[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x2","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x3[i]+beta2[i]*x4[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x3","x4")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x3[i]+beta2[i]*x5[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x3","x5")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x3[i]+beta2[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x3","x6")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x3[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x3","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x4[i]+beta2[i]*x5[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x4","x5")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x4[i]+beta2[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x4","x6")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x4[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x4","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x5[i]+beta2[i]*x6[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x5","x6")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x5[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x5","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}

{m1.model <-function() {
  #informacion a priori
  beta0[1]~dnorm(0,0.01)
  beta1[1]~dnorm(0,0.01)
  beta2[1]~dnorm(0,0.01)
  w1[1]~dnorm(0,0.01)
  w2[1]~dnorm(0,0.01)
  w3[1]~dnorm(0,0.01)  
  fi[1]~dgamma(1,1)
  for(i in 2:n)
  {
    y[i]~dnorm(mu[i],fi[i])
    mu[i]<- beta0[i]+beta1[i]*x6[i]+beta2[i]*x7[i]
    beta0[i]<-beta0[i-1]+w1[i]
    beta1[i]<-beta1[i-1]+w2[i]
    beta2[i]<-beta1[i-1]+w3[i]
    fi[i]~dgamma(1,1)
    w1[i]~dnorm(0.01,0.01)
    w2[i]~dnorm(0.01,0.01)
    w3[i]~dnorm(0.01,0.01)
  }
}
  
  m1.data <- list("y","n","x6","x7")
  m1.param <- c("mu","fi","beta0","beta1","beta2")
  set.seed(123)
  m1.fit <- jags(data=m1.data, parameters.to.save=m1.param, 
                 n.chains=3, n.iter=10000, n.burnin=1000,
                 n.thin=5, model.file=m1.model)
  dic_2=c(dic_2,m1.fit$BUGSoutput$DIC)
}



dic_1
dic_2

#print(m1.fit)
#traceplot(m1.fit)
Resumen=m1.fit$BUGSoutput$summary

#Resumen


#### modelos con xi ####

# Estimación de la media
media=Resumen[c(1:n),1]
li=Resumen[c(1:n),3]
ls=Resumen[c(1:n),7]
a1=min(li,ls)-1
a2=max(li,ls)+1
plot(media,type="l",ylim=c(a1,a2),col="aquamarine3",lwd=2, 
     main = "Intervalos de credibilidad al 95% para B0 de M1")
lines(li,type="l",col="red",lty=2)
lines(ls,type="l",col="red",lty=2)
abline(h=0,lty=2,col="gray50")

#  Estimación de  $\beta_{0t}$
media=Resumen[(n+2):(2*n),1]
li=Resumen[(n+2):(2*n),3]
ls=Resumen[(n+2):(2*n),7]
a1=min(li,ls)-1
a2=max(li,ls)+1
plot(media,type="l",ylim=c(a1,a2),col="aquamarine3",lwd=2,
     main = "Intervalos de credibilidad al 95% para B1 de M1")
lines(li,type="l",col="red",lty=2)
lines(ls,type="l",col="red",lty=2)
abline(h=0,lty=2,col="gray40") #Muy significativo

## Otro

media=Resumen[(2*n+1):(3*n-2),1]
li=Resumen[(2*n+1):(3*n-2),3]
ls=Resumen[(2*n+1):(3*n-2),7]
a1=min(li,ls)-1
a2=max(li,ls)+1
plot(media,type="l",ylim=c(a1,a2),col="aquamarine3",lwd=2,
     main = "Intervalos de credibilidad al 95% para B1 de M1")
lines(li,type="l",col="red",lty=2)
lines(ls,type="l",col="red",lty=2)
abline(h=0,lty=2,col="gray40") #Muy significativo

#  Estimación de  $\beta_{1t}$
media=Resumen[(3*n+2):(4*n),1]
li=Resumen[(3*n+2):(4*n),3]
ls=Resumen[(3*n+2):(4*n),7]
a1=min(li,ls)-1
a2=max(li,ls)+1
plot(media,type="l",ylim=c(a1,a2),col="aquamarine3",lwd=2,
     main = "Intervalos de credibilidad al 95% para Mu de M1")
lines(li,type="l",col="red",lty=2)
lines(ls,type="l",col="red",lty=2)
abline(h=0,lty=2,col="gray40") #No es significativo
lines(y[c(2:58)],type="l",lwd=2,col="gray40")
legend("topleft", legend = c("Estimado","Acción"),
       col=c("aquamarine3", "gray40"),
       lty = c("solid", "solid"), bty = "n")

MLmetrics::MAPE(media,y[c(2:58)])
