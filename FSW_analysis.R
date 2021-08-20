##
df = read.csv("medium_mn.csv")

head(df)

## fit temp
fit = lm(df$Welding.temperature.. ~ df$Rotational.speed..rpm. + df$Traveling.speed..mm.min.1. + df$Plung.load..kg.,data = df)
summary(fit)
pred = predict(fit, df)
plot(df$Welding.temperature..)
points(pred, col = "blue")

library(rpart)
fit = rpart(df$Success..O..Failure..X.~ df$Rotational.speed..rpm. + df$Traveling.speed..mm.min.1. + df$Plung.load..kg. , data = df)
plot(fit)
fit
df$Success..O..Failure..X. = as.character( df$Success..O..Failure..X.)
typeof(df$Success..O..Failure..X.)

l.fit = glm(df$Success..O..Failure..X.~., data = df[,2:4], family = binomial)
summary(l.fit)
plot(l.fit$fitted.values)

coeff = l.fit$coefficients
summ = summary(l.fit)
summ$family

pred = as.matrix(cbind(matrix(1,nrow(df)),df[,2:4])) %*% t(t(as.matrix(coeff)))
plot(pred)

plot(1/(1+exp(-pred)))

fitness = 1/(1+exp(-as.matrix(cbind(matrix(1,nrow(df)),df[,2:4])) %*% t(t(as.matrix(coeff)))))

opt = function(x){1/(1+exp(-c(1,x) %*% t(t(as.matrix(coeff)))))} + 0.3 *abs(tempf(1445,0.005, 0.7,x)-1080)
#opt(opt_result)


library(GA)
lb = apply(df[,2:4],2,min) 
ub = apply(df[,2:4],2,max)
n = 3
GA <- ga(type = "real-valued", nBits=n, fitness = function(x)-opt(x), lower = c(lb), upper = ub, popSize = 1000, maxiter = 500, run = 100)
summary(GA)
plot(GA, main = "GA Results with the Number of Generations")
GA@solution

## temp f

tempf = function(melt.T, alpha, K, x){
  (K*(x[1]^2/x[2]*10^4)^alpha)*melt.T
}
tempf(melt.T = 1445, alpha = 0.005, K = 0.7, GA@solution)
1/(1+exp(-c(1,GA@solution) %*% t(t(as.matrix(coeff)))))


aset = seq(-0.05,0.05,0.001)
kset = seq(0.01,1,0.01)
x1set = seq(min(df$Rotational.speed..rpm.),max(df$Rotational.speed..rpm.),10)
x2set = seq(min(df$Traveling.speed..mm.min.1.),max(df$Traveling.speed..mm.min.1.),10)

res = c()
for(i in 1:5000){
samp_k = sample(kset,1)
samp_a = sample(aset,1)
#sampx1 = sample(x1set,1)
#sampx2 = sample(x2set,1)
temp = tempf(melt.T = 1445, alpha = samp_a, K =samp_k, c(114.9239, 268.6899))
d = cbind(samp_k, samp_a, temp)
res = rbind(res,d)
}
res = as.data.frame(res)
## 3dplot
library(tidyverse)
library(plotly)

p <- plot_ly(res, x = res$samp_k, y = res$samp_a, z = res$temp,
marker = list(color = iris$Petal.Width, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), alpha = 0.5) %>%
add_markers() %>%
layout(scene = list(xaxis = list(title = 'k'),
  yaxis = list(title = 'alpha'),
zaxis = list(title = 'estimated temp')),
annotations = list(
                                      x =1.17, #text x?œ„ì¹?
                                        y = 1.05, #text y?œ„ì¹?
                                         text = 'Petal.Width',
                                         xref = 'paper',
                                           yref = 'paper',
               showarrow = FALSE))
p

sampx1 = sample(x1set,1)
sampx2 = sample(x2set,1)
sampx3 =  4491.798
## x1, x2  , optimal
res = c()
for(i in 1:10000){
  sampx1 = sample(x1set,1)
  sampx2 = sample(x2set,1)
  samp_x3 = 4491.798
  x = c(sampx1, sampx2,samp_x3)
  #sampx1 = sample(x1set,1)
  #sampx2 = sample(x2set,1)
  obj = {1/(1+exp(-c(1,x) %*% t(t(as.matrix(coeff)))))} + 0.3 *abs(tempf(1445,0.005, 0.7,x)-1080)
  d = cbind(sampx1, sampx2, obj)
  res = rbind(res,d)
}
res = as.data.frame(res)
## 3dplot
library(tidyverse)
library(plotly)

p <- plot_ly(res, x = res$sampx1, y = res$sampx2, z = res$V3,
             marker = list(color = iris$Petal.Width, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), alpha = 0.1) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'rpm'),
                      yaxis = list(title = 'T.speed'),
                      zaxis = list(title = 'objective function')),
         annotations = list(
           x =1.17, #text x?œ„ì¹?
           y = 1.05, #text y?œ„ì¹?
           text = 'Petal.Width',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE))
p


## si_add_medium_mn
df = read.csv("d:/data/FSW_opt/si_add_medium_mn.csv")

head(df)
nrow(df)

## fit temp
fit = lm(df$Welding.temperature.. ~ df$Rotational.speed..rpm. + df$Traveling.speed..mm.min.1. + df$Plung.load..kg.,data = df)
summary(fit)
pred = predict(fit, df)
plot(df$Welding.temperature..)
points(pred, col = "blue")

library(rpart)
fit = rpart(df$Success..O..Failure..X.~ df$Rotational.speed..rpm. + df$Traveling.speed..mm.min.1. + df$Plung.load..kg. , data = df)
plot(fit)
fit
df$Success..O..Failure..X. = as.character( df$Success..O..Failure..X.)
typeof(df$Success..O..Failure..X.)

l.fit = glm(df$Success..O..Failure..X.~., data = df[,2:4], family = binomial)
summary(l.fit)
plot(l.fit$fitted.values)

coeff = l.fit$coefficients
summ = summary(l.fit)
summ$family

pred = as.matrix(cbind(matrix(1,nrow(df)),df[,2:4])) %*% t(t(as.matrix(coeff)))
plot(pred)

plot(1/(1+exp(-pred)))

fitness = 1/(1+exp(-as.matrix(cbind(matrix(1,nrow(df)),df[,2:4])) %*% t(t(as.matrix(coeff)))))

opt = function(x){1/(1+exp(-c(1,x) %*% t(t(as.matrix(coeff)))))} + 0.3 *abs(tempf(1395,0.005, 0.7,x)-1044)
#opt(opt_result)
tempf = function(melt.T, alpha, K, x){
  (K*(x[1]^2/x[2]*10^4)^alpha)*melt.T
}

library(GA)
lb = apply(df[,2:4],2,min) 
ub = apply(df[,2:4],2,max)
n = 3
GA <- ga(type = "real-valued", nBits=n, fitness = function(x)-opt(x), lower = c(lb), upper = ub, popSize = 1000, maxiter = 500, run = 100)
summary(GA)
plot(GA, main = "GA Results with the Number of Generations")
GA@solution
tempf()

## temp f

tempf = function(melt.T, alpha, K, x){
  (K*(x[1]^2/x[2]*10^4)^alpha)*melt.T
}
tempf(melt.T = 1395, alpha = 0.005, K = 0.7, GA@solution)
tempf(melt.T = 1445, alpha = 0.005, K = 0.7, GA@solution)
1/(1+exp(-c(1,GA@solution) %*% t(t(as.matrix(coeff)))))
plot(df[,2],df[,3])
points(GA@solution[1],GA@solution[2], col = "red", pch ="X")
aset = seq(-0.05,0.05,0.001)
kset = seq(0.01,1,0.01)
x1set = seq(min(df$Rotational.speed..rpm.),max(df$Rotational.speed..rpm.),10)
x2set = seq(min(df$Traveling.speed..mm.min.1.),max(df$Traveling.speed..mm.min.1.),10)

res = c()
for(i in 1:5000){
  samp_k = sample(kset,1)
  samp_a = sample(aset,1)
  #sampx1 = sample(x1set,1)
  #sampx2 = sample(x2set,1)
  temp = tempf(melt.T = 1445, alpha = samp_a, K =samp_k, c(114.9239, 268.6899))
  d = cbind(samp_k, samp_a, temp)
  res = rbind(res,d)
}
res = as.data.frame(res)
## 3dplot
library(tidyverse)
library(plotly)

p <- plot_ly(res, x = res$samp_k, y = res$samp_a, z = res$temp,
             marker = list(color = iris$Petal.Width, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), alpha = 0.5) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'k'),
                      yaxis = list(title = 'alpha'),
                      zaxis = list(title = 'estimated temp')),
         annotations = list(
           x =1.17, #text x?œ„ì¹?
           y = 1.05, #text y?œ„ì¹?
           text = 'Petal.Width',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE))
p

sampx1 = sample(x1set,1)
sampx2 = sample(x2set,1)
sampx3 =  4491.798
## x1, x2  , optimal
res = c()
for(i in 1:10000){
  sampx1 = sample(x1set,1)
  sampx2 = sample(x2set,1)
  samp_x3 = 4491.798
  x = c(sampx1, sampx2,samp_x3)
  #sampx1 = sample(x1set,1)
  #sampx2 = sample(x2set,1)
  obj = {1/(1+exp(-c(1,x) %*% t(t(as.matrix(coeff)))))} + 0.3 *abs(tempf(1445,0.005, 0.7,x)-1080)
  d = cbind(sampx1, sampx2, obj)
  res = rbind(res,d)
}
res = as.data.frame(res)
## 3dplot
library(tidyverse)
library(plotly)

p <- plot_ly(res, x = res$sampx1, y = res$sampx2, z = res$V3,
             marker = list(color = iris$Petal.Width, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE), alpha = 0.1) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'rpm'),
                      yaxis = list(title = 'T.speed'),
                      zaxis = list(title = 'objective function')),
         annotations = list(
           x =1.17, #text x?œ„ì¹?
           y = 1.05, #text y?œ„ì¹?
           text = 'Petal.Width',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE))
p
