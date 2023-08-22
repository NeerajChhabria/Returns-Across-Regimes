#Gathering Data from Different Sources
library(data.table)
library(quantmod)
#1 Rdata_4 

load(file = 'data_4.RData')
#COMMENTS ON DATA SET: the dataset data_4 has total of 12 Coloumns. Coulmns 1-4 are populated with four scenarios. 
# based on Economic Regime. These are binary in content. 




#2 Sector Portfolio Returns 

Tickers <- c("XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")
ntick <- length(Tickers)
start_date <- "2000-01-01"
end_date <- "2013-12-01"

num <- 0
for (i in 1:ntick) {
  tkr <- Tickers[i]
  dat <- getSymbols(tkr,src="yahoo",from=start_date,to=end_date,auto.assign=FALSE)
  print(dat[1,])
  if (num == 0) {
    All <- dat[,6]
    num <- 1
  } else {
    All <- merge(All,dat[,6],join="outer")
  }      
  print(tkr)
}
names(All) <- as.character(Tickers[1:ntick])

data <- diff(log(All))
data <- data[-1,]
data
length(data$XLB)

LogReturnsXLB <- apply.monthly(data$XLB,sum)
DiscreteXLB<- exp(LogReturnsXLB)-1
length(DiscreteXLB)
DiscreteXLB                                                #Starting from 2000-01-31                                               

dt<-as.data.table(dt)
dt<-dt[325:491, ]
dt                                                            #Starting from 2000-01

LogReturnsXLE <- apply.monthly(data$XLE,sum)
DiscreteXLE<- exp(LogReturnsXLE)-1

LogReturnsXLF <- apply.monthly(data$XLF,sum)
DiscreteXLF<- exp(LogReturnsXLF)-1

LogReturnsXLI <- apply.monthly(data$XLI,sum)
DiscreteXLI<- exp(LogReturnsXLI)-1

LogReturnsXLK <- apply.monthly(data$XLK,sum)
DiscreteXLK<- exp(LogReturnsXLK)-1

LogReturnsXLP <- apply.monthly(data$XLP,sum)
DiscreteXLP<- exp(LogReturnsXLP)-1

LogReturnsXLU <- apply.monthly(data$XLU,sum)
DiscreteXLU<- exp(LogReturnsXLU)-1

LogReturnsXLV <- apply.monthly(data$XLV,sum)
DiscreteXLV<- exp(LogReturnsXLV)-1

LogReturnsXLY <- apply.monthly(data$XLY,sum)
DiscreteXLY<- exp(LogReturnsXLY)-1

FinalData<-cbind(DiscreteXLB, DiscreteXLE, DiscreteXLF, DiscreteXLI, DiscreteXLK, DiscreteXLP, DiscreteXLU, DiscreteXLV, DiscreteXLY)
FinalData

FinalData<-cbind(dt, FinalData)
dt<-as.data.frame(FinalData)

dt

comp = function(dt, sub = 'all') {
  if(!(sub %in% c('all', 'INF1GRW1', 'INF2GRW1', 'INF1GRW2', 'INF2GRW2'))) {
    stop('Invalid subset!')
  }
  if(sub != 'all'){
    dt = dt[dt[sub] == 1,]
  }
  A = dt[,c(10,14,15,16,17,18,19,20,21,22)] # assets
  R = A[,c(2,3,4,5,6,7,8,9,10)] # risk assets
  RF = A[,1] # risk free
  a = colMeans(A[,-1]) # mean
  s = apply(R, MARGIN = 2, sd) # sd
  r = colMeans(R)
  rf = mean(RF)
  sr = (r-rf)/s
  result = list(mean = a, Std = s, 'SharpeRatio' = sr)
  return(result)
}

(res1 = comp(dt))


## INF1GRW1
(res2 = comp(dt, sub = 'INF1GRW1'))


## INF1GRW1
(res2 = comp(dt, sub = 'INF1GRW1'))

## INF2GRW1
(res3 = comp(dt, sub = 'INF2GRW1'))

## INF1GRW2
(res4 = comp(dt, sub = 'INF1GRW2'))

## INF2GRW2
(res5 = comp(dt, sub = 'INF2GRW2'))

tab_mean = rbind(res1$mean, res2$mean, res3$mean, res4$mean, res5$mean) # Mean
tab_sd = rbind(res1$Std, res2$Std, res3$Std, res4$Std, res5$Std) # Standard Deviation
tab_sr = rbind(res1$SharpeRatio, res2$SharpeRatio, res3$SharpeRatio, res4$SharpeRatio, res5$SharpeRatio) # Sharpe Ratio
rownames(tab_mean)  = c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(tab_sd) =  c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_sr) =  c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")


#######################################PART II#############################################################

load(file = 'data_4.RData')

CPI = dt$Inflation #inflation
CPI_ind = cumprod(CPI+1)*100
ISMNO = dt$ISMNO.Index
my = dt$MY
par(mfrow = c(3,1))
plot(CPI_ind, type = 'l', main = 'CPI-U Inflation Index (1973 = 100)')
plot(CPI, type = 'l', main = 'ISM New orders Index (Mean = 50)')

# 1&2
aver1 = 1.3
aver2 = 2.8
aver3 = 6.5
aver4 = 10.5
aver5 = 16.9

comp = function(dt, sub = 'all') {
  if(!(sub %in% c('all', 'INF1GRW1', 'INF2GRW1', 'INF1GRW2', 'INF2GRW2'))) {
    stop('Invalid subset!')
  }
  if(sub != 'all'){
    dt = dt[dt[sub] == 1,]
  }
  A = dt[,6:10] # assets
  R = A[,1:4] # risk assets
  RF = A[,5] # risk free
  a = colMeans(A) # mean
  s = apply(R, MARGIN = 2, sd) # sd
  r = colMeans(R)
  rf = mean(RF)
  sr = (r-rf)/s
  vec1 = rep(1, length = length (r))
  corr = cor(R)
  VCV = cov(R)
  w = t(solve(VCV) %*% (r-rf)) / as.numeric(t(rep(1,4)) %*% solve(VCV) %*% (r-rf))
  w = c(w, 1-sum(w))
  w1 = t(solve(VCV) %*% vec1) / as.numeric(t(vec1) %*% solve(VCV) %*% vec1)
  w1 = c(w1, 1-sum(w1))
  w.a1 = t(solve(VCV) %*% (r-rf) / aver1)
  w.a1 = c(w.a1, 1-sum(w.a1))
  w.a2 = t(solve(VCV) %*% (r-rf) / aver2)
  w.a2 = c(w.a2, 1-sum(w.a2))
  w.a3 = t(solve(VCV) %*% (r-rf) / aver3)
  w.a3 = c(w.a3, 1-sum(w.a3))
  w.a4 = t(solve(VCV) %*% (r-rf) / aver4)
  w.a4 = c(w.a4, 1-sum(w.a4))
  w.a5 = t(solve(VCV) %*% (r-rf) / aver5)
  w.a5 = c(w.a5, 1-sum(w.a5))
  result = list(mean = a, Std = s, 'SharpeRatio' = sr, 'W.MSR' = w, 'W.GMV' = w1, 'W.aversion1.3' = w.a1, 'W.aversion2.8' = w.a2, 'W.aversion6.5' = w.a3, 'W.aversion10.5' = w.a4, 'W.aversion16.9' = w.a5)
  return(result)
}
#####This function prints mean of returns, Sharpe Ratio,, Weights of WSR, Weight of GMV, and Weights with Different Aversions.
## unconditional

(res1 = comp(dt))

## INF1GRW1
(res2 = comp(dt, sub = 'INF1GRW1'))

## INF2GRW1
(res3 = comp(dt, sub = 'INF2GRW1'))

## INF1GRW2
(res4 = comp(dt, sub = 'INF1GRW2'))

## INF2GRW2
(res5 = comp(dt, sub = 'INF2GRW2'))

tab_mean = rbind(res1$mean, res2$mean, res3$mean, res4$mean, res5$mean) # Mean
tab_sd = rbind(res1$Std, res2$Std, res3$Std, res4$Std, res5$Std) # Standard Deviation
tab_sr = rbind(res1$SharpeRatio, res2$SharpeRatio, res3$SharpeRatio, res4$SharpeRatio, res5$SharpeRatio) # Sharpe Ratio
tab_MSR_w = rbind(res1$W.MSR, res2$W.MSR, res3$W.MSR, res4$W.MSR, res5$W.MSR) # MSR weight
tab_GMV_w = rbind(res1$W.GMV, res2$W.GMV, res3$W.GMV, res4$W.GMV, res5$W.GMV) # GMV weight
tab_Aver_w1 = rbind(res1$W.aversion1.3, res2$W.aversion1.3, res3$W.aversion1.3, res4$W.aversion1.3, res5$W.aversion1.3) # Risk Aversion = 1.3 weight
tab_Aver_w2 = rbind(res1$W.aversion2.8, res2$W.aversion2.8, res3$W.aversion2.8, res4$W.aversion2.8, res5$W.aversion2.8) # Risk Aversion = 2.8 weight
tab_Aver_w3 = rbind(res1$W.aversion6.5, res2$W.aversion6.5, res3$W.aversion6.5, res4$W.aversion6.5, res5$W.aversion6.5) # Risk Aversion = 6.5 weight
tab_Aver_w4 = rbind(res1$W.aversion10.5, res2$W.aversion10.5, res3$W.aversion10.5, res4$W.aversion10.5, res5$W.aversion10.5) # Risk Aversion = 10.5 weight
tab_Aver_w5 = rbind(res1$W.aversion16.9, res2$W.aversion16.9, res3$W.aversion16.9, res4$W.aversion16.9, res5$W.aversion16.9) # Risk Aversion = 16.9 weight
colnames(tab_MSR_w) = colnames(tab_GMV_w) = colnames(tab_Aver_w1) = colnames(tab_Aver_w2) = colnames(tab_Aver_w3) = colnames(tab_Aver_w4) = colnames(tab_Aver_w5) = colnames(tab_mean)
rownames(tab_mean)  = c('Unconditional','INF1GRW1','INF2GRW1','INF1GRW2','INF2GRW2')
rownames(tab_sd) =  c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_sr) =  c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_MSR_w) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_GMV_w) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_Aver_w1) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_Aver_w2) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_Aver_w3) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_Aver_w4) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")
rownames(tab_Aver_w5) = c("Unconditional","INF1GRW1", "INF2GRW1","INF1GRW2","INF2GRW2")


