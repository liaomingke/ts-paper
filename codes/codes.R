library(rio) # import/export
library(ggplot2)
library(fBasics) # basicStats
library(fUnitRoots)
library(tseries)
library(knitr)
library(tibble)
library(vars)
library(kableExtra)
library(forecast)
library(FinTS)

# 疫情阶段划分
ch = import('./ts-paper/data/ch_diff.csv')
us = import('./ts-paper/data/us_diff.csv')
ggplot(ch,aes(日期,人数,cover=新增,group=新增)) + geom_line()
ggplot(us,aes(日期,人数,cover=新增,group=新增)) + geom_line()

# 数据预处理
CSI = import('./ts-paper/data/CSI300.csv')
DJI = import('./ts-paper/data/DJI.csv')
df = merge(CSI[,c(1,2)], DJI[,c(1,2)], by = '日期')
df$日期 = as.Date(df$日期)
df = df[order(df[,1], decreasing = F),]
df = data.frame(df[1:nrow(df)-1,1],diff(log(df[,2])),diff(log(df[,3]))) # 差分得收益率序列
names(df) = c('date','CSI','DJI')

# 阶段划分
df0 = df[df$date < '2020-01-22',] # 疫情爆发前
df1 = df[df$date < '2021-01-01' & df$date > '2020-01-21',] # 疫情爆发后
export(df0, file = './ts-paper/data/df0.csv')
export(df1, file = './ts-paper/data/df1.csv')

# 描述性统计
des0 = cbind(basicStats(df0[,2]), basicStats(df0[,3])) # 疫情前
names(des0) = c('LRCSI0','LRDJI0')
des1 = cbind(basicStats(df1[,2]), basicStats(df1[,3])) # 疫情后
names(des1) = c('LRCSI1','LRDJI1')
des = cbind(des0, des1)
write.table(des, file = './ts-paper/data/des.csv',sep = ',', quote = F)

# 时序图
par(mfrow=c(2,1))
date = df[,1]
csi = CSI[,2]
lrcsi = df[,2]
plot(date,csi,type='l')
plot(date,lrcsi,type='l')

par(mfrow=c(2,1))
dji = DJI[,2]
lrdji = df[,3]
plot(date,lrdji)

# ADF检验
adf.test(df0[,2])
adf.test(df0[,3])
adf.test(df1[,2])
adf.test(df1[,3])

# 自相关检验
## 疫情前
par(mfrow=c(2,2),mar=c(2,2,2,2))
acf(df0[,2],main="",xlab="滞后期",ylab="ACF") # 自相关图
title(main = "ACF of CSI300",cex.main=0.95)
pacf(df0[,2],main="",xlab="滞后期",ylab="PACF",las=1) # 偏自相关图
title(main="PACF of CSI300",cex.main=0.95)

acf(df0[,3],main="",xlab="滞后期",ylab="ACF") # 自相关图
title(main = "ACF of DJI",cex.main=0.95)
pacf(df0[,3],main="",xlab="滞后期",ylab="PACF",las=1) # 偏自相关图
title(main="PACF of DJI",cex.main=0.95)

## 疫情后
par(mfrow=c(2,2),mar=c(2,2,2,2))
acf(df1[,2],main="",xlab="滞后期",ylab="ACF") # 自相关图
title(main = "ACF of CSI300",cex.main=0.95)
pacf(df1[,2],main="",xlab="滞后期",ylab="PACF",las=1) # 偏自相关图
title(main="PACF of CSI300",cex.main=0.95)

acf(df1[,3],main="",xlab="滞后期",ylab="ACF") # 自相关图
title(main = "ACF of DJI",cex.main=0.95)
pacf(df1[,3],main="",xlab="滞后期",ylab="PACF",las=1) # 偏自相关图
title(main="PACF of DJI",cex.main=0.95)

# ARMA定阶
auto.arima(df0[,2])
auto.arima(df0[,3])
auto.arima(df1[,2])
auto.arima(df1[,3])
csi0.fit = arima(df0[,2],order=c(1,0,1))
dji0.fit = arima(df0[,3],order=c(1,0,0))
csi1.fit = arima(df1[,2],order=c(2,0,2))
dji1.fit = arima(df1[,3],order=c(0,0,2))

# 残差序列白噪声检验
Box.test(csi0.fit$residual,lag=6,type='Ljung-Box')
Box.test(dji0.fit$residual,lag=6,type='Ljung-Box')
Box.test(csi1.fit$residual,lag=6,type='Ljung-Box')
Box.test(dji1.fit$residual,lag=6,type='Ljung-Box')

win.graph(width=6,height=6)
tsdiag(csi0.fit,gof=6,omit.initial=F)
win.graph(width=6,height=6)
tsdiag(dji0.fit,gof=6,omit.initial=F)
win.graph(width=6,height=6)
tsdiag(csi1.fit,gof=6,omit.initial=F)
win.graph(width=6,height=6)
tsdiag(dji1.fit,gof=6,omit.initial=F)

qqnorm(csi0.fit$residuals)
qqline(csi0.fit$residuals)
qqnorm(dji0.fit$residuals)
qqline(dji0.fit$residuals)
qqnorm(csi1.fit$residuals)
qqline(csi1.fit$residuals)
qqnorm(dji1.fit$residuals)
qqline(dji1.fit$residuals)

# 残差图
res_csi0 = residuals(csi0.fit)
res_dji0 = residuals(dji0.fit)
res_csi1 = residuals(csi1.fit)
res_dji1 = residuals(dji1.fit)

par(mfrow=c(2,2),mar=c(2,2,2,2)) 
plot(df0[,1],res_csi0,type="l",xlab="日期",ylab="残差",cex.main=0.95,las=1)
plot(df0[,1],res_dji0,type="l",xlab="日期",ylab="残差",cex.main=0.95,las=1)
plot(df1[,1],res_csi1,type="l",xlab="日期",ylab="残差",cex.main=0.95,las=1)
plot(df1[,1],res_dji1,type="l",xlab="日期",ylab="残差",cex.main=0.95,las=1)

# ARCH效应检验
ArchTest(res_csi0,lag=12)
ArchTest(res_dji0,lag=12)
ArchTest(res_csi1,lag=12)
ArchTest(res_dji1,lag=12)

# GARCH(1,1)
rcsi0.fit = garch(csi0.fit$residual,order=c(1,1))
summary(rcsi0.fit)
rcsi0.pred = predict(rcsi0.fit)
plot(rcsi0.pred)

rdji0.fit = garch(dji0.fit$residual,order=c(1,1))
summary(rdji0.fit)
rdji0.pred = predict(rdji0.fit)
plot(rdji0.pred)

rcsi1.fit = garch(csi1.fit$residual,order=c(1,1))
summary(rcsi1.fit)
rcsi1.pred = predict(rcsi1.fit)
plot(rcsi1.pred)

rdji1.fit = garch(dji1.fit$residual,order=c(1,1))
summary(rdji1.fit)
rdji1.pred = predict(rdji1.fit)
plot(rdji1.pred)

## 正态分布
library(fGarch, quietly = TRUE)
mod1 = garchFit(~ 1 + garch(1,1), data=df0[,2], trace=FALSE)
summary(mod1)
mod1 = garchFit(~ 1 + garch(1,1), data=df0[,3], trace=FALSE)
summary(mod1)
mod1 = garchFit(~ 1 + garch(1,1), data=df1[,2], trace=FALSE)
summary(mod1)
mod1 = garchFit(~ 1 + garch(1,1), data=df1[,3], trace=FALSE)
summary(mod1)

vola = volatility(mod1)
hatmu = coef(mod1)["mu"]
lb.csi0 = hatmu - 2*vola
ub.csi0 = hatmu + 2*vola
ylim = range(c(df0[,2], lb.csi0, ub.csi0))
x.csi0 = c(time(df0[,2]))
plot(x.csi0, c(df0[,2]), type="l",
xlab=" 日", ylab=" 对数收益率")
lines(x.csi0, c(lb.csi0), col="red")
lines(x.csi0, c(ub.csi0), col="red")

## t分布
mod2 = garchFit(~ 1 + garch(1,1), data=df0[,2],cond.dist="std", trace=FALSE)
summary(mod2)
mod2 = garchFit(~ 1 + garch(1,1), data=df0[,3],cond.dist="std", trace=FALSE)
summary(mod2)
mod2 = garchFit(~ 1 + garch(1,1), data=df1[,2],cond.dist="std", trace=FALSE)
summary(mod2)
mod2 = garchFit(~ 1 + garch(1,1), data=df1[,3],cond.dist="std", trace=FALSE)
summary(mod2)

## 有偏t分布
mod3 = garchFit(~ 1 + garch(1,1), data=df0[,2],cond.dist="sstd", trace=FALSE)
summary(mod3)
mod3 = garchFit(~ 1 + garch(1,1), data=df0[,3],cond.dist="sstd", trace=FALSE)
summary(mod3)
mod3 = garchFit(~ 1 + garch(1,1), data=df1[,2],cond.dist="sstd", trace=FALSE)
summary(mod3)
mod3 = garchFit(~ 1 + garch(1,1), data=df1[,3],cond.dist="sstd", trace=FALSE)
summary(mod3)

# EGARCH(1,1)
Egarch(c(df0[,2]))
Egarch(c(df0[,3]))
Egarch(c(df1[,2]))
Egarch(c(df1[,3]))