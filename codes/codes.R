library(rio) # import/export
library(ggplot2)
library(fBasics) # basicStats
library(fUnitRoots)
library(tseries)
library(knitr)
library(tibble)
library(vars)
library(kableExtra)

# 数据预处理
CSI = import('./ts-paper/data/CSI300.csv')
HSI = import('./ts-paper/data/HSI.csv')
DJI = import('./ts-paper/data/DJI.csv')
df = merge(CSI[,c(1,2)], HSI[,c(1,2)], by = '日期')
df = merge(df, DJI[,c(1,2)], by = '日期')
df$日期 = as.Date(df$日期)
df = df[order(df[,1], decreasing = F),]
df = data.frame(df[1:nrow(df)-1,1],diff(log(df[,2])),diff(log(df[,3])),diff(log(df[,4]))) # 差分得收益率序列
names(df) = c('date','CSI','HSI','DJI')

# 阶段划分
df0 = df[df$date < '2020-01-22',] # 疫情爆发前
df1 = df[df$date < '2021-01-01' & df$date > '2020-01-21',] # 疫情爆发后
export(df0, file = './ts-paper/data/df0.csv')
export(df1, file = './ts-paper/data/df1.csv')

# 描述性统计
des0 = cbind(basicStats(df0[,2]), basicStats(df0[,3]), basicStats(df0[,4])) # 疫情前
names(des0) = c('LRCSI0','LRHSI0','LRDJI0')
des1 = cbind(basicStats(df1[,2]), basicStats(df1[,3]), basicStats(df1[,4])) # 疫情后
names(des1) = c('LRCSI1','LRHSI1','LRDJI1')
des = cbind(des0, des1)
write.table(des, file = './ts-paper/data/des.csv',sep = ',', quote = F)
