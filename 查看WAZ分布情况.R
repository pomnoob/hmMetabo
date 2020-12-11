# 还需要看一下婴儿体重z评分的情况
# 因为目前还无法确定是用身高还是体重评估婴儿生长发育更好

library(tidyverse)

# 二月龄所有数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m2MonBMI.Rdata")
# 二月龄母亲BMI筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m2MBMI.Rdata")
# 二月龄LAZ筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m2Izs.Rdata")

# 查看二月龄所有样本中WAZ大于1或者小于-1的样本量
length(bm.m2MonBMI$wfaz[bm.m2MonBMI$wfaz>1]) # 67
length(bm.m2MonBMI$wfaz[bm.m2MonBMI$wfaz<-1]) # 1

# 以母亲BMI筛选数据，看一下WAZ大于1或者小于-1的样本量
length(bm.m2MBMI$wfaz[bm.m2MBMI$wfaz>1]) # 32
length(bm.m2MBMI$wfaz[bm.m2MBMI$wfaz<-1]) # 1

# 以LAZ筛选数据，看一下WAZ大于1或者小于-1的样本量
length(bm.m2Izs$wfaz[bm.m2Izs$wfaz>1]) # 42
length(bm.m2Izs$wfaz[bm.m2Izs$wfaz<-1]) # 1

# TODO: 二月龄样本中WAZ小于-1的样本量只有1个

###############################################################################

# 6月龄所有数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6MonBMI.Rdata")
# 6月龄母亲BMI筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6MBMI.Rdata")
# 6月龄LAZ筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6Izs.Rdata")

# 查看6月龄所有样本中WAZ大于1或者小于-1的样本量
length(bm.m6MonBMI$wfaz[bm.m6MonBMI$wfaz>1]) # 73
length(bm.m6MonBMI$wfaz[bm.m6MonBMI$wfaz<-1]) # 1

# 以母亲BMI筛选数据，看一下WAZ大于1或者小于-1的样本量
length(bm.m6MBMI$wfaz[bm.m6MBMI$wfaz>1]) # 48
length(bm.m6MBMI$wfaz[bm.m6MBMI$wfaz<-1]) # 1

# 以LAZ筛选数据，看一下WAZ大于1或者小于-1的样本量
length(bm.m6Izs$wfaz[bm.m6Izs$wfaz>1]) # 51
length(bm.m6Izs$wfaz[bm.m6Izs$wfaz<-1]) # 1

# TODO: WAZ小于-1的样本量只有1个