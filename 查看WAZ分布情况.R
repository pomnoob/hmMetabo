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
wfaz2_1 <- bm.m2MonBMI %>%
  filter(wfaz < -1)
wfaz2_2 <- bm.m2MonBMI %>%
  filter(wfaz > 1)


# 以母亲BMI筛选数据，看一下WAZ大于1或者小于-1的样本量
wfaz3_1 <- bm.m2MBMI %>%
  filter(wfaz < -1)
wfaz3_2 <- bm.m2MBMI %>%
  filter(wfaz > 1)


# 以LAZ筛选数据，看一下WAZ大于1或者小于-1的样本量
wfaz4_1 <- bm.m2Izs %>%
  filter(wfaz < -1)
wfaz4_2 <- bm.m2Izs %>%
  filter(wfaz > 1)

# TODO: 二月龄样本中WAZ小于-1的样本量只有1个

###############################################################################

# 6月龄所有数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6MonBMI.Rdata")
# 6月龄母亲BMI筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6MBMI.Rdata")
# 6月龄LAZ筛选数据
load("D:/Rdata/hmMetabo/hmMetabo/data/bm.m6Izs.Rdata")

# 查看6月龄所有样本中WAZ大于1或者小于-1的样本量
wfaz5_1 <- bm.m6MonBMI %>%
  filter(wfaz < -1)
wfaz5_2 <- bm.m6MonBMI %>%
  filter(wfaz > 1)

# 以母亲BMI筛选数据，看一下WAZ大于1或者小于-1的样本量
wfaz6_1 <- bm.m6MBMI %>%
  filter(wfaz < -1)
wfaz6_2 <- bm.m6MBMI %>%
  filter(wfaz > 1)

# 以LAZ筛选数据，看一下WAZ大于1或者小于-1的样本量
wfaz7_1 <- bm.m6Izs %>%
  filter(wfaz < -1)
wfaz7_2 <- bm.m6Izs %>%
  filter(wfaz > 1)

# TODO: WAZ小于-1的样本量只有1个