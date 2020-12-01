library(tidyverse)
##导入文件
bm.all <- read.csv(file = "Metadata of breastmilk questionnaire.csv",stringsAsFactors = F)
bm.all$id <- as.numeric(bm.all$id)

bm.all$mon_birth <- as.Date(paste(bm.all$A2A,bm.all$A2B,
                                  bm.all$A2C,sep="-"))#乳母出生日期
bm.all$baby_birth<- as.Date(paste(bm.all$A5A,
                                  bm.all$A5B,bm.all$A5C,sep="-"))
bm.all$sample_date <- as.Date(bm.all$N1,
                              format="%d-%m-%Y")#样本采集日期

bm.all <- bm.all %>%
  mutate(age_mon=(sample_date-mon_birth)/365,
         age_baby=(sample_date-baby_birth))
bm.all$age_baby <- as.numeric(bm.all$age_baby)
bm.all$age_mon <- as.numeric(bm.all$age_mon)

#去除一部分月龄有问题的样本：负数
bm.all <- bm.all %>%
  filter(age_baby>0 & age_baby<=180)
#1月龄
bm.m1 <- bm.all %>%
  filter(age_baby<=30)
bm.m2 <- bm.all %>%
  filter(age_baby >=31 & age_baby <=60)
bm.m3 <- bm.all %>%
  filter(age_baby %in% c(61:90))
bm.m4 <- bm.all %>%
  filter(age_baby >=91 & age_baby <=120)
bm.m5 <- bm.all %>%
  filter(age_baby >=121 & age_baby <=150)
bm.m6 <- bm.all %>%
  filter(age_baby>=151 & age_baby<=180)
#先看1月龄的样本
summary(bm.m1$TB1)
summary(bm.m1$TB3)
boxplot(bm.m1$TB3)#去掉两个异常点
bm.m1 <-  bm.m1 %>%
  filter(TB3>1 & TB3<10)
summary(bm.m1$TB6)
boxplot(bm.m1$TA3)#乳母体重
boxplot(bm.m1$TA1)#乳母身高
bm.m1$MonBMI <- bm.m1$TA3/((bm.m1$TA1/100)^2)
summary(bm.m1$MonBMI)
#1月龄z评分
library(zscorer)
bm.m1 <- addWGSR(data = bm.m1, sex = "A4", firstPart = "TB3", 
                  secondPart = "TB1", thirdPart = "age_baby", index = "bfa")#A4为婴儿性别

bm.m1 <- addWGSR(data = bm.m1, sex = "A4", firstPart = "TB3",
                  secondPart = "age_baby", index = "wfa")

bm.m1 <- addWGSR(data = bm.m1, sex = "A4", firstPart = "TB1",
                  secondPart = "age_baby", index = "hfa")
summary(bm.m1$wfaz)
#看2月龄的样本
summary(bm.m2$TB1)
boxplot(bm.m2$TB1)
summary(bm.m2$TB3)
boxplot(bm.m2$TB3)
bm.m2 <-  bm.m2 %>%
  filter(TB1>10)#去掉1个婴儿身高异常点
bm.m2 <-  bm.m2 %>%
  filter(TB3<20)
summary(bm.m2$TB6)
boxplot(bm.m2$TA3)#乳母体重
#处理一下体重有问题的数据
#思路:保守一些，把大于100kg的都设为缺失值,根据boxplot
#数据质量不高，cutoff的选择比较难
bm.m2$TA3[bm.m2$TA3>=100] <- NA
bm.m2 <- bm.m2 %>%
  filter(TA3<=82)

boxplot(bm.m2$TA1)#乳母身高分布正常
bm.m2$MonBMI <- bm.m2$TA3/((bm.m2$TA1/100)^2)
summary(bm.m2$MonBMI)
boxplot(bm.m2$MonBMI)
length(bm.m2$MonBMI[bm.m2$MonBMI>27.5])
length(bm.m2$MonBMI[bm.m2$MonBMI>=18.5 & bm.m2$MonBMI<=23])
length(bm.m2$MonBMI[bm.m2$MonBMI>23])
#2月龄z评分
library(zscorer)
boxplot(bm.m2$TB3)
boxplot(bm.m2$TB1)
bm.m2 <- addWGSR(data = bm.m2, sex = "A4", firstPart = "TB3", 
                 secondPart = "TB1", thirdPart = "age_baby", index = "bfa")#A4为婴儿性别

bm.m2 <- addWGSR(data = bm.m2, sex = "A4", firstPart = "TB3",
                 secondPart = "age_baby", index = "wfa")

bm.m2 <- addWGSR(data = bm.m2, sex = "A4", firstPart = "TB1",
                 secondPart = "age_baby", index = "hfa")
boxplot(bm.m2$bfaz)
boxplot(bm.m2$hfaz)
boxplot(bm.m2$wfaz)

length(bm.m2$bfaz[bm.m2$bfaz<=-1]) # 60
length(bm.m2$bfaz[bm.m2$bfaz>=1]) # 110
length(bm.m2$bfaz[bm.m2$wfaz>=1]) # 104
length(bm.m2$bfaz[bm.m2$wfaz<=-1]) #57
length(bm.m2$bfaz[bm.m2$hfaz>=2]) # 100
length(bm.m2$bfaz[bm.m2$hfaz<=-2]) # 96

# 对2月的样本进行初步筛选
# 先把乳母BMI>=18.5的选出来
bm.m2MonBMI <- bm.m2 %>%
  filter(MonBMI>=18.5)
write.csv(bm.m2MonBMI,file = "questionaire data for month 2.csv",
          row.names = F)
#1为肥胖，2为正常，3为超重
bm.m2MonBMI <- bm.m2MonBMI %>%
  mutate(nBMI=case_when(MonBMI>27.5~1,
                        MonBMI>=18.5 & MonBMI<=23~2,
                        MonBMI>23 & MonBMI<=27.5~3))
table(bm.m2MonBMI$nBMI)
#去掉hfaz的极端值：绝对值大于5
bm.m2MonBMI <- bm.m2MonBMI %>%
  filter(abs(hfaz)<=5)
#生成hfaz四分位
bm.m2MonBMI <- bm.m2MonBMI %>%
  filter(city %in% c("北京",
                 "成都",
                 "广州",
                 "金华",
                 "兰州",
                 "威海",
                 "郑州"))
bm.m2MonBMI$nhfaz <- ntile(bm.m2MonBMI$hfaz,4)
table(bm.m2MonBMI$nhfaz)
bm.m2Head <- bm.m2MonBMI %>%
  arrange(desc(MonBMI)) %>%
  slice(c(1:50,324:373))
table(bm.m2Head$nhfaz)
table(bm.m2Head$B401)
length(bm.m2Head$hfaz[bm.m2Head$hfaz<=-1])
length(bm.m2Head$hfaz[bm.m2Head$hfaz>=1])


bm.m2Yibai <- bm.m2MonBMI %>%
  arrange(desc(MonBMI)) %>%
  slice(c(1:60,314:373))
table(bm.m2Yibai$city)

write.csv(bm.m2Yibai,file = "month 2 samples for metabolome.csv")
table(bm.m2Yibai$nhfaz)
length(bm.m2Yibai$hfaz[bm.m2Yibai$hfaz<=-1])
length(bm.m2Yibai$hfaz[bm.m2Yibai$hfaz>=1])

#nBMI=1
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==1 & bm.m2MonBMI$nhfaz==1]) #4
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==1 & bm.m2MonBMI$nhfaz==4]) #8
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==2 & bm.m2MonBMI$nhfaz==1]) #62
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==2 & bm.m2MonBMI$nhfaz==4]) #47
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==3 & bm.m2MonBMI$nhfaz==1]) #28
length(bm.m2MonBMI$id[bm.m2MonBMI$nBMI==3 & bm.m2MonBMI$nhfaz==4]) #38



length(bm.m2MonBMI$bfaz[bm.m2MonBMI$bfaz<=-1]) # 59
length(bm.m2MonBMI$bfaz[bm.m2MonBMI$bfaz>=1]) # 99
length(bm.m2MonBMI$bfaz[bm.m2MonBMI$wfaz>=1]) # 97
length(bm.m2MonBMI$bfaz[bm.m2MonBMI$wfaz<=-1]) #54
length(bm.m2MonBMI$bfaz[bm.m2MonBMI$hfaz>=1]) # 97
length(bm.m2MonBMI$bfaz[bm.m2MonBMI$hfaz<=-1]) # 91

bm.m2Zscore <- bm.m2MonBMI %>%
  filter(bfaz>=1 | bfaz<=-1 | MonBMI>27.5)

length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>27.5])
length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>=18.5 & bm.m2Zscore$MonBMI<=23])
length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>23])

##################
##################
#6月龄的数据

summary(bm.m6$TB1)
boxplot(bm.m6$TB1)
#有两个值明显偏低，去掉
bm.m6 <- bm.m6 %>%
  filter(TB1>30)
summary(bm.m6$TB3)
boxplot(bm.m6$TB3)
bm.m6 <-  bm.m6 %>%
  filter(TB3<30 & TB3 >=5)#去掉4个婴儿体重异常点

boxplot(bm.m6$TA3)#乳母体重
#处理一下体重有问题的数据
#思路:保守一些，把大于100kg的都设为缺失值,根据boxplot
#数据质量不高，cutoff的选择比较难
bm.m6 <- bm.m6 %>%
  filter(TA3<=90)

boxplot(bm.m6$TA1)#乳母身高分布正常
bm.m6$MonBMI <- bm.m6$TA3/((bm.m6$TA1/100)^2)
summary(bm.m6$MonBMI)
boxplot(bm.m6$MonBMI)
length(bm.m6$MonBMI[bm.m6$MonBMI>27.5]) # 33
length(bm.m6$MonBMI[bm.m6$MonBMI>=18.5 & bm.m6$MonBMI<=23]) # 126
length(bm.m6$MonBMI[bm.m6$MonBMI>23]) # 119
#6月龄z评分
library(zscorer)
boxplot(bm.m6$TB3)
boxplot(bm.m6$TB1)
bm.m6 <- addWGSR(data = bm.m6, sex = "A4", firstPart = "TB3", 
                 secondPart = "TB1", thirdPart = "age_baby", index = "bfa")#A4为婴儿性别

bm.m6 <- addWGSR(data = bm.m6, sex = "A4", firstPart = "TB3",
                 secondPart = "age_baby", index = "wfa")

bm.m6 <- addWGSR(data = bm.m6, sex = "A4", firstPart = "TB1",
                 secondPart = "age_baby", index = "hfa")

length(bm.m6$bfaz[bm.m6$bfaz<=-1]) # 41
length(bm.m6$bfaz[bm.m6$bfaz>=1]) # 85
length(bm.m6$bfaz[bm.m6$wfaz>=1]) # 105
length(bm.m6$bfaz[bm.m6$wfaz<=-1]) #16
length(bm.m6$bfaz[bm.m6$hfaz>=1]) # 99
length(bm.m6$bfaz[bm.m6$hfaz<=-1]) # 39

# 对6月的样本进行初步筛选
# 先把乳母BMI>=18.5的选出来
bm.m6MonBMI <- bm.m6 %>%
  filter(MonBMI>=18.5)

#1为肥胖，2为正常，3为超重
bm.m6MonBMI <- bm.m6MonBMI %>%
  mutate(nBMI=case_when(MonBMI>27.5~1,
                        MonBMI>=18.5 & MonBMI<=23~2,
                        MonBMI>23 & MonBMI<=27.5~3))
table(bm.m6MonBMI$nBMI)
#去掉hfaz的极端值：绝对值大于5
bm.m6MonBMI <- bm.m6MonBMI %>%
  filter(abs(hfaz)<=5)
#生成hfaz四分位
bm.m6MonBMI$nhfaz <- ntile(bm.m6MonBMI$hfaz,4)
table(bm.m6MonBMI$nhfaz)

bm.m6Head <- bm.m6MonBMI %>%
  arrange(desc(MonBMI)) %>%
  slice(c(1:60,182:241))
write.csv(bm.m6Head,file = "month 6 samples for metabolome.csv",
          row.names = F)

#nBMI=1
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==1 & bm.m6MonBMI$nhfaz==1]) #5
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==1 & bm.m6MonBMI$nhfaz==4]) #9
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==2 & bm.m6MonBMI$nhfaz==1]) #33
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==2 & bm.m6MonBMI$nhfaz==4]) #28
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==3 & bm.m6MonBMI$nhfaz==1]) #23
length(bm.m6MonBMI$id[bm.m6MonBMI$nBMI==3 & bm.m6MonBMI$nhfaz==4]) #23





length(bm.m6MonBMI$bfaz[bm.m6MonBMI$bfaz<=-1]) # 40
length(bm.m6MonBMI$bfaz[bm.m6MonBMI$bfaz>=1]) # 82
length(bm.m6MonBMI$bfaz[bm.m6MonBMI$wfaz>=1]) # 101
length(bm.m6MonBMI$bfaz[bm.m6MonBMI$wfaz<=-1]) # 16
length(bm.m6MonBMI$bfaz[bm.m6MonBMI$hfaz>=1]) # 97
length(bm.m6MonBMI$bfaz[bm.m6MonBMI$hfaz<=-1]) # 36

bm.m2Zscore <- bm.m2MonBMI %>%
  filter(bfaz>=1 | bfaz<=-1 | MonBMI>27.5)

length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>27.5])
length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>=18.5 & bm.m2Zscore$MonBMI<=23])
length(bm.m2Zscore$MonBMI[bm.m2Zscore$MonBMI>23])
