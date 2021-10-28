# 10.28 大学院講義

#==================================================
# 失敗例　おし！！！
url="https://raw.githubusercontent.com/snagatas/stat/main/pmi_jis.csv"
dat = read.csv(url)
head(dat) # check the properties of data

dat2=dat[, c("cond","pmi","reaction")];
head(dat2) # check the properties of data

names(df) <- c("age", "height", "weight")
dat2=

# varX=dat$cond;varM=dat$pmi;varY=dat$reaction;

install.packages("plyr");
install.packages("mediation");

library(plyr)
library(mediation)


set.seed(12345)
mx= lm(pmi ~ cond, data=dat2)
yx = lm(reaction~ cond + pmi, data=dat2)

medi_result= mediate(mx, yx, treat="cond", madiator="pmi", boot=T,sims=1000)

summary(medi_result); plot(mdi_result)


#==================================================
# 成功例
#https://msmsfjsw.netlify.app/2021/01/r%E3%81%AB%E3%82%88%E3%82%8B%E5%9B%A0%E6%9E%9C%E5%AA%92%E4%BB%8B%E5%88%86%E6%9E%901-baron-kenny%E6%B3%95/
library(mediation)
data("jobs")
model.m <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
model.y <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
m.out <- mediate(model.m, model.y, sims=100, treat="treat", mediator="job_seek")
summary(m.out)
