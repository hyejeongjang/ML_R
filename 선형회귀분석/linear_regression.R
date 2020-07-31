### 데이터 불러오기
#install.packages("MASS")
library(MASS)
data=Boston

### 단순선형회귀
lm.bh=lm(medv~., data=data)
summary(lm.bh)
anova(lm.bh)

### PLOT
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm.bh)

### 로지스틱회귀모형
library(boot)
data(nodal)
rd = nodal[,-1]
gfit = glm(r~., data=rd, family="binomial")
summary(gfit)
