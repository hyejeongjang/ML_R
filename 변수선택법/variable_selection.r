### ������ �ҷ�����
library(ElemStatLearn)
Data = prostate
str(Data)
data.use = Data[,-ncol(Data)]

### Linear Regression : ��� �������� �� ����ȸ�͸� �����. 
lm.full = lm(lpsa~., data=data.use)
par(mfrow=c(2,2))
plot(lm.full)

#### AIC�� �̿��� �������Ź�
backward.aic = step(lm.full, lpsa~1, direction="backward")
summary(backward.aic)

#### BIC�� �̿��� �������ù�
backward.bic = step(lm.full, lpsa~1, direction="backward", k=log(nrow(data.use)))
summary(backward.bic)

### Logistic Regression

#### ������ �ҷ�����
library(MASS)
library(mlbench)
data(Sonar)
y.tmp = rep(1,nrow(Sonar))
y.tmp[which(Sonar[,61]=="R")] = 0
Sonar[,61] = y.tmp

#### AIC�� �̿��� �������ù�
glm.const = glm(Class~1, data=Sonar,family=binomial)
fmla = as.formula(paste("Class"," ~ ",paste(colnames(Sonar)[-ncol(Sonar)],
                                            collapse="+")))
forward.aic = step(glm.const, fmla, direction="forward")
#summary(forward.aic)
forward.aic

#### BIC�� �̿��� �������ù�
glm.const = glm(Class~1, data=Sonar,family=binomial)
fmla = as.formula(paste("Class"," ~ ",paste(colnames(Sonar)[-ncol(Sonar)],
                                            collapse="+")))
forward.bic = step(glm.const, fmla, direction="forward", k=log(nrow(Sonar)))
summary(forward.bic)

### �������ù�, 5-cross validation �̿�
library(lars)
cv.log = function(yy, xx, K=5)
{
  cverr = rep(0,K)
  folder = cv.folds(length(yy),K)
  for(k in 1:K)
  {
    xx = as.matrix(xx)
    gg = glm(yy[-folder[[k]]]~xx[-folder[[k]],],family=binomial)
    pyy = cbind(1,xx[folder[[k]],])%*%gg$coef
    pyy = exp(pyy)/(1+exp(pyy))
    po = which(pyy>=0.5);pyy[po]=1;pyy[-po]=0
    cverr[k] = sum(abs(pyy-yy[folder[[k]]]))/length(yy[folder[[k]]])
  }
  return(cverr)
}

select.var = c();full.var = colnames(Sonar)[-ncol(Sonar)]
cv.err = 1
for(i in 1:60)
{
  cv.err.tmp = c()
  for(j in 1:length(full.var))
  {
    cv.err.tmp[j] = mean(cv.log(Sonar[,61],Sonar[,c(select.var,full.var[j])]),K=10)
  }
  select.var[i] = full.var[which.min(cv.err.tmp)]
  full.var = full.var[-which.min(cv.err.tmp)]
  print(select.var);print(min(cv.err.tmp))
  if(cv.err<=min(cv.err.tmp))
    break
  cv.err = min(cv.err.tmp)
}