####### Chapter 4 #######

#####  German Credit
library(caret)
data(GermanCredit)
german = GermanCredit
str(german)
german$Class = ifelse(GermanCredit$Class=="Bad",0,1)

tr.ind = sample(1000, 700) #n=1000
tr.german = german[tr.ind,] #70% of data training set
ts.german = german[-tr.ind,] #30% of data test set

#####  Forward selection by AIC
lm.const = lm(Class~1, data=tr.german)
glm.const = glm(Class~1, data=tr.german, family=binomial)
fmla = as.formula(paste("Class"," ~ ",paste(colnames(tr.german)
	[-ncol(tr.german)],collapse="+")))
lm.aic = step(lm.const, fmla, direction="forward", trace=FALSE)
glm.aic = step(glm.const, fmla, direction="forward", trace=FALSE)
summary(lm.aic)
summary(glm.aic)

##### confusion matrix
pred.lm = predict(lm.aic)
pred = rep(0,length(pred.lm))
pred[pred.lm>=0.5] = 1
table(true=tr.german$Class, pred)

pred.glm = predict(glm.aic,type="response")
pred = rep(0, length(pred.glm))
pred[pred.glm>=0.5] = 1
table(true=tr.german$Class, pred)

##### ROC curve
library(ROCR)
ROC.lm = performance(prediction(pred.lm, tr.german$Class),
	 "tpr","fpr")
ROC.glm = performance(prediction(pred.glm, tr.german$Class),
	 "tpr","fpr")
plot(ROC.lm, main="ROC curve for linear regression")
x = unlist(attr(ROC.glm, "x.values"))
y = unlist(attr(ROC.glm, "y.values"))
lines(x, y, lty=2, col=2)
legend('bottomright', legend=c("linear","logistic"),
	lty=1:2, col=1:2)

##### Lift chart

# function
fun.lift = function(xx, K)
{
    num.class = round(nrow(xx)/K)
    base.lift = sum(xx[,1])/nrow(xx)
    class.lift = list();output = c()
    for(i in 1:K)
    {
        if(i!=K) {
            class.lift[[i]] = seq((1+num.class*(i-1)), num.class*i) 
        } else
            class.lift[[i]] = seq((1+num.class*(i-1)),nrow(xx))
        CR = 100*sum(xx[class.lift[[i]],1])/sum(xx[,1])
        resp = sum(xx[class.lift[[i]],1])/length(class.lift[[i]])
        lift = resp/base.lift
        output = rbind(output, c(CR, resp, lift))
    }
    colnames(output) = c("%Captured response","%Response","Lift")
    return(list(base.lift=base.lift,num.class=K,lift.chart=output))
}

# lift table
sort.pred.lm = sort(pred.lm,decreasing=T)
sort.tr.german = tr.german$Class[order(pred.lm,decreasing=T)]
lm.lift.data = cbind(sort.tr.german, sort.pred.lm) #First column: observed Y
lm.lift = fun.lift(lm.lift.data,10)
lm.lift

sort.pred.glm = sort(pred.glm, decreasing=T)
sort.tr.german = tr.german$Class[order(pred.glm, decreasing=T)]
glm.lift.data = cbind(sort.tr.german, sort.pred.glm)
glm.lift = fun.lift(glm.lift.data,10)
glm.lift

# lift chart
plot(lm.lift$lift.chart[,3], type="o",ylab="Lift",
	xlab="Class",pch=7,lty=2,col="red")
lines(glm.lift$lift.char[,3], type="o",pch=7,lty=2,
	col="blue")
legend('topright', legend=c("linear","logistic"),lty=2,
	col=c(2,4), pch=7 )
