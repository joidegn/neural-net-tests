library(neuralnet)
data(infert)

#infert.nn <- data.frame(case=as.numeric(infert$case), education=as.numeric(infert$education), age=infert$age, induced=infert$induced, spontaneous=infert$spontaneous, stratum=infert$stratum)
nn.model <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2, linear.output=FALSE, algorithm="backprop", err.fct="ce", act.fct="logistic", learningrate=0.01)
nn.model2 <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2, err.fct="ce", linear.output=FALSE, act.fct="logistic")

plot(nn.model2)
gwplot(nn.model2, selected.covariate="age")

prediction <- compute(nn.model2, covariate=matrix(c(22, 1, 1, 0), byrow=T, nrow=1))
# prediction should be probability that a 22 year old of parity 1 with 1 induced abortion and 0 spontaneous abortions is infertile
