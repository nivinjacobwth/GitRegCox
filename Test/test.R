install_github("nivinjacobwth/RegCox")
library(RegCox)

train_data <- read.csv("synthetic1-survival_norm.csv")
#train_data <- read.csv("breast_norm.csv")


train_time <- train_data$time
train_event <-train_data$event
x <- as.matrix(train_data[,3:ncol(train_data)])
y <- as.matrix(cbind(time=train_time,status=train_event))
t <- mean(train_data$time)

fit <- regcox(x, y, t, "Lasso",nfolds=5, stdbeta=F, standardize=F)
fit <- regcox(x, y, t, "Enet",nfolds=5 , alpha=0.1, stdbeta=F, standardize=F)
fit <- regcox(x, y, t, "Lapnet", nfolds=5, alpha=0.1, stdbeta=F, standardize=F)
fit <- regcox(x, y, t, "Kernelnet", nfolds=5, alpha=0.1, stdbeta=F, standardize=F)
fit <- regcox(x, y, t, "Oscar", nfolds=5, lambda1=0.01, lambda2=0.01, stdbeta=T, standardize=F)
fit <- regcox(x, y, t, "Fear", nfolds=5, tol=1e-3, lambda=0.1, itermax=100, stdbeta=T, standardize=F)
fit <- regcox(x, y, t, "Cocktail", nfolds=5, tol=1e-3, lambda=0.1, itermax=100,alpha=.5, weight=1, stdbeta=T, standardize=F)

filename = "synthetic1-survival_norm.csv"
auc <- arc(filename, regtype="Lasso", metric="AUC", nfolds=5, standardize=T, alpha=.5, lambda=.01 )
print(auc)
auc <- arc(filename, regtype="Enet", metric="AUC", nfolds=5, alpha=.5, lambda=.01, standardize=T)
print(auc)
auc <- arc(filename, regtype="Lapnet", metric="AUC", nfolds=5, alpha=.5, lambda=.1, standardize=T )
print(auc)
auc <- arc(filename, regtype="Kernelnet", metric="AUC", nfolds=5, alpha=.5, lambda=.01, standardize=T )
print(auc)

mse <- arc(filename, regtype="Lasso", metric="MSE", nfolds=5, alpha=.5, lambda=.01, standardize=F )
print(mse)
mse <- arc(filename, regtype="Enet", metric="MSE", nfolds=5, alpha=.5, lambda=.01, standardize=T )
print(mse)
mse <- arc(filename, regtype="Lapnet", metric="MSE", nfolds=5, alpha=.5, lambda=.01, standardize=T )
print(mse)
mse <- arc(filename, regtype="Kernelnet", metric="MSE", nfolds=5, alpha=.5, lambda=.01, standardize=T )
print(mse)

filename = "breast_norm.csv"
auc <- arc(filename, regtype="Lasso", metric="AUC", nfolds=5, alpha=.5, lambda=.01 )
print(auc)
auc <- arc(filename, regtype="Enet", metric="AUC", nfolds=5, alpha=.5, lambda=.01 )
print(auc)
auc <- arc(filename, regtype="Lapnet", metric="AUC", nfolds=5, alpha=.5, lambda=.2 )
print(auc)
auc <- arc(filename, regtype="Kernelnet", metric="AUC", nfolds=5, alpha=.5, lambda=.01 )
print(auc)

mse <- arc(filename, regtype="Lasso", metric="MSE", nfolds=5, alpha=.5, lambda=.01 )
print(mse)
mse <- arc(filename, regtype="Enet", metric="MSE", nfolds=5, alpha=.5, lambda=.01 )
print(mse)
mse <- arc(filename, regtype="Lapnet", metric="MSE", nfolds=5, alpha=.5, lambda=.01 )
print(mse)
mse <- arc(filename, regtype="Kernelnet", metric="MSE", nfolds=5, alpha=.5, lambda=.01 )
print(mse)