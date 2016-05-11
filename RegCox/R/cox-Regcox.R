#' Learn a Cox model with regularization
#' @param x Input matrix. Each row is an observation vector.
#' @param y Response variable. y should be a two-column matrix with columns named 'time' and 'status'. The latter is a binary variable, with '1' indicating event, and '0' indicating right censored
#' @param t Time variable used for risk set calculation
#' @param regtype Type of regularization to apply {"Lasso", "Enet", "Lapnet", "Kernelnet", "Oscar", "Fear", "Cocktail"}
#' @param nfolds No of folds for cross validation
#' @param lambda1 Additional parameter for Oscar regularization
#' @param lambda2 Additional parameter for Oscar regularization
#' @param alpha ratio between L1 and Laplacian for "Lapnet". This value should be between 0 and 1
#' @param lambda Additional parameter for Fear regularization
#' @param tol Convergence tolerance
#' @param itermax Maximum number of iterations needed for convergence
#' @param weight Weight parameter for Cocktail regularization
#' @export

regcox <- function(x, y, t, regtype, nfolds, lambda1=0, lambda2=0, alpha=0.5, lambda=0, tol=1e-3, itermax, weight=1)
{
  if(regtype == "Lasso")
    return(coxLasso(x, y, t, nfolds))
  if(regtype == "Enet")
    return(coxEnet(x, y, t, nfolds, alpha))
  if(regtype == "Lapnet")
    return(coxLapnet(x, y, t, nfolds, alpha))
  if(regtype == "Kernelnet")
    return(coxKernelnet(x, y, t, nfolds, alpha))
  if(regtype == "Oscar")
    return(regcoxfista(y[,2], x, as.vector(y[,1]), t, y, lambda1, lambda2))
  if(regtype == "Fear")
    return(fear.cmd(x, y, t, nfolds, tol, lambda, itermax))
  if(regtype == "Cocktail")
    return(cocktail.cmd(x, y, t, nfolds, tol, lambda, itermax, alpha, weight))
  else
    print("Unknown regularization type")
}
