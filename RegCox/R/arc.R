#' Active learning using regularized cox models
#' @param filename Input matrix. Each row is an observation vector.
#' @param regtype Type of regularization to apply {"Lasso", "Enet", "Lapnet", "Kernelnet"}
#' @param metric Type of evaluation metric to fetch {"AUC", "MSE"}
#' @param nfolds Number of folds for cross validation
#' @param alpha ratio between L1 and Laplacian for "Lapnet", or between L1 and L2 for "Enet". Not needed for "Lasso". Value should be between 0 and 1
#' @param lambda a user supplied decreasing sequence. can range from 0.01 to 1
#' @export

arc <- function(filename, regtype, metric, nfolds, alpha, lambda)
{

if(metric == "AUC"){
  if(regtype == "Lasso")
    return(arc.lasso.auc(filename, nfolds, lambda))
  if(regtype == "Enet")
    return(arc.enet.auc(filename, nfolds, alpha, lambda))
  if(regtype == "Lapnet")
    return(arc.lapnet.auc(filename, nfolds, alpha, lambda))
  if(regtype == "Kernelnet")
    return(arc.kernelnet.auc(filename, nfolds, alpha, lambda))
  else
    print("Unknown regularization type")}
if(metric == "MSE"){
  if(regtype == "Lasso")
    return(arc.lasso.mse(filename, nfolds, lambda))
  if(regtype == "Enet")
    return(arc.enet.mse(filename, nfolds, alpha, lambda))
  if(regtype == "Lapnet")
    return(arc.lapnet.mse(filename, nfolds, alpha, lambda))
  if(regtype == "Kernelnet")
    return(arc.kernelnet.mse(filename, nfolds, alpha, lambda))
  else
    print("Unknown regularization type")}
else
    print("Unknown metric")

}
