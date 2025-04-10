#' Holt's Two-parameter Exponential Smoothing
#' @description Performs Holt's two-parameter exponential smoothing for linear
#' trend or damped trend.
#' @param x a numeric vector or univariate time series.
#' @param type the type of interaction between the level and the linear trend. See 
#' details.
#' @param alpha the parameter for the level smoothing. The default is \code{0.2}.
#' @param beta the parameter for the trend smoothing. The default is \code{0.1057}.
#' @param lead the number of steps ahead for which prediction is required. 
#' The default is \code{0}.
#' @param damped a logical value indicating a damped trend. See details. The default is
#' \code{FALSE}.
#' @param phi a smoothing parameter for damped trend. The default is \code{0.98}, only valid
#' for \code{damped = TRUE}.
#' @param plot a logical value indicating to print the plot of original data v.s smoothed 
#' data. The default is \code{TRUE}.
#' @details Holt's two parameter is used to forecast a time series with trend, but 
#' wihtout seasonal pattern. For the additive model (\code{type = "additive"}), the 
#' \eqn{h}-step-ahead forecast is given by \eqn{hat{x}[t+h|t] = level[t] + h*b[t]},
#'  where
#' \deqn{level[t] = \alpha *x[t] + (1-\alpha)*(b[t-1] + level[t-1]),}
#' \deqn{b[t] = \beta*(level[t] - level[t-1]) + (1-\beta)*b[t-1],}
#' in which \eqn{b[t]} is the trend component.
#' For the multiplicative (\code{type = "multiplicative"}) model, the 
#' \eqn{h}-step-ahead forecast is given by \eqn{hat{x}[t+h|t] = level[t] + h*b[t]},
#'  where
#' \deqn{level[t] = \alpha *x[t] + (1-\alpha)*(b[t-1] * level[t-1]),}
#' \deqn{b[t] = \beta*(level[t] / level[t-1]) + (1-\beta)*b[t-1].} 
#' 
#' Compared with the Holt's linear trend that displays a constant increasing or 
#' decreasing, the damped trend generated by exponential smoothing method shows a 
#' exponential growth or decline, which is a situation between simple exponential
#' smoothing (with 0 increasing or decreasing rate) and Holt's two-parameter smoothing.
#' If \code{damped = TRUE}, the additive model becomes
#' \deqn{hat{x}[t+h|t] = level[t] + (\phi + \phi^{2} + ... + \phi^{h})*b[t],}
#' \deqn{level[t] = \alpha *x[t] + (1-\alpha)*(\phi*b[t-1] + level[t-1]),}
#' \deqn{b[t] = \beta*(level[t] - level[t-1]) + (1-\beta)*\phi*b[t-1].}
#' The multiplicative model becomes
#' \deqn{hat{x}[t+h|t] = level[t] *b[t]^(\phi + \phi^{2} + ... + \phi^{h}),}
#' \deqn{level[t] = \alpha *x[t] + (1-\alpha)*(b[t-1]^{\phi} * level[t-1]),}
#' \deqn{b[t] = \beta*(level[t] / level[t-1]) + (1-\beta)*b[t-1]^{\phi}.}
#' See Chapter 7.4 for more details in R. J. Hyndman and G. Athanasopoulos (2013).
#' @note Missing values are removed before analysis. 
#' @return A list with class "\code{Holt}" containing the following components:
#' \item{estimate}{the estimate values.}
#' \item{alpha}{the smoothing parameter used for level.}
#' \item{beta}{the smoothing parameter used for trend.}
#' \item{phi}{the smoothing parameter used for damped trend.}
#' \item{pred}{the predicted values, only available for \code{lead} > 0.}
#' \item{accurate}{the accurate measurements.}
#' @author Debin Qiu
#' @references R. J. Hyndman and G. Athanasopoulos, "Forecasting: principles and
#' practice," 2013. [Online]. Available: \url{http://otexts.org/fpp/}.
#' @seealso \code{\link{HoltWinters}}, \code{\link{expsmooth}}, \code{\link{Winters}}
#' @examples x <- (1:100)/100
#' y <- 2 + 1.2*x + rnorm(100)
#' 
#' ho0 <- Holt(y) # with additive interaction
#' ho1 <- Holt(y,damped = TRUE) # with damped trend
#' 
#' # multiplicative model for AirPassengers data, 
#' # although seasonal pattern exists.
#' ho2 <- Holt(AirPassengers,type = "multiplicative")
#' @importFrom stats is.ts
#' @importFrom stats ts
#' @importFrom graphics plot
#' @importFrom graphics lines
#' @export 
Holt <- function(x,type = c("additive","multiplicative"),
                 alpha = 0.2,beta = 0.1057,lead = 0,
                 damped = FALSE,phi = 0.98,plot = TRUE)
{
  if (NCOL(x) > 1)
    stop("'x' must be a numeric vector or univariate time series ")
  if (any(c(alpha,beta) > 1) || any(c(alpha,beta) < 0))
    stop("'alpha' or 'beta' must be between 0 and 1")
  if (phi > 1 || phi < 0)
    stop("'phi' must be between 0 and 1")
  type <- match.arg(type)
  if (any(!is.finite(x)))
    warning(paste("missing values exist at time",which(!is.finite(x)),
                  "and will be removed."))
  if (is.ts(x))
    x <- ts(x[is.finite(x)],start = time(x)[1],frequency = frequency(x))
  else
    x <- x[is.finite(x)]
  n <- length(x)
  if (n < 1L)
    stop("invalid length of 'x'")
  level <- c(x[1],numeric(n-1))
  trend <- numeric(n)
  trend[1] <- switch(type,additive = (x[n] - x[1])/n,
                     multiplicative = x[2]/x[1])
  x.hat <- c(x[1],numeric(n-1))
  phi <- ifelse(damped,phi,1)
  for (i in 2:n) {
    if (type == "additive") {
      level[i] <- alpha*x[i] + (1 - alpha)*(level[i-1] + phi*trend[i-1])
      trend[i] <- beta*(level[i] - level[i-1]) + (1 - beta)*phi*trend[i-1]
      x.hat[i] <- level[i-1] + phi*trend[i-1]
    }
    else {
      level[i] <- alpha*x[i] + (1 - alpha)*(level[i-1] * trend[i-1]^phi)
      trend[i] <- beta*(level[i] / level[i-1]) + (1 - beta)*trend[i-1]^phi
      x.hat[i] <- level[i-1] * trend[i-1]^phi
    }
  }
  if (is.ts(x))
    x.hat <- ts(x.hat,start = time(x)[1],frequency = frequency(x))
  result <- list(estimate = x.hat,alpha = alpha, beta = beta)
  if (lead > 0) {
    if (lead < 0 || lead%%1 != 0) 
      stop("'lead' must be a positive integer")
    l.s <- 1:lead
    x.pred <- switch(type, additive = level[n] + cumsum(phi^l.s)*trend[n],
                     multiplicative = level[n] + trend[n]^(cumsum(phi^l.s)))
    names(x.pred) <- n + 1:lead
    result <- c(result,list(phi = phi, pred = x.pred))
  }
  if (plot) {
    plot(x,main = "original v.s smoothed data",type = "l")
    lines(x.hat,col = 2)
  }
  k <- ifelse(damped,3,2)
  result <- c(result,list(accurate = accurate(x,x.hat,k,output = FALSE)))
  class(result) <- "Holt"
  return(result)
}

#' Accurate Computation
#' @description Computes the accurate criterion of smoothed (fitted) values.
#' @param x a numeric vector of original values.
#' @param x.hat a numeric vector of smoothed (fitted) values.
#' @param k the number of parameters in obtaining the smoothed (fitted) values.
#' @param output a logical value indicating to print the results in R console. The default is
#' \code{TRUE}.
#' @details See \url{http://www.dms.umontreal.ca/~duchesne/chap12.pdf} in page 616 - 617 for
#' the details of calculations for each criterion.
#' @note If the model fits the series badly, the model error sum of squares \code{SSE}
#' may be larger than \code{SST} and the \code{R.squared} or \code{RW.R.squared} statistics 
#' will be negative. The \code{RW.R.squared} uses the random walk model for the purpose of 
#' comparison.
#' @return A vector containing the following components:
#' \item{SST}{the total sum of squares.}
#' \item{SSE}{the sum of the squared residuals.}
#' \item{MSE}{the mean squared error.}
#' \item{RMSE}{the root mean square error.}
#' \item{MAPE}{the mean absolute percent error.}
#' \item{MPE}{the mean percent error.}
#' \item{MAE}{the mean absolute error.}
#' \item{ME}{the mean error.}
#' \item{R.squared}{R^2 = 1 - SSE/SST.}
#' \item{R.adj.squared}{the adjusted R^2.}
#' \item{RW.R.squared}{the random walk R^2.}
#' \item{AIC}{the Akaike's information criterion.}
#' \item{SBC}{the Schwarz's Bayesian criterion.}
#' \item{APC}{the Amemiya's prediction criterion}
#' @author Debin Qiu
#' @examples X <- matrix(rnorm(200),100,2)
#' y <- 0.1*X[,1] + 2*X[,2] + rnorm(100)
#' y.hat <- fitted(lm(y ~ X))
#' accurate(y,y.hat,2)
#' @importFrom stats embed
#' @export
accurate <- function(x,x.hat,k,output = TRUE)
{
  n <- length(x)
  SST <- sum((x - mean(x))^2)
  SSE <- sum((x - x.hat)^2)
  MSE <- SSE/(n - k)
  RMSE <- sqrt(MSE)
  MAPE <- 100*mean(abs((x - x.hat)/x))
  MPE <- 100*mean((x - x.hat)/x)
  MAE <- mean(abs(x - x.hat))
  ME <- mean(x - x.hat)
  R2 <- 1 - SSE/SST
  ADJ.R2 <- 1 - (n - 1)*(1 - R2)/(n - k)
  z <- embed(x,2)
  RW.R2 <- 1 - (n - 1)*SSE/(n*sum(z[,1] - z[,2] - mean(z[,1] - z[,2])))
  AIC <- n*log(SSE/n) + 2*k
  SBC <- n*log(SSE/n) + k*log(n)
  APC <- ((n + k)/(n*(n - k)))*SSE
  result <- c(SST,SSE,MSE,RMSE,MAPE,MPE,MAE,ME,R2,ADJ.R2,RW.R2,AIC,SBC,APC)
  names(result) <- c("SST","SSE","MSE","RMSE","MAPE","MPE","MAE","ME","R.squared",
                     "R.adj.squared","RW.R.squared","AIC","SBC","APC")
  if (output) {
    cat("SST:",SST, "; SSE:", SSE, "; MSE:", MSE, "; RMSE:", RMSE, "\n")
    cat("MAPE:", MAPE, "; MPE:", MPE, "; MAE:", MAE, "; ME:", ME, "\n")
    cat("R.squared:", R2, "; R.adj.squared:", ADJ.R2, "; RW.R.squared:", RW.R2,"\n")
    cat("AIC:", AIC, "; SBC:", SBC, "; APC:", APC)
  }
  accurate <- result
}
