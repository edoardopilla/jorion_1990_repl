# import necessary libraries
library(readxl)

library(NlcOptim)

library(RColorBrewer)

# read data
sector <- read_excel(path = "Data_Exercise 4.xlsx",
                     sheet = "s&p 500 sector returns")

capw <- read_excel(path = "Data_Exercise 4.xlsx",
                   sheet = "s&p 500 capweighted returns")

equalw <- read_excel(path = "Data_Exercise 4.xlsx",
                     sheet = "s&p 500 equalweighted returns")

tbills <- read_excel(path = "Data_Exercise 4.xlsx", sheet = "Tbills Yield")

# prepare data
sector_new <- as.matrix(sector[nrow(sector): 1, - 1])

capw_new <- as.matrix(capw[nrow(capw): 1, - 1])

equalw_new <- as.matrix(equalw[nrow(equalw): 1, - 1])

tbills_new <- as.matrix(tbills[nrow(tbills): 1, - 1])

sector_new <- log(1 + sector_new / 100) * 100

capw_new <- log(1 + capw_new / 100) * 100

equalw_new <- log(1 + equalw_new / 100) * 100

tbills_new <- log(1 + tbills_new / 100) * 100

sector_new <- sector_new - matrix(data = tbills_new, nrow = nrow(sector_new),
                                  ncol = ncol(sector_new))

capw_new <- capw_new - tbills_new

equalw_new <- equalw_new - tbills_new

# define generic inputs
dt <- 60

windows <- nrow(sector_new) - dt + 1

sectors <- ncol(sector_new)

# create function to estimate mu
mu_func <- function(rets, ind, method, dt){
  windows <- nrow(rets) - dt + 1
  
  sectors <- ncol(rets)
  
  mu_est <- matrix(data = NA, nrow = windows, ncol = sectors)
  
  switch (method,
    "sample" = {
      for(j in 1: sectors){
        for(i in 1: windows){
          mu_est[i, j] <- apply(X = as.matrix(rets[i: (dt + i - 1), j]),
                                MARGIN = 2, FUN = mean)
        }
      }
    },
    
    "capm" = {
      for(j in 1: sectors){
        for(i in 1: windows){
          beta <- lm(formula = rets[i: (dt + i - 1), j] ~
                       ind[i: (dt + i - 1),])$coefficients[2]
          
          mu_est[i, j] <- beta * max(0, mean(ind[i: (dt + i - 1),]))
        }
      }
    },
    
    stop("Please enter either 'sample' or 'capm' as method.")
  )
  
  return(mu_est)
}

# estimate mu for different methods
mu_sample <- mu_func(rets = sector_new, ind = capw_new, method = "sample",
                     dt = 60)

mu_capw <- mu_func(rets = sector_new, ind = capw_new, method = "capm", dt = 60)

mu_equalw <- mu_func(rets = sector_new, ind = equalw_new, method = "capm",
                     dt = 60)

# create function to compute mse sums for different methods
mse_sum <- function(est_mu, rets, dt){
  windows <- nrow(rets) - dt + 1
  
  error <- est_mu[-windows,] - rets[-(1: dt),]
  
  mse <- apply(X = error ^ 2, MARGIN = 2, FUN = mean)
  
  return(sum(mse))
}

# compute mse sums for different methods
sum_mse_sample <- mse_sum(mu_sample, sector_new, 60)

sum_mse_capw <- mse_sum(mu_capw, sector_new, 60)

sum_mse_equalw <- mse_sum(mu_equalw, sector_new, 60)

# create function to compute tangency portfolios weights and plot evolution
tan_pf_func <- function(rets, est_mu, dt){
  windows <- nrow(rets) - dt + 1
  
  sectors <- ncol(rets)
  
  cov_arr <- array(data = NA, dim = c(sectors, sectors, windows))
  
  tan_pf_w <- matrix(data = NA, nrow = windows, ncol = sectors)
  
  for(i in 1: windows){
    cov_arr[,,i] <- cov(rets[i: (dt + i - 1),])
    
    tan_pf_w[i,] <- solnl(X = rep(x = 1 / sectors, times = sectors),
                          objfun = function(w) -(t(w) %*% est_mu[i,]) /
                            (sqrt(t(w) %*% cov_arr[,,i] %*% w)),
                          Aeq = matrix(data = 1, nrow = 1, ncol = sectors),
                          Beq = 1, lb = rep(x = 0, times = sectors))$par
  }
  
  fig_col <- brewer.pal(n = sectors, name = "Paired")
  
  par(xpd = T, mar = par()$mar + c(0, 0, 0, 8))
  
  barplot(height = t(tan_pf_w), col = fig_col,
          main = "Tangency Portfolio Weights over Time", ylab = "weight",
          xlab = "time")
  
  legend(x = 'topright', legend = colnames(rets), fill = fig_col,
         bty = "n", bg = NA, cex = .7, pt.cex = .7, xjust = 1,
         inset = c(-.5, 0))
  
  par(mar = c(5, 4, 4, 2) + .1)
  
  return(tan_pf_w)
}

# compute tangency portfolios for different methods and draw related plots
tan_pf_sample <- tan_pf_func(rets = sector_new, est_mu = mu_sample, dt = 60)

tan_pf_capw <- tan_pf_func(rets = sector_new, est_mu = mu_capw, dt = 60)

tan_pf_equalw <- tan_pf_func(rets = sector_new, est_mu = mu_equalw, dt = 60)

# create function to compute out of sample returns and related statistics
oos_rets_func <- function(tan_pf_weights, rets, dt){
  windows <- nrow(rets) - dt + 1
  
  oos_rets <- rowSums(tan_pf_weights[-windows,] * rets[-(1: dt),])
  
  oos_stats <- list("Mean" = mean(oos_rets),
                    "Standard deviation" = sd(oos_rets),
                    "Sharpe ratio" = mean(oos_rets) / sd(oos_rets))
  
  return(oos_stats)
}

# compute stats for the different methods
oos_sample <- oos_rets_func(tan_pf_weights = tan_pf_sample, rets = sector_new,
                            dt = 60)

oos_capw <- oos_rets_func(tan_pf_capw, sector_new, dt)

oos_equalw <- oos_rets_func(tan_pf_equalw, sector_new, dt)
