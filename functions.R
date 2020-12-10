#beta-binomial model
beta_binom <- function(p_a, p_b, d_n, d_x){ 
  
  
  theta = seq(0, 1, 0.0001)
  
  #prior
  prior = dbeta(theta, p_a, p_b)
  
  prior_mean = p_a / (p_a + p_b)
  prior_sd = sqrt((p_a / (p_a + p_b))*(1 - p_a/(p_a + p_b))/(p_a + p_b + 1))
  
  
  #likelihood
  likelihood = dbinom(d_x, d_n, theta)
  
  data_mean = round(d_x/d_n, 4)
  data_sd = "N/A"
  
  #posterior 
  post_a = p_a + d_x 
  post_b = p_b + (d_n - d_x)
  posterior = dbeta(theta, post_a, post_b)
  
  post_mean = round(post_a / (post_a + post_b), 4)
  post_sd = round(sqrt((post_a / (post_a + post_b))*(1 - post_a/(post_a + post_b))/(post_a + post_b + 1)), 4)
  
  #plotting
  ymax = dbeta((post_a-1)/(post_a+post_b-2), post_a, post_b)
  scaled_likelihood = likelihood * ymax / max(likelihood)
  
  plot(theta, prior, type='l', col='orange', xlim=c(0, 1), ylim=c(0, ymax), ylab='', yaxt='n')
  par(new=T)
  plot(theta, scaled_likelihood, type='l', col='skyblue', xlim=c(0, 1), ylim=c(0, ymax), ylab='',  yaxt='n')
  par(new=T)
  plot(theta, posterior, type='l', col='seagreen', xlim=c(0, 1), ylim=c(0, ymax), ylab='', yaxt='n')
  legend("topleft", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))
  
  
  result <- data.frame(" " = c("S", "F", "n", "Mean" , "SD"), "Prior" = c(p_a, p_b, p_a + p_b, prior_mean, prior_sd), "Data" = c(d_x, d_n - d_x, d_n, data_mean, data_sd), "Posterior" = c(post_a, post_b, post_a + post_b, post_mean, post_sd))
  
  return(list(result, prior, scaled_likelihood, posterior, ymax, theta))
}  



#poisson-gamma model

poisson_gamma <- function(p_count, p_size, d_count, d_n){ 
  theta_ub = p_size + d_n + 10
  theta = seq(0, theta_ub, 0.0001)
  
  #prior
  prior = dgamma(theta, p_count, p_size)
  
  prior_mean = p_count/ p_size
  prior_sd = sqrt(p_count / (p_size^2))
  
  
  #likelihood
  likelihood = dpois(d_count, d_n*theta)
  
  data_mean = d_count/d_n
  data_sd = "N/A"
  
  #posterior 
  post_count = p_count + d_count 
  post_size = p_size + d_n
  posterior = dgamma(theta, post_count, post_size)
  
  post_mean = (post_count / post_size)
  post_sd = sqrt(post_count/post_size^2)
  
  #plotting
  ymax = max(posterior)
  scaled_likelihood = likelihood * ymax / max(likelihood)
  
 
  result <- data.frame(" " = c("Total Count", "Sample Size", "Mean", "SD"), "Prior" = c(p_count, p_size, prior_mean, prior_sd), "Data" = c(d_count, d_n,data_mean, data_sd), "Posterior" = c(post_count, post_size, post_mean, post_sd))
  
  return(list(result, prior, scaled_likelihood, posterior, ymax, theta))
}  





normal <- function(p_tau, p_mean, d_n , d_x, sigma){
  
  options(digits=3, scipen=999)
  theta = seq(p_mean-4*p_tau, p_mean+4*p_tau, 0.0001)
  
  #prior estimates
  p_prec = 1 / (p_tau^2)
  prior = dnorm(theta, p_mean, p_tau)
  #data estimates
  d_tau = sigma / (d_n^(1/2))
  d_prec = d_n / (sigma^2)
  d_mean = d_x 
  xlims = c(d_mean - 5*sigma, d_mean + 5*sigma)
  
  #likelihood
  likelihood = dnorm(d_x, theta, sigma/sqrt(d_n))
  
  # posterior
  post_prec = 1/p_tau^2 + d_n/sigma^2
  post_tau = sqrt(1/post_prec)
  post_mean = ((1/p_tau^2)*p_mean + (d_n/sigma^2)*d_x) / post_prec
  posterior = dnorm(theta, post_mean, post_tau)
  
  # plot
  ymax = dnorm(post_mean, post_mean, post_tau)
  scaled_likelihood = likelihood * ymax / max(likelihood)
  
  plot(theta, prior, type='l', col='orange', xlim=xlims, ylim=c(0, ymax), ylab='', yaxt='n')
  par(new=T)
  plot(theta, scaled_likelihood, type='l', col='skyblue', xlim=xlims, ylim=c(0, ymax), ylab='',yaxt='n')
  par(new=T)
  plot(theta, posterior, type='l', col='seagreen', xlim=xlims, ylim=c(0, ymax), ylab='', yaxt='n')
  legend("topright", c("prior", "scaled likelihood", "posterior"), lty=1, col=c("orange", "skyblue", "seagreen"))
  
  result <- data.frame(" " = c("SD", "Precision", "Mean"), "Prior" = c(p_tau, p_prec, p_mean), "Data" = c(d_tau, d_prec, d_mean), "Posterior" = c(post_tau, post_prec, post_mean))
  
  return(list(result, prior, scaled_likelihood, posterior, ymax, theta))
}


expon_gamma <- function(p_alpha, p_lambda, d_n, d_mean){ 
  theta_ub = p_alpha + p_lambda + d_n + 10
  
  theta = seq(.0001, theta_ub, 0.0001)
  n = d_n
  
  #prior
  prior = dgamma(theta, p_alpha, p_alpha/p_lambda)
  
  prior_mean = p_alpha/p_lambda
  prior_sd = sqrt(p_alpha/(p_lambda^2))
  
  #likelihood
  #likelihood = dexp(d_n, 1/d_mean)
  likelihood = dgamma(d_n*d_mean, shape=d_n, rate=theta)
  
  data_mean = d_mean
  data_sd = "N/A"
  
  
  #posterior 
  post_alpha = p_alpha + d_n
  post_lambda = p_lambda + (d_n*d_mean)
  posterior = dgamma(theta, post_alpha, post_lambda)
  
  post_mean = (post_alpha / post_lambda)
  post_sd = sqrt(post_alpha/post_lambda^2)
  
  ymax = max(prior, posterior)
  scaled_likelihood = likelihood * ymax / max(likelihood)
  ymax = max(prior, scaled_likelihood, posterior)
  result <- data.frame(" " = c("Total Count", "Total Time", "Rate", "Mean", "SD"), "Prior" = c(p_alpha, p_lambda, prior_mean, prior_mean, prior_sd), "Data" = c(d_n, d_n*d_mean, 1/d_mean, data_mean, data_sd), "Posterior" = c(post_alpha, post_lambda, post_alpha/post_lambda, post_mean, post_sd))
  
  return(list(result, prior, scaled_likelihood, posterior, ymax, theta, post_lambda))
}
