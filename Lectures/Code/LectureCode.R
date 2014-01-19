# Internal functions for demonstrations etc.


# Calculate estimates with standard errors with simple random sampling (SRS)
ySRS <- function(n, N, ybar, s2){
  results <- list(est = ybar, 
                  SE = sqrt((s2/n)*(1-n/N)))
                  return(results)
}

tSRS <- function(n, N, ybar, s2){
  results <- list(est = ybar*N, 
                  SE = N*sqrt((s2/n)*(1-n/N)))
                  return(results)
}

pSRS <- function(n, N, p){
  results <- list(est = p, 
                  SE = sqrt(p*(1-p)*(1/(n-1))*(1-n/N)))
                  return(results)
}