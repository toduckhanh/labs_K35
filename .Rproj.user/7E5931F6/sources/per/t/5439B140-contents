source("R/kernel_functions.R")

kde <- function(data, y, h, kernel = "gaus") {
  # n <- length(data)
  if(kernel == "gaus"){
    u <- kernel_gaus((data - y)/h)/h
    kde_out <- mean(u)
  } else{ # tam thoi khong tinh
    kde_out <- NA
  }
  return(kde_out)
}

kde_vect <- Vectorize(FUN = kde, vectorize.args = "y")


