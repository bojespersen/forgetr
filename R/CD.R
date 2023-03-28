# Different useful functions 
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


# Critical difference calculations based on ISO 5725
CDIP <- function(s_r, s_T, n1, n2, alpha=0.05, k_dec=Inf) {
  k <- round(qnorm(1-(alpha/2))*sqrt(2),k_dec)
  sqrt((k*s_T)^2+(k*s_r)^2*(1/(2*n1)+1/(2*n2)))
}

CDr <- function(s_r, n1, n2, alpha=0.05, k_dec=Inf) {
  k <- round(qnorm(1-(alpha/2))*sqrt(2),k_dec)
  k*s_r*sqrt(1/(2*n1)+1/(2*n2))
}

CDR <- function(s_r, s_R, n1, n2, alpha=0.05, k_dec=Inf) {
  k <- round(qnorm(1-(alpha/2))*sqrt(2),k_dec)
  sqrt((k*s_R)^2-(k*s_r)^2*(1-(1/(2*n1))-(1/(2*n2))))
}

CDs <- function(s_r, s_R, n, alpha=0.05, k_dec=Inf) {
  k <- round(qnorm(1-(alpha/2))*sqrt(2),k_dec)
  1/sqrt(2)*sqrt((k*s_R)^2-(k*s_r)^2*((n-1)/n))
}

### Calculation of repeatability limit
# s: best estimate of true repeatability
# n: number of repetitions of the analysis
# Alpha: significance level (0 to 1)
# Output: prediction interval for the standard deviation (repeatability)
sInt <- function(s, n, alpha) {
  sqrt(s^2/(n-1)*qchisq(1-alpha, n-1))
}

### Calculating the true standard deviation based on the interval size and number og repetitions
# s: prediction interval for the standard deviation (repeatability)
# n: number of repetitions of the analysis
# Alpha: significance level (0 to 1)
# Output: best estimate of true repeatability corresponding to the interval limit
sInt_inv <- function(s, n, alpha) {
  sqrt((s^2*n-s^2)/qchisq(1-alpha, n-1))
}