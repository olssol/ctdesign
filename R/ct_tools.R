#' Get relative risk
#'
#'
#'@export
#'
rd_rr <- function(r1, r2, ratio = 1) {
    rr   <- r1 / r2
    abe  <- 1 - rr
    p    <- rr * ratio / (1 + rr * ratio)

    cbind(r1, r2, ratio, rr, abe, p)
}

#' Get incidence rate by VE
#'
#'@export
#'
rd_incidence <- function(ve, lambda_placebo) {
    lambda_placebo * (1 - ve)
}

#' Get censoring risk by annual dropout rate
#'
#'@export
#'
#'
rd_lambda_censor <- function(annual_dropout, tp = 1) {
    - log(1 - annual_dropout) / tp
}
