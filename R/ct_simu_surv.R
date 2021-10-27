#' Simulate from piecewise constant exponential
#'
#'
#' @examples
#' rd_sim_pwexp(hazards = c("0.5" = 0.045, "Inf" = 0.025), offset = 0)
#'
#' @export
#'
rd_simu_pwexp <- function(hazards, offset = 0) {
    if (1 == length(hazards)) {
        tte <- rexp(1,  hazards)
    } else {
        segments <- as.numeric(names(hazards))
        segments <- sort(segments) - offset

        inx    <- min(which(segments > 0))
        cur_t  <- 0
        flag   <- FALSE
        while (!flag) {
            cur_h   <- hazards[inx]
            cur_int <- rexp(1, cur_h)

            if ((cur_t + cur_int) <= segments[inx]) {
                flag <- TRUE
                tte  <- cur_t + cur_int
                break
            }

            cur_int <- segments[inx]
            inx     <- inx + 1
        }
    }

    ## return
    rst <- c(tte    = tte,
             offset = offset)

    return(rst)
}
