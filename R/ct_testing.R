#' Hochberg multiplicity control
#'
#' @export
rd_hochberg <- function(pvals_0, alpha = 0.05, ...) {
    p_inx <- order(pvals_0, decreasing = TRUE)
    pvals <- pvals_0[p_inx]

    rej   <- rep(1, length(pvals))
    for (i in seq_len(length(pvals))) {
        if (pvals[i] < alpha / i)
            break

        rej[i] <- 0
    }

    rej[order(p_inx)]
}

#' Holmes multiplicity control
#'
#' @export
rd_holms <- function(pvals, alpha = 0.05, ...) {
    p_inx <- order(pvals)
    pvals <- pvals[p_inx]
    k     <- length(pvals)

    rej   <- rep(0, k)
    for (i in seq_len(k)) {
        if (pvals[i] > alpha / (k - i + 1))
            break

        rej[i] <- 1
    }

    rej[order(p_inx)]
}

#' Hierarchical
#'
#' @export
rd_hierarchi <- function(pvals, alpha  = 0.05, p_inx = NULL,  ...) {
    k <- length(pvals)
    if (is.null(p_inx)) {
        p_inx <- seq_len(k)
    }

    pvals <- pvals[p_inx]
    rej   <- rep(0, k)
    for (i in seq_len(k)) {
        if (pvals[i] > alpha)
            break

        rej[i] <- 1
    }

    rej[order(p_inx)]
}

#' Pairwise P-values
#'
#'@export
#'
rd_pairwise_pval <- function(counts, p_null = 0.5, alternative = "less", ...) {
    n_arm <- length(counts)
    y_pla <- counts[n_arm]

    pvals <- NULL
    for (i in seq_len(n_arm - 1)) {
        y_trt    <- counts[i]
        cur_test <- binom.test(y_trt, y_trt + y_pla,
                               p = p_null, alternative = alternative)
        pvals    <- c(pvals, cur_test$p.value)
    }

    stopifnot(all(!is.na(pvals)))

    ## return
    pvals
}

#' Pairwise Rejections (Strategy I)
#'
#'@export
#'
rd_rejection_s1 <- function(p_vals, ...) {
    rej <- rd_hochberg(p_vals, alpha = 0.025)
    rej
}

#' Pairwise Rejections
#'
#'@export
#'
rd_rejection_s2 <- function(p_vals, ...) {
    rej <- rd_holms(p_vals, alpha = 0.025)
    rej
}

#' Pairwise Rejections
#'
#'@export
#'
rd_rejection_s3 <- function(p_vals, ...) {
    rej <- rd_hierarchi(p_vals, alpha = 0.025)
    rej
}

#' @export
rd_rejection_s4 <- function(p_vals, ...) {
    rej_1 <- p_vals[1] <= 0.025 / 2
    rej_2 <- rd_holms(p_vals[2:3], alpha = 0.025 /2)
    c(rej_1, rej_2)
}

#' @export
rd_rejection_s5 <- function(p_vals, ...) {
    rej_1 <- p_vals[1] <= 0.025 / 2
    rej_2 <- rd_hochberg(p_vals[2:3], alpha = 0.025 /2)
    c(rej_1, rej_2)
}

#' @export
rd_rejection_s6 <- function(p_vals, ...) {
    rej_1 <- p_vals[1] <= 0.025 / 2
    rej_2 <- rd_hierarchi(p_vals[2:3], alpha = 0.025 /2)
    c(rej_1, rej_2)
}


#' rejection all
#'
#' @export
rd_rejection_all <- function(cur_data, hyp_tests, ...) {

    n_arm   <- nrow(cur_data)
    n_tests <- length(hyp_tests)
    labs    <- c(levels(cur_data$Arm)[-n_arm],
                 "Any Arm")
    labs    <- factor(labs, levels = labs)
    abes    <- c(cur_data$AbE[-n_arm], NA)

    if (is.null(names(hyp_tests)))
        names(hyp_tests) <- paste("Test ", seq_len(n_tests), sep = "")

    ## pvalues
    pvals <- rd_pairwise_pval(cur_data$N_Event, ...)

    ## rejection
    rst <- NULL
    for (i in seq_len(n_tests)) {
        f_rej   <- hyp_tests[[i]]
        cur_rej <- f_rej(pvals, ...)
        cur_rej <- c(cur_rej, any(1 == cur_rej))
        cur_rst <- data.frame(Multi = names(hyp_tests)[[i]],
                              Arm   = labs,
                              Pval  = c(pvals, NA),
                              Abe   = abes,
                              Rej   = cur_rej)
        rst     <- rbind(rst, cur_rst)
    }

    rst
}
