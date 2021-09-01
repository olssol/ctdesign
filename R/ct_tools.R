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


#' Simulate Survival time for a single arm
#'
#'
#'@export
#'
rd_pts_single <- function(n, ve, lambda_placebo, lambda_censor, p_evaluable) {
    lambda_trt  <- rd_incidence(ve, lambda_placebo)
    n_evaluable <- rbinom(1, n, p_evaluable)
    smp         <- rexp(n_evaluable, lambda_trt)
    smp_censor  <- rexp(n_evaluable, lambda_censor)

    rst_eval <- apply(cbind(smp, smp_censor), 1, function(x) {
        y     <- min(x)
        event <- x[1] < x[2]
        c(y, event)
    })

    rst_eval    <- t(rst_eval)
    rst_in_eval <- cbind(rep(-1, n - n_evaluable),
                         rep(-1, n - n_evaluable))

    rst <- rbind(rst_eval, rst_in_eval)
    rst[sample(n), ]
}


#' Simulate Survival time for all
#'
#'
#'@export
#'
rd_pts_all <- function(ve_trt, lambda_placebo, p_evaluable = 0.8,
                       n_tot = 15000,
                       annual_dropout = 0.05, annual_enroll = 7500,
                       seed = NULL, ...) {

    ## random seed
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    ve_all <- c(ve_trt, 0)

    ## number of patients
    n_arm  <- length(ve_all)
    n_each <- rep(ceiling(n_tot / n_arm), n_arm)

    ## enrollment time
    enroll_years <- n_tot / annual_enroll
    smp_enroll   <- runif(n_tot, min = 0, max = enroll_years)

    ## censoring
    lambda_censor <- rd_lambda_censor(annual_dropout)

    rst <- NULL
    for (i in seq_len(n_arm)) {
        smp_arm <- rd_pts_single(n_each[i],
                                 ve_all[i],
                                 lambda_placebo,
                                 lambda_censor,
                                 p_evaluable)
        rst     <- rbind(rst, cbind(i, smp_arm))
    }

    colnames(rst) <- c("Arm", "Time", "Event")
    rst           <- data.frame(rst)

    ## enroll time
    rst$Enroll       <- smp_enroll
    ## event or censoring time from study begins
    rst$CalendarTime <- rst$Time + rst$Enroll

    ## set seed
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    ## return
    rst
}

#' Count events based on target number of events
#'
#'@export
#'
rd_count_event <- function(smps_all, target_event) {
    events <- smps_all %>%
        filter(Event == 1) %>%
        arrange(CalendarTime)

    rst_count  <- NULL
    rst_enroll <- NULL
    for (j in target_event) {
        cur_rst <- events %>%
            slice_head(n = `j`)

        cur_time  <- cur_rst[j, "CalendarTime"]
        cur_count <- cur_rst %>%
            group_by(Arm) %>%
            summarize(N = n()) %>%
            mutate(Target = j)

        cur_enroll <- smps_all %>%
            filter(Enroll <= `cur_time`) %>%
            group_by(Arm) %>%
            summarize(N = n()) %>%
            mutate(Target   = `j`,
                   Duration = NA)

        rst_count  <- rbind(rst_count, cur_count)
        rst_enroll <- rbind(rst_enroll,
                            cur_enroll,
                            data.frame(Arm      = "Total",
                                       N        = sum(cur_enroll$N),
                                       Target   = j,
                                       Duration = cur_time))

    }

    list(events = data.frame(rst_count),
         enroll = data.frame(rst_enroll))
}

#' Pairwise P-values
#'
#'@export
#'
rd_pairwise_pval <- function(counts, p_null = 0.5, alternative = "less", ...) {
    n_arm <- nrow(counts)
    y_pla <- counts[n_arm, "N"]

    pvals <- NULL
    for (i in seq_len(n_arm - 1)) {
        y_trt    <- counts[i, "N"]
        cur_test <- binom.test(y_trt, y_trt + y_pla,
                               p = p_null, alternative = alternative)
        pvals    <- c(pvals, cur_test$p.value)
    }

    stopifnot(all(!is.na(pvals)))

    pvals
}

#' Pairwise Rejections (Strategy I)
#'
#'@export
#'
rd_rejection_s1 <- function(p_values, ...) {
    f_rej <- function(pvals) {
        rej <- rep(0, 2)
        if (pvals[1] < 0.0125) {
            rej[1] <- 1
            if (pvals[2] < 0.0125)
                rej[2] <- 1
        }

        rej
    }

    rst_1 <- f_rej(p_values[1:2])
    rst_2 <- f_rej(p_values[3:4])

    c(rst_1, rst_2)
}

#' Pairwise Rejections (Strategy II)
#'
#'@export
#'
rd_rejection_s2 <- function(p_values, ...) {
    f_rej <- function(pvals) {
        rej   <- rep(0, 2)
        min_p <- min(pvals)
        max_p <- max(pvals)

        if (min_p < 0.0125 / 2) {
            rej[1] <- 1
            if (max_p < 0.0125)
                rej[2] <- 1
        }

        if (min_p == pvals[2]) {
            rej <- rej[2:1]
        }

        rej
    }

    rst_1 <- f_rej(p_values[1:2])
    rst_2 <- f_rej(p_values[3:4])

    c(rst_1, rst_2)
}


#'  Overall
#'
#'@export
#'
#'
rd_simu_single <- function(ve_trt, lambda_placebo, target_event, ...,
                           seed = NULL) {

    if (is.numeric(seed)) {
        old_seed_kind <- RNGkind("L'Ecuyer-CMRG")
        old_seed      <- .Random.seed
        set.seed(seed)
    }

    arms   <- seq_len(length(ve_trt))

    pts    <- rd_pts_all(ve_trt, lambda_placebo, ...)
    counts <- rd_count_event(pts, target_event)
    events <- counts$events
    enroll <- counts$enroll

    rej_gate   <- NULL
    rej_hoch   <- NULL
    for (j in target_event) {
        cur_data <- events %>%
            filter(Target == `j`) %>%
            arrange(Arm)

        cur_pvals <- rd_pairwise_pval(cur_data, ...)
        cur_gate  <- rd_rejection_s1(cur_pvals, ...)
        cur_hoch  <- rd_rejection_s2(cur_pvals, ...)

        rej_gate  <- rbind(rej_gate,
                           data.frame(Target = j, Arm = arms, Rej = cur_gate))
        rej_hoch  <- rbind(rej_hoch,
                           data.frame(Target = j, Arm = arms, Rej = cur_hoch))
    }

    ## reset seed
    if (is.numeric(seed)) {
        set.seed(old_seed)
    }

    ## results
    rej_gate$Multi <- "Gate"
    rej_hoch$Multi <- "Hochberg"

    list(enroll    = enroll,
         events    = events,
         rejection = rbind(rej_gate, rej_hoch))
}

#' Simulate all
#'
#'@export
#'
rd_simu_all <- function(n_reps  = 2000,
                        n_cores = 5,
                        update_progress = NULL,
                        ...) {

    if (.Platform$OS.type == "windows" && n_cores > 1) {
        warning("n_cores is set to be 1 on Windows.");
        n_cores <- 1;
    }

    is_shiny <- !is.null(update_progress)

    if (is_shiny & n_cores > 1)
        update_progress$set(value = 1, detail = paste(""))

    rst <- parallel::mclapply(seq_len(n_reps), function(x) {
        if (is_shiny) {
            update_progress$set(value  = x / n_reps,
                                detail = paste("Replication",
                                               x, sep = " "))
        }
        cat("---- Replication", x, "\n")

        rd_simu_single(..., seed = 1000 * x)
    },  mc.cores = n_cores)

    ## summarize
    rst_enroll    <- NULL
    rst_rejection <- NULL
    rst_events    <- NULL
    for (i in seq_len(n_reps)) {
        cur_rst <- rst[[i]]

        if ("error" %in% class(cur_rst))
            next

        rst_enroll    <- rbind(rst_enroll,    cur_rst$enroll)
        rst_rejection <- rbind(rst_rejection, cur_rst$rejection)
        rst_events    <- rbind(rst_events   , cur_rst$events)
    }

    list(enroll    = rst_enroll,
         events    = rst_events,
         rejection = rst_rejection)
}

#' Summarize results
#'
#'@export
#'
rd_summary <- function(rst_simu) {
    rst_enroll <- rst_simu$enroll %>%
        group_by(Target, Arm) %>%
        summarize(N        = mean(N),
                  Duration = mean(Duration))

    rst_rejection <- rst_simu$rejection %>%
        group_by(Target, Arm, Multi) %>%
        summarize(Power = mean(Rej))

    rst_events <- rst_simu$events %>%
        group_by(Target, Arm) %>%
        summarize(Events = mean(N))

    list(enroll    = rst_enroll,
         events    = rst_events,
         rejection = rst_rejection)
}

#' Plot Power
#'
#'
#'
#'@export
#'
rd_plot_power <- function(rst_rejection) {
    rst_rejection <- rst_rejection %>%
        mutate(Arm = factor(Arm, levels = 1:4))

    ggplot(data = rst_rejection, aes(x = Target, y = Power)) +
        geom_line(aes(group = Arm, color = Arm)) +
        facet_wrap(~Multi) +
        theme_bw() +
        ylim(0, 1) +
        geom_hline(yintercept = 0.9)
}

#' Plot Enroll
#'
#'
#'
#'@export
#'
rd_plot_enroll <- function(rst_enroll) {
    rst_enroll <- rst_enroll %>%
        filter(Arm == "Total") %>%
        gather(key = Enroll, value = Y, N, Duration)

    ggplot(data = rst_enroll, aes(x = Target, y = Y)) +
        geom_line() +
        facet_wrap(~ Enroll, scales = "free_y") +
        theme_bw()
}

#' Plot Events
#'
#'
#'
#'@export
#'
rd_plot_event <- function(rst_events) {
    rst_events <- rst_events %>%
        mutate(Arm = factor(Arm))

    ggplot(data = rst_events, aes(x = Target, y = Events)) +
        geom_line(aes(group = Arm, color = Arm)) +
        theme_bw()
}
