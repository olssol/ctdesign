#' Simulate Survival time for a single arm
#'
#'
#'@export
#'
rd_pts_single <- function(n, ve, lambda_placebo, lambda_censor,
                          p_evaluable, max_followup) {

    lambda_trt  <- rd_incidence(ve, lambda_placebo)
    n_evaluable <- rbinom(1, n, p_evaluable)
    smp         <- rexp(n_evaluable, lambda_trt)
    smp_censor  <- rexp(n_evaluable, lambda_censor)

    rst_eval <- apply(cbind(smp, smp_censor), 1,
                      function(x) {
                          y     <- min(c(x, max_followup))
                          event <- as.numeric(y == x[1])

                          c(y, event)
                      })

    rst <- t(rst_eval)

    ## if there are inevaluable subjects
    if (n_evaluable < n) {
        rst_in_eval <- cbind(rep(-1, n - n_evaluable),
                             rep(-1, n - n_evaluable))
        rst <- rbind(rst, rst_in_eval)
    }

    ## return
    rst           <- rst[sample(n), ]
    colnames(rst) <- c("Time", "Event")
    data.frame(rst)
}


#' Simulate Survival time for all
#'
#'
#'@export
#'
rd_pts_all <- function(ve_trt, ir_placebo_1yr, p_evaluable = 1,
                       annual_dropout = 0.025, annual_enroll = 6000,
                       n_tot = 15000, max_followup_yr = 1, seed = NULL,
                       ...) {

    ## random seed
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    ## arms
    if (is.null(names(ve_trt))) {
        arms <- paste("Arm ",
                      seq_len(length(ve_trt)),
                      sep = "")
    } else {
        arms <- names(ve_trt)
    }
    arms <- paste(arms, "(AbE=", ve_trt, ")", sep = "")

    ## add placebo
    ve_all   <- c(ve_trt, 0)
    arms_all <- c(arms,
                  paste("Placebo(IR=", ir_placebo_1yr, ")", sep = ""))
    n_arm    <- length(ve_all)

    ## number of patients
    n_each <- rep(ceiling(n_tot / n_arm), n_arm)

    ## enrollment time
    enroll_years <- n_tot / annual_enroll
    smp_enroll   <- runif(n_tot, min = 0, max = enroll_years)

    ## lambda for placebo
    lambda_placebo <- ir_placebo_1yr

    ## lambda for censoring
    lambda_censor <- rd_lambda_censor(annual_dropout)

    rst <- NULL
    for (i in seq_len(n_arm)) {
        smp_arm <- rd_pts_single(n_each[i],
                                 ve_all[i],
                                 lambda_placebo,
                                 lambda_censor,
                                 p_evaluable,
                                 max_followup = max_followup_yr)

        rst     <- rbind(rst,
                         cbind(Arm    = arms_all[i],
                               Arm_id = i,
                               smp_arm))
    }

    ## enroll time
    rst$Enroll       <- smp_enroll
    ## event or censoring time from study begins
    rst$CalendarTime <- rst$Time + rst$Enroll
    ## order arms
    rst$Arm <- factor(rst$Arm, levels = arms_all)

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

    rst_enroll   <- NULL
    rst_duration <- NULL
    rst_count    <- NULL

    for (j in target_event) {
        cur_duration  <- events[j, "CalendarTime"]

        ## samples censored or had event in duration
        cur_smps_1 <- smps_all %>%
            filter(Enroll       <= `cur_duration` &
                   CalendarTime <= `cur_duration`)

        ## otherwise, censor them at the end
        cur_smps_2 <- smps_all %>%
            filter(Enroll       <= `cur_duration` &
                   CalendarTime >  `cur_duration`) %>%
            mutate(Event = 0,
                   Time  = `cur_duration` - Enroll)

        cur_smps   <- rbind(cur_smps_1, cur_smps_2)
        cur_enroll <- cur_smps %>%
            group_by(Arm, Arm_id) %>%
            summarize(N = n()) %>%
            mutate(Target = `j`)

        cur_count <- cur_smps %>%
            filter(Event %in% 0:1) %>%
            group_by(Arm, Arm_id) %>%
            summarize(N_Event = sum(Event),
                      PTime   = sum(Time)) %>%
            mutate(IR = N_Event / PTime) %>%
            data.frame()

        ir_placebo <- cur_count[nrow(cur_count), "IR"]
        cur_count <- cur_count %>%
            rowwise() %>%
            mutate(AbE = 1 - IR / `ir_placebo`) %>%
            mutate(Target = `j`)

        ## expand
        rst_count  <- rbind(rst_count, cur_count)
        rst_enroll <- rbind(rst_enroll,
                            cur_enroll,
                            data.frame(Arm      = "Total",
                                       N        = sum(cur_enroll$N),
                                       Target   = j))
        rst_duration <- rbind(rst_duration,
                              data.frame(Target = j,
                                         Duration = cur_duration))
    }

    rst_enroll <- rst_enroll %>%
        mutate(Arm = factor(Arm,
                            levels = c(levels(rst_count$Arm),
                                       "Total")))

    list(event    = data.frame(rst_count),
         enroll   = data.frame(rst_enroll),
         duration = data.frame(rst_duration))
}



#'  Overall
#'
#'@export
#'
#'
rd_simu_single <- function(ve_trt, target_event, hyp_tests, ir_placebo_1yr, ...,
                           scenario = 1, rep = 1,  seed = NULL) {

    f <- function(dta) {
        cbind(dta,
              Rep            = rep,
              Scenario       = scenario,
              IR_Placebo_1Yr = ir_placebo_1yr)
    }



    if (is.numeric(seed)) {
        old_seed_kind <- RNGkind("L'Ecuyer-CMRG")
        old_seed      <- .Random.seed
        set.seed(seed)
    }

    pts      <- rd_pts_all(ve_trt, ir_placebo_1yr, ...)
    counts   <- rd_count_event(pts, target_event)
    event    <- counts$event
    enroll   <- counts$enroll
    duration <- counts$duration

    rejection  <- NULL
    for (j in target_event) {
        cur_data <- event %>%
            filter(Target == `j`) %>%
            arrange(Arm)
        cur_pvals <- rd_pairwise_pval(cur_data, ...)
        cur_rej   <- rd_rejection_all(cur_pvals, hyp_tests)
        rejection <- rbind(rejection,
                           cbind(Target = j, cur_rej))
    }

    ## reset seed
    if (is.numeric(seed)) {
        set.seed(old_seed)
    }

    ## results
    list(enroll    = f(enroll),
         event     = f(event),
         duration  = f(duration),
         rejection = f(rejection))
}

#' Simulate all
#'
#'@export
#'
rd_simu_all <- function(reps           = 1:2000,
                        scenario       = 1,
                        ir_placebo_1yr = 0.04,
                        n_cores        = 5,
                        update_progress = NULL,
                        ...,
                        seed = NULL) {

    ## system
    if (.Platform$OS.type == "windows" && n_cores > 1) {
        warning("n_cores is set to be 1 on Windows.");
        n_cores <- 1
    }

    is_shiny <- !is.null(update_progress)
    if (is_shiny & n_cores > 1)
        update_progress$set(value = 1, detail = paste(""))

    ## random seed
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    seeds <- abs(rnorm(max(reps)) * 1000000)

    ## simu
    n_reps <- length(reps)
    rst    <- parallel::mclapply(seq_len(n_reps), function(x) {
        if (is_shiny) {
            update_progress$set(value  = x / n_reps,
                                detail = paste("Replication",
                                               x, sep = " "))
        }
        cat("--Rep ", x, "\n")

        rd_simu_single(ir_placebo_1yr = ir_placebo_1yr,
                       scenario       = scenario,
                       rep            = reps[x],
                       ...,
                       seed = seeds[reps[x]])

    }, mc.cores = n_cores)


    ## set seed
    if (!is.null(seed))
        old_seed <- set.seed(seed)

    ## return
    rst
}
