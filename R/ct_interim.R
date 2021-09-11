#' Get interim test results
#'
#'
#' @export
#'
rd_interim_test <- function(rst_rejection, target_event,
                            inter_looks,
                            hyp_test, n_cores = 10, ...) {

    inter_fractions <- as.numeric(names(inter_looks))
    inter_alphas    <- as.numeric(inter_looks)
    inter_events    <- ceiling(target_event * inter_fractions)
    n_inter         <- length(inter_fractions)
    reps            <- unique(rst_rejection$Rep)

    rst_rejection   <- rst_rejection %>%
        filter(Arm != "Any Arm")

    rst <- parallel::mclapply(reps, function(r) {
        rst <- NULL
        for (i in seq_len(n_inter)) {
            cur_data <- rst_rejection %>%
                filter(Rep    == r               &
                       Target == inter_events[i]) %>%
                arrange(Arm) %>%
                select(Arm, Pval, Abe) %>%
                data.frame()

            cur_rej <- hyp_test(cur_data$Pval,
                                alpha = inter_alphas[i],
                                ...)
            cur_rst <- cbind(Target  = target_event,
                             Rep     = r,
                             Interim = i,
                             Interim_Event = inter_events[i],
                             Interim_Alpha = inter_alphas[i],
                             cur_data,
                             Rej = cur_rej)
            rst <- rbind(rst, cur_rst)
        }
        rst
    }, mc.cores = n_cores)

    rst <- data.table::rbindlist(rst)

    list(interim = rst,
         n_reps  = length(reps))
}

#' Summary Interim
#'
#' @export
#'
rd_inerim_summary_single <- function(inter_rejection) {
    n_reps <- inter_rejection$n_reps
    rst    <- inter_rejection$interim %>%
        filter(Rej == 1) %>%
        group_by(Target, Rep, Arm) %>%
        arrange(Interim) %>%
        slice(n = 1) %>%
        group_by(Target, Arm, Interim) %>%
        summarize(N_Rej = n(),
                  Min_Abe = min(Abe)) %>%
        mutate(Rej = N_Rej / n_reps) %>%
        mutate(CumuRej = cumsum(Rej)) %>%
        data.frame()

    rst
}

#' Summary all
#'
#' @export
#'
rd_interim_summary <- function(rst_rejection, target_events,
                               inter_looks, ...) {
    sce <- unique(rst_rejection$Scenario)
    ir  <- unique(rst_rejection$IR_Placebo_1Yr)

    rst_all <- NULL
    for (s in sce) {
        for (r in ir) {
            cur_data <- rst_rejection %>%
                filter(Scenario == s &
                       IR_Placebo_1Yr == r) %>%
                select(Target, Arm, Pval, Abe, Rep) %>%
                distinct()

            for (te in target_events) {
                cat(s, r, te, "\n")

                cur_test <- rd_interim_test(cur_data, te, inter_looks, ...)
                cur_rej  <- rd_inerim_summary_single(cur_test)

                cur_rej$Scenario       <- s
                cur_rej$IR_Placebo_1Yr <- r
                cur_rej$Target         <- te
                rst_all                <- rbind(rst_all, cur_rej)
            }
        }
    }

    rst_all %>%
        mutate(Interim_Lab = factor(Interim,
                                    levels = seq_len(length(inter_looks)),
                                    labels = paste("IF=",
                                                   names(inter_looks),
                                                   sep = "")))
}
