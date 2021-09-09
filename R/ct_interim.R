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

    rst <- parallel::mclapply(reps, function(r) {
        rst <- NULL
        for (i in seq_len(n_inter)) {
            cur_data <- rst_rejection %>%
                filter(Rep    == r               &
                       Target == inter_events[i] &
                       Arm    != "Any Arm") %>%
                arrange(Arm) %>%
                select(Arm, Pval) %>%
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

    data.table::rbindlist(rst)
}

#' Summary Interim
#'
#' @export
#'
rd_inerim_summary_single <- function(inter_rejection) {
    inter_rejection %>%
        group_by(Target, Rep, Arm) %>%
        arrange(desc(Rej), Interim) %>%
        slice(n = 1) %>%
        mutate(Interim = if_else(Rej == 1,
                                  as.character(Interim),
                                 "Failed")) %>%
        group_by(Target, Arm, Interim) %>%
        summarize(N_Rej = n()) %>%
        mutate(Rej = N_Rej / sum(N_Rej)) %>%
        data.frame()

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
                select(Target, Arm, Pval, Rep) %>%
                distinct()

            for (te in target_events) {
                cat(s, r, te, "\n")

                cur_test <- rd_interim_test(cur_data, te, inter_looks, ...)
                cur_rej  <- rd_inerim_summary_single(cur_test)

                cur_rej$Scenario       <- s
                cur_rej$IR_Placebo_1Yr <- r
                cur_rej$Target         <- te
                rst_all <- rbind(rst_all, cur_rej)
            }
        }
    }

    rst_all %>%
        mutate(Interim_Lab = factor(Interim,
                                    levels = c(seq_len(length(inter_looks)),
                                               "Failed"),
                                    labels = c(paste("IF=",
                                                     names(inter_looks),
                                                     sep = ""),
                                               "Failed"))
               )
}
