#' Summarize results
#'
#'@export
#'
rd_summary <- function(rst_simu) {
    rst_enroll <- rst_simu$enroll %>%
        group_by(Scenario, IR_Placebo_1Yr, Target, Arm) %>%
        summarize(Rep   = n(),
                  Value = mean(N),
                  Type  = "Total Enroll")

    rst_duration <- rst_simu$duration %>%
        group_by(Scenario, IR_Placebo_1Yr, Target) %>%
        summarize(Rep   = n(),
                  Value = mean(Duration)) %>%
        mutate(Arm = "Total",
               Type = "Duration")

    rst_rejection <- rst_simu$rejection %>%
        group_by(Scenario, IR_Placebo_1Yr, Target, Arm, Multi) %>%
        summarize(Rep   = n(),
                  Value = mean(Rej),
                  Type  = "Power")

    rst_event <- rst_simu$event %>%
        group_by(Scenario, IR_Placebo_1Yr, Target, Arm) %>%
        summarize(Rep = n(),
                  Observed_Event = mean(N_Event),
                  Observed_IR    = mean(IR),
                  Observed_AbE   = mean(AbE),
                  SD_Events      = sd(N_Event),
                  SD_IR          = sd(IR),
                  SD_AbE         = sd(AbE)) %>%
        gather(key   = "Type",
               value = "Value",
               Observed_Event,
               Observed_IR,
               Observed_AbE,
               SD_Events,
               SD_IR,
               SD_AbE
               )

    rst_info <- rbind(rst_enroll, rst_duration, rst_event) %>%
        mutate(Multi = NA) %>%
        rbind(rst_rejection) %>%
        mutate(Arm = factor(Arm,
                            levels = c(levels(rst_event$Arm),
                                       "Any Arm", "Total")),
               Scenario = factor(Scenario),
               Type     = factor(Type),
               Multi    = factor(Multi))
    rst_info
}

#' Get target number of events
#'
#'
#' @export
rd_get_results <- function(rst_summary,
                           multi = NULL,
                           power_by = NULL,
                           power_level = 0.9) {

    f_j0 <- function(dta) {
        dta %>%
            left_join(rst_rejection %>%
                      filter(Arm != "Any Arm") %>%
                      rename(Arm_Specific_Power = Power) %>%
                      select(Scenario, IR_Placebo_1Yr, Target, Arm, Multi,
                             Arm_Specific_Power))
    }

    f_j1 <- function(dta) {
        dta %>%
            left_join(rst_summary %>%
                      filter(Type == "Duration") %>%
                      mutate(Duration = Value) %>%
                      select(Scenario, IR_Placebo_1Yr, Target, Rep,
                             Duration)) %>%
            left_join(rst_summary %>%
                      filter(Type == "Total Enroll" &
                             Arm  == "Total") %>%
                      mutate(SampleSize = Value) %>%
                      select(Scenario, IR_Placebo_1Yr, Target,
                             SampleSize))
    }

    f_j2 <- function(dta) {
        stats <- c("Observed_Event",
                   "Observed_IR",
                   "Observed_AbE",
                   "SD_Events",
                   "SD_IR",
                   "SD_AbE")

        for (s in stats) {
            dta <- dta %>%
                left_join(rst_summary %>%
                          filter(Type == s) %>%
                          rename(!!s := Value) %>%
                          select(Scenario, IR_Placebo_1Yr, Target, Arm, !!s))
        }

        dta
    }

    rst_rejection <- rst_summary %>%
        filter(Type == "Power") %>%
        select(-Type) %>%
        rename(Power = Value)

    if (!is.null(multi)) {
        rst_rejection <- rst_rejection %>%
            filter(Multi %in% `multi`)
    }

    if (!is.null(power_by)) {
        rst_rejection <- rst_rejection %>%
            filter(Arm %in% `multi`)
    }

    rst_ss <- rst_rejection %>%
        filter(Power >= `power_level`) %>%
        group_by(Scenario, IR_Placebo_1Yr, Arm, Multi) %>%
        arrange(Target) %>%
        slice_head(n = 1) %>%
        rename(Power_By = Arm)

    rst_details <- rst_ss %>%
        f_j2() %>%
        f_j0()

    rst_ss <- rst_ss %>%
        f_j1()

    ## return
    list(samplesize = rst_ss,
         details    = rst_details)
}

#' Combine simu list
#'
#' @export
rd_combine_lst <- function(lst_rst) {
    items   <- names(lst_rst[[1]])
    n_items <- length(items)

    all_lists <- rep(list(NULL), n_items)
    for (cur_rst in lst_rst) {
        for (j in seq_len(n_items))
            all_lists[[j]] <- c(all_lists[[j]], list(cur_rst[[j]]))
    }

    all_rst <- list()
    for (i in seq_len(n_items))
        all_rst[[i]] <- data.table::rbindlist(all_lists[[i]])

    names(all_rst) <- items

    all_rst
}

#' Combine simulation results
#'
#'@export
#'
rd_combine_files <- function(reps = 1:100, prefix = "./Results/rst_") {

    lst_simu <- NULL
    for (i in reps) {
        cur_f <- paste(prefix, i, ".Rdata", sep = "")
        if (!file.exists(cur_f)) {
            print(cur_f)
            next
        }
        print(i)

        lst_simu <- c(lst_simu, get(load(cur_f)))
    }

    rd_combine_lst(lst_simu)
}
