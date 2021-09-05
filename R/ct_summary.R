#' Combine simulation results
#'
#'@export
#'
rd_simu_combine <- function(reps = 1:100, prefix = "./Results/rst_") {
    rst_enroll    <- NULL
    rst_rejection <- NULL
    rst_event     <- NULL
    rst_duration  <- NULL
    for (i in reps) {
        cur_f <- paste(prefix, i, ".Rdata", sep = "")
        if (!file.exists(cur_f)) {
            print(cur_f)
            next
        }

        ss <- get(load(cur_f))
        for (j in ss) {
            rst_enroll    <- rbind(rst_enroll,    j$enroll)
            rst_rejection <- rbind(rst_rejection, j$rejection)
            rst_event     <- rbind(rst_event,     j$event)
            rst_duration  <- rbind(rst_duration,  j$duration)
        }
    }

    list(enroll    = rst_enroll,
         event     = rst_event,
         duration  = rst_duration,
         rejection = rst_rejection)
}


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
                  `Observed Events` = mean(N_Event),
                  `Observed IR`     = mean(IR),
                  `Observed AbE`    = mean(AbE)) %>%
        gather(key = "Type",
               value = "Value",
               `Observed Events`,
               `Observed IR`,
               `Observed AbE`)

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

    f_j1 <- function(dta) {
        dta %>%
            left_join(rst_summary %>%
                      filter(Type == "Duration") %>%
                      mutate(Duration = Value) %>%
                      select(Target, Rep, Duration)) %>%
            left_join(rst_summary %>%
                      filter(Type == "Total Enroll" &
                             Arm  == "Total") %>%
                      mutate(SampleSize = Value) %>%
                      select(Target, SampleSize))
    }

    f_j2 <- function(dta) {
        dta %>%
            left_join(rst_summary %>%
                      filter(Type == "Observed AbE") %>%
                      mutate(Observed_AbE = Value) %>%
                      select(Target, Arm, Observed_AbE)) %>%
            left_join(rst_summary %>%
                      filter(Type == "Observed IR") %>%
                      mutate(Observed_IR = Value) %>%
                      select(Target, Arm, Observed_IR)) %>%
            left_join(rst_summary %>%
                      filter(Type == "Observed Events") %>%
                      mutate(Observed_Event = Value) %>%
                      select(Target, Arm, Observed_Event))
    }

    rst_rejection <- rst_summary %>%
        filter(Type == "Power") %>%
        select(-Type)

    if (!is.null(multi)) {
        rst_rejection <- rst_rejection %>%
            filter(Multi %in% `multi`)
    }

    if (!is.null(power_by)) {
        rst_rejection <- rst_rejection %>%
            filter(Arm %in% `multi`)
    }


    rst_rejection %>%
        filter(Value >= `power_level`) %>%
        group_by(Arm, Multi) %>%
        arrange(Value) %>%
        slice_head(n = 1) %>%
        rename(Power_By = Arm,
               Power    = Value) %>%
        f_j1() %>%
        f_j2()
}
