#' Plot Power
#'
#'
#'
#'@export
#'
rd_plot_power <- function(rst_rejection, multi = NULL, power_level = 0.9, ...) {

    if (!is.null(multi))
        rst_rejection <- rst_rejection %>%
            filter(Multi %in% multi)

    ggplot(data = rst_rejection, aes(x = Target, y = Value)) +
        geom_line(aes(group = Arm, color = Arm, lty = Arm)) +
        facet_wrap(~Multi) +
        theme_bw() +
        geom_hline(yintercept = power_level, col = "red", lty = 2) +
        labs(x = "Target Number of Events", y = "Rejection Rate")
}

#' Plot Enroll
#'
#'
#'
#'@export
#'
rd_plot_info <- function(rst_info, lab_y, ...) {
    ggplot(data = rst_info %>%
               filter(Type == `lab_y`),
           aes(x = Target, y = Value)) +
        geom_line(aes(group = Arm, col = Arm, lty = Arm)) +
        theme_bw() +
        labs(x = "Target Number of Events",
             y = lab_y)
}


#'
#'
rd_plot_summary <- function(rst_summary, type, ...) {
    switch(type,
           power = rd_plot_power(rst_summary %>%
                                 dplyr::filter(Type == "Power"),
                                 ...),
           rd_plot_info(rst_summary, type, ...))
}
