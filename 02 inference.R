# this script is to generate figures + tables for writeup

# read model
br <- readRDS("./Models/br_final.rds")

# extract data from model
dat <- insight::get_data(br) |> data.table::as.data.table()

# to obtain "conditional effects" plots for the random slopes,
# we must hold other predictors constant
grid <- expand.grid(
    LekMGMT = unique(dat$LekMGMT),
    MoonFraction = seq(0, 1, by = 0.01),
    Time = mean(dat$Time), # mean of time is what ce does
    WOY = mean(dat$WOY), # mean of WOY is also what ce does
    Year = 2024 # ce takes mean of year but probably best to use latest year
)

# get epred off of grid
pr <- epred(br, newdata = grid)

# get draw summaries and order
pr_summary <- pr[, sum_epred(epred), by = .(LekMGMT, MoonFraction)]
pr_summary <- pr_summary[order(LekMGMT, MoonFraction)]

# random slope plots
moon_by_mgmt <- pr_summary |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = MoonFraction,
            y = epred,
            ymin = epred.lower,
            ymax = epred.upper,
            color = LekMGMT,
            fill = LekMGMT
        )
    ) +
    ggdist::geom_lineribbon(alpha = 0.5) +
    ggplot2::ylab("Predicted Males") +
    ggplot2::xlab("Moon Illumination (1 = Full Moon)") +
    ggplot2::labs(fill = "Management Area", color = "Management Area") +
    ggplot2::ggtitle(
        "Effect of Moon Illumination on Observed Lek Attendance by Management Area",
        subtitle = "2000-2024, Holding WOY, Survey Time, and Year constant"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    theme_wgfd() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_wrap(~LekMGMT, scales = "free")

formal_blue <- "#00374D"
meadowlark <- "#FFC80B"

# overall plot of moon fraction
moon <- conditional_effects(br, "MoonFraction", plot = FALSE) |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = moon_fraction,
            y = estimate,
            ymin = lower,
            ymax = upper
        )
    ) +
    ggdist::geom_lineribbon(
        alpha = 0.5,
        color = formal_blue,
        fill = formal_blue
    ) +
    ggplot2::ylab("Predicted Males") +
    ggplot2::xlab("Moon Illumination (1 = Full Moon)") +
    ggplot2::ggtitle(
        "Overall Effect of Moon Illumination on Observed Lek Attendance",
        subtitle = "2000-2024, Holding WOY, Survey Time, and Year constant"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0.01), breaks = seq(0, 1, by = 0.25)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 50, by = 1)) +
    theme_wgfd()

# overall effect of time
time <- conditional_effects(br, "Time", plot = FALSE) |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = time,
            y = estimate,
            ymin = lower,
            ymax = upper
        )
    ) +
    ggdist::geom_lineribbon(
        alpha = 0.5,
        color = formal_blue,
        fill = formal_blue
    ) +
    ggplot2::ylab("Predicted Males") +
    ggplot2::xlab("Time") +
    ggplot2::ggtitle(
        "Overall Effect of Survey Time on Observed Lek Attendance",
        subtitle = "2000-2024, Holding WOY, Moon Illumination, and Year constant"
    ) +
    ggplot2::scale_x_continuous(
        expand = c(0, 0),
        labels = function(x) paste0(x, "AM"),
        breaks = 4:9
    ) +
    theme_wgfd()

# overall effect of WOY
week_of_year <- conditional_effects(br, "WOY", plot = FALSE) |>
    _[data.table::between(woy, 5, 24)] |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = woy,
            y = estimate,
            ymin = lower,
            ymax = upper
        )
    ) +
    ggdist::geom_lineribbon(
        alpha = 0.5,
        color = formal_blue,
        fill = formal_blue
    ) +
    ggplot2::ylab("Predicted Males") +
    ggplot2::xlab("Approximate Date of Week of Year") +
    ggplot2::ggtitle(
        "Overall Effect of Week of Year on Observed Lek Attendance",
        subtitle = "2000-2024, Holding Survey Time, Moon Illumination, and Year constant"
    ) +
    ggplot2::scale_x_continuous(
        expand = c(0, 0),
        labels = function(x) {
            data.table::fcase(
                x == 5, "Feb 1",
                x == 7, "Feb 14",
                x == 9, "March 1",
                x == 11, "March 14",
                x == 14, "Apr 1",
                x == 16, "Apr 14",
                x == 18, "May 1",
                x == 20, "May 14",
                x == 22, "June 1",
                x == 24, "June 14"
            )
        },
        breaks = c(5, 7, 9, 11, 14, 16, 18, 20, 22, 24)
    ) +
    theme_wgfd()

# overall effect of year
year <- conditional_effects(br, "Year", plot = FALSE) |>
    ggplot2::ggplot(
        ggplot2::aes(
            x = year,
            y = estimate,
            ymin = lower,
            ymax = upper
        )
    ) +
    ggdist::geom_lineribbon(
        alpha = 0.5,
        color = formal_blue,
        fill = formal_blue
    ) +
    ggplot2::ylab("Predicted Males") +
    ggplot2::xlab("Year") +
    ggplot2::ggtitle(
        "Overall Effect of Year on Observed Lek Attendance",
        subtitle = "2000-2024, Holding WOY, Moon Illumination, and Survey Time constant"
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    theme_wgfd()

# plot of trend over year
year_by_mgmt <- conditional_effects(br, "Year:LekMGMT") +
    ggplot2::ylab("Predicted Males") +
    ggplot2::labs(fill = "Management Area", color = "Management Area") +
    ggplot2::ggtitle(
        "Observed Attendance Trends 2000-2024 by Management Area",
        subtitle = "Holding WOY, Survey Time, and Moon Illumination constant"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2020, by = 10)) +
    theme_wgfd() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_wrap(~LekMGMT)

# ggsave for proper rendering (aspect ratio) ----

ggplot2::ggsave(
    "./Plots/moon.png",
    plot = moon,
    height = 5,
    width = 8,
    units = c("in"),
    bg = "white"
)

ggplot2::ggsave(
    "./Plots/moon_mgmt.png",
    plot = moon_by_mgmt,
    height = 10,
    width = 8,
    units = c("in"),
    bg = "white"
)

ggplot2::ggsave(
    "./Plots/time.png",
    plot = time,
    height = 4.5,
    width = 8,
    units = c("in"),
    bg = "white"
)

ggplot2::ggsave(
    "./Plots/year.png",
    plot = year,
    height = 4.5,
    width = 8,
    units = c("in"),
    bg = "white"
)

ggplot2::ggsave(
    "./Plots/woy.png",
    plot = week_of_year,
    height = 4.5,
    width = 8,
    units = c("in"),
    bg = "white"
)

ggplot2::ggsave(
    "./Plots/year_mgmt.png",
    plot = year_by_mgmt,
    height = 10,
    width = 8,
    units = c("in"),
    bg = "white"
)

# summaries for % decrease + significance ----

fix <- brms::fixef(br, probs = c(0.005, 0.995)) |>
    as.data.frame() |>
    tibble::rownames_to_column("b") |>
    data.table::as.data.table() |>
    _[, Estimate := round(Estimate, digits = 2)]

fix <- fix[, Intercept := fix[b == "Intercept", Estimate]] |>
    _[b %in% c("MoonFraction")]

fix <- fix[, .(mgmt = "Statewide", int = Intercept, moon = Estimate, q0.5 = Q0.5, q99.5 = Q99.5)]

mgmt <- coef(br, probs = c(0.005, 0.995))[[1]] |>
    as.data.frame() |>
    tibble::rownames_to_column("MGMT") |>
    data.table::as.data.table()
colnames(mgmt) <- snakecase::to_snake_case(colnames(mgmt))
mgmt <- mgmt[, .(
    mgmt,
    int = estimate_intercept,
    moon = estimate_moon_fraction,
    q0.5 = q_0_5_moon_fraction,
    q99.5 = q_99_5_moon_fraction
)]


sum <- rbind(fix, mgmt)
rm(fix, mgmt)

sum[, percent_decrease := (1 - ((int + moon) / int)) * 100]
sum[, significant := !data.table::between(0, q0.5, q99.5)]
sum[, significant := data.table::fifelse(
    significant == TRUE, "$\\star$", ""
)]
sum[,
    c("int", "moon", "q0.5", "q99.5", "percent_decrease") := lapply(.SD, round, digits = 2),
    .SDcols = c("int", "moon", "q0.5", "q99.5", "percent_decrease")
]
