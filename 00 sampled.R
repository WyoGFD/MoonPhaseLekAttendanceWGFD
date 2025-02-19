# setup ----

# this script is to try out some models on a subset of the data before
# running through the whole computationally-expensive model

# data
x <- prep_data()

# sample
set.seed(123)
x <- x |>
    dplyr::group_by(LekMGMT) |>
    dplyr::slice_sample(n = 3000) |>
    data.table::as.data.table()

# lm ----

lm(Males ~ MoonFraction, data = x) |> summary()

gam_lm <- mgcv::gam(
    Males ~
        1 +
        # constrain spline with 3 knots to reduce noise
        s(MoonFraction, bs = "tp", k = 3),
    data = x
)

# basically the same as an lm
plot(gam_lm)

# gam ----

# in essence, using gams to see how mgcv::s()
# affects estimates before throwing into brms

# 1:
# spline on year, thin-plate spline on WOY in form of quadratic
gam_1 <- mgcv::gam(
    Males ~ 1 +
        MoonFraction +
        s(Year) +
        s(
            WOY,
            bs = "tp",
            k = 3
        ),
    data = x
)

# resonable so far
plot(gam_1)

# 2:
# do splines on WOY differ by Mgmt Area?
gam_2 <- mgcv::gam(
    Males ~ 1 +
        MoonFraction +
        s(Year) +
        s(
            WOY,
            bs = "tp",
            k = 3,
            by = LekMGMT
        ),
    data = x
)

# things start to get weird...
# A for example, looks like it only ever gets visited around the same time
# so it gets really tight in the middle but the other ends are basically
# worthless, just noise.
plot(gam_2)

# 3:
# what about  effect of year by mgmt?
gam_3 <- mgcv::gam(
    Males ~ 1 +
        MoonFraction +
        s(Year, by = LekMGMT) +
        s(
            WOY,
            bs = "tp",
            k = 3
        ),
    data = x
)

# mgmt A is looks off but only like 20 leks there so probably fine?
plot(gam_3)

# 4:
# what about  time
gam_4 <- mgcv::gam(
    Males ~ 1 +
        MoonFraction +
        s(Year, by = LekMGMT) +
        s(
            WOY,
            bs = "tp",
            k = 3
        ) +
        s(Time, bs = "tp", k = 3),
    data = x
)

# time looks about as you would expect
plot(gam_4)

# brms ----

# 1:
# only MoonFraction
br_1 <- brms::brm(
    Males ~ 1 +
        MoonFraction,
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95)
)

# 2:
# random slopes by mgmt
br_2 <- brms::brm(
    Males ~ 1 +
        MoonFraction +
        (1 + MoonFraction | LekMGMT),
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95),
    threads = brms::threading(4) # threading for more intense models
)

# much better
brms::loo(br_1, br_2)

# 3:
# now with WOY
br_3 <- brms::brm(
    Males ~ 1 +
        MoonFraction +
        s(WOY, bs = "tp", k = 3) +
        (1 + MoonFraction | LekMGMT),
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95),
    threads = brms::threading(4) # threading for more intense models
)

# loo
brms::loo(br_1, br_2, br_3)

# 4:
# now with Time
br_4 <- brms::brm(
    Males ~ 1 +
        MoonFraction +
        s(WOY, bs = "tp", k = 3) +
        s(Time, bs = "tp", k = 3) +
        (1 + MoonFraction | LekMGMT),
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95),
    threads = brms::threading(4) # threading for more intense models
)

# loo
brms::loo(br_1, br_2, br_3, br_4)

# 5:
# now with s(Year)
br_5 <- brms::brm(
    Males ~ 1 +
        MoonFraction +
        s(WOY, bs = "tp", k = 3) +
        s(Time, bs = "tp", k = 3) +
        s(Year) +
        (1 + MoonFraction | LekMGMT),
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95),
    threads = brms::threading(4)
)

# loo
brms::loo(br_1, br_2, br_3, br_4, br_5)

# 6:
# now with s(Year, by = "LekMGMT")
br_6 <- brms::brm(
    Males ~ 1 +
        MoonFraction +
        s(WOY, bs = "tp", k = 3) +
        s(Time, bs = "tp", k = 3) +
        s(Year, by = LekMGMT) +
        (1 + MoonFraction | LekMGMT),
    data = x,
    chains = 4,
    cores = 4,
    iter = 1000,
    backend = "cmdstanr",
    control = list(adapt_delta = .95),
    threads = brms::threading(4)
)

# loo
brms::loo(br_1, br_2, br_3, br_4, br_5, br_6)

# save
save_model(br_1)
save_model(br_2)
save_model(br_3)
save_model(br_4)
save_model(br_5)
save_model(br_6)

# preds + plots ----

brms::conditional_effects(br_6)
