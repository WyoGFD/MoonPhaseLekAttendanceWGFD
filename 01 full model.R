# this script is to run the final model

# data
x <- prep_data()

# model
br_final <- brms::brm(
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

# save
save_model(br_final)

# sniff check
brms::conditional_effects(br_final)
