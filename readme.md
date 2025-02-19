# Overview

This repository contains code used by the Wyoming Game & Fish Department to model the effects of moon illumination on observed lek attendance in Greater Sage-grouse.

## Files

The main scripts used in this analysis (to be run in order) are:

```
.
├── 00 sampled.R
├── 01 full model.R
├── 02 inference.R
├── MoonPhaseLekAttendanceWGFD.qmd
```

Supporting functions are within the `R` folder.

```
R
├── brms.R
├── db.R
├── deps.R
├── moon_illumination.R
├── prep_data.R
├── save_model.R
├── theme.R
├── time.R
└── utm_to_latlon.R
```

## Package Dependencies

Upon forking/cloning this repo and opening R inside the directory, please install all package dependencies with:

```r
deps()
```

## Documentation and Links to Critical R Packages

* [brms documentation](https://paulbuerkner.com/brms/)
* [suncalc GitHub](https://github.com/datastorm-open/suncalc)