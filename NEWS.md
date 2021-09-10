# didImputation 0.0.3 (2021-10-09)

## Features

* `didImputation` now supports triple difference estimation with `td` option.

## Bugs

* Rename internal variables to avoid conflict with user input.

## Misc

* Updated `README`.
* A vignette is now available.

# didImputation 0.0.2 (2021-08-05)

## Features

* New function `generateDidData()` to create fake data.
* `didImputation` now supports sample weights.

## Bugs

* Now raise error when `fixest::predict()` cannot extract fixed-effects.

## Misc

* Added a `NEWS.md` file to track changes to the package.
* Update `README` with estimation method.
