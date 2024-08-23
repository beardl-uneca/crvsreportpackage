# crvsreportpackage

## Overview

This package contains some useful functions to create a crvs report.

## Installation and Getting Started

You can install this development version from GitHub with:

```r
# install.packages(devtools)
devtools::install_github("tech-acs/crvsreportpackage")
```
 
## Usage

Once you have installed the package, here is a basic example of how to use
__crvsreportpackage__:

```r
library(crvspackage)

# Example Usage
age_grp_80 <- derive_age_groups(ageinyrs,
                                        start_age = 5, max_band = 80,
                                        step_size = 5, under_1 = TRUE)
```


## Documentation

We are currently working on our documentation, this will soon be available in
website format.

## Contributing

We welcome contributions! We will shortly add Contributing Guidelines for details
on our code of conduct and the process for submitting pull requests.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Authors

- Pamela Kakande - Contributor and Maintainer - GitHub profile
- Tesfaye Belay - Contributor and Maintainer - [GitHub profile](https://github.com/tbelay)
- Rachel Shipsey - Contributor - GitHub profile
- Liam Beardsmore - Initial work - [GitHub profile](https://github.com/beardl-ons)
- Henry Partridge (ONS) - Contributor - [GitHub profile](https://github.com/rcatlord)
- Diego Lara (ONS) - Contributor - [GitHub profile](https://github.com/diego-ons)
