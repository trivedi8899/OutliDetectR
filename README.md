# OutliDetectR

`OutliDetectR` is an R package that provides hybrid outlier detection and missing value handling using OMV Score.

## Installation

```r
devtools::install_github("yourusername/OutliDetectR")
```

## Example

```r
library(OutliDetectR)
data(iris)
result <- omv_score(iris[, -5])
head(result)
```

## Author

Ruchi Trivedi (Research Scholar, Statistics)
