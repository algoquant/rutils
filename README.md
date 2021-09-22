[![Build Status](https://travis-ci.org/algoquant/rutils.svg?branch=master)](https://travis-ci.org/algoquant/rutils)

### Utility functions for simplifying financial data management and modeling

The *rutils* package contains functions for:

-   extracting names and columns from time series,

-   calculating the end points of a time series,

-   applying lags to vectors and matrices,

-   calculating row differences of vectors and matrices,

-   recursively binding a list of objects into time series,

-   recursively applying a function to a list of objects,

-   plotting time series with custom axis range and background shading.

The *rutils* package also includes a dataset with *OHLC* time series data for a portfolio of symbols. The data is contained in an environment called *etf\_env*, which includes:

-   *sym\_bols*: a vector of strings with the portfolio symbols,

-   individual time series *VTI*, *VEU*, etc., containing daily *OHLC* prices for all the sym\_bols,

-   *price\_s*: a single *xts* time series containing daily closing prices for all the sym\_bols,

-   *re\_turns*: a single *xts* time series containing daily returns for all the sym\_bols.

Each individual *xts* time series contains the columns: Open prices, High prices, Low prices, Close prices, trading Volume, Adjusted prices.

========

### Installation and loading

Install package *rutils* from github:

``` r
install.packages("devtools")
devtools::install_github(repo="algoquant/rutils")
library(rutils)
```

<br>

Install package *rutils* from source on local drive:

``` r
install.packages(pkgs="C:/Develop/R/rutils", repos=NULL, type="source")
# Install package from source on local drive using R CMD
R CMD INSTALL C:\Develop\R\rutils
library(rutils)
```

<br>

Build reference manual for package *rutils* from *.Rd* files:

``` r
system("R CMD Rd2pdf C:/Develop/R/rutils")
R CMD Rd2pdf C:\Develop\R\rutils
```

<br>

========

### Data

The *rutils* package contains a dataset of daily *OHLC* time series in *xts* format, for a portfolio of stock symbols. The time series are contained in an environment called *etf\_env*. The data is set up for lazy loading, so it doesn't require calling `data(etf_data)` to load it before being able to call it.

``` r
# get first six rows of OHLC prices
head(etf_env$VTI)
# plot
chart_Series(x=etf_env$VTI["2009-11"])
```

========

### Examples

Extract the name of an *OHLC* time series from its first column name:

``` r
# get name for VTI
get_name(colnames(rutils::etf_env$VTI)[1])
```

<br>

Calculate a vector of equally spaced end points for a time series:

``` r
# calculate end points with initial stub interval
calc_endpoints(etf_env$VTI, inter_val=7, off_set=4)
```

<br>

Extract columns of prices from an *OHLC* time series:

``` r
# get close prices for VTI
get_col(etf_env$VTI)
# get volumes for VTI
get_col(etf_env$VTI, col_name="vol")
```

<br>

Apply a *lag* to a vector or matrix:

``` r
# lag vector by 2 periods
lag_it(1:10, lag=2)
# lag matrix by negative 2 periods
lag_it(matrix(1:10, ncol=2), lag=-2)
```

<br>

Calculate the row differences of a vector or matrix:

``` r
# diff vector by 2 periods
diff_it(1:10, lag=2)
# diff matrix by negative 2 periods
diff_it(matrix(1:10, ncol=2), lag=-2)
```

<br>

Calculate the time differences of an *xts* time series and pad with zeros:

``` r
# calculate time differences over lag by 10 periods
rutils::diff_xts(etf_env$VTI, lag=10)
```

<br>

Recursively *rbind* a list of objects:

``` r
# create xts time series
x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
# split time series into daily list
list_xts <- split(x_ts, "days")
# rbind the list back into a time series and compare with the original
identical(x_ts, do_call_rbind(list_xts))
```

<br>

Recursively *apply* a function to a list of objects:

``` r
# create xts time series
x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
# split time series into daily list
list_xts <- split(x_ts, "days")
# rbind the list back into a time series and compare with the original
identical(x_ts, do_call(rbind, list_xts))
```

<br>

Apply a function to a list of objects, merge the outputs into a single object, and *assign* the object to the output environment:

``` r
do_call_assign(
   func_tion=clo_se,
   sym_bols=etf_env$sym_bols,
   out_put="price_s",
   env_in=etf_env, env_out=new_env)
```

<br>

Plot an *xts* time series with custom y-axis range and with vertical background shading:

``` r
chart_xts(etf_env$VTI["2015-11"],
name="VTI in Nov 2015", ylim=c(102, 108),
in_dex=index(etf_env$VTI["2015-11"]) > as.Date("2015-11-18"))
```
