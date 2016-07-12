## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github(repo="algoquant/rutils")
#  library(rutils)

## ----echo=-1, eval=FALSE-------------------------------------------------
#  library(rutils)
#  # get first six rows of VTI prices
#  head(env_etf$VTI)
#  # plot
#  chart_Series(x=env_etf$VTI["2009-11"])

## ----echo=-1, eval=TRUE--------------------------------------------------
suppressMessages(suppressWarnings(library(rutils)))
# show portfolio symbols
env_etf$sym_bols
# get name for VTI
na_me(env_etf$VTI)
# get first six rows of VTI prices
head(env_etf$VTI)
# get first six rows of price_s
env_etf$price_s[1:6, 1:4]
# get first six rows of re_turns
env_etf$re_turns[1:6, 1:4]

## ----eval=FALSE----------------------------------------------------------
#  # calculate end points with initial stub interval
#  end_points(env_etf$VTI, inter_val=7, off_set=4)

## ----eval=FALSE----------------------------------------------------------
#  # get close prices for VTI
#  clo_se(env_etf$VTI)
#  # get volumes for VTI
#  clo_se(env_etf$VTI, which_col="vol")

## ----eval=FALSE----------------------------------------------------------
#  # lag vector by 2 periods
#  lag_it(1:10, lag=2)
#  # lag matrix by negative 2 periods
#  lag_it(matrix(1:10, ncol=2), lag=-2)

## ----eval=FALSE----------------------------------------------------------
#  # diff vector by 2 periods
#  diff_it(1:10, lag=2)
#  # diff matrix by negative 2 periods
#  diff_it(matrix(1:10, ncol=2), lag=-2)

## ----eval=FALSE----------------------------------------------------------
#  # calculate time differences over lag by 10 periods
#  rutils::diff_xts(env_etf$VTI, lag=10)

## ----eval=FALSE----------------------------------------------------------
#  # create xts time series
#  x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#  # split time series into daily list
#  list_xts <- split(x_ts, "days")
#  # rbind the list back into a time series and compare with the original
#  identical(x_ts, do_call_rbind(list_xts))

## ----eval=FALSE----------------------------------------------------------
#  # create xts time series
#  x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#  # split time series into daily list
#  list_xts <- split(x_ts, "days")
#  # rbind the list back into a time series and compare with the original
#  identical(x_ts, do_call(rbind, list_xts))

## ----eval=FALSE----------------------------------------------------------
#  do_call_assign(
#     func_tion=clo_se,
#     sym_bols=env_etf$sym_bols,
#     out_put="price_s",
#     env_in=env_etf, env_out=new_env)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
chart_xts(env_etf$VTI["2015-11"],
name="VTI in Nov 2015", ylim=c(102, 108),
in_dex=index(env_etf$VTI["2015-11"]) > as.Date("2015-11-18"))

