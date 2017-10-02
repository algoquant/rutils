## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github(repo="algoquant/rutils")
#  library(rutils)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
# show list of ETF time series in env_etf
rutils::env_etf$sym_bols
# get first six rows of VTI prices
head(rutils::env_etf$VTI)
# plot
quantmod::chart_Series(x=rutils::env_etf$VTI["2009-11"])

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  suppressMessages(suppressWarnings(library(rutils)))
#  # new environment for data
#  env_etf <- new.env()
#  # download data and copy it into environment
#  get_symbols("XOM", env_out=env_etf, start_date="1990-01-01")
#  # plot
#  x11()
#  quantmod::chart_Series(x=rutils::env_etf$XOM["2016/"], TA="add_Vo()", name="XOM stock")

## ----echo=-1, eval=TRUE--------------------------------------------------
suppressMessages(suppressWarnings(library(rutils)))
# get name for VTI
get_name(colnames(rutils::env_etf$VTI)[1])
# get first six rows of VTI prices
head(rutils::env_etf$VTI)
# get first six rows of price_s
rutils::env_etf$price_s[1:6, 1:4]
# get first six rows of re_turns
rutils::env_etf$re_turns[1:6, 1:4]

## ----eval=FALSE----------------------------------------------------------
#  # calculate end points with initial stub interval
#  calc_endpoints(rutils::env_etf$VTI, inter_val=7, off_set=4)

## ----eval=FALSE----------------------------------------------------------
#  # get close prices for VTI
#  get_col(rutils::env_etf$VTI)
#  # get volumes for VTI
#  get_col(rutils::env_etf$VTI, col_name="vol")

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
#  diff_xts(rutils::env_etf$VTI, lag=10)

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
#     func_tion=get_col,
#     sym_bols=rutils::env_etf$sym_bols,
#     out_put="price_s",
#     env_in=env_etf, env_out=new_env)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
chart_xts(rutils::env_etf$VTI["2015-11"],
name="VTI in Nov 2015", ylim=c(102, 108),
in_dic=index(rutils::env_etf$VTI["2015-11"]) > as.Date("2015-11-18"))

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
rutils::chart_xts2y(cbind(quantmod::Cl(rutils::env_etf$VTI), 
                          quantmod::Cl(rutils::env_etf$IEF))["2015"], 
                    x_11=FALSE)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
oh_lc <- rutils::env_etf$VTI
v_wap <- TTR::VWAP(price=quantmod::Ad(oh_lc), volume=quantmod::Vo(oh_lc), n=20)
oh_lc <- cbind(oh_lc[, c(1:3, 6)], v_wap)["2009-02/2009-04"]
rutils::chart_dygraph(oh_lc, in_dic=(oh_lc[, 4] > v_wap))

