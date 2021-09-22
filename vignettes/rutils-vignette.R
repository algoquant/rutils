## ----eval=FALSE----------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github(repo="algoquant/rutils")
#  library(rutils)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
# show list of ETF time series in etf_env
rutils::etf_env$sym_bols
# get first six rows of VTI prices
head(rutils::etf_env$VTI)
# plot
quantmod::chart_Series(x=rutils::etf_env$VTI["2009-11"])

## ----echo=TRUE, eval=FALSE-----------------------------------------------
#  suppressMessages(suppressWarnings(library(rutils)))
#  # new environment for data
#  etf_env <- new.env()
#  # download data and copy it into environment
#  get_symbols("XOM", env_out=etf_env, start_date="1990-01-01")
#  # plot
#  x11()
#  quantmod::chart_Series(x=rutils::etf_env$XOM["2016/"], TA="add_Vo()", name="XOM stock")

## ----echo=-1, eval=TRUE--------------------------------------------------
suppressMessages(suppressWarnings(library(rutils)))
# get name for VTI
get_name(colnames(rutils::etf_env$VTI)[1])
# get first six rows of VTI prices
head(rutils::etf_env$VTI)
# get first six rows of price_s
rutils::etf_env$price_s[1:6, 1:4]
# get first six rows of re_turns
rutils::etf_env$re_turns[1:6, 1:4]

## ----eval=FALSE----------------------------------------------------------
#  # calculate end points with initial stub interval
#  calc_endpoints(rutils::etf_env$VTI, inter_val=7)

## ----eval=FALSE----------------------------------------------------------
#  # get close prices for VTI
#  get_col(rutils::etf_env$VTI)
#  # get volumes for VTI
#  get_col(rutils::etf_env$VTI, col_name="vol")

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
#  diff_xts(rutils::etf_env$VTI, lag=10)

## ----eval=FALSE----------------------------------------------------------
#  # create xts time series
#  x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#  # split time series into daily list
#  list_xts <- split(x_ts, "days")
#  # rbind the list back into a time series and compare with the original
#  identical(x_ts, rutils::do_call_rbind(list_xts))

## ----eval=FALSE----------------------------------------------------------
#  # create xts time series
#  x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#  # split time series into daily list
#  list_xts <- split(x_ts, "days")
#  # rbind the list back into a time series and compare with the original
#  identical(x_ts, rutils::do_call(rbind, list_xts))

## ----eval=FALSE----------------------------------------------------------
#  rutils::do_call_assign(
#     func_tion=get_col,
#     sym_bols=rutils::etf_env$sym_bols,
#     out_put="price_s",
#     env_in=etf_env, env_out=new_env)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
chart_xts(rutils::etf_env$VTI["2015-11"], x_11=FALSE,
          name="VTI in Nov 2015", ylim=c(102, 108),
          in_dic=index(rutils::etf_env$VTI["2015-11"]) > as.Date("2015-11-18"))

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
# select VTI
oh_lc <- rutils::etf_env$VTI
# calculate volume-weighted average price
v_wap <- rutils::roll_sum(x_ts=oh_lc[, 4]*oh_lc[, 5], look_back=22)
volume_rolling <- rutils::roll_sum(x_ts=oh_lc[, 5], look_back=22)
v_wap <- v_wap/volume_rolling
v_wap[is.na(v_wap)] <- 0
# plot candlesticks with vertical background shading and trading volume.
rutils::chart_xts(oh_lc["2016"], x_11=FALSE,
                  name="VTI plus VWAP",
                  TA="add_Vo(); add_TA(v_wap['2016'], col='red', lwd=2, on=1)",
                  in_dic=(oh_lc["2016", 4] > v_wap["2016"]))

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
rutils::chart_xts2y(cbind(quantmod::Cl(rutils::etf_env$VTI), 
                          quantmod::Cl(rutils::etf_env$IEF))["2016"], 
                    x_11=FALSE)

## ----echo=-1, eval=TRUE, fig.width=6, fig.height=4-----------------------
suppressMessages(suppressWarnings(library(rutils)))
oh_lc <- rutils::etf_env$VTI
v_wap <- TTR::VWAP(price=quantmod::Cl(oh_lc), volume=quantmod::Vo(oh_lc), n=20)
oh_lc <- cbind(oh_lc[, c(1:4)], v_wap)["2016"]
rutils::chart_dygraph(oh_lc, in_dic=(oh_lc[, 4] > v_wap))

