################################################
###
### Demos for using package 'rutils'
###
################################################

# Set the time-zone to New_York
Sys.setenv(TZ="America/New_York")
# setwd("C:/Develop/data")
# search()  # get search path
options(digits.secs=6)
options(digits=7)
# suppress spurious timezone warning messages
options(xts_check_TZ=FALSE)
options(stringsAsFactors=FALSE)
options(max.print=80)
rm(list=ls())  # remove all objects

# install rutils from GitHub
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/rutils")

# install rutils ffrom local drive using install.packages()
install.packages(pkgs="C:/Develop/R/rutils", repos=NULL, type="source")
install.packages(pkgs="C:/Develop/R/rutils", repos=NULL, type="source", lib="C:/Users/Jerzy/Downloads")
install.packages(pkgs="C:/Develop/R/rutils", repos=NULL, type="source", lib="C:/Users/Jerzy/Documents/R/win-library/3.2")

# load rutils and attach the data
library(rutils)
# data(hf_data)

# set data directories
data_dir <- "E:/mktdata/sec/"
output_dir <- "E:/output/data/"

# set data directories
data_dir <- "C:/Develop/data/hfreq/src/"
output_dir <- "C:/Develop/data/hfreq/scrub/"


### extractors
# extract the name of the time series from its column name
get_name(colnames(rutils::etf_env$VTI)[1])

# extract close prices
price_s <- get_col(etf_env$VTI)
# extract high prices
price_s <- get_col(etf_env$VTI, field_name="High")
# produces error
price_s <- get_col(etf_env$VTI, field_name="blah")


### do_call_rbind()
x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
# split time series into daily list
list_xts <- split(x_ts, "days")

# do_call_rbind() the list back into a time series and compare with the original
identical(x_ts, do_call_rbind(list_xts))

# do_call() the list back into a time series and compare with the original
identical(x_ts, do_call(rbind, list_xts))

# do_call() calling paste()
do_call(paste, c("a", "b", "c"), sep="/")



### do_call_assign()
# perform do_call_assign() and compare to benchmark
load(file="C:/Develop/data/etf_data.RData")
rm(price_s, envir=new_env)
rm(price_s2, envir=new_env)

# first run benchmark
assign("price_s", do.call(merge,
                          lapply(etf_env$sym_bols, function(sym_bol) {
                            x_ts <- Cl(get(sym_bol, etf_env))
                            colnames(x_ts) <- sym_bol
                            x_ts
                          })), envir=new_env)
# perform do_call_assign() using anon function
do_call_assign(
  func_tion=function(x_ts) {
    x_ts <- Cl(x_ts)
    colnames(x_ts) <- rutils::get_name(colnames(x_ts))
    x_ts},
  sym_bols=etf_env$sym_bols,
  out_put="price_s2",
  env_in=etf_env, env_out=new_env)
# perform do_call_assign() using function get_col()
do_call_assign(
  func_tion=get_col,
  sym_bols=etf_env$sym_bols,
  out_put="price_s2",
  env_in=etf_env, env_out=new_env)
# compare to benchmark
ls(new_env)
identical(new_env$price_s, new_env$price_s2)


### estimating rolling moments using package rutils
### estimating rolling aggregations and moments using package TTR
library(TTR)
library(roll)
library(RcppRoll)

median_rolling <- runMedian(x=Cl(SPY), n=win_dow)
var_rolling <- runSD(x=Cl(SPY), n=win_dow)
chart_xts(var_rolling)

sum_rolling <- rutils::roll_sum(Cl(SPY), win_dow=win_dow)
sum_rolling <- RcppRoll::roll_sum(Cl(SPY), n=win_dow, align="right")
sum_rolling <- roll::roll_mean(Cl(SPY), width=win_dow)
sum_rolling <- TTR::runSum(x=Cl(SPY), n=win_dow)

# benchmark the speed of the functionals
library(microbenchmark)
summary(microbenchmark(
  roll_sum=rutils::roll_sum(Cl(SPY), win_dow=win_dow),
  runSum=TTR::runSum(x=Cl(SPY), n=win_dow),
  times=10))[, c(1, 4, 5)]

summary(microbenchmark(
  roll_sum=rutils::roll_sum(Cl(SPY), win_dow=win_dow),
  roll_sum_rcpp=RcppRoll::roll_sum(Cl(SPY), n=win_dow, align="right"),
  roll_mean=roll::roll_mean(Cl(SPY), width=win_dow),
  runSum=TTR::runSum(x=Cl(SPY), n=win_dow),
  times=10))[, c(1, 4, 5)]

summary(microbenchmark(
  roll_max=rutils::roll_max(Cl(SPY), win_dow=win_dow),
  roll_max_rcpp=RcppRoll::roll_max(Cl(SPY), n=win_dow, align="right"),
  roll_mean=roll::roll_mean(Cl(SPY), width=win_dow),
  runMax=TTR::runMax(x=Cl(SPY), n=win_dow),
  times=10))[, c(1, 4, 5)]
