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

# install rutils from github
install.packages("devtools")
library(devtools)
install_github(repo="algoquant/rutils")

# install rutils using install.packages()
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

# extract the name of the time series from its column name
na_me(env_etf$VTI)

# extract close prices
foo <- clo_se(env_etf$VTI)
# extract high prices
foo <- clo_se(env_etf$VTI, which_col="High")
# produces error
foo <- clo_se(env_etf$VTI, which_col="blah")



x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
# split time series into daily list
list_xts <- split(x_ts, "days")

# do_call_rbind() the list back into a time series and compare with the original
identical(x_ts, do_call_rbind(list_xts))

# do_call() the list back into a time series and compare with the original
identical(x_ts, do_call(rbind, list_xts))

# do_call() calling paste()
do_call(paste, c("a", "b", "c"), sep="/")



# perform do_call_assign() and compare to benchmark
load(file="C:/Develop/data/etf_data.RData")
rm(price_s, envir=new_env)
rm(price_s2, envir=new_env)

# first run benchmark
assign("price_s", do.call(merge,
                          lapply(env_etf$sym_bols, function(sym_bol) {
                            x_ts <- Cl(get(sym_bol, env_etf))
                            colnames(x_ts) <- sym_bol
                            x_ts
                          })), envir=new_env)
# perform do_call_assign() using anon function
do_call_assign(
  func_tion=function(x_ts) {
    x_ts <- Cl(x_ts)
    colnames(x_ts) <- na_me(x_ts)
    x_ts},
  sym_bols=env_etf$sym_bols,
  out_put="price_s2",
  env_in=env_etf, env_out=new_env)
# perform do_call_assign() using function clo_se()
do_call_assign(
  func_tion=clo_se,
  sym_bols=env_etf$sym_bols,
  out_put="price_s2",
  env_in=env_etf, env_out=new_env)
# compare to benchmark
ls(new_env)
identical(new_env$price_s, new_env$price_s2)



###########
# process single day of data

# load list of symbols from file in cwd
# sym_bols <- read.csv(file="etf_list_hf.csv")
# sym_bols <- sym_bols[[1]]
# the file "hf_data.RData" is part of "rutils" package, and contains "sym_bols"
data("hf_data")

# define sym_bol
sym_bol <- "SPY"

# load a single day of TAQ data
sym_bol <- load(
  file.path(data_dir,
            paste0(sym_bol, "/2014.05.02.", sym_bol, ".RData")))

### scrub a single day of TAQ data (don't aggregate)
taq_data <- scrub_TAQ(taq_data=get(sym_bol))

# calculate returns
xts_rets <- calc_rets(xts_data=taq_data)

### scrub and aggregate a single day of TAQ data to OHLC
ohlc_data <- scrub_agg(taq_data=get(sym_bol))
chartSeries(ohlc_data, name=sym_bol, theme=chartTheme("white"))


###########
# process TAQ data using package 'rutils': load TAQ data, aggregate to OHLC, and save to file

# aggregate TAQ data for a single symbol, and save to file
save_scrub_agg(sym_bol, data_dir=data_dir, output_dir=output_dir, period="15 min")

save_TAQ(sym_bol, data_dir=data_dir, output_dir=output_dir)

# calculate returns for a single symbol, and save to file
save_rets(sym_bol, data_dir=data_dir, output_dir=output_dir, period="15 min")

# aggregate data for list of symbols, and save to multiple files
sapply(head(sym_bols), save_scrub_agg, data_dir=data_dir, output_dir=output_dir, period="15 min")

# calculate returns for list of symbols, and save to file
sapply(head(sym_bols), save_rets, data_dir=data_dir, output_dir=output_dir, period="15 min")

# load processed OHLC data for a single symbol
# load(file=paste0(sym_bol, ".RData"))
load(file.path(output_dir, paste0(sym_bol, ".RData")))
# load(file="SPY.RData")
# plot OHLC data
chartSeries(get(sym_bol), name=sym_bol, theme=chartTheme("white"))


###########
### estimating variance, skewness, and kurtosis using package TTR

# function volatility() from package TTR
ran_ge <- "2013-11-11/2013-11-15"
vol_at <- volatility(OHLC=get(sym_bol)[ran_ge],
                     calc="yang.zhang", n=20)
vol_at <- volatility(OHLC=get(sym_bol)[ran_ge],
                     calc="rogers.satchell", n=20)
vol_at <- volatility(OHLC=get(sym_bol)[ran_ge],
                     calc="garman.klass", n=20)


# estimating rolling aggregations and moments using package TTR
vol_at <- runMedian(x=get(sym_bol)[ran_ge], n=100)
vol_at <- runSD(x=get(sym_bol)[ran_ge], n=100)
chart_xts(vol_at)


###########
# estimating rolling moments using package rutils
library(rutils)

# daily open to close volatility
vol_at <- apply.daily(x=get(sym_bol), FUN=moment_ohlc)
colnames(vol_at) <- paste(
  strsplit(colnames(get(sym_bol))[1], split="[.]")[[1]][1],
  "Vol", sep=".")
sk_ew <- apply.daily(x=get(sym_bol), FUN=moment_ohlc, mom_fun="skew_ohlc")
sk_ew <- sk_ew/(vol_at)^(1.5)
colnames(sk_ew) <- paste(
  strsplit(colnames(get(sym_bol))[1], split="[.]")[[1]][1],
  "Skew", sep=".")

chart_xts_panels(cbind(10^5*vol_at, sk_ew))
chart_xts_panels(cbind(10^5*vol_at["2013-10"], sk_ew["2013-10"]), in_dex=(abs(sk_ew["2013-10"])>1))


# daily close to close volatility



# minutely volatility and skew
re_turns <- calc_rets(xts_data=get(sym_bol)[ran_ge])
# rolling vol
vol_at <- roll_moment_ohlc(ohlc=get(sym_bol)[ran_ge])
tail(vol_at, 11)
# rolling skew
sk_ew <- roll_moment_ohlc(ohlc=get(sym_bol)[ran_ge], mom_fun="skew_ohlc")
sk_ew <- sk_ew/(vol_at)^(1.5)
sk_ew[1, ] <- 0
sk_ew <- na.locf(sk_ew)
tail(sk_ew, 11)



###########
# ploting

x11()
in_dex <- index(env_etf$VTI["2015-11"]) > as.Date("2015-11-18")
in_dex <- xts(in_dex, order.by=index(env_etf$VTI["2015-11"]))
# in_dex <- ifelse(in_dex, "lightgreen", "lightgrey")
chart_Series(x=env_etf$VTI["2015-11"], name="VTI in Nov 2015")
# add background shading of areas
# add_TA(env_etf$VTI["2015-11"], on=-1, col=in_dex)
add_TA(in_dex, on=-1, col="lightgreen", border="lightgreen")
add_TA(!in_dex, on=-1, col="lightgrey", border="lightgrey")



ch_ob <- chart_Series(x=env_etf$VTI["2015-11"], name="VTI in Nov 2015", plot=FALSE)
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(102, 108), fixed=TRUE)
ch_ob$set_ylim(y_lim)
add_TA(in_dex, on=-1, col="lightgreen", border=NA)
add_TA(!in_dex, on=-1, col="lightgrey", border=NA)


chart_xts(env_etf$VTI["2015-11"],
          name="VTI in Nov 2015",
          ylim=c(102, 108),
          in_dex=index(env_etf$VTI["2015-11"]) > as.Date("2015-11-18"))


