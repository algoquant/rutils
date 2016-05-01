#' @name etf_data
#' 
#' @title "etf_data" is a data set for package "rutils".
#' 
#' @description The file etf_data.Rdata contains a single environment called 
#'   env_etf, which contains time series data for a portfolio of symbols, and
#'   also other accompanying data:
#' 
#' "sym_bols" \code{vector} of \code{strings} with the portfolio symbols.
#' 
#' "VTI", "VEU", etc. \code{xts} time series containing daily \code{OHLC} price
#' data for the \code{sym_bols}.
#' 
#' "price_s" single \code{xts} time series containing daily closing prices for
#' all the \code{sym_bols}.
#' 
#' "re_turns" single \code{xts} time series containing daily returns for all the
#' \code{sym_bols}.
#'
#' @docType data
#' @keywords datasets
#' @usage data(etf_data)
#'
#' @format Each \code{xts} time series contains the columns:
#' \describe{
#'   \item{Open}{Open prices}
#'   \item{High}{High prices}
#'   \item{Low}{Low prices}
#'   \item{Close}{Close prices}
#'   \item{Volume}{daily trading volume}
#'   \item{Adjusted}{Adjusted closing prices}
#' }
#' 
#' @examples
#' # data(etf_data)  # not needed - data is lazy load
#' # get first six rows of prices
#' head(env_etf$VTI)
#' \donttest{chart_Series(x=env_etf$VTI["2009-11"])}
"etf_data"



