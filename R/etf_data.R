#' @name etf_data
#' @docType data
#' @keywords datasets
#'
#' @title The etf_data dataset contains a single environment called etfenv,
#'   which includes daily \code{OHLC} time series data for a portfolio of
#'   symbols.  All the prices are already adjusted.
#'
#' @description
#' The etfenv environment includes daily \code{OHLC} time series data for a
#' portfolio of symbols, and reference data:
#' \describe{
#'   \item{symbolv}{a \code{vector} of \code{strings} with the portfolio symbols.}
#'   \item{prices}{a single \code{xts} time series containing daily closing
#'   prices for all the \code{symbolv}.}
#'   \item{returns}{a single \code{xts} time series containing daily returns
#'   for all the \code{symbolv}.}
#'   \item{Individual time series}{"VTI", "VEU", etc., containing daily
#'   \code{OHLC} prices for the \code{symbolv}.}
#' }
#'
#' @format Each \code{xts} time series contains the following columns with
#'   adjusted prices and trading volume:
#' \describe{
#'   \item{Open}{Open prices}
#'   \item{High}{High prices}
#'   \item{Low}{Low prices}
#'   \item{Close}{Close prices}
#'   \item{Volume}{daily trading volume}
#' }
#'
#' @usage data(etf_data)  # not required - data is lazy load
#'
#' @examples
#' # Loading is not not needed - data is lazy load
#' # data(etf_data)
#' # Get first six rows of OHLC prices
#' head(etfenv$VTI)
#' \donttest{chart_Series(x=etfenv$VTI["2009-11"])}
"etfenv"
