#' @name etf_data
#' @docType data
#' @keywords datasets
#'
#' @title The etf_data dataset contains a single environment called etf_env,
#'   which includes daily \code{OHLC} time series data for a portfolio of
#'   symbols.  All the prices are already adjusted.
#'
#' @description
#' The etf_env environment includes daily \code{OHLC} time series data for a
#' portfolio of symbols, and reference data:
#' \describe{
#'   \item{sym_bols}{a \code{vector} of \code{strings} with the portfolio symbols.}
#'   \item{price_s}{a single \code{xts} time series containing daily closing
#'   prices for all the \code{sym_bols}.}
#'   \item{re_turns}{a single \code{xts} time series containing daily returns
#'   for all the \code{sym_bols}.}
#'   \item{Individual time series}{"VTI", "VEU", etc., containing daily
#'   \code{OHLC} prices for the \code{sym_bols}.}
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
#' head(etf_env$VTI)
#' \donttest{chart_Series(x=etf_env$VTI["2009-11"])}
"etf_env"
