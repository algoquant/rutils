############################################################
#' Calculate the Sharpe and Sortino ratios of a time series of returns.
#'
#' @export
#'
#' @param \code{retp} A time series of returns, with multiple columns.
#'
#' @param \code{riskf} The annual risk-free interest rate (the default is
#'   \code{0}).
#'
#' @param \code{nperiods} The number of time periods in a year (the default is
#'   \code{252} days).
#'
#' @return A matrix of the Sharpe and Sortino ratios.
#'
#' @details The function \code{calc_sharpe()} calculates the Sharpe and Sortino
#'   ratios of a time series of returns.
#'
#'   The function \code{calc_sharpe()} performs an sapply() loop over the
#'   columns of the \code{retp} argument. It calculates the Sharpe and Sortino
#'   ratios for each column. It subtracts from \code{retp} the annual risk-free
#'   interest rate \code{riskf} divided by \code{nperiods}. It multiplies the
#'   Sharpe and Sortino ratios by the square root of \code{nperiods}, in order
#'   to obtain the annual ratios.
#'
#'   The Sharpe ratio \eqn{S_r} is defined as:
#'   \deqn{
#'     S_r = \sqrt{n} \frac{\bar{r}}{\sigma_r}
#'   }
#'   Where \eqn{r} are the daily excess returns (the returns minus the risk-free
#'   rate), \eqn{\bar{r}} are the average excess returns, and \eqn{\sigma_r} is
#'   their daily standard deviation.
#'
#'   The Sortino ratio \eqn{{So}_r} is defined as:
#'   \deqn{
#'     {So}_r = \sqrt{n} \frac{\bar{r}}{\sigma_d}
#'   }
#'   Where \eqn{r} are the daily excess returns (the returns minus the risk-free
#'   rate), \eqn{\bar{r}} are the average excess returns, and \eqn{\sigma_d} is
#'   their daily \emph{downside deviation}.
#'   The \emph{downside deviation} \eqn{\sigma_d} is equal to the standard
#'   deviation of the downside returns \eqn{r_d}, the returns that are less than
#'   the risk-free rate.
#'
#' @examples
#' # Calculate the Sharpe and Sortino ratios of VTI and IEF returns
#' rutils::calc_sharpe(rutils::etfenv$returns[, c("VTI", "IEF")])

calc_sharpe <- function(retp, riskf=0.0, nperiods=252) {

  rf <- riskf/nperiods
  retx <- (retp - rf)
  return(sqrt(nperiods)*sapply(retx, function(x) {
    x <- na.omit(x)
    mx <- mean(x)
    return(c(Sharpe=mx/sd(x), Sortino=mx/sd(x[x < rf])))
  }))

}  # end calc_sharpe



############################################################
#' Calculate the skewness or kurtosis of a time series of returns.
#'
#' @export
#'
#' @param \code{retp} A time series of returns, with multiple columns.
#'
#' @param \code{expn} The power (exponent) to raise the returns (the default is
#'   \code{3} for skewness).
#'
#' @return A matrix of the skewness or kurtosis values.
#'
#' @details The function \code{calc_skew()} calculates the skewness or kurtosis
#'   of a time series of returns.
#'
#'   The function \code{calc_skew()} performs an sapply() loop over the columns
#'   of the \code{retp} argument. It raise the returns to the power \code{expn}.
#'   If \code{expn = 3} it calculates the skewness.
#'   If \code{expn = 4} it calculates the kurtosis.
#'
#'   The skewness \eqn{\varsigma} is defined as:
#'   \deqn{
#'     \varsigma = \frac{1}{n-1} \sum_{i=1}^n {(\frac{r - \bar{r}}{\sigma})^3}
#'   }
#'   Where \eqn{r} are the daily returns, \eqn{\bar{r}} are the average returns,
#'   and \eqn{\sigma} is their standard deviation.
#'
#'   The kurtosis \eqn{\kappa} is defined as:
#'   \deqn{
#'     \kappa = \frac{1}{n-1} \sum_{i=1}^n {(\frac{r - \bar{r}}{\sigma})^4}
#'   }
#'
#' @examples
#' # Calculate the kurtosis of VTI and IEF returns
#' rutils::calc_skew(rutils::etfenv$returns[, c("VTI", "IEF")], expn=4)

calc_skew <- function(retp, expn=3) {

  return(sapply(retp, function(x) {
    x <- na.omit(x)
    mx <- mean(x)
    return(sum(((x - mx)/sd(x))^expn)/(NROW(x)-1))
  }))

}  # end calc_skew



############################################################
#' Extract symbol names (tickers) from a vector of \emph{character} strings.
#'
#' @export
#' @param \code{strng} A vector of \emph{character} strings containing symbol
#'   names.
#'
#' @param \code{posv} The position of the name in the string, i.e. the integer
#'   index of the field to be extracted (the default is \code{1}, i.e. the name
#'   is at the beginning of the string,)
#'
#' @param \code{sep} The name separator, i.e. the single \emph{character}
#'   that separates the symbol name from the rest of the string (the default is
#'   "\code{[.]}").
#'
#' @return A vector of \emph{character} \emph{strings} containing symbol names.
#'
#' @details The function \code{get_name()} extracts the symbol names (tickers)
#'   from a vector of \emph{character} strings. If the input is a vector of
#'   strings, then \code{get_name()} returns a vector of names.
#'
#'   The input string is assumed to be in the format "\emph{name}.csv", with the
#'   name at the beginning of the string, but \code{get_name()} can also parse
#'   the name from other string formats as well. For example, it extracts the
#'   name "VTI" from the string "VTI.Close", or it extracts the name "XLU" from
#'   the string "XLU2017_09_05.csv" (with \code{sep="_"}).
#'
#' @examples
#' # Extract symbols "XLU" and"XLP" from file names
#' rutils::get_name(c("XLU.csv", "XLP.csv"))
#' # Extract symbols from file names
#' rutils::get_name("XLU2017_09_05.csv", sep="_")
#' rutils::get_name("XLU 2017 09 05.csv", sep=" ")
#' # Extract fields "Open", "High", "Low", "Close" from the column names
#' rutils::get_name(c("VTI.Open", "VTI.High", "VTI.Low", "VTI.Close"), posv=2)

get_name <- function(strng, posv=1, sep="[.]") {
  # strng <- strsplit(strng, split=sep)
  # sapply(strng, function(x) x[posv])
  do.call(rbind, strsplit(strng, split=sep))[, posv]
}  # end get_name



############################################################
#' Calculate a vector of equally spaced end points along the elements of a
#' vector, matrix, or time series.
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or time series.
#' @param \code{interval} The number of elements between neighboring end
#'   points. or a \emph{string} representing a time period (minutes, hours,
#'   days, etc.)
#' @param \code{stub_front} A \emph{Boolean} argument: if \code{TRUE} then add a
#'   stub interval at the beginning, else add a stub interval at the end. (the
#'   default is \code{TRUE})
#'
#' @return An \emph{integer} vector of equally spaced end points (vector of
#'   integers).
#'
#' @details The end points are a vector of integers which divide the elements
#'   (rows) of \code{xtsv} into equally spaced intervals.
#'
#'   If \code{interval} is an \emph{integer} then \code{calc_endpoints()}
#'   calculates the number of whole intervals that fit over the elements (rows)
#'   of \code{xtsv}.
#'   If a whole number of intervals doesn't fit over the elements (rows) of
#'   \code{xtsv}, then \code{calc_endpoints()} adds a stub interval either at
#'   the beginning (the default) or at the end.
#'
#'   If \code{interval} is a \emph{string} representing a time period (minutes,
#'   hours, days, etc.), then \code{calc_endpoints()} simply calls the function
#'   \code{endpoints()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#'   The function \code{calc_endpoints()} is a generalization of function
#'   \code{endpoints()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, since
#'   \code{interval} can accept both \emph{integer} and \emph{string} values.
#'   Similar to \code{xts::endpoints()}, the first integer returned by
#'   \code{calc_endpoints()} is equal to zero.
#'
#' @examples
#' # Calculate end points with initial stub interval
#' rutils::calc_endpoints(1:100, interval=11)
#' # Calculate end points with a stub interval at the end
#' rutils::calc_endpoints(rutils::etfenv$VTI, interval=365, stub_front=FALSE)
#' # Calculate end points at the end of every hour
#' rutils::calc_endpoints(rutils::etfenv$VTI, interval="hours")

calc_endpoints <- function(xtsv, interval, stub_front=TRUE) {
  if (is.character(interval)) {
    xts::endpoints(xtsv, on=interval)
  } else if (is.numeric(interval)) {
    # Calculate number of intervals that fit over xtsv
    nrows <- NROW(xtsv)
    num_agg <- nrows %/% interval
    if (nrows > interval*num_agg) {
      # Need to add stub interval
      if (stub_front)
        # Stub interval at beginning
        endd <- c(0, nrows - num_agg*interval + (0:num_agg)*interval)
      else
        # Stub interval at end
        endd <- c((0:num_agg)*interval, nrows)
    } else
      endd <- (0:num_agg)*interval
    # end if
    endd
  } else {  # interval is neither numeric nor a string
    warning(paste0("Argument \"", deparse(substitute(interval)), "\" must be either numeric or a string."))
    return(NULL)  # Return NULL
  }  # end if

}  # end calc_endpoints



############################################################
#' Replace \code{NA} values with the most recent non-\code{NA} values prior to
#' them.
#'
#' @export
#' @param \code{inputv} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#'
#' @param \code{fromLast} A  \emph{Boolean} argument: should non-\code{NA}
#'   values be carried backward rather than forward? (the default is \code{FALSE})
#'
#' @param \code{narm} A \emph{Boolean} argument: should any remaining (leading
#'   or trailing) \code{NA} values be removed? (the default is \code{FALSE})
#'
#' @param \code{maxgap} The maximum number of neighboring \code{NA} values that
#'   can be replaced (the default is \code{NROW(inputv)}).
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   and data type as the argument \code{inputv}.
#'
#' @details The function \code{nalocf()} replaces \code{NA} values with the
#'   most recent non-\code{NA} values prior to them.
#'
#'   If the \code{fromLast} argument is \code{FALSE} (the default), then the
#'   previous or past non-\code{NA} values are carried forward to replace the
#'   \code{NA} values.
#'   If the \code{fromLast} argument is \code{TRUE}, then the following or
#'   future non-\code{NA} values are carried backward to replace the \code{NA}
#'   values.
#'
#'   The function \code{nalocf()} performs the same operations as the function
#'   \code{zoo::na.locf()} from package
#'   \href{https://cran.r-project.org/web/packages/zoo/index.html}{zoo}.
#'
#'   The function \code{nalocf()} calls the function \code{xts:::na.locf.xts()}
#'   from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#' @examples
#' # Create vector containing NA values
#' inputv <- sample(22)
#' inputv[sample(NROW(inputv), 4)] <- NA
#' # Replace NA values with the most recent non-NA values
#' rutils::nalocf(inputv)
#' # Create matrix containing NA values
#' inputv <- sample(44)
#' inputv[sample(NROW(inputv), 8)] <- NA
#' inputv <- matrix(inputv, nc=2)
#' # Replace NA values with the most recent non-NA values
#' rutils::nalocf(inputv)
#' # Create xts series containing NA values
#' inputv <- xts::xts(inputv, order.by=seq.Date(from=Sys.Date(),
#'   by=1, length.out=NROW(inputv)))
#' # Replace NA values with the most recent non-NA values
#' rutils::nalocf(inputv, fromLast=TRUE)

nalocf <- function(inputv, fromLast=FALSE, narm=FALSE, maxgap=NROW(inputv)) {

  if (!(is.numeric(inputv) || is.logical(inputv) || xts::is.timeBased(inputv) || xts::is.xts(inputv))) {  # input is not numeric
    warning(paste("Argument", deparse(substitute(inputv)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  if (xts::is.xts(inputv)) {  # input is an xts
    return(xts:::na.locf.xts(inputv, fromLast=fromLast, na.rm=narm, maxgap=maxgap))
  } else {
    return(na.locf(inputv, fromLast=fromLast, na.rm=narm, maxgap=maxgap))
  } # end if

  ### old version below
  ## "na_locf" not available for .Call() for package "xts"
  #
  # if (!(is.numeric(input) || is.logical(input) || xts::is.timeBased(input) || xts::is.xts(input))) {  # input is not numeric
  #   warning(paste("Argument", deparse(substitute(input)), "must be numeric, date, Boolean, or xts series."))
  #   return(NULL)  # Return NULL
  # }  # end if
  #
  # if (NCOL(input) > 1) {
  #   for (n in 1:NCOL(input))
  #     input[, n] <- .Call("na_locf", input[, n], from_last, max_gap, Inf, PACKAGE="xts")
  # }
  # else {
  #   input <- .Call("na_locf", input, from_last, max_gap, Inf, PACKAGE="xts")
  # }
  # if (na_rm) {
  #   return(structure(na.omit(input), na.action=NULL))
  # }
  # else
  #   input

  ### oldest version below
  # if (NCOL(inputv) > 1)
  #   return(apply(inputv, MARGIN=2, rutils::nalocf))
  # if (fromLast && is.na(inputv[1]))
  #   inputv[1] <- 0
  # if (!fromLast && is.na(inputv[NROW(inputv)]))
  #   inputv[NROW(inputv)] <- 0
  # indeks <- is.na(inputv)
  # indices <- rutils::diffit(indeks)
  # indices <- which(indices==1)
  # indices <- indices - 1
  # diff_indices <- rutils::diffit(indices)
  # diff_indices[1] <- indices[1]
  # new_indices <- numeric(NROW(inputv))
  # new_indices[indices] <- diff_indices
  # new_indices <- cumsum(new_indices)
  # inputv[indeks] <- inputv[new_indices[indeks]]
  # inputv

}  # end nalocf



############################################################
#' Aggregate an \emph{xts} time series to a lower periodicity.
#'
#' Given an \emph{xts} time series at high periodicity (say seconds),
#' calculate the \emph{OHLC} prices at a lower periodicity (say minutes).
#'
#' @param \code{timeser} An \emph{xts} time series of prices.
#'
#' @param \code{period} Aggregation interval ("seconds", "minutes", "hours",
#'   "days", "weeks", "months", "quarters", and "years").
#'
#' @param \code{k} The number of periods to aggregate over (for example if
#'   \code{period="minutes"} and \code{k=2}, then aggregate over two minute
#'   intervals.)
#'
#' @return A \emph{OHLC} time series of prices in \emph{xts} format, with a
#'   lower periodicity.
#'
#' @details The function \code{to_period()} is a wrapper for the function
#'   \code{xts::to.period()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#' @examples
#' \dontrun{
#' # Aggregate the OHLC prices from minutes to days:
#' ohlc <- rutils::to_period(timeser=HighFreq::SPY["2009"], period="days")
#' }
#'
#' @export
to_period <- function(timeser, period="minutes", k=1) {

  # Calculate the column names
  if (NCOL(timeser) > 2) {
    colv <- colnames(timeser)
  } else if (NCOL(timeser) == 2) {
    # Second column is the volume
    namev <- deparse(substitute(timeser))
    colv <- paste0(namev, ".", c("Open", "High", "Low", "Close", "Volume"))
  } else {
    # Single column in the input time series
    # No volume column in the input time series
    namev <- deparse(substitute(timeser))
    colv <- paste0(namev, ".", c("Open", "High", "Low", "Close"))
  } # end if

  # Aggregate to lower periodicity
  outv <- xts::to_period(x=timeser, period=period, k=k)
  colnames(outv) <- colv
  outv

  # Alternative code below - doesn't preserve original column names
  # namev <- deparse(substitute(timeser))
  # xts::to_period(x=timeser, period=period, k=k, name=namev)

  # Old code below
  # .Call("C_toPeriod", timeser, as.integer(endpoints), TRUE, NCOL(timeser),
  #       FALSE, FALSE, colnames(timeser), PACKAGE="xts")

}  # end to_period



############################################################
#' Extract columns of data from \emph{OHLC} time series using column field
#' names.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series in \emph{xts} format, or a
#'   vector of \emph{character} strings with the names of \emph{OHLC} time
#'   series.
#' @param \code{fieldn} A vector of strings with the field names of the
#'   columns to be be extracted (the default is \code{"Close"}).
#' @param \code{datenv} The environment containing \emph{OHLC} time series
#'   (the default is \code{NULL}).
#'
#' @return The specified columns of the \emph{OHLC} time series bound into a
#'   single \emph{xts} time series, with the same number of rows as the input
#'   time series.
#'
#' @details The function \code{get_col()} extracts columns from \emph{OHLC} time
#'   series and binds them into a single \emph{xts} time series.
#'   \code{get_col()} can extract columns from a single \emph{xts} time series,
#'   or from multiple time series.
#'
#'   The function \code{get_col()} extracts columns by partially matching field
#'   names with column names. The \emph{OHLC} column names are assumed to be in
#'   the format \code{"symbol.fieldn"}, for example \code{"VTI.Close"}.
#'
#'   In the simplest case when \code{ohlc} is a single \emph{xts} time series
#'   and \emph{fieldn} is a single string, the function \code{get_col()}
#'   performs a similar operation to the extractor functions \code{Op()},
#'   \code{Hi()}, \code{Lo()}, \code{Cl()}, and \code{Vo()}, from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   But \code{get_col()} is able to handle symbols like \emph{LOW}, which the
#'   function \code{Lo()} can't handle. The fieldn argument is partially
#'   matched, for example "Vol" is matched to \code{Volume} (but it's case
#'   sensitive).
#'
#'   In the case when \code{ohlc} is a vector of strings with the names of
#'   \emph{OHLC} time series, the function \code{get_col()} reads the
#'   \emph{OHLC} time series from the environment \code{datenv}, extracts the
#'   specified columns, and binds them into a single \emph{xts} time series.
#'
#' @examples
#' # get close prices for VTI
#' rutils::get_col(rutils::etfenv$VTI)
#' # get volumes for VTI
#' rutils::get_col(rutils::etfenv$VTI, fieldn="Vol")
#' # get close prices and volumes for VTI
#' rutils::get_col(rutils::etfenv$VTI, fieldn=c("Cl", "Vol"))
#' # get close prices and volumes for VTI and IEF
#' rutils::get_col(ohlc=c("VTI", "IEF"), fieldn=c("Cl", "Vol"),
#'   datenv=rutils::etfenv)

get_col <- function(ohlc, fieldn="Close", datenv=NULL) {
  if (is.xts(ohlc)) {
    # Extract columns, and return them
    colv <- strsplit(colnames(ohlc), split="[.]")
    colv <- sapply(colv, function(coln) coln[2])
    return(ohlc[, pmatch(fieldn, colv)])
  } else if (is.character(ohlc)) {
    # Loop over the symbols in ohlc, extract columns, and cbind them
    datav <- lapply(ohlc, function(symbol) {
      ohlc <- get(symbol, envir=datenv)
      colv <- strsplit(colnames(ohlc), split="[.]")
      colv <- sapply(colv, function(coln) coln[2])
      ohlc[, pmatch(fieldn, colv)]
    })  # end lapply
    return(rutils::do_call(cbind, datav))
  }  # end if
  ## below is a different version using as.list()
  # datav <- lapply(as.list(datenv)[ohlc], function(ohlc) {
  #   colv <- strsplit(colnames(ohlc), split="[.]")
  #   colv <- sapply(colv, function(coln) coln[2])
  #   ohlc[, pmatch(fieldn, colv)]
  # })  # end lapply
  #
  ## below is the old version using grep()
  #   indeks <- grep(paste0(".", fieldn), colnames(ohlc), ignore.case=TRUE)
  #   if (NROW(indeks)>0)
  # #    outv <- xts::.subset_xts(ohlc, 1:NROW(ohlc), indeks:indeks)
  #     ohlc[, indeks]
  #   else
  #     stop(paste0("No column name containing \"", fieldn, "\""))
}  # end get_col



############################################################
#' Adjust the first four columns of \emph{OHLC} data using the "adjusted" price
#' column.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series of prices in \emph{xts} format.
#'
#' @return An \emph{OHLC} time series with the same dimensions as the input
#'   series.
#'
#' @details Adjusts the first four \emph{OHLC} price columns by multiplying them
#'   by the ratio of the "adjusted" (sixth) price column, divided by the
#'   \emph{Close} (fourth) price column.
#'
#' @examples
#' # Adjust VTI prices
#' VTI <- rutils::adjust_ohlc(rutils::etfenv$VTI)

adjust_ohlc <- function(ohlc) {
  # Adjust OHLC prices
  ohlc[, 1:4] <- as.vector(ohlc[, 6] / ohlc[, 4]) * coredata(ohlc[, 1:4])
  ohlc
}  # end adjust_ohlc



############################################################
#' Subset an \emph{xts} time series (extract an \emph{xts} sub-series
#' corresponding to the input dates).
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series.
#'
#' @param \code{startd} The start date of the extracted time series data.
#'
#' @param \code{endd} The end date of the extracted time series data, or the
#'   number of data rows to be extracted.
#'
#' @param \code{get_rows} A \emph{Boolean} argument: if \code{TRUE} then extract the
#'   given number of rows of data, else extract the given number of calendar
#'   days (the default is \code{TRUE}).
#'
#' @return An \emph{xts} time series with the same number of columns as the
#'   input time series.
#'
#' @details The function \code{sub_set()} extracts an \emph{xts} sub-series
#'   corresponding to the input dates.  If \code{endd} is a date object or
#'   a character string representing a date, then \code{sub_set()} performs
#'   standard bracket subsetting using the package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#'   The rows of data don't necessarily correspond to consecutive calendar days
#'   because of weekends and holidays.  For example, 10 consecutive rows of data
#'   may correspond to 12 calendar days. So if \code{endd} is a number, then
#'   we must choose to extract either the given number of rows of data
#'   (\code{get_rows=TRUE}) or the given number of calendar days
#'   (\code{get_rows=FALSE}).
#'
#'   If \code{endd} is a positive number then \code{sub_set()} returns the
#'   specified number of data rows from the future, and if it's negative then it
#'   returns the data rows from the past.
#'
#'   If \code{endd} is a number, and either \code{startd} or
#'   \code{endd} are outside the date range of \code{xtsv}, then
#'   \code{sub_set()} extracts the maximum available range of \code{xtsv}.
#'
#' @examples
#' # Subset an xts time series using two dates
#' rutils::sub_set(rutils::etfenv$VTI, startd="2015-01-01", endd="2015-01-10")
#' # Extract 6 consecutive rows of data from the past, using a date and a negative number
#' rutils::sub_set(rutils::etfenv$VTI, startd="2015-01-01", endd=-6)
#' # Extract 6 calendar days of data
#' rutils::sub_set(rutils::etfenv$VTI, startd="2015-01-01", endd=6, get_rows=FALSE)
#' # Extract up to 100 consecutive rows of data
#' rutils::sub_set(rutils::etfenv$VTI, startd="2016-08-01", endd=100)

sub_set <- function(xtsv, startd, endd, get_rows=TRUE) {
  if (inherits(endd, c("Date", "POSIXt", "character"))) {
    xtsv[paste(startd, endd, sep = "/")]
  } else if (is.numeric(endd)) {
    # Coerce startd from string into Date or POSIXt, depending on time index class of xtsv
    if (inherits(startd, "character")) {
      indeks <- index(xtsv[1, ])
      if (inherits(indeks, "Date")) {
        startd <- as.Date(startd)
      } else if (inherits(indeks, "POSIXt")) {
        startd <- as.POSIXct(startd)
      }
    }  # end if
    # Extract either a number of calendar days or a number of rows of data
    if (get_rows) {
      # Find the row number closest to startd
      startpoint <- findInterval(startd, index(xtsv))
      if (startd > index(xtsv[startpoint])) {
        startpoint <- startpoint + 1
      }  # end if
      endpoint <- (startpoint + endd - sign(endd))
      endpoint <- max(1, min(endpoint, NROW(xtsv)))
      xtsv[startpoint:endpoint]
    } else {
      endd <- startd + endd
      endd <- max(start(xtsv), min(endd, end(xtsv)))
      # Add numeric endd to startd to get the endd as a date
      if (endd > startd) {
        xtsv[paste(startd, endd, sep = "/")]
      } else {
        xtsv[paste(endd, startd, sep = "/")]
      }  # end if
    }  # end if
  } else {  # endd is neither a date nor a string
    warning(paste0("Argument \"", deparse(substitute(endd)), "\" must be either a date object or a string representing a date."))
    return(NULL)  # Return NULL
  }  # end if

}  # end sub_set



############################################################
#' Apply a lag to a \emph{numeric} or \emph{Boolean} vector, matrix, or
#' \emph{xts} time series.
#'
#' @export
#' @param \code{inputv} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#'
#' @param \code{lagg} An \emph{integer} equal to the number of time periods
#'   (rows) of lag (the default is \code{1}).
#'
#' @param \code{pad_zeros} A \emph{Boolean} argument: Should the output be
#'   padded with zeros? (The default is \code{pad_zeros = TRUE}.)
#'
#' @return A vector, matrix, or \emph{xts} time series. with the same dimensions
#'   as the input object.
#'
#' @details The function \code{lagit()} applies a lag to the input object by
#'   shifting its rows by the number of time periods equal to the integer
#'   argument \code{lagg}.
#'
#'   For positive \code{lagg} values, the current row is replaced with the row
#'   that is \code{lagg} rows above it.  And vice versa for a negative
#'   \code{lagg} values.  This also applies to vectors, since they can be viewed
#'   as single-column matrices.
#'   If \code{lagg = 0}, then \code{lagit()} returns the input object unchanged.
#'
#'   To avoid leading or trailing \code{NA} values, the output object is padded
#'   with zeroes, or with elements from either the first or the last row.
#'
#'   For the lag of asset returns, they should be padded with zeros, to avoid
#'   look-ahead bias.
#'   For the lag of prices, they should be padded with the first or last prices,
#'   not with zeros.
#'
#'   When applied to \emph{xts} time series, the function \code{lagit()} calls
#'   the function \code{lag.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads the output with the first and last rows, instead of with \code{NA}s.
#'
#' @examples
#' # Lag vector by 2 periods
#' rutils::lagit(1:10, lag=2)
#' # Lag matrix by negative 2 periods
#' rutils::lagit(matrix(1:10, ncol=2), lag=-2)
#' # Lag an xts time series
#' lag_ged <- rutils::lagit(rutils::etfenv$VTI, lag=10)

lagit <- function(inputv, lagg=1, pad_zeros=TRUE, ...) {

  if (lagg == 0) {
    # Return input if lagg is zero
    return(inputv)
  }  # end if

  if (!(is.numeric(inputv) || is.logical(inputv) || xts::is.timeBased(inputv) || xts::is.xts(inputv))) {
    # Return NULL if input is not numeric
    warning(paste("Argument", deparse(substitute(inputv)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  nrows <- NROW(inputv)
  ncol <- NCOL(inputv)

  padd <- 1
  if (pad_zeros) padd <- 0

  if (xts::is.xts(inputv)) {  # input is an xts
    firstv <- inputv[1, ]
    lastv <- inputv[nrows, ]
    outv <- xts::lag.xts(inputv, k=lagg, ...)
    if (lagg>0) {
      outv[1:lagg, ] <- matrix(rep(padd*firstv, lagg), byrow=TRUE, nr=lagg)
    } else {
      outv[(nrows+lagg+1):nrows, ] <- matrix(rep(padd*lastv, -lagg), byrow=TRUE, nr=-lagg)
    }  # end if
    return(outv)
  } else if (is.null(dim(inputv))) {  # input is a vector
    if (lagg>0) {
      outv <- c(rep(padd*inputv[1], lagg), inputv)
      return(outv[1:nrows])
    } else {
      outv <- c(inputv, rep(padd*inputv[nrows], -lagg))
      return(outv[-(1:(-lagg))])
    }  # end if
  } else if (is.matrix(inputv)) {  # input is a matrix
    if (lagg>0) {
      outv <- rbind(matrix(rep(padd*inputv[1, ], lagg), byrow=TRUE, nr=lagg), inputv)
      return(outv[1:nrows, ])
    } else {
      outv <- rbind(inputv, matrix(rep(padd*inputv[nrows, ], -lagg), byrow=TRUE, nr=-lagg))
      return(outv[-(1:(-lagg)), ])
    }  # end if
  } else {  # input is neither a vector nor matrix
    warning(paste0("Argument \"", deparse(substitute(inputv)), "\" must be either a vector, matrix, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

}  # end lagit



############################################################
#' Calculate the row differences of a \emph{numeric} or \emph{Boolean} vector,
#' matrix, or \emph{xts} time series.
#'
#' @export
#' @param \code{inputv} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#'
#' @param \code{lagg} An \emph{integer} equal to the number of time periods of
#'   lag (the default is \code{1}).
#'
#' @return A vector, matrix, or \emph{xts} time series. with the same dimensions
#'   as the input object.
#'
#' @details The function \code{diffit()} calculates the row differences between
#'   rows that are \code{lagg} rows apart. Positive \code{lagg} means that the
#'   difference is calculated as the current row minus the row that is
#'   \code{lagg} rows above. (vice versa for a negative \code{lagg}).  This also
#'   applies to vectors, since they can be viewed as single-column matrices. The
#'   leading or trailing stub periods are padded with \emph{zeros}.
#'
#'   When applied to \emph{xts} time series, the function \code{diffit()} calls
#'   the function \code{diff.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads the output with zeros instead of with \code{NA}s.
#'
#' @examples
#' # Diff vector by 2 periods
#' rutils::diffit(1:10, lagg=2)
#' # Diff matrix by negative 2 periods
#' rutils::diffit(matrix(1:10, ncol=2), lagg=-2)
#' # Diff an xts time series
#' rutils::diffit(rutils::etfenv$VTI, lagg=10)

diffit <- function(inputv, lagg=1, ...) {

  if (!(is.numeric(inputv) || is.logical(inputv) || xts::is.timeBased(inputv) || xts::is.xts(inputv))) {  # input is not numeric
    warning(paste("Argument", deparse(substitute(inputv)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  nrows <- NROW(inputv)
  ncol <- NCOL(inputv)

  if (xts::is.xts(inputv)) {  # input is an xts
    if (lagg>0) {
      outv <- xts::diff.xts(inputv, lag=lagg, ...)
      outv[1:lagg, ] <- 0
    } else {
      outv <- xts::diff.xts(inputv, lag=-lagg, ...)
      outv[(nrows+lagg+1):nrows, ] <- 0
    }  # end if
    return(outv)
  } else if (is.null(dim(inputv))) {  # input is a vector
    if (lagg>0) {
      lagv <- c(inputv[1:lagg], inputv[1:(nrows-lagg)])
    } else {
      lagv <- c(inputv[-(1:(-lagg))], inputv[(nrows+lagg+1):nrows])
    }  # end if
  } else if (is.matrix(inputv)) {  # input is a matrix
    if (lagg>0) {
      lagv <- rbind(inputv[1:lagg, , drop=FALSE], inputv[1:(nrows-lagg), , drop=FALSE])
    } else {
      lagv <- rbind(inputv[-(1:(-lagg)), , drop=FALSE], inputv[(nrows+lagg+1):nrows, , drop=FALSE])
    }  # end if
  } else {  # input is neither a vector nor matrix
    warning(paste0("Argument \"", deparse(substitute(inputv)), "\" must be either a vector, matrix, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  return(inputv - lagv)

}  # end diffit



############################################################
#' Apply a time lag to an \emph{xts} time series.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series.
#'
#' @param \code{lagg} An \emph{integer} equal to the number of time periods of
#'   lag (the default is \code{1}).
#'
#' @param \code{pad_zeros} A \emph{Boolean} argument: Should the output be
#'   padded with zeros? (The default is \code{pad_zeros = TRUE}.)
#'
#' @param \code{...} Additional arguments to function \code{xts::lagxts()}.
#'
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input \code{xtsv} time series.
#'
#' @details Applies a time lag to an \emph{xts} time series and pads with the
#'   first and last elements instead of \code{NA}s.
#'
#'   A positive lag argument \code{lagg} means elements from \code{lagg} periods
#'   in the past are moved to the present. A negative lag argument \code{lagg}
#'   moves elements from the future to the present.
#'   If \code{lagg = 0}, then \code{lagxts()} returns the input time series
#'   unchanged.
#'
#'   To avoid leading or trailing \code{NA} values, the output xts is padded
#'   with zeroes, or with elements from either the first or the last row.
#'
#'   For the lag of asset returns, they should be padded with zeros, to avoid
#'   look-ahead bias.
#'   For the lag of prices, they should be padded with the first or last prices,
#'   not with zeros.
#'
#'   The function \code{lagxts()} is just a wrapper for function
#'   \code{lag.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with the first and last elements instead of \code{NA}s.
#'
#'   The function \code{lagit()} has incorporated the functionality of
#'   \code{lagxts()}, so that \code{lagxts()} will be retired in future
#'   package versions.
#'
#' @examples
#' # Lag by 10 periods
#' rutils::lagxts(rutils::etfenv$VTI, lag=10)

lagxts <- function(xtsv, lagg=1, pad_zeros=TRUE, ...) {

  if (lagg == 0) {
    # Return xtsv if lagg is zero
    return(xtsv)
  }  # end if

  if (!xts::is.xts(inputv)) {
    # Return NULL if xtsv is not an xts series
    warning(paste("Argument", deparse(substitute(inputv)), "must be an xts series."))
    return(NULL)  # Return NULL
  }  # end if

  nrows <- NROW(xtsv)

  xtsv <- xts::lag.xts(xtsv, k=lagg, ...)

  if (pad_zeros) {
    firstv <- 0
    lastv <- 0
  } else {
    firstv <- xtsv[1, ]
    lastv <- xtsv[nrows, ]
  }  # end if

  if (lagg > 0)
    xtsv[1:lagg, ] <- matrix(rep(firstv, lagg), byrow=TRUE, nr=lagg)
  else
    xtsv[(nrows+lagg+1):nrows, ] <- matrix(rep(lastv, -lagg), byrow=TRUE, nr=-lagg)

  xtsv

}  # end lagxts



############################################################
#' Calculate the time differences of an \emph{xts} time series.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series.
#' @param \code{lagg} An \emph{integer} equal to the number of time periods of
#'   lag (the default is \code{1}).
#' @param \code{...} Additional arguments to function \code{xts::diff.xts()}.
#'
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input series.
#'
#' @details The function \code{diffxts()} calculates the time differences of an
#'   \emph{xts} time series and pads with \emph{zeros} instead of \code{NA}s.
#'   Positive \code{lagg} means differences are calculated with values from
#'   \code{lagg} periods in the past (vice versa for a negative \code{lagg}).
#'   The function \code{diff()} is just a wrapper for \code{diff.xts()} from
#'   package \href{https://cran.r-project.org/web/packages/xts/index.html}{xts},
#'   but it pads with \emph{zeros} instead of \code{NA}s.
#'
#'   The function \code{diffit()} has incorporated the functionality of
#'   \code{diffxts()}, so that \code{diffxts()} will be retired in future
#'   package versions.
#'
#' @examples
#' # Calculate time differences over lag by 10 periods
#' rutils::diffxts(rutils::etfenv$VTI, lag=10)

diffxts <- function(xtsv, lagg=1, ...) {
  xtsv <- xts::diff.xts(xtsv, lag=lagg, ...)
  xtsv[!complete.cases(xtsv), ] <- 0
  xtsv
}  # end diffxts



############################################################
#' Calculate the reduced form of an \emph{OHLC} time series, or calculate the
#' standard form from the reduced form of an \emph{OHLC} time series.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series of prices in \emph{xts} format.
#' @param \code{reducit} A \emph{Boolean} argument: should the reduced form be
#'   calculated or the standard form? (the default is \code{TRUE})
#' @param \code{...} Additional arguments to function \code{xts::diff.xts()}.
#'
#' @return An \emph{OHLC} time series with five columns for the \emph{Open},
#'   \emph{High}, \emph{Low}, \emph{Close} prices, and the \emph{Volume}, and
#'   with the same time index as the input series.
#'
#' @details The reduced form of an \emph{OHLC} time series is obtained by
#'   calculating the time differences of its \emph{Close} prices, and by
#'   calculating the differences between its \emph{Open}, \emph{High}, and
#'   \emph{Low} prices minus the \emph{Close} prices. The standard form is the
#'   original \emph{OHLC} time series, and can be calculated from its reduced
#'   form by reversing those operations.
#'
#' @examples
#' # Calculate reduced form of an OHLC time series
#' diffVTI <- rutils::diffohlc(rutils::etfenv$VTI)
#' # Calculate standard form of an OHLC time series
#' VTI <- rutils::diffohlc(diffVTI, reducit=FALSE)
#' identical(VTI, rutils::etfenv$VTI[, 1:5])

diffohlc <- function(ohlc, reducit=TRUE, ...) {
  if (reducit) {
    # Calculate differencces of Close prices
    closep <- xts::diff.xts(ohlc[, 4], lag=1, ...)
    # Find index of overnight price jumps
    indeks <- c(60, diff(.index(ohlc))) > 60
    # Set overnight price jumps to zero
    closep[indeks] <- 0
    # Remember first Close price
    closep[1] <- ohlc[1, 4]
    # Calculate differences of OHLC prices with respect to Close prices
    openp <- ohlc[, 1] - ohlc[, 4]
    highp <- ohlc[, 2] - ohlc[, 4]
    lowp <- ohlc[, 3] - ohlc[, 4]
    cbind(openp, highp, lowp, closep, ohlc[, 5])
  }
  else {
    closep <- cumsum(ohlc[, 4])
    openp <- ohlc[, 1] + closep
    highp <- ohlc[, 2] + closep
    lowp <- ohlc[, 3] + closep
    cbind(openp, highp, lowp, closep, ohlc[, 5])
  }  # end if
}  # end diffohlc



############################################################
#' Calculate the rolling sum of a \emph{numeric} vector, matrix, or \emph{xts}
#' time series over a sliding window (lookback period).
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or \emph{xts} time series containing one
#'   or more columns of data.
#' @param \code{lookb} The size of the lookback window, equal to the number
#'   of data points for calculating the rolling sum.
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   as the input series.
#'
#' @details For example, if lookb=3, then the rolling sum at any point is
#'   equal to the sum of \code{xtsv} values for that point plus two preceding
#'   points. The initial values of roll_sum() are equal to cumsum() values, so
#'   that roll_sum() doesn't return any \code{NA} values.
#'
#'   The function \code{roll_sum()} performs the same operation as function
#'   \code{runSum()} from package
#'   \href{https://cran.r-project.org/web/packages/TTR/index.html}{TTR}, but
#'   using vectorized functions, so it's a little faster.
#'
#' @examples
#' # Rolling sum of vector
#' vectorv <- rnorm(1000)
#' rutils::roll_sum(vectorv, lookb=3)
#' # Rolling sum of matrix
#' matrixv <- matrix(rnorm(1000), nc=5)
#' rutils::roll_sum(matrixv, lookb=3)
#' # Rolling sum of xts time series
#' xtsv <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' rutils::roll_sum(xtsv, lookb=3)

roll_sum <- function(xtsv, lookb) {
  if (xts::is.xts(xtsv)) {
    cumsumv <- cumsum(xtsv)
    rolls <- (cumsumv - rutils::lagit(cumsumv, lag=lookb))
    rolls[1:lookb, ] <- cumsumv[1:lookb, ]
  }
  else {
    if (is.null(dim(xtsv))) {
      cumsumv <- cumsum(xtsv)
      rolls <- (cumsumv - rutils::lagit(cumsumv, lag=lookb))
      rolls[1:lookb] <- cumsumv[1:lookb]
    }
    else {
      cumsumv <- apply(xtsv, MARGIN=2, function(colnum) cumsum(colnum))
      rolls <- cumsumv - rutils::lagit(cumsumv, lag=lookb)
      rolls[1:lookb, ] <- cumsumv[1:lookb, ]
    }
  }
  rolls
}  # end roll_sum



############################################################
#' Calculate the rolling maximum of an \emph{xts} time series over a sliding
#' window (lookback period).
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series containing one or more columns
#'   of data.
#' @param \code{lookb} The size of the lookback window, equal to the number
#'   of data points for calculating the rolling sum.
#'
#' @return An \emph{xts} time series with the same dimensions as the input
#'   series.
#'
#' @details For example, if lookb=3, then the rolling sum at any point is
#'   equal to the sum of \code{xtsv} values for that point plus two preceding
#'   points.
#'
#'   The initial values of roll_max() are equal to cumsum() values, so that
#'   roll_max() doesn't return any \code{NA} values.
#'
#'   The function \code{roll_max()} performs the same operation as function
#'   \code{runMax()} from package
#'   \href{https://cran.r-project.org/web/packages/TTR/index.html}{TTR}, but
#'   using vectorized functions, so it's a little faster.
#'
#' @examples
#' # Create xts time series
#' xtsv <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' rutils::roll_max(xtsv, lookb=3)

roll_max <- function(xtsv, lookb) {

  rollm <- RcppRoll::roll_max(c(rep(0,lookb-1), coredata(xtsv)), n=lookb, align="right")
  rollm <- xts(x=rollm, order.by=index(xtsv))
  colnames(rollm) <- colnames(xtsv)
  return(rollm)

}  # end roll_max



############################################################
#' Recursively \sQuote{\code{rbind}} a list of objects, such as \emph{xts} time
#' series.
#'
#' @export
#' @param \code{listv} A list of objects, such as \emph{vectors}, \emph{matrices},
#'   \emph{data frames}, or \emph{time series}.
#'
#' @return A single \emph{vector}, \emph{matrix}, \emph{data frame}, or
#'   \emph{time series}.
#'
#' @details Performs lapply loop, each time binding neighboring elements and
#'   dividing the length of \code{listv} by half. The result of performing
#'   \code{do_call_rbind(list_xts)} on a list of \emph{xts} time series is
#'   identical to performing \code{do.call(rbind, list_xts)}. But
#'   \code{do.call(rbind, list_xts)} is very slow, and often causes an
#'   \sQuote{out of memory} error.
#'
#'   The function \code{do_call_rbind()} performs the same operation as
#'   \code{do.call(rbind, listv)}, but using recursion, which is much faster and
#'   uses less memory. This is the same function as
#'   \sQuote{\code{\link[qmao]{do.call.rbind}}} from package
#'   \sQuote{\href{https://r-forge.r-project.org/R/?group_id=1113}{qmao}}.
#'
#' @examples
#' # Create xts time series
#' xtsv <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' # Split time series into daily list
#' list_xts <- split(xtsv, "days")
#' # rbind the list back into a time series and compare with the original
#' identical(xtsv, rutils::do_call_rbind(list_xts))

do_call_rbind <- function(listv) {

  while (NROW(listv) > 1) {
# index of odd list elements
    oddel <- seq(from=1, to=NROW(listv), by=2)
# bind odd and even elements, and divide listv by half
    listv <- lapply(oddel, function(indeks) {
      if (indeks==NROW(listv)) return(listv[[indeks]])
      rbind(listv[[indeks]], listv[[indeks+1]])
    })  # end lapply
  }  # end while

  # listv has only one element - return it
  return(listv[[1]])

}  # end do_call_rbind



############################################################
#' Recursively apply a function to a list of objects, such as \emph{xts} time
#' series.
#'
#' Performs a similar operation as \code{do.call()}, but using recursion, which
#' is much faster and uses less memory. The function \code{do_call()} is a
#' generalization of function \code{do_call_rbind()}.
#'
#' @export
#' @param \code{func} The name of function that returns a single object
#'   from a list of objects.
#' @param \code{listv} A list of objects, such as \emph{vectors},
#'   \emph{matrices}, \emph{data frames}, or \emph{time series}.
#' @param \code{...} Additional arguments to function \code{func()}.
#'
#' @return A single \emph{vector}, \emph{matrix}, \emph{data frame}, or
#'   \emph{time series}.
#'
#' @details The function \code{do_call()} performs an lapply loop, each time
#'   binding neighboring elements and dividing the length of \code{listv} by
#'   half. The result of performing \code{do_call(rbind, list_xts)} on a list of
#'   \emph{xts} time series is identical to performing \code{do.call(rbind,
#'   list_xts)}. But \code{do.call(rbind, list_xts)} is very slow, and often
#'   causes an \sQuote{out of memory} error.
#'
#' @examples
#' # Create xts time series
#' xtsv <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' # Split time series into daily list
#' list_xts <- split(xtsv, "days")
#' # rbind the list back into a time series and compare with the original
#' identical(xtsv, rutils::do_call(rbind, list_xts))

do_call <- function(func, listv, ...) {

# Produce function name from argument
  func <- match.fun(func)
  while (NROW(listv) > 1) {
# index of odd list elements
    oddel <- seq(from=1, to=NROW(listv), by=2)
# bind neighboring elements and divide listv by half
    listv <- lapply(oddel, function(indeks) {
      if (indeks==NROW(listv)) {
        return(listv[[indeks]])
      }
      return(func(listv[[indeks]], listv[[indeks+1]], ...))
    })  # end lapply
  }  # end while

  # listv has only one element - return it
  return(listv[[1]])

}  # end do_call



############################################################
#' Apply a function to a list of objects, merge the outputs into a single
#' object, and assign the object to the output environment.
#'
#' @export
#' @param \code{func} The name of a function that returns a single object
#'   (\emph{vector}, \emph{xts} time series, etc.)
#' @param \code{symbolv} A vector of \emph{character} strings with the names of
#'   input objects.
#' @param \code{outv} The string with name of output object.
#' @param \code{inenv} The environment containing the input \code{symbolv}.
#' @param \code{outenv} The environment for creating the \code{outv}.
#' @param \code{...} Additional arguments to function \code{func()}.
#'
#' @return A single object (\emph{matrix}, \emph{xts} time series, etc.)
#'
#' @details The function \code{do_call_assign()} performs an lapply loop over
#'   \code{symbolv}, applies the function \code{func()}, merges the
#'   outputs into a single object, and creates the object in the environment
#'   \code{outenv}.  The output object is created as a side effect, while its
#'   name is returned invisibly.
#'
#' @examples
#' newenv <- new.env()
#' rutils::do_call_assign(
#'    func=get_col,
#'    symbolv=rutils::etfenv$symbolv,
#'    outv="prices",
#'    inenv=etfenv, outenv=newenv)

do_call_assign <- function(func, symbolv=NULL, outv,
                           inenv=.GlobalEnv, outenv=.GlobalEnv, ...) {

# Produce function name from argument
  func <- match.fun(func)
  if (is.null(symbolv)) symbolv <- ls(inenv)
  assign(outv, do.call(merge, lapply(mget(inenv$symbolv, envir=inenv), func, ...)),
  envir=outenv)
  invisible(outv)

}  # end do_call_assign



############################################################
## Functions for plotting
############################################################


############################################################
#' Calculate the autocorrelation function (ACF) of a time series of returns,
#' and plot it.
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or time series of returns.
#'
#' @param \code{lagg} The maximum lag at which to calculate the ACF (the default is
#'   \code{10}).
#'
#' @param \code{plotobj} A \emph{Boolean} argument: should a plot be made?
#'   (the default is \code{TRUE})
#'
#' @param \code{xlab} A string with the x-axis label.
#'
#' @param \code{ylab} A string with the y-axis label.
#'
#' @param \code{main} A string with the plot title.
#'
#' @param \code{...} Additional arguments to the function \code{stats::acf()}.
#'
#' @return Returns the ACF data invisibly and creates a plot.
#'
#' @details The function \code{plot_acf()} calculates the autocorrelation
#'   function (ACF) of a time series of returns, and plots it.  The function
#'   \code{plot_acf()} is just a wrapper for the function \code{stats::acf()}.
#'   The function \code{stats::acf()} calculates the autocorrelation function,
#'   including the lag zero autocorrelation, which is by definition equal to
#'   \code{1}.
#'
#'   The function \code{plot_acf()} calls the function \code{stats::acf()},
#'   removes the spurious lag zero autocorrelation, creates a plot, and returns
#'   the ACF data invisibly.
#'
#' @examples
#' # Plot the ACF of random returns
#' rutils::plot_acf(rnorm(1e4), lag=10, main="ACF of Random Returns")
#' # Plot the ACF of VTI returns
#' rutils::plot_acf(na.omit(rutils::etfenv$returns$VTI), lag=10, main="ACF of VTI Returns")

plot_acf <- function(xtsv, lagg=10,
                     plotobj=TRUE,
                     xlab="Lag", ylab="",
                     main="", ...) {

  # Calculate the ACF without a plot
  acf_data <- acf(x=xtsv, lag.max=lagg, plot=FALSE, ...)
  # Remove first element of ACF data
  acf_data$acf <-  array(data=acf_data$acf[-1],
                         dim=c((dim(acf_data$acf)[1]-1), 1, 1))
  acf_data$lag <-  array(data=acf_data$lag[-1],
                         dim=c((dim(acf_data$lag)[1]-1), 1, 1))
  # Plot ACF
  if (plotobj) {
    ci <- qnorm((1+0.95)/2)*sqrt(1/NROW(xtsv))
    ylim <- c(min(-ci, range(acf_data$acf[-1])),
              max(ci, range(acf_data$acf[-1])))
    plot(acf_data, xlab=xlab, ylab=ylab,
         ylim=ylim, main="", ci=0)
    title(main=main, line=0.5)
    abline(h=c(-ci, ci), col="blue", lty=2)
  }

  # Return the ACF data invisibly
  invisible(acf_data)

}  # end plot_acf




############################################################
#' Plot either a line plot or a candlestick plot of an \emph{xts} time series,
#' with custom line colors, y-axis range, and with vertical background shading.
#'
#' A wrapper for function \code{chart_Series()} from package
#' \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series or an \emph{OHLC} time series.
#' @param \code{colors} A vector of \emph{strings} with the custom line colors.
#' @param \code{ylim} A \emph{numeric} vector with two elements containing the
#'   y-axis range.
#' @param \code{indic} A \emph{Boolean} vector or \emph{xts} time series for
#'   specifying the shading areas, with TRUE indicating "lightgreen" shading,
#'   and FALSE indicating "antiquewhite" shading.
#' @param \code{x11} A \emph{Boolean} argument: if \code{TRUE} then open x11 window for
#'   plotting, else plot in standard window (the default is \code{TRUE}).
#' @param \code{...} Additional arguments to function \code{chart_Series()}.
#'
#' @return A \code{chart_Series()} object returned invisibly.
#'
#' @details The function \code{chart_xts()} plots a line plot of a \emph{xts}
#'   time series, or a candlestick plot if \emph{xtsv} is a \emph{OHLC} time
#'   series. The function \code{chart_xts()} plots with custom line colors and
#'   vertical background shading, using the function \code{chart_Series()} from
#'   package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   By default \code{chart_xts()} opens and plots in an x11 window.
#'
#'   The function \code{chart_xts()} extracts the \code{chart_Series()} chart
#'   object and modifies its \emph{ylim} parameter using accessor and setter
#'   functions. It also adds background shading specified by the \code{indic}
#'   argument, using function \code{add_TA()}. The \code{indic} argument should
#'   have the same length as the \code{xtsv} time series. Finally the function
#'   \code{chart_xts()} plots the chart object and returns it invisibly.
#'
#' @examples
#' # Plot candlestick chart with shading
#' rutils::chart_xts(rutils::etfenv$VTI["2015-11"],
#'   name="VTI in Nov 2015", ylim=c(102, 108),
#'   indic=zoo::index(rutils::etfenv$VTI["2015-11"]) > as.Date("2015-11-18"))
#' # Plot two time series with custom line colors
#' rutils::chart_xts(na.omit(cbind(rutils::etfenv$XLU[, 4],
#'   rutils::etfenv$XLP[, 4])), colors=c("blue", "green"))

chart_xts <- function(xtsv, colors=NULL, ylim=NULL, indic=NULL, x11=TRUE, ...) {

  stopifnot(xts::is.xts(xtsv))
  if (x11) {
    x11(10, 7)  # open x11 plot window
  }  # end if
  # Set plot margins
  par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
  # Pass color parameters into chart_Series() using chart_theme()
  plotheme <- quantmod::chart_theme()
  if (!is.null(colors)) {
    plotheme$col$line.col <- colors
  }  # end if
  # Extract chob chart object
  chobj <- quantmod::chart_Series(x=xtsv,
                                  theme=plotheme,
                                  plot=FALSE, ...)
  # Modify ylim using accessor and setter functions
  if (!is.null(ylim)) {
    ylim <- chobj$get_ylim()
    ylim[[2]] <- structure(ylim, fixed=TRUE)
    chobj$set_ylim(ylim)
  }  # end if
# Add vertical background shading
  if (!is.null(indic)) {
    if (!xts::is.xts(indic))
      indic <- xts::xts(indic, order.by=zoo::index(xtsv))
    quantmod::add_TA(indic, on=-1, col="lightgreen", border=NA)
    quantmod::add_TA(!indic, on=-1, col="antiquewhite", border=NA)
  }  # end if

  # Render the plot and return the chob invisibly
  plot(chobj)
  invisible(chobj)

}  # end chart_xts



############################################################
#' Plot two \emph{xts} time series with two y-axes in an x11 window.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series with two columns.
#' @param \code{color} A string specifying the color of the second line and
#'   axis (the default is \code{"red"}).
#' @param \code{x11} A \emph{Boolean} argument: if \code{TRUE} then open x11
#'   window for plotting, else plot in standard window (the default is \code{TRUE}).
#' @param \code{...} Additional arguments to function \code{plot.zoo()}.
#'
#' @return The \emph{xtsv} column names returned invisibly, and a plot
#'   in an x11 window produced as a side effect.
#'
#' @details The function \code{chart_xts2y()} creates a plot of two \emph{xts}
#'   time series with two y-axes.
#'   By default \code{chart_xts2y()} opens and plots in an x11 window.
#'   The function \code{chart_xts2y()} uses the standard plotting functions from
#'   base \emph{R}, and the function \code{plot.zoo()} from package
#'   \href{https://cran.r-project.org/web/packages/zoo/index.html}{zoo}.
#'
#' @examples
#' # Plot two time series
#' rutils::chart_xts2y(cbind(quantmod::Cl(rutils::etfenv$VTI),
#'                           quantmod::Cl(rutils::etfenv$IEF))["2015"])

chart_xts2y <- function(xtsv, color="red", x11=TRUE, ...) {
  stopifnot(xts::is.xts(xtsv))
  if (x11) {
    x11(10, 7)  # open x11 plot window
  }  # end if
  # Set plot margins
  par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
  par(las=1)  # Set text printing to horizontal
  ## Plot with two y-axes - plot first time series
  zoo::plot.zoo(xtsv[, 1], lwd=2, xlab=NA, ylab=NA, ...)
  par(new=TRUE)  # Allow new plot on same chart
  # Plot second time series without y-axis
  zoo::plot.zoo(xtsv[, 2], xlab=NA, ylab=NA,
       lwd=2, yaxt="n", col=color, ...)
  # Plot second y-axis on right
  axis(side=4, col=color)
  # Add axis labels
  colv <- colnames(xtsv)
  mtext(colv[1], side=2, adj=-0.5, padj=-15)
  mtext(colv[2], side=4, adj=1.5, padj=-15, col=color)
  # Add title and legend
  title(main=paste0(colv, collapse=" and "),
        line=0.5)
  legend("top", legend=colv,
         bg="white", lty=c(1, 1), lwd=c(6, 6),
         col=c("black", color), bty="n")
  invisible(colv)
}  # end chart_xts2y



############################################################
#' Plot an interactive \emph{dygraphs} candlestick plot with background shading
#' for an \emph{OHLC} time series in \emph{xts} format.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series in \emph{xts} format.
#' @param \code{indic} A \emph{Boolean} time series in \emph{xts} format for
#'   specifying the shading areas, with TRUE indicating "lightgreen" shading,
#'   and FALSE indicating "antiquewhite" shading (the default is \code{NULL}).
#' @param \code{...} Additional arguments to function \code{dygraphs::dygraph()}.
#'
#' @return A \code{dygraphs} plot object, and a \code{dygraphs} plot produced as
#'   a side effect.
#'
#' @details The function \code{chart_dygraph()} creates an interactive dygraphs
#'   candlestick plot with background shading for an \emph{OHLC} time series.
#'   The function \code{chart_dygraph()} uses plotting functions from the
#'   package
#'   \href{https://cran.r-project.org/web/packages/dygraphs/index.html}{dygraphs}.
#'
#' @examples
#' # Plot an interactive dygraphs candlestick plot with background shading
#' ohlc <- rutils::etfenv$VTI
#' vwapv <- TTR::VWAP(price=quantmod::Ad(ohlc), volume=quantmod::Vo(ohlc), n=20)
#' ohlc <- cbind(ohlc[, c(1:3, 6)], vwapv)["2009-02/2009-04"]
#' rutils::chart_dygraph(ohlc, indic=(ohlc[, 4] > vwapv))

chart_dygraph <- function(ohlc, indic=NULL, ...) {
  stopifnot(inherits(ohlc, "xts"))
  # Create dygraphs object
  dyplot <- dygraphs::dygraph(ohlc, ...) %>% dygraphs::dyCandlestick()
  if (!is.null(indic)) {
    # Add shading to dygraph object
    indic <- rutils::diffit(indic)
    indic <- rbind(cbind(which(indic==1), 1),
                    cbind(which(indic==(-1)), -1))
    indic <- indic[order(indic[, 1]), ]
    indic <- rbind(c(1, -indic[1, 2]), indic,
                    c(NROW(ohlc), -indic[NROW(indic), 2]))
    indic <- data.frame(index(ohlc)[indic[, 1]], indic[, 2])
    # Add shading to dygraph object
    for (i in 1:(NROW(indic)-1)) {
      if (indic[i, 2] == 1)
        dyplot <- dyplot %>% dygraphs::dyShading(from=indic[i, 1], to=indic[i+1, 1], color="lightgreen")
      else
        dyplot <- dyplot %>% dygraphs::dyShading(from=indic[i, 1], to=indic[i+1, 1], color="antiquewhite")
    }  # end for
  }  # end if add shading
  # Render dygraph plot
  dyplot
}  # end chart_dygraph



############################################################
#' Plot an interactive \emph{dygraphs} line plot for two \emph{xts} time series,
#' with two \emph{"y"} axes.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series with two columns.
#' @param \code{...} Additional arguments to function \code{dygraphs::dygraph()}.
#'
#' @return A \code{dygraphs} plot object.
#'
#' @details The function \code{chart_dygraph2y()} creates an interactive dygraphs
#'   line plot with two \emph{"y"} axes.
#'   The function \code{chart_dygraph2y()} uses plotting functions from the
#'   package
#'   \href{https://cran.r-project.org/web/packages/dygraphs/index.html}{dygraphs}.
#'
#' @examples
#' # Plot an interactive dygraphs line plot with two "y" axes
#' pricev <- cbind(Ad(rutils::etfenv$VTI), Ad(rutils::etfenv$IEF))
#' colnames(pricev) <- get_name(colnames(pricev), posv=2)
#' rutils::chart_dygraph2y(pricev)

chart_dygraph2y <- function(xtsv, ...) {
  stopifnot(xts::is.xts(xtsv))
  colv <- colnames(xtsv)
  # Create dygraphs object
  dyplot <- dygraphs::dygraph(xtsv, main=paste(colv, collapse=" and "), ...) %>%
    dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
    dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
    dySeries(colv[2], axis="y2", col=c("red", "blue"))
  # Render dygraph plot
  dyplot
}  # end chart_dygraph2y



############################################################
#' Load \emph{OHLC} time series data into an environment, either from an
#' external source (download from \emph{YAHOO}), or from \emph{CSV} files in a
#' local drive.
#'
#' @export
#' @param \code{symbolv} A vector of strings representing instrument symbols
#'   (tickers).
#' @param \code{datadir} The directory containing \emph{CSV} files (the default is
#'   \code{NULL}).
#' @param \code{datenv} The environment for loading the data into.
#' @param \code{startd} The start date of time series data (the default is
#'   "2007-01-01").
#' @param \code{endd} The end date of time series data (the default is
#'   \code{Sys.Date()}).
#' @param \code{func} The name of the function for formatting the date
#'   fields in the \emph{CSV} files (the default is \code{as.Date()}).
#' @param \code{formatv} The format of the date fields in the \emph{CSV} files
#'   (the default is \code{\%Y-\%m-\%d}).
#' @param \code{header} A \emph{Boolean} argument: if \code{TRUE} then read the
#'   header in the \emph{CSV} files (the default is \code{TRUE}).
#' @param \code{echo} A \emph{Boolean} argument: if \code{TRUE} then print to
#'   console information on the progress of \emph{CSV} file loading (the default is
#'   \code{TRUE}).
#' @param \code{scrub} A \emph{Boolean} argument: if \code{TRUE} then remove
#'   \code{NA} values using function \code{rutils::nalocf()} (the default is
#'   \code{TRUE}).
#'
#' @return A vector of \code{symbolv} returned invisibly.
#'
#' @details The function \code{get_data()} loads \emph{OHLC} time series data
#'   into an environment (as a side-effect), and returns invisibly the vector of
#'   \code{symbolv}.
#'
#'   If the argument \code{datadir} is specified, then \code{get_data()} loads
#'   from \emph{CSV} files in that directory, and overwrites \code{NA} values if
#'   \code{scrub=TRUE}.
#'   If the argument \code{datadir} is \emph{not} specified, then
#'   \code{get_data()} downloads adjusted \emph{OHLC} prices from \emph{YAHOO}.
#'
#'   The function \code{get_data()} calls the function \code{getSymbols.yahoo()}
#'   for downloading data from \emph{YAHOO}, and performs a similar operation to
#'   the function \code{getSymbols()} from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   But \code{get_data()} is faster because it performs less overhead
#'   operations, and it's able to handle symbols like \emph{LOW}, which
#'   \code{getSymbols()} can't handle because the function \code{Lo()} can't
#'   handle them. The \code{startd} and \code{endd} must be either of
#'   class \emph{Date}, or a string in the format \emph{"YYYY-mm-dd"}.
#'
#' @examples
#' \dontrun{
#' newenv <- new.env()
#' # Load prices from local csv files
#' rutils::get_data(symbolv=c("SPY", "EEM"),
#'             datadir="C:/Develop/data/bbg_records",
#'             datenv=newenv)
#' # Download prices from YAHOO
#' rutils::get_data(symbolv=c("MSFT", "XOM"),
#'             datenv=newenv,
#'             startd="2012-12-01",
#'             endd="2015-12-01")
#' }

get_data <- function(symbolv,
                     datadir=NULL, # The directory containing csv files
                     datenv, # The environment for writing xts into
                     startd="2007-01-01",
                     endd=Sys.Date(),
                     func=match.fun("as.Date"),
                     formatv="%Y-%m-%d",
                     header=TRUE,
                     echo=TRUE,
                     scrub=TRUE) {
  if (is.null(datadir)) {
    # Download prices from YAHOO
    outv <- quantmod::getSymbols.yahoo(symbolv,
                                          env=datenv,
                                          from=startd,
                                          to=endd,
                                          adjust=TRUE)
    # Adjust the OHLC prices and save back to datenv
    # outv <- lapply(symbolv,
    #                   function(symbol) {
    #                     assign(symbol,
    #                            value=adjust_ohlc(get(symbol, envir=datenv)),
    #                            envir=datenv)
    #                     symbol
    #                   }
    # )  # end lapply
    invisible(outv)
  }
  else {
    # Load from csv files
    file_names <- file.path(datadir, paste0(symbolv, ".csv"))
    invisible(sapply(file_names, function(file_name) {
      if (echo)
        cat("Loading instrument: \t", file_name, "\n")
      datav <- xts::as.xts(zoo::read.zoo(file=file_name,
                                         header=header, sep=",",
                                         drop=FALSE,
                                         FUN=func,
                                         format=formatv))
      if (scrub) {
        # overwrite NA values
        datav <- rutils::nalocf(datav)
        datav <- rutils::nalocf(datav, fromLast=TRUE)
      }  # end if
      assign(rutils::get_name(colnames(datav)[1]),
             datav,
             envir=datenv)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data



############################################################
#' Download an \emph{OHLC} time series of prices from Polygon.
#'
#' @export
#' @param \code{symbol} The stock symbol (ticker).
#'
#' @param \code{startd} The start date (the default is "1997-01-01").
#'
#' @param \code{endd} The end date (the default is \code{Sys.Date()}).
#'
#' @param \code{tspan} The data frequency, i.e. the time span for data
#'   aggregations (the default is "day" for daily data).
#'
#' @param \code{apikey} The API key issued by Polygon.
#'
#' @return An \emph{OHLC} time series of class \emph{xts}.
#'
#' @details The function \code{getpoly()} downloads historical prices from
#'   \href{https://polygon.io}{Polygon}, and returns an \emph{OHLC} time series
#'   of class \emph{xts}.
#'
#'   \href{https://polygon.io}{Polygon} is a provider of live and historical
#'   prices for stocks, options, foreign currencies, and cryptocurrencies.
#'
#'   The function \code{getpoly()} sends a request for data to the
#'   \href{https://polygon.io}{Polygon} rest API, using the function
#'   \code{read_json()} from package
#'   \href{https://cran.r-project.org/web/packages/jsonlite/index.html}{jsonlite}.
#'   The query requires an API key issued by \href{https://polygon.io}{Polygon}.
#'   The API key must be passed to the argument \code{apikey}.
#'
#'   \href{https://polygon.io}{Polygon} returns data in \emph{JSON} format,
#'   which is then formatted into an \emph{OHLC} time series of class
#'   \emph{xts}.
#'
#'   The argument \code{tspan} determines the data frequency, i.e. it's the time
#'   span for data aggregations.  The default is "day" for daily data.  Other
#'   possible values of \code{tspan} are "minute", "hour", "week", and "month".
#'
#' @examples
#' \dontrun{
#' # Polygon API key - user must obtain their own key
#' apikey <- "0Q2f7j8CwAbdY5M8VYt_8pwdP0V4TunxbvRVC_"
#' # Download SPY prices from Polygon
#' ohlc <- rutils::getpoly(symbol="SPY", apikey=apikey)
#' # Plot candlesticks of SPY OHLC prices
#' library(highcharter)
#' highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")
#' }

getpoly <- function(symbol="SPY", startd=as.Date("1997-01-01"), endd=Sys.Date(), tspan="day", apikey) {

  # Create url for download
  urll <- "https://api.polygon.io/v2/aggs/ticker/"
  urll <- paste0(urll, symbol, "/range/1/", tspan, "/", startd, "/", endd,
                 "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
  # Download JSON format data with OHLC bars
  ohlc <- jsonlite::read_json(urll)
  # Extract list of prices from JSON object
  ohlc <- ohlc$results
  # Coerce from list to matrix
  ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
  ohlc <- do.call(rbind, ohlc)
  # Coerce time from milliseconds to date-time
  dates <- ohlc[, "t"]/1e3
  dates <- as.POSIXct(dates, origin="1970-01-01")
  # Coerce from matrix to xts
  ohlc <- ohlc[, -1]
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
  # Return xts
  xts::xts(ohlc, order.by=dates)

}  # end getpoly



############################################################
#' Calculate the density of the non-standard Student's t-distribution.
#'
#' @param \code{x} A \emph{numeric} value for which to calculate the density of
#'   the t-distribution.
#'
#' @param \code{dfree} An \emph{integer} value equal to the degrees of freedom
#'   (the default is \code{3}).
#'
#' @param \code{loc} A \emph{numeric} value equal to the location (center) of
#'   the distribution (the default is \code{0}).
#'
#' @param \code{scalev} A \emph{numeric} value equal to the scale (width) of the
#'   distribution (the default is \code{1}).
#'
#' @return A \emph{numeric} value equal to the density of the non-standard
#'   Student's t-distribution.
#'
#' @details The function \code{tdistr()} calculates the density of the
#'   non-standard Student's t-distribution by calling the function
#'   \code{gamma()}.
#'   The density function of the non-standard Student's t-distribution is given
#'   by:
#'   \deqn{
#'     f(t) = \frac{\Gamma((\nu+1)/2)}{\sqrt{\pi \nu} \, \sigma \, \Gamma(\nu/2)} \, (1 + (\frac{t - \mu}{\sigma})^2/\nu)^{-(\nu+1)/2}
#'   }
#' Where \eqn{\Gamma()} is the gamma function, and \eqn{\nu} are the degrees of
#' freedom.
#'
#' The non-standard Student's density function has a mean equal to the location
#' parameter \eqn{\mu}, and a standard deviation proportional to the scale
#' parameter \eqn{\sigma}.
#'
#' @examples
#' \dontrun{
#' # Student t-distribution at x=1, with df=4, location=-1 and scale=2
#' tdistr(1, df=4, loc=-1, scalev=2)
#' # Student t-distribution at x=1, with location=0 and scale=1 - same as dt()
#' all.equal(tdistr(1, df=3), dt(1, df=3))
#' }
#'
#' @export
tdistr <- function(x, dfree=3, loc=0, scalev=1) {

  gamma((dfree+1)/2)/(sqrt(pi*dfree)*gamma(dfree/2)*scalev)*
    (1+((x-loc)/scalev)^2/dfree)^(-(dfree+1)/2)

  # Equivalent code, bust slightly slower.
  # dt((x-loc)/scalev, df=dfree)/scalev

}  # end tdistr


############################################################
## Optimization
############################################################


############################################################
#' Perform one-dimensional optimization using the Newton-Raphson method.
#'
#' @param \code{f} An objective function to minimize.
#'
#' @param \code{x0} The initial starting value for the optimization.
#'
#' @param \code{h} The step size for calculating numerical derivatives (default 1e-5).
#'
#' @param \code{maxiter} The maximum number of iterations (default 50).
#'
#' @param \code{tol} The tolerance for convergence based on the first derivative (default 1e-8).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{par}: The optimal value of \code{x} that minimizes the function.
#'     \item \code{history}: A vector of \code{x} values at each iteration.
#'   }
#'
#' @details
#'   The function \code{optim_newton()} finds the minimum of a univariate
#'   function using the Newton-Raphson method. It updates the estimate in a loop
#'   using the recursive formula:
#'   \deqn{
#'     x_{n+1} = x_n - \frac{f'(x_n)}{f''(x_n)}
#'   }
#'
#'   The derivatives are calculated numerically using central finite differences:
#'   \itemize{
#'     \item First derivative: \eqn{f'(x) \approx \frac{f(x+h) - f(x-h)}{2h}}
#'     \item Second derivative: \eqn{f''(x) \approx \frac{f(x+h) - 2f(x) + f(x-h)}{h^2}}
#'   }
#'
#'   The algorithm converges when the absolute value of the first derivative
#'   falls below the tolerance \code{tol}, indicating a stationary point.
#'
#' @examples
#' # Calculate the minimum using quasi-Newton method
#' funx <- function(x) x^4 - 3*x^3 + 2
#' optiml <- rutils::optim_newton(funx, x0 = 3)
#' optiml$par  # Solution
#' optiml$history  # Optimization path
#' plot(optiml$history, type="b", main="Newton-Raphson Optimization Path",
#'      xlab="iteration", ylab="x value")
#'
#' @export
optim_newton <- function(f, x0, h = 1e-5, maxiter = 50, tol = 1e-8) {
  # Initialize the variables
  x <- x0                     # Initial guess for coordinate
  histv <- numeric(maxiter + 1) # History of coordinate updates
  histv[1] <- x
  # Iterate using Newton-Raphson formula
  for (n in 1:maxiter) {

    # Calculate the first derivative
    fph <- f(x + h)
    fmh <- f(x - h)
    fp <- (fph - fmh) / (2 * h)
    # Calculate the second derivative
    fpp <- (fph - 2 * f(x) + fmh) / (h^2)
    # Update the coordinate
    x <- x - fp / fpp
    histv[n + 1] <- x
    # Check for convergence
    if (abs(fp) < tol) {
      return(list(par = x, history = histv[1:n]))
    } # end if

  } # end for n

  # Return the results
  return(list(par = x, history = histv))

} # end optim_newton



############################################################
#' Perform multivariate optimization using the BFGS method.
#'
#' @param \code{f} An objective function to minimize (must accept a single
#'   vector argument).
#'
#' @param \code{x0} A vector of initial starting values for the optimization.
#'
#' @param \code{maxiter} The maximum number of iterations (default 200).
#'
#' @param \code{tol} The tolerance for convergence based on the step size and
#'   increment norm (default 1e-6).
#'
#' @param \code{h} The step size for calculating numerical gradients using
#'   central differences (default 1e-5).
#'
#' @param \code{c1} The Armijo condition tolerance parameter (default 1e-4).
#'
#' @param \code{rho} The scaling factor for the Armijo condition (default 0.5).
#'
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{par}: The optimal parameter vector that minimizes the function.
#'     \item \code{value}: The function value at the minimum.
#'     \item \code{grad}: The gradient at the minimum.
#'     \item \code{history}: A matrix of parameter values at each iteration.
#'     \item \code{iter}: The number of iterations performed.
#'   }
#'
#' @details
#'   The function \code{optim_bfgs()} finds the minimum of a multivariate
#'   function using the Broyden-Fletcher-Goldfarb-Shanno (BFGS) quasi-Newton
#'   method.
#'
#'   The input objective function \code{f} must accept a single vector argument.
#'   The vector argument contains the variables (coordinates) of the function to
#'   be minimized.
#'
#'   It updates the estimate of the minimum coordinates using a
#'   recursive formula:
#'   \deqn{
#'     x_{n+1} = x_n + \alpha \delta_n
#'   }
#'   Where \eqn{\delta_n = -H^{-1}_n \nabla f(x_n)} is the increment of $x$,
#'   \eqn{H^{-1}_n} is the inverse Hessian, and \eqn{\alpha} is the scaling
#'   factor to satisfy the Armijo condition.
#'
#'   The Armijo condition ensures that the objective function decreases with
#'   each iteration and that it doesn't overshoot the minimum:
#'   \deqn{
#'     f(x_n + \alpha \delta_n) \leq f(x_n) + c_1 \alpha \nabla f_n^T \delta_n
#'   }
#'   If the Armijo condition is not satisfied, then the factor \eqn{\alpha} is
#'   multiplied by the factor \eqn{rho} until the condition is satisfied or the
#'   step size becomes too small.
#'
#'   The gradient is calculated numerically using central differences:
#'   \deqn{
#'     \nabla f = \frac{\partial f}{\partial x} \approx \frac{f(x + h) - f(x - h)}{2h}
#'   }
#'
#'   The inverse Hessian is updated using the BFGS formula:
#'   \deqn{
#'     H^{-1}_{n+1} = V_n H^{-1}_n V_n^T + \rho_n s_n s_n^T
#'   }
#'   where \eqn{s_n = \alpha \delta_n}, \eqn{y_n = \nabla f_{n+1} - \nabla f_n},
#'   \eqn{\rho_n = 1 / (y_n^T s_n)}, and \eqn{V_n = I - \rho_n s_n y_n^T}.
#'
#'   The BFGS iteration terminates when the increment norm or step size fall
#'   below the tolerance, or when the maximum iterations are reached.
#'
#'   The advantage of the BFGS method is that it converges more rapidly than
#'   simple gradient descent because it approximates second-order information of
#'   the objective function.
#'   The disadvantage is that it requires more memory to store the whole inverse
#'   Hessian matrix. This is a limitation for very high-dimensional objective
#'   functions.
#'
#' @examples
#' # Define the Rosenbrock function
#' rosenbrock <- function(x) {
#'   (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
#' } # end rosenbrock
#' # Calculate the minimum using BFGS method
#' optiml <- rutils::optim_bfgs(rosenbrock, x0 = c(-1.2, 1))
#' optiml$par      # Minimum coordinates
#' optiml$value    # Function value at the minimum
#' optiml$iter     # Number of iterations
#' # Solve using optim() for comparison
#' optim(c(-1.2, 1), rosenbrock, method = "L-BFGS-B")
#'
#' @export
optim_bfgs <- function(f, x0, maxiter = 200, tol = 1e-6,
                       h = 1e-5, c1 = 1e-4, rho = 0.5) {

  # Initialize the variables
  tol2 <- tol^2               # Square of tolerance for solution
  x <- x0                     # Initial guess for solution
  narg <- NROW(x)             # Number of coordinates

  # Calculate the gradient using central difference
  calc_grad <- function(x) {
    grad <- numeric(narg)
    for (j in 1:narg) {
      e <- rep(0, narg); e[j] <- 1
      fp <- f(x + h * e)
      fm <- f(x - h * e)
      grad[j] <- (fp - fm) / (2 * h)
    } # end for j
    return(grad)
  } # end calc_grad

  # Initialize the function value, gradient and inverse Hessian
  fx <- f(x)                  # Function value
  grad <- calc_grad(x)          # Gradient
  H <- diag(narg)            # Inverse Hessian

  # Initialize the history matrix
  history <- matrix(NA_real_, nrow = maxiter + 1, ncol = narg)
  history[1, ] <- x; itern <- 1

  # Iterate to find the minimum
  for (n in 1:maxiter) {

    dn <- - H %*% grad  # Increment of x
    # Stop if the increment is too small
    if (sum(dn^2) < tol2) break

    # Perform loop to satisfy the Armijo condition
    alpha <- 1
    unchflag <- FALSE # Flag if x is unchanged
    while (alpha > 1e-12) {
      xn <- as.vector(x + alpha * dn) # New solution
      fxn <- f(xn)                  # New function value

      # Check for convergence based on step size
      if (sum((xn - x)^2) < tol2) { # Step size norm
        unchflag <- TRUE # x is unchanged - stop loop
        break
      } # end if

      # Armijo condition for sufficient decrease in function value
      if (fxn <= fx + c1 * alpha * sum(grad * dn)) break
      # Decrease the step size
      alpha <- alpha * rho
    } # end while

    # If unchanged condition is detected, accept the current values and stop loop
    if (unchflag) {
      x <- xn; fx <- fxn
      history[itern, ] <- x; itern <- itern + 1
      break
    } # end if

    # If the Armijo condition failed then stop
    if (alpha <= 1e-12) break

    # BFGS update of the gradient and inverse Hessian
    dx <- xn - x
    gradn <- calc_grad(xn)
    dg <- gradn - grad
    ys <- sum(dg * dx)
    if (ys > 1e-12) {
      I <- diag(narg)
      V <- I - outer(dx, dg) / ys
      H <- V %*% H %*% t(V) + outer(dx, dx) / ys
    } # end if

    # Copy variables for next iteration
    x <- xn; grad <- gradn; fx <- fxn

    # Store the history
    history[itern, ] <- x; itern <- itern + 1

  } # end for n

  # Trim unused rows
  history <- history[1:(itern - 1), , drop = FALSE]
  # Return the results
  return(list(par = x, value = fx, grad = grad,
              history = history, iter = itern - 1))

} # end optim_bfgs



############################################################
#' Perform multivariate optimization using coordinate descent with
#' quasi-Newton updates.
#'
#' @param \code{f} An objective function to minimize (must accept a single
#'   vector argument).
#'
#' @param \code{x0} A vector of initial starting values for the optimization.
#'
#' @param \code{maxiter} The maximum number of outer iterations (default 100).
#'
#' @param \code{tol} The tolerance for convergence based on the step size
#'   (default 1e-6).
#'
#' @param \code{h} The step size for calculating numerical gradients using
#'   central differences (default 1e-5).
#'
#' @param \code{c1} The Armijo condition tolerance parameter (default 1e-4).
#'
#' @param \code{rho} The scaling factor for the Armijo condition (default 0.5).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{par}: The optimal parameter vector that minimizes the function.
#'     \item \code{value}: The function value at the minimum.
#'     \item \code{grad}: The gradient at the minimum.
#'     \item \code{history}: A matrix of parameter values at each iteration.
#'     \item \code{iter}: The number of iterations performed.
#'   }
#'
#' @details
#'   The function \code{optim_coordescent()} finds the minimum of a multivariate
#'   function by optimizing along each coordinate direction, one at a time in a
#'   loop.
#'
#'   The input objective function \code{f} must accept a single vector argument.
#'   The vector argument contains the variables (coordinates) of the function to
#'   be minimized.
#'
#'   It uses the BFGS quasi-Newton formula to approximate the inverse Hessian
#'   along each coordinate direction separately.
#'
#'   The update for coordinate \eqn{x_{j, n}} is calculated using the increment
#'   \eqn{\delta_j = -H^{-1}_j \nabla f_j}:
#'   \deqn{
#'     x_{j, n+1} = x_{j, n} + \alpha \delta_j
#'   }
#'   Where \eqn{H^{-1}_j} is the approximation of the inverse Hessian for
#'   coordinate \eqn{j}, \eqn{\nabla f_j} is the gradient (partial derivative),
#'   and \eqn{\alpha} is the scaling factor to satisfy the Armijo condition.
#'
#'   For each coordinate, the Armijo condition is applied to ensure that the
#'   objective function decreases with each iteration and that it doesn't
#'   overshoot the minimum:
#'   \deqn{
#'     f(x + \alpha \delta_j) \leq f(x) + c_1 \alpha \nabla f_j \delta_j
#'   }
#'   Where \eqn{\delta_j = x_{j, n+1} - x_{j, n}} is the increment of the
#'   coordinate \eqn{j}.
#'
#'   If the Armijo condition is not satisfied, the factor \eqn{\alpha} is
#'   multiplied by the factor \eqn{rho} until the condition is satisfied or the
#'   step size becomes too small.
#'
#'   The gradient is calculated numerically using central differences:
#'   \deqn{
#'     \nabla f_j = \frac{\partial f}{\partial x_j} \approx \frac{f(x + h e_j) - f(x - h e_j)}{2h}
#'   }
#'   Where \eqn{e_j} is the unit vector along coordinate \eqn{j}.
#'
#'   The inverse Hessian \eqn{H^{-1}_{j, n+1}} for coordinate \eqn{j} at step
#'   \eqn{n+1} is updated using the secant formula:
#'   \deqn{
#'     H^{-1}_{j, n+1} = \frac{\alpha \delta_j}{\nabla f_{j, n+1} - \nabla f_{j, n}}
#'   }
#'   The secant formula states that the inverse Hessian is equal to the ratio of
#'   the change of the coordinate divided by the change of the gradient.
#'
#'   This is equivalent to saying that the Hessian (second derivative) is equal
#'   to the ratio of the change of the gradient divided by the change of the
#'   coordinate:
#'   \deqn{
#'     H_{j, n+1} = \frac{\nabla f_{j, n+1} - \nabla f_{j, n}}{\alpha \delta_j}
#'   }
#'
#'   The coordinate descent loop terminates when the norm of the total step
#'   across all coordinates falls below the tolerance, or when maximum
#'   iterations are reached.
#'
#'   The advantage of the coordinate descent method is that it doesn't require
#'   memory to store the whole inverse Hessian matrix. Each coordinate update is
#'   very fast since it doesn't have to multiply large matrices. So it's
#'   suitable for very high-dimensional objective functions.
#'
#'   The disadvantage is that it can be slower to converge than quasi-Newton
#'   methods like BFGS. For certain functions like the Rosenbrock function, it
#'   may get stuck in suboptimal solutions.
#'
#' @examples
#' # Define the objective function
#' funx <- function(v) {
#'   (v[1] - 2)^2 + (v[2] + 1)^2
#' } # end funx
#' # Calculate the minimum using coordinate descent method
#' optiml <- rutils::optim_coordescent(funx, x0 = c(-2, 1))
#' optiml$par      # Minimum coordinates
#' optiml$value    # Function value at the minimum
#' optiml$iter     # Number of iterations
#' # Solve using optim() for comparison
#' optim(c(0, 0), funx, method = "L-BFGS-B")
#'
#' @export
optim_coordescent <- function(f, x0, maxiter = 100, tol = 1e-6,
                              h = 1e-5, c1 = 1e-4, rho = 0.5) {

  # Initialize the variables
  tol2 <- tol^2               # Square of tolerance for solution
  x <- x0                     # Initial guess for solution
  narg <- NROW(x)             # Number of coordinates
  H <- rep(1, narg)           # Diagonal inverse-Hessian approximation

  # Calculate the gradient using central difference
  calc_grad <- function(x) {
    grad <- numeric(narg)
    for (j in 1:narg) {
      e <- rep(0, narg); e[j] <- 1
      fp <- f(x + h * e)
      fm <- f(x - h * e)
      grad[j] <- (fp - fm) / (2 * h)
    } # end for j
    return(grad)
  } # end calc_grad

  # Initialize the function value and gradient
  fx <- f(x) # Function value
  grad <- calc_grad(x) # Gradient

  # Initialize the history matrix
  history <- matrix(NA_real_, nrow = maxiter + 1, ncol = narg)
  history[1, ] <- x; itern <- 1

  # Iterate to find the minimum
  for (n in 1:maxiter) {
    xold <- x # Previous value of x

    # Loop over the coordinates
    for (j in 1:narg) {
      gj <- grad[j]
      if (abs(gj) < tol2) next # Skip this coordinate if gradient is too small

      # The increment of the coordinate j
      dj <- - H[j] * gj
      if (abs(dj) < tol2) next # Skip this coordinate if step is too small

      # Perform Armijo condition loop along coordinate j
      alpha <- 1
      repeat {
        xtr <- x # Trial value of x
        xtr[j] <- x[j] + alpha * dj
        fxtr <- f(xtr)  # Trial function value
        # Break if Armijo condition is satisfied
        if ((fxtr <= fx + c1 * alpha * gj * dj) || (alpha < 1e-12))
          break
        # Reduce the step size and repeat test
        alpha <- alpha * rho
      } # end repeat
      # If the Armijo condition failed then skip this coordinate
      dx <- alpha * dj
      if (abs(dx) < tol2) next

      # Accept the step
      x[j] <- x[j] + dx
      fx <- fxtr

      # Update gradient component using central difference
      e <- rep(0, narg); e[j] <- 1
      fp <- f(x + h * e)[1]
      fm <- f(x - h * e)[1]
      gn <- (fp - fm) / (2 * h) # New gradient component
      dg <- (gn - gj) # Change in gradient

      # Hessian secant update
      if (dg * dx > 1e-12) {
        H[j] <- max(1e-12, dx / dg)
      } else {
        H[j] <- 1
      } # end if
      # Update the gradient
      grad[j] <- gn

    } # end loop over the coordinates

    # Step-size stopping condition
    if (sum((x - xold)^2) < tol2) {
      itern <- itern + 1
      history[itern, ] <- x
      break
    } # end if

    # Recompute full gradient
    grad <- calc_grad(x)

    # Store the history
    itern <- itern + 1
    history[itern, ] <- x

  } # end for n

  # Trim unused rows
  history <- history[1:itern, , drop = FALSE]
  # Return the results
  return(list(par = x, value = fx, grad = grad,
              history = history, iter = itern - 1))

} # end optim_coordescent


