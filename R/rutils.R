######################################################################
#' Extract symbol names (tickers) from a vector of \emph{character} strings.
#'
#' @export
#' @param \code{strng} A vector of \emph{character} strings containing symbol
#'   names.
#'
#' @param \code{field} The position of the name in the string, i.e. the integer
#'   index of the field to be extracted (default is \code{1}, i.e. the name is
#'   at the beginning of the string,)
#'
#' @param \code{sep} The name separator, i.e. the single \emph{character}
#'   that separates the symbol name from the rest of the string (default is
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
#' rutils::get_name(c("VTI.Open", "VTI.High", "VTI.Low", "VTI.Close"), field=2)

get_name <- function(strng, field=1, sep="[.]") {
  # strng <- strsplit(strng, split=sep)
  # sapply(strng, function(x) x[field])
  do.call(rbind, strsplit(strng, split=sep))[, field]
}  # end get_name



######################################################################
#' Calculate a vector of equally spaced end points along the elements of a
#' vector, matrix, or time series.
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or time series.
#' @param \code{interval} The number of elements between neighboring end
#'   points. or a \emph{string} representing a time period (minutes, hours,
#'   days, etc.)
#' @param \code{stub_front} A \emph{Boolean} argument: if \code{TRUE} then add a
#'   stub interval at the beginning, else add a stub interval at the end.
#'   (default is \code{TRUE})
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



######################################################################
#' Replace \code{NA} values with the most recent non-\code{NA} values prior to
#' them.
#'
#' @export
#' @param \code{input} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#' @param \code{from_last} A  \emph{Boolean} argument: should non-\code{NA}
#'   values be carried backward rather than forward? (default is \code{FALSE})
#' @param \code{na_rm} A \emph{Boolean} argument: should any remaining (leading
#'   or trailing) \code{NA} values be removed? (default is \code{FALSE})
#' @param \code{max_gap} The maximum number of neighboring \code{NA} values that
#'   can be replaced (default is \code{NROW(input)}).
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   and data type as the argument \code{input}.
#'
#' @details The function \code{na_locf()} replaces \code{NA} values with the
#'   most recent non-\code{NA} values prior to them.
#'
#'   If the \code{from_last} argument is \code{FALSE} (the default), then the
#'   previous or past non-\code{NA} values are carried forward to replace the
#'   \code{NA} values.
#'   If the \code{from_last} argument is \code{TRUE}, then the following or
#'   future non-\code{NA} values are carried backward to replace the \code{NA}
#'   values.
#'
#'   The function \code{na_locf()} performs the same operation as function
#'   \code{xts:::na.locf.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/zoo/index.html}{zoo}, but
#'   it also accepts vectors as input.
#'
#'   The function \code{na_locf()} calls the compiled function \code{na_locf()}
#'   from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, which
#'   allows it to perform its calculations about three times faster than
#'   \code{xts:::na.locf.xts()}.
#'
#' @examples
#' # Create vector containing NA values
#' input <- sample(22)
#' input[sample(NROW(input), 4)] <- NA
#' # Replace NA values with the most recent non-NA values
#' rutils::na_locf(input)
#' # Create matrix containing NA values
#' input <- sample(44)
#' input[sample(NROW(input), 8)] <- NA
#' input <- matrix(input, nc=2)
#' # Replace NA values with the most recent non-NA values
#' rutils::na_locf(input)
#' # Create xts series containing NA values
#' input <- xts::xts(input, order.by=seq.Date(from=Sys.Date(),
#'   by=1, length.out=NROW(input)))
#' # Replace NA values with the most recent non-NA values
#' rutils::na_locf(input)

na_locf <- function(input, from_last=FALSE, na_rm=FALSE, max_gap=NROW(input)) {

  if (!(is.numeric(input) || is.logical(input) || xts::is.timeBased(input) || xts::is.xts(input))) {  # input is not numeric
    warning(paste("Argument", deparse(substitute(input)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  if (NCOL(input) > 1) {
    for (n in 1:NCOL(input))
      input[, n] <- .Call("na_locf", input[, n], from_last, max_gap, Inf, PACKAGE="xts")
  }
  else {
    input <- .Call("na_locf", input, from_last, max_gap, Inf, PACKAGE="xts")
  }
  if (na_rm) {
    return(structure(na.omit(input), na.action=NULL))
  }
  else
    input
  ### old version below
  # if (NCOL(input) > 1)
  #   return(apply(input, MARGIN=2, rutils::na_locf))
  # if (from_last && is.na(input[1]))
  #   input[1] <- 0
  # if (!from_last && is.na(input[NROW(input)]))
  #   input[NROW(input)] <- 0
  # indeks <- is.na(input)
  # indices <- rutils::diffit(indeks)
  # indices <- which(indices==1)
  # indices <- indices - 1
  # diff_indices <- rutils::diffit(indices)
  # diff_indices[1] <- indices[1]
  # new_indices <- numeric(NROW(input))
  # new_indices[indices] <- diff_indices
  # new_indices <- cumsum(new_indices)
  # input[indeks] <- input[new_indices[indeks]]
  # input
}  # end na_locf



######################################################################
#' Aggregate an \emph{OHLC} time series to a lower periodicity.
#'
#' Given an \emph{OHLC} time series at high periodicity (say seconds),
#' calculates the \emph{OHLC} prices at a lower periodicity (say minutes).
#'
#' @param \code{tseries} An \emph{OHLC} time series of prices in \emph{xts}
#'   format.
#' @param \code{period} aggregation interval ("seconds", "minutes", "hours",
#'   "days", "weeks", "months", "quarters", and "years").
#' @param \code{k} The number of periods to aggregate over (for example if
#'   \code{period="minutes"} and \code{k=2}, then aggregate over two minute
#'   intervals.)
#' @param \code{endpoints} An integer vector of end points.
#'
#' @return A \emph{OHLC} time series of prices in \emph{xts} format, with a
#'   lower periodicity defined by the endpoints.
#'
#' @details The function \code{to_period()} performs a similar aggregation as
#'   function \code{xts::to.period()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but has
#'   the flexibility to aggregate to a user-specified vector of end points. The
#'   function \code{to_period()} simply calls the compiled function
#'   \code{toPeriod()} (from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}), to
#'   perform the actual aggregations.  If \code{endpoints} are passed in
#'   explicitly, then the \code{period} argument is ignored.
#'
#' @examples
#' \dontrun{
#' # Define end points at 10-minute intervals (HighFreq::SPY are minutes bars)
#' endd <- rutils::calc_endpoints(HighFreq::SPY["2009"], interval=10)
#' # Aggregate over 10-minute endpoints:
#' ohlc <- rutils::to_period(tseries=HighFreq::SPY["2009"], endpoints=endd)
#' # Aggregate over days:
#' ohlc <- rutils::to_period(tseries=HighFreq::SPY["2009"], period="days")
#' # Equivalent to:
#' ohlc <- xts::to.period(x=HighFreq::SPY["2009"], period="days", name=rutils::get_name(colnames(HighFreq::SPY)[1])
#' }
#'
#' @export
to_period <- function(tseries,
                      period="minutes", k=1,
                      endpoints=xts::endpoints(tseries, period, k)) {
  .Call("toPeriod", tseries, as.integer(endpoints), TRUE, NCOL(tseries),
        FALSE, FALSE, colnames(tseries), PACKAGE="xts")
}  # end to_period



######################################################################
#' Extract columns of data from \emph{OHLC} time series using column field
#' names.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series in \emph{xts} format, or a
#'   vector of \emph{character} strings with the names of \emph{OHLC} time
#'   series.
#' @param \code{field_name} A vector of strings with the field names of the
#'   columns to be be extracted (default is \code{"Close"}).
#' @param \code{data_env} The environment containing \emph{OHLC} time series
#'   (default is \code{NULL}).
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
#'   the format \code{"symbol.field_name"}, for example \code{"VTI.Close"}.
#'
#'   In the simplest case when \code{ohlc} is a single \emph{xts} time series
#'   and \emph{field_name} is a single string, the function \code{get_col()}
#'   performs a similar operation to the extractor functions \code{Op()},
#'   \code{Hi()}, \code{Lo()}, \code{Cl()}, and \code{Vo()}, from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   But \code{get_col()} is able to handle symbols like \emph{LOW}, which the
#'   function \code{Lo()} can't handle. The field_name argument is partially
#'   matched, for example "Vol" is matched to \code{Volume} (but it's case
#'   sensitive).
#'
#'   In the case when \code{ohlc} is a vector of strings with the names of
#'   \emph{OHLC} time series, the function \code{get_col()} reads the
#'   \emph{OHLC} time series from the environment \code{data_env}, extracts the
#'   specified columns, and binds them into a single \emph{xts} time series.
#'
#' @examples
#' # get close prices for VTI
#' rutils::get_col(rutils::etfenv$VTI)
#' # get volumes for VTI
#' rutils::get_col(rutils::etfenv$VTI, field_name="Vol")
#' # get close prices and volumes for VTI
#' rutils::get_col(rutils::etfenv$VTI, field_name=c("Cl", "Vol"))
#' # get close prices and volumes for VTI and IEF
#' rutils::get_col(ohlc=c("VTI", "IEF"), field_name=c("Cl", "Vol"),
#'   data_env=rutils::etfenv)

get_col <- function(ohlc, field_name="Close", data_env=NULL) {
  if (is.xts(ohlc)) {
    # Extract columns, and return them
    colv <- strsplit(colnames(ohlc), split="[.]")
    colv <- sapply(colv, function(coln) coln[2])
    return(ohlc[, pmatch(field_name, colv)])
  } else if (is.character(ohlc)) {
    # Loop over the symbols in ohlc, extract columns, and cbind them
    datav <- lapply(ohlc, function(symbol) {
      ohlc <- get(symbol, envir=data_env)
      colv <- strsplit(colnames(ohlc), split="[.]")
      colv <- sapply(colv, function(coln) coln[2])
      ohlc[, pmatch(field_name, colv)]
    })  # end lapply
    return(rutils::do_call(cbind, datav))
  }  # end if
  ## below is a different version using as.list()
  # datav <- lapply(as.list(data_env)[ohlc], function(ohlc) {
  #   colv <- strsplit(colnames(ohlc), split="[.]")
  #   colv <- sapply(colv, function(coln) coln[2])
  #   ohlc[, pmatch(field_name, colv)]
  # })  # end lapply
  #
  ## below is the old version using grep()
  #   indeks <- grep(paste0(".", field_name), colnames(ohlc), ignore.case=TRUE)
  #   if (NROW(indeks)>0)
  # #    output <- xts::.subset_xts(ohlc, 1:NROW(ohlc), indeks:indeks)
  #     ohlc[, indeks]
  #   else
  #     stop(paste0("No column name containing \"", field_name, "\""))
}  # end get_col



######################################################################
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



######################################################################
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
#'   days (default is \code{TRUE}).
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



######################################################################
#' Apply a lag to a \emph{numeric} or \emph{Boolean} vector, matrix, or
#' \emph{xts} time series.
#'
#' @export
#' @param \code{input} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#'
#' @param \code{lagg} An integer equal to the number of time periods (rows) of
#'   lag (default is \code{1}).
#'
#' @param \code{pad_zeros} A \emph{Boolean} argument: Should the output be padded
#'   with zeros? (The default is \code{pad_zeros = TRUE}.)
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

lagit <- function(input, lagg=1, pad_zeros=TRUE, ...) {

  if (lagg == 0) {
    # Return input if lagg is zero
    return(input)
  }  # end if

  if (!(is.numeric(input) || is.logical(input) || xts::is.timeBased(input) || xts::is.xts(input))) {
    # Return NULL if input is not numeric
    warning(paste("Argument", deparse(substitute(input)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  nrows <- NROW(input)
  ncol <- NCOL(input)
  if (pad_zeros)
    padd <- 0
  else
    padd <- 1
  if (is.null(dim(input))) {  # input is a vector
    if (lagg>0) {
      input <- c(rep(padd*input[1], lagg), input)
      input[1:nrows]
    } else {
      input <- c(input, rep(padd*input[nrows], -lagg))
      input[-(1:(-lagg))]
    }  # end if
  } else if (xts::is.xts(input)) {  # input is an xts
    firstv <- input[1, ]
    lastv <- input[nrows, ]
    input <- xts::lag.xts(input, k=lagg, ...)
    if (lagg>0) {
      input[1:lagg, ] <- matrix(rep(padd*firstv, lagg), byrow=TRUE, nr=lagg)
    } else {
      input[(nrows+lagg+1):nrows, ] <- matrix(rep(padd*lastv, -lagg), byrow=TRUE, nr=-lagg)
    }  # end if
    return(input)
  } else if (is.matrix(input)) {  # input is a matrix
      if (lagg>0) {
        input <- rbind(matrix(rep(padd*input[1, ], lagg), byrow=TRUE, nr=lagg), input)
        input[1:nrows, ]
      } else {
        input <- rbind(input, matrix(rep(padd*input[nrows, ], -lagg), byrow=TRUE, nr=-lagg))
        input[-(1:(-lagg)), ]
      }  # end if
  } else {  # input is not a vector or matrix
    warning(paste0("Argument \"", deparse(substitute(input)), "\" must be either a vector, matrix, or xts series."))
    return(NULL)  # Return NULL
  }  # end if
}  # end lagit



######################################################################
#' Calculate the row differences of a \emph{numeric} or \emph{Boolean} vector,
#' matrix, or \emph{xts} time series.
#'
#' @export
#' @param \code{input} A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#'
#' @param \code{lagg} An integer equal to the number of time periods of lag
#'   (default is \code{1}).
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

diffit <- function(input, lagg=1, ...) {

  if (!(is.numeric(input) || is.logical(input) || xts::is.timeBased(input) || xts::is.xts(input))) {  # input is not numeric
    warning(paste("Argument", deparse(substitute(input)), "must be numeric, date, Boolean, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  nrows <- NROW(input)
  ncol <- NCOL(input)
  if (xts::is.xts(input)) {  # input is an xts
    if (lagg>0) {
      input <- xts::diff.xts(input, lag=lagg, ...)
      input[1:lagg, ] <- 0
    } else {
      input <- xts::diff.xts(input, lag=-lagg, ...)
      input[(nrows+lagg+1):nrows, ] <- 0
    }  # end if
    return(input)
  } else if (is.null(dim(input))) {  # input is a vector
    if (lagg>0) {
      lagg_ed <- c(input[1:lagg], input[1:(nrows-lagg)])
    } else {
      lagg_ed <- c(input[-(1:(-lagg))], input[(nrows+lagg+1):nrows])
    }  # end if
  } else if (is.matrix(input)) {  # input is a matrix
    if (lagg>0) {
      lagg_ed <- rbind(input[1:lagg, , drop=FALSE], input[1:(nrows-lagg), , drop=FALSE])
    } else {
      lagg_ed <- rbind(input[-(1:(-lagg)), , drop=FALSE], input[(nrows+lagg+1):nrows, , drop=FALSE])
    }  # end if
  } else {  # input is not a vector or matrix
    warning(paste0("Argument \"", deparse(substitute(input)), "\" must be either a vector, matrix, or xts series."))
    return(NULL)  # Return NULL
  }  # end if

  input - lagg_ed

}  # end diffit



######################################################################
#' Apply a time lag to an \emph{xts} time series.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series.
#'
#' @param \code{lagg} An integer equal to the number of time periods of lag
#'   (default is \code{1}).
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

  if (!xts::is.xts(input)) {
    # Return NULL if xtsv is not an xts series
    warning(paste("Argument", deparse(substitute(input)), "must be an xts series."))
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



######################################################################
#' Calculate the time differences of an \emph{xts} time series.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series.
#' @param \code{lagg} An integer equal to the number of time periods of lag
#'   (default is \code{1}).
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



######################################################################
#' Calculate the reduced form of an \emph{OHLC} time series, or calculate the
#' standard form from the reduced form of an \emph{OHLC} time series.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series of prices in \emph{xts} format.
#' @param \code{reducit} A \emph{Boolean} argument: should the reduced form be
#'   calculated or the standard form? (default is \code{TRUE})
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
#' diff_VTI <- rutils::diffohlc(rutils::etfenv$VTI)
#' # Calculate standard form of an OHLC time series
#' VTI <- rutils::diffohlc(diff_VTI, reducit=FALSE)
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



######################################################################
#' Calculate the rolling sum of a \emph{numeric} vector, matrix, or \emph{xts}
#' time series over a sliding window (lookback period).
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or \emph{xts} time series containing one
#'   or more columns of data.
#' @param \code{look_back} The size of the lookback window, equal to the number
#'   of data points for calculating the rolling sum.
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   as the input series.
#'
#' @details For example, if look_back=3, then the rolling sum at any point is
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
#' rutils::roll_sum(vectorv, look_back=3)
#' # Rolling sum of matrix
#' matrixv <- matrix(rnorm(1000), nc=5)
#' rutils::roll_sum(matrixv, look_back=3)
#' # Rolling sum of xts time series
#' xtsv <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' rutils::roll_sum(xtsv, look_back=3)

roll_sum <- function(xtsv, look_back) {
  if (xts::is.xts(xtsv)) {
    cumsumv <- cumsum(xtsv)
    roll_sum <- cumsumv - rutils::lagit(cumsumv, lag=look_back)
    roll_sum[1:look_back, ] <- cumsumv[1:look_back, ]
  }
  else {
    if (is.null(dim(xtsv))) {
      cumsumv <- cumsum(xtsv)
      roll_sum <- cumsumv - rutils::lagit(cumsumv, lag=look_back)
      roll_sum[1:look_back] <- cumsumv[1:look_back]
    }
    else {
      cumsumv <- apply(xtsv, MARGIN=2, function(colnum) cumsum(colnum))
      roll_sum <- cumsumv - rutils::lagit(cumsumv, lag=look_back)
      roll_sum[1:look_back, ] <- cumsumv[1:look_back, ]
    }
  }
  roll_sum
}  # end roll_sum



######################################################################
#' Calculate the rolling maximum of an \emph{xts} time series over a sliding
#' window (lookback period).
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series containing one or more columns
#'   of data.
#' @param \code{look_back} The size of the lookback window, equal to the number
#'   of data points for calculating the rolling sum.
#'
#' @return An \emph{xts} time series with the same dimensions as the input
#'   series.
#'
#' @details For example, if look_back=3, then the rolling sum at any point is
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
#' rutils::roll_max(xtsv, look_back=3)

roll_max <- function(xtsv, look_back) {
  roll_max <- RcppRoll::roll_max(c(rep(0,look_back-1), coredata(xtsv)), n=look_back, align="right")
  roll_max <- xts(x=roll_max, order.by=index(xtsv))
  colnames(roll_max) <- colnames(xtsv)
  roll_max
}  # end roll_max



######################################################################
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
    odd_index <- seq(from=1, to=NROW(listv), by=2)
# bind odd and even elements, and divide listv by half
    listv <- lapply(odd_index, function(indeks) {
      if (indeks==NROW(listv)) return(listv[[indeks]])
      rbind(listv[[indeks]], listv[[indeks+1]])
    })  # end lapply
  }  # end while
# listv has only one element - return it
  listv[[1]]
}  # end do_call_rbind



######################################################################
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
    odd_index <- seq(from=1, to=NROW(listv), by=2)
# bind neighboring elements and divide listv by half
    listv <- lapply(odd_index, function(indeks) {
      if (indeks==NROW(listv)) {
        return(listv[[indeks]])
      }
      return(func(listv[[indeks]], listv[[indeks+1]], ...))
    })  # end lapply
  }  # end while
# listv has only one element - return it
  listv[[1]]
}  # end do_call



######################################################################
#' Apply a function to a list of objects, merge the outputs into a single
#' object, and assign the object to the output environment.
#'
#' @export
#' @param \code{func} The name of a function that returns a single object
#'   (\emph{vector}, \emph{xts} time series, etc.)
#' @param \code{symbolv} A vector of \emph{character} strings with the names of
#'   input objects.
#' @param \code{output} The string with name of output object.
#' @param \code{env_in} The environment containing the input \code{symbolv}.
#' @param \code{env_out} The environment for creating the \code{output}.
#' @param \code{...} Additional arguments to function \code{func()}.
#'
#' @return A single object (\emph{matrix}, \emph{xts} time series, etc.)
#'
#' @details The function \code{do_call_assign()} performs an lapply loop over
#'   \code{symbolv}, applies the function \code{func()}, merges the
#'   outputs into a single object, and creates the object in the environment
#'   \code{env_out}.  The output object is created as a side effect, while its
#'   name is returned invisibly.
#'
#' @examples
#' new_env <- new.env()
#' rutils::do_call_assign(
#'    func=get_col,
#'    symbolv=rutils::etfenv$symbolv,
#'    output="prices",
#'    env_in=etfenv, env_out=new_env)

do_call_assign <- function(func, symbolv=NULL, output,
                           env_in=.GlobalEnv, env_out=.GlobalEnv, ...) {
# Produce function name from argument
  func <- match.fun(func)
  if (is.null(symbolv)) symbolv <- ls(env_in)
  assign(output,
         do.call(merge,
                 lapply(mget(env_in$symbolv, envir=env_in), func, ...)),
         envir=env_out)
  invisible(output)
}  # end do_call_assign



######################################################################
## Functions for plotting
######################################################################


######################################################################
#' Calculate the autocorrelation function (ACF) for a time series of returns,
#' and plot it.
#'
#' @export
#' @param \code{xtsv} A vector, matrix, or time series of returns.
#'
#' @param \code{lagg} The maximum lag at which to calculate the ACF (default is
#'   \code{10}).
#'
#' @param \code{plotobj} A \emph{Boolean} argument: should a plot be made?
#'   (default is \code{TRUE})
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
#'   function (ACF) for a time series of returns, and plots it.  The function
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




######################################################################
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
#'   plotting, else plot in standard window (default is \code{TRUE}).
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
  plot_theme <- quantmod::chart_theme()
  if (!is.null(colors)) {
    plot_theme$col$line.col <- colors
  }  # end if
  # Extract chob chart object
  chobj <- quantmod::chart_Series(x=xtsv,
                                  theme=plot_theme,
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



######################################################################
#' Plot two \emph{xts} time series with two y-axes in an x11 window.
#'
#' @export
#' @param \code{xtsv} An \emph{xts} time series with two columns.
#' @param \code{color} A string specifying the color of the second line and
#'   axis (default is \code{"red"}).
#' @param \code{x11} A \emph{Boolean} argument: if \code{TRUE} then open x11
#'   window for plotting, else plot in standard window (default is \code{TRUE}).
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



######################################################################
#' Plot an interactive \emph{dygraphs} candlestick plot with background shading
#' for an \emph{OHLC} time series in \emph{xts} format.
#'
#' @export
#' @param \code{ohlc} An \emph{OHLC} time series in \emph{xts} format.
#' @param \code{indic} A \emph{Boolean} time series in \emph{xts} format for
#'   specifying the shading areas, with TRUE indicating "lightgreen" shading,
#'   and FALSE indicating "antiquewhite" shading (default is \code{NULL}).
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



######################################################################
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
#' prices <- cbind(Ad(rutils::etfenv$VTI), Ad(rutils::etfenv$IEF))
#' colnames(prices) <- get_name(colnames(prices), field=2)
#' rutils::chart_dygraph2y(prices)

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



######################################################################
#' Load \emph{OHLC} time series data into an environment, either from an
#' external source (download from \emph{YAHOO}), or from \emph{CSV} files in a
#' local drive.
#'
#' @export
#' @param \code{symbolv} A vector of strings representing instrument symbols
#'   (tickers).
#' @param \code{data_dir} The directory containing \emph{CSV} files (default is
#'   \code{NULL}).
#' @param \code{data_env} The environment for loading the data into.
#' @param \code{startd} The start date of time series data (default is
#'   "2007-01-01").
#' @param \code{endd} The end date of time series data (default is
#'   \code{Sys.Date()}).
#' @param \code{func} The name of the function for formatting the date
#'   fields in the \emph{CSV} files (default is \code{as.Date()}).
#' @param \code{formatv} The format of the date fields in the \emph{CSV} files
#'   (default is \code{\%Y-\%m-\%d}).
#' @param \code{header} A \emph{Boolean} argument: if \code{TRUE} then read the
#'   header in the \emph{CSV} files (default is \code{TRUE}).
#' @param \code{echo} A \emph{Boolean} argument: if \code{TRUE} then print to
#'   console information on the progress of \emph{CSV} file loading (default is
#'   \code{TRUE}).
#' @param \code{scrub} A \emph{Boolean} argument: if \code{TRUE} then remove
#'   \code{NA} values using function \code{rutils::na_locf()} (default is
#'   \code{TRUE}).
#'
#' @return A vector of \code{symbolv} returned invisibly.
#'
#' @details The function \code{get_data()} loads \emph{OHLC} time series data
#'   into an environment (as a side-effect), and returns invisibly the vector of
#'   \code{symbolv}.
#'
#'   If the argument \code{data_dir} is specified, then \code{get_data()} loads
#'   from \emph{CSV} files in that directory, and overwrites \code{NA} values if
#'   \code{scrub=TRUE}.
#'   If the argument \code{data_dir} is \emph{not} specified, then
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
#' new_env <- new.env()
#' # Load prices from local csv files
#' rutils::get_data(symbolv=c("SPY", "EEM"),
#'             data_dir="C:/Develop/data/bbg_records",
#'             data_env=new_env)
#' # Download prices from YAHOO
#' rutils::get_data(symbolv=c("MSFT", "XOM"),
#'             data_env=new_env,
#'             startd="2012-12-01",
#'             endd="2015-12-01")
#' }

get_data <- function(symbolv,
                     data_dir=NULL, # The directory containing csv files
                     data_env, # The environment for writing xts into
                     startd="2007-01-01",
                     endd=Sys.Date(),
                     func=match.fun("as.Date"),
                     formatv="%Y-%m-%d",
                     header=TRUE,
                     echo=TRUE,
                     scrub=TRUE) {
  if (is.null(data_dir)) {
    # Download prices from YAHOO
    output <- quantmod::getSymbols.yahoo(symbolv,
                                          env=data_env,
                                          from=startd,
                                          to=endd,
                                          adjust=TRUE)
    # Adjust the OHLC prices and save back to data_env
    # output <- lapply(symbolv,
    #                   function(symbol) {
    #                     assign(symbol,
    #                            value=adjust_ohlc(get(symbol, envir=data_env)),
    #                            envir=data_env)
    #                     symbol
    #                   }
    # )  # end lapply
    invisible(output)
  }
  else {
    # Load from csv files
    file_names <- file.path(data_dir, paste0(symbolv, ".csv"))
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
        datav <- rutils::na_locf(datav)
        datav <- rutils::na_locf(datav, from_last=TRUE)
      }  # end if
      assign(rutils::get_name(colnames(datav)[1]),
             datav,
             envir=data_env)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data



######################################################################
#' Download an \emph{OHLC} time series of prices from Polygon.
#'
#' @export
#' @param \code{symbol} The stock symbol (ticker).
#'
#' @param \code{startd} The start date (default is "1997-01-01").
#'
#' @param \code{endd} The end date (default is \code{Sys.Date()}).
#'
#' @param \code{tspan} The data frequency, i.e. the time span for data
#'   aggregations (default is "day" for daily data).
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


