#' Extract symbol names (tickers) from a vector of \emph{character} strings.
#'
#' @export
#' @param str_ing A vector of \emph{character} strings containing symbol names.
#' @param sepa_rator The name separator, i.e. the single \emph{character} that
#'   separates the symbol name from the rest of the string (default is "[.]").
#' @param field The position of the name in the string, i.e. the integer index
#'   of the field to be extracted (default is \code{1}, i.e. the name is at
#'   the beginning of the string,)
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
#'   the string "XLU_2017_09_05.csv" (with \code{sepa_rator="_"}).
#'   
#'   JK: I really don't like sepa_rator
#'
#' @examples
#' # extract symbols "XLU" and"XLP" from file names
#' rutils::get_name(c("XLU.csv", "XLP.csv"))
#' # extract symbols from file names
#' rutils::get_name("XLU_2017_09_05.csv", sep="_")
#' rutils::get_name("XLU 2017 09 05.csv", sep=" ")
#' # extract fields "Open", "High", "Low", "Close" from column names
#' rutils::get_name(colnames(rutils::env_etf$VTI), field=2)

get_name <- function(str_ing, sepa_rator="[.]", field=1) {
  str_ing <- strsplit(str_ing, split=sepa_rator)
  sapply(str_ing, function(x) x[field])
}  # end get_name




#' Calculate a vector of equally spaced end points along the elements of a
#' vector, matrix, or time series.
#'
#' @export
#' @param x_ts A vector, matrix, or time series.
#' @param inter_val The number of elements between neighboring end points. or a
#'   \emph{string} representing a time period (minutes, hours, days, etc.)
#' @param stub_front \emph{Boolean} argument: if \code{TRUE} then add a stub
#'   interval at the beginning, else add a stub interval at the end.  (default
#'   is \code{TRUE})
#'
#' @return An \emph{integer} vector of equally spaced end points (vector of
#'   integers).
#'
#' @details The end points are a vector of integers which divide the elements
#'   (rows) of \code{x_ts} into equally spaced intervals.
#'   If \code{inter_val} is an \emph{integer} then \code{calc_endpoints()}
#'   calculates the number of whole intervals that fit over the elements (rows)
#'   of \code{x_ts}.
#'   If a whole number of intervals doesn't fit over the elements (rows) of
#'   \code{x_ts}, then \code{calc_endpoints()} adds a stub interval either at
#'   the beginning (the default) or at the end.
#'
#'   The function \code{calc_endpoints()} is a generalization of function
#'   \code{endpoints()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, since
#'   \code{inter_val} can accept both \emph{integer} and \emph{string} values.
#'
#'   If \code{inter_val} is a \emph{string} representing a time period (minutes,
#'   hours, days, etc.), then \code{calc_endpoints()} simply calls the function
#'   \code{endpoints()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#' @examples
#' # calculate end points with initial stub interval
#' rutils::calc_endpoints(1:100, inter_val=11)
#' # calculate end points with a stub interval at the end
#' rutils::calc_endpoints(rutils::env_etf$VTI, inter_val=365, stub_front=FALSE)
#' # calculate end points at the end of every hour
#' rutils::calc_endpoints(rutils::env_etf$VTI, inter_val="hours")

calc_endpoints <- function(x_ts, inter_val, stub_front=TRUE) {
  if (is.character(inter_val)) {
    xts::endpoints(x_ts, on=inter_val)
  } else if (is.numeric(inter_val)) {
    # calculate number of inter_vals that fit over x_ts
    n_row <- NROW(x_ts)
    num_agg <- n_row %/% inter_val
    end_points <- (0:num_agg)*inter_val
    if (n_row > inter_val*num_agg) {
      # need to add stub interval
      if (stub_front)
        # stub interval at beginning
        end_points <- c(0, n_row - num_agg*inter_val + end_points)
      else
        # stub interval at end
        end_points <- c(end_points, n_row)
    }  # end if
    end_points
  } else {  # inter_val is neither numeric nor a string
    warning(paste0("argument \"", deparse(substitute(inter_val)), "\" must be either numeric or a string."))
    return(NULL)  # return NULL
  }  # end if

}  # end calc_endpoints




#' Replace \emph{NA} values with the most recent \emph{non-NA} values prior to
#' them.
#'
#' @export
#' @param in_put A \emph{numeric} or \emph{Boolean} vector or matrix, or
#'   \emph{xts} time series.
#' @param from_last \emph{Boolean} argument: should \emph{non-NA} values be
#'   carried backward rather than forward? (default is \code{FALSE})
#' @param na_rm \emph{Boolean} argument: should any remaining (leading or
#'   trailing) \emph{NA} values be removed? (default is \code{FALSE})
#' @param max_gap The maximum number of neighboring \emph{NA} values that can be
#'   replaced (default is \code{NROW(in_put)}).
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   and data type as the argument \code{in_put}.
#'
#' @details The function \code{na_locf()} replaces \emph{NA} values with the
#'   most recent \emph{non-NA} values prior to them.
#'
#'   If the \code{from_last} argument is \code{FALSE} (the default), then the
#'   previous or past \emph{non-NA} values are carried forward to replace the
#'   \emph{NA} values.
#'   If the \code{from_last} argument is \code{TRUE}, then the following or
#'   future \emph{non-NA} values are carried backward to replace the \emph{NA}
#'   values.
#'
#'   The function \code{na_locf()} performs the same operation as function
#'   \code{na.locf()} from package
#'   \href{https://cran.r-project.org/web/packages/zoo/index.html}{zoo}, but
#'   it also accepts vectors as input.
#'
#'   The function \code{na_locf()} calls the compiled function \code{na_locf()}
#'   from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, which
#'   allows it to perform its calculations about three times faster than
#'   \code{na.locf()}.
#'
#' @examples
#' # create vector containing NA values
#' in_put <- sample(22)
#' in_put[sample(NROW(in_put), 4)] <- NA
#' # replace NA values with the most recent non-NA values
#' rutils::na_locf(in_put)
#' # create matrix containing NA values
#' in_put <- sample(44)
#' in_put[sample(NROW(in_put), 8)] <- NA
#' in_put <- matrix(in_put, nc=2)
#' # replace NA values with the most recent non-NA values
#' rutils::na_locf(in_put)
#' # create xts series containing NA values
#' in_put <- xts::xts(in_put, order.by=seq.Date(from=Sys.Date(),
#'   by=1, length.out=NROW(in_put)))
#' # replace NA values with the most recent non-NA values
#' rutils::na_locf(in_put)

na_locf <- function(in_put, from_last=FALSE, na_rm=FALSE, max_gap=NROW(in_put)) {
  if (!(is.numeric(in_put) | is.logical(in_put) | xts::is.xts(in_put))) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric, Boolean, or xts."))
    return(NULL)  # return NULL
  }  # end if
  if (NCOL(in_put) > 1) {
    for (n in 1:NCOL(in_put))
      in_put[, n] <- .Call("na_locf", in_put[, n], from_last, max_gap, Inf, PACKAGE="xts")
  }
  else {
    in_put <- .Call("na_locf", in_put, from_last, max_gap, Inf, PACKAGE="xts")
  }
  if (na_rm) {
    return(structure(na.omit(in_put), na.action=NULL))
  }
  else
    in_put
  ### old version below
  # if (NCOL(in_put) > 1)
  #   return(apply(in_put, MARGIN=2, rutils::na_locf))
  # if (from_last && is.na(in_put[1]))
  #   in_put[1] <- 0
  # if (!from_last && is.na(in_put[NROW(in_put)]))
  #   in_put[NROW(in_put)] <- 0
  # in_dex <- is.na(in_put)
  # in_dices <- rutils::diff_it(in_dex)
  # in_dices <- which(in_dices==1)
  # in_dices <- in_dices - 1
  # diff_indices <- rutils::diff_it(in_dices)
  # diff_indices[1] <- in_dices[1]
  # new_indices <- numeric(NROW(in_put))
  # new_indices[in_dices] <- diff_indices
  # new_indices <- cumsum(new_indices)
  # in_put[in_dex] <- in_put[new_indices[in_dex]]
  # in_put
}  # end na_locf




#' Aggregate an \emph{OHLC} time series to a lower periodicity.
#'
#' Given an \emph{OHLC} time series at high periodicity (say seconds),
#' calculates the \emph{OHLC} prices at lower periodicity (say minutes).
#'
#' @export
#' @param oh_lc an \emph{OHLC} time series of prices in \emph{xts} format.
#' @param period aggregation interval ("seconds", "minutes", "hours", "days",
#'   "weeks", "months", "quarters", and "years").
#' @param k number of periods to aggregate over (for example if period="minutes"
#'   and k=2, then aggregate over two minute intervals.)
#' @param end_points an integer vector of end points.
#'
#' @return A \emph{OHLC} time series of prices in \emph{xts} format, with a
#'   lower periodicity defined by the end_points.
#'
#' @details The function \code{to_period()} performs a similar aggregation as
#'   function \code{to.period()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but has
#'   the flexibility to aggregate to a user-specified vector of end points. The
#'   function \code{to_period()} simply calls the compiled function
#'   \code{toPeriod()} (from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}), to
#'   perform the actual aggregations.  If \code{end_points} are passed in
#'   explicitly, then the \code{period} argument is ignored.
#'
#' @examples
#' \dontrun{
#' # define end points at 10-minute intervals (HighFreq::SPY is minutely bars)
#' end_points <- rutils::calc_endpoints(HighFreq::SPY["2009"], inter_val=10)
#' # aggregate over 10-minute end_points:
#' rutils::to_period(oh_lc=HighFreq::SPY["2009"], end_points=end_points)
#' # aggregate over days:
#' rutils::to_period(oh_lc=HighFreq::SPY["2009"], period="days")
#' # equivalent to:
#' to.period(x=HighFreq::SPY["2009"], period="days", name=rutils::get_name(colnames(HighFreq::SPY)[1])
#' }

to_period <- function(oh_lc,
                      period="minutes", k=1,
                      end_points=xts::endpoints(oh_lc, period, k)) {
  .Call("toPeriod", oh_lc, as.integer(end_points), TRUE, NCOL(oh_lc),
        FALSE, FALSE, colnames(oh_lc), PACKAGE="xts")
}  # end to_period




#' Extract columns of data from \emph{OHLC} time series using column field
#' names.
#'
#' @export
#' @param oh_lc An \emph{OHLC} time series in \emph{xts} format, or a vector of
#'   \emph{character} strings with the names of \emph{OHLC} time series.
#' @param field_name A vector of strings with the field names of the columns to
#'   be be extracted (default is \emph{"Close"}).
#' @param data_env The environment containing \emph{OHLC} time series (default
#'   is \emph{NULL}).
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
#'   In the simplest case when \code{oh_lc} is a single \emph{xts} time series
#'   and \emph{field_name} is a single string, the function \code{get_col()}
#'   performs a similar operation to the extractor functions \code{Op()},
#'   \code{Hi()}, \code{Lo()}, \code{Cl()}, and \code{Vo()}, from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   But \code{get_col()} is able to handle symbols like \emph{LOW}, which the
#'   function \code{Lo()} can't handle. The field_name argument is partially
#'   matched, for example "Vol" is matched to \code{Volume} (but it's case
#'   sensitive).
#'
#'   In the case when \code{oh_lc} is a vector of strings with the names of
#'   \emph{OHLC} time series, the function \code{get_col()} reads the
#'   \emph{OHLC} time series from the environment \code{data_env}, extracts the
#'   specified columns, and binds them into a single \emph{xts} time series.
#'
#' @examples
#' # get close prices for VTI
#' rutils::get_col(rutils::env_etf$VTI)
#' # get volumes for VTI
#' rutils::get_col(rutils::env_etf$VTI, field_name="Vol")
#' # get close prices and volumes for VTI
#' rutils::get_col(rutils::env_etf$VTI, field_name=c("Cl", "Vol"))
#' # get close prices and volumes for VTI and IEF
#' rutils::get_col(oh_lc=c("VTI", "IEF"), field_name=c("Cl", "Vol"),
#'   data_env=rutils::env_etf)

get_col <- function(oh_lc, field_name="Close", data_env=NULL) {
  if (is.xts(oh_lc)) {
    # extract columns, and return them
    col_names <- strsplit(colnames(oh_lc), split="[.]")
    col_names <- sapply(col_names, function(col_name) col_name[2])
    return(oh_lc[, pmatch(field_name, col_names)])
  } else if (is.character(oh_lc)) {
    # loop over the symbols in oh_lc, extract columns, and cbind them
    da_ta <- lapply(oh_lc, function(sym_bol) {
      oh_lc <- get(sym_bol, envir=data_env)
      col_names <- strsplit(colnames(oh_lc), split="[.]")
      col_names <- sapply(col_names, function(col_name) col_name[2])
      oh_lc[, pmatch(field_name, col_names)]
    })  # end lapply
    return(rutils::do_call(cbind, da_ta))
  }  # end if
  ## below is a different version using as.list()
  # da_ta <- lapply(as.list(data_env)[oh_lc], function(oh_lc) {
  #   col_names <- strsplit(colnames(oh_lc), split="[.]")
  #   col_names <- sapply(col_names, function(col_name) col_name[2])
  #   oh_lc[, pmatch(field_name, col_names)]
  # })  # end lapply
  #
  ## below is the old version using grep()
  #   in_dex <- grep(paste0(".", field_name), colnames(oh_lc), ignore.case=TRUE)
  #   if (NROW(in_dex)>0)
  # #    out_put <- xts::.subset_xts(oh_lc, 1:NROW(oh_lc), in_dex:in_dex)
  #     oh_lc[, in_dex]
  #   else
  #     stop(paste0("No column name containing \"", field_name, "\""))
}  # end get_col




#' Adjust the first four columns of \emph{OHLC} data using the "adjusted" price
#' column.
#'
#' @export
#' @param oh_lc an \emph{OHLC} time series of prices in \emph{xts} format.
#'
#' @return An \emph{OHLC} time series with the same dimensions as the input
#'   series.
#'
#' @details Adjusts the first four \emph{OHLC} price columns by multiplying them
#'   by the ratio of the "adjusted" (sixth) price column, divided by the \emph{Close}
#'   (fourth) price column.
#'
#' @examples
#' # adjust VTI prices
#' VTI <- rutils::adjust_ohlc(rutils::env_etf$VTI)

adjust_ohlc <- function(oh_lc) {
  # adjust OHLC prices
  oh_lc[, 1:4] <- as.vector(oh_lc[, 6] / oh_lc[, 4]) * coredata(oh_lc[, 1:4])
  oh_lc
}  # end adjust_ohlc




#' Subset an \emph{xts} time series (extract an \emph{xts} sub-series
#' corresponding to the input dates).
#'
#' @export
#' @param x_ts an \emph{xts} time series.
#' @param start_date the start date of the extracted time series data.
#' @param end_date either the end date of the extracted time series data, or the
#'   number of data rows to be extracted.
#' @param get_rows \emph{Boolean} argument: if \code{TRUE} then extract the
#'   given number of rows of data, else extract the given number of calendar
#'   days (default is \code{TRUE}).
#'
#' @return An \emph{xts} time series with the same number of columns as the
#'   input time series.
#'
#' @details The function \code{sub_set()} extracts an \emph{xts} sub-series
#'   corresponding to the input dates.  If \code{end_date} is a date object or
#'   a character string representing a date, then \code{sub_set()} performs
#'   standard bracket subsetting using the package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}.
#'
#'   The rows of data don't necessarily correspond to consecutive calendar days
#'   because of weekends and holidays.  For example, 10 consecutive rows of data
#'   may correspond to 12 calendar days. So if \code{end_date} is a number, then
#'   we must choose to extract either the given number of rows of data
#'   (\code{get_rows=TRUE}) or the given number of calendar days
#'   (\code{get_rows=FALSE}).
#'
#'   If \code{end_date} is a positive number then \code{sub_set()} returns the
#'   specified number of data rows from the future, and if it's negative then it
#'   returns the data rows from the past.
#'
#'   If \code{end_date} is a number, and either \code{start_date} or
#'   \code{end_date} are outside the date range of \code{x_ts}, then
#'   \code{sub_set()} extracts the maximum available range of \code{x_ts}.
#'
#' @examples
#' # subset an xts time series using two dates
#' rutils::sub_set(rutils::env_etf$VTI, start_date="2015-01-01", end_date="2015-01-10")
#' # extract 6 consecutive rows of data from the past, using a date and a negative number
#' rutils::sub_set(rutils::env_etf$VTI, start_date="2015-01-01", end_date=-6)
#' # extract 6 calendar days of data
#' rutils::sub_set(rutils::env_etf$VTI, start_date="2015-01-01", end_date=6, get_rows=FALSE)
#' # extract up to 100 consecutive rows of data
#' rutils::sub_set(rutils::env_etf$VTI, start_date="2016-08-01", end_date=100)

sub_set <- function(x_ts, start_date, end_date, get_rows=TRUE) {
  if (inherits(end_date, c("Date", "POSIXt", "character"))) {
    x_ts[paste(start_date, end_date, sep = "/")]
  } else if (is.numeric(end_date)) {
    # coerce start_date from string into Date or POSIXt, depending on time index class of x_ts
    if (inherits(start_date, "character")) {
      in_dex <- index(x_ts[1, ])
      if (inherits(in_dex, "Date")) {
        start_date <- as.Date(start_date)
      } else if (inherits(in_dex, "POSIXt")) {
        start_date <- as.POSIXct(start_date)
      }
    }  # end if
    # extract either a number of calendar days or a number of rows of data
    if (get_rows) {
      # find the row number closest to start_date
      start_point <- findInterval(start_date, index(x_ts))
      if (start_date > index(x_ts[start_point])) {
        start_point <- start_point + 1
      }  # end if
      end_point <- (start_point + end_date - sign(end_date))
      end_point <- max(1, min(end_point, NROW(x_ts)))
      x_ts[start_point:end_point]
    } else {
      end_date <- start_date + end_date
      end_date <- max(start(x_ts), min(end_date, end(x_ts)))
      # add numeric end_date to start_date to get the end_date as a date
      if (end_date > start_date) {
        x_ts[paste(start_date, end_date, sep = "/")]
      } else {
        x_ts[paste(end_date, start_date, sep = "/")]
      }  # end if
    }  # end if
  } else {  # end_date is neither a date nor a string
    warning(paste0("argument \"", deparse(substitute(end_date)), "\" must be either a date object or a string representing a date."))
    return(NULL)  # return NULL
  }  # end if

}  # end sub_set




#' Apply a lag to a \emph{numeric} vector or matrix.
#'
#' @export
#' @param in_put a \emph{numeric} vector or matrix.
#' @param lag integer equal to the number of time periods of lag (default is 1).
#'
#' @return A vector or matrix with the same dimensions as the input object.
#'
#' @details Applies a lag to a vector or matrix, by shifting its values by a
#'   certain number of rows, equal to the integer \code{lag}, and pads the
#'   leading or trailing stub periods with \emph{zeros}. Positive \code{lag}
#'   means that values in the current row are replaced with values from the row
#'   that are \code{lag} rows above. (vice versa negative \code{lag}).  This
#'   also applies to vectors, since they can be viewed as single-column
#'   matrices.
#'
#' @examples
#' # lag vector by 2 periods
#' rutils::lag_it(1:10, lag=2)
#' # lag matrix by negative 2 periods
#' rutils::lag_it(matrix(1:10, ncol=2), lag=-2)

lag_it <- function(in_put, lag=1) {
  if (!(is.numeric(in_put) | is.logical(in_put))) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric or Boolean."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(in_put)) {  # in_put is a vector
    if (lag>0) {
      in_put <- c(numeric(lag), in_put)
      in_put[-((NROW(in_put)-lag+1):NROW(in_put))]
    } else {
      in_put <- c(in_put, numeric(-lag))
      in_put[-(1:(-lag))]
    }
  } else if (is.matrix(in_put)) {  # in_put is a matrix
      if (lag>0) {
        in_put <- rbind(matrix(numeric(lag*NCOL(in_put)), nc=NCOL(in_put)), in_put)
        in_put[-((NROW(in_put)-lag+1):NROW(in_put)), ]
      } else {
        in_put <- rbind(in_put, matrix(numeric(-lag*NCOL(in_put)), nc=NCOL(in_put)))
        in_put[-(1:(-lag)), ]
      }
    } else {  # in_put is not a vector or matrix
      warning(paste0("argument \"", deparse(substitute(in_put)), "\" must be a vector or matrix."))
    return(NULL)  # return NULL
  }  # end if
}  # end lag_it




#' Calculate the row differences of a \emph{numeric} vector or matrix.
#'
#' @export
#' @param in_put a \emph{numeric} vector or matrix.
#' @param lag integer equal to the number of time periods of lag (default is 1).
#'
#' @return A vector or matrix with the same dimensions as the input object.
#'
#' @details The function \code{diff_it()} calculates the row differences between
#'   rows that are \code{lag} rows apart. The leading or trailing stub periods
#'   are padded with \emph{zeros}. Positive \code{lag} means that the difference
#'   is calculated as the current row minus the row that is \code{lag} rows
#'   above. (vice versa negative \code{lag}).  This also applies to vectors,
#'   since they can be viewed as single-column matrices.
#'
#' @examples
#' # diff vector by 2 periods
#' rutils::diff_it(1:10, lag=2)
#' # diff matrix by negative 2 periods
#' rutils::diff_it(matrix(1:10, ncol=2), lag=-2)

diff_it <- function(in_put, lag=1) {
  if (!(is.numeric(in_put) | is.logical(in_put))) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric or Boolean."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(in_put)) {  # in_put is a vector
    if (lag>0) {
      lagg_ed <- c(in_put[1:lag], in_put)
      lagg_ed <- lagg_ed[-((NROW(lagg_ed)-lag+1):NROW(lagg_ed))]
    } else {
      lagg_ed <- c(in_put, in_put[(NROW(in_put)+lag+1):NROW(in_put)])
      lagg_ed <- lagg_ed[-(1:(-lag))]
    }
  } else if (is.matrix(in_put)) {  # in_put is a matrix
    if (lag>0) {
      lagg_ed <- rbind(in_put[1:lag, ], in_put)
      lagg_ed <- lagg_ed[-((NROW(lagg_ed)-lag+1):NROW(lagg_ed)), ]
    } else {
      lagg_ed <- rbind(in_put, in_put[(NROW(in_put)+lag+1):NROW(in_put), ])
      lagg_ed <- lagg_ed[-(1:(-lag)), ]
    }
  } else {  # in_put is not a vector or matrix
    warning(paste0("argument \"", deparse(substitute(in_put)), "\" must be a vector or matrix."))
    return(NULL)  # return NULL
  }  # end if
  in_put - lagg_ed
}  # end diff_it




#' Apply a time lag to an \emph{xts} time series.
#'
#' @export
#' @param x_ts an \emph{xts} time series.
#' @param lag integer equal to the number of time periods of lag (default is 1).
#' @param ... additional arguments to function \code{xts::lag_xts()}.
#'
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input \code{x_ts} time series.
#'
#' @details Applies a time lag to an \emph{xts} time series and pads with the
#'   first and last values instead of \emph{NAs}.
#'
#'   A positive lag argument \code{lag} means values from \code{lag} periods in
#'   the past are moved to the present. A negative lag argument \code{lag} moves
#'   values from the future to the present.  The function \code{lag()} is just a
#'   wrapper for function \code{lag_xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with the first and last values instead of \emph{NAs}.
#'
#' @examples
#' # lag by 10 periods
#' rutils::lag_xts(rutils::env_etf$VTI, lag=10)

lag_xts <- function(x_ts, lag=1, ...) {
  n_rows <- NROW(x_ts)
  fir_st <- x_ts[1, ]
  la_st <- x_ts[n_rows, ]
  x_ts <- xts::lag.xts(x_ts, k=lag, ...)
  if (lag>0)
    x_ts[1:lag, ] <- fir_st
  else
    x_ts[(n_rows+lag+1):n_rows, ] <- la_st
  x_ts
}  # end lag_xts




#' Calculate the time differences of an \emph{xts} time series.
#'
#' @export
#' @param x_ts an \emph{xts} time series.
#' @param lag integer equal to the number of time periods of lag (default is 1).
#' @param ... additional arguments to function \code{xts::diff.xts()}.
#'
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input series.
#'
#' @details The function \code{diff_xts()} calculates the time differences of an
#'   \emph{xts} time series and pads with \emph{zeros} instead of \emph{NAs}.
#'   Positive \code{lag} means differences are calculated with values from
#'   \code{lag} periods in the past (vice versa negative \code{lag}).  The
#'   function \code{diff()} is just a wrapper for \code{diff.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with \emph{zeros} instead of \emph{NAs}.
#'
#' @examples
#' # calculate time differences over lag by 10 periods
#' rutils::diff_xts(rutils::env_etf$VTI, lag=10)

diff_xts <- function(x_ts, lag=1, ...) {
  x_ts <- xts::diff.xts(x_ts, lag=lag, ...)
  x_ts[!complete.cases(x_ts), ] <- 0
  x_ts
}  # end diff_xts




#' Calculate the reduced form of an \emph{OHLC} time series, or calculate the
#' standard form from the reduced form of an \emph{OHLC} time series.
#'
#' @export
#' @param oh_lc an \emph{OHLC} time series of prices in \emph{xts} format.
#' @param re_duce \emph{Boolean} argument: should the reduced form be calculated
#'   or the standard form? (default is \code{TRUE})
#' @param ... additional arguments to function \code{xts::diff.xts()}.
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
#' # calculate reduced form of an OHLC time series
#' diff_VTI <- rutils::diff_ohlc(rutils::env_etf$VTI)
#' # calculate standard form of an OHLC time series
#' VTI <- rutils::diff_ohlc(diff_VTI, re_duce=FALSE)
#' identical(VTI, rutils::env_etf$VTI[, 1:5])

diff_ohlc <- function(oh_lc, re_duce=TRUE, ...) {
  if (re_duce) {
    # calculate differencces of Close prices
    cl_ose <- xts::diff.xts(oh_lc[, 4], lag=1, ...)
    # find index of overnight price jumps
    in_dex <- c(60, diff(.index(oh_lc))) > 60
    # set overnight price jumps to zero
    cl_ose[in_dex] <- 0
    # remember first Close price
    cl_ose[1] <- oh_lc[1, 4]
    # calculate differences of OHLC prices with respect to Close prices
    op_en <- oh_lc[, 1] - oh_lc[, 4]
    hi_gh <- oh_lc[, 2] - oh_lc[, 4]
    lo_w <- oh_lc[, 3] - oh_lc[, 4]
    cbind(op_en, hi_gh, lo_w, cl_ose, oh_lc[, 5])
  }
  else {
    cl_ose <- cumsum(oh_lc[, 4])
    op_en <- oh_lc[, 1] + cl_ose
    hi_gh <- oh_lc[, 2] + cl_ose
    lo_w <- oh_lc[, 3] + cl_ose
    cbind(op_en, hi_gh, lo_w, cl_ose, oh_lc[, 5])
  }  # end if
}  # end diff_ohlc




#' Calculate the rolling sum of a \emph{numeric} vector, matrix, or \emph{xts}
#' time series over a sliding window (lookback period).
#'
#' @export
#' @param x_ts a vector, matrix, or \emph{xts} time series containing one or
#'   more columns of data.
#' @param look_back the size of the lookback window, equal to the number of data
#'   points for calculating the rolling sum.
#'
#' @return A vector, matrix, or \emph{xts} time series with the same dimensions
#'   as the input series.
#'
#' @details For example, if look_back=3, then the rolling sum at any point is
#'   equal to the sum of \code{x_ts} values for that point plus two preceding
#'   points. The initial values of roll_sum() are equal to cumsum() values, so
#'   that roll_sum() doesn't return any NA values.
#'
#'   The function \code{roll_sum()} performs the same operation as function
#'   \code{runSum()} from package
#'   \href{https://cran.r-project.org/web/packages/TTR/index.html}{TTR}, but
#'   using vectorized functions, so it's a little faster.
#'
#' @examples
#' # rolling sum of vector
#' vec_tor <- rnorm(1000)
#' rutils::roll_sum(vec_tor, look_back=3)
#' # rolling sum of matrix
#' mat_rix <- matrix(rnorm(1000), nc=5)
#' rutils::roll_sum(mat_rix, look_back=3)
#' # rolling sum of xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' rutils::roll_sum(x_ts, look_back=3)

roll_sum <- function(x_ts, look_back) {
  if (xts::is.xts(x_ts)) {
    cum_sum <- cumsum(x_ts)
    roll_sum <- cum_sum - rutils::lag_xts(cum_sum, lag=look_back)
    roll_sum[1:look_back] <- cum_sum[1:look_back]
  }
  else {
    if (is.null(dim(x_ts))) {
      cum_sum <- cumsum(x_ts)
      roll_sum <- cum_sum - rutils::lag_it(cum_sum, lag=look_back)
      roll_sum[1:look_back] <- cum_sum[1:look_back]
    }
    else {
      cum_sum <- apply(x_ts, MARGIN=2, function(col_umn) cumsum(col_umn))
      roll_sum <- cum_sum - rutils::lag_it(cum_sum, lag=look_back)
      roll_sum[1:look_back, ] <- cum_sum[1:look_back, ]
    }
  }
  roll_sum
}  # end roll_sum




#' Calculate the rolling maximum of an \emph{xts} time series over a sliding
#' window (lookback period).
#'
#' @export
#' @param x_ts an \emph{xts} time series containing one or more columns of data.
#' @param look_back the size of the lookback window, equal to the number of data
#'   points for calculating the rolling sum.
#'
#' @return An \emph{xts} time series with the same dimensions as the input
#'   series.
#'
#' @details For example, if look_back=3, then the rolling sum at any point is
#'   equal to the sum of \code{x_ts} values for that point plus two preceding
#'   points.
#'
#'   The initial values of roll_max() are equal to cumsum() values, so that
#'   roll_max() doesn't return any NA values.
#'
#'   The function \code{roll_max()} performs the same operation as function
#'   \code{runMax()} from package
#'   \href{https://cran.r-project.org/web/packages/TTR/index.html}{TTR}, but
#'   using vectorized functions, so it's a little faster.
#'
#' @examples
#' # create xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' rutils::roll_max(x_ts, look_back=3)

roll_max <- function(x_ts, look_back) {
  roll_max <- RcppRoll::roll_max(c(rep(0,look_back-1), coredata(x_ts)), n=look_back, align="right")
  roll_max <- xts(x=roll_max, order.by=index(x_ts))
  colnames(roll_max) <- colnames(x_ts)
  roll_max
}  # end roll_max




#' Recursively \sQuote{\code{rbind}} a list of objects, such as \emph{xts} time
#' series.
#'
#' @export
#' @param li_st list of objects, such as \code{vectors}, \code{matrices},
#'   \code{data frames}, or \code{time series}.
#'
#' @return A single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#'
#' @details Performs lapply loop, each time binding neighboring elements and
#'   dividing the length of \code{li_st} by half. The result of performing
#'   \code{do_call_rbind(list_xts)} on a list of \emph{xts} time series is
#'   identical to performing \code{do.call(rbind, list_xts)}. But
#'   \code{do.call(rbind, list_xts)} is very slow, and often causes an
#'   \sQuote{out of memory} error.
#'
#'   The function \code{do_call_rbind()} performs the same operation as
#'   \code{do.call(rbind, li_st)}, but using recursion, which is much faster and
#'   uses less memory. This is the same function as
#'   \sQuote{\code{\link[qmao]{do.call.rbind}}} from package
#'   \sQuote{\href{https://r-forge.r-project.org/R/?group_id=1113}{qmao}}.
#'
#' @examples
#' # create xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' # split time series into daily list
#' list_xts <- split(x_ts, "days")
#' # rbind the list back into a time series and compare with the original
#' identical(x_ts, rutils::do_call_rbind(list_xts))

do_call_rbind <- function(li_st) {
  while (NROW(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=NROW(li_st), by=2)
# bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==NROW(li_st)) return(li_st[[in_dex]])
      rbind(li_st[[in_dex]], li_st[[in_dex+1]])
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call_rbind




#' Recursively apply a function to a list of objects, such as \emph{xts} time
#' series.
#'
#' Performs a similar operation as \code{do.call()}, but using recursion, which
#' is much faster and uses less memory. The function \code{do_call()} is a
#' generalization of function \code{do_call_rbind()}.
#'
#' @export
#' @param func_tion name of function that returns a single object from a list of
#'   objects.
#' @param li_st list of objects, such as \code{vectors}, \code{matrices},
#'   \code{data frames}, or \code{time series}.
#' @param ... additional arguments to function \code{func_tion()}.
#'
#' @return A single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#'
#' @details The function \code{do_call()} performs an lapply loop, each time
#'   binding neighboring elements and dividing the length of \code{li_st} by
#'   half. The result of performing \code{do_call(rbind, list_xts)} on a list of
#'   \emph{xts} time series is identical to performing \code{do.call(rbind,
#'   list_xts)}. But \code{do.call(rbind, list_xts)} is very slow, and often
#'   causes an \sQuote{out of memory} error.
#'
#' @examples
#' # create xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' # split time series into daily list
#' list_xts <- split(x_ts, "days")
#' # rbind the list back into a time series and compare with the original
#' identical(x_ts, rutils::do_call(rbind, list_xts))

do_call <- function(func_tion, li_st, ...) {
# produce function name from argument
  func_tion <- match.fun(func_tion)
  while (NROW(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=NROW(li_st), by=2)
# bind neighboring elements and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==NROW(li_st)) {
        return(li_st[[in_dex]])
      }
      return(func_tion(li_st[[in_dex]], li_st[[in_dex+1]], ...))
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call




#' Apply a function to a list of objects, merge the outputs into a single
#' object, and assign the object to the output environment.
#'
#' @export
#' @param func_tion name of function that returns a single object
#'   (\code{vector}, \emph{xts} time series, etc.)
#' @param sym_bols a vector of \emph{character} strings with the names of input
#'   objects.
#' @param out_put the string with name of output object.
#' @param env_in the environment containing the input \code{sym_bols}.
#' @param env_out the environment for creating the \code{out_put}.
#' @param ... additional arguments to function \code{func_tion()}.
#'
#' @return A single object (\code{matrix}, \emph{xts} time series, etc.)
#'
#' @details The function \code{do_call_assign()} performs an lapply loop over
#'   \code{sym_bols}, applies the function \code{func_tion()}, merges the
#'   outputs into a single object, and creates the object in the environment
#'   \code{env_out}.  The output object is created as a side effect, while its
#'   name is returned invisibly.
#'
#' @examples
#' new_env <- new.env()
#' rutils::do_call_assign(
#'    func_tion=get_col,
#'    sym_bols=rutils::env_etf$sym_bols,
#'    out_put="price_s",
#'    env_in=env_etf, env_out=new_env)

do_call_assign <- function(func_tion, sym_bols=NULL, out_put,
                           env_in=.GlobalEnv, env_out=.GlobalEnv, ...) {
# produce function name from argument
  func_tion <- match.fun(func_tion)
  if (is.null(sym_bols)) sym_bols <- ls(env_in)
  assign(out_put,
         do.call(merge,
                 lapply(mget(env_in$sym_bols, envir=env_in), func_tion, ...)),
         envir=env_out)
  invisible(out_put)
}  # end do_call_assign




#' Plot either a line plot or a candlestick plot of an \emph{xts} time series,
#' with custom line colors, y-axis range, and with vertical background shading.
#'
#' A wrapper for function \code{chart_Series()} from package
#' \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'
#' @export
#' @param x_ts An \emph{xts} time series or an \emph{OHLC} time series.
#' @param col_ors A vector of \emph{strings} with the custom line colors.
#' @param ylim A \emph{numeric} vector with two elements containing the y-axis
#'   range.
#' @param in_dic A \emph{Boolean} vector or \emph{xts} time series for
#'   specifying the shading areas, with TRUE indicating "lightgreen" shading,
#'   and FALSE indicating "antiquewhite" shading.
#' @param x_11 \emph{Boolean} argument: if \code{TRUE} then open x11 window for
#'   plotting, else plot in standard window (default is \code{TRUE}).
#' @param ... additional arguments to function \code{chart_Series()}.
#'
#' @return A \code{chart_Series()} object returned invisibly.
#'
#' @details The function \code{chart_xts()} plots a line plot of a \emph{xts}
#'   time series, or a candlestick plot if \emph{x_ts} is a \emph{OHLC} time
#'   series. The function \code{chart_xts()} plots with custom line colors and
#'   vertical background shading, using the function \code{chart_Series()} from
#'   package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   By default \code{chart_xts()} opens and plots in an x11 window.
#'
#'   The function \code{chart_xts()} extracts the \code{chart_Series()} chart
#'   object and modifies its \emph{ylim} parameter using accessor and setter
#'   functions. It also adds background shading specified by the \code{in_dic}
#'   argument, using function \code{add_TA()}. The \code{in_dic} argument should
#'   have the same length as the \code{x_ts} time series. Finally the function
#'   \code{chart_xts()} plots the chart object and returns it invisibly.
#'
#' @examples
#' # plot candlestick chart with shading
#' rutils::chart_xts(rutils::env_etf$VTI["2015-11"],
#'   name="VTI in Nov 2015", ylim=c(102, 108),
#'   in_dic=zoo::index(rutils::env_etf$VTI["2015-11"]) > as.Date("2015-11-18"))
#' # plot two time series with custom line colors
#' rutils::chart_xts(na.omit(cbind(rutils::env_etf$XLU[, 4],
#'   rutils::env_etf$XLP[, 4])), col_ors=c("blue", "green"))

chart_xts <- function(x_ts, col_ors=NULL, ylim=NULL, in_dic=NULL, x_11=TRUE, ...) {
  stopifnot(xts::is.xts(x_ts))
  if (x_11) {
    x11(10, 7)  # open x11 plot window
  }  # end if
  # set plot margins
  par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
  # pass color parameters into chart_Series() using chart_theme()
  plot_theme <- quantmod::chart_theme()
  if (!is.null(col_ors)) {
    plot_theme$col$line.col <- col_ors
  }  # end if
  # extract chob chart object
  ch_ob <- quantmod::chart_Series(x=x_ts,
                                  theme=plot_theme,
                                  plot=FALSE, ...)
  # modify ylim using accessor and setter functions
  if (!is.null(ylim)) {
    y_lim <- ch_ob$get_ylim()
    y_lim[[2]] <- structure(ylim, fixed=TRUE)
    ch_ob$set_ylim(y_lim)
  }  # end if
# add vertical background shading
  if (!is.null(in_dic)) {
    if (!xts::is.xts(in_dic))
      in_dic <- xts::xts(in_dic, order.by=zoo::index(x_ts))
    quantmod::add_TA(in_dic, on=-1, col="lightgreen", border=NA)
    quantmod::add_TA(!in_dic, on=-1, col="antiquewhite", border=NA)
  }  # end if
# render the plot and return the chob invisibly
  plot(ch_ob)
  invisible(ch_ob)
}  # end chart_xts




#' Plot two \emph{xts} time series with two y-axes in an x11 window.
#'
#' @export
#' @param An x_ts \emph{xts} time series with two columns.
#' @param col_or A string specifying the color of the second line and axis
#'   (default is \code{"red"}).
#' @param x_11 \emph{Boolean} argument: if \code{TRUE} then open x11 window for
#'   plotting, else plot in standard window (default is \code{TRUE}).
#' @param ... additional arguments to function \code{plot.zoo()}.
#'
#' @return returns the \emph{x_ts} column names invisibly, and produces a plot
#'   in an x11 window as a side effect.
#'
#' @details The function \code{chart_xts2y()} creates a plot of two \emph{xts}
#'   time series with two y-axes.
#'   By default \code{chart_xts2y()} opens and plots in an x11 window.
#'   The function \code{chart_xts2y()} uses the standard plotting functions from
#'   base \emph{R}, and the function \code{plot.zoo()} from package
#'   \href{https://cran.r-project.org/web/packages/zoo/index.html}{zoo}.
#'
#' @examples
#' # plot two time series
#' rutils::chart_xts2y(cbind(quantmod::Cl(rutils::env_etf$VTI),
#'                           quantmod::Cl(rutils::env_etf$IEF))["2015"])

chart_xts2y <- function(x_ts, col_or="red", x_11=TRUE, ...) {
  stopifnot(xts::is.xts(x_ts))
  if (x_11) {
    x11(10, 7)  # open x11 plot window
  }  # end if
  # set plot margins
  par(mar=c(3, 3, 3, 3), oma=c(0, 0, 0, 0))
  par(las=1)  # set text printing to horizontal
  ## plot with two y-axes - plot first time series
  zoo::plot.zoo(x_ts[, 1], lwd=2, xlab=NA, ylab=NA, ...)
  par(new=TRUE)  # allow new plot on same chart
  # plot second time series without y-axis
  zoo::plot.zoo(x_ts[, 2], xlab=NA, ylab=NA,
       lwd=2, yaxt="n", col=col_or, ...)
  # plot second y-axis on right
  axis(side=4, col=col_or)
  # add axis labels
  col_names <- colnames(x_ts)
  mtext(col_names[1], side=2, adj=-0.5, padj=-15)
  mtext(col_names[2], side=4, adj=1.5, padj=-15, col=col_or)
  # add title and legend
  title(main=paste0(col_names, collapse=" and "),
        line=0.5)
  legend("top", legend=col_names,
         bg="white", lty=c(1, 1), lwd=c(6, 6),
         col=c("black", col_or), bty="n")
  invisible(col_names)
}  # end chart_xts2y




#' Plot an interactive \emph{dygraphs} candlestick plot with background shading
#' for an \emph{OHLC} time series in \emph{xts} format.
#'
#' @export
#' @param oh_lc An \emph{OHLC} time series in \emph{xts} format.
#' @param in_dic A \emph{Boolean} time series in \emph{xts} format for
#'   specifying the shading areas, with TRUE indicating "lightgreen" shading,
#'   and FALSE indicating "antiquewhite" shading (default is \code{NULL}).
#' @param ... additional arguments to function \code{dygraphs::dygraph()}.
#'
#' @return A \code{dygraphs} plot object.
#'
#' @details The function \code{chart_dygraph()} creates an interactive dygraphs
#'   candlestick plot with background shading for an \emph{OHLC} time series.
#'   The function \code{chart_dygraph()} uses plotting functions from the
#'   package
#'   \href{https://cran.r-project.org/web/packages/dygraphs/index.html}{dygraphs}.
#'
#' @examples
#' # plot an interactive dygraphs candlestick plot with background shading
#' oh_lc <- rutils::env_etf$VTI
#' v_wap <- TTR::VWAP(price=quantmod::Ad(oh_lc), volume=quantmod::Vo(oh_lc), n=20)
#' oh_lc <- cbind(oh_lc[, c(1:3, 6)], v_wap)["2009-02/2009-04"]
#' rutils::chart_dygraph(oh_lc, in_dic=(oh_lc[, 4] > v_wap))

chart_dygraph <- function(oh_lc, in_dic=NULL, ...) {
  stopifnot(inherits(oh_lc, "xts"))
  if (!require("dygraphs")) {
    warning("You must install package dygraphs first")
    return(NULL)
  }  # end if
  # create dygraphs object
  dy_graph <- dygraphs::dygraph(oh_lc, ...) %>% dygraphs::dyCandlestick()
  if (!is.null(in_dic)) {
    # add shading to dygraph object
    in_dic <- rutils::diff_xts(in_dic)
    in_dic <- rbind(cbind(which(in_dic==1), 1),
                    cbind(which(in_dic==(-1)), -1))
    in_dic <- in_dic[order(in_dic[, 1]), ]
    in_dic <- rbind(c(1, -in_dic[1, 2]), in_dic,
                    c(NROW(oh_lc), -in_dic[NROW(in_dic), 2]))
    in_dic <- data.frame(index(oh_lc)[in_dic[, 1]], in_dic[, 2])
    # add shading to dygraph object
    for (i in 1:(NROW(in_dic)-1)) {
      if (in_dic[i, 2] == 1)
        dy_graph <- dy_graph %>% dygraphs::dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="lightgreen")
      else
        dy_graph <- dy_graph %>% dygraphs::dyShading(from=in_dic[i, 1], to=in_dic[i+1, 1], color="antiquewhite")
    }  # end for
  }  # end if
  # render dygraph plot
  dy_graph
}  # end chart_dygraph




#' Plot an interactive \emph{dygraphs} line plot for two \emph{xts} time series,
#' with two \emph{"y"} axes.
#'
#' @export
#' @param x_ts An \emph{xts} time series with two columns.
#' @param ... additional arguments to function \code{dygraphs::dygraph()}.
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
#' # plot an interactive dygraphs line plot with two "y" axes
#' price_s <- cbind(Ad(rutils::env_etf$VTI), Ad(rutils::env_etf$IEF))
#' colnames(price_s) <- get_name(colnames(price_s), field=2)
#' rutils::chart_dygraph2y(price_s)

chart_dygraph2y <- function(x_ts, ...) {
  stopifnot(xts::is.xts(x_ts))
  if (!require("dygraphs")) {
    warning("You must install package dygraphs first")
    return(NULL)
  }  # end if
  col_names <- colnames(x_ts)
  # create dygraphs object
  dy_graph <- dygraphs::dygraph(x_ts, main=paste(col_names, collapse=" and "), ...) %>%
    dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
    dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
    dySeries(col_names[2], axis="y2", col=c("red", "blue"))
  # render dygraph plot
  dy_graph
}  # end chart_dygraph2y




#' Load \emph{OHLC} time series data into an environment, either from an
#' external source (download from \emph{YAHOO}), or from \emph{CSV} files in a
#' local drive.
#'
#' @export
#' @param sym_bols A vector of strings representing instrument symbols
#'   (tickers).
#' @param data_dir The directory containing \emph{CSV} files (default is
#'   \code{NULL}).
#' @param data_env The environment for loading the data into.
#' @param start_date The start date of time series data (default is
#'   "2007-01-01").
#' @param end_date The end date of time series data (default is
#'   \code{Sys.Date()}).
#' @param date_fun The name of the function for formatting the date fields in
#'   the \emph{CSV} files (default is \code{as.Date()}).
#' @param for_mat The format of the date fields in the \emph{CSV} files
#'   (default is \code{\%Y-\%m-\%d}).
#' @param header \emph{Boolean} argument: if \code{TRUE} then read the header in
#'   the \emph{CSV} files (default is \code{TRUE}).
#' @param e_cho \emph{Boolean} argument: if \code{TRUE} then print to console
#'   information on the progress of \emph{CSV} file loading (default is
#'   \code{TRUE}).
#' @param scrub \emph{Boolean} argument: if \code{TRUE} then remove \code{NA}
#'   values using function \code{rutils::na_locf()} (default is \code{TRUE}).
#'
#' @return A vector of \code{sym_bols} returned invisibly.
#'
#' @details The function \code{get_data()} loads \emph{OHLC} time series data
#'   into an environment (as a side-effect), and returns invisibly the vector of
#'   \code{sym_bols}.
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
#'   handle them. The \code{start_date} and \code{end_date} must be either of
#'   class \emph{Date}, or a string in the format \emph{"YYYY-mm-dd"}.
#'
#' @examples
#' \dontrun{
#' new_env <- new.env()
#' # load prices from local csv files
#' rutils::get_data(sym_bols=c("SPY", "EEM"),
#'             data_dir="C:/Develop/data_bbg_records",
#'             data_env=new_env)
#' # download prices from YAHOO
#' rutils::get_data(sym_bols=c("MSFT", "XOM"),
#'             data_env=new_env,
#'             start_date="2012-12-01",
#'             end_date="2015-12-01")
#' }

get_data <- function(sym_bols,
                     data_dir=NULL, # the directory containing csv files
                     data_env, # the environment for writing xts into
                     start_date="2007-01-01",
                     end_date=Sys.Date(),
                     date_fun=match.fun("as.Date"),
                     for_mat="%Y-%m-%d",
                     header=TRUE,
                     e_cho=TRUE,
                     scrub=TRUE) {
  if (is.null(data_dir)) {
    # download prices from YAHOO
    out_put <- quantmod::getSymbols.yahoo(sym_bols,
                                          env=data_env,
                                          from=start_date,
                                          to=end_date,
                                          adjust=TRUE)
    # adjust the OHLC prices and save back to data_env
    # out_put <- lapply(sym_bols,
    #                   function(sym_bol) {
    #                     assign(sym_bol,
    #                            value=adjust_ohlc(get(sym_bol, envir=data_env)),
    #                            envir=data_env)
    #                     sym_bol
    #                   }
    # )  # end lapply
    invisible(out_put)
  }
  else {
    # load from csv files
    file_names <- file.path(data_dir, paste0(sym_bols, ".csv"))
    invisible(sapply(file_names, function(file_name) {
      if (e_cho)
        cat("Loading instrument: \t", file_name, "\n")
      da_ta <- xts::as.xts(zoo::read.zoo(file=file_name,
                                         header=header, sep=",",
                                         drop=FALSE,
                                         FUN=date_fun,
                                         format=for_mat))
      if (scrub) {
        # overwrite NA values
        da_ta <- rutils::na_locf(da_ta)
        da_ta <- rutils::na_locf(da_ta, from_last=TRUE)
      }  # end if
      assign(rutils::get_name(colnames(da_ta)[1]),
             da_ta,
             envir=data_env)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data


