#' Extract the name of an \emph{OHLC} time series from its first column name.
#'
#' @export
#' @param x_ts \emph{OHLC} time series.
#' @param field the integer index of the field to be extracted.
#' @return A \emph{string} with the name of time series.
#' @details Extracts the symbol name (ticker) from the name of the first column
#'   of an \emph{OHLC} time series.  The column name is assumed to be in the
#'   format "\emph{symbol}.Open". It can also extract the field name after the
#'   "." separator, for example "Open" from "SPY.Open".
#' @examples
#' # get name for VTI
#' na_me(env_etf$VTI)

na_me <- function(x_ts, field=1) strsplit(colnames(x_ts), split="[.]")[[1]][field]




#' Calculate an index (integer vector) of equally spaced end points for a time
#' series.
#'
#' @export
#' @param x_ts vector or time series.
#' @param inter_val the number of data points per interval.
#' @param off_set the number of data points in the first interval (stub
#'   interval).
#' @return An \emph{integer} vector of equally spaced end points.
#' @details The end points divide the time series into equally spaced intervals.
#'   The off_set argument shifts the end points forward and creates an initial
#'   stub interval.
#' @examples
#' # calculate end points with initial stub interval
#' end_points(env_etf$VTI, inter_val=7, off_set=4)

end_points <- function(x_ts, inter_val=10, off_set=0) {
  if (off_set >= inter_val)
    stop("off_set must be less than inter_val")
# calculate number of inter_vals that fit over x_ts
  n_row <- NROW(x_ts)
  num_agg <- n_row %/% inter_val
  end_points <- off_set + inter_val*(0:(num_agg+1))
  if (off_set > 0)
    end_points <- c(0, end_points)
  if (xts::last(end_points) > n_row)
    end_points <- end_points[-length(end_points)]
  if (xts::last(end_points) > n_row)
    end_points <- end_points[-length(end_points)]
  if (xts::last(end_points) < n_row)
    end_points <- c(end_points, n_row)
  as.integer(end_points)
}  # end end_points




#' Replace \emph{NA} values with the most recent \emph{non-NA} values prior to
#' them.
#'
#' @export
#' @param in_put \emph{numeric} or \emph{Boolean} vector or matrix, or \emph{xts}
#'   time series.
#' @param from_last \emph{Boolean} argument: should \emph{non-NA} values be
#'   carried backward rather than forward? (default is \code{FALSE})
#' @param na_rm \emph{Boolean} argument: should an remaining (leading or
#'   trailing) \emph{NA} values be removed? (default is \code{FALSE})
#' @param max_gap \emph{integer} the maximum number of neighboring \emph{NA}
#'   values that can be replaced.
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
#' @return A \emph{OHLC} time series of prices in \emph{xts} format, with a
#'   lower periodicity defined by the end_points.
#' @details The function \code{to_period()} performs a similar aggregation as
#'   function \code{to.period()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but has
#'   the flexibility to aggregate to a user-specified vector of end points. The
#'   function \code{to_period()} simply calls the compiled function
#'   \code{toPeriod()} (from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}), to
#'   perform the actual aggregations.  If \code{end_points} are passed in
#'   explicitly, then the \code{period} argument is ignored.
#' @examples
#' # define end points at 10-minute intervals (SPY is minutely bars)
#' end_points <- rutils::end_points(SPY["2009"], inter_val=10)
#' # aggregate over 10-minute end_points:
#' to_period(oh_lc=SPY["2009"], end_points=end_points)
#' # aggregate over days:
#' to_period(oh_lc=SPY["2009"], period="days")
#' # equivalent to:
#' to.period(x=SPY["2009"], period="days", name=rutils::na_me(SPY))

to_period <- function(oh_lc,
                      period="minutes", k=1,
                      end_points=xts::endpoints(oh_lc, period, k)) {
  .Call("toPeriod", oh_lc, as.integer(end_points), TRUE, NCOL(oh_lc),
        FALSE, FALSE, colnames(oh_lc), PACKAGE="xts")
}  # end to_period




#' Extract columns of prices from an \emph{OHLC} time series.
#'
#' @export
#' @param oh_lc an \emph{OHLC} time series.
#' @param col_name string with the field name of the column to be be extracted.
#'   (default is \emph{Close})
#' @return A single column \emph{OHLC} time series in \emph{xts} format.
#' @details Extracts columns of prices from an \emph{OHLC} time series by
#'   \emph{grepping} column names for the \code{col_name} string. The
#'   \emph{OHLC} column names are assumed to be in the format
#'   \code{"symbol.field_name"}, for example \code{"VTI.Close"}. Performs a
#'   similar operation to the extractor functions \code{Op()}, \code{Hi()},
#'   \code{Lo()}, \code{Cl()}, and \code{Vo()}, from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'    But \code{ex_tract()} is able to handle symbols like \emph{LOW}, which
#'   the function \code{Lo()} can't handle. The col_name argument is partially
#'   matched, for example "vol" is matched to \emph{Volume}.
#' @examples
#' # get close prices for VTI
#' ex_tract(env_etf$VTI)
#' # get volumes for VTI
#' ex_tract(env_etf$VTI, col_name="vol")

ex_tract <- function(oh_lc, col_name="Close") {
  in_dex <- grep(paste0(".", col_name), colnames(oh_lc), ignore.case=TRUE)
  if (length(in_dex)>0)
#    out_put <- xts::.subset_xts(oh_lc, 1:NROW(oh_lc), in_dex:in_dex)
    oh_lc[, in_dex]
  else
    stop(paste0("No column name containing \"", col_name, "\""))
}  # end ex_tract




#' Adjust the first four columns of \emph{OHLC} data using the "adjusted" price
#' column.
#'
#' @export
#' @param oh_lc an \emph{OHLC} time series of prices in \emph{xts} format.
#' @return An \emph{OHLC} time series with the same dimensions as the input
#'   series.
#' @details Adjusts the first four \emph{OHLC} price columns by multiplying them
#'   by the ratio of the "adjusted" (sixth) price column, divided by the \emph{Close}
#'   (fourth) price column.
#' @examples
#' # adjust VTI prices
#' VTI <- adjust_ohlc(env_etf$VTI)

adjust_ohlc <- function(oh_lc) {
  # adjust OHLC prices
  oh_lc[, 1:4] <- as.vector(oh_lc[, 6] / oh_lc[, 4]) * coredata(oh_lc[, 1:4])
  oh_lc
}  # end adjust_ohlc




#' Apply a lag to a \emph{numeric} vector or matrix.
#'
#' @export
#' @param in_put a \emph{numeric} vector or matrix.
#' @param lag integer equal to the number of time periods of lag. (default is 1)
#' @return A vector or matrix with the same dimensions as the input object.
#' @details Applies a lag to a vector or matrix, by shifting its values by a
#'   certain number of rows, equal to the integer \code{lag}, and pads the
#'   leading or trailing stub periods with \emph{zeros}. Positive \code{lag}
#'   means that values in the current row are replaced with values from the row
#'   that are \code{lag} rows above. (vice versa negative \code{lag}).  This
#'   also applies to vectors, since they can be viewed as single-column
#'   matrices.
#' @examples
#' # lag vector by 2 periods
#' lag_it(1:10, lag=2)
#' # lag matrix by negative 2 periods
#' lag_it(matrix(1:10, ncol=2), lag=-2)

lag_it <- function(in_put, lag=1) {
  if (!(is.numeric(in_put) | is.logical(in_put))) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric or Boolean."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(in_put)) {  # in_put is a vector
    if(lag>0) {
      in_put <- c(numeric(lag), in_put)
      in_put[-((length(in_put)-lag+1):length(in_put))]
    } else {
      in_put <- c(in_put, numeric(-lag))
      in_put[-(1:(-lag))]
    }
  } else if (is.matrix(in_put)) {  # in_put is a matrix
      if(lag>0) {
        in_put <- rbind(matrix(numeric(lag*NCOL(in_put)), NCOL(in_put)), in_put)
        in_put[-((NROW(in_put)-lag+1):NROW(in_put)), ]
      } else {
        in_put <- rbind(in_put, matrix(numeric(-lag*NCOL(in_put)), NCOL(in_put)))
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
#' @param lag integer equal to the number of time periods of lag. (default is 1)
#' @return A vector or matrix with the same dimensions as the input object.
#' @details Calculates the row differences between rows that are \code{lag} rows
#'   apart. The leading or trailing stub periods are padded with \emph{zeros}.
#'   Positive \code{lag} means that the difference is calculated as the current
#'   row minus the row that is \code{lag} rows above. (vice versa negative
#'   \code{lag}).  This also applies to vectors, since they can be viewed as
#'   single-column matrices.
#' @examples
#' # diff vector by 2 periods
#' diff_it(1:10, lag=2)
#' # diff matrix by negative 2 periods
#' diff_it(matrix(1:10, ncol=2), lag=-2)

diff_it <- function(in_put, lag=1) {
  if (!(is.numeric(in_put) | is.logical(in_put))) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric or Boolean."))
    return(NULL)  # return NULL
  }  # end if
  if (is.vector(in_put)) {  # in_put is a vector
    if(lag>0) {
      lagg_ed <- c(in_put[1:lag], in_put)
      lagg_ed <- lagg_ed[-((length(lagg_ed)-lag+1):length(lagg_ed))]
    } else {
      lagg_ed <- c(in_put, in_put[(length(in_put)+lag+1):length(in_put)])
      lagg_ed <- lagg_ed[-(1:(-lag))]
    }
  } else if (is.matrix(in_put)) {  # in_put is a matrix
    if(lag>0) {
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
#' @param lag integer equal to the number of time periods of lag. (default is 1)
#' @param ... additional arguments to function \code{xts::lag_xts()}.
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input \code{x_ts} time series.
#' @details Applies a time lag to an \emph{xts} time series and pads with the
#'   first and last values instead of \emph{NAs}.
#'
#'   A positive lag argument \code{lag} means values from \code{lag} periods in
#'   the past are moved to the present. A negative lag argument \code{lag} moves
#'   values from the future to the present.  The function \code{lag()} is just a
#'   wrapper for function \code{lag_xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with the first and last values instead of \emph{NAs}.
#' @examples
#' # lag by 10 periods
#' rutils::lag_xts(env_etf$VTI, lag=10)

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
#' @param lag integer equal to the number of time periods of lag. (default is 1)
#' @param ... additional arguments to function \code{xts::diff.xts()}.
#' @return An \emph{xts} time series with the same dimensions and the same time
#'   index as the input series.
#' @details Calculates the time differences of an \emph{xts} time series and
#'   pads with \emph{zeros} instead of \emph{NAs}.  Positive \code{lag} means
#'   differences are calculated with values from \code{lag} periods in the past
#'   (vice versa negative \code{lag}).  The function \code{diff()} is just a
#'   wrapper for \code{diff.xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with \emph{zeros} instead of \emph{NAs}.
#' @examples
#' # calculate time differences over lag by 10 periods
#' rutils::diff_xts(env_etf$VTI, lag=10)

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
#' @return An \emph{OHLC} time series with five columns for the \emph{Open},
#'   \emph{High}, \emph{Low}, \emph{Close} prices, and the \emph{Volume}, and
#'   with the same time index as the input series.
#' @details The reduced form of an \emph{OHLC} time series is obtained by
#'   calculating the time differences of its \emph{Close} prices, and by
#'   calculating the differences between its \emph{Open}, \emph{High}, and
#'   \emph{Low} prices minus the \emph{Close} prices. The standard form is the
#'   original \emph{OHLC} time series, and can be calculated from its reduced
#'   form by reversing those operations.
#' @examples
#' # calculate reduced form of an OHLC time series
#' diff_VTI <- rutils::diff_ohlc(env_etf$VTI)
#' # calculate standard form of an OHLC time series
#' VTI <- rutils::diff_ohlc(diff_VTI, re_duce=FALSE)
#' identical(VTI, env_etf$VTI[, 1:5])

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




#' Calculate the rolling sum of an \emph{xts} time series over a sliding window
#' (lookback period).
#'
#' @export
#' @param x_ts an \emph{xts} time series containing one or more columns of data.
#' @param win_dow the size of the lookback window, equal to the number of data
#'   points for calculating the rolling sum.
#' @return An \emph{xts} time series with the same dimensions as the input
#'   series.
#' @details For example, if win_dow=3, then the rolling sum at any point is
#'   equal to the sum of \code{x_ts} values for that point plus two preceding
#'   points.
#'   The initial values of roll_sum() are equal to cumsum() values, so that
#'   roll_sum() doesn't return any NA values.
#'
#'   The function \code{roll_sum()} performs the same operation as function
#'   \code{runSum()} from package
#'   \href{https://cran.r-project.org/web/packages/TTR/index.html}{TTR}, but
#'   using vectorized functions, so it's a little faster.
#'
#' @examples
#' # create xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' roll_sum(x_ts, win_dow=3)

roll_sum <- function(x_ts, win_dow) {
  cum_sum <- cumsum(x_ts)
  roll_sum <- cum_sum - lag(x=cum_sum, k=win_dow)
  roll_sum[1:win_dow, ] <- cum_sum[1:win_dow, ]
  roll_sum
}  # end roll_sum




#' Calculate the rolling maximum of an \emph{xts} time series over a sliding
#' window (lookback period).
#'
#' @export
#' @param x_ts an \emph{xts} time series containing one or more columns of data.
#' @param win_dow the size of the lookback window, equal to the number of data
#'   points for calculating the rolling sum.
#' @return An \emph{xts} time series with the same dimensions as the input
#'   series.
#' @details For example, if win_dow=3, then the rolling sum at any point is
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
#' roll_max(x_ts, win_dow=3)

roll_max <- function(x_ts, win_dow) {
  roll_max <- RcppRoll::roll_max(c(rep(0,win_dow-1), coredata(x_ts)), n=win_dow, align="right")
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
#' @return A single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
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
#' identical(x_ts, do_call_rbind(list_xts))

do_call_rbind <- function(li_st) {
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind odd and even elements, and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(li_st)) return(li_st[[in_dex]])
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
#' @return A single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#' @details Performs lapply loop, each time binding neighboring elements and
#'   dividing the length of \code{li_st} by half. The result of performing
#'   \code{do_call(rbind, list_xts)} on a list of \emph{xts} time series is
#'   identical to performing \code{do.call(rbind, list_xts)}. But
#'   \code{do.call(rbind, list_xts)} is very slow, and often causes an
#'   \sQuote{out of memory} error.
#' @examples
#' # create xts time series
#' x_ts <- xts(x=rnorm(1000), order.by=(Sys.time()-3600*(1:1000)))
#' # split time series into daily list
#' list_xts <- split(x_ts, "days")
#' # rbind the list back into a time series and compare with the original
#' identical(x_ts, do_call(rbind, list_xts))

do_call <- function(func_tion, li_st, ...) {
# produce function name from argument
  func_tion <- match.fun(func_tion)
  while (length(li_st) > 1) {
# index of odd list elements
    odd_index <- seq(from=1, to=length(li_st), by=2)
# bind neighboring elements and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(li_st)) {
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
#' @param sym_bols vector of strings with names of input objects.
#' @param out_put string with name of output object.
#' @param env_in environment containing the input \code{sym_bols}.
#' @param env_out environment for creating the \code{out_put}.
#' @param ... additional arguments to function \code{func_tion()}.
#' @return A single object (\code{matrix}, \emph{xts} time series, etc.)
#' @details Performs an lapply loop over \code{sym_bols}, applies the function
#'   \code{func_tion()}, merges the outputs into a single object, and creates
#'   the object in the environment \code{env_out}.  The output object is created
#'   as a side effect, while its name is returned invisibly.
#' @examples
#' new_env <- new.env()
#' do_call_assign(
#'    func_tion=ex_tract,
#'    sym_bols=env_etf$sym_bols,
#'    out_put="price_s",
#'    env_in=env_etf, env_out=new_env)

do_call_assign <- function(func_tion, sym_bols=NULL, out_put,
                           env_in=.GlobalEnv, env_out=.GlobalEnv, ...) {
# produce function name from argument
  func_tion <- match.fun(func_tion)
  if(is.null(sym_bols)) sym_bols <- ls(env_in)
  assign(out_put,
         do.call(merge,
                 lapply(mget(env_in$sym_bols, envir=env_in), func_tion, ...)),
         envir=env_out)
  invisible(out_put)
}  # end do_call_assign




#' Plot an \emph{xts} time series with custom y-axis range and with vertical
#' background shading.
#'
#' A wrapper for function \code{chart_Series()} from package
#' \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'
#' @export
#' @param x_ts \emph{xts} time series.
#' @param ylim \emph{numeric} vector with two elements containing the y-axis
#'   range.
#' @param in_dex  \emph{Boolean} vector or \emph{xts} time series for specifying
#'   the shading areas, with TRUE indicating "lightgreen" shading, and FALSE
#'   indicating "lightgrey" shading.
#' @param ... additional arguments to function \code{chart_Series()}.
#' @return A chart object \emph{chob} returned invisibly.
#' @details Extracts the chart object and modifies its ylim parameter using
#'   accessor and setter functions.
#'   Also adds background shading using function \code{add_TA()}. The
#'   \code{in_dex} argument should have the same length as the \code{x_ts} time
#'   series.
#'   Finally the function \code{chart_xts()} plots the chart object and returns
#'   it invisibly.
#' @examples
#' quantmod::chart_xts(env_etf$VTI["2015-11"],
#'   name="VTI in Nov 2015", ylim=c(102, 108),
#'   in_dex=zoo::index(env_etf$VTI["2015-11"]) > as.Date("2015-11-18"))

chart_xts <- function(x_ts, ylim=NULL, in_dex=NULL, ...) {
  stopifnot(inherits(x_ts, "xts"))
# extract chob and modify ylim using accessor and setter functions
  ch_ob <- chart_Series(x=x_ts, plot=FALSE, ...)
  if (!is.null(ylim)) {
    y_lim <- ch_ob$get_ylim()
    y_lim[[2]] <- structure(ylim, fixed=TRUE)
    ch_ob$set_ylim(y_lim)
  }  # end if
# add vertical background shading
  if (!is.null(in_dex)) {
    if (!xts::is.xts(in_dex))
      in_dex <- xts::xts(in_dex, order.by=zoo::index(x_ts))
    quantmod::add_TA(in_dex, on=-1, col="lightgreen", border=NA)
    quantmod::add_TA(!in_dex, on=-1, col="lightgrey", border=NA)
  }  # end if
# render the plot and return the chob invisibly
  plot(ch_ob)
  invisible(ch_ob)
}  # end chart_xts




#' Download time series data from an external source (by default \emph{OHLC}
#' prices from \emph{YAHOO}), and save it into an environment.
#'
#' @export
#' @param sym_bols vector of strings representing instrument symbols (tickers).
#' @param env_out environment for saving the data.
#' @param start_date start date of time series data.  (default is "2007-01-01")
#' @param end_date end date of time series data.  (default is \code{Sys.Date()})
#' @return A vector of \code{sym_bols} returned invisibly.
#' @details The function \code{get_symbols()} downloads \emph{OHLC} prices from
#'   \emph{YAHOO} into an environment, adjusts the prices, and saves them back
#'   to that environment. The function \code{get_symbols()} calls the function
#'   \code{getSymbols.yahoo()} to download the \emph{OHLC} prices, and performs
#'   a similar operation to the function \code{getSymbols()} from package
#'   \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'   But \code{get_symbols()} is faster (because it's more specialized), and is
#'   able to handle symbols like \emph{LOW}, which \code{getSymbols()} can't
#'   handle because the function \code{Lo()} can't handle them. The
#'   \code{start_date} and \code{end_date} must be either of class \emph{Date},
#'   or a string in the format "YYYY-mm-dd".
#'   \code{get_symbols()} returns invisibly the vector of \code{sym_bols}.
#' @examples
#' \dontrun{
#' new_env <- new.env()
#' get_symbols(sym_bols=c("MSFT", "XOM"),
#'             env_out=new_env,
#'             start_date="2012-12-01",
#'             end_date="2015-12-01")
#' }

get_symbols <- function(sym_bols,
                        env_out,
                        start_date="2007-01-01",
                        end_date=Sys.Date()) {
  # download prices from YAHOO
  quantmod::getSymbols.yahoo(sym_bols,
                             env=env_out,
                             from=start_date,
                             to=end_date)
  # adjust the OHLC prices and save back to env_out
  out_put <- lapply(sym_bols,
                    function(sym_bol) {
                      assign(sym_bol,
                             value=adjust_ohlc(get(sym_bol, envir=env_out)),
                             envir=env_out)
                      sym_bol
                    }
  )  # end lapply
  invisible(out_put)
}  # end get_symbols


