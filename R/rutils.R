#' Extract the name of an \code{OHLC} time series from its first column name.
#'
#' @export
#' @param x_ts \code{OHLC} time series.
#' @param field the integer index of the field to be extracted.
#' @return \code{string} with name of time series.
#' @details Extracts the symbol name (ticker) from the name of the first column
#'   of an \code{OHLC} time series.  The column name is assumed to be in the
#'   format "symbol.Open". It can also extract the field name after the "."
#'   separator, for example "Open" from "SPY.Open".
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
#' @return \code{integer} vector of equally spaced end points.
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
  if (last(end_points) > n_row)
    end_points <- end_points[-length(end_points)]
  if (last(end_points) > n_row)
    end_points <- end_points[-length(end_points)]
  if (last(end_points) < n_row)
    end_points <- c(end_points, n_row)
  as.integer(end_points)
}  # end end_points




#' Extract close prices from an \code{OHLC} time series.
#'
#' @export
#' @param x_ts an \code{OHLC} time series.
#' @param which_col partial name of the column to be be extracted.
#' @return single column \code{OHLC} time series in \code{xts} format.
#' @details Extracts close prices from an \code{OHLC} time series.  Assigns a
#'   column name corresponding to its symbol (ticker) and the column name, for
#'   example "VTI.Close".  The which_col argument is partially matched, for
#'   example "vol" is matched to "Volume".
#' @examples
#' # get close prices for VTI
#' clo_se(env_etf$VTI)
#' # get volumes for VTI
#' clo_se(env_etf$VTI, which_col="vol")

clo_se <- function(x_ts, which_col="Close") {
  in_dex <- grep(which_col, colnames(x_ts), ignore.case=TRUE)
  if (length(in_dex)>0)
#    out_put <- xts::.subset_xts(x_ts, 1:NROW(x_ts), in_dex:in_dex)
    x_ts[, in_dex]
  else
    stop(paste0("No column name containing \"", which_col, "\""))
}  # end clo_se




#' Apply a lag to a \code{numeric} vector or matrix.
#'
#' @export
#' @param in_put a \code{numeric} vector or matrix.
#' @param lag integer equal to the number of periods of lag.
#' @return vector or matrix with the same dimensions as the input object.
#' @details Applies a lag to a vector or matrix, by shifting its values by a
#'   certain number of rows, equal to the integer \code{lag}, and pads the
#'   leading or trailing stub periods with \code{zeros}. Positive \code{lag}
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
  if (!is.numeric(in_put)) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric."))
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




#' Calculate the row differences of a \code{numeric} vector or matrix.
#'
#' @export
#' @param in_put a \code{numeric} vector or matrix.
#' @param lag integer equal to the number of periods of lag.
#' @return vector or matrix with the same dimensions as the input object.
#' @details Calculates the row differences between rows that are \code{lag} rows
#'   apart. The leading or trailing stub periods are padded with \code{zeros}.
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
  if (!is.numeric(in_put)) {  # in_put is not numeric
    warning(paste("argument", deparse(substitute(in_put)), "must be numeric."))
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




#' Apply a time lag to an \code{xts} time series.
#'
#' @export
#' @param x_ts an \code{xts} time series.
#' @param k integer equal to the number of time periods of lag.
#' @param ... additional arguments to function \code{xts::lag_xts()}.
#' @return \code{xts} time series with the same dimensions and the same time
#'   index as the input series.
#' @details Applies a time lag to an \code{xts} time series and pads with
#'   \code{zeros} instead of \code{NAs}.  Positive lag \code{k} means values
#'   from \code{k} periods in the past are moved to the present.  Negative lag
#'   \code{k} moves values from the future to the present.  The function
#'   \code{lag()} is just a wrapper for function \code{lag_xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts},
#'   but it pads with \code{zeros} instead of \code{NAs}.
#' @examples
#' # lag by 10 periods
#' rutils::lag_xts(env_etf$VTI, k=10)

lag_xts <- function(x_ts, k=1, ...) {
  x_ts <- xts::lag.xts(x_ts, k=k, ...)
  x_ts[!complete.cases(x_ts), ] <- 0
  x_ts
}  # end lag_xts




#' Calculate the time differences of an \code{xts} time series.
#'
#' @export
#' @param x_ts an \code{xts} time series.
#' @param lag integer equal to the number of time periods of lag.
#' @param ... additional arguments to function \code{xts::diff_xts()}.
#' @return \code{xts} time series with the same dimensions and the same time
#'   index as the input series.
#' @details Calculates the time differences of an \code{xts} time series and
#'   pads with \code{zeros} instead of \code{NAs}.  Positive \code{lag} means
#'   differences are calculated with values from \code{lag} periods in the past
#'   (vice versa negative \code{lag}).  The function \code{diff()} is just a
#'   wrapper for \code{diff_xts()} from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but it
#'   pads with \code{zeros} instead of \code{NAs}.
#' @examples
#' # calculate time differences over lag by 10 periods
#' rutils::diff_xts(env_etf$VTI, lag=10)

diff_xts <- function(x_ts, lag=1, ...) {
  x_ts <- xts::diff.xts(x_ts, lag=lag, ...)
  x_ts[!complete.cases(x_ts), ] <- 0
  x_ts
}  # end diff_xts




#' Recursively \sQuote{\code{rbind}} a list of objects, such as \code{xts} time
#' series.
#'
#' Performs the same operation as \code{do.call(rbind, li_st)}, but using
#' recursion, which is much faster and uses less memory. This is the same
#' function as \sQuote{\code{\link[qmao]{do.call.rbind}}} from package
#' \sQuote{\href{https://r-forge.r-project.org/R/?group_id=1113}{qmao}}.
#'
#' @export
#' @param li_st list of objects, such as \code{vectors}, \code{matrices},
#'   \code{data frames}, or \code{time series}.
#' @return single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#' @details Performs lapply loop, each time binding neighboring elements and
#'   dividing the length of \code{li_st} by half. The result of performing
#'   \code{do_call_rbind(list_xts)} on a list of \code{xts} time series is
#'   identical to performing \code{do.call(rbind, list_xts)}. But
#'   \code{do.call(rbind, list_xts)} is very slow, and often causes an
#'   \sQuote{out of memory} error.
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
# bind neighboring elements and divide li_st by half
    li_st <- lapply(odd_index, function(in_dex) {
      if (in_dex==length(li_st)) {
        return(li_st[[in_dex]])
      }
      return(rbind(li_st[[in_dex]], li_st[[in_dex+1]]))
    })  # end lapply
  }  # end while
# li_st has only one element - return it
  li_st[[1]]
}  # end do_call_rbind




#' Recursively apply a function to a list of objects, such as \code{xts} time
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
#' @return single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#' @details Performs lapply loop, each time binding neighboring elements and
#'   dividing the length of \code{li_st} by half. The result of performing
#'   \code{do_call(rbind, list_xts)} on a list of \code{xts} time series is
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
#'   (\code{vector}, \code{xts} time series, etc.)
#' @param sym_bols vector of strings with names of input objects.
#' @param out_put string with name of output object.
#' @param env_in environment containing \code{sym_bols}.
#' @param env_out environment for creating \code{out_put}.
#' @param ... additional arguments to function \code{func_tion()}.
#' @return single object (\code{matrix}, \code{xts} time series, etc.)
#' @details Performs an lapply loop over \code{sym_bols}, applies the function
#'   \code{func_tion()}, merges the outputs into a single object, and creates
#'   the object in the environment \code{env_out}.  The output object is created
#'   as a side effect, while its name is returned invisibly.
#' @examples
#' new_env <- new.env()
#' do_call_assign(
#'    func_tion=clo_se,
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




#' Plot an \code{xts} time series with custom y-axis range and with vertical
#' background shading.
#'
#' A wrapper for function \code{chart_Series()} from package
#' \href{https://cran.r-project.org/web/packages/quantmod/index.html}{quantmod}.
#'
#' @export
#' @param x_ts \code{xts} time series.
#' @param ylim \code{numeric} vector with two elements containing the y-axis
#'   range.
#' @param in_dex  \code{Boolean} vector or \code{xts} time series for specifying
#'   the shading areas, with TRUE indicating "lightgreen" shading, and FALSE
#'   indicating "lightgrey" shading.
#' @param ... additional arguments to function \code{chart_Series()}.
#' @return chart object \code{chob} returned invisibly.
#' @details Extracts the chart object and modifies its ylim parameter using
#'   accessor and setter functions.
#'   Also adds background shading using function \code{add_TA()}. The
#'   \code{in_dex} argument should have the same length as the \code{x_ts} time
#'   series.
#'   Finally the function \code{chart_xts()} plots the chart object and returns
#'   it invisibly.
#' @examples
#' chart_xts(env_etf$VTI["2015-11"],
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
      in_dex <- xts(in_dex, order.by=zoo::index(x_ts))
    add_TA(in_dex, on=-1, col="lightgreen", border=NA)
    add_TA(!in_dex, on=-1, col="lightgrey", border=NA)
  }  # end if
# render the plot and return the chob invisibly
  plot(ch_ob)
  invisible(ch_ob)
}  # end chart_xts


