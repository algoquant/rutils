#' Extract the name of an \code{OHLC} time series, corresponding to its symbol
#' (ticker).
#' 
#' @param x_ts \code{OHLC} time series.
#' @return  \code{string} with name of time series.
#' @details Extracts a name (string) from the first column of an \code{OHLC}
#'   time series, assumed to be in the format "symbol.Open".
#' @examples
#' # get name for VTI
#' col_name(env_etf$VTI)
col_name <- function(x_ts) strsplit(colnames(x_ts), split="[.]")[[1]][1]




#' Extract close prices from an \code{OHLC} time series.
#' 
#' @param x_ts \code{OHLC} time series.
#' @details Extracts close prices from an \code{OHLC} time series, and assigns a
#'   column name corresponding to its symbol (ticker).
#' @examples
#' # get close prices for VTI
#' get_close(env_etf$VTI)
get_close <- function(x_ts)
{
  which_col <- grep("Close", colnames(x_ts), ignore.case = TRUE)
  if (length(which_col)>0) {
    x_ts <- x_ts[, which_col]
    colnames(x_ts) <- strsplit(colnames(x_ts), split="[.]")[[1]][1]
    x_ts
    }
  else
    stop("No column name containing \"Close\"")
}  # end get_close




#' Recursively \sQuote{\code{rbind}} a list of objects, such as \code{xts} time
#' series.
#' 
#' Performs the same operation as \code{do.call(rbind, li_st)}, but using 
#' recursion, which is much faster and uses less memory. This is the same 
#' function as \sQuote{\code{\link[qmao]{do.call.rbind}}} from package 
#' \sQuote{\href{https://r-forge.r-project.org/R/?group_id=1113}{qmao}}.
#' 
#' @param li_st list of objects, such as \code{vectors}, \code{matrices},
#'   \code{data frames}, or \code{time series}.
#' @return  single \code{vector}, \code{matrix}, \code{data frame}, or
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
#' Performs the same operation as \code{do.call()}, but using recursion, which 
#' is much faster and uses less memory. The function \code{do_call()} is a
#' generalization of function \code{do_call_rbind()}.
#' 
#' @param func_tion name of function that returns a single object from a list of
#'   objects.
#' @param li_st list of objects, such as \code{vectors}, \code{matrices},
#'   \code{data frames}, or \code{time series}.
#' @return  single \code{vector}, \code{matrix}, \code{data frame}, or
#'   \code{time series}.
#' @details Performs lapply loop, each time binding neighboring elements and 
#'   dividing the length of \code{li_st} by half. The result of performing 
#'   \code{do_call(list_xts)} on a list of \code{xts} time series is identical
#'   to performing \code{do.call(rbind, list_xts)}. But \code{do.call(rbind,
#'   list_xts)} is very slow, and often causes an \sQuote{out of memory} error.
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




#' Applies a function to a list of objects, merges the outputs into a single
#' object, and creates the object in the output environment.
#' 
#' @param func_tion name of function that returns a single object
#'   (\code{vector}, \code{xts} time series, etc.)
#' @param sym_bols vector of strings with names of input objects.
#' @param out_put string with name of output object.
#' @param env_in environment containing \code{sym_bols}.
#' @param env_out environment for creating \code{out_put}.
#' @param ... additional parameters to \code{func_tion()}.
#' @return single object (\code{matrix}, \code{xts} time series, etc.)
#' @details Performs an lapply loop over \code{sym_bols}, applies the function 
#'   \code{func_tion()}, merges the outputs into a single object, and creates 
#'   the object in the environment \code{env_out}.  The output object is created
#'   as a side effect, while its name is returned invisibly.
#' @examples
#' do_call_assign(
#' func_tion=get_close, 
#' sym_bols=env_etf$sym_bols, 
#' out_put="price_s2", 
#' env_in=env_etf, env_out=new_env)
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

