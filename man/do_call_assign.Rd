% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rutils.R
\name{do_call_assign}
\alias{do_call_assign}
\title{Apply a function to a list of objects, merge the outputs into a single
object, and assign the object to the output environment.}
\usage{
do_call_assign(
  func_tion,
  sym_bols = NULL,
  out_put,
  env_in = .GlobalEnv,
  env_out = .GlobalEnv,
  ...
)
}
\arguments{
\item{\code{func_tion}}{The name of a function that returns a single object
(\emph{vector}, \emph{xts} time series, etc.)}

\item{\code{sym_bols}}{A vector of \emph{character} strings with the names of
input objects.}

\item{\code{out_put}}{The string with name of output object.}

\item{\code{env_in}}{The environment containing the input \code{sym_bols}.}

\item{\code{env_out}}{The environment for creating the \code{out_put}.}

\item{\code{...}}{Additional arguments to function \code{func_tion()}.}
}
\value{
A single object (\emph{matrix}, \emph{xts} time series, etc.)
}
\description{
Apply a function to a list of objects, merge the outputs into a single
object, and assign the object to the output environment.
}
\details{
The function \code{do_call_assign()} performs an lapply loop over
  \code{sym_bols}, applies the function \code{func_tion()}, merges the
  outputs into a single object, and creates the object in the environment
  \code{env_out}.  The output object is created as a side effect, while its
  name is returned invisibly.
}
\examples{
new_env <- new.env()
rutils::do_call_assign(
   func_tion=get_col,
   sym_bols=rutils::etf_env$sym_bols,
   out_put="price_s",
   env_in=etf_env, env_out=new_env)
}
