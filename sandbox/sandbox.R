
##############
### Prototype of function get_data() for rutils

get_data <- function(symbolv,
                     data_dir = NULL, ##  the directory containing csv files
                     data_env = NULL, ##  the environment for writing xts into
                     startd = "2000-01-01",
                     endd = Sys.Date(),
                     date_fun = match.fun("as.Date"),
                     formatv = "%Y-%m-%d",
                     header = TRUE,
                     echo = TRUE,
                     scrub = TRUE,
                     api.key = NULL) {
  if (is.null(data_dir)) {
    ##  download prices from Tiingo
    output <- quantmod::getSymbols.tiingo(symbolv,
                                          env = data_env,
                                          from = startd,
                                          to = endd,
                                          adjust = TRUE,
                                          auto.assign = TRUE,
                                          api.key = api.key)
    ##  Adjust the OHLC prices and save back to data_env
    ##  output <- lapply(symbolv,
    ##                    function(symbol) {
    ##                      assign(symbol,
    ##                             value = adjust_ohlc(get(symbol, envir = data_env)),
    ##                             envir = data_env)
    ##                      symbol
    ##                    }
    ##  )  ##  end lapply
    invisible(output)
  } else {
    ##  load from csv files
    filens <- file.path(data_dir, paste0(symbolv, ".csv"))
    invisible(sapply(filens, function(filen) {
      if (echo)
        cat("Loading instrument: \t", filen, "\n")
      datav <- xts::as.xts(zoo::read.zoo(file = filen,
                                         header = header, sep = ",",
                                         drop = FALSE,
                                         FUN = date_fun,
                                         format = formatv))
      if (scrub) {
        ##  overwrite NA values
        datav <- rutils::na_locf(datav)
        datav <- rutils::na_locf(datav, from_last = TRUE)
      }  ##  end if
      assign(rutils::get_name(colnames(datav)[1]),
             datav,
             envir = data_env)
      filen
    }))  ##  end sapply
  }  ##  end if
}  ##  end get_data


