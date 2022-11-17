fit <- function(obj,
                data_type = c("yearly", "quarterly", "monthly"),
                fit_type = c("lm", "loess", "smooth.spline")) {
  UseMethod("fit")
}

fit.climr <- function(obj,
                      data_type = c("yearly", "quarterly", "monthly"),
                      fit_type = c("lm", "loess", "smooth.spline")) {

  ## Find out which data set to use
  data_type <- match.arg(data_type)

  ## Find out what type of fitting method to use
  fit_type <- match.arg(fit_type)

  ## Find out which bit of the data to use and get it
  dat_choose <- switch(data_type,
                       yearly = "clim_year",
                       quarterly = "clim_quarter",
                       monthly = "clim_month")
  dat <- obj[[dat_choose]]

  ## Fit some models
  mod <- switch(fit_type,
                lm = {
                  dat |> lm(temp ~ x, data=_)
                },
                loess = {
                  dat |> loess(temp ~ x, data=_)
                },
                smooth.spline = {
                  dat |> (\(x) smooth.spline(x$x, x$temp))()
                })
  print(mod)

  ## Output so it can be plotted
  output <- list(model = mod,
                 data = dat,
                 data_type = data_type,
                 fit_type = fit_type)
  attr(output, "source") <- attr(obj, "source")
  class(output) <- c("climr_fit", "listof")
  invisible(output)
}
