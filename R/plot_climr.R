#' Plot climr output
#'
#' Loads in global or hemispheric data from NASA at different intervals.
#'
#' @param x An object of class \code{"climr_fit"} outputted from the \code{link{fit}} function.
#' @param time_grid An optional time grid over which to produce fitted values of the model.
#' @param ... Catches unused arguments to \code{plot} (not currently implemented).
#'
#' @return Nothing, just a nice ggplot.
#'
#' @export
#' @author Keefe Murphy - <\email{keefe.murphy@@mu.ie}>
#' @seealso \code{\link{fit}}, \code{\link{load_climr}}
#' @examples
#' dat <- load_climr(type = "SH")
#' mod1 <- fit(dat)
#' mod2 <- fit(dat, data_type="monthly", fit_type="smooth.spline")
#' mod3 <- fit(dat, data_type="quarterly", fit_type="loess")
#' plot(mod1)
#' plot(mod2)
#' plot(mod3)
plot.climr_fit <- function(x, time_grid = pretty(x$data$x, n=100), ...) {

  ## Create a nice plot from the output of fit.climr()

  ## First, get the data set
  df <- x$data

  ## Get some predicted values based on the time grid
  fits <- switch(x$fit_type,
                 lm = {
                   tibble(time_grid, pred=predict(x$model,
                                                  newdata=tibble(time_grid)))
                 },
                 loess = {
                   tibble(time_grid, pred=predict(x$model,
                                                  newdata=tibble(x=time_grid))) |> na.omit()
                 },
                 smooth.spline = {
                   tibble(time_grid, pred = predict(x$model, tibble(time_grid))$y[,1])
                 })

  ## Finally, create the plot
  ggplot(df, aes(x=x, y=temp)) +
    geom_point(aes(colour=temp)) +
    theme_bw() +
    xlab("Year") +
    ylab("Temperature Anomaly") +
    ggtitle(paste(x$fit_type, "based on", x$data_type, attr(x, "source"), "data")) +
    geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
    theme(legend.position = "None") +
    scale_color_viridis_c()
}
