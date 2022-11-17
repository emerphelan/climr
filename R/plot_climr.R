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
