load_climr <- function(type = c("GLB", "NH", "SH")) {

  ## match.arg() is a useful debugging function:
  ## it checks whether the supplied type is one of the available options,
  ## otherwise, it will return an error

  type <- match.arg(type)

  ## Get the URL of the data set

  url <- paste0("http://data.giss.nasa.gov/gistemp/tabledata_v3/", type, ".Ts+dSST.csv")

  ## Read in the data (using readr)

  output <- read_csv(url,
                     skip = 1, # skip the first (meaningless) line
                     na = "***", # specify the character for missing values
                     col_types = paste(c("i", rep("d", 18)), collapse=""),
                     progress = FALSE) # last two lines avoid stuff read_csv() prints by default

  ## Sort out the yearly data

  out_year <- output |>
    na.omit() |>
    mutate(year = Year, # note: changing to lowercase for the sake of tidyness
           temp = `J-D`, # note: use of backticks to extract numbers rather than characters
           x = year) |> # note: using x here to have a generic "x" for use with modelling later
    select(year, temp, x) %>%
    arrange(x)

  ## Sort out the monthly data

  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  out_month <- output |>
    pivot_longer(Jan:Dec,
                 names_to = "month",
                 values_to = "temp",
                 values_drop_na = TRUE) |>
    mutate(month = parse_factor(month, levels=months, ordered=TRUE),
           year = Year,
           x = year + as.numeric(month)/12) |>
    select(year, month, temp, x) |>
    arrange(x)

  ## Sort out the quarterly data
  quarters <- c("DJF", "MAM", "JJA", "SON")
  out_quarter <- output |>
    pivot_longer(DJF:SON,
                 names_to = "quarter",
                 values_to = "temp",
                 values_drop_na = TRUE) |>
    mutate(quarter = parse_factor(quarter, levels=quarters, ordered=TRUE),
           year = Year,
           x= year + as.numeric(quarter)/4 - 0.25) |>
    select(year, quarter, temp, x) |>
    arrange(x)

  ## Put it all in a list and return

  output <- list(clim_year = out_year,
                 clim_quarter = out_quarter,
                 clim_month = out_month)
  attr(output, "source") <- type
  class(output) <- c("climr", "listof")
  ## the "listof" class accounts for "climr" class objects not being of base type
  ## and prints the output nicely without having to define "print.climr()`
  return(output)
}
