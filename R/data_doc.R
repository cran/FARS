#' @title US GDP Growth Series 
#'
#' @description Quarterly US GDP growth series used as the dependent variable in the replication.
#'
#' @details
#' The original series contains quarterly GDP levels for 63 countries.
#' For replication, all series are converted to log-differenced annualized growth rates
#' (\code{diff(log(x)) * 400}). From this dataset, the U.S. series is extracted
#' and the first observation dropped to obtain 59 observations in total.
#'
#' @format A time series object with 59 quarterly observations.
#' @source Replication materials of González-Rivera et al. (2024).
#' @docType data
#' @name dep_variable
#' @usage data(dep_variable)
#' @keywords datasets
"dep_variable"


#' @title Macro-Financial Database 
#'
#' @description Macro-financial variables used in the replication exercise.
#'
#' @details
#' The original dataset contains 624 variables. For replication, the first 519 variables
#' are selected, converted to a numeric matrix, and outliers are corrected using
#' the function \code{correct_outliers(..., threshold = 5)} provided in FARS.
#'
#' @format A numeric matrix with 59 rows and 519 columns.
#' @source Replication materials of González-Rivera et al. (2024).
#' @docType data
#' @name mf_data
#' @usage data(mf_data)
#' @keywords datasets
"mf_data"


#' @title European Countries Inflation Series
#'
#' @description Monthly inflation series for 38 European countries
#'
#' @details
#' Derived from the Excel file \emph{inflation.xlsx} included in \code{inst/extdata/}.
#' The original series contains monthly HCPI series for 38 European countries.
#' For replication, HCPI prices are transformed into annualized month-on-month (mom)
#' inflation, with each inflation series sequentially cleaned of seasonal effects and outliers
#'
#' @format A numeric matrix with 239 rows and 38 columns.
#' @source Ha, Kose, and Ohnsorge (2023)
#' @docType data
#' @name inflation_data
#' @usage data(inflation_data)
#' @keywords datasets
"inflation_data"
