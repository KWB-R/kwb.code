# filter_scripts ---------------------------------------------------------------
#' @importFrom kwb.utils matchesCriteria removeEmptyColumns
filter_scripts <- function(scriptInfo, fun.min = 5, epf.min = 10)
{
  criteria <- c(paste("fun >=", fun.min), paste("epf >=", epf.min))

  scriptInfo <- scriptInfo[kwb.utils::matchesCriteria(scriptInfo, criteria), ]

  kwb.utils::removeEmptyColumns(scriptInfo)
}
