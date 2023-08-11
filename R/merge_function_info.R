# merge_function_info ----------------------------------------------------------
merge_function_info <- function(scriptInfo, functionInfo)
{
  funExpressions <- expressions_per_function(functionInfo)
  
  merge(
    scriptInfo,
    funExpressions[, c("script", "epf")],
    by = "script",
    all.x = TRUE
  )
}
