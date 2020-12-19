#' Short summary of the HLM Model
#'
#'
#' @param nlme_object lme object from `nlme::lme`
#' @param sig_threshold default 0.05. The significant column will show `sig` in rows that are below the sig_threshold
#'
#' @return a dataframe with estimate, degree of freedom, p_value, and whether that p_value is significant
#' @export
#'
#' @examples model_summary(model)
#'
#'
model_summary <- function(nlme_object, sig_threshold = 0.05) {

  summary =  as.data.frame(summary(nlme_object)[20])

  return_df = summary %>%
    tibble::rownames_to_column(., var = 'variable') %>%
    select(variable,tTable.Value,tTable.DF,tTable.p.value) %>%
    mutate(estimate = round(tTable.Value,3)) %>%
    rename(DF = tTable.DF) %>%
    mutate(p_value = round(tTable.p.value, 5)) %>%
    mutate(significant = if_else(p_value < sig_threshold, 'sig.', '')) %>%
    select(variable,estimate,DF, p_value,significant)

  icc = as.numeric(performance::icc(nlme_object)[1])
  icc_df = data.frame(variable = 'ICC_Adjusted', estimate= round(icc,3), DF = NA, p_value = NA,significant = NA)
  return_df = rbind(return_df,icc_df)

  return(return_df)
}
