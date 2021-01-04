#' Short summary of the HLM Model
#'
#'
#' @param nlme_object lme object from `nlme::lme`
#' @param sig_threshold default 0.05. The significant column will show `sig` in rows that are below the sig_threshold
#' @param model_performance vector. default to c('R2_fixed_effect','R2_full_model'). `R2_full_model` for conditional R^2. `R2_fixed_effect` for marginal R^2. `icc` for intraclass correlation coefficient. Used the `performance::r2()` and `performance::icc()` for model performance
#'
#' @return a dataframe with estimate, degree of freedom, p_value, and whether that p_value is significant
#' @export
#'
#' @examples model_summary(model)
#'
#'
model_summary <- function(nlme_object,
                          sig_threshold = 0.05,
                          model_performance = c('R2_fixed_effect','R2_full_model')) {

  summary =  as.data.frame(summary(nlme_object)[20])

  return_df = summary %>%
    tibble::rownames_to_column(., var = 'variable') %>%
    select(variable,tTable.Value,tTable.DF,tTable.p.value) %>%
    mutate(estimate = round(tTable.Value,3)) %>%
    rename(DF = tTable.DF) %>%
    mutate(p_value = round(tTable.p.value, 5)) %>%
    mutate(significant = if_else(p_value < sig_threshold, 'sig.', '')) %>%
    select(variable,estimate,DF, p_value,significant)

  R2_conditional_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
  R2_marginal_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)
  icc_df = as.data.frame(NULL,NULL,NULL,NULL,NULL)

  if (any(model_performance %in% 'R2_full_model')) {
    R2_conditional = as.numeric(performance::r2(nlme_object)[1][['R2_conditional']])
    R2_conditional_df = data.frame(variable = 'R^2_full_model', estimate= round(R2_conditional,3), DF = NA, p_value = NA,significant = NA)
  }
  if (any(model_performance %in% 'R2_fixed_effect')) {
    R2_marginal = as.numeric(performance::r2(nlme_object)[2][['R2_marginal']])
    R2_marginal_df = data.frame(variable = 'R^2_fixed_effect', estimate= round(R2_marginal,3), DF = NA, p_value = NA,significant = NA)
  }
  if (any(model_performance %in% 'icc')) {
    icc = as.numeric(performance::icc(nlme_object)[1])
    icc_df = data.frame(variable = 'ICC_Adjusted', estimate= round(icc,3), DF = NA, p_value = NA,significant = NA)
  }
  return_df = rbind(return_df,R2_conditional_df,R2_marginal_df,
                    icc_df)
  return(return_df)

}
