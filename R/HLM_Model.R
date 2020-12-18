#' HLM Model
#' The function calls the `nlme::lme`. See `nlme::lme` for full documentation
#'
#' @param data dataframe
#' @param response_variable character or vector of length 1
#' @param level_1_factors vector. Lower level variables (e.g., individual-level)
#' @param level_2_factors vector. Contains higher level variables (e.g., country-level)
#' @param two_way_interaction_factor vector of length more than 2. Default to `null``
#' @param three_way_interaction_factor vector of length 3. Two-way interaction factors are included
#' @param id character or vector of length 1. The nesting variable (e.g. country)
#' @param estimation_method default to `REML`. See `nlme::lme` for other option
#' @param optim_control default to `optim`. See `nlme::lme` for other option
#' @param na.action default to `na.exclude`. See `nlme::lme` for other option
#'
#' @return An object of class "lme" representing the linear mixed-effects model fit.
#' @export
#'
#' @examples model_summary(data = processed_df,
#'              response_variable = 'JS_SCALE',
#'              level_1_factors = c('JI_Individual','Gender_Individual', 'SES_Individual'),
#'              level_2_factors = c('UE_Country'),
#'              two_way_interaction_factor = c('JI_Individual', 'SES_Individual'), # pass either two-way or three-way interaction factor, not both
#'              id = 'Country')
#'
#'
HLM_model <- function(data, response_variable,
                      level_1_factors,
                      level_2_factors,
                      two_way_interaction_factor = NULL,
                      three_way_interaction_factor = NULL,
                      id,
                      estimation_method = 'REML',
                      optim_control = 'optim',
                      na.action = na.exclude)
{
  # Fixed factor inlcude both level factor
  fixed_factors = c(level_1_factors, level_2_factors)

  # Random factor only include individual_level factor
  random_factors = level_1_factors

  two_way_interaction_terms = NULL
  three_way_interaction_terms = NULL
  # Check if interaction term exist, if so, add interaction terms to fixed factor
  if (!is.null(two_way_interaction_factor)) {
    two_way_interaction_terms = two_way_interaction_terms(two_way_interaction_factor)
  }

  if (!is.null(three_way_interaction_factor)) {
    two_way_interaction_terms = NULL
    three_way_interaction_terms = paste(three_way_interaction_factor,collapse = '*')
  }
  fixed_factors = c(fixed_factors,two_way_interaction_terms,three_way_interaction_terms)
  # Create the formula for fixed factor
  fixed_factors_formula = as.formula(paste(paste(response_variable, '~'), paste(fixed_factors, collapse = '+')))
  # Created the formula for random factors
  random_factors_formula = as.formula(paste('~ 1 +', paste(random_factors, collapse = '+'), paste('|',id)))
  ctrl = lmeControl(opt=optim_control)
  # Run lme model
  model = do.call("lme", list(fixed = fixed_factors_formula,
                              random = random_factors_formula,
                              data = quote(data),
                              na.action = na.action,
                              control = ctrl,
                              method = estimation_method))

  return(model)
}
