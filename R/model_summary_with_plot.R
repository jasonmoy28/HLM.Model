#' Model summary with interaction plot
#'
#' A integrated function that combines all of the functionality of the pacakge.
#'
#' @param data dataframe
#' @param response_variable character or vector of length 1
#' @param level_1_factors vector. Lower level variables (e.g., individual-level)
#' @param level_2_factors vector. Higher level variables (e.g., country-level)
#' @param two_way_interaction_factor vector of length more than 2. Default to `null`. vector in the form of c(predict_var1, predict_var2)
#' @param three_way_interaction_factor vector of length 3.  Default to `null`. vector in the form of c(predict_var1, predict_var2,predict_var3)
#' @param id character or vector of length 1. The nesting variable (e.g. country)
#' @param graph_label_name vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, [predict_var3]). Function should be passed as a switch function. See below for an example.
#' @param estimation_method default to `REML`. See `nlme::lme` for other options
#' @param return_result default to `none`. Choose from `short_summary`,`long_summary`, `model`,`plot`,`none`. `none` return nothing. `short_summary` return a short model summary. `long_summary` return the summary using the `base::summary` function. `model` return a lme object. `plot` return the interaction plot.
#' @param print_result  default to `both`. Choose from `both`, `long_summary`, `short_summary`, `plot`, `none`. `both` return a short_summary and plot. `short_summary` return a short model summary. `long_summary` return the summary using the base::summary. `plot` return the interaction plot.
#' @param optim_control default to `nlminb`. Another common option is `optim`. See `nlme::lme` for other options
#' @param na.action default to `na.exclude`. See `nlme::lme` for other options
#'
#' @return
#' @export
#'
#' @examples
#' model_summary(data = processed_df,
#'              response_variable = 'JS_SCALE',
#'              level_1_factors = c('JI_Individual','Gender_Individual', 'SES_Individual'),
#'              level_2_factors = c('UE_Country'),
#'              two_way_interaction_factor = c('JI_Individual', 'SES_Individual'),
#'              id = 'Country',
#'              graph_label_name = graph_label_name)
#'
#' # graph_label_function should be able to return the name of the label if the variable name is passed in
#' # example of graoh_label_name function, you must load the function in the script
#' graph_label_name <- function(var_name) {
#'                       var_name_processed =
#'                          switch (var_name,
#'                              'Variable_Name1' = 'Label_Name1',
#'                              'Variable_Name2' = 'Label_Name2',
#'                              'Variable_Name3' = 'Label_Name3')
#'                          if (is.null(var_name_processed)) {
#'                              var_name_processed = var_name }
#'                          return(var_name_processed)
#'                        }
#'
#'
model_summary_with_plot = function(data, response_variable,
                          level_1_factors,
                          level_2_factors = NULL,
                          two_way_interaction_factor = NULL,
                          three_way_interaction_factor = NULL,
                          id,
                          graph_label_name = NULL,
                          estimation_method = 'REML',
                          opt_control = NULL,
                          na.action = na.exclude,
                          return_result = 'none',
                          print_result = 'both') {
  # Required library

  # All data must be dummy-code or factorized before passing into the function
  # Check datatype is correct
  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% mutate_all(as.numeric)
  } else{
    return('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
  }

  if (!is.null(two_way_interaction_factor) & !is.null(three_way_interaction_factor)) {
    return('Error: Cannot passed both two_way_interaction_factor and three_way_interaction_factor. Passing three_way_interaction_factor automatically include all two-way interactions.')
  }


  model = HLM_model(data = data,
                    response_variable = response_variable,
                    level_1_factors = level_1_factors,
                    level_2_factors = level_2_factors,
                    two_way_interaction_factor = two_way_interaction_factor,
                    three_way_interaction_factor = three_way_interaction_factor,
                    id = id,
                    opt_control = opt_control,
                    na.action = na.action,
                    estimation_method = estimation_method)

  if (!is.null(two_way_interaction_factor)) {
    graphing_interaction_factor = two_way_interaction_factor[1:2]
    interaction_plot = two_way_interaction_plot(data = data,
                                                nlme_object = model,
                                                predict_var_name = graphing_interaction_factor,
                                                graph_label_name = graph_label_name)

    } else if(!is.null(three_way_interaction_factor)){
    interaction_plot = three_way_interaction_plot(data = data,
                                                  nlme_object = model,
                                                  predict_var_name = three_way_interaction_factor,
                                                  graph_label_name = graph_label_name)
  } else{
    error_message = 'Error: object two_way_interaction_factor is not provided'
  }

  model_summary_df = model_summary(nlme_object = model)

  # Check print result
  if (print_result == 'both') {
    print(model_summary_df)
    try(print(interaction_plot))

  } else if(print_result == 'short_summary'){
    print(model_summary_df)

  } else if(print_result == 'plot'){
    try(print(interaction_plot))

  } else if(print_result == 'long_summary'){
    print(summary(model))

  } else{
    return('Error: Print result must be set to both, dataframe, plot, or none. Print result cannot set to plot if interaction_plot is not provided. ')
  }


  # Check return result
  if(return_result == 'plot'){
    try(print(interaction_plot))

  } else if (return_result == 'short_summary ') {
    return(model_summary_df)

  } else if (return_result == 'none'){
    return_result = 'none' # doing something so it won't return null

  } else if(return_result == 'model'){
    return(model)

  } else if(return_result == 'long_summary'){
    return(summary(model))
  }
  else {
    return('Error: return_result must be set to short_summary, long_summary, plot, model, or none')
  }
}
