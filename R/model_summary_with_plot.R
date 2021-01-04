#' Model summary with interaction plot
#'
#' A integrated function that combines all of the functionality of the pacakge.
#'
#' @param data required dataframe
#' @param response_variable required character or vector of length 1
#' @param level_1_factors required vector. Lower level variables (e.g., individual-level)
#' @param level_2_factors optional vector. Higher level variables (e.g., country-level)
#' @param two_way_interaction_factor optional vector of length more than 2. Default to `null`. vector in the form of c(predict_var1, predict_var2)
#' @param three_way_interaction_factor optional vector of length 3.  Default to `null`. vector in the form of c(predict_var1, predict_var2,predict_var3)
#' @param id required character or vector of length 1. The nesting variable (e.g. country)
#' @param graph_label_name optional vector or function. vector of length 2 for two-way interaction graph. vector of length 3 for three-way interaction graph. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, [predict_var3]). Function should be passed as a switch function. See below for an example.
#' @param estimation_method optional character. See `nlme::lme` for other options
#' @param return_result optional vector. Choose from `short_summary`,`long_summary`, `model`,`plot`,`none`. `none` return nothing. `short_summary` return a short model summary. `long_summary` return the summary using the `base::summary` function. `model` return a lme object. `plot` return the interaction plot.
#' @param print_result  optional vector. Choose from `both`, `long_summary`, `short_summary`, `plot`, `none`. `short_summary` return a short model summary. `long_summary` return the summary using the base::summary. `plot` return the interaction plot.
#' @param na.action required default to `na.exclude`. See `nlme::lme` for other options
#' @param cateogrical_var optional vector.
#' @param opt_control optional character. default to `optim`. Be aware that `nlme::lme` default to nlminb. See `nlme::lme` for other option
#' @param model_performance optional vector. default to c('R2_fixed_effect','R2_full_model'). `R2_full_model` for conditional R^2. `R2_fixed_effect` for marginal R^2. `icc` for intraclass correlation coefficient. Used the `performance::r2()` and `performance::icc()` for model performance
#'
#' @return
#' return a list of all requested items in the order of model, short_summary, long_summary, plot
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
                          cateogrical_var = NULL,
                          id,
                          graph_label_name = NULL,
                          estimation_method = 'REML',
                          opt_control = 'optim',
                          na.action = na.exclude,
                          model_performance = c('R2_fixed_effect','R2_full_model'),
                          return_result = NULL,
                          print_result = c('short_summary','plot')) {
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

  if (!is.null(two_way_interaction_factor) & (any(print_result %in% 'plot') | any(return_result %in% 'plot'))) {
    graphing_interaction_factor = two_way_interaction_factor[1:2]
    interaction_plot = two_way_interaction_plot(data = data,
                                                nlme_object = model,
                                                predict_var_name = graphing_interaction_factor,
                                                cateogrical_var = cateogrical_var,
                                                graph_label_name = graph_label_name)

    } else if (!is.null(three_way_interaction_factor) & (any(print_result %in% 'plot') | any(return_result %in% 'plot'))) {
    interaction_plot = three_way_interaction_plot(data = data,
                                                  nlme_object = model,
                                                  predict_var_name = three_way_interaction_factor,
                                                  cateogrical_var = cateogrical_var,
                                                  graph_label_name = graph_label_name)
    } else{
      interaction_plot = NULL
    }

  if (any(print_result %in% 'short_summary') | any(return_result %in% 'short_summary')) {
    model_summary_df = model_summary(nlme_object = model,model_performance = model_performance)
  } else {
    model_summary_df = NULL
  }

  # Check print result
  if (any(print_result %in% 'short_summary')) {
    print(model_summary_df)
  }

  if(any(print_result %in% 'long_summary')){
    print(summary(model))
  }

  if(any(print_result %in% 'plot')){
    try(print(interaction_plot))
  }



  # Check return result
  if (length(return_result) != 0) {
    if(any(return_result %in% 'plot')) {
      return_plot = interaction_plot
    } else{return_plot = NULL}

    if (any(return_result %in% 'short_summary')) {
      return_short_summary = model_summary_df
    } else{return_short_summary = NULL}

    if(any(return_result %in% 'model')) {
      return_model = model
    } else{return_model = NULL}

    if(any(return_result %in% 'long_summary')) {
      return_long_summary = summary(model)
    } else{return_long_summary = NULL}

    return_list = list(return_model,return_short_summary,return_long_summary,return_plot)
    return_list = return_list[!sapply(return_list,is.null)]
    return(return_list)
  }

  }
