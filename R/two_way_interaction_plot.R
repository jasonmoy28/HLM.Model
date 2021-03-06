#' Create a two-way interaction plot
#'
#' This will create a three-way interaction plot. It uses the predict function on the lme object to get the values of the independent variable using the values of the independent variable that is 1 standard deviation above and below the mean.
#'
#' @param data dataframe.
#' @param nlme_object lme object from `nlme::lme`
#' @param predict_var_name vector of length 2. predictive variable names
#' @param graph_label_name vector of length 3 or function. Vector should be passed in the form of c(response_var, predict_var1, predict_var2). Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#' @param cateogrical_var list. use the form list(var_name1 = c(upper_bound1, lower_bound1), [var_name2 = c(upper_bound2, lower_bound2])
#' @param y_lim vector of length 2. c(lower_limit, upper_limit)
#' @param plot_color logical. default as F. Set to T if you want to plot in color
#' @param debug ingore this parameter
#'
#' @return ggplot object. two-way interaction plot
#' @export
#'
#' @examples
#'
#'
two_way_interaction_plot = function(data,
                                    nlme_object,
                                    predict_var_name,
                                    graph_label_name = NULL,
                                    cateogrical_var = NULL,
                                    y_lim = NULL,
                                    plot_color = F,
                                    debug = F) {

  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = dplyr::mutate_all(data, as.numeric)
  } else{
    return('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
  }

  response_var = as.character(nlme_object$call$fixed)[2]
  predict_var1 = predict_var_name[1]
  predict_var2 = predict_var_name[2]
  mean_df = dplyr::summarise_all(data, mean,na.rm = T)

  upper_df = dplyr::summarise_all(data, .funs = function(.){mean(.,na.rm = T) + 1*stats::sd(.,na.rm = T)})

  lower_df =
    dplyr::summarise_all(data, .funs = function(.){mean(.,na.rm = T) - 1*stats::sd(.,na.rm = T)})

  # Specify the categorical variable upper and lower bound directly
  if (!is.null(cateogrical_var)) {
    for (name in names(cateogrical_var)) {
      upper_df[name] = cateogrical_var[[name]][1]
      lower_df[name] = cateogrical_var[[name]][2]
    }
  }
  # Get the variable names of the model
  model_var_name = names(nlme_object$fixDF$terms[-1])

  # Update values in the new_data_df to the values in predicted_df & get the predicted value
  upper_upper_df = mean_df
  upper_upper_df[predict_var1] = upper_df[predict_var1]
  upper_upper_df[predict_var2] = upper_df[predict_var2]

  upper_lower_df = mean_df
  upper_lower_df[predict_var1] = upper_df[predict_var1]
  upper_lower_df[predict_var2] = lower_df[predict_var2]

  lower_upper_df = mean_df
  lower_upper_df[predict_var1] = lower_df[predict_var1]
  lower_upper_df[predict_var2] = upper_df[predict_var2]

  lower_lower_df = mean_df
  lower_lower_df[predict_var1] = lower_df[predict_var1]
  lower_lower_df[predict_var2] = lower_df[predict_var2]

  upper_upper_predicted_value = stats::predict(nlme_object,newdata = upper_upper_df,level = 0)
  upper_lower_predicted_value = stats::predict(nlme_object,newdata = upper_lower_df,level = 0)
  lower_upper_predicted_value = stats::predict(nlme_object,newdata = lower_upper_df,level = 0)
  lower_lower_predicted_value = stats::predict(nlme_object,newdata = lower_lower_df,level = 0)

  final_df = data.frame(
    value = c(upper_upper_predicted_value,upper_lower_predicted_value,lower_upper_predicted_value,lower_lower_predicted_value),
    var1_category = factor(c('High','High','Low','Low'),levels = c('Low', 'High')),
    var2_category = c('High','Low','High','Low'))

  # Get the correct label for the plot
  if (!is.null(graph_label_name)) {
    # If a vector of string is passed as argument, slice the vector
    if (class(graph_label_name)== 'character') {
      response_var_plot_label = graph_label_name[1]
      predict_var1_plot_label = graph_label_name[2]
      predict_var2_plot_label = graph_label_name[3]
      # if a function of switch_case is passed as an argument, use the function
    }else if (class(graph_label_name)== 'function') {
      response_var_plot_label = graph_label_name(response_var)
      predict_var1_plot_label = graph_label_name(predict_var1)
      predict_var2_plot_label = graph_label_name(predict_var2)
      # All other case use the original label
    } else{
      response_var_plot_label = response_var
      predict_var1_plot_label = predict_var1
      predict_var2_plot_label = predict_var2
    }
    # All other case use the original label
  } else{
    response_var_plot_label = response_var
    predict_var1_plot_label = predict_var1
    predict_var2_plot_label = predict_var2}


  if (is.null(y_lim)) {
    y_lim = c(floor(min(final_df$value)) - 0.5,ceiling(max(final_df$value)) + 0.5)
  }

  if (debug) {
    return(final_df)
  }

  if (plot_color){
    plot =
      ggplot2::ggplot(final_df, ggplot2::aes(y = value, x = var1_category, color = var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = var2_category)) +
      ggplot2::labs(y = response_var_plot_label,
           x = predict_var1_plot_label,
           color = predict_var2_plot_label) +
      papaja::theme_apa() +
      ggplot2::ylim(y_lim[1],y_lim[2])
  } else{
    plot =
      ggplot2::ggplot(final_df, ggplot2::aes(y = value, x = var1_category, group = var2_category)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(linetype = var2_category)) +
      ggplot2::labs(y = response_var_plot_label,
           x = predict_var1_plot_label,
           linetype = predict_var2_plot_label) +
      papaja::theme_apa() +
      ggplot2::ylim(y_lim[1],y_lim[2])
  }

  return(plot)

}
