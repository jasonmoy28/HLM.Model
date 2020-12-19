#' Create a three-way interaction plot
#'
#' This will create a three-way interaction plot. It uses the predict function on the lme object to get the values of the independent variable using the values of the independent variable that is 1 standard deviation above and below the mean.
#'
#' @param data dataframe
#' @param nlme_object lme object from `nlme::lme`
#' @param predict_var_name vector of length 3. the variables' name for the two-way interaction plot
#' @param graph_label_name vector of length 3 or function. Vector should be passed in the form of c(response_var, predict_var1, predict_var2, predict_var3). Function should be passed as a switch function that return the label based on the name passed (e.g., a switch function)
#'
#' @return ggplot object. three-way interaction plot
#'
#' @export
#'
#' @examples
#' three_way_interaction_plot(data = processed_df,
#'                                    predcit_var_name = c('JI_Individual','Gender_Individual', 'SES_Country'), # only 3 elements
#'                                    graph_label_name = c('Job Satisfaction', 'Job Insecurity', 'Gender')) # response variable is job satisfaction. Two prediction variables are job insecurity and gender
#'
#'          three_way_interaction_plot(data = processed_df,
#'                                    predcit_var_name = c('JI_Individual','Gender_Individual', 'SES_Country'), # only 3 elements
#'                                    graph_label_name = graph_label_name) # graph_label_name is a switch function, do not use parenthesis
#'
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
#'
#'
#'
three_way_interaction_plot = function(data, nlme_object, predict_var_name, graph_label_name){
  # Convert to numeric if convertiable
  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% mutate_all(as.numeric)
  } else{
    return('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
  }

  response_var = as.character(nlme_object$call$fixed)[2]
  predict_var1 = predict_var_name[1]
  predict_var2 = predict_var_name[2]
  predict_var3 = predict_var_name[3]

  mean_df = data %>%
    summarise_all(mean,na.rm = T)

  upper_df = data %>%
    summarise_all(.funs = function(.){mean(.,na.rm = T) + 1*sd(.,na.rm = T)})

  lower_df = data %>%
    summarise_all(.funs = function(.){mean(.,na.rm = T) - 1*sd(.,na.rm = T)})
  # Get the variable names of the model
  model_var_name = names(nlme_object$fixDF$terms[-1])

  # Update values in the new_data_df to the values in predicted_df & get the predicted value
  # Plot 1
  upper_upper_upper_df = mean_df
  upper_upper_upper_df[predict_var1] = upper_df[predict_var1]
  upper_upper_upper_df[predict_var2] = upper_df[predict_var2]
  upper_upper_upper_df[predict_var3] = upper_df[predict_var3]

  upper_lower_upper_df = mean_df
  upper_lower_upper_df[predict_var1] = upper_df[predict_var1]
  upper_lower_upper_df[predict_var2] = lower_df[predict_var2]
  upper_lower_upper_df[predict_var3] = upper_df[predict_var3]

  lower_upper_upper_df = mean_df
  lower_upper_upper_df[predict_var1] = lower_df[predict_var1]
  lower_upper_upper_df[predict_var2] = upper_df[predict_var2]
  lower_upper_upper_df[predict_var3] = upper_df[predict_var3]


  lower_lower_upper_df = mean_df
  lower_lower_upper_df[predict_var1] = lower_df[predict_var1]
  lower_lower_upper_df[predict_var2] = lower_df[predict_var2]
  lower_lower_upper_df[predict_var3] = lower_df[predict_var3]

  upper_upper_upper_predicted_value = predict(nlme_object,newdata = upper_upper_upper_df,level = 0)
  upper_lower_upper_predicted_value = predict(nlme_object,newdata = upper_lower_upper_df,level = 0)
  lower_upper_upper_predicted_value = predict(nlme_object,newdata = lower_upper_upper_df,level = 0)
  lower_lower_upper_predicted_value = predict(nlme_object,newdata = lower_lower_upper_df,level = 0)

  # Second plot
  upper_upper_lower_df = mean_df
  upper_upper_lower_df[predict_var1] = upper_df[predict_var1]
  upper_upper_lower_df[predict_var2] = upper_df[predict_var2]
  upper_upper_lower_df[predict_var3] = lower_df[predict_var3]

  upper_lower_lower_df = mean_df
  upper_lower_lower_df[predict_var1] = upper_df[predict_var1]
  upper_lower_lower_df[predict_var2] = lower_df[predict_var2]
  upper_lower_lower_df[predict_var3] = lower_df[predict_var3]

  lower_upper_lower_df = mean_df
  lower_upper_lower_df[predict_var1] = lower_df[predict_var1]
  lower_upper_lower_df[predict_var2] = upper_df[predict_var2]
  lower_upper_lower_df[predict_var3] = lower_df[predict_var3]
  lower_lower_lower_df = mean_df

  lower_lower_lower_df[predict_var1] = lower_df[predict_var1]
  lower_lower_lower_df[predict_var2] = lower_df[predict_var2]
  lower_lower_lower_df[predict_var3] = lower_df[predict_var3]

  upper_upper_lower_predicted_value = predict(nlme_object,newdata = upper_upper_lower_df,level = 0)
  upper_lower_lower_predicted_value = predict(nlme_object,newdata = upper_lower_lower_df,level = 0)
  lower_upper_lower_predicted_value = predict(nlme_object,newdata = lower_upper_lower_df,level = 0)
  lower_lower_lower_predicted_value = predict(nlme_object,newdata = lower_lower_lower_df,level = 0)

  final_df = data.frame(
    value = c(
      upper_upper_upper_predicted_value,
      upper_lower_upper_predicted_value,
      lower_upper_upper_predicted_value,
      lower_lower_upper_predicted_value,

      upper_upper_lower_predicted_value,
      upper_lower_lower_predicted_value,
      lower_upper_lower_predicted_value,
      lower_lower_lower_predicted_value),
    var1_category = factor(c('High','High','Low','Low','High','High','Low','Low'),levels = c('Low', 'High')),
    var2_category = c('High','Low','High','Low','High','Low','High','Low'),
    var3_category = c('High','High','High','High','Low','Low','Low','Low'))

  # Get the correct label for the plot
  if (!is.null(graph_label_name)) {
    # If a vector of string is passed as argument, slice the vector
    if (class(graph_label_name)== 'character') {
      response_var_plot_label = graph_label_name[1]
      predict_var1_plot_label = graph_label_name[2]
      predict_var2_plot_label = graph_label_name[3]
      predict_var2_plot_label = graph_label_name[4]
      # if a function of switch_case is passed as an argument, use the function
    }else if (class(graph_label_name)== 'function') {
      response_var_plot_label = graph_label_name(response_var)
      predict_var1_plot_label = graph_label_name(predict_var1)
      predict_var2_plot_label = graph_label_name(predict_var2)
      predict_var3_plot_label = graph_label_name(predict_var3)

      # All other case use the original label
    } else{
      response_var_plot_label = response_var
      predict_var1_plot_label = predict_var1
      predict_var2_plot_label = predict_var2
      predict_var3_plot_label = predict_var3
    }
    # All other case use the original label
  } else{
    response_var_plot_label = response_var
    predict_var1_plot_label = predict_var1
    predict_var2_plot_label = predict_var2
    predict_var2_plot_label = predict_var3}

  final_df = final_df %>%
    mutate(var3_category = str_c(var3_category, ' ', predict_var3_plot_label))

  plot = final_df %>%
    ggplot(aes(y = value, x = var1_category, color = var2_category)) +
    geom_point() +
    geom_line(aes(group = var2_category)) +
    labs(y = "Job Satisfaction",
         x = predict_var1_plot_label,
         color = predict_var2_plot_label) +
    facet_wrap(~var3_category) +
    theme_bw() +
    ylim(-1,1)

  return(plot)
}
