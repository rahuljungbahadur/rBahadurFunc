#' @title Function for running typical residual analysis procedures on models
#' @description This function runs the commonly used residual analysis functions on model objects
#' @param modelObject A model object of class lm(), lmer(), nlme(), gls(), nlmer() or others (not tested for others)
#' @param data (Optional) A data frame which was used to fit the models. Required only for certain residual plots.
#' @param ... Optional grouping parameters for testing the grouping of residuals by these parameters.
#' @return A list of plots. Namely * Pearson residuals vs Fitted plot, normality of pearson residuals,
#' Shapiro-Wilk test of normality of the residuals, qqnorm plot of the residuals,
#'  boxplots of the residuals grouped by the grouping parameters (if specified).
#' @export
funcResidAnalysis <- function(modelObject, data, ...){
  grpList <- rlang::list2(...)
  residNormTest <- stats::shapiro.test(resid(modelObject, type = "pearson"))

  print(plot(modelObject, title = "residual plot"))
  plot(density(resid(modelObject, type = "pearson")))
  curve(dnorm(x, mean = 0, sd = sd(resid(modelObject, type = "pearson"))), col = "red", add = T)

  lapply(grpList, FUN = function(x) print(lattice::bwplot(resid(modelObject, type = "pearson") ~ eval(parse(text = x)),
                                                 data = data)))
  qqnorm(resid(modelObject, type = "pearson"))
  qqline(resid(modelObject, type = "pearson"), col = "red")

  return(list(residNormTest, AIC(modelObject)))
}
