## Functions ##

# added variable plot function

avplot <- function(model, predictor) {
  require(tidyverse)
  # Extract the response variable
  response <- as.character(formula(model)[[2]])

  # Extract the first predictor variable
  predictor <- predictor

  # Extract all other covariates
  new <- str_remove(as.character(model$call)[2], paste0(response, " ~ "))
  words <- unlist(strsplit(new, " "))
  pattern <- ifelse(predictor == words[length(words)],
                    paste0("\\+ ", predictor),
                    paste0(predictor, " \\+ "))
  covariates <- str_remove(new, pattern)

  df <- tibble(
    x=resid(lm(paste0(predictor, " ~ ", covariates), data=model$model)),
    y=resid(lm(paste0(response, " ~ ", covariates), data=model$model))
  )

  df
}

# added variable plot function for survey models

svy_avplot <- function(model, predictor, design) {
  require(tidyverse)
  require(survey)
  # Extract the response variable
  response <- as.character(formula(model)[[2]])

  # Extract the first predictor variable
  predictor <- predictor

  # Extract all other covariates
  new <- str_remove(as.character(model$call)[2], paste0(response, " ~ "))
  words <- unlist(strsplit(new, " "))
  pattern <- ifelse(predictor == words[length(words)],
                    paste0("\\+ ", predictor),
                    paste0(predictor, " \\+ "))
  covariates <- str_remove(new, pattern)

  df <- tibble(
    x=resid(svyglm(paste0(predictor, " ~ ", covariates), design=design)),
    y=resid(svyglm(paste0(response, " ~ ", covariates), design=design)),
    weights=model$weights
  )

  df
}

## ggplot themes ##

theme_sharp <- function() {
  theme_classic(base_size = 12) +
    theme(panel.grid.major.y = element_line(),
          plot.tag = element_text(face="bold"))

}

theme_sharp2 <- function() {
  theme_classic(base_size = 12) +
    theme(panel.grid.major.x = element_line(),
          plot.tag = element_text(face="bold"))
}

theme_sharp2_small <- function() {
  theme_classic(base_size = 10) +
    theme(panel.grid.major.x = element_line(),
          plot.tag = element_text(face="bold"),
          axis.title.x = element_text(face="bold"))
}

theme_forest <- function() {
  theme_classic(base_size = 12) +
    theme(plot.tag = element_text(face="bold"))
}



