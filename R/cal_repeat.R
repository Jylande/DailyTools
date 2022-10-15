#' @title Combinatorial operation between multiple groups of repeated samples
#'
#' @description This function can perform combinatorial computations on multiple sets of repeated samples, specifically to perform calculations (subtraction, addition, multiplication, division, etc.) 
#' in specifying x and y (two columns) under a category, as well as to compare all combinations.
#'
#' @param data The data should be a data frame that could be long or wide data, details, for example.
#' @param y A specified active object between two groups defaults to NULL if not specified
#' @param x A specified passive object between two groups defaults to NULL if not specified.
#' @param treat_reserve Using this parameter when processing variables are in the data set means that computes all subsets of the selected processing cases in the data set. The value of this parameter cannot exceed five. Do not set it as the default.
#' @param multi_vars When the data is long, use the parameter to select the target variable for comparison. If consider multiple variables between multiple groups of duplicate samples, the default value is NULL if not specified. 
#' If specified, select the columns of the corresponding variables in the data frame (for example, 4:5, fourth and fifth column variables). 
#' When the data is wide, use the parameter to compare the group—for example, the groups X2001 through X2005 in examples, or 7:11 columns in the data.
#' @param class_name Specifies the name of the variable to classify. After parameter is specified, it is divided into active or passive objects. It is only used for long data (because it needs to be identified when transposed to wide data). For wide data, use the default value.
#' @param var_name The parameter specification means only running evaluations of a single target variable. If the data is long, multi_vars and the parameter cannot be set simultaneously. If you don't specify the parameter, indicate the default value.
#' @param FUN A function to use on the outer products, found via match.fun (except for the special case "*", "-", "/", "+").
#' @export
#' @return
#' @author Haowei Ni <jylande@@163.com >
#' @examples
#'
#'# Trial data sets (Refer to the data structure of the example)
#' # long data
#' treat1 <- c("YES", "NO")
#' treat2 <- c("A", "B", "C")
#' treat3 <- c("a", "b", "c")
#' treat4 <- c("e", "f", "g")
#' treat5 <- c("D", "E")
#' class <- 2001:2005
#' vars_long <- expand.grid(treat1, treat2, treat3, treat4, treat5, class, stringsAsFactors = FALSE)
#' 
#' set.seed(1)
#' data_long <- data.frame(vars_long,
#'                     A = rnorm(n = nrow(vars_long), mean = 450, sd = 35),
#'                     B = rnorm(n = nrow(vars_long), mean = 100, sd = 5),
#'                     C = rnorm(n = nrow(vars_long), mean = 50, sd = 2)); data_long
#' 
#'  
#' # wide data
#' vars_wide <- expand.grid(treat1, treat2, treat3, treat4, treat5)
#' vars <-  rep(rep(LETTERS[1:3], each = 3), nrow(vars_wide))
#' 
#' set.seed(1)
#' data_wide <- data.frame(vars, vars_wide,  
#'                     "2001" = rnorm(length(vars), mean = 100, sd = 10),
#'                     "2002" = rnorm(length(vars), mean = 200, sd = 10),
#'                     "2003" = rnorm(length(vars), mean = 300, sd = 10),
#'                     "2004" = rnorm(length(vars), mean = 400, sd = 10),
#'                     "2005" = rnorm(length(vars), mean = 500, sd = 10)); data_wide
#'                      
#' 
#' #1 For long data, consider multiple variables
#' result1 <- cal_repeat(data = data_long, multi_vars = 7:9, treat_reserve = NULL, 
#'                    class_name = "Var6", var_name = NULL, y = NULL, x = NULL,
#'                    FUN = "-"); result1
#' 
#' result1.1 <- cal_repeat(data = data_long, multi_vars = 7:8, treat_reserve = NULL, 
#'                      class_name = "Var6", var_name = NULL, y = NULL, x = NULL,
#'                      FUN = "-"); result1.1
#' 
#' 
#' #1.2 Specify a single variable
#' result2 <- cal_repeat(data = data_long, y = NULL, x = NULL, multi_vars = NULL, 
#'                       class_name = "Var6", treat_reserve = NULL, 
#'                       var_name = "B", FUN = "-"); result2
#' 
#' result2.1 <- cal_repeat(data = data_long, y = NULL, x = NULL, multi_vars = NULL, 
#'                         class_name = "Var6", treat_reserve = NULL, 
#'                         var_name = "A", FUN = "-"); result2.1
#' 
#' 
#' #1.3 Specify a single variable and specify x and y
#' result3 <- cal_repeat(data = data_long, y = "2005", x = "2001", multi_vars = NULL, 
#'                       class_name = "Var6", treat_reserve = NULL,  
#'                       var_name = "B", FUN = "-"); result3
#' 
#' 
#' #1.4 Specify other categorical variable names
#' result4 <- cal_repeat(data = data_long, y = "E", x = "D", multi_vars = NULL,
#'                       class_name = "Var5", treat_reserve = NULL, 
#'                       var_name = "B", FUN = "-"); result4
#'  
#' result4.1 <- cal_repeat(data = data_long, class_name = "Var5", multi_vars = NULL,
#'                         treat_reserve = NULL, y = NULL, x = NULL,
#'                         var_name = "B", FUN = "-"); result4.1
#'  
#' result4.2 <- cal_repeat(data = data_long, multi_vars = 7:8, var_name = NULL, 
#'                         treat_reserve = NULL, y = NULL, x = NULL,
#'                         class_name = "Var5", FUN = "-"); result4.2
#'  
#'  
#' #2.1 For wide data, consider multiple variables
#' result5 <- cal_repeat(data = data_wide, y = NULL, x = NULL, multi_vars = 7:9, 
#'                       treat_reserve = NULL, class_name = NULL, var_name = NULL, 
#'                       FUN = "-"); result5
#'                       
#' result5.1 <- cal_repeat(data = data_wide, y = NULL, x = NULL, multi_vars = 7:9, 
#'                         treat_reserve = NULL, class_name = NULL, var_name = "A", 
#'                         FUN = "-"); result5.1
#'  
#' #2.2 Specify x and y
#' result6 <- cal_repeat(data = data_wide, y = "X2002", x = "X2001", multi_vars = NULL, 
#'                       treat_reserve = NULL, class_name = NULL, var_name = NULL, 
#'                       FUN = "-"); result6
#'                       
#' result6.1 <- cal_repeat(data = data_wide, y = "X2002", x = "X2001", multi_vars = NULL, 
#'                       treat_reserve = NULL, class_name = NULL, var_name = "A", 
#'                       FUN = "-"); result6.1
#'                       
#'                         
#' #3.1 More control variables
#' # for long data
#' result7 <- cal_repeat(data = data_long, y = NULL, x = NULL, 
#'                       treat_reserve = c("Var1", "Var2"), var_name = NULL,
#'                       multi_vars = 7:8, class_name = "Var6", 
#'                       FUN = "-"); result7
#' 
#' result7.1 <- cal_repeat(data = data_long, y = NULL, x = NULL, 
#'                       treat_reserve = c("Var1", "Var2", "Var3"), var_name = NULL,
#'                       multi_vars = 7:8, class_name = "Var6", 
#'                       FUN = "-"); result7.1     
#'                       
#' result7.2 <- cal_repeat(data = data_long, y = "2002", x = "2001", 
#'                       treat_reserve = c("Var1", "Var2", "Var3"), var_name = NULL,
#'                       multi_vars = NULL, class_name = "Var6", 
#'                       FUN = "-"); result7.2   
#'                                                                                                                       
#' result7.3 <- cal_repeat(data = data_long, y = "2002", x = "2001", 
#'                        treat_reserve = c("Var1", "Var2", "Var3", "Var4", "Var5"),
#'                        var_name = NULL, multi_vars = NULL, class_name = "Var6", 
#'                        FUN = "-"); result7.3                         
#'                    
#' # for wide data
#' result8 <- cal_repeat(data = data_wide,  y = NULL, x = NULL,
#'                        treat_reserve = c("Var1", "Var2"), 
#'                        multi_vars = 7:9, class_name = NULL, var_name = NULL,
#'                        FUN = "-"); result8
#' 
#' result8.1 <- cal_repeat(data = data_wide,  y = "X2002", x = "X2001",
#'                        treat_reserve = c("Var1", "Var2", "Var3"), 
#'                        multi_vars = NULL, class_name = NULL, var_name = NULL,
#'                        FUN = "-"); result8.1
#' 
#' 
#' #4.1 Other calculation methods (such as division)
#' result9 <- cal_repeat(data = data_long, multi_vars = 7:8, treat_reserve = NULL, 
#'                        class_name = "Var6", var_name = NULL, y = NULL, x = NULL,
#'                        FUN = "/"); result9
#'        
#'        
#'                                                    

cal_repeat <- function (data, y = NULL, x = NULL, 
                        treat_reserve = NULL, multi_vars = NULL, 
                        class_name = NULL, var_name = NULL, FUN="-") 
{
  
  require(tidyr)
  require(tidyverse)
  require(dplyr)

  values = NULL
  results = NULL
  labels = NULL
  names = NULL
  
  class_labels = NULL
  class_names = NULL
  var_labels = NULL
  var_names = NULL
  
  if ( is.null(data$vars) && is.null(class_name) ){
    stop("For long data, it is necessary to specify `class_name`.")
  }
  
  if ( !is.null(data$vars) && !is.null(multi_vars) && !is.null(x) && !is.null(y) ){
    stop("For long data, the `multi_vars` parameter cannot be specified at the same time as the `x` and `y` parameters.")
  }
  
  if ( (!is.null(x) && is.null(y) | is.null(x) && !is.null(y)) ){
    stop("The `x` and `y` parameters must be specified simultaneously.")
  }
  
  if ( !is.null(treat_reserve) && (length(treat_reserve) > 5) ){
    stop("The `treat_reserve` parameter cannot exceed 5.")
  }
  
  if ( is.null(data$vars) && !is.null(multi_vars) && !is.null(var_name) ){
    stop("For long data, the `multi_vars` cannot be specified at the same time as the `var_name` parameter.")
  }
  
  if ( !is.null(data$vars) && !is.null(class_name) ){
    stop("For wide data, the `class_name` parameter should not be specified.")
  }
  
  if ((ncol(data) < nrow(data)) & is.null(treat_reserve) & 
      !is.null(class_name) & !is.null(var_name)) {
    
    data = data[order(data[,class_name]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, class_name, vars, values)
    }
    
    rownames(df) <- 1:nrow(df)
    
    df1 = spread(df, class_name, values)
    df2 = as.data.frame(df1[, -1])
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2)-1)) {
        for (j in 1:(ncol(df2)-1)) {
          if (i <= j) next
          values <- c(outer((df2 %>% dplyr::select(-vars))[, i], 
                            (df2 %>% dplyr::select(-vars))[, j], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(colnames((df2 %>% dplyr::select(-vars)))[i], 
                                    colnames((df2 %>% dplyr::select(-vars)))[j], sep = FUN), times = length(values))
          class_names <- c(class_names, class_labels)
          var_labels <- rep(var_name, times = length(values))
          var_names <- c(var_names, var_labels)
          total <- data.frame(class_names, var_names, 
                              results)
        }
      }
      return(total)
    }
    
    if (!is.null(x) & !is.null(y)) {
      values <- c(outer(df2[, y], df2[, x], FUN))
      class_names <- rep(paste(y, x, sep = FUN), times = length(values))
      var_labels <- rep(var_name, times = length(values))
      var_names <- c(var_names, var_labels)
      total <- data.frame(class_names, var_names, values)
    }
    return(total)
  }
  
  if (is.null(data$vars) & !is.null(multi_vars) & is.null(treat_reserve) & 
      !is.null(class_name) & is.null(var_name)) {
    
    data = data[order(data[,class_name]),]
    
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, class_name, vars, values)
    }
    
    rownames(df) <- 1:nrow(df)  
    
    df1 = spread(df, class_name, values) %>% arrange(vars)
    df2 = as.data.frame(df1[, -1])
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2) - 1)) {
        for (j in 1:(ncol(df2) - 1)) {
          for (k in 1:length(unique(df2$vars))) {
            if (i <= j) next
            values <- c(outer((df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% dplyr::select(!vars))[, i], 
                              (df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% dplyr::select(!vars))[, j], 
                              FUN))
            results <- c(results, values)
            class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% 
                                                 dplyr::select(!vars))[i], 
                                      colnames(df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% 
                                                 dplyr::select(!vars))[j], sep = FUN), 
                                times = length(values))
            class_names <- c(class_names, class_labels)
            var_labels <- rep(unique(df2$vars)[k], 
                              times = length(values))
            var_names <- c(var_names, var_labels)
            total <- data.frame(class_names, var_names, 
                                results) %>% arrange(var_names)
          }
        }
      }
      return(total)
    }
    
    if (!is.null(x) & !is.null(y)) {
      for (k in 1:length(unique(df2$vars))) {
        values <- c(outer((df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% 
                             dplyr::select(!vars))[, y], 
                          (df2 %>% dplyr::filter(vars == unique(df2$vars)[k]) %>% 
                             dplyr::select(!vars))[, x], FUN))
        results <- c(results, values)
        class_labels <- rep(paste(y, x, sep = FUN), 
                            times = length(values))
        class_names <- c(class_names, class_labels)
        var_labels <- rep(unique(df2$vars)[k], times = length(values))
        var_names <- c(var_names, var_labels)
        total <- data.frame(class_names, var_names, 
                            results) %>% arrange(var_names)
      }
      return(total)
    }
  }
  
  if (is.null(data$vars) & (!is.null(multi_vars) | !is.null(var_name)) & !is.null(treat_reserve) &
      (length(treat_reserve) == 1) & !is.null(class_name)) {
    
    data = data[order(data[,class_name]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, class_name, vars, values)
    }
    
    rownames(df) <- 1:nrow(df)  
    
    df1 = pivot_wider(df, names_from = class_name, values_from = values) %>% 
      rename(treat_names = treat_reserve) %>% 
      arrange(vars)  
    
    df2 = as.data.frame(df1[, -1])
    
    treat_names = NULL
    results = NULL   
    class_names = NULL
    treat_names = NULL
    var_names = NULL  
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2) - 2)) {
        for (j in 1:(ncol(df2) - 2)) {
          for (k in 1:length(unique(df2$treat_names))) {
            for(m in 1:length(unique(df2$vars))){
              if (i <= j) next
              values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                                         vars == unique(df$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] & 
                                                                         vars == unique(df2$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(df2$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      return(final)
    }
    
    if (!is.null(x) & !is.null(y)){
      for (k in 1:length(unique(df2$treat_names))) {
        for(m in 1:length(unique(df2$vars))){
          values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), 
                              times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(df2$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      return(final)
    }
  }
  
  if (is.null(data$vars) & (!is.null(multi_vars) | !is.null(var_name)) & !is.null(treat_reserve) &
      (length(treat_reserve) == 2) & !is.null(class_name)) {
    
    data = data[order(data[,class_name]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    }
    
    total = unite(df, treat_names, c(treat_reserve[1], treat_reserve[2]))
    
    rownames(total) <- 1:nrow(total)
    
    df1 = pivot_wider(total, names_from = class_name, values_from = values) %>% arrange(vars)
    df2 = as.data.frame(df1[, -1])
    
    treat_names = NULL
    results = NULL   
    class_names = NULL
    treat_names = NULL
    var_names = NULL  
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2) - 2)) {
        for (j in 1:(ncol(df2) - 2)) {
          for (k in 1:length(unique(df2$treat_names))) {
            for(m in 1:length(unique(df2$vars))){
              if (i <= j) next
              values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                                         vars == unique(df$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] & 
                                                                         vars == unique(df2$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(df2$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2]), 
                        sep = "_")
      return(final)
    }
    
    if (!is.null(x) & !is.null(y)){
      for (k in 1:length(unique(df2$treat_names))) {
        for(m in 1:length(unique(df2$vars))){
          values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), 
                              times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(df2$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (is.null(data$vars) & (!is.null(multi_vars) | !is.null(var_name)) & !is.null(treat_reserve) &
      (length(treat_reserve) == 3) & !is.null(class_name)) {
    
    data = data[order(data[,class_name]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    }
    
    total = unite(df, treat_names, c(treat_reserve[1], treat_reserve[2], treat_reserve[3]))
    
    rownames(total) <- 1:nrow(total)
    
    df1 = pivot_wider(total, names_from = class_name, values_from = values) %>% arrange(vars)
    df2 = as.data.frame(df1[, -1])
    
    treat_names = NULL
    results = NULL   
    class_names = NULL
    treat_names = NULL
    var_names = NULL  
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2) - 2)) {
        for (j in 1:(ncol(df2) - 2)) {
          for (k in 1:length(unique(df2$treat_names))) {
            for(m in 1:length(unique(df2$vars))){
              if (i <= j) next
              values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                                         vars == unique(df$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] & 
                                                                         vars == unique(df2$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(df2$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], treat_reserve[3]),
                        sep = "_")
      return(final)
    }
    
    if (!is.null(x) & !is.null(y)){
      for (k in 1:length(unique(df2$treat_names))) {
        for(m in 1:length(unique(df2$vars))){
          values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), 
                              times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(df2$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], treat_reserve[3]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (is.null(data$vars) & (!is.null(multi_vars) | !is.null(var_name)) & !is.null(treat_reserve) &
      (length(treat_reserve) == 4) & !is.null(class_name)) {
    
    data = data[order(data[, class_name], data[, treat_reserve[1]], data[, treat_reserve[2]],
                      data[, treat_reserve[3], data[, treat_reserve[4]]]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    }
    
    total = unite(df, treat_names, c(treat_reserve[1], treat_reserve[2], 
                                     treat_reserve[3], treat_reserve[4]))
    
    rownames(total) <- 1:nrow(total)
    
    df1 = pivot_wider(total, names_from = class_name, values_from = values) %>% arrange(vars)
    df2 = as.data.frame(df1[, -1])
    
    treat_names = NULL
    results = NULL   
    class_names = NULL
    treat_names = NULL
    var_names = NULL  
    
    if ( is.null(x) & is.null(y) ) {
      for (i in 1:(ncol(df2) - 2)) {
        for (j in 1:(ncol(df2) - 2)) {
          for (k in 1:length(unique(df2$treat_names))) {
            for(m in 1:length(unique(df2$vars))){
              if (i <= j) next
              values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                                         vars == unique(df$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] & 
                                                                         vars == unique(df2$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(df2$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names, results)
            }
          }
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], 
                                 treat_reserve[3], treat_reserve[4]),
                        sep = "_")
      return(final)
    }
    
    if (!is.null(x) & !is.null(y)){
      for (k in 1:length(unique(df2$treat_names))) {
        for(m in 1:length(unique(df2$vars))){
          values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), 
                              times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(df2$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], 
                                 treat_reserve[3], treat_reserve[4]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (is.null(data$vars) & (!is.null(multi_vars) | !is.null(var_name)) & !is.null(treat_reserve) &
      (length(treat_reserve) == 5) & !is.null(class_name)) {
    
    data = data[order(data[,class_name]),]
    df = data %>% 
      gather(key = "vars", value = "values", 
             multi_vars, na.rm = TRUE)
    
    x2 = NULL
    for(i in 1:length(unique(df[, class_name]))){
      x1 = c(1:nrow(df[ df[,class_name]== unique(df[, class_name])[i], ]))
      x2 = c(x2, x1) 
    }
    
    if( is.null(df$vars) ){
      df = df %>% 
        rename(values = var_name) %>% 
        cbind(vars = var_name) %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    } else {
      df = df[order(df[,class_name]),]
      df = df %>% 
        cbind(x2) %>% 
        rename(x = x2) %>% 
        dplyr::select(x, treat_reserve, class_name, vars, values)
    }
    
    total = unite(df, treat_names, c(treat_reserve[1], treat_reserve[2], treat_reserve[3],
                                     treat_reserve[4], treat_reserve[5]))
    
    rownames(total) <- 1:nrow(total)
    
    df1 = pivot_wider(total, names_from = class_name, values_from = values) %>% arrange(vars)
    df2 = as.data.frame(df1[, -1])
    
    treat_names = NULL
    results = NULL   
    class_names = NULL
    treat_names = NULL
    var_names = NULL  
    
    if (is.null(x) & is.null(y)) {
      for (i in 1:(ncol(df2) - 2)) {
        for (j in 1:(ncol(df2) - 2)) {
          for (k in 1:length(unique(df2$treat_names))) {
            for(m in 1:length(unique(df2$vars))){
              if (i <= j) next
              values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                         vars == unique(df2$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                                         vars == unique(df$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] & 
                                                                         vars == unique(df2$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(df2$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], 
                                 treat_reserve[3], treat_reserve[4],
                                 treat_reserve[5]),
                        sep = "_")
      return(final)
    }
    
    if (!is.null(x) & !is.null(y)){
      for (k in 1:length(unique(df2$treat_names))) {
        for(m in 1:length(unique(df2$vars))){
          values <- c(outer((df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (df2 %>% dplyr::filter(treat_names == unique(df2$treat_names)[k] &
                                                     vars == unique(df2$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), 
                              times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(df2$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(df2$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], 
                                 treat_reserve[3], treat_reserve[4],
                                 treat_reserve[5]),
                        sep = "_")
      return(final)
    }
  }
  
  if (is.null(class_name) & is.null(treat_reserve) & 
      !is.null(var_name) & (length(var_name) == 1)) {
    
    results = NULL
    class_labels = NULL
    class_names = NULL
    var_labels = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      df2 = data %>% dplyr::filter(vars == var_name) %>% dplyr::select(vars, multi_vars)
      
      for (i in 1:(ncol(df2)-1)) {
        for (j in 1:(ncol(df2)-1)) {
          if (i <= j) next
          values <- c(outer((df2 %>% dplyr::select(-vars))[, i], 
                            (df2 %>% dplyr::select(-vars))[, j], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(colnames((df2 %>% dplyr::select(-vars)))[i], 
                                    colnames((df2 %>% dplyr::select(-vars)))[j], sep = FUN), times = length(values))
          class_names <- c(class_names, class_labels)
          var_labels <- rep(var_name, times = length(values))
          var_names <- c(var_names, var_labels)
          total <- data.frame(class_names, var_names, 
                              results)
        }
      }
      return(total)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)) {
      
      df2 = data %>% filter(vars == var_name) %>% dplyr::select(vars, x, y)
      
      values <- c(outer(df2[, y], df2[, x], FUN))
      class_names <- rep(paste(y, x, sep = FUN), times = length(values))
      var_labels <- rep(var_name, times = length(values))
      var_names <- c(var_names, var_labels)
      total <- data.frame(class_names, var_names, values)
    }
    return(total)
  }
  
  if (!is.null(data$vars) & is.null(treat_reserve) & 
      is.null(class_name) & is.null(var_name)) {
    
    results = NULL
    class_labels = NULL
    class_names = NULL
    var_labels = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      df = data %>% dplyr::select(vars, multi_vars)
      
      for (i in 1:(ncol(df) - 1)) {
        for (j in 1:(ncol(df) - 1)) {
          for (k in 1:length(unique(df$vars))) {
            if (i <= j) next
            values <- c(outer((df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% dplyr::select(-vars))[, i], 
                              (df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% dplyr::select(-vars))[, j], 
                              FUN))
            results <- c(results, values)
            class_labels <- rep(paste(colnames(df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% 
                                                 dplyr::select(-vars))[i], 
                                      colnames(df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% 
                                                 dplyr::select(-vars))[j], sep = FUN), 
                                times = length(values))
            class_names <- c(class_names, class_labels)
            var_labels <- rep(unique(df$vars)[k], 
                              times = length(values))
            var_names <- c(var_names, var_labels)
            total <- data.frame(class_names, var_names, 
                                results) %>% arrange(var_names)
          }
        }
      }
      return(total)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)) {
      
      df = data %>% dplyr::select(vars, x, y)
      
      for (k in 1:length(unique(df$vars))) {
        values <- c(outer((df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% 
                             dplyr::select(-vars))[, y], 
                          (df %>% dplyr::filter(vars == unique(df$vars)[k]) %>% 
                             dplyr::select(-vars))[, x], FUN))
        results <- c(results, values)
        class_labels <- rep(paste(y, x, sep = FUN), 
                            times = length(values))
        class_names <- c(class_names, class_labels)
        var_labels <- rep(unique(df$vars)[k], times = length(values))
        var_names <- c(var_names, var_labels)
        total <- data.frame(class_names, var_names, 
                            results) %>% arrange(var_names)
      }
      return(total)
    }
  }
  
  if (!is.null(data$vars) & is.null(class_name) & 
      is.null(var_name) & !is.null(treat_reserve) & (length(treat_reserve) == 1)) {
    
    results = NULL
    class_names = NULL
    treat_names = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      total = data %>% rename(treat_names = treat_reserve) %>% 
        dplyr::select(vars, treat_names, multi_vars)
      
      for (i in 1:(ncol(total) - 2)) {
        for (j in 1:(ncol(total) - 2)) {
          for (k in 1:length(unique(total$treat_names))) {
            for(m in 1:length(unique(total$vars))){
              if (i <= j) next
              values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                                           vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] & vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(total$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      return(final)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)){
      
      total = data %>% rename(treat_names = treat_reserve) %>% 
        dplyr::select(vars, treat_names, x, y)
      
      for (k in 1:length(unique(total$treat_names))) {
        for(m in 1:length(unique(total$vars))){
          values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(total$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      return(final)
    }
  }
  
  if (!is.null(data$vars) & is.null(class_name) & 
      is.null(var_name) & !is.null(treat_reserve) & (length(treat_reserve) == 2)) {
    
    results = NULL
    class_names = NULL
    treat_names = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      total = unite(data, treat_names, c(treat_reserve[1], treat_reserve[2])) %>% 
        dplyr::select(vars, treat_names, multi_vars-1)
      
      for (i in 1:(ncol(total) - 2)) {
        for (j in 1:(ncol(total) - 2)) {
          for (k in 1:length(unique(total$treat_names))) {
            for(m in 1:length(unique(total$vars))){
              if (i <= j) next
              values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                                           vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] & vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(total$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2]), 
                        sep = "_")
      return(final)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)){
      
      total = unite(data, treat_names, c(treat_reserve[1], treat_reserve[2])) %>% 
        dplyr::select(vars, treat_names, x, y)
      
      for (k in 1:length(unique(total$treat_names))) {
        for(m in 1:length(unique(total$vars))){
          values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(total$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (!is.null(data$vars) & is.null(class_name) & 
      is.null(var_name) & !is.null(treat_reserve) & (length(treat_reserve) == 3)) {
    
    results = NULL
    class_names = NULL
    treat_names = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      total = unite(data, treat_names, 
                    c(treat_reserve[1], treat_reserve[2], treat_reserve[3])) %>% 
        dplyr::select(vars, treat_names, multi_vars-2)
      
      for (i in 1:(ncol(total) - 2)) {
        for (j in 1:(ncol(total) - 2)) {
          for (k in 1:length(unique(total$treat_names))) {
            for(m in 1:length(unique(total$vars))){
              if (i <= j) next
              values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                                           vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] & vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(total$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], treat_reserve[3]), 
                        sep = "_")
      return(final)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)){
      
      total = unite(data, treat_names, 
                    c(treat_reserve[1], treat_reserve[2], treat_reserve[3])) %>% 
        dplyr::select(vars, treat_names, x, y)
      
      for (k in 1:length(unique(total$treat_names))) {
        for(m in 1:length(unique(total$vars))){
          values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(total$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2], treat_reserve[3]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (!is.null(data$vars) & is.null(class_name) & 
      is.null(var_name) & !is.null(treat_reserve) & (length(treat_reserve) == 4)) {
    
    results = NULL
    class_names = NULL
    treat_names = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      total = unite(data, treat_names, c(treat_reserve[1], treat_reserve[2],
                                         treat_reserve[3], treat_reserve[4])) %>% 
        dplyr::select(vars, treat_names, multi_vars-3)
      
      for (i in 1:(ncol(total) - 2)) {
        for (j in 1:(ncol(total) - 2)) {
          for (k in 1:length(unique(total$treat_names))) {
            for(m in 1:length(unique(total$vars))){
              if (i <= j) next
              values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                                           vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] & vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(total$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2],
                                 treat_reserve[3], treat_reserve[4]), 
                        sep = "_")
      return(final)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)){
      
      total = unite(data, treat_names, 
                    c(treat_reserve[1], treat_reserve[2], treat_reserve[3])) %>% 
        dplyr::select(vars, treat_names, x, y)
      
      for (k in 1:length(unique(total$treat_names))) {
        for(m in 1:length(unique(total$vars))){
          values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(total$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2],
                                 treat_reserve[3], treat_reserve[4]), 
                        sep = "_")
      return(final)
    }
  }
  
  if (!is.null(data$vars) & is.null(class_name) & 
      is.null(var_name) & !is.null(treat_reserve) & (length(treat_reserve) == 5)) {
    
    results = NULL
    class_names = NULL
    treat_names = NULL
    var_names = NULL
    
    if (!is.null(multi_vars) & is.null(x) & is.null(y)) {
      
      total = unite(data, treat_names, c(treat_reserve[1], treat_reserve[2],
                                         treat_reserve[3], treat_reserve[4],
                                         treat_reserve[5])) %>% 
        dplyr::select(vars, treat_names, multi_vars-4)
      
      for (i in 1:(ncol(total) - 2)) {
        for (j in 1:(ncol(total) - 2)) {
          for (k in 1:length(unique(total$treat_names))) {
            for(m in 1:length(unique(total$vars))){
              if (i <= j) next
              values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[,i], 
                                (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                           vars == unique(total$vars)[m]) %>% 
                                   dplyr::select(-vars, -treat_names))[, j], FUN))
              results <- c(results, values)
              class_labels <- rep(paste(colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                                           vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[i], 
                                        colnames(total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] & vars == unique(total$vars)[m]) %>% 
                                                   dplyr::select(-vars, -treat_names))[j], 
                                        sep = FUN), times= length(values))
              class_names <- c (class_names, class_labels)
              treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
              treat_names <- c(treat_names, treat_labels)
              var_labels <- rep(unique(total$vars)[m], times = length(values))
              var_names <- c(var_names, var_labels)
              final <- data.frame(class_names, var_names, treat_names,
                                  results) %>% arrange(treat_names, var_names)
            }
          }
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2],
                                 treat_reserve[3], treat_reserve[4],
                                 treat_reserve[5]), 
                        sep = "_")
      return(final)
    }
    
    if (is.null(multi_vars) & !is.null(x) & !is.null(y)){
      
      total = unite(data, treat_names, c(treat_reserve[1], treat_reserve[2],
                                         treat_reserve[3], treat_reserve[4],
                                         treat_reserve[5])) %>% 
        dplyr::select(vars, treat_names, x, y)
      
      for (k in 1:length(unique(total$treat_names))) {
        for(m in 1:length(unique(total$vars))){
          values <- c(outer((total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, y], 
                            (total %>% dplyr::filter(treat_names == unique(total$treat_names)[k] &
                                                       vars == unique(total$vars)[m]) %>% 
                               dplyr::select(-vars, -treat_names))[, x], FUN))
          results <- c(results, values)
          class_labels <- rep(paste(y, 
                                    x, 
                                    sep = FUN), times= length(values))
          class_names <- c (class_names, class_labels)
          treat_labels <- rep(unique(total$treat_names)[k], times = length(values))
          treat_names <- c(treat_names, treat_labels)
          var_labels <- rep(unique(total$vars)[m], times = length(values))
          var_names <- c(var_names, var_labels)
          final <- data.frame(class_names, var_names, treat_names,
                              results) %>% arrange(treat_names, var_names)
        }
      }
      final <- separate(final, 
                        col = treat_names, 
                        into = c(treat_reserve[1], treat_reserve[2],
                                 treat_reserve[3], treat_reserve[4],
                                 treat_reserve[5]), 
                        sep = "_")
      return(final)
    }
  }
}
