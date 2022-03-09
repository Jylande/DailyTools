#' @title Revalue the vector
#'
#' @description You can choose the same character or factor vectors in the data and change them to the target vectors.
#' If data are the numeric vectors can be changed to the target vectors by comparing sizes.
#' The target vectors can be character, factor, or numeric vectors.
#'
#' @param data should be a vector, a matrix, or a dataframe
#' @param from character or factor vectors that need to be changed in data, this parameter is not used with the less, greater and equal parameter
#' @param to target vector, this parameter is used in conjunction with the from parameter, this parameter is not used with the less, greater and equal parameter
#' @param less if data are the numeric vectors, use this parameter as a comparison of less than objects
#' @param greater if data are the numeric vectors, use this parameter as a comparison of greater than objects
#' @param equal if data are the numeric vectors, use this parameter as a comparison of equality objects
#' @param revalue if data are the numeric vectors, use this parameter to specify the target vectors
#' @export
#' @return
#' @author Haowei Ni <jylande@@163.com >
#' @examples
#' ## Create a numeric vector
#' set.seed(123)
#' test_num <- runif(10, min=-1, max=2)
#' compare_revalue(data = test_num, less = 0.5, revalue = 1)
#'
#' test_num1 <- c(0, 2,1,3,4,-0.5,-0.11,-0.25,0.5,1.5,2.5, 0.5)
#' compare_revalue(data = test_num1, great = 0.5, equal  = 0.5, revalue = 1)
#'
#'
#' ## Construct a character vector
#' test_char <- rep("a", 10)
#' compare_revalue(test_char, from = "a", to = "A")
#'
#'
#' ## Construct a factor vector
#' test_factor <- factor(test_char)
#' compare_revalue(test_char, from = "a", to = "A")
#'
#'
#' ## Construct a matrix
#' set.seed(123)
#' test_matrix1 <- matrix(runif(1000000, min = -0.5, max = 1), nrow = 1000)
#' result1 <- compare_revalue(data = test_matrix1[1:100,1:100], less = 0.5, equal = 0.5, revalue = 0)
#' result1
#'
#' set.seed(123)
#' test_matrix2 <- data.frame(a = sample(c("a","b","c"), 1000,replace = TRUE),
#'                           b = sample(c("A","B"), 1000, replace = TRUE),
#'                           c = sample(c("huawei","apple"), 1000, replace = TRUE),
#'                            d = sample(c(0, 1), 1000, replace = TRUE))
#' result2 <- compare_revalue(data = test_matrix2, from = "A", to = "C" )
#' result2
#'
#' set.seed(123)
#' test_matrix3 <- data.frame(a = sample(c(1,2,3), 1000,replace = TRUE),
#'                           b = sample(c(5,6,7), 1000, replace = TRUE),
#'                           c = sample(c(-1,-2,-3), 1000, replace = TRUE),
#'                           d = sample(c(0, 1), 1000, replace = TRUE))
#' result3 <- compare_revalue(data = test_matrix3, equal = 0.5, revalue = 0)
#' result3
#'
compare_revalue <- function(data, from=NULL, to=NULL, less=NULL, greater=NULL, equal=NULL, revalue=NULL){
  
  if (  !is.null(from) && !is.null(less) ){
    stop("The `from` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }
  
  if (  !is.null(from) && !is.null(greater) ){
    stop("The `from` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }
  
  if (  !is.null(from) && !is.null(equal) ){
    stop("The `from` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }
  
  if (  !is.null(from) && !is.null(revalue) ){
    stop("The `from` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }
  
  if (  is.null(from) && is.vector(data) && is.character(data) | is.factor(data)  ){
    stop("The `from` value is missing.")
  }
  
  if ( is.vector(data) && is.character(data) | is.factor(data) ){
    result = ifelse(data == from, to, data)
    return(result)
  }
  
  if ( is.vector(data) && is.numeric(data) ){
    
    if( is.null(less) && is.null(equal) ){
      result = ifelse(data > greater, revalue, data)
      return(result)
    }
    
    if( is.null(greater) && is.null(equal) ){
      result = ifelse(data < less, revalue, data)
      return(result)
    }
    
    if( is.null(greater) && is.null(less) ){
      result = ifelse(data == equal, revalue, data)
      return(result)
    }
    
    if( is.null(greater) && less==equal ){
      result = ifelse(data <= less, revalue, data)
      return(result)
    }
    
    if( is.null(less) && greater==equal){
      result = ifelse(data >= greater, revalue, data)
      return(result)
    }
  }
  
  if ( is.matrix(data) | is.array(data) | is.data.frame(data) ){
    
    data = as.matrix(data)
    
    result = data.frame()
    
    if( !is.null(from) && !is.null(to) && is.null(less) && is.null(equal) && is.null(greater) && is.null(revalue) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] == from, to, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
    
    if( is.null(less) && is.null(equal) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] > greater, revalue, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
    
    if( is.null(greater) && is.null(equal)){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] < less, revalue, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
    
    if( is.null(greater) && is.null(less) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] == equal, revalue, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
    
    if( is.null(greater) && less==equal  ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] <= less, revalue, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
    
    if( is.null(less) && greater==equal ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result [i,j] <- ifelse(data[i,j] >= greater, revalue, data[i,j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
  }
}
