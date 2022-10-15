#' @title Revalue the vectors
#'
#' @description You can choose the same character or factor vectors in the data and change them to the target vectors. 
#' If data are the numeric vectors can be adjusted to the target vectors by comparing sizes. 
#' The target vectors can be character, factor, or numeric vectors.
#'
#' @param data The data could be a vector, a matrix, or a data.frame.
#' @param id_cols The specified column (names) in the data. The parameter could be a vector or matrix, or data.frame, such as id_cols = "a", or id_cols =1, or id_cols = 1:3.
#' @param from Character or factor vectors that need to be adjusted in data, this parameter is not used with the less, greater and equal parameter.
#' @param to Target vector is used in conjunction with the from parameter, this parameter is not used with the less, greater and equal parameter.
#' @param less If data are the numeric vectors, use this parameter as a comparison of less than objects.
#' @param greater If data are the numeric vectors, use this parameter as a comparison of greater than objects.
#' @param equal If data are the numeric vectors, use this parameter as a comparison of equality objects.
#' @param revalue If data are the numeric vectors, use this parameter to specify the target vectors.
#' @export
#' @return
#' @author Haowei Ni <jylande@@163.com >
#' @examples
#'
#' ## For vectors
#' ## Create the "numeric" vector
#' set.seed(1)
#' test_num1 <- runif(10, min=-1, max=2)
#' compare_revalue(data = test_num1, less = 0.5, revalue = 1)
#'
#' test_num2 <- c(0, 2, 1, 3, 4, -0.5, -0.11, -0.25, 0.5, 1.5, 2.5, 0.5)
#' compare_revalue(data = test_num2, greater = 0.5, equal  = 0.5, revalue = 1)
#'
#'
#' ## Construct the "character" vector
#' test_char <- rep("a", 10)
#' compare_revalue(test_char, from = "a", to = "A")
#'
#'
#' ## Construct the "factor" vector
#' test_factor <- factor(test_char)
#' compare_revalue(test_factor, from = "a", to = "A")
#'
#'
#' ## For matrix
#' ## Construct the "matrix"
#' set.seed(1)
#' test_matrix1 <- matrix(runif(1000000, min = -0.5, max = 1), nrow = 1000)
#' result1 <- compare_revalue(data = test_matrix1[1:100,1:500],
#'                            less = 0.5, equal = 0.5, revalue = 0);result1
#'
#' set.seed(1)
#' test_matrix2 <- data.frame(a = sample(c("a","b","c"), 1000, replace = TRUE),
#'                            b = sample(c("A","B"), 1000, replace = TRUE),
#'                            c = sample(c("huawei","apple"), 1000, replace = TRUE),
#'                            d = sample(c(0, 1), 1000, replace = TRUE))
#' result2 <- compare_revalue(data = test_matrix2, from = "A", to = "C" );result2
#'
#' set.seed(1)
#' test_matrix3 <- data.frame(a = sample(c(1,2,3), 1000,replace = TRUE),
#'                            b = sample(c(5,6,7), 1000, replace = TRUE),
#'                            c = sample(c(-1,-2,-3), 1000, replace = TRUE),
#'                            d = sample(c(0, 1), 1000, replace = TRUE))
#' result3 <- compare_revalue(data = test_matrix3, equal = 0.5, revalue = 0);result3
#'
#'
#' ##How to perform batch character substitution and numerical comparison for specific columns 
#' ##such as matrix, dataframe
#' result4 <- compare_revalue(data = test_matrix3, id_cols = 1:3, 
#'                            equal = 1, revalue = 0);result4
#' 
#' result5 <- compare_revalue(data = test_matrix3, id_cols = 1, 
#'                            equal = 1, revalue = 0);result5
#'
#' result6 <- compare_revalue(data = test_matrix2, id_cols = "b", 
#'                            from = "A", to = "c");result6

compare_revalue <- function(data, id_cols=NULL, from=NULL, to=NULL,
                            less=NULL, greater=NULL, equal=NULL, revalue=NULL){

  if (  !is.null(from) && !is.null(less) ){
    stop("The `from & to` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }

  if (  !is.null(from) && !is.null(greater) ){
    stop("The from & to` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }

  if (  !is.null(from) && !is.null(equal) ){
    stop("The `from & to` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }

  if (  !is.null(from) && !is.null(revalue) ){
    stop("The from & to` and `less`, `greater`, `equal` or `revalue` cannot be used at the same time.")
  }

  if (  is.null(from) && is.null(to) && is.vector(data) && (is.character(data) | is.factor(data))  ){
    stop("The `from & to` value is missing.")
  }

  if (  is.null(from) && !is.null(to) && is.vector(data) && (is.character(data) | is.factor(data))  ){
    stop("The `from` value is missing.")
  }

  if (  !is.null(from) && is.null(to) && is.vector(data) && (is.character(data) | is.factor(data))  ){
    stop("The `to` value is missing.")
  }

  if ( is.null(id_cols) && is.vector(data) && (is.character(data) | is.factor(data)) ){
    result = ifelse(data == from, to, data)
    return(result)
  }

  if ( is.null(id_cols) && is.vector(data) && is.numeric(data) ){

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


  if ( is.null(id_cols) && (is.matrix(data) | is.array(data) | is.data.frame(data)) ){

    data = as.matrix(data)

    result = data.frame()

    if( !is.null(from) && !is.null(to) && is.null(less) && is.null(equal) && is.null(greater) && is.null(revalue) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i, j] <- ifelse(data[i, j] == from, to, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }

    if(  is.null(less) && is.null(equal) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i,j] <- ifelse(data[i, j] > greater, revalue, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }

    if( is.null(greater) && is.null(equal)){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i,j] <- ifelse(data[i, j] < less, revalue, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }

    if( is.null(greater) && is.null(less) ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i, j] <- ifelse(data[i, j] == equal, revalue, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }

    if( is.null(greater) && less==equal  ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i, j] <- ifelse(data[i, j] <= less, revalue, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }

    if( is.null(less) && greater==equal ){
      for(i in 1:nrow(data)){
        for(j in 1:ncol(data)){
          result[i, j] <- ifelse(data[i, j] >= greater, revalue, data[i, j])
        }
      }
      colnames(result) = colnames(data)
      return(result)
    }
  }

  if ( !is.null(id_cols) && (length(id_cols) <= 1) &&
       (is.matrix(data) | is.array(data) | is.data.frame(data)) ){

    data = as.data.frame(data)

    id_result = data.frame()

    id_data = as.vector(data[, id_cols])

    if( !is.null(from) && is.character(id_data) | is.factor(id_data) && !is.null(to) && is.null(less) && is.null(equal) &&
        is.null(greater) && is.null(revalue) ){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] == from, to, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( (is.vector(id_data) && is.numeric(id_data) | is.integer(id_data) | is.double(id_data)) &&
        is.null(less) && is.null(equal) ){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] > greater, revalue, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( (is.vector(id_data) && is.numeric(id_data) | is.integer(id_data) | is.double(id_data)) &&
        is.null(greater) && is.null(equal)){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] < less, revalue, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( (is.vector(id_data) && is.numeric(id_data) | is.integer(id_data) | is.double(id_data)) &&
        is.null(greater) && is.null(less) ){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] == equal, revalue, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( (is.vector(id_data) && is.numeric(id_data) | is.integer(id_data) | is.double(id_data)) &&
        is.null(greater) && less==equal  ){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] <= less, revalue, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( (is.vector(id_data) && is.numeric(id_data) | is.integer(id_data) | is.double(id_data)) &&
        is.null(less) && greater==equal ){
      for(i in 1:length(id_data)){
        id_result[i, 1] <- ifelse(id_data[i] >= greater, revalue, id_data[i])
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }
  }

  if ( !is.null(id_cols) && (length(id_cols) > 1) &&
       (is.matrix(data) | is.array(data) | is.data.frame(data)) ){

    data = as.data.frame(data)

    id_result = data.frame()

    id_data = data[, id_cols]

    if( !is.null(from) && !is.null(to) && is.null(less) && is.null(equal) &&
        is.null(greater) && is.null(revalue) ){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] == from, to, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( is.null(less) && is.null(equal) ){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] > greater, revalue, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( is.null(greater) && is.null(equal)){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] < less, revalue, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( is.null(greater) && is.null(less) ){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] == equal, revalue, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( is.null(greater) && less==equal  ){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] <= less, revalue, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }

    if( is.null(less) && greater==equal ){
      for(i in 1:nrow(id_data)){
        for(j in 1:ncol(id_data)){
          id_result[i, j] <- ifelse(id_data[i, j] >= greater, revalue, id_data[i, j])
        }
      }
      colnames(id_result) <- colnames(data)[id_cols]
      return(id_result)
    }
  }
}



