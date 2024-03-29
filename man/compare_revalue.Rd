% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_revalue.R
\name{compare_revalue}
\alias{compare_revalue}
\title{Revalue the vectors}
\usage{
compare_revalue(
  data,
  id_cols = NULL,
  from = NULL,
  to = NULL,
  less = NULL,
  greater = NULL,
  equal = NULL,
  revalue = NULL
)
}
\arguments{
\item{data}{The data could be a vector, a matrix, or a data.frame.}

\item{id_cols}{The specified column (names) in the data. The parameter could be a vector or matrix, or data.frame, such as id_cols = "a", or id_cols =1, or id_cols = 1:3.}

\item{from}{Character or factor vectors that need to be adjusted in data, this parameter is not used with the less, greater and equal parameter.}

\item{to}{Target vector is used in conjunction with the from parameter, this parameter is not used with the less, greater and equal parameter.}

\item{less}{If data are the numeric vectors, use this parameter as a comparison of less than objects.}

\item{greater}{If data are the numeric vectors, use this parameter as a comparison of greater than objects.}

\item{equal}{If data are the numeric vectors, use this parameter as a comparison of equality objects.}

\item{revalue}{If data are the numeric vectors, use this parameter to specify the target vectors.}
}
\description{
You can choose the same character or factor vectors in the data and change them to the target vectors. 
If data are the numeric vectors can be adjusted to the target vectors by comparing sizes. 
The target vectors can be character, factor, or numeric vectors.
}
\examples{

## For vectors
## Create the "numeric" vector
set.seed(1)
test_num1 <- runif(10, min=-1, max=2)
compare_revalue(data = test_num1, less = 0.5, revalue = 1)

test_num2 <- c(0, 2, 1, 3, 4, -0.5, -0.11, -0.25, 0.5, 1.5, 2.5, 0.5)
compare_revalue(data = test_num2, greater = 0.5, equal  = 0.5, revalue = 1)


## Construct the "character" vector
test_char <- rep("a", 10)
compare_revalue(test_char, from = "a", to = "A")


## Construct the "factor" vector
test_factor <- factor(test_char)
compare_revalue(test_factor, from = "a", to = "A")


## For matrix
## Construct the "matrix"
set.seed(1)
test_matrix1 <- matrix(runif(1000000, min = -0.5, max = 1), nrow = 1000)
result1 <- compare_revalue(data = test_matrix1[1:100,1:500],
                           less = 0.5, equal = 0.5, revalue = 0);result1

set.seed(1)
test_matrix2 <- data.frame(a = sample(c("a","b","c"), 1000, replace = TRUE),
                           b = sample(c("A","B"), 1000, replace = TRUE),
                           c = sample(c("huawei","apple"), 1000, replace = TRUE),
                           d = sample(c(0, 1), 1000, replace = TRUE))
result2 <- compare_revalue(data = test_matrix2, from = "A", to = "C" );result2

set.seed(1)
test_matrix3 <- data.frame(a = sample(c(1,2,3), 1000,replace = TRUE),
                           b = sample(c(5,6,7), 1000, replace = TRUE),
                           c = sample(c(-1,-2,-3), 1000, replace = TRUE),
                           d = sample(c(0, 1), 1000, replace = TRUE))
result3 <- compare_revalue(data = test_matrix3, equal = 0.5, revalue = 0);result3


##How to perform batch character substitution and numerical comparison for specific columns 
##such as matrix, dataframe
result4 <- compare_revalue(data = test_matrix3, id_cols = 1:3, 
                           equal = 1, revalue = 0);result4

result5 <- compare_revalue(data = test_matrix3, id_cols = 1, 
                           equal = 1, revalue = 0);result5

result6 <- compare_revalue(data = test_matrix2, id_cols = "b", 
                           from = "A", to = "c");result6
}
\author{
Haowei Ni <jylande@163.com >
}
