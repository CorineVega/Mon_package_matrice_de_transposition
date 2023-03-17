
#' Transposition
#'
#' @param M Any Matrix
#' @param i The row index of the non-zero value of Eij: May help to know which rows to swap
#' @param j The column index of the non-zero value of Eij: Can help to know the columns to permute
#' @description Determines whether a matrix is a transpose matrix or not. This information can help developers to do new experiments on matrices
#'
#' @return Boolean: TRUE if the matrix is a transpose matrix and FALSE otherwise
#' @export
#'
#' @examples transposition(array(c(1,0,0,0,0,1,0,1,0), dim = c(3,3)), 2, 3): This returns TRUE
#' transposition(array(c(2,5,0,0,2,1,0,1,2), dim = c(3,3)), 2, 3): this return FALSE

transposition <- function(M, i, j){

  In <- array(0, dim = c(dim(M)[1], dim(M)[2]))
  diag(In) <- 1
  Eii <- array(0, dim = c(dim(M)[1], dim(M)[2]))
  Eii[i, i] <- 1
  cat("MAT Eii", "\n")
  print(Eii)
  Ejj <- array(0, dim = c(dim(M)[1], dim(M)[2]))
  Ejj[j, j] <-  1
  cat("MAT Ejj", "\n")
  print(Ejj)
  Eij <- array(0, dim = c(dim(M)[1], dim(M)[2]))
  Eij[i, j] <- 1
  cat("MAT Eij", "\n")
  print(Eij)
  Eji <- array(0, dim = c(dim(M)[1], dim(M)[2]))
  Eji[j, i] <- 1
  cat("MAT Eji", "\n")
  print(Eji)
  Tij <- In - Eii - Ejj + Eij + Eji

  if(M != Tij && dim(M)[1] != dim(M)[2]){
    return(FALSE)
  }

  # On entre ici lorsque M = Tij

  else{

    A <- array(sample(1:3, dim(M)[1]*dim(M)[2], replace = TRUE), dim = c(dim(M)[1], dim(M)[2]))


    useA <- A
    cat("MAT A", "\n")
    print(useA)

    B <- array(sample(1:3, dim(M)[1]*dim(M)[2], replace = TRUE), dim = c(dim(M)[1], dim(M)[2]))


    useB <- B
    cat("MAT B", "\n")
    print(B)


    temp <- A[i, ]
    A[i, ] <- A[j, ]
    A[j, ] <- temp

    perm1 <- A

    temp2 <- B[, i]
    B[, i] <- B[, j]
    B[, j] <- temp2

    perm2 <- B


    if(solve(M) == M && M%*%useA == perm1 && useB%*%M == perm2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }

  }

}



