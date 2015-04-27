library(Matrix)
source("formula_parsers.R")

poly_ridge_regression <- function(formula, data, lambda=1) {
  obj <- structure(list(), class="poly.ridge.lm")

  obj$base_formula <- formula(get_nonspecial_terms(formula))
  obj$response_names <- get_response_name(formula)
  obj$poly_terms <- parse_special_terms(get_special_terms(formula))

  obj$train_matrix <- .poly_ridge_regression_design_matrix(
    obj$base_formula, obj$poly_terms, data, lambda
  )

  obj$train_response <- .poly_ridge_regression_response(
    data[, obj$response_name], obj$poly_terms
  )

  obj$lm <- lm.fit(obj$train_matrix, obj$train_response)

  obj
}

# Make a design matrix for polynomial ridge regression.
.poly_ridge_regression_design_matrix <- function(base_formula, poly_terms, data, lambda) {
  base_matrix <- model.matrix(base_formula, data)
  # Iterively add blocks
  if(!is.null(poly_terms)) {
    n_col_added_sofar <- 0
    n_poly <- length(poly_terms)
    for(i in 1:n_poly) {
      poly_term <- poly_terms[[i]]
      new_blocks <- .make_poly_blocks(
        poly_term, data, lambda,
        n_col_added_sofar=n_col_added_sofar,
        curr_ncol=ncol(base_matrix)
      )
      side_block <- rbind(new_blocks$pmatrix, new_blocks$sblock)
      bottom_block <- cbind(new_blocks$bblock, new_blocks$rmatrix)
      base_matrix <- cbind(base_matrix, side_block)
      base_matrix <- rbind(base_matrix, bottom_block)
      n_col_added_sofar <- n_col_added_sofar + poly_term$cdeg
    }
  }
  base_matrix[, "(Intercept)"] <- 1
  base_matrix
}

.poly_ridge_regression_response <- function(raw_response, poly_terms) {
  if(is.null(poly_terms)) {
    .return <- raw_response
  } else {
    n_poly <- length(poly_terms)
    tot_degree <- 0
    for(i in 1:n_poly) {tot_degree <- tot_degree + poly_terms[[i]]$cdeg}
    .return <- c(raw_response, rep(0, tot_degree))
  }
  .return
}

.make_poly_blocks <- function(poly_term, data, lambda, n_col_added_sofar, curr_ncol) {
  .return <- list()

  vname <- as.character(poly_term$cvar)
  vdegree <- poly_term$cdeg

  .return$pmatrix <- .make_poly_matrix(data, vname, vdegree)
  .return$rmatrix <- .make_shrinkage_matrix(vname, vdegree, lambda)
  .return$bblock <- .make_bottom_block(curr_ncol, vdegree)
  .return$sblock <- .make_side_block(n_col_added_sofar, vdegree)
  .return
}

.make_bottom_block <- function(curr_ncol, degree) {
  bm <- rep(0, degree*curr_ncol)
  dim(bm) <- c(degree, curr_ncol)
  bm
}

.make_side_block <- function(n_col_added_sofar, degree) {
  sm <- rep(0, n_col_added_sofar*degree)
  dim(sm) <- c(n_col_added_sofar, degree)
  sm
}

.make_poly_matrix <- function(data, vname, vdegree) {
  pmatrix <- poly(data[, vname], degree=vdegree)
  colnames(pmatrix) <- paste(vname, 1:vdegree, sep=".d.")
  pmatrix
}

.make_shrinkage_matrix <- function(vname, vdegree, lambda) {
  rmatrix <- as.matrix(Diagonal(x=sqrt(rep(lambda, vdegree)*1:vdegree)))
  colnames(rmatrix) <- paste(vname, 1:vdegree, sep=".d.")
  rmatrix
}