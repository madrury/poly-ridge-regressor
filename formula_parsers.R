get_nonspecial_terms <- function(formula) {
  trms <- terms(formula, specials="poly.shrink")
  trms_labels <- attr(trms, "term.labels")
  is_response <- attr(trms, "response")
  special_idxs <- attr(trms, "specials")$poly.shrink
  if(length(special_idxs) == length(trms_labels)) {
    resp_nm <- .get_response_name(formula)
    formula(paste(resp_nm, "1", sep="~"))
  } else if(is.null(special_idxs)) {
    trms
  } else {
    drop_terms <- special_idxs - is_response
    drop.terms(trms, dropx=drop_terms, keep.response=TRUE)
  }
}

get_special_terms <- function(formula) {
  trms <- terms(formula, specials="poly.shrink")
  trms_labels <- attr(trms, "term.labels")
  is_response <- attr(trms, "response")
  special_idxs <- attr(trms, "specials")$poly.shrink
  if(is.null(special_idxs)) {
    NULL
  } else {
    drop_terms <- is_response - special_idxs
    drop.terms(trms, dropx=drop_terms, keep.response=FALSE)
  }
}

parse_special_terms <- function(special_terms) {
  # The first call object in "variables" is call(list)
  if(is.null(special_terms)) {
    .return <- NULL
  } else {
    n_terms <- length(attr(special_terms, "term.labels"))
    call_objs <- attr(special_terms, "variables")[-1]
    .return = list()
    for(i in 1:n_terms) {
      call_obj <- call_objs[[i]]
      .return[[i]] <- list(cname=call_obj[[1]], cvar=call_obj[[2]], cdeg=call_obj[[3]])
    }
  }
  .return
}

.get_response_name <- function(formula) {
    resp_nm <- as.character(attr(terms(formula), "variables")[[2]])
}