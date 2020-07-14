saveout.seiqhrf.icm <- function(dat, s, out = NULL) {

  ## new code ---
  alist2df <- function(dat, s) {
    alist <- list()
    alist$expTime <- dat$attr$expTime
    alist$infTime <- dat$attr$infTime
    alist$quarTime <- dat$attr$quarTime
    alist$recovTime <- dat$attr$recovTime
    alist$hospTime <- dat$attr$hospTime
    alist$dischTime <- dat$attr$dischTime
    alist$fatTime <- dat$attr$fatTime
    alist <- lapply(alist, `length<-`, max(lengths(alist)))
    return(data.frame(alist))
  }
  ## end new code ---

  if (s == 1) {
    out <- list()
    out$param <- dat$param
    out$control <- dat$control
    for (j in 1:length(dat$epi)) {
      out$epi[[names(dat$epi)[j]]] <- data.frame(dat$epi[j])
    }
  } else {
    for (j in 1:length(dat$epi)) {
      out$epi[[names(dat$epi)[j]]][, s] <- data.frame(dat$epi[j])
    }
  }

  # capture transition times from attribs
  # ## new code ---
  if (dat$control$type %in% c("SEIQHR", "SEIQHRF")) {
    out$times[[paste("sim", s, sep = "")]] <- alist2df(dat, s)
    out$attr_hist[[paste("sim", s, sep = "")]] <- dat$attr_hist
  }
  ## end new code ---

  ## Processing for final run
  if (s == dat$control$nsims) {

    # Remove functions from control list
    # ftodel <- grep(".FUN", names(out$control), value = TRUE)
    # out$control[ftodel] <- NULL
    # Set column names for varying list elements
    for (i in as.vector(which(lapply(out$epi, class) == "data.frame"))) {
      colnames(out$epi[[i]]) <- paste0("sim", 1:dat$control$nsims)
    }
  }

  return(out)
}

saveout_step.icm <- function (dat, at) {

  attr2df <- function(dat) {
    alist <- list()
    alist <- dat$attr
    alist <- lapply(alist, `length<-`, max(lengths(alist)))
    alist <- data.frame(alist)
    alist$status <- as.character(alist$status)
    return(alist)
  }

  dat$attr_hist[[at]]<- attr2df(dat)

  return(dat)
}
