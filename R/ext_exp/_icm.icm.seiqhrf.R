merge.seiqhrf.icm <- function(x, y, ...) {

  ## Check structure
  if (length(x) != length(y) || !identical(names(x), names(y))) {
    stop("x and y have different structure")
  }
  if (x$control$nsims > 1 & y$control$nsims > 1 &
    !identical(sapply(x, class), sapply(y, class))) {
    stop("x and y have different structure")
  }

  ## Check params
  check1 <- identical(x$param, y$param)
  check2 <- identical(
    x$control[-which(names(x$control) %in% c("nsims", "currsim", "initialize.FUN", "infection.FUN", "recovery.FUN", "departures.FUN",
"arrivals.FUN", "get_prev.FUN"))],
    y$control[-which(names(y$control) %in% c("nsims", "currsim", "initialize.FUN", "infection.FUN", "recovery.FUN", "departures.FUN",
"arrivals.FUN", "get_prev.FUN"))]
  )

  if (check1 == FALSE) {
    stop("x and y have different parameters")
  }
  if (check2 == FALSE) {
    stop("x and y have different controls")
  }


  z <- x
  new.range <- (x$control$nsims + 1):(x$control$nsims + y$control$nsims)

  # Merge data
  for (i in 1:length(x$epi)) {
    if (x$control$nsims == 1) {
      x$epi[[i]] <- data.frame(x$epi[[i]])
    }
    if (y$control$nsims == 1) {
      y$epi[[i]] <- data.frame(y$epi[[i]])
    }
    z$epi[[i]] <- cbind(x$epi[[i]], y$epi[[i]])
    names(z$epi[[i]])[new.range] <- paste0("sim", new.range)
  }

  z$control$nsims <- max(new.range)

  return(z)
}


icm.seiqhrf <- function(param, init, control) {
  if (param$start_point > 0) { # && !is.null(param$sim)) {
    reinit <- TRUE
  } else {
    reinit <- FALSE
  }

  crosscheck.icm(param, init, control)
  verbose.icm(control, type = "startup")
  nsims <- control$nsims
  ncores <- ifelse(control$nsims == 1, 1, min(future::availableCores(), control$ncores))
  control$ncores <- ncores

  if (ncores == 1) {

    # Simulation loop start
    for (s in 1:control$nsims) {

      ## Initialization module
      if (reinit) {
        dat <- param$sim
        dat$epi <- lapply(param$sim$epi, function(x) x[, s])
        dat$attr <- as.list(param$sim$attr_hist[[s]][[param$start_point]])
        dat$attr_hist <- param$sim$attr_hist[[s]]
        dat$init <- list()
        dat$init[["e.num"]] <- dat$epi$e.num[[param$start_point]]
        dat$init[["i.num"]] <- dat$epi$i.num[[param$start_point]]
        dat$init[["q.num"]] <- dat$epi$q.num[[param$start_point]]
        dat$init[["h.num"]] <- dat$epi$h.num[[param$start_point]]
        dat$init[["r.num"]] <- dat$epi$r.num[[param$start_point]]
        dat$init[["f.num"]] <- dat$epi$f.num[[param$start_point]]
        # dat <- init_status.icm(dat)
        dat <- get_prev.icm(dat, at = param$start_point)
        nsteps <- (param$start_point + 1):control$nsteps
      } else if (!is.null(control[["initialize.FUN"]])) {
        # dat <- do.call(control[["initialize.FUN"]], list(param, init, control))
        dat <- initialize.icm(param, init, control)
        dat <- saveout_step.icm(dat, 1)
        nsteps <- 2:control$nsteps
      }

      # Timestep loop
      for (at in nsteps) {

        ## User Modules
        um <- control$user.mods
        if (length(um) > 0) {
          for (i in 1:length(um)) {
            dat <- do.call(control[[um[i]]], list(dat, at))
          }
        }

        ## Infection
        # if (!is.null(control[["infection.FUN"]])) {
        #   dat <- do.call(control[["infection.FUN"]], list(dat, at))
        # }
        dat <- infection.seiqhrf.icm(dat, at)

        ## Recovery
        # if (!is.null(control[["recovery.FUN"]])) {
        #   dat <- do.call(control[["recovery.FUN"]], list(dat, at))
        # }
        dat <- progress.seiqhrf.icm(dat, at)

        ## Departure Module
        # if (!is.null(control[["departures.FUN"]])) {
        #   dat <- do.call(control[["departures.FUN"]], list(dat, at))
        # }
        dat <- departures.seiqhrf.icm(dat, at)

        ## Arrival Module
        # if (!is.null(control[["arrivals.FUN"]])) {
        #   dat <- do.call(control[["arrivals.FUN"]], list(dat, at))
        # }
        dat <- arrivals.icm(dat, at)
        # dat <- arrivals.seiqhrf.icm(dat, at)

        ## Outputs
        # if (!is.null(control[["get_prev.FUN"]])) {
        #   dat <- do.call(control[["get_prev.FUN"]], list(dat, at))
        # }
        dat <- get_prev.seiqhrf.icm(dat, at)

        ## Track progress
        verbose.icm(dat, type = "progress", s, at)
        dat <- saveout_step.icm(dat, at)
      }

      # Set output
      if (s == 1) {
        out <- saveout.seiqhrf.icm(dat, s)
      } else {
        out <- saveout.seiqhrf.icm(dat, s, out)
      }
    } # Simulation loop end

    class(out) <- "icm"
  } # end of single core execution

  if (ncores > 1) {
    doParallel::registerDoParallel(ncores)

    sout <- foreach(s = 1:nsims, .packages = "EpiModel") %dopar% {

      # source("./ext/_icm.mod.init.seiqhrf.R") # dirty fix, foreach doest not find this functions
      source("D:/Workspaces/RStudio/Covid-19/R/ext_exp/_icm.saveout.seiqhrf.R") # dirty fix, foreach doest not find this functions
      #
      control$nsims <- 1
      control$currsim <- s

      set.seed(control$currsim)

      ## Initialization module
      if (reinit) {
        dat <- param$sim
        dat$epi <- lapply(param$sim$epi, function(x) x[, s])
        dat$attr <- as.list(param$sim$attr_hist[[s]][[param$start_point]])
        dat$attr_hist <- param$sim$attr_hist[[s]]
        dat$init <- list()
        dat$init[["e.num"]] <- dat$epi$e.num[[param$start_point]]
        dat$init[["i.num"]] <- dat$epi$i.num[[param$start_point]]
        dat$init[["q.num"]] <- dat$epi$q.num[[param$start_point]]
        dat$init[["h.num"]] <- dat$epi$h.num[[param$start_point]]
        dat$init[["r.num"]] <- dat$epi$r.num[[param$start_point]]
        dat$init[["f.num"]] <- dat$epi$f.num[[param$start_point]]
        # dat <- init_status.icm(dat)
        dat <- get_prev.icm(dat, at = param$start_point)
        nsteps <- (param$start_point + 1):control$nsteps
      } else if (!is.null(control[["initialize.FUN"]])) {
        # dat <- do.call(control[["initialize.FUN"]], list(param, init, control))
        dat <- initialize.icm(param, init, control)
        dat <- saveout_step.icm(dat, 1)
        nsteps <- 2:control$nsteps
      }

      # Timestep loop
      for (at in nsteps) {

        ## User Modules
        um <- control$user.mods
        if (length(um) > 0) {
          for (i in 1:length(um)) {
            dat <- do.call(control[[um[i]]], list(dat, at))
          }
        }

        ## Infection
        if (!is.null(control[["infection.FUN"]])) {
          dat <- do.call(control[["infection.FUN"]], list(dat, at))
        }
        # dat <- infection.seiqhrf.icm(dat, at)

        ## Recovery
        if (!is.null(control[["recovery.FUN"]])) {
          dat <- do.call(control[["recovery.FUN"]], list(dat, at))
        }
        # dat <- progress.seiqhrf.icm(dat, at)

        ## Departure Module
        if (!is.null(control[["departures.FUN"]])) {
          dat <- do.call(control[["departures.FUN"]], list(dat, at))
        }
        # dat <- departures.seiqhrf.icm(dat, at)

        ## Arrival Module
        if (!is.null(control[["arrivals.FUN"]])) {
          dat <- do.call(control[["arrivals.FUN"]], list(dat, at))
        }
        # dat <- arrivals.icm(dat, at)
        # dat <- arrivals.seiqhrf.icm(dat, at)

        ## Outputs
        if (!is.null(control[["get_prev.FUN"]])) {
          dat <- do.call(control[["get_prev.FUN"]], list(dat, at))
        }
        # dat <- get_prev.seiqhrf.icm(dat, at)

        ## Track progress
        verbose.icm(dat, type = "progress", s, at)
        dat <- saveout_step.icm(dat, at)
      }

      # Set output
      out <- saveout.seiqhrf.icm(dat, s = 1)
      class(out) <- "icm"
      return(out)
    }

    # aggregate results collected from each thread
    collected_times <- list()

    # collect the times from sout then delete them
    for (i in 1:length(sout)) {
      collected_times[[paste0("sim", i)]] <- sout[[i]]$times$sim1
      sout[[i]]$times <- NULL
    }

    # merge $epi structures
    merged.out <- sout[[1]]
    for (i in 2:length(sout)) {
      merged.out <- merge.seiqhrf.icm(merged.out, sout[[i]], param.error = FALSE)
    }
    out <- merged.out

    # add the collected timing data
    out$times <- collected_times

    class(out) <- "icm"
  } # end of parallel execution

  return(out)
}


orig_icm.seiqhrf <- function(param, init, control) {
  crosscheck.icm(param, init, control)
  verbose.icm(control, type = "startup")
  nsims <- control$nsims
  ncores <- ifelse(control$nsims == 1, 1, min(future::availableCores(), control$ncores))
  control$ncores <- ncores

  if (ncores == 1) {

    # Simulation loop start
    for (s in 1:control$nsims) {

      ## Initialization module
      if (!is.null(control[["initialize.FUN"]])) {
        dat <- do.call(control[["initialize.FUN"]], list(param, init, control))
      }


      # Timestep loop
      for (at in 2:control$nsteps) {

        ## User Modules
        um <- control$user.mods
        if (length(um) > 0) {
          for (i in 1:length(um)) {
            dat <- do.call(control[[um[i]]], list(dat, at))
          }
        }

        ## Infection
        if (!is.null(control[["infection.FUN"]])) {
          dat <- do.call(control[["infection.FUN"]], list(dat, at))
        }


        ## Recovery
        if (!is.null(control[["recovery.FUN"]])) {
          dat <- do.call(control[["recovery.FUN"]], list(dat, at))
        }


        ## Departure Module
        if (!is.null(control[["departures.FUN"]])) {
          dat <- do.call(control[["departures.FUN"]], list(dat, at))
        }


        ## Arrival Module
        if (!is.null(control[["arrivals.FUN"]])) {
          dat <- do.call(control[["arrivals.FUN"]], list(dat, at))
        }


        ## Outputs
        if (!is.null(control[["get_prev.FUN"]])) {
          dat <- do.call(control[["get_prev.FUN"]], list(dat, at))
        }


        ## Track progress
        verbose.icm(dat, type = "progress", s, at)
      }

      # Set output
      if (s == 1) {
        out <- saveout.seiqhrf.icm(dat, s)
      } else {
        out <- saveout.seiqhrf.icm(dat, s, out)
      }
    } # Simulation loop end

    class(out) <- "icm"
  } # end of single core execution

  if (ncores > 1) {
    doParallel::registerDoParallel(ncores)

    sout <- foreach(s = 1:nsims) %dopar% {
      control$nsims <- 1
      control$currsim <- s

      ## Initialization module
      if (!is.null(control[["initialize.FUN"]])) {
        dat <- do.call(control[["initialize.FUN"]], list(param, init, control))
      }

      # Timestep loop
      for (at in 2:control$nsteps) {

        ## User Modules
        um <- control$user.mods
        if (length(um) > 0) {
          for (i in 1:length(um)) {
            dat <- do.call(control[[um[i]]], list(dat, at))
          }
        }

        ## Infection
        if (!is.null(control[["infection.FUN"]])) {
          dat <- do.call(control[["infection.FUN"]], list(dat, at))
        }


        ## Recovery
        if (!is.null(control[["recovery.FUN"]])) {
          dat <- do.call(control[["recovery.FUN"]], list(dat, at))
        }


        ## Departure Module
        if (!is.null(control[["departures.FUN"]])) {
          dat <- do.call(control[["departures.FUN"]], list(dat, at))
        }


        ## Arrival Module
        if (!is.null(control[["arrivals.FUN"]])) {
          dat <- do.call(control[["arrivals.FUN"]], list(dat, at))
        }


        ## Outputs
        if (!is.null(control[["get_prev.FUN"]])) {
          dat <- do.call(control[["get_prev.FUN"]], list(dat, at))
        }


        ## Track progress
        verbose.icm(dat, type = "progress", s, at)
      }

      # Set output
      out <- saveout.seiqhrf.icm(dat, s = 1)
      class(out) <- "icm"
      return(out)
    }

    # aggregate results collected from each thread
    collected_times <- list()

    # collect the times from sout then delete them
    for (i in 1:length(sout)) {
      collected_times[[paste0("sim", i)]] <- sout[[i]]$times$sim1
      sout[[i]]$times <- NULL
    }

    # merge $epi structures
    merged.out <- sout[[1]]
    for (i in 2:length(sout)) {
      merged.out <- merge.seiqhrf.icm(merged.out, sout[[i]], param.error = FALSE)
    }
    out <- merged.out

    # add the collected timing data
    out$times <- collected_times

    class(out) <- "icm"
  } # end of parallel execution

  return(out)
}
