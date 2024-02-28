
library("fishblicc")

# The following are functions to help manipulate the fishblicc data object

#' Possible selectivity models
#'
select_types <- c("Logistic", "Normal",
                  "Single-side Normal", "Double-sided Normal")


#' Calculates the mortality from selectivities not being edited
#'
mortality <- function(blicc_ld, ed_seln, ed_gear) {

  Sm <- exp(blicc_ld$polSm)

  Ski <- list()
  GSki <- list()
  for (si in 1:blicc_ld$NS) {
    if (si==ed_seln) {
      Ski[[si]] <- double(blicc_ld$NB)
    } else {
      Indx <- with(blicc_ld, sp_i[si] : sp_e[si])
      Ski[[si]] <- with(blicc_ld,
                        switch(fSel[si],
                               Rsel_logistic(Sm[Indx], LMP),
                               Rsel_normal(Sm[Indx], LMP),
                               Rsel_ssnormal(Sm[Indx], LMP),
                               Rsel_dsnormal(Sm[Indx], LMP)))
    }
  }

  gear <- integer(0)
  wt <- double(0)
  Fk <- double(0)
  for (gi in 1:blicc_ld$NG) {
    if (blicc_ld$GSbase[gi]==ed_seln) {
      gear <- c(gear, gi)
      wt <- c(wt, 1)
      if (blicc_ld$Fkg[gi] > 0) Fk <- with(blicc_ld, c(Fk, exp(polFkm[Fkg[gi]]))) else Fk <- c(Fk, 0)
      GSki[[gi]] <- double(blicc_ld$NB)
    } else
      GSki[[gi]] <- Ski[[blicc_ld$GSbase[gi]]]
    gii <- 1L + (gi-1L)*2L
    if (blicc_ld$GSmix1[gii] > 0) {
      for (si in blicc_ld$GSmix1[gii]:blicc_ld$GSmix1[gii+1L])
        if (GSmix2[si] == ed_seln) {
          gear <- c(gear, gi)
          wt <- c(wt, Sm[NP+si])
          if (blicc_ld$Fkg[gi] > 0) Fk <- with(blicc_ld, c(Fk, exp(polFkm[Fkg[gi]]))) else Fk <- c(Fk, 0)
        } else
          GSki[[gi]] <- with(blicc_ld, GSki[[gi]] + Ski[[GSmix2[si]]] * Sm[NP+si])
    }
  }


  Zk <- with(blicc_ld, exp(polMkm)*M_L)
  for (gi in 1:length(GSki)) {
    if (blicc_ld$Fkg[gi] > 0)
      Zk <- with(blicc_ld, Zk + exp(polFkm[Fkg[gi]]) * GSki[[gi]])
  }

  return(list(ed_seln = ed_seln, ed_indx = which(ed_gear==gear),
              par_indx = with(blicc_ld, sp_i[ed_seln] : sp_e[ed_seln]),
              Zk = Zk, gear=gear,
              weight=wt, Fk=Fk,
              fSel = blicc_ld$fSel[ed_seln], LMP=blicc_ld$LMP,
              GSki = GSki[[ed_gear]]))
}

expect_freq <- function(blicc_ld, mort, spar, N) {
  Zki <- mort$Zk
  sel <- with(mort,
              switch(fSel,
                     Rsel_logistic(spar, LMP),
                     Rsel_normal(spar, LMP),
                     Rsel_ssnormal(spar, LMP),
                     Rsel_dsnormal(spar, LMP)))
  # Update Zki
  with(mort, {
    for (i in seq(weight))
      if (Fk[i]>0) Zki <- Zki + Fk[i] * weight[i] * sel
  })

  pop <- with(blicc_ld, Rpop_len(gl_nodes, gl_weights, LLB, Zki,
                                 exp(polGam), exp(polGam)/poLinfm))
  sel <- with(mort, weight[ed_indx]*sel*pop)

  gear_sel <- with(mort, (GSki*pop + sel))
  Adjust <- N / sum(gear_sel)
  gear_sel <- gear_sel*Adjust
  sel <- sel*Adjust
  return(data.frame(LMP = rep(mort$LMP, 2),
                    Freq = c(gear_sel, sel),
                    Selectivity = rep(c("Gear", "Component"), each=length(sel))))
}

# mort <- mortality(yft_ld, 4, 4)
#
# spar <- with(yft_ld, exp(polSm[mort$par_indx]))
# N <- sum(yft_ld$fq[[4]])
#
# expect <- expect_freq(yft_ld, mort, spar, N)


#' Get selectivity function definitions for a single gear in a list
#'
#' NOT USED
get_selectivity <- function(ld, gear) {
  sel <- list()
  sel[[1]] <- ld$fq[[gear]]
  sel[[2]] <- with(ld, list(
    si = GSbase[gear],
    wt = 1,
    selfun = fSel[GSbase[gear]],
    par = exp(polSm[sp_i[GSbase[gear]]:sp_e[GSbase[gear]]])
  ))
  i <- 3
  si <- 2L*gear-1L
  if (ld$GSmix1[si] > 0) {
    for (gi in ld$GSmix1[si]:ld$GSmix1[si+1L]) {
      sel[[i]] <- with(ld,
                       list(
                         si = GSmix2[gi],
                         wt = with(ld, exp(polSm[NP + gi])),
                         selfun = with(ld, fSel[GSmix2[gi]]),
                         par = with(ld, exp(polSm[sp_i[GSmix2[gi]]:sp_e[GSmix2[gi]]]))
                       ))
      i <- i + 1
    }
  }
  return(sel)
}




#' Set selectivity function definitions for a single gear in a list
#'
#' NOT USED
set_selectivity <- function(ld, gear, sel) {
  # This is a complicated function that will need to rework the data object
  # because the links between the selectivity functions and the functions
  # themselves may have changed
  sel_ind <- integer(length(sel)-1)
  wt <- double(length(sel)-1)
  selfun <- sel_ind

  for (i in 2:length(sel)) {
    sel_ind[i-1] <- sel[[i]]$si
    wt[i-1] <- sel[[i]]$wt
    selfun[i-1] <- sel[[i]]$selfun
  }
  ld <- blicc_selfun(ld, sel_ind, selfun)
  ld <- blicc_gear_sel(ld, gear_sel = list(sel_ind), gear)
  if (length(sel_ind) > 1) ld <- blip_mix(ld, gear, mix_wt = wt)
  for (i in 2:length(sel)) {
    si <- sel[[i]]$si
    ld$polSm[ld$sp_i[si]:ld$sp_e[si]] <- log(sel[[i]]$par)
  }

  return(ld)
}

#' Number of selectivities associated with a gear
#'
NoofSelect <- function(ld, gear) {
  si <- 2L*gear-1L
  if (ld$GSmix1[si] > 0) {
    N <- 2L + ld$GSmix1[si] - ld$GSmix1[si+1L]
  } else {
    N <- 1L
  }
  return(N)
}

#' Get model function index from a slider reference
#'
SelectivityN <- function(ld, gear, N) {
  N <- 1
  si <- 2L*gear-1L
  if (N==1 | ld$GSmix1[si] == 0) {
    return(ld$GSbase[gear])
  } else {
    mix_sel <-  (blicc_ld$GSmix1[si]:blicc_ld$GSmix1[si+1L])[N-1L]
    return(blicc_ld$GSmix2[mix_sel])
  }
}

