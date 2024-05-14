
#1) Read csv and create a blicc data object with default settings
#2) Allow edits of priors for M/K, Linf, F/K, Galpha, and NBphi
#3) Allow "informative" option reducing sd for lognormal for above priors
#4) Allow log slope option for selectivity prior
#5) Allow "informative" option for selectivity parameters
#6) Allow edit of relative catches among gears


library("fishblicc")

# The following are functions to help manipulate the fishblicc data object

#' Possible selectivity models
#'
select_types <- c("Logistic", "Normal",
                  "Single-side Normal", "Double-sided Normal")


#' Calculates the mortality from all selectivities not being edited
#'
#' @param blicc_ld the data list being edited
#' @param ed_seln  the index for the selectivity function being edited
#' @param ed_gear  the index for the gear being edited
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
                               fishblicc::Rsel_logistic(Sm[Indx], LMP),
                               fishblicc::Rsel_normal(Sm[Indx], LMP),
                               fishblicc::Rsel_ssnormal(Sm[Indx], LMP),
                               fishblicc::Rsel_dsnormal(Sm[Indx], LMP)))
    }
  }

  gear <- integer(0)
  wt <- double(0)
  gFk <- double(0)
  for (gi in 1:blicc_ld$NG) {
    if (blicc_ld$GSbase[gi]==ed_seln) {
      gear <- c(gear, gi)
      wt <- c(wt, 1)
      if (blicc_ld$Fkg[gi] > 0) gFk <- with(blicc_ld, c(gFk, exp(polFkm[Fkg[gi]]))) else gFk <- c(gFk, 0)
      GSki[[gi]] <- double(blicc_ld$NB)
    } else
      GSki[[gi]] <- Ski[[blicc_ld$GSbase[gi]]]

    gii <- 1L + (gi-1L)*2L
    if (blicc_ld$GSmix1[gii] > 0) {
      for (si in blicc_ld$GSmix1[gii]:blicc_ld$GSmix1[gii+1L])
        if (blicc_ld$GSmix2[si] == ed_seln) {
          gear <- c(gear, gi)
          wt <- c(wt, Sm[blicc_ld$NP+si])
          if (blicc_ld$Fkg[gi] > 0) gFk <- with(blicc_ld, c(gFk, exp(polFkm[Fkg[gi]]))) else gFk <- c(gFk, 0)
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
              weight=wt, Fk=gFk,
              fSel = blicc_ld$fSel[ed_seln], LMP=blicc_ld$LMP,
              GSki = GSki[[ed_gear]]))
}


#' Calculates the expected count length frequency
#'
#' @param blicc_ld the data list being edited
#' @param mort list of outputs from mortality function
#' @param spar selectivity parameters for the current selectivity component
#' @param wt   weight for the current component
#' @param N    Sample size used to scale calculated values
#'
expect_freq <- function(blicc_ld, mort, spar, wt, N) {
  Zki <- mort$Zk
  sel <- with(mort,
              switch(fSel,
                     Rsel_logistic(spar, LMP),
                     Rsel_normal(spar, LMP),
                     Rsel_ssnormal(spar, LMP),
                     Rsel_dsnormal(spar, LMP)))
  # Update Zki with this selectivity component
  mort$weight[mort$ed_indx] <- wt
  for (i in seq(mort$weight))
    if (mort$Fk[i] > 0) Zki <- Zki + mort$Fk[i] * mort$weight[i] * sel

  pop <- with(blicc_ld, fishblicc::Rpop_len(gl_nodes, gl_weights, LLB, Zki,
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

#' Get the selectivity parameters for selectivity function sel_i
#'
#' @inheritParams get_sel_wt
#'
get_sel_par <- function(ld, sel_i) {
  if (sel_i==0) return(0)
  return(with(ld, polSm[sp_i[sel_i]:sp_e[sel_i]]))
}


#' Get the selectivity weight for a gear and a selectivity function
#'
#' @param ld the blicc data list being edited
#' @param sel_i integer index of the selectivity component in ld
#' @param gear_i integer index for the gear
#'
get_sel_wt <- function(ld, gear_i, sel_i) {

  if (gear_i==0 | sel_i==0) return(0)
  if (ld$GSbase[gear_i]==sel_i) return(1)  # base component

  res <- 0
  si <- 2L*gear_i-1L
  if (ld$GSmix1[si] > 0) {
    selfun <- with(ld, GSmix2[GSmix1[si]:GSmix1[si+1L]])
    res <- with(ld, polSm[NP+match(sel_i, selfun)])
  }
  return(res)
}


#' Set the selectivity weight for a gear and a selectivity function
#'
#' @inheritParams get_sel_wt
#' @param new_wt new weight value for the function
#'
set_sel_wt <- function(ld, gear_i, sel_i, new_wt) {
  si <- 2L*gear_i-1L
  if (ld$GSmix1[si] > 0) {
    selfun <- with(ld, GSmix2[GSmix1[si]:GSmix1[si+1L]])
    ld$polSm[ld$NP+match(sel_i, selfun)] <- log(new_wt)
  }
  return(ld)
}

#' Identifies weights associated with gear/selectivity function
#'
#' @inheritParams get_sel_wt
#'
hash_wt <- function(ld) {
  wt <- double(0)
  hash <- pi <- integer(0)
  for (gi in 1:ld$NG) {
    si <- gi*2L-1L
    if (ld$GSmix1[si] > 0) {
      pii <- ld$GSmix1[si]:ld$GSmix1[si+1L]
      hash <- c(hash, gi*1000L + ld$GSmix2[pii])
      pi <- c(pi, pii)
      wt <- c(wt, ld$polSm[ld$NP+pii])
    }
  }
  return(list(hash, pi, wt))
}

#' Retrieves selectivity component indices for a single gear
#'
#' @inheritParams get_sel_wt
#'
get_sel_indices <- function(ld, gear_i) {
  if (is.null(ld)) return(0)
  si <- 2L*gear_i-1L
  if (ld$GSmix1[si] > 0)
    return(with(ld, c(GSbase[gear_i], GSmix2[GSmix1[si]:GSmix1[si+1L]])))
  else
    return(ld$GSbase[gear_i])
}


