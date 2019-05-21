### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort


#' Susceptibility function
#'
#' This function initializes the population before running the model.
#' @param nindiv Number of individuals in the simulated population. Defaults to 1000.
#' @param years Number of years to simulate over. Defaults to 200.
#' @param maxage Maximum age of individuals after which they die and are removed from the population. Defaults to 80.
#' @return List containing initialized infection history matrix, vaccination history matrix, susceptibility matrix, attack rate vector, and incidence vector.
#' @keywords morevac
#' @export
#' @examples
#' suscept_func()
suscept_func <- function(inf_history, vac_history, ve, drift_x, drift_v, version = 1){
  # version = 1 is either-or
  # version = 2 is multiplicative

  #if(is.na(inf_history)){inf_history<-999}

  vac_ind <- 0
  if(vac_history == 0){vac_ind <- 1}
  # never infected
  if (drift_x > 0 & drift_v > 0){
    if (inf_history == 999){
      if (vac_history > 1/drift_v){ # never vaccinated or vaccinated long enough ago for drift to have diminished protection
        rtn <- 1
      } else if (vac_history<=1/drift_v){ # vaccinated this year or within last few years
        rtn <- (vac_ind[i,a]*ve) + (vac_history*drift_v)
      }
    }

  # infected and drift>0
    if (inf_history != 999){
      if (vac_history > 1/drift_v){ # never vaccinated
        if (inf_history < 1/drift_x){
         rtn <- inf_history*drift_x
        } else if (inf_history>= 1/drift_x) {
          rtn <- 1
        }
      } else if (vac_history<=1/drift_v){ # vaccinated
          if (version == 1){
            rtn <- min(inf_history*drift_x,(vac_ind*ve)+(vac_history*drift_v)) # either-or
          } else if (version == 2) {
            rtn <- inf_history*drift_x * (ve +(vac_history*drift_v)) # multiplicative
          }
      }
    }
  }
  # infected and drift=0
  if (drift_x==0 & drift_v==0){
    if (inf_history != 999){
      rtn <- 0
    }
  }

  return(rtn)

}

#suscept_mat[i,a] <- suscept_func(inf_history = x[i,a], vac_history = v[i,a], ve = mygamma, drift_x = delta_x, drift_v = delta_v)
