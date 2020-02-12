### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Plot multi-annual attack rates
#'
#' This function initializes the population before running the model.
#' @param dat data frame of annual attack rates. Should have two colums: Age and Life_Inf.
#'            If by_vac=TRUE then there should be a third column: Vac_Strategy.
#' @param by_vac Logical. If true, lifetime infections will be plotted by vaccintion strategy.
#' @param no_legend logical. if TRUE legend will be suppressed.
#' @param x x-coordinate position of the legend.
#' @param y y-coordinate postion of the legend
#' @return plot of lifetime infections by age.
#' @keywords morevac
#' @export

plot_lifetime_infections <- function(dat, by_vac = FALSE, no_legend = FALSE, x = 0.3, y = 0.95){

  if (by_vac){
    if (no_legend){
      p1 <- ggplot(dat, aes(x = Age, y = Lifetime_Infections,fill = Vac_Strategy)) +
        geom_boxplot() +
        ylab('Number of Lifetime Infections') +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = "none"
        )
    } else {
      p1 <- ggplot(dat, aes(x = Age, y = Lifetime_Infections,fill = Vac_Strategy)) +
        geom_boxplot() +
        ylab('Number of Lifetime Infections') +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = c(x, y),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6),
              legend.key = element_rect(fill = "white")
        )
    }

  } else{
    p1 <- ggplot(dat, aes(x = Age, y = Lifetime_Infections)) +
          geom_boxplot() +
          ylab('Number of Lifetime Infections') +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black")
          )

  }
  return(p1)
}
