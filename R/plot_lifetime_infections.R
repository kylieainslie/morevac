### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Plot multi-annual attack rates
#'
#' This function initializes the population before running the model.
#' @param dat data frame of annual attack rates. Should have two colums: Age and Life_Inf.
#' @param by Name of variable you want to plot attack rates by.
#' @return plot of lifetime infections by age.
#' @keywords morevac
#' @export

plot_lifetime_infections <- function(dat, by = NULL){

  p1 <- ggplot(dat, aes(x = Age, y = Life_Inf,fill = by)) +
        geom_boxplot() +
        ylab('Number of Lifetime Infections') +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = c(.25, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6),
              legend.key = element_rect(fill = "white")
        )

  return(p1)
}
