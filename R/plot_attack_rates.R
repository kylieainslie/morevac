### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Plot multi-annual attack rates
#'
#' This function initializes the population before running the model.
#' @param dat data frame of annual attack rates. Should have two colums: Year and Attack_Rate.
#'             If by_age=TRUE, dat should have a third column: Age_Group.
#' @param by Name of variable you want to plot attack rates by.
#' @param c_bands logical. Plot confidence bands or not. Must have columns named Lower and Upper.
#' @param y_max maximum value for y-axis.
#' @return plot of multi-annual attack rates with vertical dashed line indicating year of start of
#'         vaccination and a horizontal dashed line indicating mean attack rate prior to start of vaccination.
#' @keywords morevac
#' @export

plot_attack_rates <- function(dat, by = NULL, c_bands = FALSE, y_max = 0.5){

  years <- unique(dat$Year)

  if (is.null(by)){
    if (c_bands) {
      p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate)) +
            geom_line() +
            geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA),alpha=0.2)+
            xlab('Year') +
            ylab('Attack Rate') +
            scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))
    } else {
      p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate)) +
            geom_line() +
            scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
            xlab('Year') +
            ylab('Attack Rate') +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))
        }
  } else if (!is.null(by)) {
      if (c_bands){
        p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= by)) +
              geom_line() +
              geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=by),alpha=0.2)+
              xlab('Year') +
              ylab('Attack Rate') +
              scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.position = c(.95, .95),
                    legend.justification = c("right", "top"),
                    legend.box.just = "right",
                    legend.margin = margin(6, 6, 6, 6),
                    legend.key = element_rect(fill = "white"))
      } else {
        p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= by)) +
              geom_line() +
              xlab('Year') +
              ylab('Attack Rate') +
              scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.position = c(.95, .95),
                    legend.justification = c("right", "top"),
                    legend.box.just = "right",
                    legend.margin = margin(6, 6, 6, 6),
                    legend.key = element_rect(fill = "white"))
      }
  }

  return(p1)
}

