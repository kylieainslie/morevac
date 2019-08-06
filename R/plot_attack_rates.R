### MoReVac - Modelling Repeat Vaccination ###
### Agent-based model of repeat vaccination in birth cohort

#' Plot multi-annual attack rates
#'
#' This function initializes the population before running the model.
#' @param dat data frame of annual attack rates. Should have two colums: Year and Attack_Rate.
#'             If by_vac=TRUE, dat should have a third column: Vac_Strategy.
#' @param by_vac logical. if TRUE then plot by vaccination strategy.
#' @param c_bands logical. Plot confidence bands or not. Must have columns named Lower and Upper.
#' @param y_max maximum value for y-axis.
#' @param no_legend logical. if TRUE legend will be suppressed.
#' @param legend_x x position of legend.
#' @param legend_y y position of legend.
#' @return plot of multi-annual attack rates with vertical dashed line indicating year of start of
#'         vaccination and a horizontal dashed line indicating mean attack rate prior to start of vaccination.
#' @keywords morevac
#' @export

plot_attack_rates <- function(dat, by_vac = FALSE, by_age_group = FALSE, c_bands = FALSE, y_max = 0.5,
                              no_legend = FALSE, legend_x = 0.95, legend_y = 0.95){
  if(by_vac ==TRUE && by_age_group == TRUE){stop("Please select only one group to plot by (by_vac or by_age_group)")}
  years <- unique(dat$Year)

  if (by_vac == FALSE && by_age_group == FALSE){
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
  } else if (by_vac == TRUE && by_age_group == FALSE) {
      if (no_legend) {
        if (c_bands){
          p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
                geom_line() +
                geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
                xlab('Year') +
                ylab('Attack Rate') +
                scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = "none"
                     )
        } else {
          p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
                geom_line() +
                xlab('Year') +
                ylab('Attack Rate') +
                scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = "none"
                      )
        }
      } else if (by_vac == FALSE && by_age_group == TRUE) {
        if (c_bands){
          p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= Age_Group)) +
                geom_line() +
                geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Age_Group),alpha=0.2)+
                xlab('Year') +
                ylab('Attack Rate') +
                scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      legend.position = c(legend_x, legend_y),
                      legend.justification = c("right", "top"),
                      legend.box.just = "right",
                      legend.margin = margin(6, 6, 6, 6),
                      legend.key = element_rect(fill = "white"))
          } else {
            p1 <- ggplot(data = dat, aes(x = Year, y = Attack_Rate, colour= Age_Group)) +
                  geom_line() +
                  xlab('Year') +
                  ylab('Attack Rate') +
                  scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = c(legend_x, legend_y),
                        legend.justification = c("right", "top"),
                        legend.box.just = "right",
                        legend.margin = margin(6, 6, 6, 6),
                        legend.key = element_rect(fill = "white"))
        }
      }

  }

  return(p1)
}

