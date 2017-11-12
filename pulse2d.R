#Make a figure that mimics 5-3 in Or et al 2009:

pulse2d <- function(pulse_conc = 0.5, 
                    pulse_time = 5,
                    time_limits = c(0, 100), 
                    L = 100, 
                    v = 2, 
                    R = 1, 
                    D1 = 0.5, 
                    D2 = 1, 
                    D3 = 10){
  
  require(tidyverse)
  require(viridis)
  require(ggthemes)
  require(data.table)
  
  D <- c(D1, D2, D3)
  
  time_range <- seq(time_limits[1], time_limits[2], length.out = 100)
 
  output <- data.frame(time = time_range)
  
  #Jury et al's solution to solute pulse transport (eq 196):
  #Should boundaries be adjusted?
  A <- pulse_conc*pulse_time #think about units: (g/g)*sec?
  
  for(i in 1:length(D)){
    df <- output %>%
      mutate(p1 = A*L*sqrt(R)/(2*sqrt(pi*D[i]*(time^3)))) %>%
      mutate(p2 = exp(-((R*L - v*time)^2)/(4*R*D[i]*time))) %>%
      mutate(c_t = p1*p2) %>%
      dplyr::select(c_t)
    output <- bind_cols(output, df)
    names(output)[i + 1] <- paste0("d", D[i])
  }
  
  plot_dat <- output %>%
    data.table::melt(id.vars = "time") %>%
    rename("c_t" = "value", "D" = "variable") %>%
    mutate(D = as.numeric(gsub("d", "", D))) %>%
    mutate(c_co = c_t/pulse_conc) %>%
    mutate(dimensionless_time = v*time/L) %>%
    mutate(peclet = as.factor(v*L/D))
  
  #Plot annotation preparation
  #peclet <- v*L/D
  #y_val <- 0.75*max(plot_dat$c_co, na.rm = TRUE)
  #x_val <- 0.1*max(plot_dat$dimensionless_time, na.rm = TRUE)

  p <- ggplot(plot_dat, aes(x = dimensionless_time, y = c_co, group = peclet, color = peclet)) + 
          geom_line(size = 1) + 
          #annotate("text", x = x_val, y = y_val, 
           #        label = paste0("Peclet Number = ", peclet),
            #      size = 5, hjust = 0) +
          theme_few() +
          labs(x = "Dimensionless time (T = vt/L)",
               y = "Relative concentration (C/Co)",
               color = "Peclet \nnumber",
               caption = "Peclet number: P = vL/D") +
          theme(panel.grid.minor = element_line(color = "grey95"),
                panel.grid.major = element_line(color = "grey85"),
                plot.caption = element_text(hjust = 0))
  p
  
  return(p)
  
}
