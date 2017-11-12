
#Function to model concentration of a solute at L and time t based on a pulse input. 
#Should maybe output a vector of concentrations at different times. 
pulse3d <- function(pulse_conc, 
                  pulse_time,
                  max_val = 0.1,
                  time_limits = c(0, 100), 
                  L_limits = c(0, 100),
                  v = 2, R = 1, D = 0.5,
                  value_limit = TRUE){
  #L is 30 cm, v is 2 cm/day, R is 1 (dimensionless).
  #For testing:
  # pulse_conc = 0.5; D = 0.5; pulse_time = 2
  
  require(tidyverse)
  require(viridis)
  require(ggthemes)
  
  time_range <- seq(time_limits[1], time_limits[2], length.out = 100)
  L_range <- seq(L_limits[1], L_limits[2], length.out = 100)
  
  output <- expand.grid(time_range, L_range) 
  names(output) <- c("time", "length")
  
  #Jury et al's solution to solute pulse transport (eq 196):
  #Should boundaries be adjusted?
  A <- pulse_conc*pulse_time #think about units: (g/g)*sec?
  output <- output %>%
    mutate(p1 = A*length*sqrt(R)/(2*sqrt(pi*D*(time^3)))) %>%
    mutate(p2 = exp(-((R*length - v*time)^2)/(4*R*D*time))) %>%
    mutate(c_l_t = p1*p2) %>%
    dplyr::select(-p1, -p2)
  
  output <- output %>% 
    mutate(exceeded_max = ifelse(c_l_t > max_val, 1, 0))
  
    p <- ggplot(output, aes(x = length, y = time, fill = c_l_t)) + 
      geom_raster() +
      scale_fill_viridis() +
      theme_few() +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) + 
      labs(x = "Distance (cm)", y = "Time (sec)", fill = "Solute \nconcentration") +
      theme(panel.border = element_blank())

 if(value_limit == TRUE){
   p <- p + geom_contour(aes(x = length, y= time, z = exceeded_max), 
                         color = "white", size = 0.5)
 }

  return(p)
  
  }