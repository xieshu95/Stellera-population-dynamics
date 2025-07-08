library(ggplot2)

# parameters
time <- 50       # running time
r <- 0.3             # growth rate
K <- 10              # carrying capacity
R <- 0.7             # reproduction rates
death <- 0.005   # death rate
germ_prob <- 0.7     # germination probability

# creat list to record all alive individuals 
create_plant <- function(id, biomass = K * 0.6, age = 1) {
  list(
    id = id,
    biomass = biomass,
    age = age,
    alive = TRUE
  )
}

# initial state
population <- list(create_plant(1))
next_id <- 2

time_points <- c(0)
population_size <- c(1)
event_types <- c("Initial")

current_time <- 0
set.seed(123) 
# Loop
while (current_time < time && length(population) > 0) {
  # 1. calculate rates
  growth_rates <- numeric()
  reprod_rates <- numeric()
  death_rates <- numeric()
  
  for (plant in population) {
    # growth rates
    growth_rate <- r * plant$biomass * max(0, (1 - plant$biomass/K))
    growth_rates <- c(growth_rates, growth_rate)
    
    # reproduction rates
    if (plant$biomass >= K/2 & current_time > 1) {
      reprod_rate <- R * plant$biomass * germ_prob
    } else {
      reprod_rate <- 0
    }
    reprod_rates <- c(reprod_rates, reprod_rate)
    
    # death rates
    death_rate <- death * plant$age
    death_rates <- c(death_rates, death_rate)
  }
  
  # 2. total rates
  total_rate <- sum(growth_rates) + sum(reprod_rates) + sum(death_rates)
  if (total_rate <= 0) {
    cat("No events possible at time", current_time, "\n")
    break
  }
  
  # 3. waiting time
  dt <- rexp(1, rate = total_rate)
  
  # update time
  if (current_time + dt > time) {
    # final state
    time_points <- c(time_points, time)
    population_size <- c(population_size, length(population))
    event_types <- c(event_types, "End")
    break
  }
  
  current_time <- current_time + dt
  
  # 4. select event
  all_rates <- c(growth_rates, reprod_rates, death_rates)
  event_idx <- sample(1:length(all_rates), size = 1, prob = all_rates)
  
  # 5. update states
  n_plants <- length(population)
  if (event_idx <= n_plants) {
    # growth 
    plant_idx <- event_idx
    event_type <- "Growth"
    B0 <- population[[plant_idx]]$biomass 
    population[[plant_idx]]$biomass <- population[[plant_idx]]$biomass + r* B0 * (1- B0/K) * dt 
    
  } else if (event_idx <= 2 * n_plants) {
    # reproduction
    plant_idx <- event_idx - n_plants
    event_type <- "Reproduction"
    # add new individuals
    new_plant <- create_plant(next_id, biomass = 0.1, age = 0)
    next_id <- next_id + 1
    population <- c(population, list(new_plant))
    
  } else {
    # death
    plant_idx <- event_idx - 2 * n_plants
    event_type <- "Death"
    
    # move individuals
    population[[plant_idx]]$alive <- FALSE
    population <- population[sapply(population, function(p) p$alive)]
  }
  
  # update age for all individuals
  for (i in seq_along(population)) {
    population[[i]]$age <- population[[i]]$age + dt
  }
  
  # record current time
  time_points <- c(time_points, current_time)
  population_size <- c(population_size, length(population))
  event_types <- c(event_types, event_type)
}

# plot time VS population size
plot_data <- data.frame(
  Time = time_points,
  Population = population_size,
  Event = factor(event_types)
)

ggplot(plot_data, aes(x = Time, y = Population)) +
  geom_step(color = "darkgreen", linewidth = 1) +
  geom_point(aes(color = Event), size = 3, alpha = 0.8) +
  scale_color_manual(values = c("Growth" = "blue", "Reproduction" = "red", 
                                "Death" = "black", "Initial" = "purple", "End" = "orange")) +
  labs(title = "Population size dynamics (Gillespie)",
       x = "Time (Year)", y = "Population size") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Events
event_table <- table(plot_data$Event[-1])  
cat("\nEvents:\n")
print(event_table)

# population statistics
if (length(population) > 0) {
  biomasses <- sapply(population, function(p) p$biomass)
  ages <- sapply(population, function(p) p$age)
  
  cat("\nFinal population:\n")
  cat("Number of individuals:", length(population), "\n")
  cat("Mean biomass:", mean(biomasses), "\n")
  cat("Maximum biomass:", max(biomasses), "\n")
  cat("Mean age:", mean(ages), "\n")
} else {
  cat("\nExtinct on the", round(current_time, 2), "th year\n")
}
