# initial parameters
years <- 50       # running time
r <- 0.2          # growth rates
K <- 10           # carrying capacity
R <- 0.5          # reproduction rates
germ_prob <- 0.7  # germination probability
death <- 0.005 # death rate

# initial state (a single individual)
plants <- data.frame(
  id = 1,
  biomass = K * 0.6,  # initial biomass
  age = 1,
  alive = TRUE
)

# record population dynamics
pop_size <- numeric(years)
pop_size[1] <- 1

# Loop
set.seed(123)  
for (year in 2:years) {
  if (nrow(plants) == 0) {
    pop_size[year:years] <- 0 # To avoid empty population
    break
  }
  
  # 1. growth process
  plants$biomass <- plants$biomass + r * plants$biomass * (1 - plants$biomass/K)
  plants$age <- plants$age + 1
  
  # 2. reporoduction process
  new_seeds_count <- 0
  adults <- plants[plants$biomass >= K/2 & plants$age > 2, ]
  
  if (nrow(adults) > 0) {
    # seed/offspring generated every year
    new_seeds_count <- sum(floor(R * adults$biomass))
  }
  
  # 3. germination of seed -- generate new individuals
  if (new_seeds_count > 0) {
    germinated <- rbinom(1, new_seeds_count, germ_prob)
    if (germinated > 0) {
      new_plants <- data.frame(
        id = max(plants$id) + 1:germinated,
        biomass = 0.1,  # biomass of each new individual
        age = 0,
        alive = TRUE
      )
      plants <- rbind(plants, new_plants)
    }
  }
  
  # 4. death process (age-dependent)
  death_prob <- death * plants$age 
  plants$alive <- runif(nrow(plants)) > death_prob
  plants <- plants[plants$alive, ]
  
  # update population size
  pop_size[year] <- nrow(plants)
  cat("Year:", year, "Population:", pop_size[year], "\n")
}

# plot population size dynamics through time
plot(1:years, pop_size, type = "l", col = "darkgreen", lwd = 2,
     xlab = "Time (Year)", ylab = "Population Size")
grid()

# 添加种群统计信息
if (pop_size[years] > 0) {
  cat("Final population size:", pop_size[years], "\n")
  cat("Maximum biomass:", max(plants$biomass), "\n")
  cat("Average age:", mean(plants$age), "\n")
} else {
  cat("Extinction on the year", which(pop_size == 0)[1], "\n")
}

