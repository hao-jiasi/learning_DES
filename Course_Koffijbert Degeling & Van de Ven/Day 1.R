#////////////////////////////////////////////////////////////////////////#
#========================================================================#
# Course DES from Koffijbert, Degeling & van de Ven
# DAY 1
# LAST UPDATE: 2025 Oct 21
# AUTHOR: Jiasi Hao
#========================================================================#
#////////////////////////////////////////////////////////////////////////#
### Documentation
# https://r-simmer.org/
# vignette(package = "simmer")
# unit of time: unitless, as long as the user keep it consistent across the model (i.e., can be hours, days, months or years of choice)

### Note
# Search for [xx]: Missing, Input, etc
# [IMP] important!!!


rm(list = ls()); gc() # good to use after removing very large objects
Sys.setenv(LANG = "en")

### =========================================== ###
### ====== ====== Global settings ====== ====== ### ######
### =========================================== ###
#== Package required ######
invisible(lapply(c("rstudioapi",
                   "dplyr", "tidyverse",
                   "simmer",
                   # extention for simmer
                   "simmer.plot", "simmer.bricks"
                   # "mrgsim" # similar but specialize in pharmacokinetics/pharmacodynamics
                   
                   #"foreign","haven","readr","readxl","openxlsx",
                   #"tidyr","naniar","crayon","RColorBrewer","scales",
                   #"tableone",
                   #"lubridate","survival","muhaz","flexsurv","survminer",
                   #"stringr","car","ggplot2","purrr","gridExtra"
                   ), 
                 function(pkg) {
                   outdated <- old.packages()[, "Package"]
                   if (!require(pkg, character.only = TRUE)) {
                     install.packages(pkg, dependencies = TRUE) # install if not installed
                   } else if (pkg %in% outdated) {
                     message(sprintf("Updating package: %s", pkg)) # update if installed but outdated
                     install.packages(pkg)
                   }
                   require(pkg, character.only = TRUE) # library all needed packages
                   }))

#== Path ######
setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
getwd()
#/////////////////////////////////////////////////////////////////////////# ######


### ====================================================== ###
### ====== ====== DES initiation & first run ====== ====== ### ######
### ====================================================== ###
### The following as very basic

#== init_: define trajectory ######
traj <- trajectory(name = "traj") %>%
  # seize the "doctor" resource
  seize(resource = "doctor", amount = 1) %>%
  # delay the "patient" entity for the duration of a consultation; to implement a "delay", it is a time duration!! # to read more: ?timeout
  timeout(task = 15) %>%
  # release the "doctor" resource
  release(resource = "doctor", amount = 1);

plot(traj)

#== init_: define simulation environment ######
sim <- simmer(name = "sim") %>%
  # add 2 "doctor" resources to the environment
  add_resource(name = "doctor", capacity = 2) %>% # resource name
  # add the "patient" entities
  add_generator(
    # entity ID
    name_prefix = "patient",
    # traj followed (as defined above)
    trajectory = traj,
    # the interval & time of new entity arrival
    # if random, can specify: e.g., rexp(1, rate=0.2) which is exponentially distributed random times
    # ?at (can also be from, to, from_to, when_activated)
    distribution = at(rep(x = 0, times = 100)), # meaning: 100 entities all enter at T=0
    # monitoring level
    mon = 2 # typically 2-detailed (tracking attributes), as 0-no & 1-simple
  )

#== 1st run: of the model above ######
reset(.env = sim) # reset to init_ states
run(.env = sim) # run
now(.env = sim) # observe at what time the simulation was completed

# Alternative
sim %>% reset() %>% run() # this: only less informative output
sim %>% now()
#/////////////////////////////////////////////////////////////////////////# ######


### ======================================== ###
### ====== ====== Expand model ====== ====== ### ######
### ======================================== ###

#== expand from the basic model ######
#> traj
traj <- trajectory(name = "traj") %>%
  
  # "nurse" intake
  seize(resource = "nurse", amount = 1) %>%
  timeout(task = 20) %>%
  release(resource = "nurse", amount = 1) %>%
  
  # "doctor" consultation
  seize(resource = "doctor", amount = 1) %>%
  timeout(task = 15) %>%
  release(resource = "doctor", amount = 1)

plot(traj)

#> sim env
sim <- simmer(name = "sim") %>%
  
  # total nurse = 3
  add_resource(name = "nurse", capacity = 3) %>%
  # total doctor = 2
  add_resource(name = "doctor", capacity = 2) %>%
  # entity
  add_generator(name_prefix = "patient",
                trajectory = traj,
                distribution = at(rep(x = 0, times = 100)),
                mon = 2);

#== branching the trajectory ######
#> define params & func to handle the roll back
p_second_doctor_visit <- 0.4

fun_rollback_doctor <- function(n_doctor_visits) {
  # if the patient already visited two times, no additional visit should occur
  if (n_doctor_visits == 2) {
    another_visit <- FALSE;
    # otherwise, sample whether a second visit occurs
  } else if (n_doctor_visits == 1) {
    another_visit <- if(runif(1) < p_second_doctor_visit) {TRUE} else {FALSE} # runif() random number from 0 to 1 from uniform distribution
  }
  # return the outcome
  return(another_visit)
}

#> refine definition of traj
traj <- trajectory(name = "traj") %>%
  
  # "nurse" intake
  seize(resource = "nurse", amount = 1) %>%
  timeout(task = 20) %>%
  release(resource = "nurse", amount = 1) %>%
  
  # "doctor" consultation
  # 1) branch to reflect that 20% of "entity" (aka pt) do not need to see a doctor after the intake
  branch(option = function() sample(x = c(0, 1), size = 1, prob = c(0.20, 0.80)),
         continue = c(T),
         
         # 2) for the 80%, consultation takes place
         trajectory(name = "traj_doctor") %>%
           
           seize(resource = "doctor", amount = 1) %>%
           timeout(task = 15) %>%
           release(resource = "doctor", amount = 1) %>%
           
           # attribute: to count the number of visits (>> this is for individual entity, serving as an input for fun_rollback_doctor below)
           set_attribute(key = "n_doctor_visits", value = 1, mod = "+") %>%
           
           # 3) rollback for a potential second visit
           rollback(target = 4, # old argument 'amount': both mean "the trajectory step you want to roll back to" (i.e., how far back to go)
                    check = function() {
                      fun_rollback_doctor(n_doctor_visits = get_attribute(.env = sim,
                                                                          keys = "n_doctor_visits"))
                    })
  )

plot(traj)

#> refine definition of traj (alternative: for cleaner view)
# 1) define external function for branching (20/80 split)
fun_see_doctor <- function() {
   out <- sample(x = c(0, 1), size = 1, prob = c(0.20, 0.80))
   return(out)
}
fun_see_doctor() # can try the function by calling it a cew times

# 2) define sub-traj "traj_doctor"
traj_doctor <- trajectory(name = "traj_doctor") %>%
  seize(resource = "doctor", amount = 1) %>%
  timeout(task = 15) %>%
  release(resource = "doctor", amount = 1) %>%
  
  set_attribute(key = "n_doctor_visits", value = 1, mod = "+") %>%
  
  rollback(target = 4,
           check = function() {
             fun_rollback_doctor(n_doctor_visits = get_attribute(.env = sim,
                                                                 keys = "n_doctor_visits"))
           })

# 3) define high-level overall traj
traj <- trajectory(name = "traj") %>%
  
  # "nurse"
  seize(resource = "nurse", amount = 1) %>%
  timeout(task = 20) %>%
  release(resource = "nurse", amount = 1) %>%
  
  # "doctor"
  branch(option = function() fun_see_doctor(), # call func to define 20/80 HERE
         continue = c(T),
         
         # add sub-traj "traj_doctor" HERE
         traj_doctor
  )

plot(traj)

#> run sim
set.seed(123)
sim %>% reset() %>% run()
sim %>% now()

#== implement patient-level variation ######
#> define parametric time-to-event distribution
data_time_nurse <- c(10, 4, 27, 39, 18, 31, 25, 27, 18, 18, 20, 23,
                     17, 19, 12, 30, 9, 4, 12, 19, 24, 33, 7, 17, 28);
data_time_doctor <- c(3, 13, 20, 6, 16, 34, 18, 26, 29, 41, 27, 21,
                      5, 22, 16, 7, 17, 18, 28, 2, 20, 17);

#> distribution fitted to data
weibull_time_nurse <- c(shape = 2.325996, scale = 22.137460)
weibull_time_doctor <- c(shape = 1.887695, scale = 20.677025) # where the number comes (i.e. how fitted) see later course [question]

#> perform a simple simulation of hypothetical patients to assess the fit
# 10,000 sim for "nurse"
samples_time_nurse <- rweibull(n = 10000,
                               shape = weibull_time_nurse["shape"],
                               scale = weibull_time_nurse["scale"])
# 10,000 sim for "doctor"
samples_time_doctor <- rweibull(n = 10000,
                                shape = weibull_time_doctor["shape"],
                                scale = weibull_time_doctor["scale"])

#> compare the observed mean minutes spent to the simulated mean minutes spent
c(nurse_observed = mean(data_time_nurse), nurse_sampled = mean(samples_time_nurse))
c(doctor_observed = mean(data_time_doctor), doctor_sampled = mean(samples_time_doctor))

#> visualization
par(mfrow = c(1, 2));
# "nurse" time
hist(data_time_nurse, prob = TRUE, xlim = c(0, 60));
lines(density(samples_time_nurse), col = "darkred", lwd = 3);
legend("topright", legend = c("observed", "sampled"),
       col = c("black", "darkred"), lty = 1, lwd = 2, bty = 'n');
# "doctor" time
hist(data_time_doctor, prob = TRUE, xlim = c(0, 60));
lines(density(samples_time_doctor), col = "darkred", lwd = 3);
legend("topright", legend = c("observed", "sampled"),
       col = c("black", "darkred"), lty = 1, lwd = 2, bty = 'n');

#> refine the definition of traj
traj <- trajectory(name = "traj") %>%
  # "nurse"
  seize(resource = "nurse", amount = 1) %>%
  timeout(task = function() rweibull(n = 1,
                                     shape = weibull_time_nurse["shape"],
                                     scale = weibull_time_nurse["scale"])
  ) %>%
  release(resource = "nurse", amount = 1) %>%
  
  # "doctor"
  branch(option = function() sample(x = c(0, 1), size = 1, prob = c(0.20, 0.80)),
         continue = c(T),
         
         trajectory(name = "traj_doctor") %>%
           seize(resource = "doctor", amount = 1) %>%
           timeout(
             task = function() rweibull(n = 1,
                                        shape = weibull_time_doctor["shape"],
                                        scale = weibull_time_doctor["scale"])
           ) %>%
           release(resource = "doctor", amount = 1)
  )

plot(traj) # this one does not have rollback

#> run
set.seed(123)
sim %>% reset() %>% run()
sim %>% now()

#== use attributes within ######
#> define functions to sample from the distribution
fun_time_nurse <- function() rweibull(n = 1,
                                      shape = weibull_time_nurse["shape"],
                                      scale = weibull_time_nurse["scale"])
fun_time_doctor <- function() rweibull(n = 1,
                                       shape = weibull_time_doctor["shape"],
                                       scale = weibull_time_doctor["scale"])
fun_see_doctor <- function() sample(x = c(0, 1), size = 1, prob = c(0.20, 0.80))

#> define traj
traj <- trajectory(name = "traj") %>%
  
  # "nurse"
  seize(resource = "nurse", amount = 1) %>%
  
  set_attribute(keys = "time_nurse", value = function() fun_time_nurse()) %>%
  timeout(task = function() get_attribute(.env = sim, keys = "time_nurse")) %>%
  
  release(resource = "nurse", amount = 1) %>%
  
  # "doctor" 20/80 split
  set_attribute(keys = "see_doctor", values = function() fun_see_doctor()) %>%
  branch(option = function() get_attribute(.env = sim, keys = "see_doctor"),
         continue = c(T),
         
         # "doctor" consultation
         trajectory(name = "traj_doctor") %>%
           seize(resource = "doctor", amount = 1) %>%
           
           set_attribute(keys = "time_doctor", value = function() fun_time_doctor()) %>%
           timeout_from_attribute(key = "time_doctor") %>%
           
           release(resource = "doctor", amount = 1)
  )

plot(traj)

#> run
sim <- simmer(name = "sim") %>%
  add_resource(name = "nurse", capacity = 3) %>%
  add_resource(name = "doctor", capacity = 2) %>%
  add_generator(name_prefix = "patient",
                trajectory = traj,
                distribution = at(rep(x = 0, times = 100)),
                mon = 2) 

set.seed(123)
sim %>% reset() %>% run()
#/////////////////////////////////////////////////////////////////////////# ######


### ======================================================== ###
### ====== ====== Observe monitored attributes ====== ====== ### ######
### ======================================================== ###
#== extract data from the simulation ######
mon_arrivals <- get_mon_arrivals(.envs = sim, per_resource = TRUE)
mon_resources <- get_mon_resources(.envs = sim)
mon_attributes <- get_mon_attributes(.envs = sim)

# examination
head(mon_arrivals); dim(mon_arrivals)
# mon_arrivals %>%
#   arrange(name) %>%
#   View() 
# [IMP]
# explanation: nrow(mon_arrivals) != 100 (pt)
# because the by per row is the n(entity) x n(event happened); therefore, it is not exactly 100 OR 100*2 or sth
table(mon_resources$resource)

dim(mon_attributes)
mon_attributes[95:100, ]

#== operational outcomes ######
plot(x = mon_arrivals, metric = "waiting_time");

#> calculate the mean waiting time for each resources & total
mon_arrivals <- mon_arrivals %>%
  mutate(waiting_time = end_time - start_time - activity_time)

mean_waiting_time <- mon_arrivals %>%
  group_by(resource) %>%
  summarize(mean_waiting_time = mean(waiting_time))

mean_waiting_time
sum(mean_waiting_time$mean_waiting_time) # avg total waiting time

#> plot the resource usage over the simulation time
plot(x = mon_resources, metric = "usage", item = "server", steps = TRUE)

#== patient outcomes ######
#> function to convert the simulation object into a data.frame with a row for each entity
fun_summarize_attributes <- function(sim_out, keys) {
  
  # INPUTS:
  # - sim_out the simulation object obtained using the 'get_mon_attributes()' function
  # - keys character vectors defining the names of the attributes to be included
  
  # OUTPUT:
  # - df_out data.frame with the name of the entity and all required variables
  
  df_out <- sim_out %>%
    filter(key %in% keys) %>% # filter to include rows that have information attributes of interest
    group_by(name, key) %>% # group so that for every entity rows are grouped by the attributes
    arrange(time) %>% # for each entity and attribute, order the rows by the simulation time
    slice(n()) %>% # only select the last row, representing the final attribute value
    dplyr::select(name, key, value) %>% # select columns including the entity name and attribute name and value
    spread(key = key, value = value) # long to wide format
  
  return(df_out[, c("name", keys)])
}

# transform mon_attributes object into a patient-level summary of the final value of all attributes
df_attributes <- fun_summarize_attributes(sim_out = mon_attributes,
                                          keys = c("time_nurse", "see_doctor", "time_doctor"))
head(x = df_attributes, n = 5)

# see whether the probability of seeing a doctor is similar to 80%
# note: we use the mean of a vector of 0 and 1 values, which is equivalent to 
# taking the sum of the vector (i.e., counting the number of 1 values) and dividing
# it by the length of the vector
mean(x = df_attributes$see_doctor)

#> extract the simulated time (alternative: calculate from the mon_arrivals data.frame)
out_time_nurse <- df_attributes$time_nurse;
out_time_doctor <- df_attributes$time_doctor;

#> visualization
par(mfrow = c(1, 2));

# "nurse"
hist(data_time_nurse, prob = TRUE, xlim = c(0, 60));
lines(density(samples_time_nurse), col = "darkred", lwd = 3);
lines(density(out_time_nurse), col = "darkblue", lwd = 3);
legend("topright", legend = c("observed", "sampled", "simulated"),
       col = c("black", "darkred", "darkblue"), lty = 1, lwd = 2, bty = 'n')

# "doctor"
hist(data_time_doctor, prob = TRUE, xlim = c(0, 60));
lines(density(samples_time_doctor), col = "darkred", lwd = 3);
lines(density(out_time_doctor, na.rm = T), col = "darkblue", lwd = 3);
legend("topright", legend = c("observed", "sampled", "simulated"),
       col = c("black", "darkred", "darkblue"), lty = 1, lwd = 2, bty = 'n')

# [IMP]
# observed: raw collected data;
# sampled: the fitted distribution (specified by formula)
# simulated: distribution of the random draw during the simulation based on the specified distribution to be sampled from
#/////////////////////////////////////////////////////////////////////////# ######


#////////////////////////////////////////////////////////////////////////#
#========================================================================#
# END OF SCRIPT # END OF SCRIPT # END OF SCRIPT # END OF SCRIPT
#========================================================================#
#////////////////////////////////////////////////////////////////////////#

