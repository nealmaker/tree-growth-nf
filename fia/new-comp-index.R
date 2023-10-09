library("tidyverse")
library("caret")
library("ranger")

# Define States & counties (FIPS codes) in Northern Forest region --------

states <- c("NY", "VT", "NH", "ME")

NY_counties <- c(75, 65, 49, 45, 89, 43, 35, 41, 33, 31, 19, 113)
VT_counties <- c(11, 19, 9, 7, 15, 5, 23, 1, 17, 13)
NH_counties <- c(7, 9, 3)
ME_counties <- c(17, 7, 25, 1, 11, 27, 9, 29, 19, 21, 3)


################################################################################
# Import FIA data
################################################################################

# Fetch FIA tree, growth, plot, & condition data for Northern Forest states
# and filter to keep only northern forest counties
# (this may take a while; ~140MB of downloads + reading)

temp <- tempfile()

for(state in states){
  download.file(paste("https://apps.fs.usda.gov/fia/datamart/CSV/",
                      state, "_TREE.zip", sep = ""),
                temp, mode = "wb")
  unzip(temp, paste(state, "_TREE.csv", sep = ""))
}

TREE <- lapply(states, function(x){
  read.csv(paste(x, "_TREE.csv", sep = ""), header = T) %>%
    filter(COUNTYCD %in% eval(as.name(paste(x, "_counties", sep = ""))),
           DIAHTCD == 1) %>% # excludes seedlings measured at root collar
    select(CN, PLT_CN, SUBP, PREV_TRE_CN, CONDID, DIA, SPCD, STATUSCD,
           MORTYR, CR, CCLCD, TREECLCD, HT, CDENCD) %>%
    mutate(ba_ac = if_else(DIA >= 5,
                           # poles & larger from 24' radius subplots
                           # saplings from 6.8' radius microplots
                           0.005454*DIA^2*(43560/(pi*24^2)),  
                           0.005454*DIA^2*(43560/(pi*6.8^2))))
})

nf_trees <- do.call(rbind, TREE)


################################################################################
# Explore crown density
################################################################################

nf_trees %>% ggplot(aes(CDENCD)) + geom_histogram()

den_mod <- lm(CDENCD ~ DIA + HT + CR + CCLCD + SPCD, 
              data = filter(nf_trees, !is.na(CDENCD)))

den_mod$coefficients

dat <- nf_trees %>% 
  select(CDENCD, DIA, HT, CR, CCLCD, SPCD) %>% 
  filter(!is.na(CDENCD))
den_mod_rf <- ranger(CDENCD ~ ., data = dat, importance = "impurity")

den_mod_rf$variable.importance

cor(dat$CDENCD, predict(den_mod, newdata = dat)) ^ 2
den_mod_rf$r.squared

dat %>% ggplot(aes(DIA, CDENCD)) + geom_smooth() + geom_jitter(height = 2.5, 
                                                               alpha = .05) 
dat %>% ggplot(aes(HT, CDENCD)) + geom_jitter(height = 2.5, alpha = .05) + 
  geom_smooth()
dat %>% ggplot(aes(CR, CDENCD)) + geom_jitter(height = 2.5, width = 2.5, 
                                              alpha = .05) + 
  geom_smooth()
dat %>% ggplot(aes(CCLCD, CDENCD)) + geom_jitter(height = 2.5, alpha = .05) + 
  geom_smooth()
dat %>% ggplot(aes(CDENCD)) + geom_boxplot() + facet_wrap(~ CCLCD, ncol = 1)
dat %>% ggplot(aes(CR)) + geom_boxplot() + facet_wrap(~ CCLCD, ncol = 1)

# CONCLUSIONS:
# CR is by far the best predictor of crown density, and they have a positive,
# pretty linear relationship.

# Crown class is slightly negatively related to density, but that's probably 
# mostly due to the negative relationship between crown class and CR.

# HT & DBH are pretty much unrelated.

lin <- lm(CDENCD ~ CR, data = dat)
loes <- loess(CDENCD ~ CR, data = dat)
cor(dat$CDENCD, predict(lin, newdata = dat)) ^ 2
cor(dat$CDENCD, predict(loes, newdata = dat)) ^ 2

dat %>% mutate(lin = predict(lin, newdata = dat), 
               loes = predict(loes, newdata = dat)) %>% 
  ggplot() + 
  geom_jitter(aes(CR, CDENCD), alpha = .05, height = 2.5, width = 2.5) + 
  geom_line(aes(CR, lin), col = "purple", size = 2) + 
  geom_line(aes(CR, loes), col = "green", size = 2) 
dat %>% ggplot(aes(CR)) + geom_histogram()

# I still think CR and density have a generally linear relationship.
# Loess only makes a difference in the tails, and there's very little data there
# to draw inferences from.
lin$coefficients
# Density = 32 + .45*CR

# If density was just a function of crown thickness (width; since it's amt of 
# light blocked by the crown horizontally), we would expect it to be correlated 
# to age or height, not just CR, since a taller older tree has a wider crown. Or 
# if crown width is roughly a proportional to crown height, we would expect a 
# correlation with HT*CR, independent of CR:
dat %>% ggplot(aes(HT)) + geom_histogram()
dat %>% mutate(HT = cut(HT, breaks = c(0, 30, 50, 60))) %>% 
  ggplot(aes(CR, CDENCD, col = HT)) + geom_smooth(method = "lm") 

mod <- lm(CDENCD ~ CR*HT, data = dat)
cor(dat$CDENCD, predict(mod, newdata = dat)) ^ 2
# This is not the case.


## Looking at species differences:
dat %>% mutate(SPCD = as.factor(SPCD)) %>% 
  filter(SPCD %in% c(318, 12, 746, 261)) %>% #sugar maple, balsam fir, quaking aspen, hemlock
  ggplot(aes(CR, CDENCD, col = SPCD)) + geom_smooth(method = "lm") 
dat %>% mutate(SPCD = as.factor(SPCD)) %>% 
  filter(SPCD %in% c(318, 12, 746, 261)) %>% # sugar maple, balsam fir, quaking aspen, hemlock
  ggplot(aes(SPCD, CDENCD)) + geom_boxplot() 
# they don't seem to make much difference at all...