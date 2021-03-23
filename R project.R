# Installing packages -----------------------------------------------------
library(tidyverse)
library(rinat)
# importing the data ------------------------------------------------------
Clay2020<-get_inat_obs(
  place_id = "13448",
  year = 2020,
  maxresults = 9999,
)
Clay2019<-get_inat_obs(
  place_id = "13448",
  year = 2019,
  maxresults = 9999,
)

MSUM2019<-get_inat_obs_project("msum-ecology-2019")

MSUM2020<-get_inat_obs_project( "msum-ecology-2020")


# Figuring out who are MSUM students  -------------------------------------



# Clarification for  Clay county ---------------------------------------------
RClay2020<-distinct(Clay2020,user_login)
RClay2019<-distinct(Clay2019,user_login)
C29<-bind_rows(RClay2019,RClay2020)
RC29<-distinct(C29)
# Clarification MSUM Eco --------------------------------------------------

RMSUM2019<-select(MSUM2019,user_login)
RMSUM2020<-select(MSUM2020,user_login)

# Counting Unique Clay county  --------------------------------------------------------

count(RClay2019,user_login)
count(RClay2020,user_login)

# Counting unique MSUM eco  -----------------------------------------------

count(RMSUM2019, user_login)
count(RMSUM2020, user_login)

# Counting student occurences in clay county  -----------------------------

