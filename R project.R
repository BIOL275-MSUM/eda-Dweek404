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

Clay2021<-get_inat_obs(
  place_id = "13448",
  year = 2021,
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

# Binding non unique clay county to count occurences later  ---------------

F29<-bind_rows(Clay2020,Clay2019)

# Clarification MSUM Eco --------------------------------------------------

RMSUM2019<-select(MSUM2019,user_login)
RMSUM2020<-select(MSUM2020,user_login)

# Counting Unique Clay county  --------------------------------------------------------

count(RClay2019,user_login)
count(RClay2020,user_login)

# Counting unique MSUM eco  -----------------------------------------------

uniquestudent2019<-distinct(RMSUM2019, user_login)%>%
  pull(user_login)
uniquestudent2020<-distinct(RMSUM2020, user_login)

# Counting student occurences in clay county  -----------------------------

f<-filter(C29,user_login %in% c(uniquestudent2019))
f2<-filter(uniquestudent2020,user_login %in% c(C29))



# Creating graph of who did the most observations 2019 ----------------------------------------------------------
forbind2019<-(MSUM2019)
forbind2020<-(MSUM2020)
L<-bind_rows(forbind2019,forbind2020)
L2<-count(L,user_login)
L2
MSUM20191<-filter(MSUM2019,!(user_login =="chrismerkord"))
ggplot(data = MSUM20191) +
  geom_bar(mapping = aes(x = fct_infreq(user_login)), fill = "#C5351B",width = .93) +
  labs(x = "User", y= "Number of observations") +
  theme_classic(base_size = 8) + 
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1)),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.ticks.x = element_blank()
  )

# Figuring out who did observations outside of class.  --------------------

count(F29,user_login)
F291<-select(F29,user_login) 

#Use  line 81 to remind who Im typing for filtration these need to be MSUM 2019 
distinct(MSUM2019,user_login)

WHOCONTINUEDIN2020<-filter(Clay2020, user_login %in% c("sierravilmo18"
                                                       ,"madistrand"
                                                       ,"jordanwitherill"
                                                       ,"aaronerlandson"
                                                       ,"kassicook"
                                                       ,"paulofstedal"
                                                       ,"erikka_starr"
                                                       ,"andiewood"
                                                       ,"lisebee"
                                                       ,"keithcrisp"
                                                       ,"j_breezy05"
                                                       ,"kathleenedzards"
                                                       ,"arimabarii"
                                                       ,"kristofersando"
                                                       ,"juliaimdieke"
                                                       ,"cedvall"
                                                       ,"mck2000"
                                                       ,"alexhexum"
                                                       ,"riley93"
                                                       ,"paulsonda"))
                           

count(Clay2020,user_login == "chrismerkord") #test to see if code works 
count(MSUM2019, user_login == "chrismerkord") # test to see if code works. 

#might not need these leave here for now 
f<-filter(C29,user_login %in% c(uniquestudent2019))
f2<-filter(uniquestudent2020,user_login %in% c(C29))


# Graph of those who continued in 2020  ------------------------



# Figuring out who continued 2021  ----------------------------------------
distinct(Clay2021,user_login)
distinct(MSUM2020,user_login)

filter(Clay2021, user_login %in% c("chrismerkord"))  #test


# Figuring out extra data for the poster  ---------------------------------
count(MSUM2019,user_login)
x<-c(17,14,23,2,18,10,10,13,19,10,12,10,13,11,12,12,18,11,10,10)
max(x)
min(x)
mean(x)
