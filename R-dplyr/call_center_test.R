library(dplyr)
library(ggplot2)
library(ggvis)
require(gridExtra)
library(reshape2)

leads <- tbl_df(read.csv("leads.csv", stringsAsFactors = FALSE))
signups <- tbl_df(read.csv("signups.csv", stringsAsFactors = FALSE))
calls <- tbl_df(read.csv("calls.csv", stringsAsFactors = FALSE))

# 1.  Which agent made the most calls? [1]

#Here I only have to refer to the 'calls' dataset. 

p1 <- calls %>% 
  group_by(Agent) %>%
  summarise(Numb.calls = n()) %>% 
  arrange(desc(Numb.calls)) %>% 
  ggplot(aes(x = Agent, y = Numb.calls)) + geom_bar(stat ="identity")

# 2.For the leads that received one or more calls, how many calls were received on average? [2]

calls %>% group_by(Phone.Number) %>%
  summarise(number_of_calls = n()) %>% 
  filter(number_of_calls > 0) %>% #With this I select the phone number that were called more than once.
  arrange(desc(number_of_calls)) %>% summary()


# 3.For the leads that signed up, how many calls were made on average ? 

inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>% 
  select(Name, Phone.Number, Approval.Decision) %>% group_by(Phone.Number) %>% summarise(nomb = n()) %>%
   summary()

# 4.  Which agent had the most signups? Which assumptions did you make?
# (note that there is a many-to-one relationship between calls and leads) [4]


#We could expect that all sign ups come from calls to clients that are some point 
# showed interest, this is, the Call.Outcome was "INTERESTED". In fact this can be proved with the
#following code. Here I take part of the code written for the Q3. 

data_Q4 <- inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>%
  select(Name, Phone.Number, Call.Outcome, Agent, Approval.Decision)

#This dataset contains only data of leads that at some point signed up. 
#The rows are the calls performed. It is noticable that it took more than one call
#to get some people to sign up. 
#By only taking the rows with Call.Outcome "INTERESTED", I get the very same 768 data points 
#of the 'leads' dataset. This could be taken as hypothesis and then prove it.

#I consider the signup as a reaction to the call, therefore the signup should be merited to the AGENT
#that managed to speak with the client. 


#SCENARIO 1. The signup is accounted on the last agent that called. 

inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>%
  select(Name, Phone.Number, Call.Outcome, Agent, Approval.Decision) %>% 
  filter(Call.Outcome == "INTERESTED") %>% group_by(Agent) %>% summarise(total_signups = n ()) %>%
  arrange(desc(total_signups)) %>%
  ggplot(aes(x = Agent, y = total_signups)) + geom_bar(stat = "identity")



# 5.  Which agent had the most signups per call? [2]

b1 <- calls %>% 
  group_by(Agent) %>%
  summarise(Numb.calls = n()) %>% 
  arrange(desc(Numb.calls))

b2 <- inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>%
  select(Name, Phone.Number, Call.Outcome, Agent, Approval.Decision) %>% 
  filter(Call.Outcome == "INTERESTED") %>% group_by(Agent) %>% summarise(total_signups = n ()) %>%
  arrange(desc(total_signups))

left_join(b1,b2,by ="Agent") %>% mutate(signup_per_call =total_signups/Numb.calls) %>%
  arrange(desc(signup_per_call))



  
# 6.  Was the variation between the agents signups-per-call statistically significant? Why? [5]


#NO CLUE


# 7.  A lead from which region is most likely to be "interested" in the product? [3]
c1 <- calls %>% filter(Call.Outcome == "INTERESTED") %>% select(Phone.Number, Call.Outcome) %>%
  left_join(leads, by = "Phone.Number") %>% group_by(Region) %>% summarise(numb_interested = n()) %>% 
  arrange(desc(numb_interested))

c2 <- calls %>%  left_join(leads, by = "Phone.Number") %>% group_by(Region) %>% 
  filter(Call.Outcome %in% c('INTERESTED','NOT INTERESTED')) %>%
  summarise(total_calls = n())%>%
  arrange(desc(total_calls))

#I calculate the ratio between calls with outcome interested and the total number of calls
#that returned a signifcant output (interested or not interested). Therefore I only consider a 
#part of the total calls as I am not interested in the calls that ended up with dead line or "call
#me later". With this, the ratio is more realistic, but has to be well-understood.

left_join(c1,c2, by = "Region") %>% mutate(ratio = numb_interested/total_calls) %>% 
  arrange(desc(ratio)) %>%
  ggplot(aes(x = Region, y = numb_interested, size = ratio)) + geom_point()


#8. A lead from which sectm or is most likely to be "interested" in the product? [1]

#Same procedure as before but grouping by sector insted of region. 

c3 <- calls %>% filter(Call.Outcome == "INTERESTED") %>% select(Phone.Number, Call.Outcome) %>%
  left_join(leads, by = "Phone.Number") %>% group_by(Sector) %>% summarise(numb_interested = n()) %>% 
  arrange(desc(numb_interested))

c4 <- calls %>%  left_join(leads, by = "Phone.Number") %>% group_by(Sector) %>% 
  filter(Call.Outcome %in% c('INTERESTED', 'NOT INTERESTED')) %>%
  summarise(total_calls_significant_output = n())%>%
  arrange(desc(total_calls_significant_output))

left_join(c3,c4, by ="Sector") %>% mutate(ratio = numb_interested/total_calls_significant_output) %>% 
  arrange(desc(ratio)) 

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

# 9.   Given a lead has already expressed interest and signed up, 
# a.	signups from which REGION are most likely to be approved? [2]
# b.	Is this statistically significant? Why? [5]

#----- 2 sample t test------
inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>%
  filter(Call.Outcome == "INTERESTED") %>%
  select(Name, Region, Sector, Approval.Decision) %>% 
  mutate(Approved = as.numeric(Approval.Decision == "APPROVED")) %>%
  group_by(Region) %>% select(Region,Approved) %>%   
  summarise(ratio = mean(Approved), n = n()) %>%
  arrange(desc(ratio))



e <- inner_join(calls, leads, by ="Phone.Number") %>% inner_join(signups, by = c("Name" = "Lead")) %>%
  filter(Call.Outcome == "INTERESTED") %>%
  select(Name, Region, Sector, Approval.Decision) %>% 
  mutate(Approved = as.numeric(Approval.Decision == "APPROVED")) %>%
  select(Region,Approved) %>%
  mutate(Region = ifelse(Region %in% c("london","north-east","south-east","midlands",
                                       "scotland","wales","south-west","south",
                                       "northern-ireland"), "Other", Region))
table(e)
fisher.test(table(e))
chisq.test(table(e))$p.value
  t.test(e$Approved ~ e$Region)
  oneway.test(e$Approved ~ e$Region)

#----- end of two sample t test------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------



# 10.
#(A)

#Change Age to the group. 

#Calls with outcome interested
m <- inner_join(calls,leads, by = "Phone.Number") %>% arrange(Age) %>% 
  filter(Call.Outcome %in% c("INTERESTED")) %>% left_join(signups, by = c("Name" = "Lead")) %>%
  filter(Age <= 50)

k <- inner_join(calls,leads, by = "Phone.Number") %>% arrange(Age) %>% 
  filter(Call.Outcome %in% c("INTERESTED", "NOT INTERESTED")) %>% left_join(signups, by = c("Name" = "Lead")) %>%
  filter(Age <= 50) %>% arrange(Age)

m_1 <- m[0:trunc(nrow(m)*0.2),]
m_2 <- m[(1+trunc(nrow(m)*0.2)):(trunc(nrow(m)*0.2*2)),]
m_3 <- m[(1+(trunc(nrow(m)*0.2*2))):(trunc(nrow(m)*0.2*3)),]
m_4 <- m[(1+(trunc(nrow(m)*0.2*3))):(trunc(nrow(m)*0.2*4)),]
m_5 <- m[(1+(trunc(nrow(m)*0.2*4))):nrow(m),]

k_1 <- k[0:trunc(nrow(k)*0.2),]
k_2 <- k[(1+trunc(nrow(k)*0.2)):(trunc(nrow(k)*0.2*2)),]
k_3 <- k[(1+(trunc(nrow(k)*0.2*2))):(trunc(nrow(k)*0.2*3)),]
k_4 <- k[(1+(trunc(nrow(k)*0.2*3))):(trunc(nrow(k)*0.2*4)),]
k_5 <- k[(1+(trunc(nrow(k)*0.2*4))):nrow(k),]

m_1 %>% mutate(group_age = 1)
m_2 %>% mutate(group_age = 2)
m_3 %>% mutate(group_age = 3)
m_4 %>% mutate(group_age = 4)
m_5 %>% mutate(group_age = 5)

m <- rbind(m_1 %>% mutate(group_age = 1),m_2 %>% mutate(group_age = 2),m_3 %>% mutate(group_age = 3),
           m_4 %>% mutate(group_age = 4), m_5 %>% mutate(group_age = 5) )

k <- rbind(k_1 %>% mutate(group_age = 1),k_2 %>% mutate(group_age = 2),k_3 %>% mutate(group_age = 3),
           k_4 %>% mutate(group_age = 4), k_5 %>% mutate(group_age = 5) ) 

m <- m %>% mutate(signed_up = as.numeric(Approval.Decision %in% c("APPROVED","REJECTED")))
k <- k %>% mutate(signedup = as.numeric(Approval.Decision %in% c("APPROVED","REJECTED")))



test <- k %>% group_by(Region,Sector, group_age) %>% summarise(total_calls = n(), number_signups = sum(signedup)) %>%
  mutate(ratio_sign_up = number_signups/total_calls) %>%
  data.frame() %>%
  arrange(desc(ratio_sign_up)) %>% tbl_df()


