import pandas as pd
import numpy as np
import scipy.stats 

'''
I have designed one function for each question. 
'''

''''
Load the datasets
'''
leads = pd.read_csv("leads.csv")
signups = pd.read_csv("signups.csv")
calls = pd.read_csv("calls.csv")



 
def questions_one_two(calls):

    calls_agent_grouped = calls.groupby("Agent")
    calls_lead_grouped = calls.groupby("Phone Number")
    q1 = calls_agent_grouped['Agent'].count()

    q2 = calls_lead_grouped["Phone Number"].count().mean()
    
    return q1, q2

def join_dfs(df1,df2,df3):
    '''
    calls,leads,signups
    '''
    
    merged_df1_df2 = pd.merge(df1,df2, how = 'inner', on = "Phone Number")
    
    tot_merge = pd.merge(merged_df1_df2, df3, left_on = "Name", right_on = "Lead")
    
    return tot_merge
    


def question_three(df1,df2,df3):
    
    '''
    join datasets
    First by phone number, then by lead name
    '''
    q3 = join_dfs(df1,df2,df3)
    
    return q3.groupby("Name")["Name"].count().mean()
    
q1,q2 = questions_one_two(calls)
    
q4 = join_dfs(calls,leads,signups)


quest_four = q4[q4["Call Outcome"] == "INTERESTED"].groupby("Agent")["Agent"].count()


print "QUESTION 4" + "\n" + "\n" + "Number of signups per Agent" + "\n"
print quest_four

print "\n" + "\n" + "QUESTION 5" + "\n" + "\n" + "Signups per call on each agent" + "\n"
print quest_four/q1


'''
q5 = pd.merge(calls,leads, how = "inner", on = "Phone Number")[["Agent","Name","Call Outcome"]]

q5 = q5[(q5["Call Outcome"] == "INTERESTED") | (q5["Call Outcome"] == "NOT INTERESTED")]

q5 = pd.merge(q5,signups,how = "left", left_on = "Name", right_on = "Lead")[["Name","Agent","Call Outcome", "Approval Decision"]]
var_it = pd.get_dummies(q5['Approval Decision'])

q5["Signedup"]= var_it["APPROVED"] + var_it["REJECTED"]

q5 = q5[["Agent","Signedup"]]

'''
 
    
'''
Question 7: A lead from which region is most likely to be interested in the product ?

To answer this question I calculate the ratio between calls with outcome interested
and the total number of calls that returned a signifcant output (interested or not interested). 
Therefore I only consider a part of the total calls as I am not interested in the calls 
that ended up with "dead line","call me later" or "answer machine". 
With this, the ratio is more realistic.
'''



'''
Question 8. A lead from which sector is most likely to be interested in the product? [1]
The approach to this question is the same as the question before.
'''


def quest_seven_eight(group_by_variable):
        df = pd.merge(calls[(calls["Call Outcome"] == "INTERESTED") | (calls["Call Outcome"] == "NOT INTERESTED")], leads, how = "inner", on = "Phone Number")
        var_it = pd.get_dummies(df["Call Outcome"])
        df["interested"] = var_it["INTERESTED"]
        return df.groupby(group_by_variable)["interested"].mean()
        
print  "\n" + "\n" + "QUESTION 7" + "\n" + "\n" + "Ratio of leads interested per region" + "\n"

print quest_seven_eight("Region")

print  "\n" + "\n" + "QUESTION 8" + "\n" + "\n" + "Ratio of leads interested per sector" + "\n"

print quest_seven_eight("Sector")
print "\n" + "\n"




'''
Question 10. Suppose you wanted to pick the 1000 leads most likely to sign up
(who have not been called so far), based only on age, sector and region.

This question contains 4 subquestions. To answer them, we have to combine `calls`,`leads` and 
`signups` datasets to show only the calls that had a significant outcome, this is 
"INTERESTED" or "NOT INTERESTED". By doing this, I assume that it is ok to make more than one call. 

It is noticable that roughly 40% of people that showed interest did not sign up. 
For this question I optimize towards the ratio between signups and total calls.

This is the dataset I use to make all the computations. As mentioned, it contains all 
calls that gave an outcome (interested, not interested) as well as information regarding
if the user signed up.
'''

'''
a.  What criteria would you use to pick those leads? [10]
'''

df = pd.merge(calls[(calls["Call Outcome"] == "INTERESTED") | (calls["Call Outcome"] == "NOT INTERESTED")], leads, how = "inner", on = "Phone Number")

def age_group(data_frame):

    data_frame = data_frame[data_frame["Age"] <= 50]
    data_frame = data_frame.sort("Age")
    data_frame.index = range(1,len(data_frame) + 1)
    arr = np.array(data_frame["Age"])
    arr = np.split(arr,4)
    i = 1
    l= []
    for element in arr:
        k = pd.DataFrame(element)
        k["group_age"] = i
        l.append(k)
        i = i + 1
    a = pd.concat(l)
    a.index = range(1,len(a) + 1)
    data_frame["group_age"] = a["group_age"]
    return data_frame


df = age_group(df)
df = pd.merge(df,signups, how = "left", left_on = "Name", right_on = "Lead")[["Region","Sector","group_age", "Name", "Call Outcome", "Approval Decision"]]

var = pd.get_dummies(df["Approval Decision"])

df["signedup"] = var["APPROVED"] + var["REJECTED"]


grouped_df = df.groupby(["Region","Sector","group_age"]).agg({"Call Outcome":len, "signedup":["mean","sum"]}).sort([("signedup","mean")], ascending = False)

print grouped_df

''''
c. How many signups would you expect assuming they were being called by random agents?
'''


'''
Here I didn't manage to create the da

'''

m = pd.read_csv("leads-calls.csv")
m = m.iloc[0:5900]

m_age_group = age_group(m).groupby(["Region","Sector","group_age"]).agg({"Region": len})

da = grouped_df["signedup","mean"]



