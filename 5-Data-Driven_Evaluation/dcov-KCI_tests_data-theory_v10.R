library(DBI)
library(RODBC)
library(RPostgreSQL)


#create connection object
con <- dbConnect(drv =PostgreSQL(), 
                 user="commitminer", 
                 password="commitminer",
                 host="localhost", 
                 port=5432, 
                 dbname="Causal_CI_Quality_v4")



##########################################################################
# To use the "pcalg" package, we need two auxilliary packages "RBGL"
# and "graph", which are not available on CRAN but on BioConductor
##########################################################################
library(kpcalg)
library(CondIndTests)
library(energy)


###############################################################
################### QUERIES TO OBTAIN DATA ####################
###############################################################

ci <- dbGetQuery(con, "SELECT CASE WHEN CI=TRUE THEN 1 WHEN CI=FALSE THEN 0 END CI FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
bugs <- dbGetQuery(con, "SELECT bugs FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#commit_s <- dbGetQuery(con, "SELECT commit_size_mean commit_size FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#commit frequency as commit_size
commit_s <- dbGetQuery(con, "SELECT qty_commits commit_size FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#merge_c <- dbGetQuery(con, "SELECT merge_conflicts Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
merge_c <- dbGetQuery(con, "SELECT merge_conflicts_git Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
communication <- dbGetQuery(con, "SELECT communication_mean Communication FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
test_v <- dbGetQuery(con, "SELECT test_volume_proportional test_vol FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
age <- dbGetQuery(con, "SELECT age FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
author_freq <- dbGetQuery(con, "SELECT author_frequency_mean FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")



df <- data.frame(ci,bugs,commit_s,merge_c,communication,test_v,age,author_freq)#,issue_type)
df[is.na(df)]=0
df





#Verify normality with Anderson-Darling normality test
library(nortest)
ad.test(df$ci)
shapiro.test(df$merge_c)
ad.test(df$bugs)
ad.test(df$commit_size)
ad.test(df$merge_c)
ad.test(df$communication)
ad.test(df$test_v)
ad.test(df$age)
ad.test(df$author_freq)
#ad.test(df$issue_type)



#####################################################
##  Literature-driven Model (Adição de author frequency e considerando commit_size como commit frequency)
##  http://dagitty.net/mwvyrqw
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#Age ⊥ Commit_Size                                                                                                  49.5566 DEPENDENT
dcov.test(df$commit_s,df$age,R=200)

#Age ⊥ Tests_Volume                                                                                                 4.308763 DEPENDENT
dcov.test(df$test_vol,df$age,R=200)

#Age ⊥ frequency_authors                                                                                            4.106676 DEPENDENT
dcov.test(df$age,df$author_frequency_mean,R=200)

#Commit_Size ⊥ frequency_authors                                                                                    2.700899
dcov.test(df$commit_size,df$author_frequency_mean,R=200)

#Tests_Volume ⊥ frequency_authors                                                                                   0.1029429
dcov.test(df$test_vol,df$author_frequency_mean,R=200)

#Communication ⊥ frequency_authors                                                                                  0.3065156
dcov.test(df$communication,df$author_frequency_mean,R=200)

#Continuous_Integration ⊥ frequency_authors                                                                         0.09195952
dcov.test(df$ci,df$author_frequency_mean,R=200)

#H8	Age ⊥ Communication | Commit_Size, Continuous_Integration                                                        0.0001035991 DEPENDENT
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")

#H9	Age ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                                      0.2696356 
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#10 - Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                           0.4865323
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#H11 - Continuous_Integration ⊥ Tests_Volume | Commit_Size                                                          3.463896e-14
CondIndTest(df$ci,df$test_vol,df$commit_size, method = "KCI")

#       Bug_Report ⊥ Commit_Size      | Age, Communication, Continuous_Integration, Merge_Conflicts, Tests_Volume, frequency_authors        0
#H12 -	Bug_Report ⊥ Commit_Frequency | Age, Communication, Continuous_Integration, Merge_Conflicts, Tests_Volume, frequency_authors
dfa <- data.frame(age,communication,ci,merge_c,test_v,author_freq)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")

#H13 - Merge_Conflicts ⊥ Tests_Volume | Commit_Size                                                                 0.5421594
CondIndTest(df$merge_conf,df$test_vol,df$commit_size, method = "KCI")





#####################################################
##  Literature-driven Model - Failed Hypothesis
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1 - Age ⊥ Commit_Size
dcov.test(df$age,df$commit_size,R=200)
      
      # ADDING AN EDGE AGE -> COMMIT FREQUENCY
      # Age and commit size  should be independent because the collider on CI.
      # The edges orientation could be wrong, in this case, when conditioning on CI, they should be independent
      CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")   # 1.41219e-07 Dependent
      # We DO NOT confirm this. Even conditioning on CI, they remain dependent.
      #Therefore, we add a new edge
    
#H2 - Age ⊥ Tests_Volume
      dcov.test(df$age,df$test_vol,R=200) #4.308763
      
      #Testing if the edge Commit_Frequency -> CI is wrong. When conditioning on Commit_Frequency should open the flow.
      CondIndTest(df$age,df$test_vol,df$commit_size, method = "KCI") #0
      #Testing if conditioning on bugs the flow opens...
      CondIndTest(df$age,df$test_vol,df$bugs, method = "KCI") #0
      
      
      #ADDING EDGE AGE -> TEST VOLUME
      #Even conditioning by CI    (2.395195e-12)
      #and bug_reports, the variables AGE and TEST VOLUME remains dependent  (1.068682e-08)
      #Without conditioning, they are dependent (4.308763)
      CondIndTest(df$age,df$test_vol,df$ci, method = "KCI")   #2.395195e-12  DEPENDENT
      
      
      
#H3 - Age ⊥ frequency_authors             4.106676 DEPENDENT
      dcov.test(df$age,df$author_freq,R=200)
    
      
      #ADDING EDGE AGE -> frequency_authors
      #Even conditioning by merge conflicts and by bug reports they remain dependents...
      #Without conditioning, they are dependent, as tested in the original causal hipothesis
      CondIndTest(df$age,df$author_freq,df$merge_conf, method = "KCI")  # 8.580812e-06
      CondIndTest(df$age,df$author_freq,df$bugs, method = "KCI")  # 0.00139212
      
      
#H4 - Commit_Size ⊥ frequency_authors
      dcov.test(df$commit_size,df$author_freq,R=200)  #2.700899
      
      #ADDING EDGE frequency_authors -> Commit_size
      #Even conditioning by merge conflict
      #Even conditioning by bug reports they remain dependents...
      #Even conditioning by age or ci they remain dependents...
      #Without conditioning, they are dependent, as tested in the original causal hipothesis
      CondIndTest(df$commit_size,df$author_freq,df$merge_conf, method = "KCI")  # 0
      CondIndTest(df$commit_size,df$author_freq,df$bugs, method = "KCI")     #  0
      CondIndTest(df$commit_size,df$author_freq,df$age, method = "KCI")   # 0
      CondIndTest(df$commit_size,df$author_freq,df$ci, method = "KCI")  # 0
   

#H8 - Age ⊥ Communication | Commit_Size, Continuous_Integration
      dfa <- data.frame(ci,commit_s)
      dfa[is.na(dfa)]=0
      CondIndTest(df$age,df$communication,dfa, method = "KCI")    #4.072992e-06
      
      # ADDING AGE -> COMMUNICATION
      # Even conditioning by CI and Commit size, the variables remains dependent
      # without conditioning, they are dependent  ()
      dcov.test(df$age,df$communication,R=200)  #11.86268
      
      
      CondIndTest(df$age,df$communication,df$ci, method = "KCI") #1.162458e-06

      
#H11 - Continuous_Integration ⊥ Tests_Volume | Commit_Size      
    CondIndTest(df$test_vol,df$ci,df$commit_size, method = "KCI") #6.739054e-14
    
    # INVERTING THE EDGE CI -> COMMIT SIZE (CONFIRMING WHAT OCCURRED IN H1)
    # Testing the independence under the conditioning, they are dependent
    # Testing without conditioning, they are independent
    # Thus, we suppose they form a collider on Commit Size, then when conditioning on it, the flow open...
    dcov.test(df$test_vol,df$ci,R=200)  #0.1128856  independent


#H12	Bug_Report ⊥ Commit_Size | Age, Communication, Continuous_Integration, Merge_Conflicts, Tests_Volume, frequency_authors 
    dfa <- data.frame(age,communication,ci,merge_c,test_v,author_freq)
    dfa[is.na(dfa)]=0
    CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")
    
    # ADDING  Commit_frequency -> Bug_Report
    # Even conditioning by age,communication,ci,merge_c,test_v,author_freq the variables remains dependent
    # without conditioning, they are dependent  ()
    dcov.test(df$bugs,df$commit_size,R=200)                       #6.009902
    
    
  
  
#####################################################
## theory_data_v10_0
## http://dagitty.net/muceYWH
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################
    
#H1	Age ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration, frequency_authors               #0.6059621
    dfa <- data.frame(commit_s,ci,author_freq)
    dfa[is.na(dfa)]=0
    CondIndTest(df$age,df$merge_conf,dfa, method = "KCI") 
    
#H2	Communication ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration, frequency_authors     #0.4201362
    dfa <- data.frame(commit_s,ci,author_freq)
    dfa[is.na(dfa)]=0
    CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI") 
    
#H3	Communication ⊥ Tests_Volume | Age                                                                #0
    CondIndTest(df$communication,df$test_vol,df$age, method = "KCI")                    
    
#H4	Communication ⊥ frequency_authors | Age                                                           #1.321165e-13
    CondIndTest(df$communication,df$author_frequency_mean,df$age, method = "KCI")                
    
#H5	Continuous_Integration ⊥ Tests_Volume | Age                                                       #0.7855025
    CondIndTest(df$ci,df$test_vol,df$age, method = "KCI")                
    
#H6	Continuous_Integration ⊥ frequency_authors | Age                                                  #0.803015
    CondIndTest(df$ci,df$author_frequency_mean,df$age, method = "KCI")                
    
#H7	Merge_Conflicts ⊥ Tests_Volume | Commit_Frequency, Continuous_Integration, frequency_authors      #0.3249785
    dfa <- data.frame(commit_s,ci,author_freq)
    dfa[is.na(dfa)]=0
    CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI") 
    
#H8	Tests_Volume ⊥ frequency_authors | Age                                                            #6.926176e-05
    CondIndTest(df$test_vol,df$author_frequency_mean,df$age, method = "KCI")                

    
    

#####################################################
## theory_data_v8_1  - Failed Hypothesis
## http://dagitty.net/mQeAMMT
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################
    
#H1 - Communication ⊥ Tests_Volume | Age
    CondIndTest(df$communication,df$test_vol,df$age, method = "KCI")        #0
    
    #ADDING EDGE  Tests_Volume -> Communication
    #We tested if conditioning by bugs the dependency would stop.
    #we tested if conditioning by commit_frequency(size)... (the hipothesis of edges reorienting...)
    CondIndTest(df$communication,df$test_vol,df$commit_size, method = "KCI")       #0
    
    dcov.test(df$communication,df$test_vol,R=200)                           #R = 0.3482406 dependent
    
    CondIndTest(df$communication,df$test_vol,df$bugs, method = "KCI")       #0
    
    
#H2 - Communication ⊥ frequency_authors | Age
    CondIndTest(df$communication,df$author_frequency_mean,df$age, method = "KCI")           #1.321165e-13  **
    
    #ADDING EDGE  Communication -> frequency_authors
    CondIndTest(df$communication,df$author_frequency_mean,df$commit_size, method = "KCI")           #4.868867e-06
    dcov.test(df$communication,df$author_frequency_mean,R=200)                           #R = 0.3065156 dependent
    
    

#H4 - Tests_Volume ⊥ frequency_authors | Age    
    CondIndTest(df$test_vol,df$author_frequency_mean,df$age, method = "KCI")                # 6.926176e-05 **
    
    
    #ADDING EDGE frequency_authors --->  Tests_Volume
    CondIndTest(df$test_vol,df$author_frequency_mean,df$commit_size, method = "KCI")                # 8.673738e-10
    dcov.test(df$test_vol,df$author_frequency_mean,R=200)   #0.1029429

    CondIndTest(df$test_vol,df$author_frequency_mean,df$bugs, method = "KCI")                #0.0001293085
    CondIndTest(df$test_vol,df$author_frequency_mean,df$merge_conf, method = "KCI")                #1.648851e-08

    
    
    
    
    
  
###################################################################
  
  t.test(df$merge_conf[df$ci==0],df$merge_conf[df$ci==1])
  t.test(df$commit_size[df$ci==0],df$commit_size[df$ci==1])
  t.test(df$age[df$ci==0],df$age[df$ci==1])
  t.test(df$bugs[df$ci==0],df$bugs[df$ci==1])
 
  res <- cor.test(df$age, df$bugs, method = c("pearson"))
  res
  res <- cor.test(df$communication, df$bugs, method = c("pearson"))
  res
     