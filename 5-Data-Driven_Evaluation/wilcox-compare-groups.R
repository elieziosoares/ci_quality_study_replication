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

#projects_issue <- dbGetQuery(con, "SELECT P.repo_name, count(I.id) qty_issues, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN ISSUE I ON P.repo_name = I.repo_name WHERE rq1_included is true and((ci_service is NOT null and I.created_at between P.analysis_before_ci and P.analysis_init)OR(ci_service is null and I.created_at between P.analysis_init and P.analysis_finish)) GROUP BY P.repo_name order by P.repo_name;")
#projects_prs <- dbGetQuery(con, "SELECT P.repo_name, count(PR.id) qty_prs, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN PULLREQUESTS PR ON P.repo_name = PR.project_name WHERE rq1_included is true and((ci_service is NOT null and PR.created_at between P.analysis_before_ci and P.analysis_init) OR (ci_service is null and PR.created_at between P.analysis_init and P.analysis_finish)) GROUP BY P.repo_name order by P.repo_name;")
#projects_bugs <- dbGetQuery(con, "SELECT P.repo_name, count(I.id) qty_bugs, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN ISSUE I ON P.repo_name = I.repo_name WHERE rq1_included is true and isbug is true and((ci_service is NOT null and I.created_at between P.analysis_before_ci and P.analysis_init)OR(ci_service is null and I.created_at between P.analysis_init and P.analysis_finish)) GROUP BY P.repo_name order by P.repo_name;")
projects_size <- dbGetQuery(con, "SELECT P.repo_name, P.size, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P WHERE rq1_included;")
projects_stars <- dbGetQuery(con, "SELECT P.repo_name, P.qtd_stars stars, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P WHERE rq1_included;")
projects_issues <- dbGetQuery(con, "SELECT P.repo_name, P.qtd_issues issues, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P WHERE rq1_included order by ci_service;")

projects_issue <- dbGetQuery(con, "SELECT P.repo_name, count(I.id) qty_issues, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN ISSUE I ON P.repo_name = I.repo_name WHERE rq1_included is true and I.created_at between P.analysis_init and P.analysis_finish GROUP BY P.repo_name order by P.repo_name;")
projects_prs <- dbGetQuery(con, "SELECT P.repo_name, count(PR.id) qty_prs, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN PULLREQUESTS PR ON P.repo_name = PR.project_name WHERE rq1_included is true and PR.created_at between P.analysis_init and P.analysis_finish GROUP BY P.repo_name order by P.repo_name;")
projects_bugs <- dbGetQuery(con, "SELECT P.repo_name, count(I.id) qty_bugs, CASE WHEN P.ci_service IS NOT NULL THEN 1 ELSE 0 END ci FROM PROJECTS P INNER JOIN ISSUE I ON P.repo_name = I.repo_name WHERE rq1_included is true and isbug is true and I.created_at between P.analysis_init and P.analysis_finish GROUP BY P.repo_name order by P.repo_name;")


projects_issue
projects_prs$qty_prs
projects_issue$ci
projects_issue$qty_issues
projects_issue$repo_name
projects_size

df_analise <- data.frame(projects_issue$repo_name,projects_issue$ci,projects_issue$qty_issues)
df_analise[is.na(df_analise)]=0
df_analise
t.test(df_analise$projects_issue.qty[df_analise$projects_issue.ci==0],df_analise$projects_issue.qty[df_analise$projects_issue.ci==1])
wilcox.test(df_analise$projects_issue.qty[df_analise$projects_issue.ci==0],df_analise$projects_issue.qty[df_analise$projects_issue.ci==1])


df_analise <- data.frame(projects_prs$repo_name,projects_prs$ci,projects_prs$qty_prs)
df_analise[is.na(df_analise)]=0
t.test(df_analise$projects_prs.qty_prs[df_analise$projects_prs.ci==0],df_analise$projects_prs.qty_prs[df_analise$projects_prs.ci==1])
wilcox.test(df_analise$projects_prs.qty_prs[df_analise$projects_prs.ci==0],df_analise$projects_prs.qty_prs[df_analise$projects_prs.ci==1])

projects_bugs
df_analise <- data.frame(projects_bugs$repo_name,projects_bugs$ci,projects_bugs$qty_bugs)
df_analise[is.na(df_analise)]=0
t.test(df_analise$projects_bugs.qty_bugs[df_analise$projects_bugs.ci==0],df_analise$projects_bugs.qty_bugs[df_analise$projects_bugs.ci==1])
wilcox.test(df_analise$projects_bugs.qty_bugs[df_analise$projects_bugs.ci==0],df_analise$projects_bugs.qty_bugs[df_analise$projects_bugs.ci==1])
  
projects_size
df_analise <- data.frame(projects_size$repo_name,projects_size$ci,projects_size$size)
df_analise[is.na(df_analise)]=0
t.test(df_analise$projects_size.size[df_analise$projects_size.ci==0],df_analise$projects_size.size[df_analise$projects_size.ci==1])
wilcox.test(df_analise$projects_size.size[df_analise$projects_size.ci==0],df_analise$projects_size.size[df_analise$projects_size.ci==1])
  
projects_stars
df_analise <- data.frame(projects_stars$repo_name,projects_stars$ci,projects_stars$stars)
df_analise[is.na(df_analise)]=0
t.test(df_analise$projects_stars.stars[df_analise$projects_stars.ci==0],df_analise$projects_stars.stars[df_analise$projects_stars.ci==1])
wilcox.test(df_analise$projects_stars.stars[df_analise$projects_stars.ci==0],df_analise$projects_stars.stars[df_analise$projects_stars.ci==1])

projects_issues
df_analise <- data.frame(projects_issues$repo_name,projects_issues$ci,projects_issues$issues)
df_analise[is.na(df_analise)]=0
t.test(df_analise$projects_issues.issues[df_analise$projects_issues.ci==0],df_analise$projects_issues.issues[df_analise$projects_issues.ci==1])





