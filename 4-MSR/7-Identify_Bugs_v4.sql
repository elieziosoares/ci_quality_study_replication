select I.issue_number, I.repo_name FROM ISSUE I 
                INNER JOIN ISSUELABEL IL  ON IL.issue_number = I.issue_number AND IL.repo_name = I.repo_name
                INNER JOIN LABEL L ON IL.idlabel = L.idlabel
                where (name ilike '%bug%' AND (name not like '%not a bug%' AND name not like '%notabug%' AND name not like '%not-bug%' AND name NOT ilike '%bug fix%' 
                            AND name NOT ilike '%bug-fix%' AND name NOT ilike '%bugfix%' AND name NOT ilike '%bugsnag%' AND name NOT ilike '%debug%'))
                    OR name ilike '%please verify%' OR name ilike '%waiting for fix%' OR name ilike '%awaiting fix%' OR name ilike '%reproduc%'
                    OR name ilike '%fixme%'
                    OR NAME like 'issue' OR NAME like 'type: issue'
    OR (name ilike '%known issue%' and I.repo_name like 'Azure/azure-event-hubs-spark')
    OR (name ilike '%defect%' AND I.repo_name not like 'top-think/framework')
    OR (name ilike '%confirmed%' AND (name not ilike '%unconfirmed%' AND I.repo_name not like 'Dogfalo/materialize' AND I.repo_name not like 'particle-iot/device-os'))
    OR (I.repo_name LIKE 'jquery/jquery-mobile' AND (name ilike '%Priority: Blocker%' or name ilike '%Priority: High%'))
    OR I.repo_name LIKE 'jruby/activerecord-jdbc-adapter'
    OR I.repo_name LIKE 'microsoft/malmo'
    OR (I.repo_name LIKE 'Atmosphere/atmosphere' AND name NOT in ('Documentation','Enhancement','not a bug','sample'))
    OR (I.repo_name LIKE 'intel-analytics/BigDL' AND name NOT in ('document','dataset','example','question','test'))
    OR (I.repo_name LIKE 'rubinius/rubinius' AND name NOT in ('Community','Documentation','C-API Compatibility','Build & Packaging','test','Feature Request'))
    OR (I.repo_name LIKE 'jshint/jshint' AND name IN ('P1','P2'))


   --88.621

   
-- Identified BUGS    
-- 348643
 select count(I.issue_number) from issue I where isbug is true


 -- Projects with no classified bugs 127
-- Projects with classified bugs 713
 SELECT count(repo_name) From PROJECTS 
 WHERE commits_mined IS TRUE and analysis_releases > 1 and analysis_issues > 0 and analysis_prs > 0
 AND repo_name in (
 	select distinct repo_name from issue where isbug is true
 )
 
-- CI Projects included 547
-- NOCI Projects included 67
SELECT repo_name From PROJECTS 
    WHERE commits_mined2 IS TRUE and analysis_releases > 1 and analysis_issues > 0 and analysis_prs > 0
	and qty_bugs_period > 0
    and CI_SERVICE is null 
	--and CI_SERVICE ILIKE 'TRAVIS CI'
    order by repo_name desc;
 
-- Median bugs in 12 months: 11
SELECT PERCENTILE_DISC(0.5) WITHIN GROUP(ORDER BY qty_bugs_period) FROM PROJECTS
WHERE commits_mined IS TRUE and analysis_releases > 1 and analysis_issues > 0 and analysis_prs > 0 and qty_bugs_period > 0
-- Average bugs in 12 months: 47.5
SELECT avg(qty_bugs_period) From PROJECTS 
 WHERE commits_mined IS TRUE and analysis_releases > 1 and analysis_issues > 0 and analysis_prs > 0 and qty_bugs_period > 0


-- Projects with merge conflicts classified 1123
SELECT count(repo_name) From PROJECTS 
    WHERE merge_conflict_mined is TRUE


-- Pullrequests with identifies merge conflicts 9634
SELECT COUNT(PR_NUMBER) FROM PULLREQUESTS WHERE mergeconflict is true

