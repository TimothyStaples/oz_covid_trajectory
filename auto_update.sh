Rscript oz_covid_analysis.R

message="auto-commit from on $(date)"
GIT=`which git`
REPO_DIR=~/Dropbox/Tim/data/oz_covid_trajectory
cd ${REPO_DIR}
${GIT} add --all .
${GIT} commit -m "$message"
${GIT} push https://github.com/TimothyStaples/oz_covid_trajectory
timothy.staples@uqconnect.edu.au
0INJi0VPSbf8

echo "$gitPush"
