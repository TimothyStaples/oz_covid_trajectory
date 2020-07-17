Rscript oz_covid_analysis.R

message="auto-commit from on $(date)"
GIT=`which git`
REPO_DIR=~/Dropbox/Tim/data/oz_covid_trajectory
cd ${REPO_DIR}
${GIT} add --all .
${GIT} commit -m "$message"
${GIT} push git@github.com:TimothyStaples/oz_covid_trajectory

echo "$gitPush"


