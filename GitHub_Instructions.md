# Instructions for Engagement Repository
* Master Repository URL: git@github.com:AMacumber/D4G_BCGO_Engagement.git

## CREATE A GITHUB ACCOUNT

1. https://github.com/

2. Fork a copy of https://github.com/AMacumber/D4G_BCGO_Engagement to your Git Hub account


## SETUP GIT & CREATE LOCAL COPY OF REPOSITORY

1. Download and install Git: https://git-scm.com/

2. Open Git Bash

3. Navigate to where you would like to save the repository

  > cd /your/directory/path/

4. Create a local copy

  > git clone git@github.com:AMacumber/D4G_BCGO_Engagement.git

5. You should now have a folder called D4G_BCGO_Engagement with all the files


## TYPICAL WORKFLOW

1. Navigate to your local copy of the repository (D4G_BCGO_Engagement)

2. Make sure your local copy is up to date

  > git pull origin

3. Switch to the specific project branch. Example, Sankey_Age_Fiscal_Boys

  > git checkout Sankey_Age_Fiscal_Boys

4. Create and modify files.

5. Check the status of files within your repository

  > git status

6. Add your modified/deleted files to the staging area

  > git add -u

7. Add a new file to the staging area

  > git add filename

8. Commit your changes

  > git commit -m 'Your message'

9. Send your changes to GitHub (remote repository)

  > git push origin
