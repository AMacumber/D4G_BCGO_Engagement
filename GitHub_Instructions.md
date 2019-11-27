Master Repository URL: git@github.com:AMacumber/D4G_BCGO_Engagement.git

SETUP GIT

  1) Download and install Git: https://git-scm.com/

  2) Open Git Bash

  3) Navigate to where you would like to save the repository

    >>> cd /your/directory/path/

  4) Create a local copy

    >>> git clone git@github.com:AMacumber/D4G_BCGO_Engagement.git

  5) You should now have a folder called D4G_BCGO_Engagement with all the files


TYPICAL WORKFLOW

  1) Navigate to your local copy of the repository (D4G_BCGO_Engagement)

  2) Make sure your local copy is up to date

    >>> git pull origin

  3) Switch to the project branch. Example, Alluvial_Age_Fiscal_Boys

    >>> git checkout Alluvial_Age_Fiscal_Boys

  4) Make some changes to the files

  5) Check the status of files within your repository

    >>> git status

  6a) Add your modified/deleted files to the staging area

    >>> git add -u

  6b) Add a new file to the staging area

    >>> git add filename

  7) Commit your changes

    >>> git commit -m 'Your message'

  8) Send your changes to GitHub (remote repository)

    >>> git push origin
