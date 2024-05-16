# Pulling-The-Goalie-Thesis
My thesis project as part of completing my Masters of Science in Sports Data Analytics at the University of Oklahoma. In this repository, you will find the code itself, the thesis document, histograms produced, and other files produced during the process. My thesis defense was recorded and has been published as a video on [YouTube](https://youtu.be/QRhfQMxP6rs?si=2FAP53p4M2GHGa4N). More detailed project information can be found in the document, but an overview will be provided below. Please reach out with questions. 

**Project History** 
For my thesis, I wanted to conduct a project focusing on hockey in-game decision making. Hockey has no in-game decision making strategy compared to baseball (Sabremetrics), football (4th down), and basketball (3pt revolution). This thought process lead me to the decision of pulling the goalie. Much of the research I found focused on when to pull. This research assumed it is a beneficial strategy to teams. With a plethora of research deciding when to pull, this research hopes to address the long term viability of pulling the goalie.

**Project Methods**
Data was collected using the [hockeyR package](https://github.com/danmorse314/hockeyR) by Dan Morse to gather play by play data from the 2017-2018 season until the 2022-2023 season. This research is a historical, descriptive, and causal-comparative approach to answer the question. 12 variables were extraced from the game summary data that were relavent to analysis. The steps of research were to 
1) determine the optimal criteria for pulling the goalie
2) create a panel regression with team fixed effects to calculate the expected points added from optimal pull percent
3) create a logit panel regression with team fixed effects to calculate the change in likelihood of making the playoffs
4) Evaluate model fits and determine if pulling the goalie is an effective strategy

**Project Results**
The results of both models showed that pulling the goalie does not have a statistically significant impact on a teams standing points or making the playoffs. The goals per game and goals against average were significant factors in increasing the amount of standing points a team earns throughout the season; however, only goals per game was significant in the likelihood to make the playoffs. One limitaiton of the model is that it uses coarse measuring tools to gain insights. This study also does not include the individual game level trends and such. With the high variability of this strategy, that element should be explored in the future research. 
