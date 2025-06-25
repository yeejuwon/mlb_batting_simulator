# Optimizing Batting Orders: Monte Carlo Game Simulation Based on Batter Swing Clustering

! WINNERS OF CSAS 2025 : DATA CHALLENGE !

![포스터](./images/포스터_최종_3차.png)

## Introduction
- Our team, Yonsei Blues, aimed to evaluate whether clustering batters based on their physical swing characteristics and plate discipline stats is appropriate for assessing the scoring potential of batting lineups.
- We used K-Means Clustering as the clustering method and built a simulator using Monte Carlo Simulation.
- Our approach demonstrated that batters' swing types can be defined through characteristics such as bat speed and swing length, combined with swing-related statistics like plate discipline.
- Furthermore, by integrating existing Monte Carlo Simulation-based game simulation models, we confirmed that meaningful simulation results can be derived solely from the classification of batters' swing types.
- We also built a web app named 'CSAS 2025: MLB Batting Lineup Simulator,' which can simulate various scenarios.
- Web app link: https://yeejuwon.shinyapps.io/mlb_batting_simulator/
- !Note!: Due to computational complexity, a high number of simulations may result in prolonged computation time.
- For example, calculating the 'Best Batting Position' with 1,000 simulations will take approximately a minute and a half.

## Additional Data
- We used the Batting Splits statistics provided by Fangraphs. 
- Since the Fangraphs page offers an "Export Data" option, no web scraping code was required.
- Instead, simple preprocessing was performed to create the file 'fangraphs+platedisc.csv'.
- Reference : https://www.fangraphs.com/leaders/major-league?pos=all&stats=bat&lg=all&qual=0&season=2024&season1=2024&ind=0&players=19470&team=22%2Cts&hand=

## Files
- mlb_batting_simulator
  - switch_hitters_split.ipynb (1)
    - This code takes 'statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv' as input and separates the names of switch hitters based on the batting stance.
    - e.g., Tommy Edman -> Tommy Edman-R, Tommy Edman-L
  - anova_test.ipynb (2)
    - This code performs a One-Way ANOVA based on player_name.
  - attack_angle.ipynb (3)
    - This code calculates the attack_angle.
  - mean_statistics.ipynb (4)
    - This code computes mean statistics.
  - clustering_dataset.ipynb (5)
    - This code prepares the final dataset for clustering.
  - clustering_final.ipynb (6)
    - This code conducts the final clustering.
  - statcast+cluster.ipynb (7)
    - This code links the clustering results with Statcast data.
  - bat_prob_matrix.ipynb (8)
    - This code calculates the batting probability matrix for left-handed and right-handed pitchers separately.
  - state_transit_matrix.ipynb (9)
    - This code calculates the state transition matrix.
  - runs_and_outs_matrix.ipynb (10)
    - This code calculates the runs matrix and outs matrix.
  - simulation.ipynb (11)
    - This code performs the final batting order simulation.
  - REPORT.pdf
    - Report for the CSAS 2025 : Data Challenge
  - app.R
    - This code generates our web app.
    - This includes UI generating code and backend code.
  - data/
    - Please add 'statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv' in this folder.
    - fangraphs+platedisc.csv
      - Basic + Advanced + Batted Ball + Plate Discipline stats we retrieved from Fangraphs.
    - cluster_stats.csv
      - This file contains the stats organized by batter cluster.
    - batstatmat_v_rhp.csv, batstatmat_v_lhp.csv
      - The batting probability matrices for left-handed and right-handed pitchers.
    - outs_matrix.csv
      - A matrix representing the changes in out count based on situational changes.
    - runs_matrix.csv
      - A matrix showing changes in the out count according to situational changes.
    - state_transition_matrix.csv
      - A matrix defining situational changes.
    - batting_stats_final.csv
      - A dataset linking player stats to clusters.
  - rsconnect/
    - This folder includes files that helps publishing our web app to shinyapps.io
  - submission.Rproj
    - An R project file for creating relative paths for R codes. Please open this file before 'app.R' for configuration.

## How to Run
- The files that must initially exist in the data folder are 'statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv' used in the Data Challenge, and 'fangraphs+platedisc.csv' retrieved from Fangraph.
- Execute the files in the order specified in the 'Files'. e.g. switch_hitters_split.ipynb (1): (1) indicates that it should be run first.
- The code is written with relative paths, so it should run smoothly.
- Our codes will generate files for next codes sequentially.
- Please open 'submission.Rproj' before 'app.R' for configuration.

## Contact Info
- Juwon Lee(Team Captain): leeju1_424@yonsei.ac.kr
- Jiyong Lee: jiyong18@yonsei.ac.kr
- Yugyung Kim: yugyung@yonsei.ac.kr 
