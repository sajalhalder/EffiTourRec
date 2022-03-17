# EffiTourRec
Abstract: Abstract. Personalized itinerary recommendation has garnered wide research interests for their ubiquitous applications. Recommending personalized itineraries is complex because of the large number of points of interest (POI) to consider in order to construct an itinerary based on visitors' interest and preference, time budget, and uncertain queuing time. Previous studies typically aim to plan itineraries that maximize POI popularity, visitors' interest and minimize queuing time. However, existing solutions may not reflect visitor preferences because when creating itineraries, they prefer to recommend POIs with short prior visiting periods. These recommendations can conict with real-life scenarios as visitors typically spend less time at POIs that they do not enjoy, thus leading to the inclusion of unsuitable POIs. Moreover, constructing itineraries based on selected POIs is a challenging and time-consuming process. Existing approaches involve searching through a large number of non-optimal, duplicate itineraries that are time-consuming to review and generate. To address these issues, we propose an adaptive Monte Carlo Tree Search (MCTS) based reinforcement learning algorithm EffiTourRec using an effective POI selection strategy by giving preference to POIs with long visiting times and short queuing times along with high POI popularity and visitor interest. In addition, to reduce non-optimal and duplicated itineraries generation, we propose an efficient MCTS search pruning technique to explore a smaller, more promising portion of solution space. Experiment results in real theme park datasets show clear advantages of our proposed method over baselines, where our method outperforms the current state-of-the-art by 20.89% to 52.32% in precision, 8.36% to 21.35% in F1-score and 40.00% to 67.64% in execution time.


## mainEffiTourRec is the main runable file and ModelFiles contains necessary functions. 


If we use these codes, have to cite the following paper:

Sajal Halder, Kwan Hui Lim, Jeffrey Chan, and Xiuzhen Zhang. Efficient Itinerary Recommendation via Personalised POI Selection and Pruning. In Knowledge and Information Systems, 2022. DOI: https://doi.org/10.1007/s10115-021-01648-3

