# All-Seasons-Fund-Part-2

All Seasons Portfolio Part 2  
My third post to connect my learnings with my personal passion.

Business Understanding
Developing a Portfolio based on Ray Dalio’s All Weather Fund:

In Tony Robbin’s book - Master the Game, I learned about the All Weather Fund and more about personal
investing for the general public. Since then I decided to complete a deeper dive on this portfolio and the
different asset classes to determine if data science can help me to unlock some more financial benefits.

Objective and Key Result:

• Objective: Optimize an investment portfolio based on Ray Dalio’s All Weather Fund  
• Hypothesis and Key Result: An asset weighting with better return per unit of risk exists beyond
what Ray Dalio has prescribed  

My Workflow:
Here’s a breakdown of the workflow I used to create the All Seasons Portfolio:
1. Collecting Data: Source a reproducible function to import, transform and build a stock portfolio
using tidyquant package. Create a second function to pull fama french factors.
2. Visualize Data: Visualize the data to understand the correlation of Fama French Factors to portfolio
3. Volatility of Portfolio: Chart the comparison of asset and portfolio standard deviation comparison
4. Modelling: Forecast the Portfolio returns with machine learning using h2o package
5. Tuning: Initial dive into tuning parameters of a deep_learning algorithm
