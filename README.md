# mlb-pitchers
STA4930 Captsone Course with the TB Rays <br>
<br>
Code pipeline:
create-subset.R -> data-cleaning.ipynb -> eda.ipynb -> modeling.ipynb

DISCLAIMER: Upon cleaning the code, an error was discovered in the way rates were cummalatively updated as the test set continued. The error was fixed, but results were worsened. Results were then improved almost back to the original level by discarding the rates for batters, and combining the rates for different types of counts for pitchers into one column, where the rate matches whatever count it is. For example, in a neutral count, the neutral rate for that pitcher would be present in the pitcher rate column for that row, and this would switch to their batter count rate when it was a batter count. This simplified the model to just the count dummy variables and one pitcher rate variable. The updated code and results can be found in updated-modeling.ipynb. However, since our paper is due in 24 hours and the mistake was found late, our presentation work will not contain this new process, and the results discussed are those of the original model. The overall conclusions remain the same, there are just some minor shifts in our top 10 and bottom 10 predictable pitchers. Didn't want to ignore this mistake and pretend it wasn't there.

TLDR: paper and presentation results found in modeling.ipynb that has a small mistake in updating rates through the test set, fixed results found in updated-modeling.ipynb. Updated-modeling.ipynb should be used if further exploration is to be done with this project.

Random-effects.R did not end up being used, but the code is attached here to view the process with that.

TBR_project.R contains additional EDA relating to fastball rates by batting count and outs.
