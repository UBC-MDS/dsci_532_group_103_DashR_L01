## Team Reflection

Our dashboard, which is based on average annual hate crime data from 2010-2016 across all US state,  has undergone various changes and has been updated based on the feedback provided by our peers and the TAs. Based on the feedback received, we have tried making the app easier to navigate for a user by adding description of what the app is about and details for how to completely explore and understand the app.

### Limitations of the app
- For Tab 2, we tried incorporating a box plot with jitter points but due to the nature of the data, the graph was overplotted and would not have been easy for a user to derive insights from. Although a box plot with jitter points would have been an ideal choice to explain the research question, we decided to go with a bar chart since it overcomes the problem of overplotting.
- We had to let go of the crossfiltering of charts in Tab 2 which the Python version of the app had. This was done due to time constraints as this feature is not easily incorporated with Plotly for R.
- One limitation of ggplotly() is that it can't translate the ggplot legend position. You can use layout to change the legend position, but it doesn't work for the colorbar legend. As a result, we can't change the colorbar legend to the bottom of the map, although it doesn't affect the effectiveness of the plot much.

### Future Improvements
- The dashboard only compares hate crime rates before and after 2016 elections and hints that this change may be attributed to the voting trend for Donald Trump. We would like to explore this more by incorporating data of hate crimes from previous U.S. elections. This way we may be able to particularly differentiate the significance of 2016 election (with Trump in the running) as opposed to scenarios in other presidential elections.

### Incorporating Feedback
For the TA feedback received this week. we have: 
- modified the proposal to make the description of each research question more clear
- started to summarize the tasks/ changes made in the pull request description
- added a concise description of the app for the tab 1, in order for the app to be more user friendly
- changed the labels and legends of graphs to make them more informative since we got feedback from milestone 2

