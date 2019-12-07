
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(gapminder)

app <- Dash$new()

#You can change colours and styles in the app
colors <- list(
  text = '#0013a3'
)

textStyle = list(
  textAlign = 'center',
  color = colors$text
)

#let's define our data and make a plot
dat <- data.frame(
  lab_session = factor(c("Lab 1","Lab 2", "Lab 1","Lab 2")),
  likes = factor(c("Data", "Data", "Science", "Science")),
  students = c(35, 22, 15, 28)
)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, 
                 frame = year, 
                 ids = country)) +
  theme_bw() +
  labs(x = 'GDP Per capita ($)',
       y = 'Life Expectancy (years)',
       title = "Hans Rosling's famous gapminder bubble chart") + 
  scale_x_log10() + 
  theme(legend.title = element_blank()) +
  theme_bw()

p <- ggplotly(p, width = 700, height = 400) %>% 
        animation_opts(
            frame = 500, 
            easing = "elastic", 
            redraw = FALSE
        ) %>% 
        animation_button(
            x = 1, xanchor = "right", y = 0.02, yanchor = "bottom"
        ) %>%
        animation_slider(
            currentvalue = list(prefix = "YEAR: ", font = list(color="red"))
        ) %>% 
        config(displayModeBar = FALSE)

#chart_link = api_create(p, filename="animations-advanced")


graph <- dccGraph(
  id = 'dsci-graph',
  figure= p
)

#Here's some text
mdText <- "We can display some text using *Markdown*. 
- Point 1
- Point 2
"

mdImgText <- "New England Aquarium, Boston, MA image, wikipedia commons, 
                  [source](https://commons.wikimedia.org/wiki/Category:Images#/media/File:Anthozoa_on_glass-pink_0442.jpg)."

app$layout(
  htmlDiv(
    list(
      #See our styles applied to the headers
      htmlH1('Hello Dash', style = textStyle),
      htmlH2('This is our R Dashboard', style = textStyle),
      dccMarkdown(children = "",
                  style=list(color = colors$text)),
      htmlDiv(children = "Let's make a graph!", style = textStyle),
      # we can add our graph here
      graph,
      dccMarkdown(children = mdImgText),
      htmlImg(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Anthozoa_on_glass-pink_0442.jpg/2560px-Anthozoa_on_glass-pink_0442.jpg",
              width='30%')
    )
  )
)

app$run_server()

### App created by Kate Sedivy-Haley as part of the DSCI 532 Teaching Team
