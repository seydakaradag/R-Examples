library(tidyverse)

ggplot(midwest, aes(x = area, y = log(poptotal)))+
    geom_point(color = rgb(0.3,0.6,0.3),
               size = 4,
               shape = 1)+
    labs(title = "Population vs Area", 
         x = "Area",
         y = "Log Pop")+
    theme_bw(base_size = 24)


mpg %>%
    mutate(cyl = factor(cyl)) %>%
    ggplot(aes(x = displ, y = hwy, color = cyl))+
        geom_point(size = 4, aes(shape = cyl))+
        geom_smooth(aes(linetype = cyl))
    
mpg %>%
    mutate(cyl = factor(cyl)) %>%
    group_by(cyl) %>%
    summarise(meanHWY = mean(hwy, na.rm = T)) %>%
    ggplot(aes(x = cyl, y = meanHWY))+
        geom_bar(stat = "identity", fill = "red")



