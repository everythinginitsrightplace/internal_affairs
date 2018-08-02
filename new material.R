library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(highcharter)
library(plotly)

# Для начала загрузим данные по годам
MyData_2017 <- read.csv(file = "data-20180201-structure-20140727.csv", header = T, sep = ",", encoding = "UTF-8")
MyData_2018 <- read.csv(file = "data-20180427-structure-20180427.csv", header = T, sep = ",", encoding = "UTF-8")
MyData_2016 <- read.csv(file = "data-20170127-structure-20140727.csv", header = T, sep = ",", encoding = "UTF-8")
MyData_2015 <- read.csv(file = "data-20160220-structure-20140727.csv", header = T, sep = ",", encoding = "UTF-8")
MyData_2014 <- read.csv(file = "data-20150127-structure-20140727.csv", header = T, sep = ",", encoding = "UTF-8")


# Далее необходимо построить единую базу данных 
# 1-ая база - по общему количеству разыскиваемых

names(MyData_2014) <- c("Субъект", "Пункт", "Наименование", "Значение")
names(MyData_2015) <- c("Субъект", "Пункт", "Наименование", "Значение")
names(MyData_2016) <- c("Субъект", "Пункт", "Наименование", "Значение")
names(MyData_2017) <- c("Субъект", "Пункт", "Наименование", "Значение")

h <- rbind(MyData_2014[522, c(1,4)], MyData_2015[522, c(1,4)], MyData_2016[542, c(1,4)], MyData_2017[542, c(1,4)])
h$year <- 2014:2017

names(h) <- c("Subject", "Meaning", "Year")

names(h) <- c("Субъект", "Количество", "Год")


h$Количество <-  as.numeric(as.character(h$Количество))

j <-ggplot(h, aes(Год, Количество)) + 
  geom_line(color = "green", size = 1.5)+ 
  geom_point(size = 5, color = "red")+ 
  theme_modern_rc() +
  labs(x = "Года\n
       Источник данных: Министерство внутренних дел РФ ", 
       y = "Всего разыскивалось", title = "Пропавшие без вести в Татарстане")+
  ylim(6000, 8500)
  
interactive_j <- ggplotly(j)


# Строим новый сабсет 
g <- rbind(MyData_2014[523:524, c(1, 3,4)], MyData_2015[523:524, c(1, 3,4)], MyData_2016[543:544, c(1, 3, 4)], MyData_2017[543:544, c(1, 3, 4)])
g$Год <- c(2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017)

names(g) <- c("Субъект", "Всего разыскивалось", "Значение", "Год")
g$Значение <-  as.numeric(as.character(g$Значение))
g$`Всего разыскивалось` <- c("скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести")



gj <- ggplot(g, aes(fill = `Всего разыскивалось`, y = Значение, x = Год))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Информация по пропавшим без вести и скрывающимся от правосудия")+
  labs(x = "Года\n
       Источник данных: Министерство внутренних дел РФ", 
       y = "Всего разыскивалось")+
  theme_modern_rc()
  
interactive_gj <- ggplotly(gj)


# Теперь строим сабсет по лицам, которых удалось установить. Практически идентично тому, что было в самом первом графике

as <- rbind(MyData_2014[525, c(1,4)], MyData_2015[525, c(1,4)], MyData_2016[545, c(1,4)], MyData_2017[545, c(1,4)])
as$year <- 2014:2017

names(as) <- c("Субъект", "Количество", "Год")


as$Количество <-  as.numeric(as.character(as$Количество))

oj <-ggplot(as, aes(Год, Количество)) + 
  geom_line(color = "red", size = 1.5)+ 
  geom_point(size = 5, color = "green")+ 
  theme_modern_rc() +
  labs(x = "Года\n
       Источник данных: Министерство внутренних дел РФ ", 
       y = "Всего удалось установить личность", title = "Количество людей, находившихся в розыске, личность которых удалось установить")+
  ylim(4500, 7000)

interactive_oj <- ggplotly(oj)


# Настало время сабсета уже по детальном рассмотрению того, какие именно категории есть у тех, кого нашли


hh <- rbind(MyData_2014[526:527, c(1, 3,4)], MyData_2015[526:527, c(1, 3,4)], MyData_2016[546:547, c(1, 3, 4)], MyData_2017[546:547, c(1, 3, 4)])
hh$Год <- c(2014, 2014, 2015, 2015, 2016, 2016, 2017, 2017)

names(hh) <- c("Субъект", "Установлено лиц", "Значение", "Год")
hh$Значение <-  as.numeric(as.character(g$Значение))
hh$`Установлено лиц` <- c("скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести", "скрывающихся от правосудия", "пропавших без вести")



hhj <- ggplot(hh, aes(fill = `Установлено лиц`, y = Значение, x = Год))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Лица, находящиеся в розыске, личность которых удалось установить")+
  labs(x = "Года\n
       Источник данных: Министерство внутренних дел РФ", 
       y = "Удалось установить личность")+
  theme_modern_rc()

interactive_hhj <- ggplotly(hhj)


# Далее пытаемся построить pie chart, где дается информация по ПФО и по регионам

Volga_region <- rbind(MyData_2014[c(482, 492, 502, 512, 522, 532, 542, 552, 562, 572, 582, 592, 602, 612, 622), c(1,3,4)], MyData_2015[c(482, 492, 502, 512, 522, 532, 542, 552, 562, 572, 582, 592, 602, 612, 622), c(1,3,4)], MyData_2016[c(502, 512, 522, 532, 542, 552, 562, 572, 582, 592, 602, 612, 622, 632, 642), c(1,3, 4)], MyData_2017[c(502, 512, 522, 532, 542, 552, 562, 572, 582, 592, 602, 612, 622, 632, 642), c(1,3, 4)])
year_2014 <- rep(2014, 15)
year_2015 <- rep(2015, 15)
year_2016 <- rep(2016, 15)
year_2017 <- rep(2017, 15)

Volga_region$Год <- c(year_2014, year_2015, year_2016, year_2017)

Volga_region$Значение <- as.numeric(as.character(Volga_region$Значение))

only_2014 <- subset(Volga_region,  Volga_region$Год == "2014")

plot_ly(only_2014[-1,], labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся в регионах ПФО в 2014 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))




only_2015 <- subset(Volga_region,  Volga_region$Год == "2015")

plot_ly(only_2015[-1,], labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся в регионах ПФО в 2015 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


only_2016 <- subset(Volga_region,  Volga_region$Год == "2016")

plot_ly(only_2016[-1,], labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся в регионах ПФО в 2016 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


only_2017 <- subset(Volga_region,  Volga_region$Год == "2017")

plot_ly(only_2017[-1,], labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся в регионах ПФО в 2017 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Далее посмотрим на ситуацию по всем федеральным округам
Federals <- rbind(MyData_2014[c(2, 192, 332, 412, 482, 632, 712, 842), c(1,3,4)], MyData_2015[c(2, 192, 332, 412, 482, 632, 712, 842), c(1,3,4)], MyData_2016[c(2, 192, 332, 412, 502, 652, 732, 862), c(1,3, 4)], MyData_2017[c(2, 192, 332, 412, 502, 652, 732, 862), c(1,3, 4)])
year_2014 <- rep(2014, 8)
year_2015 <- rep(2015, 8)
year_2016 <- rep(2016, 8)
year_2017 <- rep(2017, 8)

Federals$Год <- c(year_2014, year_2015, year_2016, year_2017)

Federals$Значение <- as.numeric(as.character(Federals$Значение))

only_2014_f <- subset(Federals,  Federals$Год == "2014")

plot_ly(only_2014_f, labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся в федеральных округах в 2014 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


only_2015_f <- subset(Federals,  Federals$Год == "2015")

plot_ly(only_2015_f, labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся на территории федеральных округов в 2015 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


only_2016_f <- subset(Federals,  Federals$Год == "2016")

plot_ly(only_2016_f, labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся на территории федеральных округов в 2016 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



only_2017_f <- subset(Federals,  Federals$Год == "2017")

plot_ly(only_2017_f, labels = ~Субъект, values = ~Значение, type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste('$', Значение, ' человек'),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE) %>%
  layout(title = 'Число людей, разыскивавшихся на территории федеральных округов в 2017 году',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
