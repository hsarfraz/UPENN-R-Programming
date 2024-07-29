library(tidyverse)
mpg
?mpg
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=cty))
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class)) +
  facet_wrap(~ class, nrow=2)
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class)) +
  facet_wrap(~ year, nrow=2)
ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, linetype=as.factor(year)))
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy))
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_smooth() +
  geom_point()
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=class)) +
  geom_smooth()
ggplot(data=mpg) +
  geom_boxplot(mapping=aes(x=class,y=hwy))
ggplot(data=mpg) +
  geom_boxplot(mapping=aes(x=class,y=hwy)) + coord_flip()
ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class))
ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class, y=..prop..,group=1))
ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class, fill=class))
ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class, fill=as.factor(year)),position="fill")
ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class, fill=as.factor(year)),position="dodge")