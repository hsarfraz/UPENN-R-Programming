library(tidyverse)
mpg
?mpg 
mpg$cyl
mpg$hwy
mpg$cty
mpg$drv
mpg$displ
nrow(mpg)
ncol(mpg)
glimpse(mpg)
view(mpg)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=drv))

ggplot(data=mpg)+
  geom_boxplot(mapping=aes(x=as.factor(cyl),y=hwy))+coord_flip()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() 

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed() +
  geom_smooth()


# scatterplot
ggplot(data=mpg) +
  geom_point(mapping=aes(x=cyl,y=hwy))

# scatterplot with each class representing a color (can be changes to other aesthetics)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha=cty))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape=cty))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy, color=cyl < 3))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl)) +
  facet_grid(drv~cyl)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()

ggplot(data=mpg) +
  geom_bar(mapping=aes(x=class, fill=as.factor(cyl)),position="dodge")
ggplot(data=mpg) +
  geom_boxplot(mapping=aes(x=cyl,y=hwy))+coord_flip()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

