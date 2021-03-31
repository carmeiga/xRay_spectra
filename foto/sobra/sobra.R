#######################
#####################
```{r}

plot(Gd~thetad)
lines(predict(m1descuberto)~thetad,col='red')
lines(predict(m2descuberto)~thetad,col='blue')

summary(m1descuberto)
summary(m2descuberto)
```




```{r}
costheta2d=cos(thetad)^2

md=lm(Gd^2~costheta2d)



coef(md)

plot(Gd^2~costheta2d)
abline(md)

summary(md)

inid=coef(md)

mdescuberto=nls(Gd~((a+b*cos(thetad)^2)^(eme/2)),start = list(a = inid[1], b = inid[2],eme=1))

cdes=coef(mdescuberto)


plot(Gd~thetad)
lines(predict(mdescuberto)~thetad,col='red')

#costheta2=cos(theta)^2

munica=nls(G~((a+b*cos(theta)^2)^(eme/2)),start = list(a = ini[1], b = ini[2],eme=1))

coef(munica)
predict(munica)

plot(G~theta)
lines(predict(munica)~theta,col='red')

coef(munica)
mini=lm(G^2~costheta2)

plot(G^2~costheta2)
abline(mini)

ini=coef(mini)


unhafonte=polarizador[polarizador$luz_amb==0,]

G=unhafonte$ISC/unhafonte$VSC

theta=unhafonte$theta*2*pi/360

plot(G~theta)

cuberto=as.numeric(polarizador$luz_amb==0)
parcial=as.numeric(polarizador$luz_amb==1)
destapado=as.numeric(polarizador$luz_amb==2)

G=polarizador$ISC/polarizador$VSC

theta=polarizador$theta*2*pi/360

munica=nls(G~((a+b*cos(theta)^2)^(eme/2)),start = list(a = 0.53,b = ini[2],eme=1))




############

toldo=polarizador[polarizador$luz_amb==1,]

G=toldo$ISC/toldo$VSC

theta=toldo$theta*2*pi/360

plot(G~theta)

###########







```

```{r}
toldo=polarizador[polarizador$luz_amb==1,]

Gt=toldo$ISC/toldo$VSC

thetat=toldo$theta*2*pi/360

plot(Gt~thetat)

costheta2t=cos(thetat)^2

mt=lm(Gt^2~costheta2t)



coef(mt)

plot(Gt^2~costheta2t)
abline(mt)

summary(mt)

init=coef(mt)

# agora imos axustar dous modelos. 
# o primeiro usando b e m estimados a descuberto
# o segundo con b como parametro a estimar

mtoldo1=nls(Gt~((a+cdes[2]*cos(thetat)^2)^(cdes[3]/2)),start = list(a = init[1]))

summary(mtoldo1)

mtoldo2=nls(Gt~((a+b*cos(thetat)^2)^(eme/2)),start = list(a = init[1], b = init[2],eme=1))
summary(mtoldo2)

anova(mtoldo1,mtoldo2)


plot(Gt~thetat)

lines(predict(mtoldo2)~thetat,col='red')
```

```{r}
############agora a ver que pasa coa outra forma funcional



###########



descuberto=polarizador[polarizador$luz_amb==2,]

Gd=descuberto$ISC/descuberto$VSC

thetad=descuberto$theta*2*pi/360

plot(Gd~thetad)

costheta2d=cos(thetad)^2
costhetad=cos(thetad)

md=lm(Gd~costhetad)



coef(md)

plot(Gd~costhetad)
abline(md)

summary(md)

inid=coef(md)^2

mdescubertoi=nls(Gd~((a)^(eme/2)+(b*cos(thetad)^2)^(eme/2)),start = list(a = inid[1], b = inid[2],eme=1))

cdesi=coef(mdescubertoi)


plot(Gd~thetad)
lines(predict(mdescubertoi)~thetad,col='red')




toldo=polarizador[polarizador$luz_amb==1,]

Gt=toldo$ISC/toldo$VSC

thetat=toldo$theta*2*pi/360

plot(Gt~thetat)

costhetat=cos(thetat)

mt=lm(Gt~costhetat)



coef(mt)

plot(Gt~costhetat)
abline(mt)

summary(mt)

init=coef(mt)^2

# agora imos axustar dous modelos. 
# o primeiro usando b e m estimados a descuberto
# o segundo con b como parametro a estimar

mtoldo1i=nls(Gt~((a)^(cdesi[3]/2)+(cdesi[2]*cos(thetat)^2)^(cdesi[3]/2)),start = list(a = init[1]))

summary(mtoldo1i)

mtoldo2i=nls(Gt~((a)^(eme/2)+(b*cos(thetat)^2)^(eme/2)),start = list(a = init[1], b = init[2],eme=1))
summary(mtoldo2)

anova(mtoldo2i,mtoldo1i)


plot(Gt~thetat)

lines(predict(mtoldo1i)~thetat,col='red')
lines(predict(mtoldo2i)~thetat,col='blue')
plot(Gt~thetat)

lines(predict(mtoldo1)~thetat,col='red')
lines(predict(mtoldo2)~thetat,col='blue')


```
