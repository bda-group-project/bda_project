library(dplyr)
pokemon<-read.csv("C:/Users/lu936/Desktop/smu courses/6390/project/pokemon.csv",sep=",",stringsAsFactors=F)
colnames(pokemon)<-c("id","Name","Type.1","Type.2","HP","Attack","Defense","Sp.Atk","Sp.Def","Speed","Generation","Legendary")
Type.1<-c("Dragon","Steel","Flying","Psychic","Rock" ,"Fire","Electric" ,"Dark","Ghost" ,"Ground","Ice", "Water","Grass","Fighting", "Fairy" ,"Poison","Normal","Bug")
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797","#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")
COL<-data.frame(Type.1,color)
test_combats<-read.csv('C:/Users/lu936/Desktop/smu courses/6390/project/combats.csv',sep=",",stringsAsFactors=F)
names <- pokemon %>% dplyr::select(id, Name)
test_combats$First_pokemon_name<-sapply(test_combats$First_pokemon, function(x) names$Name[match(x, names$id)])
test_combats$Second_pokemon_name<-sapply(test_combats$Second_pokemon, function(x) names$Name[match(x, names$id)])

test_combats$First_pokemon_attack<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
test_combats$Second_pokemon_attack<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Attack[match(x, pokemon$Name)])
test_combats$Diff_attack<-test_combats$First_pokemon_attack - test_combats$Second_pokemon_attack

test_combats$winner_first_label<-ifelse(test_combats$Winner==test_combats$First_pokemon,1,0)

test_combats$First_pokemon_defense<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
test_combats$Second_pokemon_defense<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Defense[match(x, pokemon$Name)])
test_combats$Diff_defense<-test_combats$First_pokemon_defense - test_combats$Second_pokemon_defense

test_combats$First_pokemon_sp_defense<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
test_combats$Second_pokemon_sp_defense<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Sp.Def[match(x, pokemon$Name)])
test_combats$Diff_sp_defense<-test_combats$First_pokemon_sp_defense - test_combats$Second_pokemon_sp_defense

test_combats$First_pokemon_sp_attack<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
test_combats$Second_pokemon_sp_attack<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Sp.Atk[match(x, pokemon$Name)])
test_combats$Diff_sp_attack<-test_combats$First_pokemon_sp_attack - test_combats$Second_pokemon_sp_attack

test_combats$First_pokemon_speed<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
test_combats$Second_pokemon_speed<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Speed[match(x, pokemon$Name)])
test_combats$Diff_speed<-test_combats$First_pokemon_speed - test_combats$Second_pokemon_speed

test_combats$First_pokemon_HP<-sapply(test_combats$First_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
test_combats$Second_pokemon_HP<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$HP[match(x, pokemon$Name)])
test_combats$Diff_HP<-test_combats$First_pokemon_HP - test_combats$Second_pokemon_HP

test_combats$First_pokemon_type<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
test_combats$Second_pokemon_type<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Type.1[match(x, pokemon$Name)])
test_combats$First_pokemon_legendary<-sapply(test_combats$First_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])
test_combats$Second_pokemon_legendary<-sapply(test_combats$Second_pokemon_name, function(x) pokemon$Legendary[match(x, pokemon$Name)])

#scale numerical features
temp<- data.frame(test_combats %>% dplyr::select(winner_first_label,Diff_attack ,Diff_defense, Diff_sp_defense,Diff_sp_attack,Diff_speed ,Diff_HP, First_pokemon_legendary, Second_pokemon_legendary))
#ind <- sapply(temp, is.numeric)
#temp[ind] <- lapply(temp[ind], scale)

anes=glm(winner_first_label~Diff_attack+Diff_defense+Diff_sp_defense+Diff_sp_attack+Diff_speed+Diff_HP+First_pokemon_legendary+Second_pokemon_legendary
         ,family=binomial(link='logit'),data=temp)

summary(anes)

library(pROC)

pre <- predict(anes,temp)
modelroc <- roc(temp$winner_first_label,pre)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
