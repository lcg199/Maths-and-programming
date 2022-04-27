
urlfile='https://raw.githubusercontent.com/lcg199/Maths-and-programming/main/1977%20election%20data%20.csv'
data_1977 <- read.csv(url(urlfile))

urlfile2='https://raw.githubusercontent.com/lcg199/Maths-and-programming/main/2019%20spain%20data%20.csv'
data_2019<- read.csv(url(urlfile2))

attach(data_2019)
na.omit(data_2019)
attach(data_1977)
na.omit(data_1977)

data_2019[,12][data_2019[,12]==0] <- NA


#barchart 2019
barchart(party_english~log(partyvote), data = data_2019, 
         xlim=c(0,15),
         col = 'pink', ylab= 'Political Parties', 
         xlab= 'Logorithm of the valid number of votes received by each party', 
         main = '2019 Election Results')

#barchart 1977
barchart(party_english~log(validvote2), data = data_1977, col = 'lightblue', 
         ylab= 'Political Parties', 
         xlab= 'Logorithm of the number of votes received by each party', 
         main = '1977 Election Results')


data_2019[,12][data_2019[,12]==0] <- NA

#anvoa for 2019 
anova19 <- aov(party_english~partyvote, data= data_2019)
summary(anova19)

#anova for 1977
anova77 <- aov(party_english~validvote2, data=data_1977)
summary(anova77)

#creating dummies for each party 
data2019_dums <- dummy_cols(data_2019, select_columns = 'party_english')

#2019 regressions 

reg19basque <- lm(data2019_dums$`party_english_Basque Country Unite`~partyvote +
                  electorate + totalvote + validvote,
                   data=data2019_dums)
summary(reg19basque)

reg19citizenry <- lm(data2019_dums$`party_english_Citizens -- Party of the Citizenry`~partyvote+
                       electorate + totalvote + validvote,
                     data=data2019_dums)
summary(reg19citizenry)

reg19galacian <- lm(data2019_dums$`party_english_Galician Nationalist Block`~partyvote +
                      electorate + totalvote + validvote,
                    data=data2019_dums)
summary(reg19galacian)

reg19peoplesally <- lm(data2019_dums$`party_english_People's Alliance-Party / People's Party`~
                         partyvote + electorate + totalvote+ validvote,
                       data=data2019_dums)
summary(reg19peoplesally)

reg19socialists <- lm(data2019_dums$`party_english_Spanish Socialist Workers Party`~partyvote +
                        electorate + totalvote + validvote, 
                      data=data2019_dums)
summary(reg19socialists)

reg19catalonia <- lm(data2019_dums$`party_english_Together for Catalonia`~partyvote +
                       electorate + totalvote + validvote,
                     data=data2019_dums)
summary(reg19catalonia)

reg19voice <- lm(data2019_dums$party_english_Voice~partyvote + electorate +
                   totalvote + validvote, 
                 data = data2019_dums)
summary(reg19voice)

#1977 regression 

data_1977_dums <- dummy_cols(data_1977, select_columns = 'party_english')
attach(data_1977_dums)

reg77basqueleft <- lm(merged$`party_english_Basque Country Left`~validvote2 +
                        totalvote+validvote+electorate,
                      data=merged)
summary(reg77basqueleft)

reg77basquenat <- lm(merged$`party_english_Basque Nationalist Party`~
                       validvote2 +totalvote+validvote+electorate,
                     data=merged)
summary(reg77basquenat)

reg77catalan <- lm(merged$`party_english_Catalan Union and Catalan Christian Democrats`~
                     validvote2 + totalvote +validvote + electorate,
                   merged)
summary(reg77catalan)

reg77centre <- lm(merged$`party_english_Centre Independent Aragonese Candidacy `~
                    validvote2+validvote+totalvote+electorate, merged)
summary(reg77centre)

reg77citizens <- lm(merged$party_english_Citizens ~ validvote2+
                      totalvote + electorate + validvote, merged)
summary(reg77citizens)

reg77comparty <- lm(merged$`party_english_Communist Party of Spain`~
                      validvote2 + validvote+ totalvote + electorate,
                      merged)
summary(reg77comparty)

reg77comunion <- lm(merged$`party_english_Communist Unification of Spain `~
                      validvote2 + validvote + electorate + totalvote, 
                    merged)
summary(reg77comunion)

reg77demcatalonia <-lm(merged$`party_english_Democratic Pact for Catalonia`~
                         validvote2 + validvote + totalvote + electorate,
                       data=merged)
summary(reg77demcatalonia)

reg77indcentre <- lm(merged$`party_english_Independent Centre Candidacy `~
                       validvote2+ validvote + totalvote + electorate,
                     data=merged)
summary(reg77indcentre)

reg77leftcat <- lm(merged$`party_english_Left of Catalonia-Electoral Democratic Front`~
                     validvote2 + validvote + electorate + totalvote,
                   data=merged)
summary(reg77leftcat)

reg77others <- lm(merged$party_english_Others~validvote2 + 
                    validvote + electorate + totalvote, 
                      data=merged)
summary(reg77others)

reg77peoplesally <- lm(merged$`party_english_People's Alliance`~
                         validvote2 + validvote + electorate + totalvote, 
                       data=merged)
summary(reg77peoplesally)

reg77peoplessocialist <- lm(merged$`party_english_People's Socialist Party-Socialist Unity`~
                              validvote2 + validvote + electorate + totalvote, 
                            data=merged)
summary(reg77peoplessocialist)

reg77socialistparty <- lm(merged$`party_english_Socialists' Party of Catalonia`~
                            validvote2 + validvote + totalvote + electorate, 
                          data=merged)
summary(reg77socialistparty)

reg77unifiedsoc <- lm(merged$`party_english_Unified Socialist Party of Catalonia`~
                        validvote2 + validvote + totalvote + electorate, 
                      data=merged)
summary(reg77unifiedsoc)

reg77uniondemcentre <- lm(merged$`party_english_Union of the Democratic Centre`~
                            validvote2 + validvote + electorate + totalvote,
                          data=merged)
summary(reg77uniondemcentre)


merged <- merge(data_1977_dums, data2019_dums, 
                by='regionname')
attach(merged)

#plotting regression results 2019 
plot1 <- plot_summs(reg19basque, reg19catalonia, reg19citizenry,
           reg19galacian, reg19peoplesally, reg19socialists,
           reg19voice, 
           model.names = c('Basque Country Unite', 
                'Together for Catalonia',
                'Party of the Citizenry',
                'Galician Nationalist Block',
                'Peoples Alliance Party',
                'Spanish Socialists Workers Party',
                'Voice'), 
          legend.title = 'Independent Variables',
          coefs = c('Votes received'='partyvote',
          'Number of total votes cast'='totalvote',
          'Number of valid votes'='validvote')) 


plot1 + theme_classic() + 
  xlab('Coefficient Estimate') + ylab('Dependent Variables') +
  ggtitle('Regression results using 2019 Spanish election data')
  
          
#1977 plot of regression results 
plot2 <- plot_summs(reg77basquenat, reg77basqueleft, reg77citizens,
                    reg77peoplesally, reg77peoplessocialist, 
                    reg77uniondemcentre,
                    model.names = c('Basque Nationalist Party',
                'Basque Country Left',
                'Citizens',
                'Peoples Alliance',
                'Peoples Socialist Party-Socialist Unity',
                'Union of the Democratic Centre'),
                legend.title='Independent Variables',
                coefs=c('Votes received'='validvote2',
                        'Number of total votes cast'='totalvote',
                        'Number of valid votes'='validvote'))

plot2 + theme_classic() + 
  xlab('Coefficient Estimate') + ylab('Dependent Variables') +
  ggtitle('Regression results using 1977 Spanish election data')









