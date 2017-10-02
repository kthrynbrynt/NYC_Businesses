#Global calls

library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(maps)


#Data
DCA = fread('/Users/kathrynbryant/nycdsa/ShinyProject/DCA.csv')

#NOTE: > View(DCA) results in the following error, although the data
#frame functions normally otherwise:
# Error in if (col_min < col_max) { : missing value where TRUE/FALSE needed

#Rename all columns with spaces in their names. Note that there are two
#columns that have borough info: Boro and Borough. They seem to agree 
#in the places they both have values, but one column will often have
#values when the other column does not. 

DCA = DCA %>% rename(License.Number = `DCA License Number`, 
                     License.Type = `License Type`,
                     License.Expire.Date = `License Expiration Date`,
                     Category = `License Category`,
                     Name = `Business Name`,
                     Name2 = `Business Name 2`,
                     Building.Num = `Address Building`,
                     Street = `Address Street Name`,
                     Secondary.Street = `Secondary Address Street Name`,
                     City = `Address City`,
                     State = `Address State`,
                     Postcode = `Address Postcode`,
                     Contact.Phone = `Contact Phone Number`,
                     Boro = `Address Borough`,
                     Community.Board = `Comminity Board`,
                     Census.Tract = `Census Tract`,
                     Council.District = `Council District`
                     )

#Change all character columns (stringsAsFactors = FALSE by default in 
#data.table package) that should be factors to factors, except for Boro
#and Borough - they need to be cleaned as characters first.

DCA$License.Type = as.factor(DCA$License.Type)
DCA$License.Expire.Date = as.Date(DCA$License.Expire.Date, 
                                  format = '%d/%m/%Y')
DCA$Category = as.factor(DCA$Category)
DCA$City = as.factor(DCA$City)
DCA$State = as.factor(DCA$State)
DCA$Postcode = as.factor(DCA$Postcode)
DCA$Council.District = as.factor(DCA$Council.District)

#Note: Contact.Phone includes some values with dashes and some without. If used,
#this column will need to be cleaned.


#Make the Boro and Borough columns identical and then remove Boro. 
#Finally, make Borough into a factor.

DCA$Boro = toupper(DCA$Boro)
DCA = DCA %>% mutate(Borough = ifelse(Borough != Boro, Boro, Borough)) %>%
   select(-Boro)

DCA$Borough = as.factor(DCA$Borough)
DCA = DCA %>% filter(complete.cases(Longitude, Latitude))



industries = levels(DCA$Category)


NY = map('state', fill = FALSE, plot = TRUE, 
         region = "new york")












##########################



test = DCA %>% filter(Category %in% c("Amusement Arcade", "Laundry")) %>%
   select(Longitude)

head(test)

class(test)











