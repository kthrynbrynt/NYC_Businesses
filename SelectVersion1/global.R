#Global calls

library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(maps)


#Data
DCA = fread('/Users/kathrynbryant/nycdsa/ShinyApp/DCA.csv')

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
                     ) %>%
   mutate(Industry = Category)

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


#Reduce the number of levels in Industry:
DCA = DCA %>% mutate(Industry = gsub(pattern = "Amusement Arcade", 
                                      replacement = "Amusement Arcade/Device", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Amusement Device Permanent", 
                                      replacement = "Amusement Arcade/Device", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Amusement Device Portable", 
                                      replacement = "Amusement Arcade/Device", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Amusement Device Temporary", 
                                      replacement = "Amusement Arcade/Device", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Dealer in Products", 
                                      replacement = "Disability Products", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Home Improvement Contractor", 
                                      replacement = "Home Improvement", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Home Improvement Salesperson", 
                                      replacement = "Home Improvement", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Horse Drawn Cab Owner", 
                                      replacement = "Horse Drawn Cab", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Horse Drawn Driver", 
                                      replacement = "Horse Drawn Cab", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Laundries", 
                                      replacement = "Laundry", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Laundry Jobber", 
                                      replacement = "Laundry", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Secondhand Dealer - Auto", 
                                      replacement = "Secondhand Dealer", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Secondhand Dealer - Firearms", 
                                      replacement = "Secondhand Dealer", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Secondhand Dealer - General", 
                                      replacement = "Secondhand Dealer", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Tow Truck Exemption", 
                                      replacement = "Tow Truck Company", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "Parking Lot", 
                                      replacement = "BBB", 
                                     x = Industry))%>%
   mutate(Industry = gsub(pattern = "Garage", 
                                      replacement = "AAA", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "AAA and BBB", 
                                      replacement = "Garage/Parking Lot", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "BBB", 
                                      replacement = "Garage/Parking Lot", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "AAA", 
                                      replacement = "Garage/Parking Lot", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "General Vendor", 
                                      replacement = "CCC", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "CCC Distributor", 
                                      replacement = "General Vendor/Distributor", 
                                     x = Industry)) %>%
   mutate(Industry = gsub(pattern = "CCC", 
                                      replacement = "General Vendor/Distributor", 
                                     x = Industry))

DCA$Industry = as.factor(DCA$Industry)

industries = levels(DCA$Industry)

DCA = DCA %>% mutate(Icon = Industry)
DCA$Icon = as.factor(DCA$Icon)
levels(DCA$Icon) = c("game-pad", "bullhorn", "square", "car", "eye", "cutlery",
                     "fire", "group", "wheelchair-alt", "usd", "support", "tv",
                     "handshake-o", "game-pad", "game-pad", "car", "truck", 
                     "wrench", "map-pin", "lemon-o", "newspaper-o", "usd",
                     "taxi", "game-pad", "gear", "balance-scale", "asterisk",
                     "handshake-o", "coffee", "bus", "shopping-cart", "newspaper-o",
                     "archive", "truck")
DCA$Icon = as.character(DCA$Icon)
                     


#NY = map('state', fill = FALSE, plot = TRUE, 
         #region = "new york")

########
#Amusement Arcade/Device:
#Bingo Game Operator:
#Booting Company:
#Cabaret:
#Catering Establishment:
#Cigarette Retail Dealer:
#Commercial Lessor:
#Dealer in Products: A person or business must have a Dealer in Products for the Disabled license if primarily involved in the selling, renting, repairing or adjusting of any instrument or device designed for the disabled (for example: braces, crutches, wheelchairs, etc.).	
#Debt Collection Agency:
#Electronic & Appliance Service:
#Electronics Store:
#Employment Agency:
#Games of Chance: A Games of Chance License is required for any non-profit organization that will operate games of chance in which prizes are awarded on the basis of a designated winning number(s), color(s), or symbol(s), determined by chance.	These games fall into three general categories: Las Vegas Night, Bell Jars, Raffles.
#Gaming Cafe: A Gaming Cafe License is required for any business that will offer three or more publicly accessible computers or electronic devices in which game software has been installed, for the purpose of playing games on the premises.	
#Garage/Parking Lot: The City licenses garages and parking lots that charge money to house five or more vehicles. A business must have a Garage License to store vehicles in an entirely enclosed space. It must have a Parking Lot License to store vehicles in an unenclosed space. A combination Garage and Parking Lot License is required to store vehicles in both enclosed and unenclosed spaces.
#