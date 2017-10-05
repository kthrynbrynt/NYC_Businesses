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

DCA = DCA %>% filter(complete.cases(Longitude, Latitude, Borough)) %>%
   filter(Borough %in% c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", 
                         "STATEN ISLAND"))

DCA$Borough = as.factor(DCA$Borough)


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
   mutate(Industry = gsub(pattern = "Pedicab Business", 
                                      replacement = "Pedicab", 
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
types = levels(DCA$License.Type)

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
                     
Industry = levels(DCA$Industry)
Descriptions = c('An Amusement Arcade License is required for any business that will have ten or more amusement devices located on its premises.',
'Any business that owns or rents a space where a public auction occurs must have an Auction House (Premises) license. Only licensed Auctioneers may act as an Auctioneer at the Auction House.',                  
'A Bingo Game Operator License is required for any non-profit organization that will operate bingo games. Please note that for-profit entities are not eligible to apply for the Bingo Game Operator License. Bingo games are those in which designated numbers or symbols on a card are matched with numbers or symbols selected at random, and prizes are awarded accordingly.',	
'A business must have a Booting Company License to place wheel locks on vehicles. Wheel locks, also known as boots, are used to enforce parking rules for private parking lots or streets by preventing illegally parked vehicles from being moved until a charge is paid.',
'The City licenses bars, clubs, taverns, and discos that allow dancing. A place that is open to the public and sells food or drinks must have a Cabaret License to allow customers to dance.',
'A Catering Establishment License is required for any room, place, or space where entertainment is permitted and food and / or beverages will be served (such as a catering hall or restaurant) for a particular event which is closed to the general public.',	
'Any business that sells cigarettes directly to consumers must have a Cigarette Retail Dealer License issued by the Department of Consumer Affairs.	A license must be obtained for each location in New York City where the business sells cigarettes.',
'A Commercial Lessor License is required for a business that will lease its premises to non-profit organizations for the purpose of conducting charitable gaming activities, such as bingo games and games of chance.',
'A person or business must have a Dealer in Products for the Disabled license if primarily involved in the selling, renting, repairing or adjusting of any instrument or device designed for the disabled (for example: braces, crutches, wheelchairs, etc.).',	
'Any business that seeks to collect personal or household debts from New York City residents must have a Debt Collection Agency License. This licensing requirement includes "debt buyers," businesses that buy overdue debt, and then try to collect money sometimes by taking consumers to court.',	
'The City licenses businesses that repair electronics and appliances. A person or business must have an Electronic and Home Appliance Service Dealer License to repair or service electronic equipment and home appliances.',
'A business must have an Electronics Store license if it sells audio equipment, video equipment, photographic equipment, computers or computer equipment or calculators. A license is not required if: (1) The area used to display electronic goods is less than 20 percent of the total display area or (2) Less than 30 electronic items are on display.',
'An Employment Agency License is required for any business that, for a fee, will procure or attempt to procure employment or engagements for individuals, or will assist employers in procuring employees.',	
'A Games of Chance License is required for any non-profit organization that will operate games of chance in which prizes are awarded on the basis of a designated winning number(s), color(s), or symbol(s), determined by chance.	These games fall into three general categories: Las Vegas Night, Bell Jars, Raffles.',
'A Gaming Cafe License is required for any business that will offer three or more publicly accessible computers or electronic devices in which game software has been installed, for the purpose of playing games on the premises.',	
'The City licenses garages and parking lots that charge money to house five or more vehicles. A business must have a Garage License to store vehicles in an entirely enclosed space. It must have a Parking Lot License to store vehicles in an unenclosed space. A combination Garage and Parking Lot License is required to store vehicles in both enclosed and unenclosed spaces.',
'A General Vendor license is required for anyone that will sell, lease, or offer to sell or lease goods or services in a public space that is not a store.',
'You must have a Home Improvement Contractor License to perform construction, repair, remodeling, or other home improvement work costing more than $200 to any residential building or land.',
'The City licenses horse drawn carriage drivers and businesses. A person or business must have a Horse Drawn Cab Owner License to own a horse drawn carriage that operates for hire. Individuals must have a Horse Drawn Cab Driver License to drive a horse drawn carriage.',
'A business must have a Laundry License to wash, dry, starch, or iron clothes or other fabric for the public. A Laundry License is also required to offer the public self-service washing by automatic or coin-operated machines.',
'A Newsstand License is required for a business that will operate a stand or booth on a public sidewalk that is not readily removable and is primarily used for the sale of newspapers and periodicals. A newsstand may sell cigarettes, lottery tickets, and prepackaged snacks if the business has obtained the appropriate licenses and permits.',
'A person or business must have a Pawnbroker License to sell pawned or pledged goods, loan money on deposit or pledge of personal property, or purchase personal property on the condition of selling it back at a set price.',
'The City licenses pedicab businesses and drivers. Businesses must have a Pedicab Business License to own or lease pedicabs. Drivers must have a Pedicab Driver License to drive a pedicab for hire.',
'A Pool or Billiard Room License is required for any business that will have three or more pool, billiard or pocket billiard tables, for customer use on the premises.',
'A Process Serving Agency License is required for a person or business that assigns, distributes, or delivers process to another for actual service.',	
'A person or business must have a Scale Dealer or Repairer License to trade, sell, receive, or repair condemned, rebuilt, or used weighing or measuring devices.',
'A Scrap Metal Processor license is required to operate or maintain a business engaged primarily in the purchase, processing, and shipment of ferrous or non-ferrous scrap.',	
'A person or business must have a Secondhand Dealer General License to buy or sell secondhand items. This does not include the purchase or sale of used cars or firearms, which require separate Licenses.',
'A Sidewalk Cafe License and revocable consent are required for a business that operates a portion of a restaurant on a public sidewalk. There are three types of sidewalk cafés: Enclosed, unenclosed, and small unenclosed.',  
'A Sightseeing Bus License is required for any business that will arrange the operation of one or more motor vehicles that seats eight or more passengers, and operates these motor vehicles for hire, either to bring passengers from a fixed point in New York City to places of interest, or is used for a special trip or excursion from a starting point within New York City.',	
'A Special Sale License is required for any business that will have a public sale or offer merchandise for sale in connection with a declared purpose (such as a going out of business, liquidation, loss of lease, or renovation sale, or a sale due to fire, smoke, or water damage).',	
'A person or business must have a Stoop Line Stand License to sell items from a stand outside of and directly next to an existing store or retail establishment. Stands fall into three categories, based on what they sell: (1) Fruits, vegetables, soft drinks or flowers; (2) Confectionery, cigars, cigarettes or tobacco; (3) Ice cream.',
'A person or business must have a Storage Warehouse License to store goods for money. A license is not required for self-storage businesses.',
'A Tow Truck Company License is required for a business that tows vehicles for profit within the five boroughs of New York City.')

DCA_Describe = data.frame(Industry, Descriptions)

#NY = map('state', fill = FALSE, plot = TRUE, 
         #region = "new york")

########
#Amusement Arcade/Device: An Amusement Arcade License is required for any business that will have ten or more amusement devices located on its premises.
#Bingo Game Operator: A Bingo Game Operator License is required for any non-profit organization that will operate bingo games. Please note that for-profit entities are not eligible to apply for the Bingo Game Operator License. Bingo games are those in which designated numbers or symbols on a card are matched with numbers or symbols selected at random, and prizes are awarded accordingly.	
#Booting Company: A business must have a Booting Company License to place wheel locks on vehicles. Wheel locks, also known as boots, are used to enforce parking rules for private parking lots or streets by preventing illegally parked vehicles from being moved until a charge is paid.
#Cabaret: The City licenses bars, clubs, taverns, and discos that allow dancing. A place that is open to the public and sells food or drinks must have a Cabaret License to allow customers to dance.
#Catering Establishment: A Catering Establishment License is required for any room, place, or space where entertainment is permitted and food and / or beverages will be served (such as a catering hall or restaurant) for a particular event which is closed to the general public.	
#Cigarette Retail Dealer: Any business that sells cigarettes directly to consumers must have a Cigarette Retail Dealer License issued by the Department of Consumer Affairs.	A license must be obtained for each location in New York City where the business sells cigarettes.
#Commercial Lessor: A Commercial Lessor License is required for a business that will lease its premises to non-profit organizations for the purpose of conducting charitable gaming activities, such as bingo games and games of chance.
#Dealer in Products: A person or business must have a Dealer in Products for the Disabled license if primarily involved in the selling, renting, repairing or adjusting of any instrument or device designed for the disabled (for example: braces, crutches, wheelchairs, etc.).	
#Debt Collection Agency: Any business that seeks to collect personal or household debts from New York City residents must have a Debt Collection Agency License. This licensing requirement includes "debt buyers," businesses that buy overdue debt, and then try to collect money sometimes by taking consumers to court.	
#Electronic & Appliance Service: The City licenses businesses that repair electronics and appliances. A person or business must have an Electronic and Home Appliance Service Dealer License to repair or service electronic equipment and home appliances.
#Electronics Store: A business must have an Electronics Store license if it sells audio equipment, video equipment, photographic equipment, computers or computer equipment or calculators. A license is not required if: (1) The area used to display electronic goods is less than 20 percent of the total display area or (2) Less than 30 electronic items are on display.
#Employment Agency: An Employment Agency License is required for any business that, for a fee, will procure or attempt to procure employment or engagements for individuals, or will assist employers in procuring employees.	
#Games of Chance: A Games of Chance License is required for any non-profit organization that will operate games of chance in which prizes are awarded on the basis of a designated winning number(s), color(s), or symbol(s), determined by chance.	These games fall into three general categories: Las Vegas Night, Bell Jars, Raffles.
#Gaming Cafe: A Gaming Cafe License is required for any business that will offer three or more publicly accessible computers or electronic devices in which game software has been installed, for the purpose of playing games on the premises.	
#Garage/Parking Lot: The City licenses garages and parking lots that charge money to house five or more vehicles. A business must have a Garage License to store vehicles in an entirely enclosed space. It must have a Parking Lot License to store vehicles in an unenclosed space. A combination Garage and Parking Lot License is required to store vehicles in both enclosed and unenclosed spaces.
#General Vendor/Distributor: A General Vendor license is required for anyone that will sell, lease, or offer to sell or lease goods or services in a public space that is not a store.
#Home Improvement: You must have a Home Improvement Contractor License to perform construction, repair, remodeling, or other home improvement work costing more than $200 to any residential building or land.
#Horse Drawn Cab: The City licenses horse drawn carriage drivers and businesses. A person or business must have a Horse Drawn Cab Owner License to own a horse drawn carriage that operates for hire. Individuals must have a Horse Drawn Cab Driver License to drive a horse drawn carriage.
#Laundry: A business must have a Laundry License to wash, dry, starch, or iron clothes or other fabric for the public. A Laundry License is also required to offer the public self-service washing by automatic or coin-operated machines.
#Newsstand: A Newsstand License is required for a business that will operate a stand or booth on a public sidewalk that is not readily removable and is primarily used for the sale of newspapers and periodicals. A newsstand may sell cigarettes, lottery tickets, and prepackaged snacks if the business has obtained the appropriate licenses and permits.
#Pawnbroker: A person or business must have a Pawnbroker License to sell pawned or pledged goods, loan money on deposit or pledge of personal property, or purchase personal property on the condition of selling it back at a set price.
#Pedicab Business: The City licenses pedicab businesses and drivers. Businesses must have a Pedicab Business License to own or lease pedicabs. Drivers must have a Pedicab Driver License to drive a pedicab for hire.
#Pool or Billiard Room: A Pool or Billiard Room License is required for any business that will have three or more pool, billiard or pocket billiard tables, for customer use on the premises.
#Process Serving Agency: A Process Serving Agency License is required for a person or business that assigns, distributes, or delivers process to another for actual service.	
#Scale Dealer Repairer: A person or business must have a Scale Dealer or Repairer License to trade, sell, receive, or repair condemned, rebuilt, or used weighing or measuring devices.
#Scrap Metal Processor: A Scrap Metal Processor license is required to operate or maintain a business engaged primarily in the purchase, processing, and shipment of ferrous or non-ferrous scrap.	
#Secondhand Dealer: A person or business must have a Secondhand Dealer General License to buy or sell secondhand items. This does not include the purchase or sale of used cars or firearms, which require separate Licenses.
#Sidewalk Cafe: A Sidewalk Cafe License and revocable consent are required for a business that operates a portion of a restaurant on a public sidewalk. There are three types of sidewalk cafés: Enclosed, unenclosed, and small unenclosed.  
#Sightseeing Bus: A Sightseeing Bus License is required for any business that will arrange the operation of one or more motor vehicles that seats eight or more passengers, and operates these motor vehicles for hire, either to bring passengers from a fixed point in New York City to places of interest, or is used for a special trip or excursion from a starting point within New York City.	
#Special Sale: A Special Sale License is required for any business that will have a public sale or offer merchandise for sale in connection with a declared purpose (such as a going out of business, liquidation, loss of lease, or renovation sale, or a sale due to fire, smoke, or water damage).	
#Stoop Line Stand: A person or business must have a Stoop Line Stand License to sell items from a stand outside of and directly next to an existing store or retail establishment. Stands fall into three categories, based on what they sell: (1) Fruits, vegetables, soft drinks or flowers; (2) Confectionery, cigars, cigarettes or tobacco; (3) Ice cream.
#Storage Warehouse: A person or business must have a Storage Warehouse License to store goods for money. A license is not required for self-storage businesses.
#Tow Truck Company: A Tow Truck Company License is required for a business that tows vehicles for profit within the five boroughs of New York City.	