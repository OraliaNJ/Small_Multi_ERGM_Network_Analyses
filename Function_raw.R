#Regions
#Importations
# n_r_Imp = row_regions_diff_Imp()
row_regions_diff_Imp <- function(path2, sel_p)
{
  library(ergm.multi)
  library(data.table)
  path1 = "/home/oralian/Documents/Multi_ERGM_2025/WITS-Partners"
  path = path1
  {
    #REGIONS
    #142
    file_name = "East_Asia_&_Pacific.csv"
    n_East_Asia_Pacific <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #143
    file_name = "Europe_&_Central_Asia.csv"
    n_Europe_Central_Asia <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #144
    file_name = "Latin_America_&_Caribbean.csv"
    n_Latin_America_Caribbean <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #145
    file_name = "Middle_East_&_North_Africa.csv"
    n_Middle_East_North_Africa <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #146
    file_name = "North_America.csv"
    n_North_America <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #147
    file_name = "South_Asia.csv"
    n_South_Asia <-  extract_regions_diff_Imp(file_name, path1, path2, sel_p)
    #148
    file_name = "Sub_Saharan_Africa.csv"
    n_Saharan_Africa <- extract_regions_diff_Imp(file_name, path1, path2, sel_p)
  }
  #To show all the connections in different layers
  #Multinetworks
  {
    n <- Networks(n_East_Asia_Pacific,n_Europe_Central_Asia,n_Latin_America_Caribbean,
                 n_Middle_East_North_Africa,n_North_America,n_South_Asia,n_Saharan_Africa)
  }
  return(n)
}

#Tariffs
# n_r_Tar = row_regions_Tar(sel_p)
row_regions_Tar <- function(sel_p)
{
  library(ergm.multi)
  library(data.table)
  path1 = "/home/oralian/Documents/Multi_ERGM_2025/WITS-Partners"
  path = path1
  {
    #REGIONS
    #142
    file_name = "East_Asia_&_Pacific.csv"
    n_East_Asia_Pacific <-  extract_regions_Tar(file_name, path1, sel_p)
    #143
    file_name = "Europe_&_Central_Asia.csv"
    n_Europe_Central_Asia <-  extract_regions_Tar(file_name, path1, sel_p)
    #144
    file_name = "Latin_America_&_Caribbean.csv"
    n_Latin_America_Caribbean <-  extract_regions_Tar(file_name, path1, sel_p)
    #145
    file_name = "Middle_East_&_North_Africa.csv"
    n_Middle_East_North_Africa <-  extract_regions_Tar(file_name, path1, sel_p)
    #146
    file_name = "North_America.csv"
    n_North_America <-  extract_regions_Tar(file_name, path1, sel_p)
    #147
    file_name = "South_Asia.csv"
    n_South_Asia <-  extract_regions_Tar(file_name, path1, sel_p)
    #148
    file_name = "Sub_Saharan_Africa.csv"
    n_Saharan_Africa <- extract_regions_Tar(file_name, path1, sel_p)
  }
  #To show all the connections in different layers
  #Multinetworks
  {
    n <- Networks(n_East_Asia_Pacific,n_Europe_Central_Asia,n_Latin_America_Caribbean,
                  n_Middle_East_North_Africa,n_North_America,n_South_Asia,n_Saharan_Africa)
  }
  return(n)
}

#Countries
#Importations
# n_c_Imp = row_countries_diff_Imp(path2, sel_p)
row_countries_diff_Imp <- function(path2, sel_p)
{
  library(ergm.multi)
  library(data.table)
  path1 = "/home/oralian/Documents/Multi_ERGM_2025/WITS-Partners"
  path = path1
  {
    #1
    file_name = "Albania_22.csv"
    n_Albania <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #2
    file_name = "Andorra_22.csv"
    n_Andorra <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #3
    file_name = "Angola_22.csv"
    n_Angola <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #4
    file_name = "Argentina_22.csv"
    n_Argentina<- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #5
    file_name = "Armenia_22.csv"
    n_Armenia<- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #6
    file_name = "Australia_22.csv"
    n_Australia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #7
    file_name = "Austria_22.csv"
    n_Austria <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #8
    file_name = "Azerbaokan_22.csv"
    n_Azerbaokan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #9
    file_name = "Bahrain_22.csv"
    n_Bahrain <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #10
    file_name = "Barbados_22.csv"
    n_Barbados <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #11
    file_name = "Belgium_22.csv"
    n_Belgium <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #12
    file_name = "Belize_22.csv"
    n_Belize <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #13
    file_name = "Benin_22.csv"
    n_Benin <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #14
    file_name = "Bolivia_22.csv"
    n_Bolivia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #15
    file_name = "Bosnia_and_Herzegovina_22.csv"
    n_Bosnia_and_Herzegovina <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #16
    file_name = "Botswana_22.csv"
    n_Botswana <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #17
    file_name = "Brunei_22.csv"
    n_Brunei <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #18
    file_name = "Bulgaria_22.csv"
    n_Bulgaria <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #19
    file_name = "Burkina_Faso_22.csv"
    n_Burkina_Faso <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #20
    file_name = "Burundi_22.csv"
    n_Burundi <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #21
    file_name = "Cambodia_22.csv"
    n_Cambodia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #22
    file_name = "Canada_22.csv"
    n_Canada <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #23
    file_name = "Central_African_Republic_22.csv"
    n_Central_African_Republic <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #24
    file_name = "Chile_22.csv"
    n_Chile <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #25
    file_name = "Colombia_22.csv"
    n_Colombia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #26
    file_name = "Costa_Rica_22.csv"
    n_Costa_Rica <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #27
    file_name = "Cote_dIvoire_22.csv"
    n_Cote_dIvoire <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #28
    file_name = "Croatia_22.csv"
    n_Croatia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #29
    file_name = "Cuba_22.csv"
    n_Cuba <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #30
    file_name = "Cyprus_22.csv"
    n_Cyprus <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #31
    file_name = "Czech_Republic_22.csv"
    n_Czech_Republic <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #32
    file_name = "Democratic_Rep_Congo_22.csv"
    n_Democratic_Rep_Congo <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #33
    file_name = "Denmark_22.csv"
    n_Denmark <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #34
    file_name = "Djibouti_22.csv"
    n_Djibouti <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #35
    file_name = "Dominica_22.csv"
    n_Dominica <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #36
    file_name = "Dominican_Rep_22.csv"
    n_Dominican_Rep <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #37
    file_name = "East_Timor_22.csv"
    n_East_Timor <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #38
    file_name = "Ecuador_22.csv"
    n_Ecuador <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #39
    file_name = "Egypt_22.csv"
    n_Egypt <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #40
    file_name = "El_salvador_22.csv"
    n_El_salvador <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #41
    file_name = "Fiji_22.csv"
    n_Fiji <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #42
    file_name = "Finland_22.csv"
    n_Finland <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #43
    file_name = "France_22.csv"
    n_France <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #44
    file_name = "Gabon_22.csv"
    n_Gabon <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #45
    file_name = "Georgia_22.csv"
    n_Georgia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #46
    file_name = "Germany_22.csv"
    n_Germany <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #47
    file_name = "Ghana_22.csv"
    n_Ghana <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #48
    file_name = "Greece_22.csv"
    n_Greece <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #49
    file_name = "Grenada_22.csv"
    n_Grenada <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #50
    file_name = "Guatemala_22.csv"
    n_Guatemala  <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #51
    file_name = "Guyana_22.csv"
    n_Guyana <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #52
    file_name = "Hong_Kong_22.csv"
    n_Hong_Kong <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #51
    file_name = "Hungary_22.csv"
    n_Hungary <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #52
    file_name = "Iceland_22.csv"
    n_Iceland <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #53
    file_name = "India_22.csv"
    n_India <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #54
    file_name = "Indonesia_22.csv"
    n_Indonesia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #55
    file_name = "Iran_22.csv"
    n_Iran <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #56
    file_name = "Ireland_22.csv"
    n_Ireland <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #57
    file_name = "Israel_22.csv"
    n_Israel <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #58
    file_name = "Italy_22.csv"
    n_Italy <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #59
    file_name = "Jamaica_22.csv"
    n_Jamaica <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #60
    file_name = "Japan_22.csv"
    n_Japan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #61
    file_name = "Jordan_22.csv"
    n_Jordan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #62
    file_name = "Kazakhstan_22.csv"
    n_Kazakhstan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #63
    file_name = "Kenya_22.csv"
    n_Kenya <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #64
    file_name = "Kuwait_22.csv"
    n_Kuwait <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #65
    file_name = "Latvia_22.csv"
    n_Latvia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #66
    file_name = "Lebanon_22.csv"
    n_Lebanon <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #67
    file_name = "Lesotho_22.csv"
    n_Lesotho <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #68
    file_name = "Liberia_22.csv"
    n_Liberia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #69
    file_name = "Lithuania_22.csv"
    n_Lithuania <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #70
    file_name = "Luxembourg_22.csv"
    n_Luxembourg <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #71
    file_name = "Madagascar_22.csv"
    n_Madagascar <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #72
    file_name = "Malawi_22.csv"
    n_Malawi <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #73
    file_name = "Malaysia_22.csv"
    n_Malaysia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #74
    file_name = "Maldives_22.csv"
    n_Maldives <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #75
    file_name = "Maldova_22.csv"
    n_Maldova <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #76
    file_name = "Malta_22.csv"
    n_Malta <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #77
    file_name = "Mauritania_22.csv"
    n_Mauritania <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #78
    file_name = "Mauritius_22.csv"
    n_Mauritius <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #79
    file_name = "Mexico_22.csv"
    n_Mexico <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #80
    file_name = "Mongolia_22.csv"
    n_Mongolia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #81
    file_name = "Montenegro_22.csv"
    n_Montenegro <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #82
    file_name = "Morocco_22.csv"
    n_Morocco <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #83
    file_name = "Mozambique_22.csv"
    n_Mozambique <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #84
    file_name = "Myanmar_22.csv"
    n_Myanmar <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #85
    file_name = "Namibia_22.csv"
    n_Namibia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #86
    file_name = "Nepal_22.csv"
    n_Nepal <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #87
    file_name = "Netherlands_22.csv"
    n_Netherlands <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #88
    file_name = "New_Zealand_22.csv"
    n_New_Zealand <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #89
    file_name = "Nicaragua_22.csv"
    n_Nicaragua <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #90
    file_name = "Niger_22.csv"
    n_Niger <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #91
    file_name = "Nigeria_22.csv"
    n_Nigeria <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #92
    file_name = "North_Macedonia_22.csv"
    n_North_Macedonia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #93
    file_name = "Norway_22.csv"
    n_Norway <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #94
    file_name = "Oman_22.csv"
    n_Oman <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #95
    file_name = "Pakistan_22.csv"
    n_Pakistan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #96
    file_name = "Panama_22.csv"
    n_Panama <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #97
    file_name = "Paraguay_22.csv"
    n_Paraguay <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #98
    file_name = "Peru_22.csv"
    n_Peru <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #99
    file_name = "Philippines_22.csv"
    n_Philippines <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #100
    file_name = "Poland_22.csv"
    n_Poland <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #101
    file_name = "Portugal_22.csv"
    n_Portugal <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #102
    file_name = "Qatar_22.csv"
    n_Qatar <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #103
    file_name = "Rep_Korea_22.csv"
    n_Rep_Korea <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #104
    file_name = "Romania_22.csv"
    n_Romania <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #105
    file_name = "Rwanda_22.csv"
    n_Rwanda <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #106
    file_name = "Samoa_22.csv"
    n_Samoa <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #107
    file_name = "Sao_Tome_and_Principe_22.csv"
    n_Sao_Tome_and_Principe <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #108
    file_name = "Senegal_22.csv"
    n_Senegal <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #109
    file_name = "Serbia_22.csv"
    n_Serbia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #110
    file_name = "Seychelles_22.csv"
    n_Seychelles <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #111
    file_name = "Singapore_22.csv"
    n_Singapore <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #112
    file_name = "Siri_Lanka_22.csv"
    n_Siri_Lanka <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #113
    file_name = "Slovak_22.csv"
    n_Slovak <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #114
    file_name = "Slovenia_22.csv"
    n_Slovenia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #115
    file_name = "South_Africa_22.csv"
    n_South_Africa <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #116
    file_name = "Spain_22.csv"
    n_Spain <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #117
    file_name = "St_Vicent_22.csv"
    n_St_Vicent <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #118
    file_name = "Suriname_22.csv"
    n_Suriname <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #119
    file_name = "Sweden_22.csv"
    n_Sweden <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #120
    file_name = "Switzerland_22.csv"
    n_Switzerland <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #121
    file_name = "Tajikistan_22.csv"
    n_Tajikistan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #122
    file_name = "Tanzania_22.csv"
    n_Tanzania <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #123
    file_name = "Thailand_22.csv"
    n_Thailand <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #124
    file_name = "The_Bahamas_22.csv"
    n_The_Bahamas <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #125
    file_name = "The_Gambia_22.csv"
    n_The_Gambia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #126
    file_name = "Togo_22.csv"
    n_Togo <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #127
    file_name = "Trinidad_and_Tobago_22.csv"
    n_Trinidad_and_Tobago <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #128
    file_name = "Tunisia_22.csv"
    n_Tunisia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #129
    file_name = "Turkey_22.csv"
    n_Turkey <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #130
    file_name = "Uganda_22.csv"
    n_Uganda <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #131
    file_name = "Ukraine_22.csv"
    n_Ukraine <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #132
    file_name = "United_Arab_Emirates_22.csv"
    n_United_Arab_Emirates <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #133
    file_name = "United_Kingdom_22.csv"
    n_United_Kingdom <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #134
    file_name = "Uruguay_22.csv"
    n_Uruguay <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #135
    file_name = "Uzbekistan_22.csv"
    n_Uzbekistan <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #136
    file_name = "Vietnam_22.csv"
    n_Vietnam <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #137
    file_name = "Zambia_22.csv"
    n_Zambia <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #138
    file_name = "Zimbabwe_22.csv"
    n_Zimbabwe <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #139
    file_name = "Brazil_22.csv"
    n_Brazil <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #140
    file_name = "China_22.csv"
    n_China <-extract_country_diff_Imp(file_name, path1, path2, sel_p)
    #141
    file_name = "United_States_22.csv"
    n_US <- extract_country_diff_Imp(file_name, path1, path2, sel_p)
  }
  
  
  #To show all the connections in different layers
  #Multinetworks
  {
    n<- Networks(n_US,n_China,n_Brazil,n_Albania,n_Andorra,n_Angola,n_Argentina,n_Armenia,
                 n_Australia,n_Austria,n_Azerbaokan,n_Bahrain,n_Barbados,n_Belgium,n_Belize,
                 n_Benin,n_Bolivia,n_Bosnia_and_Herzegovina,n_Botswana,n_Brunei,n_Bulgaria,
                 n_Burkina_Faso,n_Burundi,n_Cambodia,n_Canada,n_Central_African_Republic,n_Chile,
                 n_Colombia,n_Costa_Rica, n_Cote_dIvoire,n_Croatia,n_Cuba,n_Cyprus,n_Czech_Republic,
                 n_Democratic_Rep_Congo,n_Denmark, n_Djibouti,n_Dominica,n_Dominican_Rep,n_East_Timor,
                 n_Ecuador,n_Egypt,n_El_salvador,n_Fiji,n_Finland,n_France,n_Gabon,n_Georgia,n_Germany,
                 n_Ghana,n_Greece,n_Grenada,n_Guatemala,n_Guyana,n_Hong_Kong,n_Hungary,n_Iceland,n_India,
                 n_Indonesia,n_Iran,n_Ireland,n_Israel,n_Italy,n_Jamaica,n_Japan,n_Jordan,n_Kazakhstan,
                 n_Kenya,n_Kuwait,n_Latvia,n_Lebanon,n_Lesotho,n_Liberia,n_Lithuania,n_Luxembourg,n_Madagascar,
                 n_Malawi,n_Malaysia,n_Maldova,n_Malta,n_Mauritania,n_Mauritius,n_Mexico,n_Mongolia,n_Montenegro,
                 n_Morocco,n_Mozambique,n_Myanmar,n_Namibia,n_Nepal,n_Netherlands,n_New_Zealand,n_Nicaragua,n_Niger,
                 n_Nigeria,n_North_Macedonia,n_Norway,n_Oman,n_Pakistan,n_Panama,n_Paraguay,n_Peru,n_Philippines,
                 n_Poland,n_Portugal,n_Qatar,n_Rep_Korea,n_Romania,n_Rwanda,n_Samoa,n_Sao_Tome_and_Principe,n_Senegal,
                 n_Serbia,n_Seychelles,n_Singapore,n_Siri_Lanka,n_Slovak,n_South_Africa,n_Spain,n_St_Vicent,n_Suriname,
                 n_Sweden,n_Switzerland,n_Tajikistan,n_Tanzania,n_Thailand,n_The_Bahamas,n_The_Gambia,n_Togo,
                 n_Trinidad_and_Tobago,n_Tunisia,n_Turkey,n_Uganda,n_Ukraine,n_United_Arab_Emirates,n_United_Kingdom,
                 n_Uruguay,n_Uzbekistan,n_Vietnam,n_Zambia,n_Zimbabwe)
  }
  return(n)
}

#Tariffs
# n_c_Tar = row_countries_Tar(sel_p)
row_countries_Tar <- function(sel_p)
{
  library(ergm.multi)
  library(data.table)
  path1= "/home/oralian/Documents/Multi_ERGM_2025/WITS-Partners"
  path = path1
  {
    #1
    file_name = "Albania_22.csv"
    n_Albania <- extract_country_tar(file_name, path1, sel_p)
    #2
    file_name = "Andorra_22.csv"
    n_Andorra <- extract_country_tar(file_name, path1, sel_p)
    #3
    file_name = "Angola_22.csv"
    n_Angola <- extract_country_tar(file_name, path1, sel_p)
    #4
    file_name = "Argentina_22.csv"
    n_Argentina<- extract_country_tar(file_name, path1, sel_p)
    #5
    file_name = "Armenia_22.csv"
    n_Armenia<- extract_country_tar(file_name, path1, sel_p)
    #6
    file_name = "Australia_22.csv"
    n_Australia <- extract_country_tar(file_name, path1, sel_p)
    #7
    file_name = "Austria_22.csv"
    n_Austria <- extract_country_tar(file_name, path1, sel_p)
    #8
    file_name = "Azerbaokan_22.csv"
    n_Azerbaokan <- extract_country_tar(file_name, path1, sel_p)
    #9
    file_name = "Bahrain_22.csv"
    n_Bahrain <- extract_country_tar(file_name, path1, sel_p)
    #10
    file_name = "Barbados_22.csv"
    n_Barbados <- extract_country_tar(file_name, path1, sel_p)
    #11
    file_name = "Belgium_22.csv"
    n_Belgium <- extract_country_tar(file_name, path1, sel_p)
    #12
    file_name = "Belize_22.csv"
    n_Belize <- extract_country_tar(file_name, path1, sel_p)
    #13
    file_name = "Benin_22.csv"
    n_Benin <- extract_country_tar(file_name, path1, sel_p)
    #14
    file_name = "Bolivia_22.csv"
    n_Bolivia <- extract_country_tar(file_name, path1, sel_p)
    #15
    file_name = "Bosnia_and_Herzegovina_22.csv"
    n_Bosnia_and_Herzegovina <- extract_country_tar(file_name, path1, sel_p)
    #16
    file_name = "Botswana_22.csv"
    n_Botswana <- extract_country_tar(file_name, path1, sel_p)
    #17
    file_name = "Brunei_22.csv"
    n_Brunei <- extract_country_tar(file_name, path1, sel_p)
    #18
    file_name = "Bulgaria_22.csv"
    n_Bulgaria <- extract_country_tar(file_name, path1, sel_p)
    #19
    file_name = "Burkina_Faso_22.csv"
    n_Burkina_Faso <- extract_country_tar(file_name, path1, sel_p)
    #20
    file_name = "Burundi_22.csv"
    n_Burundi <- extract_country_tar(file_name, path1, sel_p)
    #21
    file_name = "Cambodia_22.csv"
    n_Cambodia <- extract_country_tar(file_name, path1, sel_p)
    #22
    file_name = "Canada_22.csv"
    n_Canada <- extract_country_tar(file_name, path1, sel_p)
    #23
    file_name = "Central_African_Republic_22.csv"
    n_Central_African_Republic <- extract_country_tar(file_name, path1, sel_p)
    #24
    file_name = "Chile_22.csv"
    n_Chile <- extract_country_tar(file_name, path1, sel_p)
    #25
    file_name = "Colombia_22.csv"
    n_Colombia <- extract_country_tar(file_name, path1, sel_p)
    #26
    file_name = "Costa_Rica_22.csv"
    n_Costa_Rica <- extract_country_tar(file_name, path1, sel_p)
    #27
    file_name = "Cote_dIvoire_22.csv"
    n_Cote_dIvoire <- extract_country_tar(file_name, path1, sel_p)
    #28
    file_name = "Croatia_22.csv"
    n_Croatia <- extract_country_tar(file_name, path1, sel_p)
    #29
    file_name = "Cuba_22.csv"
    n_Cuba <- extract_country_tar(file_name, path1, sel_p)
    #30
    file_name = "Cyprus_22.csv"
    n_Cyprus <- extract_country_tar(file_name, path1, sel_p)
    #31
    file_name = "Czech_Republic_22.csv"
    n_Czech_Republic <- extract_country_tar(file_name, path1, sel_p)
    #32
    file_name = "Democratic_Rep_Congo_22.csv"
    n_Democratic_Rep_Congo <- extract_country_tar(file_name, path1, sel_p)
    #33
    file_name = "Denmark_22.csv"
    n_Denmark <- extract_country_tar(file_name, path1, sel_p)
    #34
    file_name = "Djibouti_22.csv"
    n_Djibouti <- extract_country_tar(file_name, path1, sel_p)
    #35
    file_name = "Dominica_22.csv"
    n_Dominica <- extract_country_tar(file_name, path1, sel_p)
    #36
    file_name = "Dominican_Rep_22.csv"
    n_Dominican_Rep <- extract_country_tar(file_name, path1, sel_p)
    #37
    file_name = "East_Timor_22.csv"
    n_East_Timor <- extract_country_tar(file_name, path1, sel_p)
    #38
    file_name = "Ecuador_22.csv"
    n_Ecuador <- extract_country_tar(file_name, path1, sel_p)
    #39
    file_name = "Egypt_22.csv"
    n_Egypt <- extract_country_tar(file_name, path1, sel_p)
    #40
    file_name = "El_salvador_22.csv"
    n_El_salvador <- extract_country_tar(file_name, path1, sel_p)
    #41
    file_name = "Fiji_22.csv"
    n_Fiji <- extract_country_tar(file_name, path1, sel_p)
    #42
    file_name = "Finland_22.csv"
    n_Finland <- extract_country_tar(file_name, path1, sel_p)
    #43
    file_name = "France_22.csv"
    n_France <- extract_country_tar(file_name, path1, sel_p)
    #44
    file_name = "Gabon_22.csv"
    n_Gabon <- extract_country_tar(file_name, path1, sel_p)
    #45
    file_name = "Georgia_22.csv"
    n_Georgia <- extract_country_tar(file_name, path1, sel_p)
    #46
    file_name = "Germany_22.csv"
    n_Germany <- extract_country_tar(file_name, path1, sel_p)
    #47
    file_name = "Ghana_22.csv"
    n_Ghana <- extract_country_tar(file_name, path1, sel_p)
    #48
    file_name = "Greece_22.csv"
    n_Greece <- extract_country_tar(file_name, path1, sel_p)
    #49
    file_name = "Grenada_22.csv"
    n_Grenada <- extract_country_tar(file_name, path1, sel_p)
    #50
    file_name = "Guatemala_22.csv"
    n_Guatemala  <- extract_country_tar(file_name, path1, sel_p)
    #51
    file_name = "Guyana_22.csv"
    n_Guyana <- extract_country_tar(file_name, path1, sel_p)
    #52
    file_name = "Hong_Kong_22.csv"
    n_Hong_Kong <- extract_country_tar(file_name, path1, sel_p)
    #51
    file_name = "Hungary_22.csv"
    n_Hungary <- extract_country_tar(file_name, path1, sel_p)
    #52
    file_name = "Iceland_22.csv"
    n_Iceland <- extract_country_tar(file_name, path1, sel_p)
    #53
    file_name = "India_22.csv"
    n_India <- extract_country_tar(file_name, path1, sel_p)
    #54
    file_name = "Indonesia_22.csv"
    n_Indonesia <- extract_country_tar(file_name, path1, sel_p)
    #55
    file_name = "Iran_22.csv"
    n_Iran <- extract_country_tar(file_name, path1, sel_p)
    #56
    file_name = "Ireland_22.csv"
    n_Ireland <- extract_country_tar(file_name, path1, sel_p)
    #57
    file_name = "Israel_22.csv"
    n_Israel <- extract_country_tar(file_name, path1, sel_p)
    #58
    file_name = "Italy_22.csv"
    n_Italy <- extract_country_tar(file_name, path1, sel_p)
    #59
    file_name = "Jamaica_22.csv"
    n_Jamaica <- extract_country_tar(file_name, path1, sel_p)
    #60
    file_name = "Japan_22.csv"
    n_Japan <- extract_country_tar(file_name, path1, sel_p)
    #61
    file_name = "Jordan_22.csv"
    n_Jordan <- extract_country_tar(file_name, path1, sel_p)
    #62
    file_name = "Kazakhstan_22.csv"
    n_Kazakhstan <- extract_country_tar(file_name, path1, sel_p)
    #63
    file_name = "Kenya_22.csv"
    n_Kenya <- extract_country_tar(file_name, path1, sel_p)
    #64
    file_name = "Kuwait_22.csv"
    n_Kuwait <- extract_country_tar(file_name, path1, sel_p)
    #65
    file_name = "Latvia_22.csv"
    n_Latvia <- extract_country_tar(file_name, path1, sel_p)
    #66
    file_name = "Lebanon_22.csv"
    n_Lebanon <- extract_country_tar(file_name, path1, sel_p)
    #67
    file_name = "Lesotho_22.csv"
    n_Lesotho <- extract_country_tar(file_name, path1, sel_p)
    #68
    file_name = "Liberia_22.csv"
    n_Liberia <- extract_country_tar(file_name, path1, sel_p)
    #69
    file_name = "Lithuania_22.csv"
    n_Lithuania <- extract_country_tar(file_name, path1, sel_p)
    #70
    file_name = "Luxembourg_22.csv"
    n_Luxembourg <- extract_country_tar(file_name, path1, sel_p)
    #71
    file_name = "Madagascar_22.csv"
    n_Madagascar <- extract_country_tar(file_name, path1, sel_p)
    #72
    file_name = "Malawi_22.csv"
    n_Malawi <- extract_country_tar(file_name, path1, sel_p)
    #73
    file_name = "Malaysia_22.csv"
    n_Malaysia <- extract_country_tar(file_name, path1, sel_p)
    #74
    file_name = "Maldives_22.csv"
    n_Maldives <- extract_country_tar(file_name, path1, sel_p)
    #75
    file_name = "Maldova_22.csv"
    n_Maldova <- extract_country_tar(file_name, path1, sel_p)
    #76
    file_name = "Malta_22.csv"
    n_Malta <- extract_country_tar(file_name, path1, sel_p)
    #77
    file_name = "Mauritania_22.csv"
    n_Mauritania <- extract_country_tar(file_name, path1, sel_p)
    #78
    file_name = "Mauritius_22.csv"
    n_Mauritius <- extract_country_tar(file_name, path1, sel_p)
    #79
    file_name = "Mexico_22.csv"
    n_Mexico <- extract_country_tar(file_name, path1, sel_p)
    #80
    file_name = "Mongolia_22.csv"
    n_Mongolia <- extract_country_tar(file_name, path1, sel_p)
    #81
    file_name = "Montenegro_22.csv"
    n_Montenegro <- extract_country_tar(file_name, path1, sel_p)
    #82
    file_name = "Morocco_22.csv"
    n_Morocco <- extract_country_tar(file_name, path1, sel_p)
    #83
    file_name = "Mozambique_22.csv"
    n_Mozambique <- extract_country_tar(file_name, path1, sel_p)
    #84
    file_name = "Myanmar_22.csv"
    n_Myanmar <- extract_country_tar(file_name, path1, sel_p)
    #85
    file_name = "Namibia_22.csv"
    n_Namibia <- extract_country_tar(file_name, path1, sel_p)
    #86
    file_name = "Nepal_22.csv"
    n_Nepal <- extract_country_tar(file_name, path1, sel_p)
    #87
    file_name = "Netherlands_22.csv"
    n_Netherlands <- extract_country_tar(file_name, path1, sel_p)
    #88
    file_name = "New_Zealand_22.csv"
    n_New_Zealand <- extract_country_tar(file_name, path1, sel_p)
    #89
    file_name = "Nicaragua_22.csv"
    n_Nicaragua <- extract_country_tar(file_name, path1, sel_p)
    #90
    file_name = "Niger_22.csv"
    n_Niger <- extract_country_tar(file_name, path1, sel_p)
    #91
    file_name = "Nigeria_22.csv"
    n_Nigeria <- extract_country_tar(file_name, path1, sel_p)
    #92
    file_name = "North_Macedonia_22.csv"
    n_North_Macedonia <- extract_country_tar(file_name, path1, sel_p)
    #93
    file_name = "Norway_22.csv"
    n_Norway <- extract_country_tar(file_name, path1, sel_p)
    #94
    file_name = "Oman_22.csv"
    n_Oman <- extract_country_tar(file_name, path1, sel_p)
    #95
    file_name = "Pakistan_22.csv"
    n_Pakistan <- extract_country_tar(file_name, path1, sel_p)
    #96
    file_name = "Panama_22.csv"
    n_Panama <- extract_country_tar(file_name, path1, sel_p)
    #97
    file_name = "Paraguay_22.csv"
    n_Paraguay <- extract_country_tar(file_name, path1, sel_p)
    #98
    file_name = "Peru_22.csv"
    n_Peru <- extract_country_tar(file_name, path1, sel_p)
    #99
    file_name = "Philippines_22.csv"
    n_Philippines <- extract_country_tar(file_name, path1, sel_p)
    #100
    file_name = "Poland_22.csv"
    n_Poland <- extract_country_tar(file_name, path1, sel_p)
    #101
    file_name = "Portugal_22.csv"
    n_Portugal <- extract_country_tar(file_name, path1, sel_p)
    #102
    file_name = "Qatar_22.csv"
    n_Qatar <- extract_country_tar(file_name, path1, sel_p)
    #103
    file_name = "Rep_Korea_22.csv"
    n_Rep_Korea <- extract_country_tar(file_name, path1, sel_p)
    #104
    file_name = "Romania_22.csv"
    n_Romania <- extract_country_tar(file_name, path1, sel_p)
    #105
    file_name = "Rwanda_22.csv"
    n_Rwanda <- extract_country_tar(file_name, path1, sel_p)
    #106
    file_name = "Samoa_22.csv"
    n_Samoa <- extract_country_tar(file_name, path1, sel_p)
    #107
    file_name = "Sao_Tome_and_Principe_22.csv"
    n_Sao_Tome_and_Principe <- extract_country_tar(file_name, path1, sel_p)
    #108
    file_name = "Senegal_22.csv"
    n_Senegal <- extract_country_tar(file_name, path1, sel_p)
    #109
    file_name = "Serbia_22.csv"
    n_Serbia <- extract_country_tar(file_name, path1, sel_p)
    #110
    file_name = "Seychelles_22.csv"
    n_Seychelles <- extract_country_tar(file_name, path1, sel_p)
    #111
    file_name = "Singapore_22.csv"
    n_Singapore <- extract_country_tar(file_name, path1, sel_p)
    #112
    file_name = "Siri_Lanka_22.csv"
    n_Siri_Lanka <- extract_country_tar(file_name, path1, sel_p)
    #113
    file_name = "Slovak_22.csv"
    n_Slovak <- extract_country_tar(file_name, path1, sel_p)
    #114
    file_name = "Slovenia_22.csv"
    n_Slovenia <- extract_country_tar(file_name, path1, sel_p)
    #115
    file_name = "South_Africa_22.csv"
    n_South_Africa <- extract_country_tar(file_name, path1, sel_p)
    #116
    file_name = "Spain_22.csv"
    n_Spain <- extract_country_tar(file_name, path1, sel_p)
    #117
    file_name = "St_Vicent_22.csv"
    n_St_Vicent <- extract_country_tar(file_name, path1, sel_p)
    #118
    file_name = "Suriname_22.csv"
    n_Suriname <- extract_country_tar(file_name, path1, sel_p)
    #119
    file_name = "Sweden_22.csv"
    n_Sweden <- extract_country_tar(file_name, path1, sel_p)
    #120
    file_name = "Switzerland_22.csv"
    n_Switzerland <- extract_country_tar(file_name, path1, sel_p)
    #121
    file_name = "Tajikistan_22.csv"
    n_Tajikistan <- extract_country_tar(file_name, path1, sel_p)
    #122
    file_name = "Tanzania_22.csv"
    n_Tanzania <- extract_country_tar(file_name, path1, sel_p)
    #123
    file_name = "Thailand_22.csv"
    n_Thailand <- extract_country_tar(file_name, path1, sel_p)
    #124
    file_name = "The_Bahamas_22.csv"
    n_The_Bahamas <- extract_country_tar(file_name, path1, sel_p)
    #125
    file_name = "The_Gambia_22.csv"
    n_The_Gambia <- extract_country_tar(file_name, path1, sel_p)
    #126
    file_name = "Togo_22.csv"
    n_Togo <- extract_country_tar(file_name, path1, sel_p)
    #127
    file_name = "Trinidad_and_Tobago_22.csv"
    n_Trinidad_and_Tobago <- extract_country_tar(file_name, path1, sel_p)
    #128
    file_name = "Tunisia_22.csv"
    n_Tunisia <- extract_country_tar(file_name, path1, sel_p)
    #129
    file_name = "Turkey_22.csv"
    n_Turkey <- extract_country_tar(file_name, path1, sel_p)
    #130
    file_name = "Uganda_22.csv"
    n_Uganda <- extract_country_tar(file_name, path1, sel_p)
    #131
    file_name = "Ukraine_22.csv"
    n_Ukraine <- extract_country_tar(file_name, path1, sel_p)
    #132
    file_name = "United_Arab_Emirates_22.csv"
    n_United_Arab_Emirates <- extract_country_tar(file_name, path1, sel_p)
    #133
    file_name = "United_Kingdom_22.csv"
    n_United_Kingdom <- extract_country_tar(file_name, path1, sel_p)
    #134
    file_name = "Uruguay_22.csv"
    n_Uruguay <- extract_country_tar(file_name, path1, sel_p)
    #135
    file_name = "Uzbekistan_22.csv"
    n_Uzbekistan <- extract_country_tar(file_name, path1, sel_p)
    #136
    file_name = "Vietnam_22.csv"
    n_Vietnam <- extract_country_tar(file_name, path1, sel_p)
    #137
    file_name = "Zambia_22.csv"
    n_Zambia <- extract_country_tar(file_name, path1, sel_p)
    #138
    file_name = "Zimbabwe_22.csv"
    n_Zimbabwe <- extract_country_tar(file_name, path1, sel_p)
    #139
    file_name = "Brazil_22.csv"
    n_Brazil <- extract_country_tar(file_name, path1, sel_p)
    #140
    file_name = "China_22.csv"
    n_China <-extract_country_tar(file_name, path1, sel_p)
    #141
    file_name = "United_States_22.csv"
    n_US <- extract_country_tar(file_name, path1, sel_p)
  }
  
  
  #To show all the connections in different layers
  #Multinetworks
  {
    n<- Networks(n_US,n_China,n_Brazil,n_Albania,n_Andorra,n_Angola,n_Argentina,n_Armenia,
                 n_Australia,n_Austria,n_Azerbaokan,n_Bahrain,n_Barbados,n_Belgium,n_Belize,
                 n_Benin,n_Bolivia,n_Bosnia_and_Herzegovina,n_Botswana,n_Brunei,n_Bulgaria,
                 n_Burkina_Faso,n_Burundi,n_Cambodia,n_Canada,n_Central_African_Republic,n_Chile,
                 n_Colombia,n_Costa_Rica, n_Cote_dIvoire,n_Croatia,n_Cuba,n_Cyprus,n_Czech_Republic,
                 n_Democratic_Rep_Congo,n_Denmark, n_Djibouti,n_Dominica,n_Dominican_Rep,n_East_Timor,
                 n_Ecuador,n_Egypt,n_El_salvador,n_Fiji,n_Finland,n_France,n_Gabon,n_Georgia,n_Germany,
                 n_Ghana,n_Greece,n_Grenada,n_Guatemala,n_Guyana,n_Hong_Kong,n_Hungary,n_Iceland,n_India,
                 n_Indonesia,n_Iran,n_Ireland,n_Israel,n_Italy,n_Jamaica,n_Japan,n_Jordan,n_Kazakhstan,
                 n_Kenya,n_Kuwait,n_Latvia,n_Lebanon,n_Lesotho,n_Liberia,n_Lithuania,n_Luxembourg,n_Madagascar,
                 n_Malawi,n_Malaysia,n_Maldova,n_Malta,n_Mauritania,n_Mauritius,n_Mexico,n_Mongolia,n_Montenegro,
                 n_Morocco,n_Mozambique,n_Myanmar,n_Namibia,n_Nepal,n_Netherlands,n_New_Zealand,n_Nicaragua,n_Niger,
                 n_Nigeria,n_North_Macedonia,n_Norway,n_Oman,n_Pakistan,n_Panama,n_Paraguay,n_Peru,n_Philippines,
                 n_Poland,n_Portugal,n_Qatar,n_Rep_Korea,n_Romania,n_Rwanda,n_Samoa,n_Sao_Tome_and_Principe,n_Senegal,
                 n_Serbia,n_Seychelles,n_Singapore,n_Siri_Lanka,n_Slovak,n_South_Africa,n_Spain,n_St_Vicent,n_Suriname,
                 n_Sweden,n_Switzerland,n_Tajikistan,n_Tanzania,n_Thailand,n_The_Bahamas,n_The_Gambia,n_Togo,
                 n_Trinidad_and_Tobago,n_Tunisia,n_Turkey,n_Uganda,n_Ukraine,n_United_Arab_Emirates,n_United_Kingdom,
                 n_Uruguay,n_Uzbekistan,n_Vietnam,n_Zambia,n_Zimbabwe)
  }
  return(n)
}
