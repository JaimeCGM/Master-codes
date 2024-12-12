
xtset cowcode year

label variable executive_power "Executive Power Index"
label variable minister_mandarin "Career separation"
label variable scope "Scope of constitution Index"
label variable tradition "Administrative Tradition"
label variable br_pres "Presidential system"
label variable ln_gdp "Log of GDP"
label variable ln_gdp_pc "Log of GDPpc"
label variable ln_pop "Log of population"
label variable length "Length of constitution (in words)"

label variable germanic_dum "Germanic"
label variable anglosaxon_dum "Anglo-Saxon"
label variable napoleonic_dum "Napoleonic"
label variable scandinavian_dum "Scandinavian"
label variable asian_dum "Asian"
label variable latin_dum "Latin-American"
label variable soviet_dum "Non-western/soviet"

label variable stability "Political stability"

// replace missing values of ICELAND and LUXEMBOURG for ethnic fractionalization
replace fe_etfra = 0.069440501 if country == "Iceland"
replace fe_etfra = 0.42393 if country == "Luxembourg"

//DESCRIPTIVE
asdoc sum executive_power minister_mandarin year scope corruption effectiveness stability regulatory rulelaw accountability ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, label

cd "C:\Users\jaime\Desktop\Thesis data\Final Dataset\Graphs\Desccriptive graphs\tradition_graphs"
xtline m_corruption, overlay i(tradition) t(year)
graph export g1.gph
xtline m_effectiveness, overlay i(tradition) t(year)
graph export g2.gph
xtline m_stability, overlay i(tradition) t(year)
graph export g3.gph
xtline m_regulatory, overlay i(tradition) t(year)
graph export g4.gph
xtline m_rulelaw, overlay i(tradition) t(year)
graph export g5.gph
xtline m_accountability, overlay i(tradition) t(year)
graph export g6.gph
graph combine g1.gph g2.gph g3.gph g4.gph g5.gph g6.gph
grc1leg g1.gph g2.gph g3.gph g4.gph g5.gph g6.gph, legendfrom(g1.gph)


label variable m_executive_power "Mean of the Executive Power Index"
xtline m_executive_power, overlay i(tradition) t(year)


//means
egen m_corruption = mean(corruption), by(tradition year)
egen m_effectiveness = mean(effectiveness), by(tradition year)
egen m_stability = mean(stability), by (tradition year)
egen m_regulatory = mean(regulatory), by (tradition year)
egen m_rulelaw = mean(rulelaw), by (tradition year)
egen m_accountability = mean(accountability), by (tradition year)


egen m_executive_power = mean(executive_power), by(tradition year)
xtline m_executive_power, overlay i(tradition) t(year)

egen m_mandarin = mean(minister_mandarin), by(tradition year)


//REGRESSION
//Indep. 
asdoc corr executive_power minister_mandarin scope germanic_dum anglosaxon_dum soviet_dum napoleonic_dum scandinavian_dum latin_dum asian_dum ln_gdp_pc ln_pop br_pres fe_etfra, abb(.) label

asdoc corr corruption effectiveness stability regulatory rulelaw accountability, label

ttest executive_power == minister_mandarin

//setting panel data
xtset cowcode year 


xtreg effectiveness executive_power minister_mandarin germanic_dum anglosaxon_dum napoleonic_dum scandinavian_dum asian_dum latin_dum scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, re robust

anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum
//no interaction
xtreg effectiveness c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, replace
xtreg corruption c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append
xtreg stability c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append
xtreg regulatory c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append
xtreg rulelaw c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append
xtreg accountability c.executive_power minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append

//With dummies
xtreg effectiveness c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, replace label
xtreg corruption c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append label
xtreg stability c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append label
xtreg regulatory c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append label
xtreg rulelaw c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append label
xtreg accountability c.executive_power minister_mandarin anglosaxon_dum napoleonic_dum germanic_dum scandinavian_dum soviet_dum asian_dum latin_dum scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append label


//Interaction - continuous
xtreg effectiveness c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, replace 
xtreg corruption c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg stability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg regulatory c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg rulelaw c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg accountability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 


//Interaction - continuous without FRACTIONALIZATION
xtreg effectiveness c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, replace 
xtreg corruption c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, append 
xtreg stability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, append 
xtreg regulatory c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, append 
xtreg rulelaw c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, append 
xtreg accountability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres, re robust
outreg2 using myreg.doc, append 


//Margins
set more off

xtreg effectiveness c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m1
xtreg stability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m2
xtreg regulatory c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m3
xtreg rulelaw c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m4
xtreg accountability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m5
xtreg corruption c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
margins minister_mandarin, at( executive_power ==(1(1)7))
marginsplot
graph save m6

graph combine m1.gph m2.gph m3.gph m4.gph m5.gph m6.gph
grc1leg m1.gph m2.gph m3.gph m4.gph m5.gph m6.gph, legendfrom(m1.gph)


//Diagnostics
xtreg effectiveness c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict effect, xb
predict Epsilon, e
twoway (scatter Epsilon Fitted), ytitle(Epsilon residuals) xtitle(Fitted values)

xtreg corruption c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict corrup, xb
predict corrupt2, e
twoway (scatter corrup corrupt2), ytitle(Epsilon residuals) xtitle(Fitted values)

xtreg stability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict stab, xb
predict stab2, e
twoway (scatter stab stab2), ytitle(Epsilon residuals) xtitle(Fitted values)

xtreg regulatory c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict reg, xb
predict reg2, e
twoway (scatter reg reg2), ytitle(Epsilon residuals) xtitle(Fitted values)

xtreg rulelaw c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict rule, xb
predict rule2, e
twoway (scatter rule rule2), ytitle(Epsilon residuals) xtitle(Fitted values)

xtreg accountability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
predict acc, xb
predict acc2, e
twoway (scatter acc acc2), ytitle(Epsilon residuals) xtitle(Fitted values)






xtreg effectiveness c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, replace 
xtreg corruption c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg stability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg regulatory c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg rulelaw c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg accountability c.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 










//Interaction - ordinal
xtreg effectiveness i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, replace keep i.executive_power##minister_mandarin i.ntradition addtext (Controls, YES)
xtreg corruption i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg stability i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg regulatory i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg rulelaw i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 
xtreg accountability i.executive_power##minister_mandarin i.ntradition scope ln_gdp_pc ln_pop br_pres fe_etfra, re robust
outreg2 using myreg.doc, append 



//Hausman
xtreg effectiveness executive_power##minister_mandarin germanic_dum anglosaxon_dum napoleonic_dum scandinavian_dum asian_dum latin_dum scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, fe
estimates store fixed
xtreg effectiveness executive_power##minister_mandarin germanic_dum anglosaxon_dum napoleonic_dum scandinavian_dum scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, re
estimates store random
hausman fixed random


xtrc effectiveness executive_power##minister_mandarin germanic_dum anglosaxon_dum napoleonic_dum scandinavian_dum length scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra

CLUSTERING????
xtreg effectiveness executive_power minister_mandarin length scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, re robust 
xtreg effectiveness executive_power minister_mandarin length scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, fe robust 
test length scope br_pres fe_etfra
test executive_power


xtreg effectiveness executive_power#minister_mandarin executive_power minister_mandarin germanic_dum anglosaxon_dum napoleonic_dum scandinavian_dum length scope ln_gdp ln_gdp_pc ln_pop br_pres fe_etfra, re robust
test executive_power
test length scope br_pres fe_etfra

