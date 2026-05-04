
global datadate "2026-01-30"
use "Z:\Outcome Data/$datadate\MAT_NUTR.dta", clear
global output "D:\Users\savannah.omalley\Documents\Maternal Outcomes\Maternal Nutrition Outcomes Report\Output/$datadate"
cap mkdir "$output"
*set graph style

ssc install grstyle
grstyle init
grstyle color background white
grstyle color major_grid dimgray
grstyle linewidth major_grid thin
grstyle yesno draw_major_hgrid yes
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
grstyle anglestyle vertical_tick horizontal


grstyle set color "0 59 92" "168 153 104" "0 156 222" "248 224 142" "166 85 35" "0 130 100"


*foreach let in "ANC20" "ANC32" "PNC6" {
	foreach let in "T1" "T2" "T3"  "PNC6" {
	gen crp_`let' = 0 if CRP_`let' ==1
	replace crp_`let' = 1 if CRP_`let' ==2 
	
	gen agp_`let' = 0 if AGP_`let' ==1
	replace agp_`let' = 1 if AGP_`let' ==2
	
	gen ferr70_`let' = 0 if FERRITIN_70_`let' ==1
	replace ferr70_`let' = 1 if FERRITIN_70_`let' ==2
	
	gen tg_`let' = 0 if HIGH_TG_44_`let' ==1
	replace tg_`let' = 1 if HIGH_TG_44_`let' ==2
}

*foreach let in "ANC20" "ANC32" "PNC6" {
foreach let in "T1" "T2" "T3"  "PNC6" {
	gen stfr_`let' =0 if inrange(STFR_`let',1,2)
	replace stfr_`let' =1 if STFR_`let'==3
	
	
	gen rbp4_`let' =0 if inrange(RBP4_`let', 3,4)
	replace rbp4_`let' =1 if inrange(RBP4_`let', 1,2)
}
*CRP
graph bar crp_*, by(SITE,legend(pos(6)) title("Prevalence of elevated CRP") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.2"20%" 0.4"40%" 0.6"60%")  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )
graph export "$output/CRP.png", as(png) name("Graph") replace
*AGP
graph bar agp_*, by(SITE,legend(pos(6)) title("Prevalence of elevated AGP") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.2"20%" 0.4"40%" 0.6"60%")  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )
graph export "$output/AGP.png", as(png) name("Graph") replace

*Ferritin
graph bar ferr70_*, by(SITE,legend(pos(6)) title("Prevalence of low ferritin") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.2"20%" 0.4"40%" 0.6"60%")  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )
graph export "$output/FERRITIN70.png", as(png) name("Graph") replace

*Tg
graph bar tg_*, by(SITE,legend(pos(6)) title("Prevalence of high thyroglobulin") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.2"20%" 0.4"40%" 0.6"60%")  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )
graph export "$output/TG.png", as(png) name("Graph") replace

*Stfr
graph bar stfr_*, by(SITE,legend(pos(6)) title("Prevalence of high sTfR") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.2"20%" 0.4"40%" 0.6"60%")  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )

graph export "$output/STFR.png", as(png) name("Graph") replace
*RBP4
graph bar rbp4_*, by(SITE,legend(pos(6)) title("Prevalence of moderate or severe VAD") note("Data from: $datadate")) bar(1, color("0 59 92"%90)) bar(2, color("168 153 104")) bar(3, color("0 156 222")) ylabel(0.1"10%" 0.2"20%" 0.3"30%" )  legend(label(1 "T1") label(2 "T2") label(3 "T3") label(4 "PNC6") ring(1)  col(3) bmargin(small)) legend( col(4) )

graph export "$output/RBP4.png", as(png) name("Graph") replace


