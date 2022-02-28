#Abrir base de datos
use "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas capítulo 4/Base_simbólica_integrada_v8.dta", clear

#Preservar datos originales
Preserve

#Collapsar 


#Generar Gráfica 4.1
twoway (connected presidentas_org_directivos_pct legentidad, mcolor(gs9) msize(medium) lcolor(gs9) lwidth(medthick)) (connected mujeres_org_directivos_pct legentidad, mcolor(black) msize(medium) msymbol(circle_hollow) lcolor(black) lwidth(medthick) lpattern(vshortdash)), ytitle(Porcentaje) ytitle(, size(medsmall)) ylabel(0(25)100, labsize(2.9) angle(horizontal) glwidth(vthin) glcolor(gs15)) xtitle(, size(zero)) xlabel(#1, labsize(3.2) valuelabel glwidth(vthin) glcolor(gs15)) legend(size(vsmall)) scheme(s1manual) xsize(20) ysize(12) by(entidad, cols(8)) subtitle(, size (medium) fcolor(white))
graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbo
> ́lica/Gráficas definitivas/Gráfica 4.1 - Órganos de dirección.gph"



#Generar Gráfica 4.3
twoway (connected mujeres_cduras_promedio legentidad, mcolor(gs9) msize(medium) lcolor(gs9) lwidth(medthick)) (connected mujeres_cblandas_promedio legentidad, mcolor(black) msize(medium) msymbol(circle_hollow) lcolor(black) lwidth(medthick) lpattern(vshortdash)), ytitle(Porcentaje) ytitle(, size(medsmall)) ylabel(0(25)100, labsize(2.9) angle(horizontal) glwidth(vthin) glcolor(gs15)) xtitle(, size(zero)) xlabel(#1, labsize(3.2) valuelabel glwidth(vthin) glcolor(gs15)) legend(size(vsmall)) scheme(s1manual) xsize(20) ysize(12) by(entidad, cols(8)) subtitle(, size(medium) fcolor(white))
graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas definitivas/Gráfica 4.2 - Integración de comisiones.gph"



#Generar Gráfica 4.4
twoway (connected presidentas_duras_pct legentidad, mcolor(gs9) msize(medium) lcolor(gs9) lwidth(medthick)) (connected presidentas_cblandas_pct legentidad, mcolor(black) msize(medium) msymbol(circle_hollow) lcolor(black) lwidth(medthick) lpattern(vshortdash)), ytitle(Porcentaje) ytitle(, size(medsmall)) ylabel(0(25)100, labsize(2.9) angle(horizontal) glwidth(vthin) glcolor(gs15)) xtitle(, size(zero)) xlabel(#1, labsize(3.2) valuelabel glwidth(vthin) glcolor(gs15)) legend(size(vsmall)) scheme(s1manual) xsize(20) ysize(12) by(entidad, cols(8)) subtitle(, size(medium) fcolor(white))
graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas definitivas/Gráfica 4.3 - Presidencias de comisiones.gph"


#Generar Gráfica 4.5
graph bar (asis) unidad estudios lenguaje, over(legentidad) nofill stack cw ylabel(0(1)3, angle(horizontal) grid glwidth(vthin) glcolor(gs15)) legend(rows(1) size(vsmall)) scheme(s1manual) xsize(20) ysize(12) by(entidad_txt, rows(4)) subtitle(, size(medlarge) fcolor(white))
graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas definitivas/Gráfica 4.5 - Unidades centros lenguaje.gph"



#Modelos órganos directivos

	#Modelo integrantes
	glm OD_integrantes c.descriptiva2 leg2 state_gpd_per_capita_rescaled percent_urban_pop avg_yr_school, fam(bin) link (logit) vce (cluster entidad)
	outreg2 using organos_directivos, word replace ctitle(integrantes) label dec(3)
	
	#Modelo presidencias
	glm OD_presidencias c.descriptiva2 leg2 state_gpd_per_capita_rescaled percent_urban_pop avg_yr_school, fam(bin) link (logit) vce (cluster entidad)
	outreg2 using organos_directivos, word append ctitle(presidencias) label dec(3)


#Modelos dinámicas internas
		
		#Modelo unidades
		glm unidad c.descriptiva2 i.legentidad  percent_urban_pop avg_yr_school state_gpd_per_capita_rescaled , link(logit) family(binomial) vce(cluster entidad) 
		
		#Modelo centros
		glm estudios c.descriptiva2 i.legentidad  percent_urban_pop avg_yr_school state_gpd_per_capita_rescaled , link(logit) family(binomial) vce(cluster entidad) 
		
		#Modelo lenguaje
		glm lenguaje c.descriptiva2 i.legentidad  percent_urban_pop avg_yr_school state_gpd_per_capita_rescaled , link(logit) family(binomial) vce(cluster entidad) 
		
		 #Gráfica predicción lenguaje
		 margins i.legentidad, at(descriptiva2=(.20(.1).70)) noci
		 marginsplot, scheme(s1manual) xsize(20) ysize(15)
		
	
	
	

	
	** falta poner abrir base en comisiones



#Modelos comisiones
	
	#Modelo integrantes
	glm mujeres_pct  c.descriptiva2 i.legentidad i.clase integrantes percent_urban_pop avg_yr_school state_gpd_per_capita_rescaled , link(logit) family(binomial) vce(cluster entidad) 
	outreg2 using comisiones_integración, word replace ctitle(Proporcion) label dec(3) 
	
	#Modelo presidencias
	glm presidentas_pct  c.descriptiva2 i.legentidad i.clase integrantes percent_urban_pop avg_yr_school state_gpd_per_capita_rescaled , link(logit) family(binomial) vce(cluster entidad) 
	outreg2 using comisiones_integración, word append ctitle(Proporcion) label dec(3)

#Gráficas modelos comisiones

	#Gráfica integrantes
	margins i.clase, at(descriptiva2=(.20(.1).70)) noci
	marginsplot,  scheme(s1manual) xsize(20) ysize(15)
	graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas definitivas/Gráfica xa - Modelo integración comisiones.gph
	
	#Gráfica presidentas
	marginsplot,  scheme(s1manual) xsize(20) ysize(15)
	graph save Graph "/Users/camilosaavedra/MEGA/Investigación/Conacyt - Representación mujeres/Representación simbólica/Gráficas definitivas/Gráfica xb - Modelo presidencias comisiones.gph"


	
