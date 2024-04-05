#libraries
pacman::p_load(shiny,shinydashboard,fresh,shinydashboardPlus,shinyjs,rmarkdown,fmsb,plotly,htmlwidgets,webshot,pagedown,pdftools,knitr)

# setting up a temporary directory
setwd(tempdir())


# writeLines for report with figures
writeLines(
  con = 'report.rmd',
  text = '---
title: "Peer Review Report"
subtitle: "Fragenkatalog zur Selbst- und Fremdbewertung in der Laboratoriumsmedizin"
output: pdf_document


params:
  name: "Name und Anschrift der Einrichtung"
  Internetadresse: "Internetadresse der Einrichtung"
  Trag: "Trägerschaft mit Erläuterung ob öffentlich oder privat"
  Zug: "Zugehörigkeit zu einem Unternehmensverbund"
  Ansprechpartner: "Ansprechpartner mit Titel und Qualifikation"
  Email: "Email-Adresse Ansprechpartner"
  Telefonnummer: "Telefonnummer Ansprechpartner"
  Berufsgruppe: "Berufsgruppe"
  Versorgung: "Versorgung / Einsender"
  sonstiges_versorgung: "Sonstiges"
  Laborbereiche: "Laborbereiche"
  sonstiges_laborbereiche: "Sonstiges"
  Leistungsspektrum: "Leistungsspektrum"
  sonstiges_leistungsspektrum: "Sonstiges"

  Anzahl_total: "Anzahl Mitarbeitende (gesamt)"
  Anzahl_mit: "Anzahl (auch Teilzeit) Ärztliches Personal"
  davon_Fachaerzte: "davon Fachärzte"
  davon_weiterbildung: "davon in Weiterbildung"
  Anzahl_Planstellen: "Anzahl Planstellen"
  davon_unbesetzt:  "davon unbesetzt"
  Anzahl_tech: "Anzahl (auch Teilzeit) technisches Personal"
  Anzahl_TPlanstellen: "Anzahl Planstellen"
  davon_Tunbesetzt: "davon unbesetzt"
  Anzahl_natur: "Anzahl (auch Teilzeit) Naturwissenschaftliches Personal"
  Anzahl_NPlanstellen: "Anzahl Planstellen"
  davon_Nunbesetzt: "davon unbesetzt"
  Anzahl_IT: "Anzahl (auch Teilzeit) IT-Mitarbeitende, die direkt dem Laborpersonalzugeordnet sind"
  Anzahl_IPlanstellen: "Anzahl Planstellen"
  davon_Iunbesetzt: "davon unbesetzt"
  Beschreibung: "Beschreibung der IT-Unterstützung, die nicht direkt dem Laborpersonal zugeordnet ist"
  WeitereInfo: "Weitere Informationen zu Personal und Stellenbesetzungen"

  AnbieterInfo: "Anbieter Laborinformationssystem"
  AnbieterOrder: "Anbieter Order-Entry"
  AnbieterMiddleware: "Anbieter Middleware, falls mehrere bitte alle eintragen"
  WeitereIT:  "Weitere für das Labor wichtige IT-Systeme"
  Angaben: "Öffnungs- und Betriebszeiten, z.B. 24/7"
  laufendenJahres: "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des laufenden Jahres"
  Vorjahres: "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des Vorjahres"
  Kompetenzschwerpunkte: "Kompetenzschwerpunkte und Alleinstellungsmerkmale"
  Ergänzung: "Es fehlt ein wichtiger Aspekt? Hier ist Raum für Ihre Ergänzung"

  Freitext1: "Selbstbewertung score"
  Selbstbewertung_text1: "Selbstbewertung"
  Freitext2: "Fremdbewertung score"
  Fremdbewertung_text2: "Fremdbewertung"

  Freitext3: "Selbstbewertung score"
  Selbstbewertung_text3: "Selbstbewertung"
  Freitext4: "Fremdbewertung score"
  Fremdbewertung_text4: "Fremdbewertung"

  Freitext5: "Selbstbewertung score"
  Selbstbewertung_text5: "Selbstbewertung"
  Freitext6: "Fremdbewertung score"
  Fremdbewertung_text6: "Fremdbewertung"

  Freitext7: "Selbstbewertung score"
  Selbstbewertung_text7: "Selbstbewertung"
  Freitext8: "Fremdbewertung score"
  Fremdbewertung_text8: "Fremdbewertung"

  Freitext9: "Selbstbewertung score"
  Selbstbewertung_text9: "Selbstbewertung"
  Freitext10: "Fremdbewertung score"
  Fremdbewertung_text10: "Fremdbewertung"

  Freitext11: "Selbstbewertung score"
  Selbstbewertung_text11: "Selbstbewertung"
  Freitext12: "Fremdbewertung score"
  Fremdbewertung_text12: "Fremdbewertung"

  Freitext13: "Selbstbewertung score"
  Selbstbewertung_text13: "Selbstbewertung"
  Freitext14: "Fremdbewertung score"
  Fremdbewertung_text14: "Fremdbewertung"

  Freitext15: "Selbstbewertung score"
  Selbstbewertung_text15: "Selbstbewertung"
  Freitext16: "Fremdbewertung score"
  Fremdbewertung_text16: "Fremdbewertung"

  p1: NA
  p2: NA
  p3: NA

  Freitext17: "Selbstbewertung score"
  Selbstbewertung_text17: "Selbstbewertung"
  Freitext18: "Fremdbewertung score"
  Fremdbewertung_text18: "Fremdbewertung"

  Freitext19: "Selbstbewertung score"
  Selbstbewertung_text19: "Selbstbewertung"
  Freitext20: "Fremdbewertung score"
  Fremdbewertung_text20: "Fremdbewertung"
  
  Freitext21: "Selbstbewertung score"
  Selbstbewertung_text21: "Selbstbewertung"
  Freitext22: "Fremdbewertung score"
  Fremdbewertung_text22: "Fremdbewertung"
  
  Freitext23: "Selbstbewertung score"
  Selbstbewertung_text23: "Selbstbewertung"
  Freitext24: "Fremdbewertung score"
  Fremdbewertung_text24: "Fremdbewertung"

  Freitext25: "Selbstbewertung score"
  Selbstbewertung_text25: "Selbstbewertung"
  Freitext26: "Fremdbewertung score"
  Fremdbewertung_text26: "Fremdbewertung"
  
  Freitext27: "Selbstbewertung score"
  Selbstbewertung_text27: "Selbstbewertung"
  Freitext28: "Fremdbewertung score"
  Fremdbewertung_text28: "Fremdbewertung"
  
  Freitext29: "Selbstbewertung score"
  Selbstbewertung_text29: "Selbstbewertung"
  Freitext30: "Fremdbewertung score"
  Fremdbewertung_text30: "Fremdbewertung"
  
  Freitext31: "Selbstbewertung score"
  Selbstbewertung_text31: "Selbstbewertung"
  Freitext32: "Fremdbewertung score"
  Fremdbewertung_text32: "Fremdbewertung"
  
  Freitext33: "Selbstbewertung score"
  Selbstbewertung_text33: "Selbstbewertung"
  Freitext34: "Fremdbewertung score"
  Fremdbewertung_text34: "Fremdbewertung"
  
  Freitext35: "Selbstbewertung score"
  Selbstbewertung_text35: "Selbstbewertung"
  Freitext36: "Fremdbewertung score"
  Fremdbewertung_text36: "Fremdbewertung"
  
  Freitext37: "Selbstbewertung score"
  Selbstbewertung_text37: "Selbstbewertung"
  Freitext38: "Fremdbewertung score"
  Fremdbewertung_text38: "Fremdbewertung"
  
  Freitext39: "Selbstbewertung score"
  Selbstbewertung_text39: "Selbstbewertung"
  Freitext40: "Fremdbewertung score"
  Fremdbewertung_text40: "Fremdbewertung"
  
  Freitext41: "Selbstbewertung score"
  Selbstbewertung_text41: "Selbstbewertung"
  Freitext42: "Fremdbewertung score"
  Fremdbewertung_text42: "Fremdbewertung"
  
  Freitext43: "Selbstbewertung score"
  Selbstbewertung_text43: "Selbstbewertung"
  Freitext44: "Fremdbewertung score"
  Fremdbewertung_text44: "Fremdbewertung"

  Freitext45: "Selbstbewertung score"
  Selbstbewertung_text45: "Selbstbewertung"
  Freitext46: "Fremdbewertung score"
  Fremdbewertung_text46: "Fremdbewertung"

  Freitext47: "Selbstbewertung score"
  Selbstbewertung_text47: "Selbstbewertung"
  Freitext48: "Fremdbewertung score"
  Fremdbewertung_text48: "Fremdbewertung"

  Freitext49: "Selbstbewertung score"
  Selbstbewertung_text49: "Selbstbewertung"
  Freitext50: "Fremdbewertung score"
  Fremdbewertung_text50: "Fremdbewertung"

  Freitext51: "Selbstbewertung score"
  Selbstbewertung_text51: "Selbstbewertung"
  Freitext52: "Fremdbewertung score"
  Fremdbewertung_text52: "Fremdbewertung"

  Freitext53: "Selbstbewertung score"
  Selbstbewertung_text53: "Selbstbewertung"
  Freitext54: "Fremdbewertung score"
  Fremdbewertung_text54: "Fremdbewertung"
  
  Freitext55: "Selbstbewertung score"
  Selbstbewertung_text55: "Selbstbewertung"
  Freitext56: "Fremdbewertung score"
  Fremdbewertung_text56: "Fremdbewertung"

  Freitext57: "Selbstbewertung score"
  Selbstbewertung_text57: "Selbstbewertung"
  Freitext58: "Fremdbewertung score"
  Fremdbewertung_text58: "Fremdbewertung"

  p4: NA
  p5: NA
  p6: NA

  Freitext59: "Selbstbewertung score"
  Selbstbewertung_text59: "Selbstbewertung"
  Freitext60: "Fremdbewertung score"
  Fremdbewertung_text60: "Fremdbewertung"

  Freitext61: "Selbstbewertung score"
  Selbstbewertung_text61: "Selbstbewertung"
  Freitext62: "Fremdbewertung score"
  Fremdbewertung_text62: "Fremdbewertung"

  Freitext63: "Selbstbewertung score"
  Selbstbewertung_text63: "Selbstbewertung"
  Freitext64: "Fremdbewertung score"
  Fremdbewertung_text64: "Fremdbewertung"

  Freitext65: "Selbstbewertung score"
  Selbstbewertung_text65: "Selbstbewertung"
  Freitext66: "Fremdbewertung score"
  Fremdbewertung_text66: "Fremdbewertung"

  Freitext67: "Selbstbewertung score"
  Selbstbewertung_text67: "Selbstbewertung"
  Freitext68: "Fremdbewertung score"
  Fremdbewertung_text68: "Fremdbewertung"

  Freitext69: "Selbstbewertung score"
  Selbstbewertung_text69: "Selbstbewertung"
  Freitext70: "Fremdbewertung score"
  Fremdbewertung_text70: "Fremdbewertung"

  Freitext71: "Selbstbewertung score"
  Selbstbewertung_text71: "Selbstbewertung"
  Freitext72: "Fremdbewertung score"
  Fremdbewertung_text72: "Fremdbewertung"

  Freitext73: "Selbstbewertung score"
  Selbstbewertung_text73: "Selbstbewertung"
  Freitext74: "Fremdbewertung score"
  Fremdbewertung_text74: "Fremdbewertung"

  Freitext75: "Selbstbewertung score"
  Selbstbewertung_text75: "Selbstbewertung"
  Freitext76: "Fremdbewertung score"
  Fremdbewertung_text76: "Fremdbewertung"

  Freitext77: "Selbstbewertung score"
  Selbstbewertung_text77: "Selbstbewertung"
  Freitext78: "Fremdbewertung score"
  Fremdbewertung_text78: "Fremdbewertung"

  Freitext79: "Selbstbewertung score"
  Selbstbewertung_text79: "Selbstbewertung"
  Freitext80: "Fremdbewertung score"
  Fremdbewertung_text80: "Fremdbewertung"

  Freitext81: "Selbstbewertung score"
  Selbstbewertung_text81: "Selbstbewertung"
  Freitext82: "Fremdbewertung score"
  Fremdbewertung_text82: "Fremdbewertung"

  Freitext83: "Selbstbewertung score"
  Selbstbewertung_text83: "Selbstbewertung"
  Freitext84: "Fremdbewertung score"
  Fremdbewertung_text84: "Fremdbewertung"

  Freitext85: "Selbstbewertung score"
  Selbstbewertung_text85: "Selbstbewertung"
  Freitext86: "Fremdbewertung score"
  Fremdbewertung_text86: "Fremdbewertung"

  p7: NA
  p8: NA
  p9: NA

  Freitext87: "Selbstbewertung score"
  Selbstbewertung_text87: "Selbstbewertung"
  Freitext88: "Fremdbewertung score"
  Fremdbewertung_text88: "Fremdbewertung"

  Freitext89: "Selbstbewertung score"
  Selbstbewertung_text89: "Selbstbewertung"
  Freitext90: "Fremdbewertung score"
  Fremdbewertung_text90: "Fremdbewertung"

  Freitext91: "Selbstbewertung score"
  Selbstbewertung_text91: "Selbstbewertung"
  Freitext92: "Fremdbewertung score"
  Fremdbewertung_text92: "Fremdbewertung"

  Freitext93: "Selbstbewertung score"
  Selbstbewertung_text93: "Selbstbewertung"
  Freitext94: "Fremdbewertung score"
  Fremdbewertung_text94: "Fremdbewertung"

  Freitext95: "Selbstbewertung score"
  Selbstbewertung_text95: "Selbstbewertung"
  Freitext96: "Fremdbewertung score"
  Fremdbewertung_text96: "Fremdbewertung"

  Freitext97: "Selbstbewertung score"
  Selbstbewertung_text97: "Selbstbewertung"
  Freitext98: "Fremdbewertung score"
  Fremdbewertung_text98: "Fremdbewertung"

  Freitext99: "Selbstbewertung score"
  Selbstbewertung_text99: "Selbstbewertung"
  Freitext100: "Fremdbewertung score"
  Fremdbewertung_text100: "Fremdbewertung"

  Freitext101: "Selbstbewertung score"
  Selbstbewertung_text101: "Selbstbewertung"
  Freitext102: "Fremdbewertung score"
  Fremdbewertung_text102: "Fremdbewertung"

  Freitext103: "Selbstbewertung score"
  Selbstbewertung_text103: "Selbstbewertung"
  Freitext104: "Fremdbewertung score"
  Fremdbewertung_text104: "Fremdbewertung"

  Freitext105: "Selbstbewertung score"
  Selbstbewertung_text105: "Selbstbewertung"
  Freitext106: "Fremdbewertung score"
  Fremdbewertung_text106: "Fremdbewertung"

  Freitext107: "Selbstbewertung score"
  Selbstbewertung_text107: "Selbstbewertung"
  Freitext108: "Fremdbewertung score"
  Fremdbewertung_text108: "Fremdbewertung"

  Freitext109: "Selbstbewertung score"
  Selbstbewertung_text109: "Selbstbewertung"
  Freitext110: "Fremdbewertung score"
  Fremdbewertung_text110: "Fremdbewertung"

  Freitext111: "Selbstbewertung score"
  Selbstbewertung_text111: "Selbstbewertung"
  Freitext112: "Fremdbewertung score"
  Fremdbewertung_text112: "Fremdbewertung"

  Freitext113: "Selbstbewertung score"
  Selbstbewertung_text113: "Selbstbewertung"
  Freitext114: "Fremdbewertung score"
  Fremdbewertung_text114: "Fremdbewertung"

  p10: NA
  p11: NA
  p12: NA

  Freitext115: "Selbstbewertung score"
  Selbstbewertung_text115: "Selbstbewertung"
  Freitext116: "Fremdbewertung score"
  Fremdbewertung_text116: "Fremdbewertung"

  Freitext117: "Selbstbewertung score"  
  Selbstbewertung_text117: "Selbstbewertung"  
  Freitext118: "Fremdbewertung score"  
  Fremdbewertung_text118: "Fremdbewertung"  

  Freitext119: "Selbstbewertung score"  
  Selbstbewertung_text119: "Selbstbewertung"  
  Freitext120: "Fremdbewertung score"  
  Fremdbewertung_text120: "Fremdbewertung"  

  Freitext121: "Selbstbewertung score"  
  Selbstbewertung_text121: "Selbstbewertung"  
  Freitext122: "Fremdbewertung score"  
  Fremdbewertung_text122: "Fremdbewertung"  

  Freitext123: "Selbstbewertung score"  
  Selbstbewertung_text123: "Selbstbewertung"  
  Freitext124: "Fremdbewertung score"  
  Fremdbewertung_text124: "Fremdbewertung"  

  Freitext125: "Selbstbewertung score"  
  Selbstbewertung_text125: "Selbstbewertung"  
  Freitext126: "Fremdbewertung score"  
  Fremdbewertung_text126: "Fremdbewertung"

  Freitext127: "Selbstbewertung score"  
  Selbstbewertung_text127: "Selbstbewertung"  
  Freitext128: "Fremdbewertung score"  
  Fremdbewertung_text128: "Fremdbewertung" 

  Freitext129: "Selbstbewertung score"  
  Selbstbewertung_text129: "Selbstbewertung"  
  Freitext130: "Fremdbewertung score"  
  Fremdbewertung_text130: "Fremdbewertung"

  Freitext131: "Selbstbewertung score"  
  Selbstbewertung_text131: "Selbstbewertung"  
  Freitext132: "Fremdbewertung score"  
  Fremdbewertung_text132: "Fremdbewertung"

  Freitext133: "Selbstbewertung score"  
  Selbstbewertung_text133: "Selbstbewertung"  
  Freitext134: "Fremdbewertung score"  
  Fremdbewertung_text134: "Fremdbewertung"

  Freitext135: "Selbstbewertung score"  
  Selbstbewertung_text135: "Selbstbewertung"  
  Freitext136: "Fremdbewertung score"  
  Fremdbewertung_text136: "Fremdbewertung" 

  Freitext137: "Selbstbewertung score"  
  Selbstbewertung_text137: "Selbstbewertung"  
  Freitext138: "Fremdbewertung score"  
  Fremdbewertung_text138: "Fremdbewertung" 

  Freitext139: "Selbstbewertung score"  
  Selbstbewertung_text139: "Selbstbewertung"  
  Freitext140: "Fremdbewertung score"  
  Fremdbewertung_text140: "Fremdbewertung" 

  Freitext141: "Selbstbewertung score"  
  Selbstbewertung_text141: "Selbstbewertung"  
  Freitext142: "Fremdbewertung score"  
  Fremdbewertung_text142: "Fremdbewertung" 

  Freitext143: "Selbstbewertung score"  
  Selbstbewertung_text143: "Selbstbewertung"  
  Freitext144: "Fremdbewertung score"  
  Fremdbewertung_text144: "Fremdbewertung"

  Freitext145: "Selbstbewertung score"  
  Selbstbewertung_text145: "Selbstbewertung"  
  Freitext146: "Fremdbewertung score"  
  Fremdbewertung_text146: "Fremdbewertung" 

  Freitext147: "Selbstbewertung score"  
  Selbstbewertung_text147: "Selbstbewertung"  
  Freitext148: "Fremdbewertung score"  
  Fremdbewertung_text148: "Fremdbewertung" 

  Freitext149: "Selbstbewertung score"  
  Selbstbewertung_text149: "Selbstbewertung"  
  Freitext150: "Fremdbewertung score"  
  Fremdbewertung_text150: "Fremdbewertung" 

  Freitext151: "Selbstbewertung score"  
  Selbstbewertung_text151: "Selbstbewertung"  
  Freitext152: "Fremdbewertung score"  
  Fremdbewertung_text152: "Fremdbewertung"

  Freitext153: "Selbstbewertung score"  
  Selbstbewertung_text153: "Selbstbewertung"  
  Freitext154: "Fremdbewertung score"  
  Fremdbewertung_text154: "Fremdbewertung" 

  Freitext155: "Selbstbewertung score"  
  Selbstbewertung_text155: "Selbstbewertung"  
  Freitext156: "Fremdbewertung score"  
  Fremdbewertung_text156: "Fremdbewertung" 

  Freitext157: "Selbstbewertung score"  
  Selbstbewertung_text157: "Selbstbewertung"  
  Freitext158: "Fremdbewertung score"  
  Fremdbewertung_text158: "Fremdbewertung" 

  Freitext159: "Selbstbewertung score"  
  Selbstbewertung_text159: "Selbstbewertung"  
  Freitext160: "Fremdbewertung score"  
  Fremdbewertung_text160: "Fremdbewertung"

  Freitext161: "Selbstbewertung score"  
  Selbstbewertung_text161: "Selbstbewertung"  
  Freitext162: "Fremdbewertung score"  
  Fremdbewertung_text162: "Fremdbewertung"  

  p13: NA
  p14: NA
  p15: NA


---

## Date: `r Sys.Date()`

## Name und Anschrift der Einrichtung
 `r params$name`

## Internetadresse der Einrichtung
  `r params$Internetadresse`

## Trägerschaft mit Erläuterung ob öffentlich oder privat
  `r params$Trag`

## Zugehörigkeit zu einem Unternehmensverbund
  `r params$Zug` 

## Ansprechpartner mit Titel und Qualifikation
  `r params$Ansprechpartner`

## Email-Adresse Ansprechpartner
  `r params$Email`

## Telefonnummer Ansprechpartner
  `r params$Telefonnummer`

## Berufsgruppe
  `r params$Berufsgruppe`

## Versorgung / Einsender
  `r params$Versorgung`

### Sonstiges
  `r params$sonstiges_versorgung`

## Laborbereiche
  `r params$Laborbereiche`

### Sonstiges
  `r params$sonstiges_laborbereiche`

## Leistungsspektrum
  `r params$Leistungsspektrum`

### Sonstiges
  `r params$sonstiges_leistungsspektrum`

\

# Perosnal

## Anzahl Mitarbeitende (gesamt) = `r params$Anzahl_total`

## Anzahl (auch Teilzeit) Ärztliches Personal = `r params$Anzahl_mit`

### davon Fachärzte = `r params$davon_Fachaerzte`

### davon in Weiterbildung = `r params$davon_weiterbildung`

### Anzahl_Planstellen =  `r params$Anzahl_Planstellen`

### davon unbesetzt = `r params$davon_unbesetzt`

## Anzahl (auch Teilzeit) technisches Personal = `r params$Anzahl_tech`

### Anzahl Planstellen = `r params$Anzahl_TPlanstellen`

### davon unbesetzt = `r params$davon_Tunbesetzt`

## Anzahl (auch Teilzeit) Naturwissenschaftliches Personal =  `r params$Anzahl_natur`

### Anzahl Planstellen = `r params$Anzahl_NPlanstellen`

### davon unbesetzt = `r params$davon_Nunbesetzt`

## Anzahl (auch Teilzeit) IT-Mitarbeitende, die direkt dem Laborpersonalzugeordnet sind = `r params$Anzahl_IT`

### Anzahl Planstellen = `r params$Anzahl_IPlanstellen`

### davon unbesetzt = `r params$davon_Iunbesetzt`

## Beschreibung der IT-Unterstützung, die nicht direkt dem Laborpersonal zugeordnet ist
  `r params$Beschreibung`

## Weitere Informationen zu Personal und Stellenbesetzungen
  `r params$WeitereInfo` 

\
  
# EDV und Kennzahlen

## Anbieter Laborinformationssystem
   `r params$AnbieterInfo`

## Anbieter Order-Entry
  `r params$AnbieterOrder`

## Anbieter Middleware, falls mehrere bitte alle eintragen
  `r params$AnbieterMiddleware` 

## Weitere für das Labor wichtige IT-Systeme
  `r params$WeitereIT`

## Öffnungs- und Betriebszeiten, z.B. 24/7
  `r params$Angaben`

## Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des laufenden Jahres
  `r params$laufendenJahres`

## Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des Vorjahres
  `r params$Vorjahres`

## Kompetenzschwerpunkte und Alleinstellungsmerkmale
  `r params$Kompetenzschwerpunkte`

##  Es fehlt ein wichtiger Aspekt? Hier ist Raum für Ihre Ergänzung
   `r params$Ergänzung`

\

# Führung

## Kriterien :  Strategie
## 1. Welche Strategie verfolgen Sie?

### Selbstbewertung score = `r params$Freitext1`

### Selbstbewertung
  `r params$Selbstbewertung_text1`

### Fremdbewertung score = `r params$Freitext2`

### Fremdbewertung  
  `r params$Fremdbewertung_text2`

## 2. Wie wird die Strategie kommuniziert?

### Selbstbewertung score = `r params$Freitext3`

### Selbstbewertung
  `r params$Selbstbewertung_text3`

### Fremdbewertung score = `r params$Freitext4`

### Fremdbewertung  
  `r params$Fremdbewertung_text4`

## Kriterien :  Ziele der Führung
## 3. Welche Ziele verfolgen Sie?

### Selbstbewertung score = `r params$Freitext5`

### Selbstbewertung
  `r params$Selbstbewertung_text5`

### Fremdbewertung score = `r params$Freitext6`

### Fremdbewertung  
  `r params$Fremdbewertung_text6`

## Kriterien :  Risikomanagement / Krisenmanagement
## 4. Haben Sie Ihre Risiken identifiziert und wie gehen Sie mit Risiken um?

### Selbstbewertung score = `r params$Freitext7`

### Selbstbewertung
  `r params$Selbstbewertung_text7`

### Fremdbewertung score = `r params$Freitext8`

### Fremdbewertung  
  `r params$Fremdbewertung_text8`

## 5. Wie ist das Back-up-Konzept?

### Selbstbewertung score = `r params$Freitext9`

### Selbstbewertung
  `r params$Selbstbewertung_text9`

### Fremdbewertung score = `r params$Freitext10`

### Fremdbewertung  
  `r params$Fremdbewertung_text10`

## Kriterien :  Ausfallmanagement
## 6. Wie gehen Sie mit z. B. Lieferabriss um?

### Selbstbewertung score = `r params$Freitext11`

### Selbstbewertung
  `r params$Selbstbewertung_text11`

### Fremdbewertung score = `r params$Freitext12`

### Fremdbewertung  
  `r params$Fremdbewertung_text12`

## Kriterien :  Veränderungsmanagement
## 7. Wie gehen Sie mit Veränderungen um?

### Selbstbewertung score = `r params$Freitext13`

### Selbstbewertung
  `r params$Selbstbewertung_text13`

### Fremdbewertung score = `r params$Freitext14`

### Fremdbewertung  
  `r params$Fremdbewertung_text14`

## 8.Wie werden Mitarbeiter ermutigt, sich an Planungen und Umsetzungen von Veränderungen zu beteiligen?

### Selbstbewertung score = `r params$Freitext15`

### Selbstbewertung
  `r params$Selbstbewertung_text15`

### Fremdbewertung score = `r params$Freitext16`

### Fremdbewertung  
  `r params$Fremdbewertung_text16`


Selbstbewertung.Plot1()
Fremdbewertung.Plot1()
Selbstbewertung_Fremdbewertung.Plot1()


\

# Mitarbeitende

## Kriterien :  Veränderungsmanagement
## 9. Welche Überlegungen gibt es zur Personalplanung für einen längerfristigen Zeitraum?

### Selbstbewertung score = `r params$Freitext17`

### Selbstbewertung
  `r params$Selbstbewertung_text17`

### Fremdbewertung score = `r params$Freitext18`

### Fremdbewertung  
  `r params$Fremdbewertung_text18`

## 10. Betreiben Sie aktive Personalakquise Wenn ja, wie?

### Selbstbewertung score = `r params$Freitext19`

### Selbstbewertung
  `r params$Selbstbewertung_text19`

### Fremdbewertung score = `r params$Freitext20`

### Fremdbewertung  
  `r params$Fremdbewertung_text20`

## Kriterien :  Personalpläne
## 11. Wie werden Dienst-, Urlaubs-, Fort-, Weiterbildungs- und andere wiederkehrenden Pläne erstellt und zwischen den Berufsgruppen abgestimmt?

### Selbstbewertung score = `r params$Freitext21`

### Selbstbewertung
  `r params$Selbstbewertung_text21`

### Fremdbewertung score = `r params$Freitext22`

### Fremdbewertung  
  `r params$Fremdbewertung_text22

## Kriterien :  Interne Kommunikation
## 12. Gibt es regelmäßige Laborbesprechungen?

  `r params$question12_binary`

### Selbstbewertung score = `r params$Freitext23`

### Selbstbewertung
  `r params$Selbstbewertung_text23`

### Fremdbewertung score = `r params$Freitext24`

### Fremdbewertung  
  `r params$Fremdbewertung_text24`


## 13. Mit welchen Besprechungsstrukturen und Kommunikationswerk-zeugen wird eine inhaltlich und zeitlich effiziente Kommunikation erreicht?

### Selbstbewertung score = `r params$Freitext25`

### Selbstbewertung
  `r params$Selbstbewertung_text25`

### Fremdbewertung score = `r params$Freitext26`

### Fremdbewertung  
  `r params$Fremdbewertung_text26`

## Kriterien :  Wertschätzender Umgang miteinander
## 14. Wie wird ein wertschätzender Umgang hierarchieübergreifend und interprofessionell erreicht und befördert?

### Selbstbewertung score = `r params$Freitext27`

### Selbstbewertung
  `r params$Selbstbewertung_text27`

### Fremdbewertung score = `r params$Freitext28`

### Fremdbewertung  
  `r params$Fremdbewertung_text28`


## 15. Welche Unterstützung wird geboten?

### Selbstbewertung score = `r params$Freitext29`

### Selbstbewertung
  `r params$Selbstbewertung_text29`

### Fremdbewertung score = `r params$Freitext30`

### Fremdbewertung  
  `r params$Fremdbewertung_text30`

## 16. Wie wird gefördert?

### Selbstbewertung score = `r params$Freitext31`

### Selbstbewertung
  `r params$Selbstbewertung_text31`

### Fremdbewertung score = `r params$Freitext32`

### Fremdbewertung  
  `r params$Fremdbewertung_text32`

## Kriterien :  Kompetenzentwicklung
## 17. Führt die Laborleitung regelmäßig strukturierte Mitarbeitergespräche?

### Selbstbewertung score = `r params$Freitext33`

### Selbstbewertung
  `r params$Selbstbewertung_text33`

### Fremdbewertung score = `r params$Freitext34`

### Fremdbewertung  
  `r params$Fremdbewertung_text34`

## Kriterien :  Fortbildung- und Weiterbildung
## 18. Wie wird die Weiterbildung bei den einzelnen Berufsgruppen durchgeführt?

### Selbstbewertung score = `r params$Freitext35`

### Selbstbewertung
  `r params$Selbstbewertung_text35`

### Fremdbewertung score = `r params$Freitext36`

### Fremdbewertung  
  `r params$Fremdbewertung_text36`

## 19. Wie erfolgt die Fortbildung (intern/extern)?

### Selbstbewertung score = `r params$Freitext37`

### Selbstbewertung
  `r params$Selbstbewertung_text37`

### Fremdbewertung score = `r params$Freitext38`

### Fremdbewertung  
  `r params$Fremdbewertung_text38`

## 20. Wie entwickeln Sie Führungs- und Fachkompetenz?

### Selbstbewertung score = `r params$Freitext39`

### Selbstbewertung
  `r params$Selbstbewertung_text39`

### Fremdbewertung score = `r params$Freitext40`

### Fremdbewertung  
  `r params$Fremdbewertung_text40`

## Kriterien :  Selbständiges Handeln
## 21. Welche Strukturen und Maßnahmen fördern das selbstständige Handeln der Mitarbeitenden im Rahmen Ihrer fachlichen Qualifikation?

### Selbstbewertung score = `r params$Freitext41`

### Selbstbewertung
  `r params$Selbstbewertung_text41`

### Fremdbewertung score = `r params$Freitext42`

### Fremdbewertung  
  `r params$Fremdbewertung_text42`


## Kriterien :  Wissensmanagement (Anzahl Mitarbeitende Fähigkeiten, Vertretungsregelung)
## 22. Gibt es eine Übersicht über die benötigten Kompetenzen?

### Selbstbewertung score = `r params$Freitext43`

### Selbstbewertung
  `r params$Selbstbewertung_text43`

### Fremdbewertung score = `r params$Freitext44`

### Fremdbewertung  
  `r params$Fremdbewertung_text44`

## 23. Ist Personal mit dem Wissen für diese vorhanden und sind Vertretungsregelungen etabliert?

### Selbstbewertung score = `r params$Freitext45`

### Selbstbewertung
  `r params$Selbstbewertung_text45`

### Fremdbewertung score = `r params$Freitext46`

### Fremdbewertung  
  `r params$Fremdbewertung_text46`

## 24. Wie werden Wissensmonopole vermieden?

### Selbstbewertung score = `r params$Freitext47`

### Selbstbewertung
  `r params$Selbstbewertung_text47`

### Fremdbewertung score = `r params$Freitext48`

### Fremdbewertung  
  `r params$Fremdbewertung_text48`

## 25. Kann das Personal ersetzen werden, während der alte Stelleninhaber noch da ist (direkte Informationsweitergabe)?

### Selbstbewertung score = `r params$Freitext49`

### Selbstbewertung
  `r params$Selbstbewertung_text49`

### Fremdbewertung score = `r params$Freitext50`

### Fremdbewertung  
  `r params$Fremdbewertung_text50`

## 26. Wie können Mitarbeitende auf Informationen zugreifen (z.B. Internet, Literatur)?

### Selbstbewertung score = `r params$Freitext51`

### Selbstbewertung
  `r params$Selbstbewertung_text51`

### Fremdbewertung score = `r params$Freitext52`

### Fremdbewertung  
  `r params$Fremdbewertung_text52`

## Kriterien :  Einarbeitungspläne
## 27. Wie ist die Einarbeitung und Inübungshaltung räumlich, zeitlich und inhaltlich für die einzelnen Berufsgruppen organisiert?

### Selbstbewertung score = `r params$Freitext53`

### Selbstbewertung
  `r params$Selbstbewertung_text53`

### Fremdbewertung score = `r params$Freitext54`

### Fremdbewertung  
  `r params$Fremdbewertung_text54`

## Kriterien :  Teamentwicklung
## 28. Welche Maßnahmen gibt es zur Teamentwicklung?

### Selbstbewertung score = `r params$Freitext55`

### Selbstbewertung
  `r params$Selbstbewertung_text55`

### Fremdbewertung score = `r params$Freitext56`

### Fremdbewertung  
  `r params$Fremdbewertung_text56`

## Kriterien :  Fehlerkultur
## 29. Welche Maßnahmen gibt es zur Teamentwicklung?

### Selbstbewertung score = `r params$Freitext57`

### Selbstbewertung
  `r params$Selbstbewertung_text57`

### Fremdbewertung score = `r params$Freitext58`

### Fremdbewertung  
  `r params$Fremdbewertung_text58`

Mitarbeitende_Selbst.Plot()
Mitarbeitende_Fremd.Plot()
Mitarbeitende_overlay.Plot()


## Kriterien : Patientensicherheit
## 30. Gibt es Identifikationsmöglich-keiten für den Patienten bei der?

### Selbstbewertung score = `r params$Freitext59`

### Selbstbewertung
  `r params$Selbstbewertung_text59`

### Fremdbewertung score = `r params$Freitext60`

### Fremdbewertung  
  `r params$Fremdbewertung_text60`

## 31. Wie erfolgt die Probenerfassung im Labor?

### Selbstbewertung score = `r params$Freitext61`

### Selbstbewertung
  `r params$Selbstbewertung_text61`

### Fremdbewertung score = `r params$Freitext62`

### Fremdbewertung  
  `r params$Fremdbewertung_text62`

## Kriterien :  Patientenidentifikation bei Probengewinnung
## 32. Wie werden Patienten eindeutig identifiziert?

### Selbstbewertung score = `r params$Freitext63`

### Selbstbewertung
  `r params$Selbstbewertung_text63`

### Fremdbewertung score = `r params$Freitext64`

### Fremdbewertung  
  `r params$Fremdbewertung_text64`

## 33.	Wie erfolgt die Probenerfassung im Labor?

### Selbstbewertung score = `r params$Freitext65`

### Selbstbewertung
  `r params$Selbstbewertung_text65`

### Fremdbewertung score = `r params$Freitext66`

### Fremdbewertung  
  `r params$Fremdbewertung_text66`

## 34. Werden Plausibilitätskontrollen durchgeführt?

### Selbstbewertung score = `r params$Freitext67`

### Selbstbewertung
  `r params$Selbstbewertung_text67`

### Fremdbewertung score = `r params$Freitext68`

### Fremdbewertung  
  `r params$Fremdbewertung_text68`

## 35. Wie gehen Sie mit Patientenstammdatenänderungen um?

### Selbstbewertung score = `r params$Freitext69`

### Selbstbewertung
  `r params$Selbstbewertung_text69`

### Fremdbewertung score = `r params$Freitext70`

### Fremdbewertung  
  `r params$Fremdbewertung_text70`

## 36. Welche Unterstützung gibt das Labor bei der korrekten Probengewinnung und beim Probentransport?

### Selbstbewertung score = `r params$Freitext71`

### Selbstbewertung
  `r params$Selbstbewertung_text71`

### Fremdbewertung score = `r params$Freitext72`

### Fremdbewertung  
  `r params$Fremdbewertung_text72`

## Kriterien :  Patient Blood Management
37. Werden Aspekte des Patient Blood Management berücksichtigt?

### Selbstbewertung score = `r params$Freitext73`

### Selbstbewertung
  `r params$Selbstbewertung_text73`

### Fremdbewertung score = `r params$Freitext74`

### Fremdbewertung  
  `r params$Fremdbewertung_text74`

## Kriterien :  Farbcode
## 38. Werden Farbcodesysteme einheitlich genutzt?

### Selbstbewertung score = `r params$Freitext75`

### Selbstbewertung
  `r params$Selbstbewertung_text75`

### Fremdbewertung score = `r params$Freitext76`

### Fremdbewertung  
  `r params$Fremdbewertung_text76`

## 39. Welches Farbcodesystem wird aktuell bei Probenröhrchen verwendet?

### Selbstbewertung score = `r params$Freitext77`

### Selbstbewertung
  `r params$Selbstbewertung_text77`

### Fremdbewertung score = `r params$Freitext78`

### Fremdbewertung  
  `r params$Fremdbewertung_text78`

## 40. Wie ist die Umsetzung von Datenschutzaspekten im Labor geregelt?

### Selbstbewertung score = `r params$Freitext79`

### Selbstbewertung
  `r params$Selbstbewertung_text79`

### Fremdbewertung score = `r params$Freitext80`

### Fremdbewertung  
  `r params$Fremdbewertung_text80`

## 41. Wie ist die Befundauskunft geregelt?

### Selbstbewertung score = `r params$Freitext81`

### Selbstbewertung
  `r params$Selbstbewertung_text81`

### Fremdbewertung score = `r params$Freitext82`

### Fremdbewertung  
  `r params$Fremdbewertung_text82`

## Kriterien :  Herausgabe von Patientenmaterial (Organspende, Staatsanwaltschaft, Forschung)
## 42. Gibt es Prozesse, in denen Patientenmaterial vom Laborpersonal herausgegeben wird?

### Selbstbewertung score = `r params$Freitext83`

### Selbstbewertung
  `r params$Selbstbewertung_text83`

### Fremdbewertung score = `r params$Freitext84`

### Fremdbewertung  
  `r params$Fremdbewertung_text84`

## Kriterien :  Aufklärung (GenDG)
## 43. Wie wird bei Ihnen das Gendiagnostikgesetz umgesetzt?

### Selbstbewertung score = `r params$Freitext85`

### Selbstbewertung
  `r params$Selbstbewertung_text85`

### Fremdbewertung score = `r params$Freitext86`

### Fremdbewertung  
  `r params$Fremdbewertung_text86`

Patient_Selbst.Plot()
Patient_Fremd.Plot()
Patient_overlay.Plot()


# Einsender und Kooperationspartner

## Kriterien :  Kundenorientierung/Kundenbeziehungen
## 44. Wie pflegen Sie den Kontakt zu Ihren Einsendern / Interessengruppen? 

### Selbstbewertung score = `r params$Freitext87`

### Selbstbewertung
  `r params$Selbstbewertung_text87`

### Fremdbewertung score = `r params$Freitext88`

### Fremdbewertung  
  `r params$Fremdbewertung_text88`

## Kriterien :  Lieferantenmanagement
## 45.	Benennen Sie Ihre Hauptlieferanten.

### Selbstbewertung score = `r params$Freitext89`

### Selbstbewertung
  `r params$Selbstbewertung_text89`

### Fremdbewertung score = `r params$Freitext90`

### Fremdbewertung  
  `r params$Fremdbewertung_text90`

## 46. Findet ein regelmäßiger Informationsaustausch mit Ihren Hauptlieferanten statt?

### Selbstbewertung score = `r params$Freitext91`

### Selbstbewertung
  `r params$Selbstbewertung_text91`

### Fremdbewertung score = `r params$Freitext92`

### Fremdbewertung  
  `r params$Fremdbewertung_text92`

## 47.	Wie gehen Sie mit Lieferabrissen um?

### Selbstbewertung score = `r params$Freitext93`

### Selbstbewertung
  `r params$Selbstbewertung_text93`

### Fremdbewertung score = `r params$Freitext94`

### Fremdbewertung  
  `r params$Fremdbewertung_text94`

## Kriterien :  Materialmanagement / Bestellwesen
## 48.	Wie ist in Ihrem Labor das Materialmanagement (Bestellwesen) geregelt?

### Selbstbewertung score = `r params$Freitext95`

### Selbstbewertung
  `r params$Selbstbewertung_text95`

### Fremdbewertung score = `r params$Freitext96`

### Fremdbewertung  
  `r params$Fremdbewertung_text96`

## 49.	Beschreiben Sie kurz das Wartungskonzept Ihrer Laborgeräte.

### Selbstbewertung score = `r params$Freitext97`

### Selbstbewertung
  `r params$Selbstbewertung_text97`

### Fremdbewertung score = `r params$Freitext98`

### Fremdbewertung  
  `r params$Fremdbewertung_text98`

## 50.	Werden Ausfallzeiten Ihrer Geräte systematisch registriert und ausgewertet?

### Selbstbewertung score = `r params$Freitext99`

### Selbstbewertung
  `r params$Selbstbewertung_text99`

### Fremdbewertung score = `r params$Freitext100`

### Fremdbewertung  
  `r params$Fremdbewertung_text100`

## Kriterien :  Fremdversand
## 51.	Wie ist der Fremdversand organisiert?

### Selbstbewertung score = `r params$Freitext101`

### Selbstbewertung
  `r params$Selbstbewertung_text101`

### Fremdbewertung score = `r params$Freitext102`

### Fremdbewertung  
  `r params$Fremdbewertung_text102`

## 52.	Erfolgt eine Bewertung der Prozess-Beteiligten des Fremdversandes?

### Selbstbewertung score = `r params$Freitext103`

### Selbstbewertung
  `r params$Selbstbewertung_text103`

### Fremdbewertung score = `r params$Freitext104`

### Fremdbewertung  
  `r params$Fremdbewertung_text104`

## Kriterien :  Umgang mit Rückmeldungen (strukturiertes Beschwerdemanagement vorhanden?)
## 53.	Wie geht das Labor mit Beschwerden um?

### Selbstbewertung score = `r params$Freitext105`

### Selbstbewertung
  `r params$Selbstbewertung_text105`

### Fremdbewertung score = `r params$Freitext106`

### Fremdbewertung  
  `r params$Fremdbewertung_text106`

## 54.	Erfolgt eine Auswertung der Beschwerden?

### Selbstbewertung score = `r params$Freitext107`

### Selbstbewertung
  `r params$Selbstbewertung_text107`

### Fremdbewertung score = `r params$Freitext108`

### Fremdbewertung  
  `r params$Fremdbewertung_text108

## Kriterien :  Studienteilnahme
## 55.	Nimmt Ihr Labor an Studien teil? 

### Selbstbewertung score = `r params$Freitext109`

### Selbstbewertung
  `r params$Selbstbewertung_text109`

### Fremdbewertung score = `r params$Freitext110`

### Fremdbewertung  
  `r params$Fremdbewertung_text110

## Kriterien :  Patientennahe Sofortdiagnostik
## 56.	Betreut Ihr Labor die POCT Diagnostik Ihrer Einsender?

### Selbstbewertung score = `r params$Freitext111`

### Selbstbewertung
  `r params$Selbstbewertung_text111`

### Fremdbewertung score = `r params$Freitext112`

### Fremdbewertung  
  `r params$Fremdbewertung_text112`

## 57.	Wie ist die Betreuung strukturiert?

### Selbstbewertung score = `r params$Freitext113`

### Selbstbewertung
  `r params$Selbstbewertung_text113`

### Fremdbewertung score = `r params$Freitext114`

### Fremdbewertung  
  `r params$Fremdbewertung_text114`

# Qualitätsindikatoren

## Kriterien : Auftragsgenerierung
## 58. Welche Verfahren zur Auftragsgenerierung bietet das Labor dem Einsender an (z. B. order entry etc.)? 

### Selbstbewertung score = `r params$Freitext115`

### Selbstbewertung
  `r params$Selbstbewertung_text115`

### Fremdbewertung score = `r params$Freitext116`

### Fremdbewertung  
  `r params$Fremdbewertung_text116`

## 59.	Gibt es Ausfallkonzepte? 

### Selbstbewertung score = `r params$Freitext117`

### Selbstbewertung
  `r params$Selbstbewertung_text117`

### Fremdbewertung score = `r params$Freitext118`

### Fremdbewertung  
  `r params$Fremdbewertung_text118`

## 60.	Erfolgt nach Auftragsgenerierung eine Kontrolle der Anforderung?

### Selbstbewertung score = `r params$Freitext119`

### Selbstbewertung
  `r params$Selbstbewertung_text119`

### Fremdbewertung score = `r params$Freitext120`

### Fremdbewertung  
  `r params$Fremdbewertung_text120`

## 61. Sind bei Ihnen Anforderungsprofile definiert? 

### Selbstbewertung score = `r params$Freitext121`

### Selbstbewertung
  `r params$Selbstbewertung_text121`

### Fremdbewertung score = `r params$Freitext122`

### Fremdbewertung  
  `r params$Fremdbewertung_text122`


## 62.	Sind bei Ihnen diagnostische Pfade definiert?

### Selbstbewertung score = `r params$Freitext123`

### Selbstbewertung
  `r params$Selbstbewertung_text123`

### Fremdbewertung score = `r params$Freitext124`

### Fremdbewertung  
  `r params$Fremdbewertung_text124`

## Kriterien : Probengewinnung
## 63.	Welche Hinweise zur Präanalytik und in welcher Form stellt das Labor den Einsendern bei der Auftragsgenerierung zur Verfügung?

### Selbstbewertung score = `r params$Freitext125`

### Selbstbewertung
  `r params$Selbstbewertung_text125`

### Fremdbewertung score = `r params$Freitext126`

### Fremdbewertung  
  `r params$Fremdbewertung_text126`

## Kriterien : Probentransport
## 64.	In welcher Weise erfolgt ein Monitoring beim Probentransport?

### Selbstbewertung score = `r params$Freitext127`

### Selbstbewertung
  `r params$Selbstbewertung_text127`

### Fremdbewertung score = `r params$Freitext128`

### Fremdbewertung  
  `r params$Fremdbewertung_text128`

## Kriterien : Präanalytik im Labor
## 65.	Welche präanalytischen Prüfungen erfolgen im Labor?

### Selbstbewertung score = `r params$Freitext129`

### Selbstbewertung
  `r params$Selbstbewertung_text129`

### Fremdbewertung score = `r params$Freitext130`

### Fremdbewertung  
  `r params$Fremdbewertung_text130`

## 66.	Werden Rückmeldungen der Einsender bei der Auswahl / Durchführung der Analytik berücksichtigt?

### Selbstbewertung score = `r params$Freitext131`

### Selbstbewertung
  `r params$Selbstbewertung_text131`

### Fremdbewertung score = `r params$Freitext132`

### Fremdbewertung  
  `r params$Fremdbewertung_text132`

## 67.	Welche Kriterien spielen bei der Auswahl, z.B.  Neueinführung der Analytik eine Rolle? 

### Selbstbewertung score = `r params$Freitext133`

### Selbstbewertung
  `r params$Selbstbewertung_text133`

### Fremdbewertung score = `r params$Freitext134`

### Fremdbewertung  
  `r params$Fremdbewertung_text134`

## Kriterien : Probenmanagement innerhalb des Labors
## 68.	Wie sieht Ihr Probenfluss-Konzept aus?

### Selbstbewertung score = `r params$Freitext135`

### Selbstbewertung
  `r params$Selbstbewertung_text135`

### Fremdbewertung score = `r params$Freitext136`

### Fremdbewertung  
  `r params$Fremdbewertung_text136`

## Kriterien : TAT Labor
## 69.	Kann für bestimmte Messgrößen die TAT im Labor ermittelt werden?

### Selbstbewertung score = `r params$Freitext137`

### Selbstbewertung
  `r params$Selbstbewertung_text137`

### Fremdbewertung score = `r params$Freitext138`

### Fremdbewertung  
  `r params$Fremdbewertung_text138`

## Kriterien : TAT gesamt (von Anforderung bis Befundregistrierung)
## 70.	Kann für bestimmte Messgrößen die TAT im Labor ermittelt werden?

### Selbstbewertung score = `r params$Freitext139`

### Selbstbewertung
  `r params$Selbstbewertung_text139`

### Fremdbewertung score = `r params$Freitext140`

### Fremdbewertung  
  `r params$Fremdbewertung_text140`


Kriterien : Befundung
71.	Werden Analysenergebnisse ärztlicherseits ergänzend textlich befundet?

### Selbstbewertung score = `r params$Freitext141`

### Selbstbewertung
  `r params$Selbstbewertung_text141`

### Fremdbewertung score = `r params$Freitext142`

### Fremdbewertung  
  `r params$Fremdbewertung_text142`

## Kriterien : Befundübermittlung
## 72.	Wie erfolgt die Befundübermittlung?

### Selbstbewertung score = `r params$Freitext143`

### Selbstbewertung
  `r params$Selbstbewertung_text143`

### Fremdbewertung score = `r params$Freitext144`

### Fremdbewertung  
  `r params$Fremdbewertung_text144`


## Kriterien : Probenarchivierung
## 73.	Wie erfolgt die Probenarchivierung?

### Selbstbewertung score = `r params$Freitext145`

### Selbstbewertung
  `r params$Selbstbewertung_text145`

### Fremdbewertung score = `r params$Freitext146`

### Fremdbewertung  
  `r params$Fremdbewertung_text146`


## Kriterien : Erbringung von Service und Beratungsleitungen
## 74.	Welche Service- und Beratungsleistungen erbringen Sie?

### Selbstbewertung score = `r params$Freitext147`

### Selbstbewertung
  `r params$Selbstbewertung_text147`

### Fremdbewertung score = `r params$Freitext148`

### Fremdbewertung  
  `r params$Fremdbewertung_text148`


## Kriterien : klinische Scores auf Basis/mit Anteil laboratoriumsmedizinischer Untersuchungen
## 74.	Welche Service- und Beratungsleistungen erbringen Sie?

### Selbstbewertung score = `r params$Freitext149`

### Selbstbewertung
  `r params$Selbstbewertung_text149`

### Fremdbewertung score = `r params$Freitext150`

### Fremdbewertung  
  `r params$Fremdbewertung_text150`

## 75.	Werden Hinweise zur DRG Gruppierung ausgegeben?

### Selbstbewertung score = `r params$Freitext151`

### Selbstbewertung
  `r params$Selbstbewertung_text151`

### Fremdbewertung score = `r params$Freitext152`

### Fremdbewertung  
  `r params$Fremdbewertung_text152`

## 76.	Kann das Labor Statistiken und Auswertungen zur Verfügung stellen?

### Selbstbewertung score = `r params$Freitext153`

### Selbstbewertung
  `r params$Selbstbewertung_text153`

### Fremdbewertung score = `r params$Freitext154`

### Fremdbewertung  
  `r params$Fremdbewertung_text154`

## Kriterien : Umgang mit interner Qualitätssicherung
## 77.	Wie wird bei Ihnen die interne Qualitätssicherung durchgeführt?

### Selbstbewertung score = `r params$Freitext155`

### Selbstbewertung
  `r params$Selbstbewertung_text155`

### Fremdbewertung score = `r params$Freitext156`

### Fremdbewertung  
  `r params$Fremdbewertung_text156`

## 78.	Werden Maßnahmen durchgeführt, die über die Forderungen der Rili-BÄK hinausgehen?

### Selbstbewertung score = `r params$Freitext157`

### Selbstbewertung
  `r params$Selbstbewertung_text157`

### Fremdbewertung score = `r params$Freitext158`

### Fremdbewertung  
  `r params$Fremdbewertung_text158`

## 79.	Wie wird bei Ihnen die externe Qualitätssicherung durchgeführt?

### Selbstbewertung score = `r params$Freitext159`

### Selbstbewertung
  `r params$Selbstbewertung_text159`

### Fremdbewertung score = `r params$Freitext160`

### Fremdbewertung  
  `r params$Fremdbewertung_text160`

## 80.	Gibt es regelmäßige Auswertungen der RV?

### Selbstbewertung score = `r params$Freitext161`

### Selbstbewertung
  `r params$Selbstbewertung_text161`

### Fremdbewertung score = `r params$Freitext162`

### Fremdbewertung  
  `r params$Fremdbewertung_text162`


Qualit_Selbst.Plot()
Qualit_Fremd.Plot()
Qualit_Overlay.Plot()



```{r PeerReview, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.height = 6)
```

```{r, echo=FALSE, comment=NA}

### plots

library(plotly)
tmpFile <- tempfile(fileext = ".png")

Selbstbewertung.Plot1()
Fremdbewertung.Plot1()
Selbstbewertung_Fremdbewertung.Plot1()

Mitarbeitende_Selbst.Plot()
Mitarbeitende_Fremd.Plot()
Mitarbeitende_overlay.Plot()

Patient_Selbst.Plot()
Patient_Fremd.Plot()
Patient_overlay.Plot()

Einsender_Selbst.Plot()
Einsender_Fremd.Plot()
Einsender_Overlay.Plot()

```
'
)

# define here mandatory fields
fieldsMandatory <- c("name", "Berufsgruppe", "Internetadresse","Trag","Ansprechpartner","Email")

# to show to the asterisk above the mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


appCSS <-
  ".mandatory_star { color: red; }
   #error { color: red; }"


# Define a global variable to store the input values
input_values <- reactiveValues(counter = 1, Beschreibung1 = "", Anmerkung1 = "")

# Helper function to render the UI with dynamic inputs
renderDynamicInputs <- function(input_values) {
  anmerkungenInputs <- lapply(1:input_values$counter, function(i) {
    tagList(
      textAreaInput(paste0("Beschreibung", i), "Beschreibung",
                    value = input_values[[paste0("Beschreibung", i)]], rows = 3),
      textInput(paste0("Anmerkung", i), "Anmerkungen des Peers",
                value = input_values[[paste0("Anmerkung", i)]])
    )
  })
  tagList(anmerkungenInputs)
}

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#003B73"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#40668d",
    dark_hover_bg = "#40668d",
    dark_color = "#FFF"
  )
)

ui <- function(request) {
  dashboardPage(
    
    dashboardHeader(),
    dashboardSidebar(collapsed = TRUE
    ),
    
    dashboardBody(
      useShinyjs(),
      shinyjs::inlineCSS(appCSS), # for the color of the mandatory asterisk
      
      # for justified titles
      tags$style(HTML("
    .title-panel {
      display: flex;
      justify-content: center;
    }
  ")),
  tags$head(
    tags$style(
      HTML(
        ".left-align { text-align: left; }"
      )
    )
  ),
  
  tags$h3("Peer Review", style = "text-align: center;"),
  tags$h4("Fragenkatalog zur Selbst- und Fremdbewertung in der Laboratoriumsmedizin",
          style = "text-align: center;"),
  bookmarkButton(),
  
  # at the moment, this submit button is basically useless because we don't have responses being collected on the server
  # but the goal is to create a "google-like-form" where user can fill in the responses and only administrators 
  # will be able to see all the details 
  actionButton("submit", "Submit"),
  
  # we work only with pdf downloads now as if user uploads another pdf in the section of "EDV and Kennzahlen"
  # this should be appended to the final filled pdf report
  
  downloadButton("download_pdf", "Download report"),
  
  #downloadButton("download_html_report", "Download HTML Report")
  # "#B097C7" "#004c6d" "#A9DF9C"
  
  # CSS code to change tab colors
  tags$style(HTML('
    .nav-tabs > li > a {
      background-color: #004c6d;
      color: #FFFFFF;
    }
    .nav-tabs > li > a:hover {
      background-color: #62a4b6;
      color: #000000;
    }
    .nav-tabs > li.active > a {
      background-color: #62a4b6;
      color: #000000;
    }
    .tab-content {
      background-color: #B4D9F9;
      border: 1px solid #7297b5;
      padding: 10px;
      border-top: none;
    }
  ')),
  #tags$head(tags$meta(charset = "German_Germany.UTF-8")),
  
  # here starts all headers and tabs of the peer-review questionnaire

  tabsetPanel(
    tabPanel("Grundlagen und Organisation",
             h4("Labor und Ansprechpartner", style = "margin-top: 0;"),
             fluidRow(
               # First Column
               column(
                 width = 4, #  width for the first column
                 #offset = 1, # Offset the first column to center it
                 div(
                   style = "width: 400px; padding: 10px;",
                   div(
                     # class = "justify-content-center text-center", # Center the content horizontally and text-align center
                     fluidRow(
                       class = "col-md-12", # Full width on medium and larger screens, 12 columns
                       textInput("name",labelMandatory("Name und Anschrift der Einrichtung:"),""),
                       textInput("Internetadresse", labelMandatory("Internetadresse der Einrichtung:"),""),
                       textInput("Trag", labelMandatory("Trägerschaft mit Erläuterung ob öffentlich oder privat:"),""),
                       textInput("Zug", "Zugehörigkeit zu einem Unternehmensverbund:"),
                       textInput("Ansprechpartner", labelMandatory("Ansprechpartner mit Titel und Qualifikation:"),""),
                       textInput("Email", labelMandatory("Email-Adresse Ansprechpartner:"),""),
                       textInput("Telefonnummer", "Telefonnummer Ansprechpartner:")
                     )
                   )
                 )
               ),
               # Second Column
               column(
                 width = 4, # Adjust the width for the second column
                 #offset = 1, # Offset the second column to center it
                 div(
                   
                   fluidRow(
                     # Add Bootstrap classes for responsive design
                     class = "col-md-12", # Full width on medium and larger screens, 12 columns
                     
                     # Berufsgruppe
                     # goal is to create a cumulative score based on the Berufsgruppe of the user
                     # for example a cumulative score for all Ärzte, IT Mitarbeiter etc.
                     radioButtons("Berufsgruppe",labelMandatory("Berufsgruppe:"),
                                  choices = c("MTL", "Wissenschaftler(-in)",
                                              "Ärzte","IT", "nicht anwendbar"),
                                  selected = "nicht anwendbar",  inline=TRUE),
                     
                     conditionalPanel(condition = "Berufsgruppe >= 1"),
                     
                     #Option 1
                     checkboxGroupInput("Versorgung", labelMandatory("Versorgung / Einsender (bitte auswählen):"),
                                        choices = c("Zentrallabor Krankenhauslabor ", "Fachabteilung",
                                                    "Niedergelassenes Labor","Sonstiges (bitte benennen)")),
                     
                     # shows an empty textbox where user should enter other options when they select Sonstiges (bitte benennen)
                     conditionalPanel(condition = 'input.Versorgung.includes("Sonstiges (bitte benennen)")',
                                      textInput("sonstiges_versorgung", "Bitte benennen:")),
                     
                     #Option 2
                     checkboxGroupInput("Laborbereiche", "Laborbereiche (bitte auswählen):",
                                        choices = c("Klinische Chemie", "Hämostaseologie", "Hämatologie",
                                                    "Immunologie", "Immunhämatologie", "Mikrobiologie",
                                                    "Infektionsserologie","QM des POCT",
                                                    "Autoantikärperdiagnostik", "Allergologie",
                                                    "Aminosäureanalytik","Toxikologie/TDM",
                                                    "Sonstiges (bitte benennen)")),
                     
                     conditionalPanel(condition = 'input.Laborbereiche.includes("Sonstiges (bitte benennen)")',
                                      textInput("sonstiges_laborbereiche", "Bitte benennen:")),
                   )
                 )
               ),
               
               # Third Column
               column(
                 width = 4, 
                 #offset = 1, # Offset the third column to center it
                 div(

                   fluidRow(
                     class = "col-md-12", # Full width on medium and larger screens, 12 columns
                     
                     #Option 3
                     checkboxGroupInput("Leistungsspektrum", "Leistungsspektrum / Methoden (bitte auswählen):",
                                        choices = c("Absorptionsspektrometrie (AAS)", "Aggregometrie",
                                                    "Coulometrie","Durchflußzytometrie (FACS)","Elektrophorese",
                                                    "Flammenphotometrie","Fluoreszenzmikroskopie","HPLC",
                                                    "Immunfixation","Immunoassays","ISE", "Koagulometrie",
                                                    "Lichtmikroskopie","MS","Nephelometrie",
                                                    "PCR / molekularbiol. Methoden","RIA","Turbidimetrie",
                                                    "Sonstiges (bitte benennen)")),
                     
                     conditionalPanel(condition = 'input.Leistungsspektrum.includes("Sonstiges (bitte benennen)")',
                                      textInput("sonstiges_leistungsspektrum", "Bitte benennen:")),
                   )
                 )
               )
             ),
             mainPanel(
               verbatimTextOutput("selected_options"),
             ),
    ),
    tabPanel("Personal",
             fluidRow(
               column(
                 width = 5,
                 offset = 1,  # To center the first column
                 style = "white-space: nowrap;",
                 fluidRow(
                   column(12,
                          offset = 1,  # To center the first column
                          style = "white-space: nowrap;",
                          numericInput("Anzahl_total", "Anzahl Mitarbeitende (gesamt)",0, min = 0, max = 500),
                          titlePanel(
                            div(
                              tags$h5(
                                align = "center",  # Center the label text
                                strong(numericInput("Anzahl_mit", "Anzahl (auch Teilzeit) Ärztliches Personal", 0, min = 0, max = 500))
                              )
                            )
                          ),
                          div(
                            style = "width: 600px;",
                            fluidRow(
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_mit >= 1",
                                       numericInput("davon_Fachaerzte", "davon Fachärzte", 0, min = 0, max = 500) 
                                       )
                              ),
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_mit >= 1",
                                       numericInput("davon_weiterbildung", "davon in Weiterbildung", 0, min = 0, max = 500)
                                     )
                              ),
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_mit >= 1",
                                       numericInput("Anzahl_Planstellen", "Anzahl Planstellen", 0, min = 0, max = 500)
                                     )
                                     
                              ),
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_mit >= 1",
                                       numericInput("davon_unbesetzt", "davon unbesetzt", 0, min = 0, max = 500)
                                     )
                              )
                            )
                          ),
                          
                          titlePanel(
                            div(
                              tags$h5(
                                align = "center",  # Center the label text
                                strong(numericInput("Anzahl_tech", "Anzahl (auch Teilzeit) technisches Personal", 0, min = 0, max = 500))
                              )
                            )
                          ),
                          
                          div(
                            style = "width: 600px;",
                            fluidRow(
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_tech >= 1",
                                       numericInput("Anzahl_TPlanstellen", "Anzahl Planstellen", 0, min = 0, max = 500)                                     )
                              ),
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_tech >= 1",
                                       numericInput("davon_unbesetzt", "davon unbesetzt", 0, min = 0, max = 500)
                                     )
                              )
                              
                            )
                          ),
                          titlePanel(
                            div(
                              tags$h5(
                                align = "center", 
                                strong(numericInput("Anzahl_natur", "Anzahl (auch Teilzeit) Naturwissenschaftliches Personal", 0, min = 0, max = 500))
                              )
                            )
                          ),
                          div(
                            style = "width: 600px;",
                            fluidRow(
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_natur >= 1",
                                       numericInput("Anzahl_NPlanstellen", "Anzahl Planstellen", 0, min = 0, max = 500)                                     )
                              ),
                              column(width = 6,
                                     conditionalPanel(
                                       condition = "input.Anzahl_natur >= 1",
                                       numericInput("davon_Nunbesetzt", "davon unbesetzt", 0, min = 0, max = 500)
                                     )
                              )
                              
                            )
                          ),
                          
                          titlePanel(
                            div(
                              tags$h5(
                                align = "center", 
                                strong(numericInput("Anzahl_IT", "Anzahl (auch Teilzeit) IT-Mitarbeitende,
 die direkt dem Laborpersonalzugeordnet sind", 0, min = 0, max = 500))
                              )
                            )
                          ),
 div(
   style = "width: 600px;",
   fluidRow(
     column(width = 6,
            conditionalPanel(
              condition = "input.Anzahl_IT >= 1",
              numericInput("Anzahl_IPlanstellen", "Anzahl Planstellen", 0, min = 0, max = 500)                                     )
     ),
     column(width = 6,
            conditionalPanel(
              condition = "input.Anzahl_IT >= 1",
              numericInput("davon_Iunbesetzt", "davon unbesetzt", 0, min = 0, max = 500)
            )
     ),
     
   ),
   
   textInput("Beschreibung", "Beschreibung der IT-Unterstützung, die nicht direkt dem Laborpersonal zugeordnet ist"),
   textInput("WeitereInfo", "Weitere Informationen zu Personal und Stellenbesetzungen (bitte benennen):")
 )
                   )
                 )
 
               )
             )
    ),
 tabPanel("EDV und Kennzahlen",
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         textInput("AnbieterInfo", "Anbieter Laborinformationssystem"),
                         textInput("AnbieterOrder", "Anbieter Order-Entry"),
                         textInput("AnbieterMiddleware", "Anbieter Middleware, falls mehrere bitte alle eintragen"),
                         textInput("WeitereIT", "Weitere für das Labor wichtige IT-Systeme"),
                  ),
                  # style = "margin: 0 auto; max-width: 800px;",
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    textInput("Angaben", "Öffnungs- und Betriebszeiten, z.B. 24/7"),
                    textInput("laufendenJahres", "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des laufenden Jahres"),
                    #textInput("laufendenJahres", "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des laufenden Jahres"),
                    textInput("Vorjahres", "Analysenanzahlen, GOÄ-Punkte oder andere Mengenkennzahlen des Vorjahres"),
                    textInput("Kompetenzschwerpunkte", "Kompetenzschwerpunkte und Alleinstellungsmerkmale")
                  ),
                  #style = "margin: 0 auto; max-width: 800px;"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         fileInput("Organigramm", "Organigramm (Dateiname, Bezeichnung der Anlage - bitte beifügen)"),
                  ),
                  # style = "margin: 0 auto; max-width: 800px;",
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    div(
                      class = "gray-text",
                      textInput("Ergänzung",
                                label = "[Es fehlt ein wichtiger Aspekt? Hier ist Raum für Ihre Ergänzung.]"))
                  ),
                )
              )
            )
          ),
          mainPanel()
 ),
 
 # this is tricky as well. Anything that user adds here is still not part of the finla pdf.
 # these inputs should be added to the final report
 
 tabPanel("Input durch Peer Review",
          titlePanel(tags$h4("Hauptthema/Anlass")),
          sidebarLayout(
            sidebarPanel(
              #textAreaInput("HauptthemaBeschreibung", "Beschreibung"),
              #textAreaInput("AnmerkungendesPeers", "Anmerkungen des Peers"),
              uiOutput("anmerkungenInputs"),
              actionButton("addAnmerkung", "Add More"),
              style = "width: 500px; background-color: #B4D9F9;"
            ),
            mainPanel()
          )
 ),
 tabPanel("Führung",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 1
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Strategie"))),
                         titlePanel(
                           div(
                             tags$h5(style = "text-align: left;",
                                     strong("1. Welche Strategie verfolgen Sie?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    radioButtons("Freitext1", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Selbstbewertung Freitext"),
                                    div(textAreaInput("Selbstbewertung_text1", label = NULL, placeholder = "Text eingeben...", height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    radioButtons("Freitext2", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Fremdbewertung Freitext"),
                                    div(textAreaInput("Fremdbewertung_text2", label = NULL, placeholder = "Text eingeben...", height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 2
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("2. Wie wird die Strategie kommuniziert?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    radioButtons("Freitext3", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Selbstbewertung Freitext"),
                                    div(textAreaInput("Selbstbewertung_text3", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    radioButtons("Freitext4", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Fremdbewertung Freitext"),
                                    div(textAreaInput("Fremdbewertung_text4", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         # Question 3
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Ziele der Führung"))),
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("3. Welche Ziele verfolgen Sie?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    radioButtons("Freitext5", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Selbstbewertung Freitext"),
                                    div(textAreaInput("Selbstbewertung_text5", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    radioButtons("Freitext6", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Fremdbewertung Freitext"),
                                    div(textAreaInput("Fremdbewertung_text6", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         # Question 4
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Risikomanagement / Krisenmanagement"))),
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("4. Haben Sie Ihre Risiken identifiziert und wie gehen Sie mit Risiken um?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    radioButtons("Freitext7", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Selbstbewertung Freitext"),
                                    div(textAreaInput("Selbstbewertung_text7", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    radioButtons("Freitext8", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    p("Fremdbewertung Freitext"),
                                    div(textAreaInput("Fremdbewertung_text8", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                  ),
                  # style = "margin: 0 auto; max-width: 800px;",
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 5
                    titlePanel(
                      div(style = "width: 600px; margin-top: 80px;", # Add margin-top to push it down
                          tags$h5(strong("5. Wie ist das Back-up-Konzept?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               radioButtons("Freitext9", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Selbstbewertung Freitext"),
                               div(textAreaInput("Selbstbewertung_text9", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               radioButtons("Freitext10", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Fremdbewertung Freitext"),
                               div(textAreaInput("Fremdbewertung_text10", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 6
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Ausfallmanagement"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("6. Wie gehen Sie mit z. B. Lieferabriss um?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               radioButtons("Freitext11", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Selbstbewertung Freitext"),
                               div(textAreaInput("Selbstbewertung_text11", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               radioButtons("Freitext12", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Fremdbewertung Freitext"),
                               div(textAreaInput("Fremdbewertung_text12", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 7
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Veränderungsmanagement"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("7. Wie gehen Sie mit Veränderungen um?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               radioButtons("Freitext13", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Selbstbewertung Freitext"),
                               div(textAreaInput("Selbstbewertung_text13", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               radioButtons("Freitext14", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Fremdbewertung Freitext"),
                               div(textAreaInput("Fremdbewertung_text14", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 8
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("8.Wie werden Mitarbeiter ermutigt, sich an Planungen und Umsetzungen von Veränderungen zu beteiligen?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               radioButtons("Freitext15", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Selbstbewertung Freitext"),
                               div(textAreaInput("Selbstbewertung_text15", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               radioButtons("Freitext16", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               p("Fremdbewertung Freitext"),
                               div(textAreaInput("Fremdbewertung_text16", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                  ),
                  fluidRow(
                    column(12, align = "center",
                           div(
                             class = "gray-text",
                             titlePanel(tags$h4("[Es fehlt ein wichtiges Kriterium? Hier ist Raum für Ihre Ergänzung.]")),
                             textAreaInput("Kriterium", label = "[Raum für Ihre Ergänzung]")),
                           #style = "width: 600px; background-color: #B4D9F9;"
                    )
                  ),
                  fluidRow(
                    column(4,
                           div(align = "left",
                               plotlyOutput("SelbstbewertungPlot1", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "center",
                               plotlyOutput("FremdbewertungPlot1", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Selbstbewertung_FremdbewertungPlot1", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                  )
                )
              )
            )
          )
 ),
 
 tabPanel("Mitarbeitende",
          
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 9
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Veränderungsmanagement"))),
                         titlePanel(
                           div(
                             tags$h5(style = "text-align: left;",
                                     strong("9. Welche Überlegungen gibt es zur Personalplanung für einen längerfristigen Zeitraum?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext17", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text17", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext18", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text18", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         # Question 10
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("10. Betreiben Sie aktive Personalakquise Wenn ja, wie?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext19", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text19", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext20", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text20", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 11
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Personalpläne"))),
                         titlePanel(
                           div(
                             tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                     strong("11. Wie werden Dienst-, Urlaubs-, Fort-, Weiterbildungs- und andere wiederkehrenden Pläne erstellt und zwischen den Berufsgruppen abgestimmt?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext21", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text21", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext22", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text22", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 12
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Interne Kommunikation"))),
                         div(
                           id = "question12_container",
                           style = "display: flex; flex-direction: row; align-items: center;",
                           radioButtons("question12_binary",inline = TRUE,
                                        label = "12. Gibt es regelmäßige Laborbesprechungen?",
                                        choices = list("Ja" = "yes", "Nein" = "no"),
                                        selected = "yes")),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext23", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text23", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext24", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text24", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 13
                         
                         titlePanel(
                           div(
                             tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                     strong("13. Mit welchen Besprechungsstrukturen und Kommunikationswerk-zeugen wird eine inhaltlich und zeitlich effiziente Kommunikation erreicht?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext25", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text25", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext26", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text26", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 14
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Wertschätzender Umgang miteinander"))),
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                     strong("14. Wie wird ein wertschätzender Umgang hierarchieübergreifend und interprofessionell erreicht und befördert?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext27", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text27", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext28", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text28", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 15
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("15. Welche Unterstützung wird geboten?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext29", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text29", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext30", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text30", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 16
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("16. Wie wird gefördert?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext31", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text31", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext32", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text32", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 17
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Kompetenzentwicklung"))),
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("17. Führt die Laborleitung regelmäßig strukturierte Mitarbeitergespräche? "))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext33", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text33", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext34", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text34", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 18
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Fortbildung- und Weiterbildung"))),
                         titlePanel(
                           div(#class = "title-panel",
                             tags$h5(strong("18. Wie wird die Weiterbildung bei den einzelnen Berufsgruppen durchgeführt?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext35", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div( textAreaInput("Selbstbewertung_text35", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext36", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text36", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         )
                  ),
                  
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 19
                    titlePanel(
                      div(style = "width: 600px; margin-top: 80px;", # Add margin-top to push it down
                          tags$h5(strong("19. Wie erfolgt die Fortbildung (intern/extern)?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext37", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text37", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext38", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text38", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 20
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("20. Wie entwickeln Sie Führungs- und Fachkompetenz?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext39", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text39", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext40", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text40", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 21
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Selbständiges Handeln"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                strong("21. Welche Strukturen und Maßnahmen fördern das selbstständige Handeln der Mitarbeitenden im Rahmen Ihrer fachlichen Qualifikation?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext41", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text41", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext42", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text42", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 22
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Wissensmanagement (Anzahl Mitarbeitende Fähigkeiten, Vertretungsregelung)"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("22. Gibt es eine Übersicht über die benötigten Kompetenzen?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext43", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text43", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext44", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text44", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 23
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("23. Ist Personal mit dem Wissen für diese vorhanden und sind Vertretungsregelungen etabliert? "))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext45", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text45", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext46", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text46", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 24
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("24. Wie werden Wissensmonopole vermieden?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext47", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text47", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext48", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text48", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 25
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                strong("25. Kann das Personal ersetzen werden, während der alte Stelleninhaber noch da ist (direkte Informationsweitergabe)?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext49", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text49", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext50", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text50", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 26
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("26. Wie können Mitarbeitende auf Informationen zugreifen (z.B. Internet, Literatur)?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext51", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text51", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext52", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text52", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 27
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Einarbeitungspläne"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                strong("27. Wie ist die Einarbeitung und Inübungshaltung räumlich, zeitlich und inhaltlich für die einzelnen Berufsgruppen organisiert?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext53", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text53", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext54", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text54", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 28
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Teamentwicklung"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("28. Welche Maßnahmen gibt es zur Teamentwicklung?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext55", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text55", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext56", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text56", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    # Question 29
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Fehlerkultur"))),
                    titlePanel(
                      div(#class = "title-panel",
                        tags$h5(strong("29. Welche Maßnahmen gibt es zur Teamentwicklung?"))
                      )
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext57", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text57", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext58", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text58", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4, align = "left",
                           div(align = "left",
                               plotlyOutput("Mitarbeitende_Selbst_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4, offet = 1,
                           div(align = "center",
                               plotlyOutput("Mitarbeitende_Fremd_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Mitarbeitende_overlay_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                    
                    
                  )
                )
              )
            )
          )
 ),
 
 tabPanel("Patient und Angehörige",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 30
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Patientensicherheit"))),
                         titlePanel(
                           div(style =  "padding-left: 20px;",
                               tags$h5(strong("30. Gibt es Identifikationsmöglich-keiten für den Patienten bei der?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext59", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext60", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 31
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("31. Wie erfolgt die Probenerfassung im Labor?"))
                         ),
                         
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext61", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext62", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 32
                         titlePanel(div(#class = "title-panel",
                           tags$h4("Kriterien :  Patientenidentifikation bei Probengewinnung"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;", strong("32. Wie werden Patienten eindeutig identifiziert?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext63", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext64", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 33
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("33.	Wie erfolgt die Probenerfassung im Labor?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext65", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext66", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 34
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("34. Werden Plausibilitätskontrollen durchgeführt?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext67", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext68", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         
                         # Question 35
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("35. Wie gehen Sie mit Patientenstammdatenänderungen um?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext69", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext70", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 36
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Präanalytik"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("36. Welche Unterstützung gibt das Labor bei der korrekten Probengewinnung und beim Probentransport?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext71", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext72", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         )
                         
                  ),
                  
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 37
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Patient Blood Management"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("37. Werden Aspekte des Patient Blood Management berücksichtigt?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext73", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext74", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    
                    # Question 38
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Farbcode"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("38. Werden Farbcodesysteme einheitlich genutzt?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext75", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext76", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 39
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("39. Welches Farbcodesystem wird aktuell bei Probenröhrchen verwendet?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext77", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div( textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext78", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 40
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("40. Wie ist die Umsetzung von Datenschutzaspekten im Labor geregelt?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext79", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext80", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div( textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 41
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("41. Wie ist die Befundauskunft geregelt?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext81", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext82", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 42
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Herausgabe von Patientenmaterial (Organspende, Staatsanwaltschaft, Forschung)"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("42. Gibt es Prozesse, in denen Patientenmaterial vom Laborpersonal herausgegeben wird?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext83", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext84", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 43
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Aufklärung (GenDG)"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("43. Wie wird bei Ihnen das Gendiagnostikgesetz umgesetzt?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext85", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext86", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4, align = "left",
                           div(align = "left",
                               plotlyOutput("Patient_Selbst_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4, offet = 1,
                           div(align = "center",
                               plotlyOutput("Patient_Fremd_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Patient_overlay_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                    
                    
                  )
                )
              )
            )
          )
        ),
 
 tabPanel("Einsender und Kooperationspartner",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 44
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Kundenorientierung/Kundenbeziehungen"))),
                         titlePanel(
                           div(style =  "padding-left: 20px;",
                               tags$h5(strong("44. Wie pflegen Sie den Kontakt zu Ihren Einsendern / Interessengruppen? "))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext87", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext88", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 45
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Lieferantenmanagement"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("45. Benennen Sie Ihre Hauptlieferanten"))
                         ),
                         
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext89", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext90", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 46
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;", strong("46. Findet ein regelmäßiger Informationsaustausch mit Ihren Hauptlieferanten statt?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext91", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext92", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 47
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("47.	Wie gehen Sie mit Lieferabrissen um?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext93", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext94", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 48
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Materialmanagement / Bestellwesen"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("48.	Wie ist in Ihrem Labor das Materialmanagement (Bestellwesen) geregelt? "))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext95", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext96", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 49
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Wartung und Management Laborgeräte"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("49.	Beschreiben Sie kurz das Wartungskonzept Ihrer Laborgeräte."))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext97", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext98", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 50
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("50.	Werden Ausfallzeiten Ihrer Geräte systematisch registriert und ausgewertet?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext99", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext100", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         )
                         
                  ),
                  
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 51
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Fremdversand"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("51.	Wie ist der Fremdversand organisiert?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext101", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext102", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 52
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("52.	Erfolgt eine Bewertung der Prozess-Beteiligten des Fremdversandes?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext103", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext104", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 53
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Umgang mit Rückmeldungen (strukturiertes Beschwerdemanagement vorhanden?)"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("53.	Wie geht das Labor mit Beschwerden um?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext105", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext106", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 54
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("54.	Erfolgt eine Auswertung der Beschwerden?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext107", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext108", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 55
                    titlePanel(div(class = "title-panel",tags$h4("Kriterien :  Studienteilnahme"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("55.	Nimmt Ihr Labor an Studien teil? "))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext109", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext110", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 56
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Patientennahe Sofortdiagnostik"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("56.	Betreut Ihr Labor die POCT Diagnostik Ihrer Einsender?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext111", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext112", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 57
                    
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("57.	Wie ist die Betreuung strukturiert?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext113", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext114", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4, align = "left",
                           div(align = "left",
                               plotlyOutput("Einsender_Selbst_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "center",
                               plotlyOutput("Einsender_Fremd_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Einsender_Overlay_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                  )
                )
              )
            )
          )
 ),
 
 tabPanel("Qualitätsindikatoren",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 58
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Auftragsgenerierung"))),
                         titlePanel(
                           div(
                             tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                     strong("58. Welche Verfahren zur Auftragsgenerierung bietet das Labor dem Einsender an (z. B. order entry etc.)? "))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext115", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext116", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 59
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("59.	Gibt es Ausfallkonzepte? "))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext117", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext118", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 60
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;", strong("60.	Erfolgt nach Auftragsgenerierung eine Kontrolle der Anforderung?"))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext119", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext120", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 61
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("61. Sind bei Ihnen Anforderungsprofile definiert? "))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext121", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext122", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 62
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("62.	Sind bei Ihnen diagnostische Pfade definiert? "))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext123", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext124", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 63
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Probengewinnung"))),
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("63.	Welche Hinweise zur Präanalytik und in welcher Form stellt das Labor den Einsendern bei der Auftragsgenerierung zur Verfügung?"))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext125", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext126", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 64
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Probentransport"))),
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("64.	In welcher Weise erfolgt ein Monitoring beim Probentransport? "))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext127", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext128", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben..." ,height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 65
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Präanalytik im Labor"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("65.	Welche präanalytischen Prüfungen erfolgen im Labor?"))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext129", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext130", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 66
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Analytik"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("66.	Werden Rückmeldungen der Einsender bei der Auswahl / Durchführung der Analytik berücksichtigt? "))
                         ),
                         
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext131", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext132", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 67
                         
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("67.	Welche Kriterien spielen bei der Auswahl, z.B.  Neueinführung der Analytik eine Rolle? "))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext133", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext134", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 68
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien :  Probenmanagement innerhalb des Labors"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("68.	Wie sieht Ihr Probenfluss-Konzept aus?"))
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext135", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext136", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 69
                         titlePanel(div(class = "title-panel",tags$h4("Kriterien :  TAT Labor"))),
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("69.	Kann für bestimmte Messgrößen die TAT im Labor ermittelt werden?"))),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext137", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext138", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         )
                         
                  ),
                  
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 70
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  TAT gesamt (von Anforderung bis Befundregistrierung)"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("70.	Kann für bestimmte Messgrößen die TAT im Labor ermittelt werden?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext139", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext140", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 71
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Befundung"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("71.	Werden Analysenergebnisse ärztlicherseits ergänzend textlich befundet?"))),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext141", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext142", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 72
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Befundübermittlung"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("72.	Wie erfolgt die Befundübermittlung?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext143", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext144", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 73
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Probenarchivierung"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("73.	Wie erfolgt die Probenarchivierung?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext145", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext146", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 74
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Erbringung von Service und Beratungsleitungen"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("74.	Welche Service- und Beratungsleistungen erbringen Sie?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext147", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext148", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 75
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  klinische Scores auf Basis/mit Anteil laboratoriumsmedizinischer Untersuchungen"))),
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("75.	Welche Service- und Beratungsleistungen erbringen Sie?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext149", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext150", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 76
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("76.	Werden Hinweise zur DRG Gruppierung ausgegeben?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext151", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext152", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 77
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("77.	Kann das Labor Statistiken und Auswertungen zur Verfügung stellen?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext153", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext154", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 78
                    titlePanel(div(class = "title-panel",
                                   tags$h4("Kriterien :  Umgang mit interner Qualitätssicherung"))),
                    
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("78.	Wie wird bei Ihnen die interne Qualitätssicherung durchgeführt?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext155", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext156", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 79
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("79.	Werden Maßnahmen durchgeführt, die über die Forderungen der Rili-BÄK hinausgehen?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext157", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext158", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 80
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("80.	Wie wird bei Ihnen die externe Qualitätssicherung durchgeführt?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext159", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext160", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                    # Question 81
                    titlePanel(
                      tags$h5(style = "width: 600px; margin-top: 80px;", # Add margin-top to push it down
                              strong("81.	Gibt es regelmäßige Auswertungen der RV?"))
                    ),
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext161", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext162", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    column(4,align = "left",
                           div(align = "left",
                               plotlyOutput("Qualit_Selbst_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "center",
                               plotlyOutput("Qualit_Fremd_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Qualit_Overlay_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                  )
                )
              )
            )
          )
 ),
 
 tabPanel("Technische und Medizinische Validation",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 83
                         titlePanel(
                           div(
                             tags$h5(strong("83. Wie erfolgt bei Ihnen der Prozess der technischen und der medizinischen Validation?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext163", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext164", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         )
                  ),
                  
                  column(
                    width = 5,
                    style = "white-space: nowrap;",
                    
                    # Question 84
                    titlePanel(
                      tags$h5(style = "text-align: left;",
                              strong("84.	Wann wird die medizinische Validation durchgeführt (bei einem 24h-Labor)?"))
                    )
                    ,
                    div(
                      style = "width: 600px;",
                      fluidRow(
                        column(width = 6,
                               p("Selbstbewertung Freitext"),
                               radioButtons("Freitext165", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        ),
                        column(width = 6,
                               p("Fremdbewertung Freitext"),
                               radioButtons("Freitext166", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                               div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                        )
                      )
                    ),
                    
                  ),
                  
                  fluidRow(
                    column(4, align = "left",
                           div(align = "left",
                               plotlyOutput("Tech_Selbst_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "center",
                               plotlyOutput("Tech_Fremd_Plot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    ),
                    column(4,
                           div(align = "right",
                               plotlyOutput("Tech_overlayPlot", height = "100%"),
                               style = "margin-right: 500px;"  # Add margin to create space
                           ),
                    )
                  ),
                  mainPanel(
                  )
                )
              )
            )
          )
 ),
 
 
 tabPanel("Mikrobiologie",
          fluidRow(
            column(
              width = 12,
              align = "center", # Center-align the contents
              div(
                tags$h4("Ausgewählte Fragestellungen und Aspekte"),
                tags$h5("Selbstein-schätzung Reifegrad (0 = kein Antwort, 1 = sehr niedrig bis 5 =sehr hoch)")
              )
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              div(
                style = "border: 1px solid #dddddd; padding: 10px;",
                tags$head(tags$style("
                  #container * {  display: inline;}")),
                
                fluidRow(
                  column(5,
                         offset = 1,  # To center the first column
                         style = "white-space: nowrap;",
                         
                         # Question 85
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Mikroskopische Verfahren"))),
                         titlePanel(
                           div(
                             tags$h5(strong("85. Welche Mikroskopischen Verfahren setzen Sie ein?"))
                           )
                         ),
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext167", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext168", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 86
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("86.	Welche Kontrollen werden verwendet? Z. B. selbst hergestellt oder gekauft, grampositive und gramnegative Referenzstämme, Zungenabstrich, Virenstämme"))
                         )
                         
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext169", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext170", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 87
                         
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("87. Wie wird die Abweichung vom Soll gemessen (z.B. Parasiten) und welche Konsequenzen werden daraus gezogen?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext171", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext172", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 88
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("88.	Wurde die Färbung (Teil-) Automatisiert, wenn ja, wie"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext173", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext174", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 89
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Kulturelle Verfahren"))),
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("89.	Wie wird die Eingangskontrolle von Nährmedien durchgeführt?Sichtkontrolle, Wachstumskontrollen, Referenzstämme, Inkubationszeiten (Sabouraud 3 Tage)"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext175", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext176", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         
                         # Question 90
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("90.	Welche Ausweichszenarien existieren, wenn Nährmedien kurzfristig nicht lieferbar sind?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext177", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext178", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         
                         # Question 91
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Erregeridentifizierung"))),
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("91.	Wie werden die orientierenden Erregeridentifizierungen durchgeführt?z.B. welche, pro Arbeitsplatz, Kontrollen, Referenzstämme, Dokumentation"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext179", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext180", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 92
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("92.	Wie erfolgt die Verifizierung von kommerziellen Identifizierungssystemen z.B. durch Referenzstämme, chargenabhängig, Dokumentationen"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext181", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext182", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 93
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("93.	Wie wird bei Abweichungen vorgegangen?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext183", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext184", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         
                         # Question 94
                         titlePanel(
                           tags$h5(style = "text-align: left;",
                                   strong("94.	Wie wird bei verunreinigten Reinheitskontrollen vorgegangen?"))
                         )
                         ,
                         div(
                           style = "width: 600px;",
                           fluidRow(
                             column(width = 6,
                                    p("Selbstbewertung Freitext"),
                                    radioButtons("Freitext185", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             ),
                             column(width = 6,
                                    p("Fremdbewertung Freitext"),
                                    radioButtons("Freitext186", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
                                    div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
                             )
                           )
                         ),
                         # Question 95
                         titlePanel(div(class = "title-panel",
                                        tags$h4("Kriterien : Empfindlichkeitsprüfung"))),
                         titlePanel(
                           tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
                                   strong("95.	Wie wird die Empfindlichkeitsprüfung vorgenommen?
z.B. automatisch, Art - Mikrodilution, Agardiffusion E-test"))
                         )
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext187", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext188", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 96
titlePanel(
  tags$h5(style = "text-align: left;",
          strong("96.	Nach welcher Norm wird getestet?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext189", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext190", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 97
titlePanel(
  tags$h5(style = "text-align: left;",
          strong("97.	Erfolgt eine Resistenztestung aus Originalmaterial (z.B. Punktate, Blutkulturen)?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext191", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext192", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 98

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("98.	Wie wird der Betalaktamase-Nachweis überprüft"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext193", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext194", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 99

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("99.	Wie häufig wurde die Empfindlichkeitsprüfung verifiziert?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext195", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext196", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 100

titlePanel(
  tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
          strong("100.	Wie erfolgt die laufende Qualitätskontrolle der Empfindlichkeitsprüfung
z.B. wöchentlich und bei Chargenwechsel; bei jeder Durchführung bei Systemen, die seltener als einmal wöchentlich eingesetzt werden; Dokumentation
"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext197", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext198", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 101

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("101.	Wie wird bei Abweichungen bei der Qualitätskontrolle vorgegangen?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext199", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext200", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 102

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("102.	Wie wird vorgegangen, wenn die Reinheitskontrolle verunreinigt ist?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext201", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext202", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),



                  ),

column(
  width = 5,
  style = "white-space: nowrap;",
  
  # Question 103
  
  titlePanel(
    tags$h5(style = "width: 600px; margin-top: 80px;", # Add margin-top to push it down
            strong("103.	Wie werden die Referenzstämme im Labor gehandhabt?"))
  ),
  
  
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext203", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext204", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 104
  titlePanel(div(class = "title-panel",tags$h4("Kriterien : Zellkultur-basierte Verfahren"))),
  
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("104.	Welche Zellkultur-basierte Verfahren werden bei Ihnen eingesetzt?"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext205", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext206", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 105
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("105.	Wie erfolgt die Überprüfung der Permissivität der Zellkulturen"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext207", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext208", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 106
  titlePanel(
    tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
            strong("106.	Wie erfolgt die Reinheitskontrolle der Zellkulturen? z.B. virale Kontamination – PCR auf Viren, Musterung negativer Kulturen auf zytopathische Effekte"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext209", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext210", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 107
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("107.	Beschreiben Sie die Vorgehensweise, Kontaminationen durch Mykoplasmen auszuschließen"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext211", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext212", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 108
  titlePanel(
    tags$h5(
      strong("108.	Wie wird bei Kontaminationen vorgegangen?"))),
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext213", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext214", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  
  # Question 109
  titlePanel(div(class = "title-panel",tags$h4("Kriterien : Molekularbiologische Verfahren"))),
  
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("109.	Welche Molekularbiologischen Verfahren werden bei Ihnen eingesetzt - PCR (quant und Schmelzkurven)"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext215", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext216", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 110
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("110.	Setzen Sie sog. Inhouse-PCRs ein?- nein nur kommerzielle"))
  )
  ,
  div(
    style = "width: 600px;",
    fluidRow(
      column(width = 6,
             p("Selbstbewertung Freitext"),
             radioButtons("Freitext217", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      ),
      column(width = 6,
             p("Fremdbewertung Freitext"),
             radioButtons("Freitext218", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
             div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
      )
    )
  ),
  
  # Question 111
  titlePanel(
    tags$h5(style = "text-align: left;",
            strong("111.	Welche Schritte erfolgen automatisiert?
- Extraktion, PCR-Ansatz, PCR
"))
  )
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext219", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext220", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 112
titlePanel(
  tags$h5(style = "text-align: left;",
          strong("112.	Welche geschlossenen (Kartuschen)- Systeme werden bei Ihnen benutzt? - LIAT, ELITECH"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext221", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext222", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 113
titlePanel(
  tags$h5(style = "text-align: left;",
          strong("113.	Wie werden die NAT-Verfahren validiert?- nach SOP"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext223", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext224", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 114
titlePanel(
  tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
          strong("114.	Wie erfolgen die Kontrollen der folgenden Schritte für die einzelnen Verfahren? Nukleinsäureniso-lierung Reaktionskomponenten/Mastermix
Amplifikation - mit internen Kontrollen, Neg/Pos-Kontrollen"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext225", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext226", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),



# Question 115
titlePanel(
  tags$h5(style = "text-align: left;",
          strong("115.	Welche Sequenzbasierten Verfahren (NAT, FISH, andere Hybridisierungsver-fahren) werden bei Ihnen eingesetzt?"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext227", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext228", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

# Question 116
titlePanel(div(class = "title-panel",tags$h4("Kriterien : Immunologische Verfahren"))),

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("116.	Wie werden diagnostische Antikörper im Labor gehandhabt (eingesetzt?)- ECLIA, ELISA"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext229", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext230", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 117
titlePanel(
  tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
          strong("117.	Wie erfolgt die Umsetzung der Qualitätskontrolle von Antigennachweisen bei:Immunchem-ischen Verfahren (ELISA, ELFA, CLIA) Schnelltests (Immunchrom-atographische Verfahren) Direkter Immunfluores-zenztest Agglutinations-assays"))
),

div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext231", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext232", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 118
titlePanel(div(class = "title-panel",tags$h4("Kriterien : Ringversuche"))),

titlePanel(
  tags$h5(style = "text-align: left;",
          strong("118.	Nach welchen Gesichtspunkten erfolgt die Ringversuchsplanung?
- alle diagnostisch eingesetzten Analyte"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext233", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext234", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),


# Question 119

titlePanel(
  tags$h5(style = "display: flex; flex-direction: row; align-items: center;white-space: pre-wrap;",
          strong("119.	Wie wird bei nicht bestandenen Ringversuchen vorgegangen?
- Überprüfung sämtlicher Prozesse, Kontrollen, Dokumentation"))
)
,
div(
  style = "width: 600px;",
  fluidRow(
    column(width = 6,
           p("Selbstbewertung Freitext"),
           radioButtons("Freitext235", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Selbstbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    ),
    column(width = 6,
           p("Fremdbewertung Freitext"),
           radioButtons("Freitext236", label = NULL, choices = c("N/A", 1:5), selected = "N/A", inline = TRUE),
           div(textAreaInput("Fremdbewertung_text", label = NULL, placeholder = "Text eingeben...",height="100px", width="250px"))
    )
  )
),

),

fluidRow(
  column(4, align = "left",
         div(align = "left",
             plotlyOutput("Mikro_Selbst_Plot", height = "100%"),
             style = "margin-right: 500px;"  # Add margin to create space
         ),
  ),
  column(4,
         div(align = "center",
             plotlyOutput("Mikro_Fremd_Plot", height = "100%"),
             style = "margin-right: 500px;"  # Add margin to create space
         ),
  ),
  column(4,
         div(align = "right",
             plotlyOutput("Mikro_overlayPlot", height = "100%"),
             style = "margin-right: 500px;"  # Add margin to create space
         ),
  )
),
mainPanel(
)
                )
              )
            )
          )
 ),

  ),
use_theme(mytheme),
shinyjs::hidden(
  div(
    id = "thankyou_msg",
    h3("Thanks, your response was submitted successfully!")                    
  )
)
    )
  )
}


server <- function(input, output, session) {
  
  
  # observe({
  #   # check if both mandatory fields have a value
  #   mandatoryFilled <- vapply(fieldsMandatory,
  #                             function(x) {
  #                               if (x == "input.Berufsgruppe") {
  #                                 !is.null(input$Berufsgruppe) && input$Berufsgruppe != "N/A"
  #                               } else {
  #                                 !is.null(input[[x]]) && input[[x]] != ""
  #                               }
  #                             },
  #                             logical(1))
  #   mandatoryFilled <- all(mandatoryFilled)
  #   
  #   # enable/disable the submit button
  #   shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  # })
  
  observe({
    # Check if all mandatory fields have a value
    mandatoryFilled <- vapply(fieldsMandatory,
                              function(x) {
                                if (x == "Berufsgruppe") {
                                  !is.null(input$Berufsgruppe) && input$Berufsgruppe != "N/A"
                                } else {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                }
                              },
                              logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # Enable/disable the submit button based on mandatory fields
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  #############################################################################
  # "Input durch Peer Review" part
  # it adds two text boxes "Beschreibung" and "Anmerkungen des Peers" when user press "Add More" button
  
  anmerkungen <- reactiveValues(counter = 1)
  
  observeEvent(input$addAnmerkung, {
    anmerkungen$counter <- anmerkungen$counter + 1
  })
  
  output$anmerkungenInputs <- renderUI({
    anmerkungenInputs <- lapply(1:anmerkungen$counter, function(i) {
      tagList(
        textAreaInput(paste0("Beschreibung", i), "Beschreibung", rows = 3),
        textInput(paste0("Anmerkung", i), "Anmerkungen des Peers")
      )
    })
    tagList(anmerkungenInputs)
  })
  
  ##############################################################################
  # Generate plot function
  # spider plots
  
  # Output graph
  output$SelbstbewertungPlot1 <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            fillcolor = "rgba(231, 184, 0, 0.5)",
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext1,input$Freitext3,input$Freitext5,
                  input$Freitext7,input$Freitext9,input$Freitext11,
                  input$Freitext13,input$Freitext15),
            theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                      "<b>6</b>","<b>7</b>","<b>8</b>"
            )) |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 400,  # Adjust the width of the plot
      height = 300  # Adjust the height of the plot
      )
    
  })
  
  
  # Creating plots individually and passing them as a list of parameters to RMD
  # Example for the first two measurands
  Selbstbewertung.Plot1 <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            fillcolor = "rgba(231, 184, 0, 0.5)",
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext1,input$Freitext3,input$Freitext5,
                  input$Freitext7,input$Freitext9,input$Freitext11,
                  input$Freitext13,input$Freitext15),
            theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                      "<b>6</b>","<b>7</b>","<b>8</b>"
            )) |> 
    layout(title = list(
      text = "Führung: Selbstbewertung",
      y = 0.99 ))

  })
  

  output$FremdbewertungPlot1 <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            fillcolor = "rgba(194, 242, 255, 0.5)",
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext2,input$Freitext4,input$Freitext6,input$Freitext8,input$Freitext10,input$Freitext12,input$Freitext14,input$Freitext16),
            theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                      "<b>6</b>","<b>7</b>","<b>8</b>"
                      
            )) |>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 400,  # Adjust the width of the plot
      height = 300  # Adjust the height of the plot
      )
    
  })
  
  
  Fremdbewertung.Plot1 <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext2,input$Freitext4,input$Freitext6,input$Freitext8,input$Freitext10,input$Freitext12,input$Freitext14,input$Freitext16),
            theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                      "<b>6</b>","<b>7</b>","<b>8</b>"
            )) |> 
      layout(title = list(
        text = "Führung: Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ))
  })
  
  
  # Overlay plot which combines both  and FremdbewertungPlot
  output$Selbstbewertung_FremdbewertungPlot1 <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext1,input$Freitext3,input$Freitext5,input$Freitext7,
              input$Freitext9,input$Freitext11,input$Freitext13,input$Freitext15),
        theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                  "<b>6</b>","<b>7</b>","<b>8</b>"),
        fillcolor = "rgba(231, 184, 0, 0.5)"
      ) |>
      
      add_trace(
        r = c(input$Freitext2,input$Freitext4,input$Freitext6,input$Freitext8,
              input$Freitext10,input$Freitext12,input$Freitext14,input$Freitext16),
        theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                  "<b>6</b>","<b>7</b>","<b>8</b>"),
        fillcolor = "rgba(194, 242, 255, 0.5)"
      ) |>
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 400,  # Adjust the width of the plot
      height = 300  # Adjust the height of the plot
      )
    
  })
  

  
  # Overlay plot which combines both  and FremdbewertungPlot
  Selbstbewertung_Fremdbewertung.Plot1 <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext1,input$Freitext3,input$Freitext5,input$Freitext7,
              input$Freitext9,input$Freitext11,input$Freitext13,input$Freitext15),
        theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                  "<b>6</b>","<b>7</b>","<b>8</b>"),
        fillcolor = "rgba(231, 184, 0, 0.5)"
      ) |>
      
      add_trace(
        r = c(input$Freitext2,input$Freitext4,input$Freitext6,input$Freitext8,
              input$Freitext10,input$Freitext12,input$Freitext14,input$Freitext16),
        theta = c("<b>1</b>","<b>2</b>","<b>3</b>","<b>4</b>","<b>5</b>",
                  "<b>6</b>","<b>7</b>","<b>8</b>"),
        fillcolor = "rgba(194, 242, 255, 0.5)"
      ) |>
      layout(title = list(
        text = "Führung: Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ))
    
  })
  
  
  
  ######### Mitarbeitende Plots ################
  output$Mitarbeitende_Selbst_Plot <- renderPlotly({
    plot_ly(
      mode = 'markers+lines',
      type = 'scatterpolar',
      fill = 'toself',
      line = list(color = 'rgb(0, 0, 0)'),
      marker = list(
        symbol = 100,
        size = 6,
        color = 'rgb(0, 0, 0)',
        line = list(color = 'rgb(0, 0, 0)', width = 1)
      ),
      r = c(
        input$Freitext17, input$Freitext19, input$Freitext21, input$Freitext23,
        input$Freitext25, input$Freitext27, input$Freitext29, input$Freitext31,
        input$Freitext33, input$Freitext35, input$Freitext37, input$Freitext39,
        input$Freitext41, input$Freitext43, input$Freitext45, input$Freitext47,
        input$Freitext49, input$Freitext51, input$Freitext53, input$Freitext55,
        input$Freitext57
      ),
      theta = c(
        "<b>9</b>", "<b>10</b>", "<b>11</b>", "<b>12</b>", "<b>13</b>", "<b>14</b>",
        "<b>15</b>", "<b>16</b>", "<b>17</b>", "<b>18</b>", "<b>19</b>", "<b>20</b>",
        "<b>21</b>", "<b>22</b>", "<b>23</b>", "<b>24</b>", "<b>25</b>", "<b>26</b>",
        "<b>27</b>", "<b>28</b>", "<b>29</b>"
      ),
      fillcolor = "rgba(231, 184, 0, 0.5)"
    ) |>
      layout(
        title = list(
          text = "Selbstbewertung",
          y = 0.99  # Adjust the value to move the title higher
        ),
        polar = list(radialaxis = list(visible = T, range = c(0, 5))),
        angularaxis = list(
          rotation = 90,  # Adjust the rotation angle to make the plot circular
          direction = "clockwise"
        ),
        showlegend = FALSE,
        margin = list(l = 30, r = 30, b = 30, t = 30),
        font = list(size = 9, color = 'black'),
        paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
        plot_bgcolor = "#B4D9F9",
        width = 500,  # Adjust the width of the plot
        height = 400  # Adjust the height of the plot
      )
    
  })
  
  Mitarbeitende_Selbst.Plot <- reactive({
    plot_ly(
      mode = 'markers+lines',
      type = 'scatterpolar',
      fill = 'toself',
      line = list(color = 'rgb(0, 0, 0)'),
      marker = list(
        symbol = 100,
        size = 6,
        color = 'rgb(0, 0, 0)',
        line = list(color = 'rgb(0, 0, 0)', width = 1)
      ),
      r = c(
        input$Freitext17, input$Freitext19, input$Freitext21, input$Freitext23,
        input$Freitext25, input$Freitext27, input$Freitext29, input$Freitext31,
        input$Freitext33, input$Freitext35, input$Freitext37, input$Freitext39,
        input$Freitext41, input$Freitext43, input$Freitext45, input$Freitext47,
        input$Freitext49, input$Freitext51, input$Freitext53, input$Freitext55,
        input$Freitext57
      ),
      theta = c(
        "<b>9</b>", "<b>10</b>", "<b>11</b>", "<b>12</b>", "<b>13</b>", "<b>14</b>",
        "<b>15</b>", "<b>16</b>", "<b>17</b>", "<b>18</b>", "<b>19</b>", "<b>20</b>",
        "<b>21</b>", "<b>22</b>", "<b>23</b>", "<b>24</b>", "<b>25</b>", "<b>26</b>",
        "<b>27</b>", "<b>28</b>", "<b>29</b>"
      ),
      fillcolor = "rgba(231, 184, 0, 0.5)"
    ) |>
      layout(
        title = list(
          text = "Mitarbeitende: Selbstbewertung",
          y = 0.99  # Adjust the value to move the title higher
        )
      )
    
  })
  
  

  
  # Generate plot function
  output$Mitarbeitende_Fremd_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext18,input$Freitext20,input$Freitext22,input$Freitext24,
                  input$Freitext26,input$Freitext28,input$Freitext30,input$Freitext32,
                  input$Freitext34,input$Freitext36,input$Freitext38,input$Freitext40,
                  input$Freitext42,input$Freitext44,input$Freitext46,input$Freitext48,
                  input$Freitext50,input$Freitext52,input$Freitext54,input$Freitext56,
                  input$Freitext58
            ),
            theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                      "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                      "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                      "<b>27</b>","<b>28</b>","<b>29</b>"),
            fillcolor = "rgba(194, 242, 255, 0.5)") |>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  
  # Generate plot function
  Mitarbeitende_Fremd.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext18,input$Freitext20,input$Freitext22,input$Freitext24,
                  input$Freitext26,input$Freitext28,input$Freitext30,input$Freitext32,
                  input$Freitext34,input$Freitext36,input$Freitext38,input$Freitext40,
                  input$Freitext42,input$Freitext44,input$Freitext46,input$Freitext48,
                  input$Freitext50,input$Freitext52,input$Freitext54,input$Freitext56,
                  input$Freitext58
            ),
            theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                      "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                      "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                      "<b>27</b>","<b>28</b>","<b>29</b>"),
            fillcolor = "rgba(194, 242, 255, 0.5)") |>
      layout(title = list(
        text = "Mitarbeitende: Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  
  output$Mitarbeitende_overlay_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext17,input$Freitext19,input$Freitext21,input$Freitext23,
              input$Freitext25,input$Freitext27,input$Freitext29,input$Freitext31,
              input$Freitext33,input$Freitext35,input$Freitext37,input$Freitext39,
              input$Freitext41,input$Freitext43,input$Freitext45,input$Freitext47,
              input$Freitext49,input$Freitext51,input$Freitext53,input$Freitext55,
              input$Freitext57
        ),
        theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                  "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                  "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                  "<b>27</b>","<b>28</b>","<b>29</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext18,input$Freitext20,input$Freitext22,input$Freitext24,
              input$Freitext26,input$Freitext28,input$Freitext30,input$Freitext32,
              input$Freitext34,input$Freitext36,input$Freitext38,input$Freitext40,
              input$Freitext42,input$Freitext44,input$Freitext46,input$Freitext48,
              input$Freitext50,input$Freitext52,input$Freitext54,input$Freitext56,
              input$Freitext58
        ),
        theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                  "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                  "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                  "<b>27</b>","<b>28</b>","<b>29</b>"),
        
        fillcolor = "rgba(194, 242, 255, 0.5)") |>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  Mitarbeitende_overlay.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext17,input$Freitext19,input$Freitext21,input$Freitext23,
              input$Freitext25,input$Freitext27,input$Freitext29,input$Freitext31,
              input$Freitext33,input$Freitext35,input$Freitext37,input$Freitext39,
              input$Freitext41,input$Freitext43,input$Freitext45,input$Freitext47,
              input$Freitext49,input$Freitext51,input$Freitext53,input$Freitext55,
              input$Freitext57
        ),
        theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                  "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                  "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                  "<b>27</b>","<b>28</b>","<b>29</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext18,input$Freitext20,input$Freitext22,input$Freitext24,
              input$Freitext26,input$Freitext28,input$Freitext30,input$Freitext32,
              input$Freitext34,input$Freitext36,input$Freitext38,input$Freitext40,
              input$Freitext42,input$Freitext44,input$Freitext46,input$Freitext48,
              input$Freitext50,input$Freitext52,input$Freitext54,input$Freitext56,
              input$Freitext58
        ),
        theta = c("<b>9</b>","<b>10</b>","<b>11</b>","<b>12</b>","<b>13</b>","<b>14</b>",
                  "<b>15</b>","<b>16</b>","<b>17</b>","<b>18</b>","<b>19</b>","<b>20</b>",
                  "<b>21</b>","<b>22</b>","<b>23</b>","<b>24</b>","<b>25</b>","<b>26</b>",
                  "<b>27</b>","<b>28</b>","<b>29</b>"),
        
        fillcolor = "rgba(194, 242, 255, 0.5)") |>
      
      layout(title = list(
        text = "Mitarbeitende: Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      )
      )
    
  })
  
  ##################################
  ##### Patient und Angehörige #####
  ##################################
  
  # Generate plot function
  output$Patient_Selbst_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext59, input$Freitext61, input$Freitext63, input$Freitext65,
                  input$Freitext67,input$Freitext69, input$Freitext71, input$Freitext73,
                  input$Freitext75, input$Freitext77,input$Freitext79, input$Freitext81,
                  input$Freitext83, input$Freitext85),
            theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                      "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                      "<b>42</b>", "<b>43</b>"),
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  # Generate plot function
  Patient_Selbst.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext59, input$Freitext61, input$Freitext63, input$Freitext65,
                  input$Freitext67,input$Freitext69, input$Freitext71, input$Freitext73,
                  input$Freitext75, input$Freitext77,input$Freitext79, input$Freitext81,
                  input$Freitext83, input$Freitext85),
            theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                      "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                      "<b>42</b>", "<b>43</b>"),
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Patient und Angehörige: Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Patient_Fremd_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          line = list(width = 1)
            ),
            r = c(input$Freitext60, input$Freitext62, input$Freitext64, input$Freitext66,
                  input$Freitext68,input$Freitext70, input$Freitext72, input$Freitext74,
                  input$Freitext76, input$Freitext78,input$Freitext80, input$Freitext82,
                  input$Freitext84, input$Freitext86),
            theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                      "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                      "<b>42</b>", "<b>43</b>"),
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  Patient_Fremd.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          line = list(width = 1)
            ),
            r = c(input$Freitext60, input$Freitext62, input$Freitext64, input$Freitext66,
                  input$Freitext68,input$Freitext70, input$Freitext72, input$Freitext74,
                  input$Freitext76, input$Freitext78,input$Freitext80, input$Freitext82,
                  input$Freitext84, input$Freitext86),
            theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                      "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                      "<b>42</b>", "<b>43</b>"),
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Patient und Angehörige: Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  
  output$Patient_overlay_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext59, input$Freitext61, input$Freitext63, input$Freitext65,
              input$Freitext67,input$Freitext69, input$Freitext71, input$Freitext73,
              input$Freitext75, input$Freitext77,input$Freitext79, input$Freitext81,
              input$Freitext83, input$Freitext85),
        theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                  "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                  "<b>42</b>", "<b>43</b>"),
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext60, input$Freitext62, input$Freitext64, input$Freitext66,
              input$Freitext68,input$Freitext70, input$Freitext72, input$Freitext74,
              input$Freitext76, input$Freitext78,input$Freitext80, input$Freitext82,
              input$Freitext84, input$Freitext86),
        theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                  "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                  "<b>42</b>", "<b>43</b>"),
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  Patient_overlay.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext59, input$Freitext61, input$Freitext63, input$Freitext65,
              input$Freitext67,input$Freitext69, input$Freitext71, input$Freitext73,
              input$Freitext75, input$Freitext77,input$Freitext79, input$Freitext81,
              input$Freitext83, input$Freitext85),
        theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                  "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                  "<b>42</b>", "<b>43</b>"),
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext60, input$Freitext62, input$Freitext64, input$Freitext66,
              input$Freitext68,input$Freitext70, input$Freitext72, input$Freitext74,
              input$Freitext76, input$Freitext78,input$Freitext80, input$Freitext82,
              input$Freitext84, input$Freitext86),
        theta = c("<b>30</b>", "<b>31</b>", "<b>32</b>", "<b>33</b>", "<b>34</b>", "<b>35</b>",
                  "<b>36</b>", "<b>37</b>", "<b>38</b>", "<b>39</b>", "<b>40</b>", "<b>41</b>",
                  "<b>42</b>", "<b>43</b>"),
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Patient und Angehörige: Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  
  ##############################################
  ##### Einsender und Kooperationespartner #####
  ##############################################
  
  output$Einsender_Selbst_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext87, input$Freitext89, input$Freitext91, input$Freitext93,
                  input$Freitext95, input$Freitext97, input$Freitext99, input$Freitext101,
                  input$Freitext103, input$Freitext105, input$Freitext107, input$Freitext109,
                  input$Freitext111, input$Freitext113),
            
            theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                      "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                      "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })

  Einsender_Selbst.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext87, input$Freitext89, input$Freitext91, input$Freitext93,
                  input$Freitext95, input$Freitext97, input$Freitext99, input$Freitext101,
                  input$Freitext103, input$Freitext105, input$Freitext107, input$Freitext109,
                  input$Freitext111, input$Freitext113),
            
            theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                      "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                      "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Einsender und Kooperationspartner: Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  
  
  output$Einsender_Fremd_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext88, input$Freitext90, input$Freitext92, input$Freitext94,
                  input$Freitext96,input$Freitext98, input$Freitext100, input$Freitext102,
                  input$Freitext104, input$Freitext106,input$Freitext108, input$Freitext110,
                  input$Freitext112, input$Freitext114),
            
            theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                      "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                      "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
            
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  Einsender_Fremd.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            r = c(input$Freitext88, input$Freitext90, input$Freitext92, input$Freitext94,
                  input$Freitext96,input$Freitext98, input$Freitext100, input$Freitext102,
                  input$Freitext104, input$Freitext106,input$Freitext108, input$Freitext110,
                  input$Freitext112, input$Freitext114),
            
            theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                      "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                      "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
            
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Einsender und Kooperationspartner: Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Einsender_Overlay_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext87, input$Freitext89, input$Freitext91, input$Freitext93,
              input$Freitext95, input$Freitext97, input$Freitext99, input$Freitext101,
              input$Freitext103, input$Freitext105, input$Freitext107, input$Freitext109,
              input$Freitext111, input$Freitext113),
        
        theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                  "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                  "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext88, input$Freitext90, input$Freitext92, input$Freitext94,
              input$Freitext96,input$Freitext98, input$Freitext100, input$Freitext102,
              input$Freitext104, input$Freitext106,input$Freitext108, input$Freitext110,
              input$Freitext112, input$Freitext114),
        
        theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                  "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                  "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
        
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  Einsender_Overlay.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext87, input$Freitext89, input$Freitext91, input$Freitext93,
              input$Freitext95, input$Freitext97, input$Freitext99, input$Freitext101,
              input$Freitext103, input$Freitext105, input$Freitext107, input$Freitext109,
              input$Freitext111, input$Freitext113),
        
        theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                  "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                  "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext88, input$Freitext90, input$Freitext92, input$Freitext94,
              input$Freitext96,input$Freitext98, input$Freitext100, input$Freitext102,
              input$Freitext104, input$Freitext106,input$Freitext108, input$Freitext110,
              input$Freitext112, input$Freitext114),
        
        theta = c("<b>44</b>", "<b>45</b>", "<b>46</b>", "<b>47</b>", "<b>48</b>",
                  "<b>49</b>", "<b>50</b>", "<b>51</b>", "<b>52</b>", "<b>53</b>",
                  "<b>54</b>", "<b>55</b>", "<b>56</b>", "<b>57</b>"),
        
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Einsender und Kooperationspartner: Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  ###################################################
  ################# Qualitätsindikatoren ############
  ###################################################
  output$Qualit_Selbst_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext115, input$Freitext117, input$Freitext119, input$Freitext121,
                  input$Freitext123, input$Freitext125, input$Freitext127, input$Freitext129,
                  input$Freitext131, input$Freitext133, input$Freitext135, input$Freitext137,
                  input$Freitext139, input$Freitext141, input$Freitext143, input$Freitext145,
                  input$Freitext147, input$Freitext149, input$Freitext151, input$Freitext153,
                  input$Freitext155, input$Freitext157, input$Freitext159, input$Freitext161),
            
            theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                      "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                      "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                      "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                      "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  
  Qualit_Selbst.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext115, input$Freitext117, input$Freitext119, input$Freitext121,
                  input$Freitext123, input$Freitext125, input$Freitext127, input$Freitext129,
                  input$Freitext131, input$Freitext133, input$Freitext135, input$Freitext137,
                  input$Freitext139, input$Freitext141, input$Freitext143, input$Freitext145,
                  input$Freitext147, input$Freitext149, input$Freitext151, input$Freitext153,
                  input$Freitext155, input$Freitext157, input$Freitext159, input$Freitext161),
            
            theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                      "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                      "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                      "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                      "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Qualitätsindikatoren : Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  

  
  
  output$Qualit_Fremd_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext116, input$Freitext118, input$Freitext120, input$Freitext122,
                  input$Freitext124, input$Freitext126, input$Freitext128, input$Freitext130,
                  input$Freitext132, input$Freitext134, input$Freitext136, input$Freitext138,
                  input$Freitext140, input$Freitext142, input$Freitext144, input$Freitext146,
                  input$Freitext148, input$Freitext150, input$Freitext152, input$Freitext154,
                  input$Freitext156, input$Freitext158, input$Freitext160, input$Freitext162),
            
            theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                      "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                      "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                      "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                      "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
            
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  Qualit_Fremd.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext116, input$Freitext118, input$Freitext120, input$Freitext122,
                  input$Freitext124, input$Freitext126, input$Freitext128, input$Freitext130,
                  input$Freitext132, input$Freitext134, input$Freitext136, input$Freitext138,
                  input$Freitext140, input$Freitext142, input$Freitext144, input$Freitext146,
                  input$Freitext148, input$Freitext150, input$Freitext152, input$Freitext154,
                  input$Freitext156, input$Freitext158, input$Freitext160, input$Freitext162),
            
            theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                      "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                      "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                      "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                      "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
            
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Qualitätsindikatoren : Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Qualit_Overlay_Plot <- renderPlotly({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext115, input$Freitext117, input$Freitext119, input$Freitext121,
              input$Freitext123, input$Freitext125, input$Freitext127, input$Freitext129,
              input$Freitext131, input$Freitext133, input$Freitext135, input$Freitext137,
              input$Freitext139, input$Freitext141, input$Freitext143, input$Freitext145,
              input$Freitext147, input$Freitext149, input$Freitext151, input$Freitext153,
              input$Freitext155, input$Freitext157, input$Freitext159, input$Freitext161),
        
        theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                  "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                  "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                  "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                  "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext116, input$Freitext118, input$Freitext120, input$Freitext122,
              input$Freitext124, input$Freitext126, input$Freitext128, input$Freitext130,
              input$Freitext132, input$Freitext134, input$Freitext136, input$Freitext138,
              input$Freitext140, input$Freitext142, input$Freitext144, input$Freitext146,
              input$Freitext148, input$Freitext150, input$Freitext152, input$Freitext154,
              input$Freitext156, input$Freitext158, input$Freitext160, input$Freitext162),
        
        theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                  "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                  "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                  "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                  "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
        
        
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  output$Qualit_Overlay.Plot <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext115, input$Freitext117, input$Freitext119, input$Freitext121,
              input$Freitext123, input$Freitext125, input$Freitext127, input$Freitext129,
              input$Freitext131, input$Freitext133, input$Freitext135, input$Freitext137,
              input$Freitext139, input$Freitext141, input$Freitext143, input$Freitext145,
              input$Freitext147, input$Freitext149, input$Freitext151, input$Freitext153,
              input$Freitext155, input$Freitext157, input$Freitext159, input$Freitext161),
        
        theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                  "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                  "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                  "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                  "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext116, input$Freitext118, input$Freitext120, input$Freitext122,
              input$Freitext124, input$Freitext126, input$Freitext128, input$Freitext130,
              input$Freitext132, input$Freitext134, input$Freitext136, input$Freitext138,
              input$Freitext140, input$Freitext142, input$Freitext144, input$Freitext146,
              input$Freitext148, input$Freitext150, input$Freitext152, input$Freitext154,
              input$Freitext156, input$Freitext158, input$Freitext160, input$Freitext162),
        
        theta = c("<b>58</b>", "<b>59</b>", "<b>60</b>", "<b>61</b>", "<b>62</b>",
                  "<b>63</b>", "<b>64</b>", "<b>65</b>", "<b>66</b>", "<b>67</b>",
                  "<b>68</b>", "<b>69</b>", "<b>70</b>", "<b>71</b>", "<b>72</b>",
                  "<b>73</b>", "<b>74</b>", "<b>75</b>", "<b>76</b>", "<b>77</b>",
                  "<b>78</b>", "<b>79</b>", "<b>80</b>", "<b>81</b>"),
        
        
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  
  Tech_SelbstPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext163,input$Freitext165),
            
            theta = c("<b>82</b>","<b>83</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Tech_Selbst_Plot <- renderUI({
    plotlyOutput("Tech_Selbst_Plot")
  })
  
  # Generate plot
  output$Tech_Selbst_Plot <- renderPlotly({
    Tech_SelbstPlot_generator()
  })
  
  
  Tech_FremdPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext164,input$Freitext166),
            
            theta = c("<b>82</b>","<b>83</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Tech_Fremd_Plot <- renderUI({
    plotlyOutput("Tech_Fremd_Plot")
  })
  
  # Generate plot
  output$Tech_Fremd_Plot <- renderPlotly({
    Tech_FremdPlot_generator()
  })
  
  Tech_overlayPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext163,input$Freitext165),
        
        theta = c("<b>82</b>","<b>83</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext164,input$Freitext166),
        
        theta = c("<b>82</b>","<b>83</b>"),
        
        
        fillcolor = "rgba(194, 242, 255, 0.5)")|>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  output$Tech_overlayPlot <- renderUI({
    plotlyOutput("Tech_overlayPlot")
  })
  output$Tech_overlayPlot <- renderPlotly({
    Tech_overlayPlot_generator()
  })
  
  Mikro_SelbstPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext167, input$Freitext169, input$Freitext171, input$Freitext173,
                  input$Freitext175, input$Freitext177, input$Freitext179, input$Freitext181,
                  input$Freitext183, input$Freitext185, input$Freitext187, input$Freitext189,
                  input$Freitext191, input$Freitext193, input$Freitext195, input$Freitext197,
                  input$Freitext199, input$Freitext201, input$Freitext203, input$Freitext205,
                  input$Freitext207, input$Freitext209, input$Freitext211, input$Freitext213,
                  input$Freitext215, input$Freitext217, input$Freitext219, input$Freitext221,
                  input$Freitext223, input$Freitext225, input$Freitext227, input$Freitext229,
                  input$Freitext231, input$Freitext233,input$Freitext235),
            
            theta = c("<b>85</b>", "<b>86</b>", "<b>87</b>", "<b>88</b>", "<b>89</b>",
                      "<b>90</b>", "<b>91</b>", "<b>92</b>", "<b>93</b>", "<b>94</b>",
                      "<b>95</b>", "<b>96</b>", "<b>97</b>", "<b>98</b>", "<b>99</b>",
                      "<b>100</b>", "<b>101</b>", "<b>102</b>", "<b>103</b>", "<b>104</b>",
                      "<b>105</b>", "<b>106</b>", "<b>107</b>", "<b>108</b>", "<b>109</b>",
                      "<b>110</b>","<b>111</b>","<b>112</b>","<b>113</b>","<b>114</b>",
                      "<b>115</b>","<b>116</b>","<b>117</b>","<b>118</b>","<b>119</b>"),
            
            fillcolor = "rgba(231, 184, 0, 0.5)") |>
      layout(title = list(
        text = "Selbstbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Mikro_Selbst_Plot <- renderUI({
    plotlyOutput("Mikro_Selbst_Plot")
  })
  
  # Generate plot
  output$Mikro_Selbst_Plot <- renderPlotly({
    Mikro_SelbstPlot_generator()
  })
  
  Mikro_FremdPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)),
            
            r = c(input$Freitext168, input$Freitext170, input$Freitext172, input$Freitext174,
                  input$Freitext176, input$Freitext178, input$Freitext180, input$Freitext182,
                  input$Freitext184, input$Freitext186, input$Freitext188, input$Freitext190,
                  input$Freitext192, input$Freitext194, input$Freitext196, input$Freitext198,
                  input$Freitext200, input$Freitext202, input$Freitext204, input$Freitext206,
                  input$Freitext208, input$Freitext210, input$Freitext212, input$Freitext214,
                  input$Freitext216, input$Freitext218, input$Freitext220, input$Freitext222,
                  input$Freitext224, input$Freitext226, input$Freitext228, input$Freitext230,
                  input$Freitext232,input$Freitext234,input$Freitext236),
            
            theta = c("<b>85</b>", "<b>86</b>", "<b>87</b>", "<b>88</b>", "<b>89</b>",
                      "<b>90</b>", "<b>91</b>", "<b>92</b>", "<b>93</b>", "<b>94</b>",
                      "<b>95</b>", "<b>96</b>", "<b>97</b>", "<b>98</b>", "<b>99</b>",
                      "<b>100</b>", "<b>101</b>", "<b>102</b>", "<b>103</b>", "<b>104</b>",
                      "<b>105</b>", "<b>106</b>", "<b>107</b>", "<b>108</b>", "<b>109</b>",
                      "<b>110</b>","<b>111</b>","<b>112</b>","<b>113</b>","<b>114</b>",
                      "<b>115</b>","<b>116</b>","<b>117</b>","<b>118</b>","<b>119</b>"),
            
            fillcolor = "rgba(194, 242, 255, 0.5)")|>
      layout(title = list(
        text = "Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
  })
  
  output$Mikro_Fremd_Plot <- renderUI({
    plotlyOutput("Mikro_Fremd_Plot")
  })
  
  # Generate plot
  output$Mikro_Fremd_Plot <- renderPlotly({
    Mikro_FremdPlot_generator()
  })
  
  Mikro_overlayPlot_generator <- reactive({
    plot_ly(mode = 'markers+lines',
            type = 'scatterpolar',
            fill = 'toself',
            line = list(color = 'rgb(0, 0, 0)'),
            marker = list(symbol = 100,
                          size = 6,
                          color = 'rgb(0, 0, 0)',
                          line = list(color = 'rgb(0, 0, 0)',
                                      width = 1)))|>
      add_trace(
        r = c(input$Freitext167, input$Freitext169, input$Freitext171, input$Freitext173,
              input$Freitext175, input$Freitext177, input$Freitext179, input$Freitext181,
              input$Freitext183, input$Freitext185, input$Freitext187, input$Freitext189,
              input$Freitext191, input$Freitext193, input$Freitext195, input$Freitext197,
              input$Freitext199, input$Freitext201, input$Freitext203, input$Freitext205,
              input$Freitext207, input$Freitext209, input$Freitext211, input$Freitext213,
              input$Freitext215, input$Freitext217, input$Freitext219, input$Freitext221,
              input$Freitext223, input$Freitext225, input$Freitext227, input$Freitext229,
              input$Freitext231, input$Freitext233, input$Freitext235),
        
        theta = c("<b>85</b>", "<b>86</b>", "<b>87</b>", "<b>88</b>", "<b>89</b>",
                  "<b>90</b>", "<b>91</b>", "<b>92</b>", "<b>93</b>", "<b>94</b>",
                  "<b>95</b>", "<b>96</b>", "<b>97</b>", "<b>98</b>", "<b>99</b>",
                  "<b>100</b>", "<b>101</b>", "<b>102</b>", "<b>103</b>", "<b>104</b>",
                  "<b>105</b>", "<b>106</b>", "<b>107</b>", "<b>108</b>", "<b>109</b>",
                  "<b>110</b>","<b>111</b>","<b>112</b>","<b>113</b>","<b>114</b>",
                  "<b>115</b>","<b>116</b>","<b>117</b>","<b>118</b>","<b>119</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      add_trace(
        r = c(input$Freitext168, input$Freitext170, input$Freitext172, input$Freitext174,
              input$Freitext176, input$Freitext178, input$Freitext180, input$Freitext182,
              input$Freitext184, input$Freitext186, input$Freitext188, input$Freitext190,
              input$Freitext192, input$Freitext194, input$Freitext196, input$Freitext198,
              input$Freitext200, input$Freitext202, input$Freitext204, input$Freitext206,
              input$Freitext208, input$Freitext210, input$Freitext212, input$Freitext214,
              input$Freitext216, input$Freitext218, input$Freitext220, input$Freitext222,
              input$Freitext224, input$Freitext226, input$Freitext228, input$Freitext230,
              input$Freitext232,input$Freitext234,input$Freitext236),
        
        theta = c("<b>85</b>", "<b>86</b>", "<b>87</b>", "<b>88</b>", "<b>89</b>",
                  "<b>90</b>", "<b>91</b>", "<b>92</b>", "<b>93</b>", "<b>94</b>",
                  "<b>95</b>", "<b>96</b>", "<b>97</b>", "<b>98</b>", "<b>99</b>",
                  "<b>100</b>", "<b>101</b>", "<b>102</b>", "<b>103</b>", "<b>104</b>",
                  "<b>105</b>", "<b>106</b>", "<b>107</b>", "<b>108</b>", "<b>109</b>",
                  "<b>110</b>","<b>111</b>","<b>112</b>","<b>113</b>","<b>114</b>",
                  "<b>115</b>","<b>116</b>","<b>117</b>","<b>118</b>","<b>119</b>"),
        
        fillcolor = "rgba(231, 184, 0, 0.5)") |>
      
      layout(title = list(
        text = "Selbstbewertung & Fremdbewertung",
        y = 0.99  # Adjust the value to move the title higher
      ),
      polar=list(radialaxis =
                   list(visible = T,range = c(0,5))),
      angularaxis = list(
        rotation = 90,  # Adjust the rotation angle to make the plot circular
        direction = "clockwise"),
      showlegend = FALSE,
      margin = list(l = 30, r = 30, b = 30, t = 30),
      font = list(size = 9, color = 'black'),
      paper_bgcolor = "#B4D9F9",  # Set the paper background color with opacity
      plot_bgcolor = "#B4D9F9",
      width = 500,  # Adjust the width of the plot
      height = 400  # Adjust the height of the plot
      )
    
  })
  
  output$Mikro_overlayPlot <- renderUI({
    plotlyOutput("Mikro_overlayPlot")
  })
  output$Mikro_overlayPlot <- renderPlotly({
    Mikro_overlayPlot_generator()
  })
  
  
  #Create a temporary RMarkdown file
  temp_report_rmd <- tempfile(fileext = ".rmd")
  
  # Define the output PDF file
  output_pdf <- tempfile(fileext = ".pdf")
  
  
  # Define the output PDF file
  output_pdf <- tempfile(fileext = ".pdf")
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      return("Peer_Review_Report.pdf")
    },
    
    content = function(file) {
      
      
      # Retrieve user inputs
      name <- input$name
      Internetadresse <- input$Internetadresse
      Trag <- input$Trag
      Zug <- input$Zug
      Ansprechpartner <- input$Ansprechpartner 
      Email <- input$Email
      Telefonnummer <- input$Telefonnummer
      Berufsgruppe <- input$Berufsgruppe
      Versorgung <- input$Versorgung
      sonstiges_versorgung <- input$sonstiges_versorgung
      Laborbereiche <- input$Laborbereiche
      sonstiges_laborbereiche <- input$sonstiges_laborbereiche
      Leistungsspektrum <- input$Leistungsspektrum
      sonstiges_leistungsspektrum <- input$sonstiges_leistungsspektrum
      
      Anzahl_total <- input$Anzahl_total
      Anzahl_mit <- input$Anzahl_mit
      davon_Fachaerzte <- input$davon_Fachaerzte
      davon_weiterbildung <- input$davon_weiterbildung
      Anzahl_Planstellen <- input$Anzahl_Planstellen
      davon_unbesetzt <- input$davon_unbesetzt
      Anzahl_tech <- input$Anzahl_tech
      Anzahl_TPlanstellen <- input$Anzahl_TPlanstellen
      davon_Tunbesetzt <- input$davon_Tunbesetzt
      Anzahl_natur <- input$Anzahl_natur
      Anzahl_NPlanstellen <- input$Anzahl_NPlanstellen
      davon_Nunbesetzt <- input$davon_Nunbesetzt
      Anzahl_IT <- input$Anzahl_IT
      Anzahl_IPlanstellen <- input$Anzahl_IPlanstellen
      davon_Iunbesetzt <- input$davon_Iunbesetzt
      Beschreibung <- input$Beschreibung
      WeitereInfo <- input$WeitereInfo
      
      AnbieterInfo <- input$AnbieterInfo
      AnbieterOrder <- input$AnbieterOrder
      AnbieterMiddleware <- input$AnbieterMiddleware
      WeitereIT <- input$WeitereIT
      Angaben <- input$Angaben
      laufendenJahres <- input$laufendenJahres
      Vorjahres <- input$Vorjahres
      Kompetenzschwerpunkte <- input$Kompetenzschwerpunkte
      
      Freitext1 <- input$Freitext1
      Selbstbewertung_text1 <- input$Selbstbewertung_text1
      Freitext2 <- input$Freitext2
      Fremdbewertung_text2 <- input$Fremdbewertung_text2
      
      Freitext3 <- input$Freitext3
      Selbstbewertung_text3 <- input$Selbstbewertung_text3
      Freitext4 <- input$Freitext4
      Fremdbewertung_text4 <- input$Fremdbewertung_text4

      Freitext5 <- input$Freitext5
      Selbstbewertung_text5 <- input$Selbstbewertung_text5
      Freitext6 <- input$Freitext6
      Fremdbewertung_text6 <- input$Fremdbewertung_text6
      
      Freitext7 <- input$Freitext7
      Selbstbewertung_text7 <- input$Selbstbewertung_text7
      Freitext8 <- input$Freitext8
      Fremdbewertung_text8 <- input$Fremdbewertung_text8
      
      Freitext9 <- input$Freitext9
      Selbstbewertung_text9 <- input$Selbstbewertung_text9
      Freitext10 <- input$Freitext10
      Fremdbewertung_text10 <- input$Fremdbewertung_text10
      
      Freitext11 <- input$Freitext11
      Selbstbewertung_text11 <- input$Selbstbewertung_text11
      Freitext12 <- input$Freitext12
      Fremdbewertung_text12 <- input$Fremdbewertung_text12
      
      Freitext13 <- input$Freitext13
      Selbstbewertung_text13 <- input$Selbstbewertung_text13
      Freitext14 <- input$Freitext14
      Fremdbewertung_text14 <- input$Fremdbewertung_text14
      
      Freitext15 <- input$Freitext15
      Selbstbewertung_text15 <- input$Selbstbewertung_text15
      Freitext16 <- input$Freitext16
      Fremdbewertung_text16 <- input$Fremdbewertung_text16
      
      Freitext17 <- input$Freitext17
      Selbstbewertung_text17 <- input$Selbstbewertung_text17
      Freitext18 <- input$Freitext18
      Fremdbewertung_text18 <- input$Fremdbewertung_text18
      
      Freitext19 <- input$Freitext19
      Selbstbewertung_text19 <- input$Selbstbewertung_text19
      Freitext20 <- input$Freitext20
      Fremdbewertung_text20 <- input$Fremdbewertung_text20
      
      Freitext21 <- input$Freitext21
      Selbstbewertung_text21 <- input$Selbstbewertung_text21
      Freitext22 <- input$Freitext22
      Fremdbewertung_text22 <- input$Fremdbewertung_text22
      
      Freitext23 <- input$Freitext23
      Selbstbewertung_text23 <- input$Selbstbewertung_text23
      Freitext24 <- input$Freitext24
      Fremdbewertung_text24 <- input$Fremdbewertung_text24
      
      Freitext25 <- input$Freitext25
      Selbstbewertung_text25 <- input$Selbstbewertung_text25
      Freitext26 <- input$Freitext26
      Fremdbewertung_text26 <- input$Fremdbewertung_text26
      
      Freitext27 <- input$Freitext27
      Selbstbewertung_text27 <- input$Selbstbewertung_text27
      Freitext28 <- input$Freitext28
      Fremdbewertung_text28 <- input$Fremdbewertung_text28
      
      Freitext29 <- input$Freitext29
      Selbstbewertung_text29 <- input$Selbstbewertung_text29
      Freitext30 <- input$Freitext30
      Fremdbewertung_text30 <- input$Fremdbewertung_text30
      
      Freitext31 <- input$Freitext31
      Selbstbewertung_text31 <- input$Selbstbewertung_text31
      Freitext32 <- input$Freitext32
      Fremdbewertung_text32 <- input$Fremdbewertung_text32
      
      Freitext33 <- input$Freitext33
      Selbstbewertung_text33 <- input$Selbstbewertung_text33
      Freitext34 <- input$Freitext34
      Fremdbewertung_text34 <- input$Fremdbewertung_text34
      
      Freitext35 <- input$Freitext35
      Selbstbewertung_text35 <- input$Selbstbewertung_text35
      Freitext36 <- input$Freitext36
      Fremdbewertung_text36 <- input$Fremdbewertung_text36
      
      Freitext37 <- input$Freitext37
      Selbstbewertung_text37 <- input$Selbstbewertung_text37
      Freitext38 <- input$Freitext38
      Fremdbewertung_text38 <- input$Fremdbewertung_text38
      
      Freitext39 <- input$Freitext39
      Selbstbewertung_text39 <- input$Selbstbewertung_text39
      Freitext40 <- input$Freitext40
      Fremdbewertung_text40 <- input$Fremdbewertung_text40
      
      Freitext41 <- input$Freitext41
      Selbstbewertung_text41 <- input$Selbstbewertung_text41
      Freitext42 <- input$Freitext42
      Fremdbewertung_text42 <- input$Fremdbewertung_text42
      
      Freitext43 <- input$Freitext43
      Selbstbewertung_text43 <- input$Selbstbewertung_text43
      Freitext44 <- input$Freitext44
      Fremdbewertung_text44 <- input$Fremdbewertung_text44
      
      Freitext45 <- input$Freitext45
      Selbstbewertung_text45 <- input$Selbstbewertung_text45
      Freitext46 <- input$Freitext46
      Fremdbewertung_text46 <- input$Fremdbewertung_text46
      
      Freitext47 <- input$Freitext47
      Selbstbewertung_text47 <- input$Selbstbewertung_text47
      Freitext48 <- input$Freitext48
      Fremdbewertung_text48 <- input$Fremdbewertung_text48
      
      Freitext49 <- input$Freitext49
      Selbstbewertung_text49 <- input$Selbstbewertung_text49
      Freitext50 <- input$Freitext50
      Fremdbewertung_text50 <- input$Fremdbewertung_text50
      
      Freitext51 <- input$Freitext51
      Selbstbewertung_text51 <- input$Selbstbewertung_text51
      Freitext52 <- input$Freitext52
      Fremdbewertung_text52 <- input$Fremdbewertung_text52
      
      Freitext53 <- input$Freitext53
      Selbstbewertung_text53 <- input$Selbstbewertung_text53
      Freitext54 <- input$Freitext54
      Fremdbewertung_text54 <- input$Fremdbewertung_text54
      
      Freitext55 <- input$Freitext55
      Selbstbewertung_text55 <- input$Selbstbewertung_text55
      Freitext56 <- input$Freitext56
      Fremdbewertung_text56 <- input$Fremdbewertung_text56
      
      Freitext57 <- input$Freitext57
      Selbstbewertung_text57 <- input$Selbstbewertung_text57
      Freitext58 <- input$Freitext58
      Fremdbewertung_text58 <- input$Fremdbewertung_text58
      
      Freitext59 <- input$Freitext59
      Selbstbewertung_text59 <- input$Selbstbewertung_text59
      Freitext60 <- input$Freitext60
      Fremdbewertung_text60 <- input$Fremdbewertung_text60
      
      Freitext61 <- input$Freitext61
      Selbstbewertung_text61 <- input$Selbstbewertung_text61
      Freitext62 <- input$Freitext62
      Fremdbewertung_text62 <- input$Fremdbewertung_text62
      
      Freitext63 <- input$Freitext63
      Selbstbewertung_text63 <- input$Selbstbewertung_text63
      Freitext64 <- input$Freitext64
      Fremdbewertung_text64 <- input$Fremdbewertung_text64
      
      Freitext65 <- input$Freitext65
      Selbstbewertung_text65 <- input$Selbstbewertung_text65
      Freitext66 <- input$Freitext66
      Fremdbewertung_text66 <- input$Fremdbewertung_text66
      
      Freitext67 <- input$Freitext67
      Selbstbewertung_text67 <- input$Selbstbewertung_text67
      Freitext68 <- input$Freitext68
      Fremdbewertung_text68 <- input$Fremdbewertung_text68
      
      Freitext69 <- input$Freitext69
      Selbstbewertung_text69 <- input$Selbstbewertung_text69
      Freitext70 <- input$Freitext70
      Fremdbewertung_text70 <- input$Fremdbewertung_text70
      
      Freitext71 <- input$Freitext71
      Selbstbewertung_text71 <- input$Selbstbewertung_text71
      Freitext72 <- input$Freitext72
      Fremdbewertung_text72 <- input$Fremdbewertung_text72
      
      Freitext73 <- input$Freitext73
      Selbstbewertung_text73 <- input$Selbstbewertung_text73
      Freitext74 <- input$Freitext74
      Fremdbewertung_text74 <- input$Fremdbewertung_text74
      
      Freitext75 <- input$Freitext75
      Selbstbewertung_text75 <- input$Selbstbewertung_text75
      Freitext76 <- input$Freitext76
      Fremdbewertung_text76 <- input$Fremdbewertung_text76
      
      Freitext77 <- input$Freitext77
      Selbstbewertung_text77 <- input$Selbstbewertung_text77
      Freitext78 <- input$Freitext78
      Fremdbewertung_text78 <- input$Fremdbewertung_text78
      
      Freitext79 <- input$Freitext79
      Selbstbewertung_text79 <- input$Selbstbewertung_text79
      Freitext80 <- input$Freitext80
      Fremdbewertung_text80 <- input$Fremdbewertung_text80
      
      Freitext81 <- input$Freitext81
      Selbstbewertung_text81 <- input$Selbstbewertung_text81
      Freitext82 <- input$Freitext82
      Fremdbewertung_text82 <- input$Fremdbewertung_text82
      
      Freitext83 <- input$Freitext83
      Selbstbewertung_text83 <- input$Selbstbewertung_text83
      Freitext84 <- input$Freitext84
      Fremdbewertung_text84 <- input$Fremdbewertung_text84
      
      Freitext85 <- input$Freitext85
      Selbstbewertung_text85 <- input$Selbstbewertung_text85
      Freitext86 <- input$Freitext86
      Fremdbewertung_text86 <- input$Fremdbewertung_text86
      
      Freitext87 <- input$Freitext87
      Selbstbewertung_text87 <- input$Selbstbewertung_text87
      Freitext88 <- input$Freitext88
      Fremdbewertung_text88 <- input$Fremdbewertung_text88
      
      Freitext89 <- input$Freitext89
      Selbstbewertung_text89 <- input$Selbstbewertung_text89
      Freitext90 <- input$Freitext90
      Fremdbewertung_text90 <- input$Fremdbewertung_text90
      
      Freitext91 <- input$Freitext91
      Selbstbewertung_text91 <- input$Selbstbewertung_text91
      Freitext92 <- input$Freitext92
      Fremdbewertung_text92 <- input$Fremdbewertung_text92
      
      Freitext93 <- input$Freitext93
      Selbstbewertung_text93 <- input$Selbstbewertung_text93
      Freitext94 <- input$Freitext94
      Fremdbewertung_text94 <- input$Fremdbewertung_text94
      
      Freitext95 <- input$Freitext95
      Selbstbewertung_text95 <- input$Selbstbewertung_text95
      Freitext96 <- input$Freitext96
      Fremdbewertung_text96 <- input$Fremdbewertung_text96
      
      Freitext97 <- input$Freitext97
      Selbstbewertung_text97 <- input$Selbstbewertung_text97
      Freitext98 <- input$Freitext98
      Fremdbewertung_text98 <- input$Fremdbewertung_text98
      
      Freitext99 <- input$Freitext99
      Selbstbewertung_text99 <- input$Selbstbewertung_text99
      Freitext100 <- input$Freitext100
      Fremdbewertung_text100 <- input$Fremdbewertung_text100
      
      Freitext101 <- input$Freitext101
      Selbstbewertung_text101 <- input$Selbstbewertung_text101
      Freitext102 <- input$Freitext102
      Fremdbewertung_text102 <- input$Fremdbewertung_text102
      
      Freitext103 <- input$Freitext103
      Selbstbewertung_text103 <- input$Selbstbewertung_text103
      Freitext104 <- input$Freitext104
      Fremdbewertung_text104 <- input$Fremdbewertung_text104
      
      Freitext105 <- input$Freitext105
      Selbstbewertung_text105 <- input$Selbstbewertung_text105
      Freitext106 <- input$Freitext106
      Fremdbewertung_text106 <- input$Fremdbewertung_text106
      
      Freitext107 <- input$Freitext107
      Selbstbewertung_text107 <- input$Selbstbewertung_text107
      Freitext108 <- input$Freitext108
      Fremdbewertung_text108 <- input$Fremdbewertung_text108
      
      Freitext109 <- input$Freitext109
      Selbstbewertung_text109 <- input$Selbstbewertung_text109
      Freitext110 <- input$Freitext110
      Fremdbewertung_text110 <- input$Fremdbewertung_text110
      
      Freitext111 <- input$Freitext111
      Selbstbewertung_text111 <- input$Selbstbewertung_text111
      Freitext112 <- input$Freitext112
      Fremdbewertung_text112 <- input$Fremdbewertung_text112
      
      Freitext113 <- input$Freitext113
      Selbstbewertung_text113 <- input$Selbstbewertung_text113
      Freitext114 <- input$Freitext114
      Fremdbewertung_text114 <- input$Fremdbewertung_text114
      
      Freitext115 <- input$Freitext115
      Selbstbewertung_text115 <- input$Selbstbewertung_text115
      Freitext116 <- input$Freitext116
      Fremdbewertung_text116 <- input$Fremdbewertung_text116
      
      Freitext117 <- input$Freitext117
      Selbstbewertung_text117 <- input$Selbstbewertung_text117
      Freitext118 <- input$Freitext118
      Fremdbewertung_text118 <- input$Fremdbewertung_text118
      
      Freitext119 <- input$Freitext119
      Selbstbewertung_text119 <- input$Selbstbewertung_text119
      Freitext120 <- input$Freitext120
      Fremdbewertung_text120 <- input$Fremdbewertung_text120
      
      Freitext121 <- input$Freitext121
      Selbstbewertung_text121 <- input$Selbstbewertung_text121
      Freitext122 <- input$Freitext122
      Fremdbewertung_text122 <- input$Fremdbewertung_text122
      
      Freitext123 <- input$Freitext123
      Selbstbewertung_text123 <- input$Selbstbewertung_text123
      Freitext124 <- input$Freitext124
      Fremdbewertung_text124 <- input$Fremdbewertung_text124
      
      Freitext125 <- input$Freitext125
      Selbstbewertung_text125 <- input$Selbstbewertung_text125
      Freitext126 <- input$Freitext126
      Fremdbewertung_text126 <- input$Fremdbewertung_text126
      
      Freitext127 <- input$Freitext127
      Selbstbewertung_text127 <- input$Selbstbewertung_text127
      Freitext128 <- input$Freitext128
      Fremdbewertung_text128 <- input$Fremdbewertung_text128
      
      Freitext129 <- input$Freitext129
      Selbstbewertung_text129 <- input$Selbstbewertung_text129
      Freitext130 <- input$Freitext130
      Fremdbewertung_text130 <- input$Fremdbewertung_text130
      
      Freitext131 <- input$Freitext131
      Selbstbewertung_text131 <- input$Selbstbewertung_text131
      Freitext132 <- input$Freitext132
      Fremdbewertung_text132 <- input$Fremdbewertung_text132
      
      Freitext133 <- input$Freitext133
      Selbstbewertung_text133 <- input$Selbstbewertung_text133
      Freitext134 <- input$Freitext134
      Fremdbewertung_text134 <- input$Fremdbewertung_text134
      
      Freitext135 <- input$Freitext135
      Selbstbewertung_text135 <- input$Selbstbewertung_text135
      Freitext136 <- input$Freitext136
      Fremdbewertung_text136 <- input$Fremdbewertung_text136
      
      Freitext137 <- input$Freitext137
      Selbstbewertung_text137 <- input$Selbstbewertung_text137
      Freitext138 <- input$Freitext138
      Fremdbewertung_text138 <- input$Fremdbewertung_text138
      
      Freitext139 <- input$Freitext139
      Selbstbewertung_text139 <- input$Selbstbewertung_text139
      Freitext140 <- input$Freitext140
      Fremdbewertung_text140 <- input$Fremdbewertung_text140
      
      Freitext141 <- input$Freitext141
      Selbstbewertung_text141 <- input$Selbstbewertung_text141
      Freitext142 <- input$Freitext142
      Fremdbewertung_text142 <- input$Fremdbewertung_text142
      
      Freitext143 <- input$Freitext143
      Selbstbewertung_text143 <- input$Selbstbewertung_text143
      Freitext144 <- input$Freitext144
      Fremdbewertung_text144 <- input$Fremdbewertung_text144
      
      Freitext145 <- input$Freitext145
      Selbstbewertung_text145 <- input$Selbstbewertung_text145
      Freitext146 <- input$Freitext146
      Fremdbewertung_text146 <- input$Fremdbewertung_text146
      
      Freitext147 <- input$Freitext147
      Selbstbewertung_text147 <- input$Selbstbewertung_text147
      Freitext148 <- input$Freitext148
      Fremdbewertung_text148 <- input$Fremdbewertung_text148
      
      Freitext149 <- input$Freitext149
      Selbstbewertung_text149 <- input$Selbstbewertung_text149
      Freitext150 <- input$Freitext150
      Fremdbewertung_text150 <- input$Fremdbewertung_text150
      
      Freitext151 <- input$Freitext151
      Selbstbewertung_text151 <- input$Selbstbewertung_text151
      Freitext152 <- input$Freitext152
      Fremdbewertung_text152 <- input$Fremdbewertung_text152
      
      Freitext153 <- input$Freitext153
      Selbstbewertung_text153 <- input$Selbstbewertung_text153
      Freitext154 <- input$Freitext154
      Fremdbewertung_text154 <- input$Fremdbewertung_text154
      
      Freitext155 <- input$Freitext155
      Selbstbewertung_text155 <- input$Selbstbewertung_text155
      Freitext156 <- input$Freitext156
      Fremdbewertung_text156 <- input$Fremdbewertung_text156
      
      Freitext157 <- input$Freitext157
      Selbstbewertung_text157 <- input$Selbstbewertung_text157
      Freitext158 <- input$Freitext158
      Fremdbewertung_text158 <- input$Fremdbewertung_text158
      
      Freitext159 <- input$Freitext159
      Selbstbewertung_text159 <- input$Selbstbewertung_text159
      Freitext160 <- input$Freitext160
      Fremdbewertung_text160 <- input$Fremdbewertung_text160
      
      Freitext161 <- input$Freitext161
      Selbstbewertung_text161 <- input$Selbstbewertung_text161
      Freitext162 <- input$Freitext162
      Fremdbewertung_text162 <- input$Fremdbewertung_text162
      
      # Create a temporary directory to store intermediate files
      temp_dir <- tempdir()
      
      # Define temporary paths for RMarkdown file and PDF output
      temp_report_rmd <- file.path(temp_dir, "report.Rmd")
      temp_report_pdf <- file.path(temp_dir, "report.pdf")
      
      # Copy the RMarkdown template file
      file.copy("download_content.Rmd", temp_report_rmd, overwrite = TRUE)
      
      # Render the R Markdown file
      rmarkdown::render(
        input = temp_report_rmd,
        output_format = pdf_document(),
        params = list(
          name = name,
          Internetadresse = Internetadresse,
          Trag = Trag,
          Zug = Zug,
          Ansprechpartner = Ansprechpartner,
          Email = Email,
          Telefonnummer = Telefonnummer,
          Berufsgruppe = Berufsgruppe,
          Versorgung = Versorgung,
          sonstiges_versorgung = sonstiges_versorgung,
          Laborbereiche = Laborbereiche,
          sonstiges_laborbereiche = sonstiges_laborbereiche,
          Leistungsspektrum = Leistungsspektrum,
          sonstiges_leistungsspektrum = sonstiges_leistungsspektrum,
          Anzahl_total = Anzahl_total,
          Anzahl_mit = Anzahl_mit,
          davon_Fachaerzte = davon_Fachaerzte,
          davon_weiterbildung = davon_weiterbildung,
          Anzahl_Planstellen = Anzahl_Planstellen,
          davon_unbesetzt = davon_unbesetzt,
          Anzahl_tech = Anzahl_tech,
          Anzahl_TPlanstellen = Anzahl_TPlanstellen,
          davon_Tunbesetzt = davon_Tunbesetzt,
          Anzahl_natur = Anzahl_natur,
          Anzahl_NPlanstellen = Anzahl_NPlanstellen,
          davon_Nunbesetzt = davon_Nunbesetzt,
          Anzahl_IT = Anzahl_IT,
          Anzahl_IPlanstellen = Anzahl_IPlanstellen,
          davon_Iunbesetzt = davon_Iunbesetzt,
          Beschreibung = Beschreibung,
          WeitereInfo = WeitereInfo,
          
          AnbieterInfo = AnbieterInfo,
          AnbieterOrder = AnbieterOrder,
          AnbieterMiddleware = AnbieterMiddleware,
          WeitereIT = WeitereIT,
          Angaben = Angaben,
          laufendenJahres = laufendenJahres,
          Vorjahres = Vorjahres,
          Kompetenzschwerpunkte = Kompetenzschwerpunkte,
          
          Freitext1 = Freitext1,
          Selbstbewertung_text1 = Selbstbewertung_text1,
          Freitext2 = Freitext2,
          Fremdbewertung_text2 = Fremdbewertung_text2,
          
          Freitext3 = Freitext3,
          Selbstbewertung_text3 = Selbstbewertung_text3,
          Freitext4 = Freitext4,
          Fremdbewertung_text4 = Fremdbewertung_text4,

          Freitext5 = Freitext5,
          Selbstbewertung_text5 = Selbstbewertung_text5,
          Freitext6 = Freitext6,
          Fremdbewertung_text6 = Fremdbewertung_text6,
          
          Freitext7 = Freitext7,
          Selbstbewertung_text7 = Selbstbewertung_text7,
          Freitext8 = Freitext8,
          Fremdbewertung_text8 = Fremdbewertung_text8,
          
          Freitext9 = Freitext9,
          Selbstbewertung_text9 = Selbstbewertung_text9,
          Freitext10 = Freitext10,
          Fremdbewertung_text10 = Fremdbewertung_text10,
          
          Freitext11 = Freitext11,
          Selbstbewertung_text11 = Selbstbewertung_text11,
          Freitext12 = Freitext12,
          Fremdbewertung_text12 = Fremdbewertung_text12,
          
          Freitext13 = Freitext13,
          Selbstbewertung_text13 = Selbstbewertung_text13,
          Freitext14 = Freitext14,
          Fremdbewertung_text14 = Fremdbewertung_text14,
          
          Freitext15 = Freitext15,
          Selbstbewertung_text15 = Selbstbewertung_text15,
          Freitext16 = Freitext16,
          Fremdbewertung_text16 = Fremdbewertung_text16,
          
          p1 = Selbstbewertung.Plot1(),
          p2 = Fremdbewertung.Plot1(),
          p3 = Selbstbewertung_Fremdbewertung.Plot1(),
          
          Freitext17 = Freitext17,
          Selbstbewertung_text17 = Selbstbewertung_text17,
          Freitext18 = Freitext18,
          Fremdbewertung_text18 = Fremdbewertung_text18,
          
          Freitext19 = Freitext19,
          Selbstbewertung_text19 = Selbstbewertung_text19,
          Freitext20 = Freitext20,
          Fremdbewertung_text20 = Fremdbewertung_text20,
          
          Freitext21 = Freitext21,
          Selbstbewertung_text21 = Selbstbewertung_text21,
          Freitext22 = Freitext22,
          Fremdbewertung_text22 = Fremdbewertung_text22,
          
          Freitext23 = Freitext23,
          Selbstbewertung_text23 = Selbstbewertung_text23,
          Freitext24 = Freitext24,
          Fremdbewertung_text24 = Fremdbewertung_text24,
          
          Freitext25 = Freitext25,
          Selbstbewertung_text25 = Selbstbewertung_text25,
          Freitext26 = Freitext26,
          Fremdbewertung_text26 = Fremdbewertung_text26,
          
          Freitext27 = Freitext27,
          Selbstbewertung_text27 = Selbstbewertung_text27,
          Freitext28 = Freitext28,
          Fremdbewertung_text28 = Fremdbewertung_text28,
          
          Freitext29 = Freitext29,
          Selbstbewertung_text29 = Selbstbewertung_text29,
          Freitext30 = Freitext30,
          Fremdbewertung_text30 = Fremdbewertung_text30,
          
          Freitext31 = Freitext31,
          Selbstbewertung_text31 = Selbstbewertung_text31,
          Freitext32 = Freitext32,
          Fremdbewertung_text32 = Fremdbewertung_text32,
          
          Freitext33 = Freitext33,
          Selbstbewertung_text33 = Selbstbewertung_text33,
          Freitext34 = Freitext34,
          Fremdbewertung_text34 = Fremdbewertung_text34,
          
          Freitext35 = Freitext35,
          Selbstbewertung_text35 = Selbstbewertung_text35,
          Freitext36 = Freitext36,
          Fremdbewertung_text36 = Fremdbewertung_text36,
          
          Freitext37 = Freitext37,
          Selbstbewertung_text37 = Selbstbewertung_text37,
          Freitext38 = Freitext38,
          Fremdbewertung_text38 = Fremdbewertung_text38,
          
          Freitext39 = Freitext39,
          Selbstbewertung_text39 = Selbstbewertung_text39,
          Freitext40 = Freitext40,
          Fremdbewertung_text40 = Fremdbewertung_text40,
          
          Freitext41 = Freitext41,
          Selbstbewertung_text41 = Selbstbewertung_text41,
          Freitext42 = Freitext42,
          Fremdbewertung_text42 = Fremdbewertung_text42,
          
          Freitext43 = Freitext43,
          Selbstbewertung_text43 = Selbstbewertung_text43,
          Freitext44 = Freitext44,
          Fremdbewertung_text44 = Fremdbewertung_text44,
          
          Freitext45 = Freitext45,
          Selbstbewertung_text45 = Selbstbewertung_text45,
          Freitext46 = Freitext46,
          Fremdbewertung_text46 = Fremdbewertung_text46,
          
          Freitext47 = Freitext47,
          Selbstbewertung_text47 = Selbstbewertung_text47,
          Freitext48 = Freitext48,
          Fremdbewertung_text48 = Fremdbewertung_text48,
          
          Freitext49 = Freitext49,
          Selbstbewertung_text49 = Selbstbewertung_text49,
          Freitext50 = Freitext50,
          Fremdbewertung_text50 = Fremdbewertung_text50,
          
          Freitext51 = Freitext51,
          Selbstbewertung_text51 = Selbstbewertung_text51,
          Freitext52 = Freitext52,
          Fremdbewertung_text52 = Fremdbewertung_text52,
          
          Freitext53 = Freitext53,
          Selbstbewertung_text53 = Selbstbewertung_text53,
          Freitext54 = Freitext54,
          Fremdbewertung_text54 = Fremdbewertung_text54,
          
          Freitext55 = Freitext55,
          Selbstbewertung_text55 = Selbstbewertung_text55,
          Freitext56 = Freitext56,
          Fremdbewertung_text56 = Fremdbewertung_text56,
          
          Freitext57 = Freitext57,
          Selbstbewertung_text57 = Selbstbewertung_text57,
          Freitext58 = Freitext58,
          Fremdbewertung_text58 = Fremdbewertung_text58,
          
          p4 = Mitarbeitende_Selbst.Plot(),
          p5 = Mitarbeitende_Fremd.Plot(),
          p6 = Mitarbeitende_overlay.Plot(),
          
          Freitext59 = Freitext59,
          Selbstbewertung_text59 = Selbstbewertung_text59,
          Freitext60 = Freitext60,
          Fremdbewertung_text60 = Fremdbewertung_text60,
          
          Freitext61 = Freitext61,
          Selbstbewertung_text61 = Selbstbewertung_text61,
          Freitext62 = Freitext62,
          Fremdbewertung_text62 = Fremdbewertung_text62,
          
          Freitext63 = Freitext63,
          Selbstbewertung_text63 = Selbstbewertung_text63,
          Freitext64 = Freitext64,
          Fremdbewertung_text64 = Fremdbewertung_text64,
          
          Freitext65 = Freitext65,
          Selbstbewertung_text65 = Selbstbewertung_text65,
          Freitext66 = Freitext66,
          Fremdbewertung_text66 = Fremdbewertung_text66,
          
          Freitext67 = Freitext67,
          Selbstbewertung_text67 = Selbstbewertung_text67,
          Freitext68 = Freitext68,
          Fremdbewertung_text68 = Fremdbewertung_text68,
          
          Freitext69 = Freitext69,
          Selbstbewertung_text69 = Selbstbewertung_text69,
          Freitext70 = Freitext70,
          Fremdbewertung_text70 = Fremdbewertung_text70,
          
          Freitext71 = Freitext71,
          Selbstbewertung_text71 = Selbstbewertung_text71,
          Freitext72 = Freitext72,
          Fremdbewertung_text72 = Fremdbewertung_text72,
          
          Freitext73 = Freitext73,
          Selbstbewertung_text73 = Selbstbewertung_text73,
          Freitext74 = Freitext74,
          Fremdbewertung_text74 = Fremdbewertung_text74,
          
          Freitext75 = Freitext75,
          Selbstbewertung_text75 = Selbstbewertung_text75,
          Freitext76 = Freitext76,
          Fremdbewertung_text76 = Fremdbewertung_text76,
          
          Freitext77 = Freitext77,
          Selbstbewertung_text77 = Selbstbewertung_text77,
          Freitext78 = Freitext78,
          Fremdbewertung_text78 = Fremdbewertung_text78,
          
          Freitext79 = Freitext79,
          Selbstbewertung_text79 = Selbstbewertung_text79,
          Freitext80 = Freitext80,
          Fremdbewertung_text80 = Fremdbewertung_text80,
          
          Freitext81 = Freitext81,
          Selbstbewertung_text81 = Selbstbewertung_text81,
          Freitext82 = Freitext82,
          Fremdbewertung_text82 = Fremdbewertung_text82,
          
          Freitext83 = Freitext83,
          Selbstbewertung_text83 = Selbstbewertung_text83,
          Freitext84 = Freitext84,
          Fremdbewertung_text84 = Fremdbewertung_text84,
          
          Freitext85 = Freitext85,
          Selbstbewertung_text85 = Selbstbewertung_text85,
          Freitext86 = Freitext86,
          Fremdbewertung_text86 = Fremdbewertung_text86,
          
          p7 = Patient_Selbst.Plot(),
          p8 = Patient_Fremd.Plot(),
          p9 = Patient_overlay.Plot(),
          
          Freitext87 = Freitext87,
          Selbstbewertung_text87 = Selbstbewertung_text87,
          Freitext88 = Freitext88,
          Fremdbewertung_text88 = Fremdbewertung_text88,
          
          Freitext89 = Freitext89,
          Selbstbewertung_text89 = Selbstbewertung_text89,
          Freitext90 = Freitext90,
          Fremdbewertung_text90 = Fremdbewertung_text90,
          
          Freitext91 = Freitext91,
          Selbstbewertung_text91 = Selbstbewertung_text91,
          Freitext92 = Freitext92,
          Fremdbewertung_text92 = Fremdbewertung_text92,
          
          Freitext93 = Freitext93,
          Selbstbewertung_text93 = Selbstbewertung_text93,
          Freitext94 = Freitext94,
          Fremdbewertung_text94 = Fremdbewertung_text94,
          
          Freitext95 = Freitext95,
          Selbstbewertung_text95 = Selbstbewertung_text95,
          Freitext96 = Freitext96,
          Fremdbewertung_text96 = Fremdbewertung_text96,
          
          Freitext97 = Freitext97,
          Selbstbewertung_text97 = Selbstbewertung_text97,
          Freitext98 = Freitext98,
          Fremdbewertung_text98 = Fremdbewertung_text98,
          
          Freitext99 = Freitext99,
          Selbstbewertung_text99 = Selbstbewertung_text99,
          Freitext100 = Freitext100,
          Fremdbewertung_text100 = Fremdbewertung_text100,
          
          Freitext101 = Freitext101,
          Selbstbewertung_text101 = Selbstbewertung_text101,
          Freitext102 = Freitext102,
          Fremdbewertung_text102 = Fremdbewertung_text102,
          
          Freitext103 = Freitext103,
          Selbstbewertung_text103 = Selbstbewertung_text103,
          Freitext104 = Freitext104,
          Fremdbewertung_text104 = Fremdbewertung_text104,
          
          Freitext105 = Freitext105,
          Selbstbewertung_text105 = Selbstbewertung_text105,
          Freitext106 = Freitext106,
          Fremdbewertung_text106 = Fremdbewertung_text106,
          
          Freitext107 = Freitext107,
          Selbstbewertung_text107 = Selbstbewertung_text107,
          Freitext108 = Freitext108,
          Fremdbewertung_text108 = Fremdbewertung_text108,
          
          Freitext109 = Freitext109,
          Selbstbewertung_text109 = Selbstbewertung_text109,
          Freitext110 = Freitext110,
          Fremdbewertung_text110 = Fremdbewertung_text110,
          
          Freitext111 = Freitext111,
          Selbstbewertung_text111 = Selbstbewertung_text111,
          Freitext112 = Freitext112,
          Fremdbewertung_text112 = Fremdbewertung_text112,
          
          Freitext113 = Freitext113,
          Selbstbewertung_text113 = Selbstbewertung_text113,
          Freitext114 = Freitext114,
          Fremdbewertung_text114 = Fremdbewertung_text114,
          
          p10 = Einsender_Selbst.Plot(),
          p11 = Einsender_Fremd.Plot(),
          p12 = Einsender_Overlay.Plot(),
          
          Freitext115 = Freitext115,
          Selbstbewertung_text115 = Selbstbewertung_text115,
          Freitext116 = Freitext116,
          Fremdbewertung_text116 = Fremdbewertung_text116, 
          
          Freitext117 = Freitext117,
          Selbstbewertung_text117 = Selbstbewertung_text117,
          Freitext118 = Freitext118,
          Fremdbewertung_text118 = Fremdbewertung_text118,
          
          Freitext119 = Freitext119,
          Selbstbewertung_text119 = Selbstbewertung_text119,
          Freitext120 = Freitext120,
          Fremdbewertung_text120 = Fremdbewertung_text120,
          
          Freitext121 = Freitext121,
          Selbstbewertung_text121 = Selbstbewertung_text121,
          Freitext122 = Freitext122,
          Fremdbewertung_text122 = Fremdbewertung_text122,
          
          Freitext123 = Freitext123,
          Selbstbewertung_text123 = Selbstbewertung_text123,
          Freitext124 = Freitext124,
          Fremdbewertung_text124 = Fremdbewertung_text124,
          
          Freitext125 = Freitext125,
          Selbstbewertung_text125 = Selbstbewertung_text125,
          Freitext126 = Freitext126,
          Fremdbewertung_text126 = Fremdbewertung_text126,
          
          Freitext127 = Freitext127,
          Selbstbewertung_text127 = Selbstbewertung_text127,
          Freitext128 = Freitext128,
          Fremdbewertung_text128 = Fremdbewertung_text128,
          
          Freitext129 = Freitext129,
          Selbstbewertung_text129 = Selbstbewertung_text129,
          Freitext130 = Freitext130,
          Fremdbewertung_text130 = Fremdbewertung_text130,
          
          Freitext131 = Freitext131,
          Selbstbewertung_text131 = Selbstbewertung_text131,
          Freitext132 = Freitext132,
          Fremdbewertung_text132 = Fremdbewertung_text132,
          
          Freitext133 = Freitext133,
          Selbstbewertung_text133 = Selbstbewertung_text133,
          Freitext134 = Freitext134,
          Fremdbewertung_text134 = Fremdbewertung_text134,
          
          Freitext135 = Freitext135,
          Selbstbewertung_text135 = Selbstbewertung_text135,
          Freitext136 = Freitext136,
          Fremdbewertung_text136 = Fremdbewertung_text136,
          
          Freitext137 = Freitext137,
          Selbstbewertung_text137 = Selbstbewertung_text137,
          Freitext138 = Freitext138,
          Fremdbewertung_text138 = Fremdbewertung_text138,
          
          Freitext139 = Freitext139,
          Selbstbewertung_text139 = Selbstbewertung_text139,
          Freitext140 = Freitext140,
          Fremdbewertung_text140 = Fremdbewertung_text140,
          
          Freitext141 = Freitext141,
          Selbstbewertung_text141 = Selbstbewertung_text141,
          Freitext142 = Freitext142,
          Fremdbewertung_text142 = Fremdbewertung_text142,
          
          Freitext143 = Freitext143,
          Selbstbewertung_text143 = Selbstbewertung_text143,
          Freitext144 = Freitext144,
          Fremdbewertung_text144 = Fremdbewertung_text144,
          
          Freitext145 = Freitext145,
          Selbstbewertung_text145 = Selbstbewertung_text145,
          Freitext146 = Freitext146,
          Fremdbewertung_text146 = Fremdbewertung_text146,
          
          Freitext147 = Freitext147,
          Selbstbewertung_text147 = Selbstbewertung_text147,
          Freitext148 = Freitext148,
          Fremdbewertung_text148 = Fremdbewertung_text148,
          
          Freitext149 = Freitext149,
          Selbstbewertung_text149 = Selbstbewertung_text149,
          Freitext150 = Freitext150,
          Fremdbewertung_text150 = Fremdbewertung_text150,
          
          Freitext151 = Freitext151,
          Selbstbewertung_text151 = Selbstbewertung_text151,
          Freitext152 = Freitext152,
          Fremdbewertung_text152 = Fremdbewertung_text152,
          
          Freitext153 = Freitext153,
          Selbstbewertung_text153 = Selbstbewertung_text153,
          Freitext154 = Freitext154,
          Fremdbewertung_text154 = Fremdbewertung_text154,
          
          Freitext155 = Freitext155,
          Selbstbewertung_text155 = Selbstbewertung_text155,
          Freitext156 = Freitext156,
          Fremdbewertung_text156 = Fremdbewertung_text156,
          
          Freitext157 = Freitext157,
          Selbstbewertung_text157 = Selbstbewertung_text157,
          Freitext158 = Freitext158,
          Fremdbewertung_text158 = Fremdbewertung_text158,
          
          Freitext159 = Freitext159,
          Selbstbewertung_text159 = Selbstbewertung_text159,
          Freitext160 = Freitext160,
          Fremdbewertung_text160 = Fremdbewertung_text160,
          
          Freitext161 = Freitext161,
          Selbstbewertung_text161 = Selbstbewertung_text161,
          Freitext162 = Freitext162,
          Fremdbewertung_text162 = Fremdbewertung_text162,
          
          p13 = Qualit_Selbst.Plot(),
          p14 = Qualit_Fremd.Plot(),
          p15 = Qualit_Overlay.Plot()
          
          
        ),
        output_file = temp_report_pdf,
        intermediates_dir = temp_dir
      )
      
      # Check if a PDF file was uploaded in the "EDV and Kennzahlen" part
      if (!is.null(input$Organigramm)) {
        # Get the uploaded PDF file path
        Organigramm <- input$Organigramm$datapath
        
        # Append the uploaded PDF to the generated PDF
        combined_pdf <- file.path(temp_dir, "combined_report.pdf")
        pdf_combine(c(temp_report_pdf, Organigramm), combined_pdf)
      } else {
        # If no file is uploaded, use the generated PDF as the final PDF
        combined_pdf <- temp_report_pdf
      }
      
      # Copy the combined PDF to the final download location
      file.copy(combined_pdf, file)
    },
    contentType = "application/pdf"
  )
  
}
  

shinyApp(ui, server, enableBookmarking = "server")


