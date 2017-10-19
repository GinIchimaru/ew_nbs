#KA4_url="C:/Users/milos.cipovic/Desktop/Baze podataka/K4 i APR/KA4_podaci.txt"
#RK_url="C:/Users/milos.cipovic/Desktop/Baze podataka/K4 i APR/RK_novi.txt"

read_KA4_RK_data<-function(KA4_url,RK_url,granica_gubitka=0.1,p=0.1,percentile=0.99,triger_za_default=0.2, consequtive_difolt=F){
###########################################################################################################  
  #funkcija sluzi za ucitavanje rk i ka4 obrasca i njihovu manipulaciju i spajanje, ciscenje              #
  #nelogicnih unosa, brisanje redova zbirno u ka4, zadrzavanje samo decembra meseca u rk,                 #
  #kreiranje indikatora difolta na jedan-jedinstveni u sektoru u slucaju duznika koji se nalaze           #
  #u vise banaka....Krajnji produkt je difault indikator sa jos nekim kolonama koje mogu biti             #
  #korisne za dalju analizu.                                                                              #
  #--------------------------------------------------------------------------------------------------------
      #KA4_url-adresa KA4 podataka u txt formatu
      #RK_url-adresa RK podataka u txt formatu
      #granica_gubitka - parametar koji oznacava granicu. Naime u jednom trenutku ce biti 
          #potrebno da se izabere da li obrisati sve observacije koje se na dati datum nalaze
          #u vise banaka. Kao kriterijum smo izabrali da ukoliko se posle brisanja svih
          #observacija koje se ponavljaju kod vise od jedne banke broj difoltiranih duznika smanji
          #za vise od granica_gubitka *100% (uz jos jedan uslov, vidi percentile ispod)onda ih 
          #necemo brisati vec zadrzavati ih u onim bankama u kojima imaju najvecu vrednost potrazivanja
          #i svesti ih na jedinstven status (difolt ili ne) postupkom na osnovu izlozenosti, slicno kao
          #i u samoj banci samo se ovde radi na nivou bankarskog sektora.
      #percentile-Kao drugi kriterijum smo izabrali da ukoliko (posle brisanja svih
          #observacija koje se ponavljaju kod vise od jedne banke za dati datum) maximum izlozenosti
          #posle brisanja bude manji od percentile percentila onda necemo brisati ponavljajuce
          # duznike vec primenjujemo postupak opisan u granica_gubitka definiciji.
      #p-kriterijum difolta za KA4 obrazac. Obzirom da u KA4 obrascu isti klijent moze imati
          #razlicite ocene u zavisnosti od pokrivenosti datog potrazivanja potrebno ga je 
          #svesti na jedinstvenu ocenu unutar banke. Kazemo ukoliko je p*100% ili vise potrazivanja
          #posmatranog klijenta, nezavisno od pokrica, klasifikovano u difolt onda sva potra
          #zivanja datog klijenta spustamo u difolt, nezavisno od pokrivenosti a izlozenost
          #racunamo tako sto saberemo izlozenosti razlicitih pokrivenosti u jednu.
      #triger_za_default-isto sto i p vrednost samo sto nam ovde sluzi na nivou potrazivanja
          #koja se ponavljaju u vise banaka za posmatrani datum. Ukoliko uslov koji se proverava
          #sa granica_gubitka parametrom i percentile parametrom nije zadovoljen onda se ranuje
          #procedura za zadrzavanje ponavljajucih klijenata (klijent koji ima potrazivanja kod 
          #vise banaka u datom datumu) i dodeljivanjem im jedinstvenog statusa difoltirao/nije difoltirao
          #. Dati status se dodeljuje na osnovu toga da li je vise od triger_za_default*100% vrednosti
          #u difoltu ili nije.
      # consequtive_difolt=T ukoliko je neko recimo 2008->2010 difoltirao i to tokom godine 2009
          #da li ga zadrzavamo kao solventnog i tada tako da 'emo ga imati i u tranziciji 2009->2011
          #(komanda consequtive_difolt=F) kao difoltiranog
          #ili ga brisemo i ostavljamo samo tranziciju 2008->2010 (komanda consequtive_difolt=T)-malo je kontraintuitivna komanda
#-----------------------------------------------------------------------------------------------------------
  
  
  
  library(readr)
  library(psych)
  library(data.table,quietly = T)
  library(gtools)
  library(gridExtra)
  library(lubridate)
  
  ###############################################################################
  ###################             Ucitavanje podataka      ######################
  ###############################################################################
  
  #dakle biramo samo kolone sa ovim rednim brojevima jer nam druge nisu interesantne
  RK_podaci_novi <-
    read_delim(RK_url,
      "|",
      escape_double = FALSE,
      col_types = cols_only(
        DATUM = col_date(format = "%d.%m.%Y"),
        "MATICNI_BROJ" = "?",
        "NAZIV_BANKE" = "?",
        "FIZL_PRAVL_IND" = "?",
        "MAT_BR_DUZ" = "?",
        "NAZIV_DUZ" = "?",
        "KLASIFIKACIJA" = "?",
        "BILANS_BRUTO" = "?",
        "BILANS_NETO" = "?"
      ),
      trim_ws = TRUE)
  
  
  gc()# free system memory
  #dakle biramo samo kolone sa ovim rednim brojevima jer nam druge nisu interesantne
  KA4_podaci <-
    read_delim(
      KA4_url,
      "|",
      locale = locale(encoding = "UTF-8"),
      trim_ws = TRUE,
      escape_double = FALSE,
      col_types = cols_only(
        DATUM = "?",
        "MATICNI_BROJ" = "?",
        "NAZIV_BANKE" = "?",
        "VRSTA_LICA"  = "?",
        "SIFRA_LICA" = "?",
        "NAZIV_LICA_LAT"  = "?",
        "NAZIV_POTR"  = "?",
        "OSNOVICA_B_16"  = "?",
        "KLASIFIKACIJA" = "?",
        "ISPR_VRED_BIL_AKT_30" = "?"
      )
    )
  gc()# free system memory
  
  
  ###############################################################################
  #################               Obrada podataka:        #######################
  ###############################################################################
  
  

  #proracunamo ispravku kao razliku bruto i neto
  RK_podaci_novi$ispravka=RK_podaci_novi$BILANS_BRUTO-RK_podaci_novi$BILANS_NETO
  RK_podaci_novi$BILANS_NETO=NULL
  gc()
  
  
  
  ###############################################################################
  #######Izbacujemo redove kojima je osnovica za obracun posebne rezerve 0:######
  ###############################################################################
  
  KA4_podaci <-KA4_podaci[KA4_podaci[,8]>0,]
  RK_podaci_novi<-RK_podaci_novi[RK_podaci_novi$BILANS_BRUTO>0,]
  gc()
  
  
  ################################################################################
  #########Sklanjamo klasifikaciju gde je polje `Klasifikacija` jednako `Zbirno`:#
  ################################################################################
  
  KA4_podaci <-KA4_podaci[KA4_podaci[,9]!="Zbirno",]
  gc()
  
  ################################################################################
  #############################Tretman `NA` vrednosti:############################
  ################################################################################
  KA4_podaci$VRSTA_LICA[is.na(KA4_podaci$VRSTA_LICA)]<-9
  KA4_podaci<-na.omit(KA4_podaci)
  RK_podaci_novi<-na.omit(RK_podaci_novi)
  gc()
  
  ################################################################################
  #####   Imenujemo kolone istim nazivima i poredjamo ih tako da se naposletku   #
  ###########        fajlovi mogu lako spojiti:         ##########################
  ################################################################################
  
  names(KA4_podaci)[1]<-"DATUM"
  names(RK_podaci_novi)[1]<-"DATUM"
  
  names(KA4_podaci)[2]<-"MATICNI_BROJ"
  names(RK_podaci_novi)[2]<-"MATICNI_BROJ"
  
  names(KA4_podaci)[3]<-"NAZIV_BANKE"
  names(RK_podaci_novi)[3]<-"NAZIV_BANKE"
  
  names(KA4_podaci)[4]<-"VRSTA_LICA"
  names(RK_podaci_novi)[4]<-"VRSTA_LICA"
  
  names(KA4_podaci)[5]<-"MAT_BR_DUZNIKA"
  names(RK_podaci_novi)[5]<-"MAT_BR_DUZNIKA"
  
  names(KA4_podaci)[6]<-"NAZIV_DUZNIKA"
  names(RK_podaci_novi)[6]<-"NAZIV_DUZNIKA"
  
  names(KA4_podaci)[8]<-"IZLOZENOST"
  names(RK_podaci_novi)[8]<-"IZLOZENOST"
  
  names(KA4_podaci)[10]<-"ISPRAVKA"
  names(RK_podaci_novi)[9]<-"ISPRAVKA"
  
  names(KA4_podaci)[9]<-"KLASIFIKACIJA"
  names(RK_podaci_novi)[7]<-"KLASIFIKACIJA"
  
  ###############################################################################
  #######Spustamo kategoriju na jednu losiju ukoliko je adekvatno potrazivanje:##
  ###############################################################################
  
  KA4_podaci_adekv<-KA4_podaci[KA4_podaci[,7]=="Pokr. adekvatnim sred. ob",]#izolujem one sa adekvatnim u poseban data.frame
  
  KA4_podaci_bez_adekvatnog<-KA4_podaci[KA4_podaci[,7]!="Pokr. adekvatnim sred. ob",]#izolujem ostatak
  
  #dodatno podelim adekvatna po kategorijama
  KA4_podaci_adekv_A<-KA4_podaci_adekv[KA4_podaci_adekv[,9]=="A",]
  KA4_podaci_adekv_B<-KA4_podaci_adekv[KA4_podaci_adekv[,9]=="B",]
  KA4_podaci_adekv_C<-KA4_podaci_adekv[KA4_podaci_adekv[,9]=="C",]
  KA4_podaci_adekv_D<-KA4_podaci_adekv[KA4_podaci_adekv[,9]=="D",]
  KA4_podaci_adekv_E<-KA4_podaci_adekv[KA4_podaci_adekv[,9]=="E",]
  
  #spustim kategoriju za jednu nize
  KA4_podaci_adekv_A[,9]<-rep("B",dim(KA4_podaci_adekv_A)[1])
  KA4_podaci_adekv_B[,9]<-rep("C",dim(KA4_podaci_adekv_B)[1])
  KA4_podaci_adekv_C[,9]<-rep("D",dim(KA4_podaci_adekv_C)[1])
  KA4_podaci_adekv_D[,9]<-rep("E",dim(KA4_podaci_adekv_D)[1])
  KA4_podaci_adekv_E[,9]<-rep("E",dim(KA4_podaci_adekv_E)[1])
  
  #vratim sve ponovo u jedan data.frame adekvatnih
  KA4_podaci_adekv<-rbind.data.frame(KA4_podaci_adekv_A,KA4_podaci_adekv_B,KA4_podaci_adekv_C,KA4_podaci_adekv_D,KA4_podaci_adekv_E)
  
  #kreiram novu bazu preko stare
  KA4_podaci<-rbind.data.frame(KA4_podaci_adekv,KA4_podaci_bez_adekvatnog)
  
  #pobrisem temporary varijable
  rm(list = c("KA4_podaci_adekv_A","KA4_podaci_adekv_B","KA4_podaci_adekv_C","KA4_podaci_adekv_D","KA4_podaci_adekv_E","KA4_podaci_adekv","KA4_podaci_bez_adekvatnog"))
  gc()
  
  #############################################################################################
  #########################Kreiranje dodatne kolone za difolt. ################################
  #############################################################################################
  #Naime uzecemo neki procenat za kriterijum tako da ukoliko u datoj banci posmatrani klijent #
  #ima vise od n% potrazivanja u difoltu onda ga posmatramo kao da je difoltirao.Pre toga cemo#
  #napraviti novu kolonu u kojoj ce klasifikacije A, B, C biti 0 i klasifikacije D i E biti 1.#
  #Proracun n% ce se potom lakse odraditi na ovoj novoj koloni.Vazno je imati u vidu da se ovo#
  #radi na nivou banke, tj. ovakav proracun da isti klijen u jednoj banci bude u              #
  #difoltu a u drugoj ne se sada ne uzima u obzir, Ovakve slucajeve cemo kasnije dodatno      #
  #razmotriti.                                                                                #
  #############################################################################################
  
  
  #kreiramo novu kolonu koja uzima 1 u slucaju D i E i O u slucaju A, B, C
  default_dummy<-as.numeric(KA4_podaci[,9]=="D"|KA4_podaci[,9]=="E")
  KA4_podaci<-cbind.data.frame(KA4_podaci,default_dummy)
  rm(default_dummy)
  
  KA4_podaciDT<-as.data.table(KA4_podaci)
  
  #dodajem novu kolonu koja pokazuje udeo datog potrazivanja u ukupnim potrazivanjima u banci
  KA4_podaciDT[,agg2:=IZLOZENOST/sum(IZLOZENOST),by=list(DATUM,NAZIV_BANKE,MAT_BR_DUZNIKA)]
  
  #definisem procenat
  
  #da li je vrednost potrazivanja za duznika u redu materijalno znacajna?
  KA4_podaciDT[,tempDefault:=as.numeric(agg2*default_dummy>p)]
  
  #sumiramo po duzniku 
  KA4_final<-KA4_podaciDT[,.(IZLOZENOST=sum(IZLOZENOST),ISPRAVKA=sum(ISPRAVKA),provera=sum(agg2),diff=sum(tempDefault)),by=list(DATUM,MATICNI_BROJ,NAZIV_BANKE,VRSTA_LICA,MAT_BR_DUZNIKA)]
  
  #ukoliko duznik ima i jednu materijalno znacajnu default izlozenost onda je on u defaultu
  #dalje sklanjamo privremenu kolonu diff
  KA4_final<-KA4_final[,default:=as.numeric(diff!=0)][,!"diff"]
  
  #proracun udela ispravke u izlozenosti
  KA4_final<-KA4_final[,udeo_ispravke:=ISPRAVKA/IZLOZENOST]
  RK_podaci_novi<-as.data.table(RK_podaci_novi)
  RK_podaci_novi<-RK_podaci_novi[,udeo_ispravke:=ISPRAVKA/IZLOZENOST]
  
  #sklanjamo nelogične vrednosti ukoliko postoje
  KA4_final=KA4_final[udeo_ispravke <=1 ]
  KA4_final=KA4_final[udeo_ispravke >=0 ]
  
  #RK je mnogo lakši jer je on nivou banke ima samo jednu ocenu
  RK_podaci_novi$default=as.numeric(RK_podaci_novi$KLASIFIKACIJA=="G" | RK_podaci_novi$KLASIFIKACIJA=="D")
  
  
  #############################################################################################
  #######          Razvrstavanje duznika UZIMAJUCI U OBZIR SAMO KRAJ GODINE           #########
  #############################################################################################

  
  rm(KA4_podaci,KA4_podaciDT)
  gc()
  #PREBACIM DATUM U DATE FORMAT
  KA4_final$DATUM=as.Date(KA4_final$DATUM,format ="%d.%m.%Y")
  
  #NAPRAVIM NOVU VARIJABLU KOJA UZIMA SAMO KRAJ GODINE
  
  KA4_date_fixed<-KA4_final[month(DATUM )==12]
  
  RK_podaci_novi$DATUM=as.Date(RK_podaci_novi$DATUM,format ="%d.%m.%Y" )
  RK_date_fixed<-RK_podaci_novi[month(DATUM)==12]
  browser()
  ############################################################################################
  ######                 Potrebno je izracunati tranzicije za svaku godinu dana     ##########
  ############################################################################################
  
  #spajamo ove dve baze u jednu bazu
  #---------------------------------
  #    brisem  nepotrebne kolone
  #    preuredim redosled kolona da bih posle mogao da spojim u jedan df
  
  RK_date_fixed[,VRSTA_LICA:=NULL]
  RK_date_fixed[,NAZIV_DUZNIKA:=NULL]
  RK_date_fixed[,KLASIFIKACIJA:=NULL]
  KA4_date_fixed[,provera:=NULL]
  KA4_date_fixed[,VRSTA_LICA:=NULL]
  KA4_date_fixed=KA4_date_fixed[,c(1,2,3,4,5,6,8,7)]
  
  #zadrzavam RK samo do 31.12.2011, jer od sledece godine pocinje KA4
  RK_date_fixed<-RK_date_fixed[DATUM<"2012-12-31",]
  
  #SPAJAM IH
  
  DATA<-rbind.data.frame(RK_date_fixed,KA4_date_fixed)
  
  #kreiramo vektor datuma
  datumi=unique(DATA[,DATUM])
  
  #dakle pocinjemo od najranijeg datuma
  pocetni_datum=min(DATA[,DATUM])
  
  
  poslednji_datum=max(DATA[,DATUM])
  
  temp_dt=data.frame(NULL)
  
  #setkey(KA4_date_fixed,)
  for( i in datumi ) {
    #kreiram sledeći datum
    j=as.Date.IDate(i)
    year(j)=year(j)+2
    #prvi datum, potrazim sve zdrave
    zdravi<-DATA[DATUM==i & default==0]
    zdravi.sledeci_datum<-DATA[DATUM==j]
    #Potrazi sada ove zdrave na kraju sledece godine:
    #ukoliko je u difoltu dodaj mu u default koloni vrednost 1
    #ukoliko je zdrav dodaj mu u default koloni vrednost 0
    #ukoliko se vise ne nalazi u bazi smatrati da je otplatio dug i da je ostao zdrav,
    # tj dodati mu vrednost o u default koloni
    temp.data.table<-merge(zdravi, zdravi.sledeci_datum,by=c("NAZIV_BANKE","MAT_BR_DUZNIKA"),all.x=TRUE)
    
    #popunjavamo NA vrednosti u koloni DATUM.y i default.y
    temp.data.table[is.na(DATUM.y),c("DATUM.y","default.y"):=.(j,0)]
    
    temp_dt<-rbind.data.frame(temp.data.table,temp_dt)
    if(j==poslednji_datum) {
      rm(temp.data.table,zdravi.sledeci_datum,zdravi,j) 
      gc()
      
      break
    }
  }
  
  default_indicator<-temp_dt

  #pobrisem nepotrebne varijable
  rm(temp_dt,DATA,KA4_date_fixed,RK_date_fixed,i,KA4_final,RK_podaci_novi, pocetni_datum,poslednji_datum)
  
  browser()
  
  ############################################################################################
  #####################               Jos ovo                 ################################
  ############################################################################################
  #trebalo bi  isčistiti i one  observacije koje su recimo postale                           #
  #default u jednoj banci a i dalje su zdrave u drugim ili još gore, pojavljuju se u kasnijim#
  #godinama kod drugih banaka kao novi duznici. Dakle posmatramo duznike koji se nalaze kod  #
  #vise banaka! Ovaj vid intervencije treba primeniti uslovno u buducim iteracijama ukoliko  #                     #
  #do njih dodje. Naime, prvi korak bi bio odstranjivanje dupliranih                         #
  #duznika u bankarskom sektoru, ukoliko ovakav metod dovede do znacajnog pada broja         #
  #default-iranih duznika u populaciji, onda treba primeniti drugaciji pristup. Predlog je da#
  #se sledeci korak uvede ukoliko se broj default-iranih observacija duznik-godina-banka     #
  #prethodno navedenom procedurom prepolovi, takodje treba voditi racuna da se ovim putem ne #
  #obrisu i najvece izlozenosti,tako da neka bude drugi uslov da se *(na primer) 99 percentil#
  #izlozenosti ne sme izgubiti brisanjem ponavljajucih difoltera jer bi se ovom procedurom   #
  #upravo najveci duznici izbrisali jer su oni, prirodno, izlozeni kod vise banaka.          #                                             #
  #                                                                                          #
  #Dakle, sortiracemo podatke po duzniku pa po datumu pa po koloni default (0 ili 1). Sledeci#
  #korak je da proverimo da li je za dati datum presao u default u bilo kojoj banci,ako jeste#
  #onda ukoliko je to znacajan iznos u bankarskom sektoru (20% izlozenosti je u defaultu)    #
  #reci cemo da je duznik u difaltu u svim bankama i pregledati sledece datume za datog      #
  #duznika, ukoliko se javio kao novi duznik u nekoj banci izbrisacemo ga iz baze u toj banci#
  #u suprotnom zadrzavamo zdrave izlozenosti i izbacujemo losu iz razmatranja. Potrebno je   #
  #prebrojati u isto vreme koliko ima datih duznika po godini.                               #
  #------------------------------------------------------------------------------------------#
  #Ukoliko zelimo da nametnemo gore navedeni postupak dovoljno je smanjiti granica_gubitka   #
  #na dovoljno mali broj i procedura ce se svejedno odraditi.                                #
  ############################################################################################
  
  
  broj_default_observacija_pre<-default_indicator[,sum(default.y)]
  percentil_izlozenosti_defaultera<-quantile( default_indicator[default.y==1,IZLOZENOST.x],percentile)
  
  #brojim u koliko banaka je posmatrana observacija zaduzena i brisem svaku koja je zaduzena u vise banaka i snimam ovo u default_indicator_unique.
  default_indicator_count<-default_indicator[,count:=.N,by=.(DATUM.x,MAT_BR_DUZNIKA)]
  default_indicator_unique<-default_indicator_count[count==1][,count:=NULL]
  
  #prebrojavam koliko sada ima defaultera bez ovih koji su bili zaduzeni u vise banaka
  broj_default_observacija_posle<-default_indicator_unique[,sum(default.y)]
  
  maximum_posle<-max(default_indicator_unique[default.y==1,IZLOZENOST.x]);
  
  #ukoliko gornja dva uslova (koji su objasnjeni u opisu) nisu ispunjena onda krecemo sa svodjenjem ponavljajucih duznika na jedinstven indikator
  if(broj_default_observacija_posle/broj_default_observacija_pre>granica_gubitka | maximum_posle<percentil_izlozenosti_defaultera){ 
    #iskoristimo vec postojecu varijablu default_indicator_count OD KOJE UZmemo sve one observacije koje se nalaze u vise banaka u posmatranom datumu i nazovemo tu varijablu default_indicator_repeated
    default_indicator_repeated<-default_indicator_count[count>1,]
    #oni koji imaju razlicite statuse po razlicitim bankama
    default_indicator_non_repeated<-default_indicator_count[count==1,][,count:=NULL]
    
    #sumiramo po koloni koja nam daje status neizmirenja obaveza
    default_indicator_repeated[,temp_indikator_kolona:=sum(default.y),by=.(DATUM.x,MAT_BR_DUZNIKA)]
    #proverimo da li ima isti status u svim bankama(da li je 0 ili je zbir jednak broju observacija datum-banka-duznik)
    #tj da li je difault usvim bankama
    
    default_indicator_repeated[,indikator_kolona:=(temp_indikator_kolona==0 | temp_indikator_kolona==count)]
    #e sada konacno razdvajamo ponavljajuce observacije na dva dela, 1 su oni koji su kod svake banke 
    #u posmatranom datumu bili u istom statusu, njih cuvamo u varijablu same_repeated i onda dalje 
    #zadrzavamo samo redove najvece izlozenosti u bankarskom sektoru od svih tih na posmatrani datum. Ove problematicne koji imaju razlicite statuse stavljamo 
    #u varijablu different_repeated i onda zavisno od udela lose izlozenosti u izlozenosti u citavom 
    #sektoru dodeljujemo im isti status za sve banke, na kraju zadrzavamo ovako dobijene observacije sa 
    #najvecom izlozenoscu a observacije u drugim bankama brisemo:
    
    #isti status:
    same_repeated<-default_indicator_repeated[
      indikator_kolona==TRUE][
        ,`:=`(indikator_kolona=NULL,count=NULL,temp_indikator_kolona=NULL)][
          ,maximum:=max(IZLOZENOST.x),by=.(DATUM.x,MAT_BR_DUZNIKA)][
            IZLOZENOST.x==maximum][,maximum:=NULL]
    #razlicit status
    different_repeated<-default_indicator_repeated[
      indikator_kolona==FALSE][
        ,`:=`(indikator_kolona=NULL,count=NULL,temp_indikator_kolona=NULL)][
          ,udeo_izlozenosti:=IZLOZENOST.x/sum(IZLOZENOST.x),by=.(DATUM.x,MAT_BR_DUZNIKA)][,udeo_izlozenosti_BS:=sum(udeo_izlozenosti),by=.(DATUM.x,MAT_BR_DUZNIKA,default.y)][
            udeo_izlozenosti_BS>triger_za_default][
              ,maximum:=max(IZLOZENOST.x),by=.(DATUM.x,MAT_BR_DUZNIKA)][
                IZLOZENOST.x==maximum][,`:=`(maximum=NULL,udeo_izlozenosti_BS=NULL,udeo_izlozenosti=NULL)]
    
    default_indicator_unique<-rbind.data.frame(default_indicator_non_repeated,same_repeated,different_repeated)
    #brisem pomocne kolone
    rm(same_repeated,different_repeated,
       default_indicator_repeated,default_indicator_non_repeated,
       maximum_posle,broj_default_observacija_posle,default_indicator_count,
       percentil_izlozenosti_defaultera,broj_default_observacija_pre)
  }
  
  temp_dt<-default_indicator_unique
  temp_dt<-temp_dt[order(temp_dt$MAT_BR_DUZNIKA,temp_dt$DATUM.x)]
  
  n.row<-nrow(temp_dt)-1
  temp_dt_reduced=vector(mode = "logical", length = n.row+1)
  temp_dt_reduced[1]=T
  
  #Uzasno sporo, u isto vreme if komanda u R-u totalno je retardirana..., ali ne mogu vise nesto drugo da pisem...elem,
  #Ako je jednom migrirao u difolt, ne moze i sledece godine, naime, imajuci u vidu da su migracije sa korakom od dve godine
  #posmatramo hipoteticki slucaj: 2008 zdrav, 2010 difoltirao-u migraciji 2008->2010 bice migracija u difolt
  #medjutim, takodje ce se desiti i u 2009->2011 jer je u 2009 on jos uvek zdrav. Nakon konsultovanja sa kolegama
  #odlucili smo da brisemo (u duhu primera) migraciju 2009->2011
  
  browser()
  if(consequtive_difolt==T){
  
  for(i in 1:n.row) {
    if (temp_dt[i, 2] == temp_dt[i + 1, 2]) {
      if (temp_dt[i, 14] == 1) {
        temp_dt[i + 1, 14] <- 1
        temp_dt_reduced[i + 1] <- F
      } else {
        temp_dt_reduced[i + 1] <- T
      }
    } else {
    temp_dt_reduced[i + 1] <- T
    }
    
    if (nrow(temp_dt) < i)
      break
  }
  } else {temp_dt_reduced<-rep(T,n.row+1) }
    
  
  
  temp_dt<-temp_dt[temp_dt_reduced]
  default_indicator_unique<-temp_dt
  rm(temp_dt,temp_dt_reduced,i,n.row,default_indicator)
  
  return(default_indicator_unique)
}
