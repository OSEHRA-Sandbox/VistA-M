DGBTSP1 ;ALB/BLD-BENEFICIARY TRAVEL SPECIAL MODE OF TRANSPORTATION; 12/18/2011@1000; 12/23/2012
 ;;1.0;Beneficiary Travel;**20**;December 27, 2011;Build 185
 ;
ACCT ;  allowed to select only valid active accounts
 S DGBTOACT=$S('$D(^DGBT(392.3,+$P(DGBTVAR(0),"^",6),0)):0,1:+$P(^DGBT(392.3,$P(DGBTVAR(0),"^",6),0),"^",5))
 K X S (DIC("B"),X)=$S(+$P(DGBTVAR(0),"^",6):$P(^DGBT(392.3,$P(DGBTVAR(0),"^",6),0),"^"),1:$$DEFLT1^DGBTEE1) S DIC("A")="Select ACCOUNT: "
 S DIC="^DGBT(392.3,",DIC(0)="AEQMZ",DIC("S")="I $P(^(0),U,3)'>DGBTDT&('$P(^(0),U,4)!($P(^(0),U,4)'<DGBTDT))"
 D ^DIC K DIC I $D(DTOUT) S DGBTTOUT=-1 K DTOUT Q
 I Y'>0 W !,"ACCOUNT IS REQUIRED!!" G ACCT
 S DGBTACTN=$P(Y,"^"),DGBTACCT=$P($G(Y(0)),"^",5)
 ;  if account is ALL OTHER - stuff in mileage info
 I $D(DGBTVAR("M")) S DGBTML=$P(DGBTVAR("M"),"^",2),DGBTOWRT=$P(DGBTVAR("M"),"^"),DGBTMLT=$J((DGBTML*DGBTOWRT*DGBTMR),0,2)
 Q
 ;
DGBTDC(DGBTDT,DFN) ;this will process denie the claim and issue a denial letter
 ;
 W !!,"CLAIM HAS BEEN DENIED AND DENIAL OF BENEFITS LETTER WILL BE ISSUED"
 I '$G(DGBTAPPTYP) D DGBTDR^DGBTDLT
 W !!,"PLEASE COMPLETE THE INVOICE INFORMATION." H 1
 Q
 ;
DELSP(DGBTDT) ;this will delete the special mode data when editing a claim and the claim changes from a Special Mode to a Mileage Claim
 ;
 N ACCT,ACTNBR,NULL,DGBTDL
 S (ACTNBR,ACCT,NULL)=""
 F  S ACCT=$O(^DGBT(392.3,"D","ALL OTHER",ACCT)) Q:ACCT=""  D  Q:ACTNBR
 .I $P(^DGBT(392.3,ACCT,0),"^",4)="" S ACTNBR=ACCT
 S ACTNBR=$$GET1^DIQ(392.3,ACCT,.01)
 K ^DGBT(392,DGBTDT,"SP"),^DGBT(392,DGBTDT,"SPAD")
 ;
 S DGBTDL("ISSUED")=""
 S DGBTDL("ISSUED DATE")=""
 S DGBTDL("CLAIM DENIED")="NO"
 S DGBTDL("DT DENIED")=""
 S DGBTDL("DENIED REASON")=""
 S DGBTDL("ACCT NBR")=ACTNBR
 S DGBTDL("CLAIM TYPE")=""
 ;
 S DGBTFDA(392,DGBTDTI_",",45)=$G(DGBTDL("ISSUED"))
 S DGBTFDA(392,DGBTDTI_",",45.1)=$G(DGBTDL("ISSUED DATE"))
 S DGBTFDA(392,DGBTDTI_",",45.2)=$G(DGBTDL("CLAIM DENIED"))
 S DGBTFDA(392,DGBTDTI_",",45.3)=$G(DGBTDL("DT DENIED"))
 S DGBTFDA(392,DGBTDTI_",",45.4)=$G(DGBTDL("DENIED REASON"))
 S DGBTFDA(392,DGBTDTI_",",6)=$G(DGBTDL("ACCT NBR"))
 S DGBTFDA(392,DGBTDTI_",",56)=$G(DGBTCMTY)
 D FILE^DIE("EKTS","DGBTFDA","ERRMSG") K DGBTFDA
 Q
 ;
FILE(DGBTDT) ;this will file the special mode into file 392 (Beneficiary Travel Claim File).
 ;
 N DGBTDTI,DGBTFDA
 W !!,"FILING....."
 ;
 S DGBTDTI=DGBTA
 D CLEANUP^DGBTSP
 S DGBTFDA(392,DGBTDTI_",",3)=$P(VAEL(1),"^",2)                     ;primary eligibility
 S DGBTFDA(392,DGBTDTI_",",4)=$P(VAEL(3),"^",3)                     ;% of disablility
 S DGBTFDA(392,DGBTDTI_",",6)=$G(DGBTSP("ACCOUNT"))
 S DGBTFDA(392,DGBTDTI_",",11)=DGBTDIVI                             ;division
 ;S DGBTFDA(392,DGBTDTI_",",12)=$P(^VA(200,DUZ,0),"^",1)             ;person who entered claim
 S DGBTFDA(392,DGBTDTI_",",13)=DT                                   ;date claim was entered 
 S DGBTFDA(392,DGBTDTI_",",56)=$G(DGBTSP("CLAIM TYPE"))
 S DGBTFDA(392,DGBTDTI_",",57)=$G(DGBTSP("MODE OF TRANS"))
 S DGBTFDA(392,DGBTDTI_",",58)=$G(DGBTSP("INVOICE NUMBER"))
 S DGBTFDA(392,DGBTDTI_",",59)=$G(DGBTSP("DT INVOICE REC"))
 S DGBTFDA(392,DGBTDTI_",",60)=$G(DGBTSP("TOTAL INVOICE"))
 S DGBTFDA(392,DGBTDTI_",",61)=$G(DGBTSP("BASE RATE FEE"))
 S DGBTFDA(392,DGBTDTI_",",62)=$G(DGBTSP("MILEAGE FEE"))
 S DGBTFDA(392,DGBTDTI_",",63)=$G(DGBTSP("NO SHOW"))
 S DGBTFDA(392,DGBTDTI_",",64)=$G(DGBTSP("WAIT TIME"))
 S DGBTFDA(392,DGBTDTI_",",65)=$G(DGBTSP("EXTRA CREW"))
 S DGBTFDA(392,DGBTDTI_",",66)=$G(DGBTSP("SP EQUIP"))
 S DGBTFDA(392,DGBTDTI_",",67)=$G(DGBTSP("RT/ONE WAY"))
 S DGBTFDA(392,DGBTDTI_",",68)=$G(DGBTSP("TOTAL MILES"))
 S DGBTFDA(392,DGBTDTI_",",69)=$G(DGBTSP("OTHER TRANS REMARKS"))
 S DGBTFDA(392,DGBTDTI_",",70)=$G(DGBTSP("PRE-AUTHORIZED"))
 S DGBTFDA(392,DGBTDTI_",",71)=$G(DGBTSP("VENDOR"))
 S DGBTFDA(392,DGBTDTI_",",72)=$G(DGBTSP("SP MODE OTHER REMARKS"))
 S DGBTFDA(392,DGBTDTI_",",73)=$G(DGBTSP("PLACE OF DEPARTURE "))
 S DGBTFDA(392,DGBTDTI_",",85)=$G(DGBTSP("AUTHORIZED"))
 I $G(DGBTSP("AUTHORIZED"))="Y" D
 .S DGBTFDA(392,DGBTDTI_",",84)=$$GET1^DIQ(200,$G(DUZ),.01)
 .S DGBTFDA(392,DGBTDI_",",86)=$$FMTE^XLFDT(DT)  ;$G(DGBTDT)
 ;
 S DGBTFDA(392,DGBTDTI_",",73)=$G(DGBTSP("PLACE OF DEPARTURE"))
 S DGBTFDA(392,DGBTDTI_",",74)=$G(DGBTSP("PLACE OF DEPARTURE 2"))
 S DGBTFDA(392,DGBTDTI_",",75)=$G(DGBTSP("CITY OF DEPARTURE"))
 S DGBTFDA(392,DGBTDTI_",",76)=$G(DGBTSP("STATE OF DEPARTURE"))
 S DGBTFDA(392,DGBTDTI_",",77)=$G(DGBTSP("ZIP CODE/DEPARTURE"))
 ;
 S DGBTFDA(392,DGBTDTI_",",78)=$G(DGBTINST("DIVISION"))
 S DGBTFDA(392,DGBTDTI_",",79)=$G(DGBTINST("INST NAME"))
 S DGBTFDA(392,DGBTDTI_",",80)=$G(DGBTINST("ADDRESS1"))
 S DGBTFDA(392,DGBTDTI_",",81)=$G(DGBTINST("CITY"))
 S DGBTFDA(392,DGBTDTI_",",82)=$G(DGBTINST("STATE"))
 S DGBTFDA(392,DGBTDTI_",",83)=$G(DGBTINST("ZIP CODE"))
 S DGBTFDA(392,DGBTDTI_",",42)=VADM(1)
 ;
 D FILE^DIE("EKTS","DGBTFDA","ERRMSG")
 I '$D(ERRMSG) W " COMPLETED..." D
 .S DGBTFDA1(392,DGBTDTI_",",12)=DUZ D FILE^DIE(,"DGBTFDA1","ERRMSG")             ;person who entered claim
 .W $$PAUSE^DGBTUTL(0) S (DGBTSP,SPCOMPLETE)=1
 I $D(ERRMSG) W !!,"DATA DID NOT FILE DUE TO ERRORS",!! M ^TMP("DGBTE",$J,DGBTDTI)=ERRMSG S PROMPT="PRESS RETURN TO QUIT AND DELETE THE CLAIM..." D  Q
 .W $$PAUSE^DGBTUTL(PROMPT)
 .S DGBTTOUT=-1,DGBTOLD=0,DGBTDT=DGBTDTI
 .K ^DGBT(392,DGBTDTI,"A")
 .D EXIT2^DGBTE S DGBTSP=0
 S DGBTSP=1,CHZFLG=1
 Q
 ;
INVAMT ;this will ask the dollar amount of any extra fees
 ;
 S DGBTINTO=0
 W !
 S BASERATE=$$GET1^DIQ(392,DGBTDT,61,"E")
 S DIR("A")="BASE RATE FEE: "
 S DIR("?")="Enter the Vendor's Base Rate. Type a Dollar Amount between 1 and 100,000, 2 Decimal Digits"
 S DIR(0)="NA^0:100000:2"
 S BASERATE=$S($G(BASERATE)'="":BASERATE,$G(DGBTSP("BASE RATE FEE"))'="":$G(DGBTSP("BASE RATE FEE")),1:"")
 I BASERATE'="" S DIR("B")=$S($G(BASERATE)'="":BASERATE,$G(DGBTSP("BASE RATE FEE"))'="":$G(DGBTSP("BASE RATE FEE")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I Y=""!($D(DTOUT))!($D(DUOUT)) S DGBTSP=0 Q
 S DGBTSP("BASE RATE FEE")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S MILEAGEFEE=$$GET1^DIQ(392,DGBTDT,62,"E")
 S DIR("A")="MILEAGE FEE: "
 S DIR("?")="Enter the Vendor's Mileage fee. Type a Dollar Amount between 1 and 10,000, 2 Decimal Digits."
 S DIR(0)="NA^0:10000:2"
 S MILEAGEFEE=$S($G(MILEAGEFEE)'="":MILEAGEFEE,$G(DGBTSP("MILEAGE FEE"))'="":$G(DGBTSP("MILEAGE FEE")),1:"")
 I MILEAGEFEE'="" S DIR("B")=$S($G(MILEAGEFEE)'="":MILEAGEFEE,$G(DGBTSP("MILEAGE FEE"))'="":$G(DGBTSP("MILEAGE FEE")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I ($D(DTOUT))!($D(DUOUT)) K DGBTSP S DGBTSP=0 Q
 S DGBTSP("MILEAGE FEE")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S NOSHOW=$$GET1^DIQ(392,DGBTDT,63,"E")
 S DIR("A")="NO-SHOW/NO-LOAD FEE: "
 S DIR("?")="Type a Dollar Amount between 1 and 1000, 2 Decimal Digits."
 S DIR(0)="FOA^0:1000:2"
 S NOSHOW=$S($G(NOSHOW)'="":NOSHOW,$G(DGBTSP("NO SHOW"))'="":$G(DGBTSP("NO SHOW")),1:"")
 I NOSHOW'="" S DIR("B")=$S($G(NOSHOW)'="":NOSHOW,$G(DGBTSP("NO SHOW"))'="":$G(DGBTSP("NO SHOW")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I ($D(DTOUT))!($D(DUOUT)) K DGBTSP S DGBTSP=0 Q
 S DGBTSP("NO SHOW")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S WAITTIME=$$GET1^DIQ(392,DGBTDT,64,"E")
 S DIR("A")="WAIT TIME FEE: "
 S DIR("?")="Enter the Vendor's fee for waiting. Type a Dollar Amount between 1 and 1000, 2 Decimal Digits."
 S DIR(0)="NOA^0:1000:2"
 S WAITTIME=$S($G(WAITTIME)'="":WAITTIME,$G(DGBTSP("WAIT TIME"))'="":$G(DGBTSP("WAIT TIME")),1:"")
 I WAITTIME'="" S DIR("B")=$S($G(WAITTIME)'="":WAITTIME,$G(DGBTSP("WAIT TIME"))'="":$G(DGBTSP("WAIT TIME")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I ($D(DTOUT))!($D(DUOUT)) K DGBTSP S DGBTSP=0 Q
 S DGBTSP("WAIT TIME")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S EXTRACREW=$$GET1^DIQ(392,DGBTDT,65,"E")
 S DIR("A")="EXTRA CREW FEE: "
 S DIR("?")="Enter the Vendor's extra crew fee. Type a Dollar Amount between 1 and 10,000, 2 Decimal Digits."
 S DIR(0)="NOA^0:10000:2"
 S EXTRACREW=$S($G(EXTRACREW)'="":EXTRACREW,$G(DGBTSP("EXTRA CREW"))'="":$G(DGBTSP("EXTRA CREW")),1:"")
 I EXTRACREW'="" S DIR("B")=$S($G(EXTRACREW)'="":EXTRACREW,$G(DGBTSP("EXTRA CREW"))'="":$G(DGBTSP("EXTRA CREW")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I ($D(DTOUT))!($D(DUOUT)) K DGBTSP S DGBTSP=0 Q
 S DGBTSP("EXTRA CREW")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S SPEQUIP=$$GET1^DIQ(392,DGBTDT,66,"E")
 S DIR("A")="SPECIAL EQUIPMENT FEE: "
 S DIR("?")="Enter the Vendor's fee for additional equipment needed. Type a Dollar Amount between 1 and 5000, 2 Decimal Digits."
 S DIR(0)="NOA^0:5000:2"
 S SPEQUIP=$S($G(SPEQUIP)'="":SPEQUIP,$G(DGBTSP("SP EQUIP"))'="":$G(DGBTSP("SP EQUIP")),1:"")
 I SPEQUIP'="" S DIR("B")=$S($G(SPEQUIP)'="":SPEQUIP,$G(DGBTSP("SP EQUIP"))'="":$G(DGBTSP("SP EQUIP")),1:0)
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I ($D(DTOUT))!($D(DUOUT)) K DGBTSP S DGBTSP=0 Q
 S DGBTSP("SP EQUIP")=Y
 S DGBTINTO=$G(DGBTINTO)+Y
 ;
 W !
 S TOTINVOICE=$$GET1^DIQ(392,DGBTDT,60,"I")
 S DIR("A")="TOTAL INVOICE: "
 S DIR("?")="Type a Dollar Amount between 1 and 127,000, 2 Decimal Digits."
 S DIR(0)="NA^0:127000:2"
 I TOTINVOICE'="" S DIR("B")=$S($G(TOTINVOICE)'="":TOTINVOICE,1:$G(DGBTSP("TOTAL INVOICE")))
 D ^DIR K DIR S:$D(^DGBT(392,DGBTDTI,"SP")) SPCOMPLETE=1 I Y=""!($D(DTOUT))!($D(DUOUT)) S DGBTSP=0 Q
 S DGBTSP("TOTAL INVOICE")=Y
 ;
 Q
 ;
DENREA(DGBTCMTY) ;denial reasons
 ;
 ;S DGBTOACT=$S('$D(^DGBT(392.5,+$P(DGBTVAR(0),"^",6),0)):0,1:+$P(^DGBT(392.3,$P(DGBTVAR(0),"^",6),0),"^",5))
 K X S (DIC("B"),X)="" S DIC("A")="Select Denial Reason:"
 S DIC="^DGBT(392.5,",DIC(0)="AEQMZ" ;,DIC("S")="I $P(^(0),U,3)'>DGBTDT&('$P(^(0),U,4)!($P(^(0),U,4)'<DGBTDT))"
 D ^DIC K DIC I $D(DTOUT) S DGBTTOUT=-1 K DTOUT Q
 S DGBTACTN=$P(Y,"^"),DGBTACCT=$P($G(Y(0)),"^",5)
 ;
 Q
