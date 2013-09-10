LRVR ;DALOI/CJS/DALOI/FHS - LAB ROUTINE DATA VERIFICATION ;3/28/90  17:06
 ;;5.2;LAB SERVICE;**42,153,263,286**;Sep 27, 1994
 N LRDUZ,LRVBY
 D INIT G QUIT:$G(LREND)
 S LRVBY=$$SELBY^LRWU4("Verify by")
 I LRVBY=0 D QUIT Q
 I LRVBY=2 D ^LRVRA,QUIT Q
DAT D ADATE^LRWU G:LRAD<1 QUIT
 I $P(^LRO(68,LRAA,0),U,3)="D" F I=0:0 S I=$O(^LRO(68,LRAA,1,LRAD,1,I)) Q:I<1  I $D(^LRO(68,LRAA,1,LRAD,1,I,3)),'$P(^(3),U,4) S LRAN=I Q
 S:$D(^LRO(68,LRAA,1,LRAD,2))&'LRAN LRAN=$P(^(2),U,4)
 D
 . N X
 . S X=$S(+$P($G(^LAB(69.9,1,0)),U,7):+$P(^(0),U,7),1:1)
 . S LRTM60=9999999-$$FMADD^XLFDT(DT,-X)
L10 S LRCFL="",EAMODE=1
 K LRTEST,C5,LRSET,LRLDT,DIC,LRNM,LRNG,LRDL,LRDEL,T,LRFP,LRAB,LRVER,Y,Z
 D WLN G QUIT:LREND
 D ^LRVR1,NEXT
 G L10
 ;
 ;
YN R X:DTIME Q:X=""!(X["N")!(X["Y")  W !,"Answer 'Y' or 'N': " G YN
 ;
WLN ;
 S LRNOP=0
 K DIR,DIRUT,DTOUT,DUOUT
 S DIR(0)="NAO^1:999999:0"
 S DIR("A")="Accession NUMBER: ",DIR("?")="^D LW^LRVR"
 I LRAN'="" S DIR("B")=LRAN
 D ^DIR K DIR
 I $D(DIRUT) G STOP
 S LRAN=Y
 I '$D(^LRO(68,LRAA,1,LRAD,1,LRAN,0)) W !,"NOT ON FILE" S LRNOP=1
 I '$G(LRNOP) D
 . S LRDFN=+^LRO(68,LRAA,1,LRAD,1,LRAN,0),LRCEN=$S($D(^(.1)):^(.1),1:0),LRODT=$S($P(^(0),U,4):$P(^(0),U,4),1:$P(^(0),U,3)),LRSN=$P(^(0),U,5)
 . S LRORU3=$G(^LRO(68,LRAA,1,LRAD,1,LRAN,.3))
 I '$G(LRNOP),$P(LRORU3,U)="" W !?10,"No UID number for this accession",! S LRNOP=1
 I '$G(LRNOP) S LRDPF=$P(^LR(LRDFN,0),U,2),DFN=$P(^(0),U,3) D PT^LRX W !,PNM,?30,SSN W:LRCEN !,"ORDER #: ",LRCEN
 S LRCDT=$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),U)
 I '$G(LRNOP),'$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),"^",3) D
 . N %DT,LRA1,LRA2,LRA3
 . S %DT("B")=$$FMTE^XLFDT(LRCDT,"1")
 . S LRSTATUS="C",LRA1=LRAA,LRA2=LRAD,LRA3=LRAN
 . D P15^LROE1
 . S LRAA=LRA1,LRAD=LRA2,LRAN=LRA3
 . I LRCDT<1 S LRNOP=1 Q
 . I '$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),U,3) S $P(^(3),U,3)=$$NOW^XLFDT
 ; If user did not update then go to next accession
 I '$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),U,3) S LRNOP=1
 S LRCDT=$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),U)
 I $G(LRCDT)<1 S (LRCDT,LRNOP)=1
 ;
 S LRSS=$P(^LRO(68,LRAA,0),U,2)
 I '$G(LRNOP),LRSS'="CH" S LRNOP=1
 ; Check for valid pointer to file #63 and entry in file #63.
 S LRIDT=$P($G(^LRO(68,LRAA,1,LRAD,1,LRAN,3)),U,5)
 I '$G(LRNOP),LRIDT<1 W !,">>>>ERROR - NO POINTER TO FILE #63 - PLEASE NOTIFY SYSTEM MANAGER^ <<<<<",! S LRNOP=1
 I '$G(LRNOP),'$D(^LR(LRDFN,LRSS,LRIDT,0)) W !,">>>>ERROR - NO ENTRY IN FILE #63 - PLEASE NOTIFY SYSTEM MANAGER<<^ <<<",! S LRNOP=1
 ;
 I '$G(LRNOP),$D(^LRO(69,LRODT,1,LRSN)),'$D(^(LRSN,1)) W !,"This Order # has not been collected",$C(7) S LRNOP=1
 I '$G(LRNOP),$D(^LRO(69,LRODT,1,LRSN,1)),$P(^LRO(69,LRODT,1,LRSN,1),U,4)'="C" W !,"You cannot verify an accession which has not been collected.",$C(7) S LRNOP=1
 I $G(LRNOP) D NEXT G WLN
 Q
 ;
 ;
LW ;
 N S
 W !,"Enter range of accession numbers which might apply."
 D LRAN^LRWU3 Q:LREND
 S LRDT=$$FMTE^XLFDT($$DT^XLFDT,"5F")
 S S("LRAA")=LRAA,S("LRAD")=LRAD,S("LRAN")=LRAN
 D W^LRWRKLST
 S LREND=0,LRAA=S("LRAA"),LRAD=S("LRAD"),LRAN=S("LRAN")
 Q
 ;
 ;
QUIT I $G(LRAN),$G(LRAA),$G(LRAD) S LREND=1 I $D(^LRO(68,LRAA,1,LRAD,0)) S:'$D(^(2)) ^(2)="^^" S ^(2)=$P(^(2),U,1,3)_U_LRAN_U_$P(^(2),U,5,99),LREND=1
 ;
CLEAN ;
 I $D(LRCSQ),'$O(^XTMP("LRCAP",LRCSQ,DUZ,0)) K ^XTMP("LRCAP",LRCSQ,DUZ)
 E  I $D(LRAA) D:$P(LRPARAM,U,14)&($P($G(^LRO(68,+LRAA,0)),U,16)) STD^LRCAPV K LRIDIV
 K DIR,LRCMTDSP,LRNOP,XP
 D ^LRVRKIL
 S ZTIO="",ZTRTN="LRCAPV2",ZTDTH=$H,ZTDESC="LAB LRCAPV2 ROUTINE"
 D ^%ZTLOAD K ZTSK
 Q
 ;
 ;
NEXT S LRAN=$O(^LRO(68,LRAA,1,LRAD,1,LRAN)) I LRAN'>0 W !,"LAST IN WORK LIST" S LRAN="^"
 S LREND=0
 Q
 ;
 ;
LIST W " the following tests: " F I=0:0 S I=$O(LRTST(I)) Q:I<1  W !,?10,$P(LRTST(I),"^",1)
 Q
 ;
 ;
EXPAND D EXPLODE^LRGP2
SKPEX Q:$O(LRVTS(0))  ; READY TO GO
STOP S LREND=1
 Q
 ;
 ;
INIT ;from LRVRW
 N DIC,LRX
 D ^LRPARAM Q:$G(LREND)  S LREND=0,LRAN=0 K LRORD,LRDUZ
 S DIC="^LRO(68.2,",DIC(0)="AEMZ",DIC("S")="S LRX=$P(^(0),U,12) Q:'$L(LRX)  I $D(^XUSEC($P($G(^DIC(19.1,LRX,0)),U),DUZ))"
 D ^DIC K DIC("S") G STOP:Y<1 S LRLL=+Y,LRTYPE=$P(Y(0),U,3)
 S LRPROF=$O(^LRO(68.2,LRLL,10,0))
 I LRPROF<1 S LREND=1 W !,"No profile defined." Q
 S B=$O(^LRO(68.2,LRLL,10,LRPROF))
 I B>0 S DIC="^LRO(68.2,"_LRLL_",10," D ^DIC G STOP:Y<1 S LRPROF=+Y
 S X=^LRO(68.2,LRLL,10,LRPROF,0),LRAA=$P(X,U,2),LRPANEL=$P(X,U) I '$D(^LRO(68,LRAA,0))#2 W !?10,$C(7),"Error in your DATABASE. There is not an accession area # ",LRAA,!! Q
 ;
 ; Select performing laboratory to use
 S LRX=$$SELPL^LRVERA($S($P(X,"^",5):$P(X,"^",5),1:DUZ(2)))
 I LRX<1 S LREND=1 Q
 I LRX,LRX'=DUZ(2) S LRDUZ(2)=LRX
 ;
 D:$P(LRPARAM,U,14)&($P($G(^LRO(68,LRAA,0)),U,16)) AUTO^LRCAPV Q:LREND
 I $P(^LRO(68,LRAA,0),U,2)="MI" D ^LRMIEDZ S LREND=1 Q
 G STOP:$P(^LRO(68,LRAA,0),U,2)'="CH"
 S LREND=0 D EXPAND G STOP:LREND!($O(LRVTS(0))<0)
 F I=0:0 S I=$O(LRORD(I)) Q:I<1  S J=LRORD(I),X=$P(^LAB(60,J,0),U,5),LRORD(I)=$P(X,";",2)
 S Y=^LRO(68,LRAA,0),LRTSE=-1
 ;
 D CMTDSP^LRVERA
 ;
REV ;
 K LRPER
 D REV^LRVER
 Q
