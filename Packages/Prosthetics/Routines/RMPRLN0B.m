RMPRLN0B ;PHX/RFM-CONTINUATION OF RMPRLNAU ;10/19/1993
 ;;2.0;PROSTHETICS;;10/19/1993
 S RMPRPDAT=Y,X1=RMPRPDAT,X2=1826 D C^%DTC S RMPREDAT=X,RMPRA=1
 K DIR,Y,DA S DIR(0)="660.1,19" D ^DIR G:$D(DTOUT)!($D(DUOUT)) EXIT^RMPRLN0 S RMPRNR=X
 S ZIEN=RMPRDATE_U_DFN_U_U_RMPRQTY1_U_RMPRCOST_U_RMPRSER_U_RMPRPDAT_U_RMPREDAT_U_1_U_RMPRDATE_"^^^"_RMPRTYPE_"^^"_RMPR("STA")_U_U_RMPRNR_U_U_U_U_RMPRITEM_U_RMPRIEN
 K DIC,DD,DO,D0 S DIC="^RMPR(660.1,",DIC(0)="L",X=DT,DLAYGO=660.1 K DD,DO D FILE^DICN K DLAYGO S ^RMPR(660.1,+Y,0)=ZIEN,(IEN,DA)=+Y,DIK=DIC D IX1^DIK K DIC
 I '$D(^RMPR(660.1,IEN,2)) S ^RMPR(660.1,IEN,2,0)="^660.18DA^^"
 K DD,DO,D0 S DIC="^RMPR(660.1,"_IEN_",2,",DIC(0)="L",DA(1)=IEN,DLAYGO=660.1 F I=RMPRDAT1,RMPRDAT2,RMPRDAT3 S X=I D FILE^DICN
 K DLAYGO S DIK="^RMPR(660.1,",DA=IEN D IX1^DIK
 L +^RMPR(660.1,IEN,0) S DIE=DIC,DR="2.5",DA=IEN D ^DIE L -^RMPR(660.1,IEN,0) G:$D(DTOUT) EXIT^RMPRLN0
 G EN^RMPRLN3
ASK1 ;ASK QUESTION TO EDIT LOAN ENTRY
 S %=2 W !!,"Would you like to edit this loan transaction" D YN^DICN G:Y<0!(%=2) POST2 I %=0 W !,"Answer `YES` or `NO`" G ASK1
EDIT S %DT="AEX",%DT("A")="DATE OF LOAN: ",Y=$P(^RMPR(660.1,IEN,0),U,10) X ^DD("DD") S %DT("B")=Y
 D ^%DT G:Y<0!($D(DTOUT)) EXIT^RMPRLN0 I Y<$P(S667,U) W !,$C(7),"Date of loan must be equal to or greater that the date the Auto-adaptive",!,"transaction was processed",! G EDIT
 I $P(^RMPR(660.1,IEN,0),U,10)'=Y S RMPRDATE=Y D SUBC
CONT K DIR S DIR(0)="660.1,3",DIR("A")="QTY",DIR("B")=$P(^RMPR(660.1,IEN,0),U,4) D ^DIR I X>$P(^RMPR(667.3,RMPRIEN,0),U,7) W $C(7)," ??",!,"Quantity cannot exceed "_$P(^(0),U,7) G CONT
 G:$D(DIRUT) DISP^RMPRLN3 S $P(^RMPR(660.1,IEN,0),U,4)=X,DIR("B")=$P(^RMPR(660.1,IEN,0),U,5) I DIR("B")'>0 G PD
COST K DIR S DIR(0)="660.1,4",DIR("A")="UNIT COST" D ^DIR G:$D(DTOUT)!($D(DUOUT)) DISP^RMPRLN3 I X>$P(^RMPR(667.3,RMPRIEN,0),U,4) W $C(7)," ??",!,"Cost cannot exceed $"_$P(^(0),U,4) G COST
 G:X'?.N.1".".2N COST S $P(^RMPR(660.1,IEN,0),U,5)=X,RMPRPD=$P(^(0),U,7)
PD L +^RMPR(660.1,IEN,0) S DIE="^RMPR(660.1,",DR="5R;19R;@3;6R;I $P(^RMPR(660.1,DA,0),U,7)>$P(S667,U) S $P(^(0),U,7)=RMPRPD W $C(7),!!,""Purchase date must be equal to or earlier than Auto-Adaptive transaction date"" S Y=""@3"";2.5",DA=IEN
 D ^DIE L -^RMPR(660.1,IEN,0) G:$D(DTOUT) EXIT^RMPRLN0 G DISP^RMPRLN3
SUBC S $P(^RMPR(660.1,IEN,0),U,10)=RMPRDATE,DA(1)=IEN,I=0 F  S I=$O(^RMPR(660.1,IEN,2,I)) Q:I=""  S DIK="^RMPR(660.1,"_DA(1)_",2,",DA=I D ^DIK
 S X1=RMPRDATE,X2=180 D C^%DTC S RMPRDAT1=X,X1=RMPRDATE,X2=360 D C^%DTC S RMPRDAT2=X,X1=RMPRDATE,X2=720 D C^%DTC S RMPRDAT3=X
 I '$D(^RMPR(660.1,IEN,2)) S ^RMPR(660.1,IEN,2,0)="^660.1DA^^"
 K DINUM,DD,DO,D0 S DIC="^RMPR(660.1,"_IEN_",2,",DIC(0)="L",DA(1)=IEN,DLAYGO=660.1 F I=RMPRDAT1,RMPRDAT2,RMPRDAT3 S X=I D FILE^DICN
 K DLAYGO S DIK="^RMPR(660.1,",DA=IEN D IX1^DIK
 Q
DEL S %=2 W !,"Would you like to delete this transaction" D YN^DICN G:$D(DTOUT) DEL1 G:%<0 EN^RMPRLN3 G:%=2 POST2 I %=0 W !,"Answer 'YES' or 'NO'" G DEL
DEL1 S DIK="^RMPR(660.1,",DA=IEN D ^DIK W !!,$C(7),"Deleted..." G EXIT^RMPRLN0
POST2 S %=1 W !,"Would you like to post this loan card" D YN^DICN G:%<0 DEL1
 G:%=2 DEL I %=0 W !,"Answer 'YES' or 'NO'" G POST2
 I $D(^RMPR(660.2,"AC",ZRMP)) S RO=$O(^RMPR(660.2,"AC",ZRMP,0))
 I  I $D(^RMPR(660.2,RO,0)) S RIEN=^RMPR(660.1,IEN,0),$P(^RMPR(660.2,RO,0),U,19)="N",$P(^RMPR(660.2,RO,0),U,17)=$P(RIEN,U,17)
 I $D(RIEN) S $P(^RMPR(660.2,RO,0),U,6)=RMPRSER S DIK="^RMPR(660.2,",DA=RO D IX^DIK G 660
 K DD,DO S DIC="^RMPR(660.2,",DIC(0)="L",X=DT,DLAYGO=660.2 D FILE^DICN S RO=+Y K DLAYGO
 S ^RMPR(660.2,RO,0)=^RMPR(660.1,IEN,0)
 S DIK=DIC,DA=RO D IX1^DIK
660 S $P(^RMPR(660.1,IEN,0),U,20)=RO S DIK="^RMPR(660.1,",DA=IEN D IX^DIK S $P(^RMPR(667.3,ZRMP,0),U,6)=RMPRSER G EXIT^RMPRLN0
