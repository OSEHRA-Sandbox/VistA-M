PSDADJI ;B'ham ISC/LTL - Balance Initializer ; 3 June 92
 ;;3.0; CONTROLLED SUBSTANCES ;**66**;13 Feb 97;Build 3
 I '$D(PSDSITE) D ^PSDSET G:'$D(PSDSITE) QUIT
 N D1,D2,DIC,DIE,DINUM,D0,D1,DLAYGO,DR,DIR,DIRUT,DTOUT,DUOUT,PSAC,PSDAT,PSDLOC,PSDLOCN,DA,PSDRUG,PSDRUGN,PSDS,PSAQ,PSDR,PSDREC,PSDT,X,Y,%,%H,%I
 D ^PSDSP I $D(PSDS),$D(^PSD(58.8,+PSDS,0)) S PSDLOC=PSDS,PSDLOCN=$P($G(^PSD(58.8,+PSDS,0)),U) G CHKD
LOOK S DIC="^PSD(58.8,",DIC(0)="AEMQ",DIC("A")="Select Dispensing Site: ",DIC("S")="I $P($G(^(0)),U,3)=+PSDSITE,$S($P($G(^(0)),U,2)[""M"":1,$P($G(^(0)),U,2)[""S"":1,1:0),$S('$D(^(""I"")):1,+^(""I"")>DT:1,'^(""I""):1,1:0)"
 D ^DIC K DIC G:$D(DTOUT)!($D(DUOUT))!(Y<1) QUIT S PSDLOC=+Y,PSDLOCN=$P(Y,U,2)
CHKD I '$O(^PSD(58.8,PSDLOC,1,0)) W !!,"There are no drugs in ",PSDLOCN G QUIT
 W !!,"Give me a second to alphabetize.",!
 S PSDRUG=0,PSDRUGN=""
 F  S PSDRUG=$O(^PSD(58.8,PSDLOC,1,PSDRUG)) Q:'PSDRUG!($P($G(^PSDRUG(+PSDRUG,0)),U)']"")  D
 .S ^TMP("PSDB",$J,$P($G(^PSDRUG(+PSDRUG,0)),U),PSDRUG)="" K Y
 W @IOF
 F PSAC=1:1 S PSDRUGN=$O(^TMP("PSDB",$J,PSDRUGN)) Q:PSDRUGN']""  S PSDRUG=$O(^TMP("PSDB",$J,PSDRUGN,0)) D  G:$D(Y) QUIT
 .I $P($G(^PSD(58.8,PSDLOC,1,PSDRUG,0)),U,4)]"" D:$Y+9>IOSL  Q:$D(DUOUT)!($D(DTOUT))  W !!,PSDRUGN," may have to be adjusted.",!!,"There's already ",$P($G(^PSD(58.8,+PSDLOC,1,+PSDRUG,0)),U,4)," on hand." Q
 ..S DIR(0)="E" D ^DIR K DIR K:Y Y W @IOF
 .W !!,PSDRUGN,!!,"Package Size: "
 .W $P($G(^PSD(58.8,+PSDLOC,1,+PSDRUG,0)),U,9),"  Breakdown Unit: ",$P($G(^(0)),U,8),!
 .F  L +^PSD(58.8,+PSDLOC,1,+PSDRUG,0):$S($G(DILOCKTM)>0:DILOCKTM,1:3) I  Q
 .S DIR(0)="N^0:999999:0",DIR("A")="Initial Balance" D ^DIR K DIR
 .I $D(DIRUT) L -^PSD(58.8,+PSDLOC,1,+PSDRUG,0) S Y=1 Q
 .S PSDREC=Y D NOW^%DTC S PSDAT=+%
 .S DIE="^PSD(58.8,+PSDLOC,1,",DA(1)=PSDLOC,DA=PSDRUG,DR="3////"_PSDREC
 .D ^DIE
 .L -^PSD(58.8,+PSDLOC,1,+PSDRUG,0)
 .Q:$G(PSDREC)']""
MON .S:'$D(^PSD(58.8,+PSDLOC,1,+PSDRUG,5,0)) ^(0)="^58.801A^^"
 .I '$D(^PSD(58.8,+PSDLOC,1,+PSDRUG,5,$E(DT,1,5)*100,0)) S DIC="^PSD(58.8,+PSDLOC,1,+PSDRUG,5,",DIC(0)="LM",DLAYGO=58.8,(X,DINUM)=$E(DT,1,5)*100,DA(2)=PSDLOC,DA(1)=PSDRUG D ^DIC K DIC,DLAYGO
 .S DIE="^PSD(58.8,+PSDLOC,1,+PSDRUG,5,",DA(2)=PSDLOC,DA(1)=PSDRUG,DA=$E(DT,1,5)*100,DR="1////0;7////^S X=PSDREC" D ^DIE
 .W !!,"Updating beginning balance and transaction history.",!
TR .F  L +^PSD(58.81,0):$S($G(DILOCKTM)>0:DILOCKTM,1:3) I  Q
FIND .S PSDT=$P(^PSD(58.81,0),U,3)+1 I $D(^PSD(58.81,PSDT)) S $P(^PSD(58.81,0),U,3)=$P(^PSD(58.81,0),U,3)+1 G FIND
 .S DIC="^PSD(58.81,",DIC(0)="L",DLAYGO=58.81,(DINUM,X)=PSDT D ^DIC K DIC,DLAYGO L -^PSD(58.81,0)
 .S DIE="^PSD(58.81,",DA=PSDT,DR="1////11;2////^S X=PSDLOC;3////^S X=PSDAT;4////^S X=PSDRUG;5////^S X=PSDREC;6////^S X=DUZ;9////0;100////1" D ^DIE K DIE
 .S:'$D(^PSD(58.8,+PSDLOC,1,+PSDRUG,4,0)) ^(0)="^58.800119PA^^"
 .S DIC="^PSD(58.8,+PSDLOC,1,+PSDRUG,4,",DIC(0)="L",DLAYGO=58.8
 .S (X,DINUM)=PSDT,DA(2)=PSDLOC,DA(1)=PSDRUG D ^DIC K DIC,DA,DLAYGO,Y
QUIT K ^TMP("PSDB",$J) Q
