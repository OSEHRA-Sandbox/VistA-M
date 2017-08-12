PRCHMA2 ;WISC/AKS-Amendments to purchase orders and requisitions ;6/9/96  20:44
 ;;5.1;IFCAP;**191**;Oct 20, 2000;Build 4
 ;Per VA Directive 6402, this routine should not be modified.
 ;
 ;PRC*5.1*191 Modify Prompt Pay % handling during Amendment to
 ;            insure ONLY one PP entry is allowed and only able
 ;            to allow/edit one entry in what is defined as a
 ;            multiple field.
 ;
EN10 ;EST. SHIPPING Edit
 N X,I,PRCHO,PRCHN,PRCHOO,PRCH0,PRCHSBOC,PRCH12,PRCHGNP,PRCHGPO,PRCHGSHP
 N PRCHSHP
 S (I,ER)=0,X=""
 ;F  S I=$O(^PRC(442,PRCHPO,11,I)) Q:I'>0  I $D(^(I,0)) S X=$P(^(0),U,8) Q:X]""
 D CAN^PRCHMA3
 I $G(NOCAN)=1 W !?5,$S($D(PRCHREQ):"REQUISITION",1:"PURCHASE ORDER")_" HAS BEEN RECEIVED, CANNOT CHANGE ESTIMATED SHIPPING!",$C(7) Q
 S PRCH0=$G(^PRC(443.6,PRCHPO,0))
 S PRCHO=$P(PRCH0,U,13),PRCHOO=$P(^PRC(443.6,PRCHPO,23),U)
 S PRCH12=$G(^PRC(443.6,PRCHPO,12)) I PRCH12]"" D
 .S PRCHGNO=$P(PRCH12,U,7),PRCHGPO=$P(PRCH12,U,8),PRCHGSHP=$P(PRCH12,U,9)
 S DR="13;I X=""""!($P($G(^PRC(442,PRCHPO,23)),U)]"""") S Y="""";13.05"
 S DIE="^PRC(443.6,",DA=PRCHPO
 I $P(^PRC(442,PRCHPO,0),U,19)=2 D
 .S PRCHSBOC=$P($G(^PRCD(420.2,2299,0)),U)
 .S DR="13;I X=""""!($P($G(^PRC(442,PRCHPO,23)),U)]"""") S Y="""";13.05////^S X=PRCHSBOC"
 D ^DIE
 S PRCHN("FOB")=$P($G(^PRC(443.6,PRCHPO,1)),U,6),PRCHSHP=$P(^(0),U,13)
 I $P($G(^PRC(443.6,PRCHPO,0)),U,13)]"" D
 .I (PRCHN("FOB")="O"&((PRCHSHP>250)!(PRCHSHP=0))) S DR="13.2;13.4;13.3" D ^DIE K DIE
 I PRCHSHP=""!(PRCHSHP'>250&(PRCHSHP'=0)) D GBL
 S PRCHX=X
 I PRCHO'=$P($G(^PRC(443.6,PRCHPO,0)),U,13) S X=$S(PRCHO]"":PRCHO,1:0) D EN4^PRCHAMXC
 I PRCHOO'=$P($G(^PRC(443.6,PRCHPO,23)),U) S X=$S(PRCHOO]"":PRCHOO,1:0) D EN11^PRCHAMXC
 I PRCHGNO'=$P($G(^PRC(443.6,PRCHPO,12)),U,7) S X=$S(PRCHGNO]"":PRCHGNO,1:0) D EN12^PRCHAMXC
 I PRCHGPO'=$P($G(^PRC(443.6,PRCHPO,12)),U,8) S X=$S(PRCHGPO]"":PRCHGPO,1:0) D EN14^PRCHAMXC
 I PRCHGSHP'=$P($G(^PRC(443.6,PRCHPO,12)),U,9) S X=$S(PRCHGSHP]"":PRCHGSHP,1:0) D EN13^PRCHAMXC
 S X=PRCHX,PRCHN=$P(^PRC(443.6,PRCHPO,0),U,13) K PRCHX,PRCHOO
 I PRCHO=""&(PRCHN]"") D
 .S $P(^PRC(443.6,PRCHPO,0),U,14)=$P(PRCH0,U,14)+1
 .S $P(^PRC(443.6,PRCHPO,0),U,18)=$P(PRCH0,U,14)+1
 I PRCHO]""&(PRCHN="") D
 .S $P(^PRC(443.6,PRCHPO,0),U,14)=$P(PRCH0,U,14)-1,$P(^(0),U,18)=""
 .S $P(^PRC(443.6,PRCHPO,23),U)=""
 S DELIVER=1 W !
 QUIT
EN11 ;F.C.P. Edit
 N X,I
 S (I,ER)=0,X=""
 ;F  S I=$O(^PRC(442,PRCHPO,11,I)) Q:'I  I $D(^(I,0)) S X=$P(^(0),U,8) Q:X]""
 D CAN^PRCHMA3
 I $G(NOCAN)=1 W !?5,$S($D(PRCHREQ):"REQUISITION",1:"PURCHASE ORDER")_" HAS BEEN RECEIVED, CANNOT CHANGE FUND CONTROL POINT!",$C(7) Q
 I $P(^PRC(442,PRCHPO,0),U,12)>0 W !!,?5,"This purchase order has a 2237 attached to it.",!,?5,"To change F.C.P. you must do the following: " D  QUIT
 .W !!,?7,"1. Cancel the purchase order." Q:$G(PRCHAUTH)
 .W !,?7,"2. Copy the 2237 to another 2237 with new FCP."
 .W !,?7,"3. Have it signed by CP Official and Accountable Officer."
 .W !,?7,"4. Attach the 2237 to a new purchase order."
 S DR="1;2;5.2",DIE="^PRC(443.6,",DA=PRCHPO D ^DIE K DIE
 QUIT
EN12 ;Change Vendor
 N X,I,DLAYGO,N,NN
 S (I,ER)=0,X=""
 ;F  S I=$O(^PRC(442,PRCHPO,11,I)) Q:I'>0  I $D(^(I,0)) S X=$P(^(0),U,8) Q:X]""
 D CAN^PRCHMA3
 I $G(NOCAN)=1 W !?5,$S($D(PRCHREQ):"REQUISITION",1:"PURCHASE ORDER")_" HAS BEEN RECEIVED, CANNOT CHANGE VENDOR!",$C(7) Q
 S DIC="^PRC(440,",DIC(0)="AEQ"
 S:$D(PRCHREQ) DIC("S")="I $P($G(^(2)),U,2)'="""""
 S:'$D(PRCHREQ) DIC("S")="I $P($G(^(2)),U,2)="""""
 S:$P($G(^PRC(443.6,PRCHPO,1)),U) DIC("B")=$P(^PRC(440,$P(^(1),U),0),U)
 D ^DIC K DIC Q:Y<0  S PRCHN=+Y
 S DR="5///"_+Y,DIE="^PRC(443.6,",DA=PRCHPO D ^DIE K DIE
 S N=""
 F  S N=$O(^PRC(443.6,PRCHPO,2,N)) Q:'N  S NN=$P(^(N,0),U,5) I '$D(^PRC(441,NN,2,PRCHN)) D
 .W !,"For item, ",$P(^PRC(441,NN,0),U,2)
 .W !?5,"Enter the following information: "
 .S DA(1)=NN,DIC="^PRC(441,"_DA(1)_",2,",DIC(0)="LZ",DIC("DR")="1;1.5;2;3;4"
 .S DIE("NO^")="",DLAYGO=441,(DA,DA(1))=NN,X=PRCHN D ^DIC K DIC,DIE("NO^")
 .S DIE="^PRC(441,"_DA(1)_",2,",DA=1,DR=6 D ^DIE
 S N=0 F  S N=$O(^PRC(443.6,PRCHPO,2,N)) Q:'N  I $P($G(^PRC(443.6,PRCHPO,2,N,2)),U,2)]'"" D
 . W !,"For line item: ",+^PRC(443.6,PRCHPO,2,N,0)
 . W !?5,"Enter the following information: "
 . S DA(1)=PRCHPO,DA=N,DIE="^PRC(443.6,"_DA(1)_",2,",DR=4 D ^DIE K DA,DIE,DR
 S DELIVER=1 W !
 QUIT
 ;
EN14 ;Prompt payment edit
 N DIC,DIE,DA,DR,Y,PRCHX,PRCHXX,PRCHVAL,PRCHDA,%X,%Y,PRCHPP
 I '$D(^PRC(443.6,PRCHPO,5)) D
 .S %X="^PRC(442,PRCHPO,5,",%Y="^PRC(443.6,PRCHPO,5," D %XY^%RCR
 ;PRC*5.1*191 Code below insures only a single Prompt Pay entry
 ;            allowed and only that single entry can be edited,
 ;            if one defined in multiple field.
 ;Begin PRC*191
 S PRCHPP=$O(^PRC(443.6,PRCHPO,5,0)) D:PRCHPP
 . S (PRCHDA,DA)=PRCHPP
 . S PRCHVAL=$G(^PRC(443.6,PRCHPO,5,DA,0)),Y(0)=PRCHVAL
 . S PRCHP0=Y(0),PRCHO=$P(Y(0),U)_"/"_$P(Y(0),U,2),PRCHXX=$P(Y(0),U,3)
 . S DR=".01//^S X=""NET"";1//^S X=30"
 . S DA(1)=PRCHPO,DIE="^PRC(443.6,"_DA(1)_",5," D ^DIE
 I 'PRCHPP S DA(1)=PRCHPO,DIC="^PRC(443.6,"_DA(1)_",5,",DIC(0)="AELQZ" D ^DIC Q:Y<0  S (PRCHDA,DA)=+Y,PRCHXX=$P(Y,U,3) D
 . S PRCHVAL=$G(^PRC(443.6,PRCHPO,5,DA,0))
 . S PRCHP0=Y(0),PRCHO=$P(Y(0),U)_"/"_$P(Y(0),U,2)
 . S $P(^PRC(443.6,PRCHPO,5,0),U,2)=$P(^DD(443.6,9.2,0),U,2)
 . S DA(1)=PRCHPO,DIE="^PRC(443.6,"_DA(1)_",5,"
 . S DR=".01//^S X=""NET"";1//^S X=30"
 . D ^DIE
 ;End PRC*191
 S DA(1)=PRCHPO,DA=PRCHDA,PRCHX=X
 S X=$S(PRCHXX=1:0,1:$P(PRCHVAL,U)) D EN0^PRCHAMXB
 S X=$S(PRCHXX=1:0,1:$P(PRCHVAL,U,2)) D EN1^PRCHAMXB
 ;S X=$S(PRCHXX=1:0,1:$P(PRCHVAL,U,5)) D EN11^PRCHAMXB
 S X=PRCHX
 W !
 QUIT
GBL ;Delete GBL information
 N DIE,DA,DR
 S DIE="^PRC(443.6,",DA=PRCHPO,DR="13.2///@;13.4///@;13.3///@"
 D ^DIE
