LBRYCK2 ;ISC2/DJM-CHECK-IN ROUTING LIST PRINTING ;[ 05/23/97  12:13 PM ]
 ;;2.5;Library;**2**;Mar 11, 1996
START S LBRYQ=0 D NOW^%DTC S LBRYDT=%,LBRYD=%\1
ST S LBRYQ=$O(^LBRY(682,"A2",LBRYQ)) G:LBRYQ="" EXIT
 S Q1=0
ST1 S Q1=$O(^LBRY(682,"A2",LBRYQ,Q1)) G ST:Q1=""
 I $P(^LBRY(682,LBRYQ,0),U,4)'=LBRYPTR G ST1
 G ST1:'$D(^LBRY(682,LBRYQ,4,Q1,0))
 S LBRYDAT=^LBRY(682,LBRYQ,4,Q1,0)
 D:$P(LBRYDAT,U)="ToC" TOC D:$P(LBRYDAT,U)'="ToC" COPY
 D A2
 G ST1
TOC S PTR=$P(LBRYDAT,U,3),PTRLST=0
TC S PTRLST=$O(^LBRY(681,PTR,2,"AC",PTRLST)) Q:PTRLST=""
 S PTRL2=$O(^LBRY(681,PTR,2,"AC",PTRLST,0)) D HEADER,NAME,TRLR,PAUSE
 G TC
HEADER W @IOF,!!!!!!!!!!,?5,"Veterans Administration ROUTING SLIP"
 W !!,?10,"ROUTE TO:",!
 Q
TRLR W !,?5 F K=1:1:40 W $C(42)
 S TTN=$P(^LBRY(681,PTR,0),U,2),TTT=$P(^LBRY(680,TTN,0),U)
 S TITLE=$E($P(^LBRY(680.5,TTT,0),U),1,39)
 W !,?5,TITLE
 S VID=^LBRY(682,LBRYQ,1),V=$P(VID,U,2),ISSUE=$P(VID,U,3),Y=$P(VID,U) X ^DD("DD")
 S ISSUE=$S(ISSUE]"":"("_ISSUE_")",1:"") S:V]"" ISSUE=V_ISSUE
 W !,?5,Y,?32,ISSUE
 S NUMB=$P(LBRYDAT,U) W:NUMB="ToC" !,?5,"TABLE of CONTENTS ROUTING SLIP"
 W:NUMB=+NUMB !,?5,"COPY ",NUMB," ROUTING SLIP"
 S LBRRSM=$P($G(^LBRY(680.6,LBRYPTR,0)),U,8) S:LBRRSM="" LBRRSM="142D"
 W !!!!!,?5,"FROM LIBRARY, "_LBRRSM,?35 S Y=LBRYD X ^DD("DD") W Y,!
 Q
NAME S NMPTR=$P(^LBRY(681,PTR,2,PTRL2,0),U)
 S NMSVC=$S(NMPTR'="":$G(^LBRY(680.4,NMPTR,0)),1:"")
 S NAME=$P(NMSVC,U),SVC=$P(NMSVC,U,2)
 S MS="" I SVC]"" S SVC=$G(^DIC(49,SVC,0)),MS=$P(SVC,U,8),SVC=$P(SVC,U)
 W !,?5 W:MS]"" MS W ?18,NAME W:SVC]"" "/",SVC W !
 Q
A2 K ^LBRY(682,"A2",LBRYQ,Q1)
 S ^LBRY(682,"A3",LBRYDT,LBRYQ,Q1)="",$P(^LBRY(682,LBRYQ,4,Q1,0),U,6)=LBRYDT
 Q
COPY S PTR=$P(LBRYDAT,U,3),PTRLST=0 D HEADER
CP S PTRLST=$O(^LBRY(681,PTR,2,"AC",PTRLST)) G:PTRLST="" COPY1
 S PTRL2=$O(^LBRY(681,PTR,2,"AC",PTRLST,0)) D NAME
 G CP
COPY1 D TRLR,PAUSE Q
PAUSE Q:IO'=IO(0)!($D(ZTSK))  S XZ="Continue//" D PAUSE^LBRYCK0 Q
EXIT D ^%ZISC K:$D(ZTSK) ^%ZTSK(ZTSK),ZTSK
 K IOP,LBRYQ,Q1,LBRYDAT,LBRYRET,PTR,PTRL2,PTRLST,VID,V,ISSUE,Y,VI,NMPTR
 K NMSVC,NAME,SVC,MS,TTT,TTN,TITLE,LBRRSM
 Q
