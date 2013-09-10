A1AXFS2 ;SLL/ALB ISC; RD FACILITY STATISTICS (LAST REVIEW DATE); 1/20/88
 ;;VERSION 1.0
 ;
 S $P(A1AXL,"=",IOM+1)=""
 S A1AXD="" F I=0:0 S A1AXD=$O(^UTILITY($J,"ST",A1AXD)) Q:A1AXD=""  D HD,PRINT G:$D(A1AXEND) EXIT
 G EXIT
HD ;
 S X="STATUS AND CONTINGENCY REPORT  " S:A1AXOPT="F" X=X_"BY FACILITY" S:A1AXOPT="O" X=X_"BY ORGANIZATION" W #,!!,?(IOM-$L(X)\2),X
 S %DT=+$H D %CDS^%H S Y=$P(%DAT1,"-",2)_" "_$P(%DAT1,"-",1)_",19"_$P(%DAT1,"-",3)
 W ?(IOM-$L(Y)),Y
 S X=$P(^DIZ(11833,1,0),"^",1)_" - DISTRICT "_A1AXD W !,?(IOM-$L(X)\2),X
 W !,A1AXL
 D FLAG^A1AXUTL W !,"CONTG-  Sum of C, U and RC",!,A1AXL
 W:A1AXOPT="F" !,"VAMC",?27,"REVIEW" W:A1AXOPT="O" !,"REVIEW",?10,"VAMC"
 W ?37,"TOTAL",?45,"TOTAL",?53,"TOTAL",?61,"TOTAL",?69,"TOTAL",?77,"TOTAL",?85,"TOTAL",?93,"TOTAL",?101,"TOTAL",?108,"TOTAL",?115,"TOTAL",?122,"REVIEW"
 W:A1AXOPT="F" !,"FAC",?27,"ORG" W:A1AXOPT="O" !,"ORG",?10,"FAC"
 W ?37,"RECOM",?45,"COMPL",?53,"IN PRO",?61,"CANCEL",?69,"NON-CC",?77,"+",?85,"R",?93,"CONTG",?101,"C",?108,"U",?115,"RC",?122,"DATE"
 W !,A1AXL
 Q
PRINT ;
 S A1AX1="" F I=0:0 Q:$D(A1AXEND)  S A1AX1=$O(^UTILITY($J,"ST",A1AXD,A1AX1)) W !,A1AXL Q:A1AX1=""  S A1AX2="" F J=0:0 Q:$D(A1AXEND)  S A1AX2=$O(^UTILITY($J,"ST",A1AXD,A1AX1,A1AX2)) Q:A1AX2=""  D PR
 Q
PR ;
 I A1AXOPT="F" S A1AXDDT=^UTILITY($J,"LAST",A1AX1,A1AX2)
 I A1AXOPT="O" S A1AXDDT=^UTILITY($J,"LAST",A1AX2,A1AX1)
 S:A1AXDDT=0 A1AXDT=""
 S:A1AXDDT>0 A1AXDT=$E(A1AXDDT,4,5)_"/"_$E(A1AXDDT,6,7)_"/"_$E(A1AXDDT,2,3) K A1AXDDT
 D:$Y+3>IOSL WAIT Q:$D(A1AXEND)
 W !
 I A1AXOPT="O" G PRO
 I DUZ(2)>999 W A1AX1
 I DUZ(2)<1000 W:A1AX1=$P(^DIZ(11837,DUZ(2),0),"^",1) A1AX1
 W ?27,A1AX2 G WR
PRO ;
 W A1AX1
 I $D(^DIZ(11837,DUZ(2),0)) W:A1AX2=$P(^(0),"^",1) ?10,A1AX2
 I DUZ(2)>999 W ?10,A1AX2
WR ;
 W ?37,$P(^UTILITY($J,"ST",A1AXD,A1AX1,A1AX2),"^",1),?45,$P(^(A1AX2),"^",2),?53,$P(^(A1AX2),"^",3),?61,$P(^(A1AX2),"^",4),?69,$P(^(A1AX2),"^",5)
 S A1AXO=$S(A1AXOPT="F":A1AX2,A1AXOPT="O":A1AX1)
 ;G:A1AXO'="JCAHO"&(A1AXO'="SPG")&(A1AXO'="OTHER") WDT
 W ?77,$P(^UTILITY($J,"CCT",A1AXD,A1AX1,A1AX2),"^",1),?85,$P(^(A1AX2),"^",2),?93,$P(^(A1AX2),"^",3),?101,$P(^(A1AX2),"^",4),?108,$P(^(A1AX2),"^",5),?115,$P(^(A1AX2),"^",6)
WDT ;
 W ?122,A1AXDT
 Q
WAIT ;
 I IOST["C-" R !!,"PRESS '^' TO STOP ",A1AXSTOP:DTIME S:A1AXSTOP["^" A1AXEND=""
 D:'$D(A1AXEND) HD
 Q
EXIT ;
 D EXIT1,CLOSE1^A1AXUTL Q
EXIT1 K AU,AA,AP,AS,AR,II,A1AXF,A1AXO,A1AXL,%DT("A"),%DT("B")
 K I,J,A1AXOPT,A1AXDT,A1AXCT,A1AX1,A1AX2,A1AXST,A1AXSTOP,A1AXEND,A1AXM,A1AXD,^UTILITY($J,"ST"),^UTILITY($J,"CCT")
 K ^UTILITY($J,"LAST")
 K %DAT,%DAT1,%DT,AN,D,POP,X,Y
 Q
