LBRYCK1 ;ISC2/DJM-SERIALS CHECK-IN OUTPUT MESSAGE ;[ 05/23/97  12:13 PM ]
 ;;2.5;Library;**2,13**;Mar 11, 1996
START F I=1:1:7 S LS(I)=""
 S XT1=$S($D(^LBRY(680,LBRYLOC,16,0)):1,1:0),XT2=$S($D(A(E0-1)):1,1:0)
 S XT3=$S($D(A(E1+1)):1,1:0),XT4=$S($D(A(1)):1,1:0),LS(1)="Choose: "
 I XT4 S LS(2)=$C(34)_"ID NUM"_$C(34)_" to check-in",LS(4)="(E)dit"
 S LS(3)="(I)nsert" S:XT1 LS(5)="see check-in (N)otes" S:XT2 LS(6)="(B)ackup" S:XT3 LS(7)="(F)orward"
 S (LINE1,LINE2)="" F I=1:1:7 Q:$L(LINE1)+$L(LS(I))'<78  S:LS(I)]"" LINE1=LINE1_LS(I) D LS2 K LS(I) I I>2&($D(LS(I+1))) S:LS(I+1)]"" LINE1=LINE1_", "
 I '$D(LS(7)) S LINE1=LINE1_"." G PRINT
 F J=I:1:7 S:LS(J)]"" LINE2=LINE2_LS(J) K LS(J) I J<7&($D(LS(J+1))) S:LS(J+1)]"" LINE2=LINE2_", "
 S LINE2=LINE2_"."
PRINT W !!,LINE1,! W:$D(LINE2) LINE2,! W "Exit// "
EXIT K LINE1,LINE2,I,J
 Q
LS2 I I=2,$D(LS(2)) I LS(2)]"" S LINE1=LINE1_", "
 Q
START1 ;LIBRARY ROUTING QUEUEING FOR "ALL" COPIES
 S LBRYC=0 D NOW^%DTC S LBRYD=%\1
ST S LBRYC=$O(^LBRY(682,A(E),4,LBRYC)) G:LBRYC'>0 QUEUE
 S LBXX=^LBRY(682,A(E),4,LBRYC,0) I $P(LBXX,U,2)>3 D LOCK G:LBRYL=0 FINI
 D:$P(LBXX,U,1)'="ToC" NUMB D:$P(LBXX,U,1)="ToC" TOC
 G ST
NUMB S LBC=$P(LBXX,U,3),LBC=$P(^LBRY(681,LBC,1),U,6)
 S $P(^LBRY(682,A(E),4,LBRYC,0),U,2)=LBC,$P(^LBRY(682,A(E),1),U,7)=LBRYD
 W !,"c",$P(LBXX,U,1)," RECEIVED." D COMP
 I $G(LBRYPTR)="" D ^LBRYASK I $G(LBRYPTR)="" G FINI
 I "12"[LBC,$P(^LBRY(680.6,LBRYPTR,0),U,5)="Y" W "  Queued to printer ",$P(^(0),U,3),"."
 L  Q
TOC S $P(^LBRY(682,A(E),4,LBRYC,0),U,2)=1,LBC=1
 W !,"ToC ROUTING LIST" W:$P(^LBRY(680.6,LBRYPTR,0),U,5)="Y" " queued to printer ",$P(^(0),U,3),"."
 D COMP
 L  Q
COMP I "12"[LBC,$P(LBXX,U,6)="" D
 . S ^LBRY(682,"A2",A(E),LBRYC)="",^LBRY(682,"A4",LBRYD,LBRYCLS,A(E),LBRYC)=""
 I LBC<4 S $P(^LBRY(682,A(E),4,LBRYC,0),U,7)=LBRYD,$P(^(0),U,8)=DUZ,$P(^LBRY(682,A(E),1),U,7)=LBRYD
 I $P(LBXX,U,1)'="ToC",$P(LBXX,U,2)=4 S $P(^LBRY(682,A(E),1),U,4)=$P(^LBRY(682,A(E),1),U,4)+1
 Q
LOCK S DIC="^LBRY(682,A(E),4,",DA=LBRYC D LOCK^LBRYEDR Q
QUEUE W !!,"Check-in completed." S LTST=$O(^LBRY(682,"A2",0)) G:LTST="" FINI
 S QUEUE=^LBRY(680.6,LBRYPTR,0),TERM=$P(QUEUE,U,3),QUEUE=$P(QUEUE,U,5)
 I QUEUE="Y",TERM]"" S ZTIO=TERM,ZTDTH=$H G QUEUE1
 S %ZIS="MQ",%IS("B")=$S(TERM]"":TERM,1:"") K IO("Q")
 D ^%ZIS G:POP FINI I '$D(IO("Q")) U IO D ^LBRYCK2 D ^%ZISC G FINI
QUEUE1 S ZTRTN="^LBRYCK2",ZTSAVE("LBRYPTR")="",ZTDESC="LBRY CHECK-IN REPORT"
 D ^%ZTLOAD D ^%ZISC K ZTSK
FINI S XZ="EXIT//" D PAUSE^LBRYCK0
 K CKIN,LBRYC,LBXX,LBC,%IS("B")
 G ^LBRYCK
ASK3 ;LIBRARY SERIALS WHAT-TO-DO PROMPT (FROM LBRYCK)
 S DTOUT=0 R X:DTIME E  W $C(7) S DTOUT=1 G ^LBRYCK
 I X="" G ^LBRYCK
 I X=" " S:$D(^TMP("LBRY",DUZ,1)) X=^(1)
 I X="??" S XQH="LBRY CHECK-IN EDIT ??" D EN^XQH G DISPLAY^LBRYCK
 I X="^" G ^LBRYCK
 I $D(A(E0-1)),"Bb"[$E(X,1) D BACKUP^LBRYCK0 G DISPLAY^LBRYCK
 I $D(A(E1+1)),"Ff"[$E(X,1) D FORWARD^LBRYCK0 G DISPLAY^LBRYCK
 I $D(^LBRY(680,DA,16,0)),"Nn"[X D UTIL,^LBRYCK5 G DISPLAY^LBRYCK
 I $D(A(1)),"Ee"[$E(X,1) D UTIL G EDIT^LBRYCK0
 I "Ii"[X D UTIL G ENEDT^LBRYCK0
ASK2 I $D(A(X)) D UTIL G ^LBRYCK3
WRONG S E=0,XTA="",XTB="",XTC="",XTD="",XTE=""
 S:$D(A(1)) XTA="a number under heading "_$C(34)_"ID NUM"_$C(34)_", E"
 S XTB=$S(XT4&('XT1)&('XT2)&'(XT3):" or I.",'XT4&('XT1)&('XT2)&('XT3):"I.",1:", I") G:XTB["." WRONG1 S XTC=$S(XT1:", N",1:""),XTD=$S(XT2&(XT3):", B or F.",XT2!(XT3):" or ",1:".") G:XTD["." WRONG1
 S XTE=$S(XT2:"B.",XT3:"F.",1:"")
WRONG1 W !!,"Enter "_XTA_XTB_XTC_XTD_XTE,!,"Enter '??' for more help."
 W !!,"Choose: Exit// " D MORE^LBRYCK G ASK3
UTIL K ^TMP("LBRY",DUZ,1) S ^(1)=X Q
