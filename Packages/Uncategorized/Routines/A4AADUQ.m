A4AADUQ ;HINES-CIOFO/JJM - CLASS III AR REPORTS 7/29/99 19:45
V ;;1.0;CLASS III AUDIT DATA UTILITIES 1/14/99
 ; QUEUE ALL AUDIT DATA REPORTS
BYALL ;check for regular run or TaskMan
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 I $D(ZTSK) S A4ADEV=ION_";"_IOST_";"_IOM_";"_IOSL D ALL^A4AADUQ K ZTSK Q
 ;
OPENALL ;check device for queueing or local
 S ZTRTN="ALL^A4AADUQ",%ZIS="QM",%ZIS("B")="" D ^%ZIS G:POP CLOSEDV S (IOP,A4ADEV)=ION_";"_IOST_";"_IOM_";"_IOSL,A4AIOSV=IO(0)
 I IO=IO(0) D @ZTRTN G CLOSEDV
 I $D(IO("Q")) D
   . S ZTDESC="CLASS III AUDIT FILE REPORTS"
  . D ^%ZTLOAD
 E  U IO D @ZTRTN
 W:$D(ZTSK) !,"TASK NUMBER ",ZTSK
 D CLOSEDV
 QUIT
ALL ;
 D RPT1
 D RPT2
 D RPT3
 D RPT4
 D RPT5
 D RPT6
 D RPT7
 D RPT8
 D RPT9
 D RPT10
 D RPT11
 QUIT
RPT1 ; REPORT 1
 W ?30,"REPORT 1",!!
 D BCHK1^A4AADP1
 W !,"END OF REPORT 1",#
 QUIT
RPT2 ; REPORT 2
 W ?30,"REPORT 2",!!
 D BY430^A4AADP5
 W !,"END OF REPORT 2",#
 QUIT
RPT3 ; REPORT 3
 W ?30,"REPORT 3",!!
 D BYCXREF^A4AADP5
 W !,"END OF REPORT 3",#
 QUIT
RPT4 ; REPORT 4
 W ?30,"REPORT 4",!!
 D BYASXR^A4AADP5
 W !,"END OF REPORT 4",#
 QUIT
RPT5 ; REPORT 5
 W ?30,"REPORT 5",!!
 D BY433^A4AADP5
 W !,"END OF REPORT 5",#
 QUIT
RPT6 ; REPORT 6
 W ?30,"REPORT 6",!!
 D AT433^A4AADP5
 W !,"END OF REPORT 6",#
 QUIT
RPT7 ; REPORT 7
 W ?30,"REPORT 7",!!
 D ATD433^A4AADP5
 W !,"END OF REPORT 7",#
 QUIT
RPT8 ; REPORT 8
 W ?30,"REPORT 8",!!
 D ^A4AA430
 W !,"END OF REPORT 8",#
 QUIT
RPT9 ; REPORT 9
 W ?30,"REPORT 9",!!
 D ^A4AA433
 W !,"END OF REPORT 9",#
 QUIT
RPT10 ; REPORT 10
 W ?30,"REPORT 10",!!
 D ^A4AA350
 W !,"END OF REPORT 10",#
 QUIT
RPT11 ; REPORT 11
 W ?30,"REPORT 11",!!
 D ^A4AA399
 W !,"END OF REPORT 11"
 QUIT
REPORT1 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="BCHK1^A4AADP1"
 S ZTDESC="CLASS III AUDIT FILE REPORT 1"
 D CHKDEV
 Q
REPORT2 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="BY430^A4AADP5"
 S ZTDESC="CLASS III AUDIT FILE REPORT 2"
 D CHKDEV
 Q
REPORT8 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="^A4AA430"
 S ZTDESC="CLASS III AUDIT FILE REPORT 8"
 D CHKDEV
 Q
REPORT9 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="^A4AA433"
 S ZTDESC="CLASS III AUDIT FILE REPORT 9"
 D CHKDEV
 Q
REPORT10 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="^A4AA350"
 S ZTDESC="CLASS III AUDIT FILE REPORT 10"
 D CHKDEV
 Q
REPORT11 ;
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="^A4AA399"
 S ZTDESC="CLASS III AUDIT FILE REPORT 11"
 D CHKDEV
 Q
PRTBILL ;
 D GETBILL^A4AADP1
 Q:'BILL
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="PBILL1^A4AARPT1"
 S ZTDESC="CLASS III AUDIT BILL"
 S ZTSAVE("BILL")=""
 D CHKDEV
 Q
CHKBILLS ; CHECK BILLS
 N A4ADEV,POP,%ZIS,ZTRTN,ZTSAVE,A4AIOSV K IOP
 S ZTRTN="^A4AARPT4"
 S ZTDESC="CLASS III CHECK BALANCE OF BILLS"
 D CHKDEV
 Q
 ;
CHKDEV ;check device for queueing or local
 S %ZIS="QM",%ZIS("B")="" D ^%ZIS G:POP CLOSEDV S (IOP,A4ADEV)=ION_";"_IOST_";"_IOM_";"_IOSL,A4AIOSV=IO(0)
 I IO=IO(0) D @ZTRTN G CLOSEDV
 I $D(IO("Q")) D
  . D ^%ZTLOAD
 E  U IO D @ZTRTN
 W:$D(ZTSK) !,"TASK NUMBER ",ZTSK
 D CLOSEDV
 QUIT
 ;
CLOSEDV ;close device and exit routine
 W:IO'=IO(0) @IOF D ^%ZISC I '$D(ZTSK) S:$D(A4AIOSV) IO(0)=A4AIOSV U IO(0)
 K X,Y,ZTSK
