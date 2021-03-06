PSONEW1 ;BIR/DSD - new Rx order entry ;10/17/92
 ;;7.0;OUTPATIENT PHARMACY;**46,104,117,143**;DEC 1997
 ;External reference ^PS(55 supported by DBIA 2228
 ;
START ;
 S (PSONEW("DFLG"),PSONEW("FIELD"),PSONEW1)=0
 ;
1 S PSONEW("FLD")=1 S PSONEW("FIELD")=0
 I $P($G(PSOPAR),"^",7)'=1 D MANUAL^PSONRXN ; Get Manual Rx number
 G:PSONEW("QFLG")!PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
2 S PSONEW("FLD")=2 D PTSTAT^PSODIR1(.PSONEW) ; Get Patient Status
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
3 S PSONEW("FLD")=3 D ^PSODRG  ; Get drug and related information
 G:PSONEW("DFLG") END D EN^PSODIAG  ; get ICD diagnosis codes for order
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
31 S PSONEW("FLD")=31 D DOSE^PSODIR(.PSONEW) ; Get Dosing
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
32 I '$G(PSONEW("ENT")) W !,"Incomplete Dosaging Data!",! K DIRUT G 31
 S PSONEW("FLD")=32 D INS^PSODIR(.PSONEW) ; Get Patient Instructions
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 I $P($G(^PS(55,PSODFN,"LAN")),"^") D SINS^PSODIR(.PSONEW)
 ;
4 D EN^PSOFSIG(.PSONEW) I $O(SIG(0)) S SIGOK=1
 ;S PSONEW("FLD")=4 D SIG^PSODIR1(.PSONEW) ; Get Rx directions
 ;G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
7 S PSONEW("FLD")=7 D DAYS^PSODIR1(.PSONEW) ; Get days supply
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
5 S PSONEW("FLD")=5 D QTY^PSODIR1(.PSONEW) ; Get quantity
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
6 I $P($G(PSOPAR),"^",15) S PSONEW("FLD")=6 D COPIES^PSODIR1(.PSONEW) ; Get label copies
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
8 S PSONEW("FLD")=8 D REFILL^PSODIR1(.PSONEW) ; Get # of refills
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
9 S PSONEW("FLD")=9 D PROV^PSODIR(.PSONEW) ; Get Provider
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 G:$G(DUZ("AG"))'="I" 11
 ;
10 Q:$G(DUZ("AG"))'="I"  S PSONEW("FLD")=10 D EXP^PSODIR2(.PSONEW) ; Get Expiration Date - Indian Health Service ONLY
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
11 S PSONEW("FLD")=11 D CLINIC^PSODIR2(.PSONEW) ; Get Clinic
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
12 S PSONEW("FLD")=12 D MW^PSODIR2(.PSONEW) ; Get Mail/Window Info
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
13 S PSONEW("FLD")=13 D RMK^PSODIR2(.PSONEW) ; Get Remarks
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
14 S PSONEW("FLD")=14 D ISSDT^PSODIR2(.PSONEW) ; Get Issue Date
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
15 S PSONEW("FLD")=15 D FILLDT^PSODIR2(.PSONEW) ; Get Fill date
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
16 S PSONEW("FLD")=16 D CLERK^PSODIR2(.PSONEW) ; Get Clerk Code
 G:PSONEW("DFLG") END G:PSONEW("FIELD") @PSONEW("FIELD")
 ;
END ;
 K PSONEW1
 Q
 ;
JUMP ;
 S PSONEW("FIELD")=$S(+Y=.01:1,+Y=3:2,+Y=6:3,+Y=10:4,+Y=7:5,+Y=10.6:6,+Y=8:7,+Y=9:8,+Y=4:9,+Y=29:10,+Y=5:11,+Y=11:12,+Y=12:13,+Y=1:14,+Y=22:15,+Y=16:16,+Y=113:31,+Y=114:32,1:PSONEW("FLD"))
 I PSONEW("FIELD")>PSONEW("FLD") W !,$C(7),"Cannot jump ahead ..",! S PSONEW("FIELD")=PSONEW("FLD")
 Q
