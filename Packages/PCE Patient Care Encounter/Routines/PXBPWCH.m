PXBPWCH ;ISL/JVS - PROMPT WHICH LINE ;11/7/96  08:57
 ;;1.0;PCE PATIENT CARE ENCOUNTER;**11,73**;Aug 12, 1996
 ;
 ;
 ;
 ;
 ;
 ;
WHICH ;--When there is more than one entry to select ask which line no.
 N TIMED,PXBUT,DIC,X,N,V
 I '$D(IOEDEOP) D TERM^PXBCC
 S TIMED="I '$T!(DATA[""^"")"
 S N=0,V=0 F I=1:1:Q S N=$O(Q(N)) S $P(V," or ",I)=N
WH W !!,"Select one of the HIGHLIGHTED lines above("_V_"): ",IOELEOL
 R DATA:DTIME
 X TIMED I  S PXBUT=1,DATA="^",EDATA="^" G WFIN
 I DATA?.N1"E".NAP S DATA=" "_DATA
 I DATA?24.N S (DATA,EDATA)=$E(DATA,1,24)
 ;---SPACE BAR---
 I DATA'=" ",DATA'["^",DATA'="" S ^DISV(DUZ,"PXBWH-7")=DATA
 I DATA=" ",$D(^DISV(DUZ,"PXBWH-7")) S DATA=^DISV(DUZ,"PXBWH-7") W DATA
 I DATA["?" G HEL
 I DATA<0 K DATA G WH
 I DATA="" S DATA="???" G HEL
 I DATA>PXBCNT K DATA G WH
 G WFIN
 Q
HEL ;--HELP
 I DATA'?1.3"?" S DATA="???"
 I DATA["?" W IOEDEOP D:DATA="?" HELP^PXBUTL0("WH1") D:DATA="??" HELP^PXBUTL0("WH2") D:DATA="???" HELP^PXBUTL0("WH3") F I=1:1:5 W IOCUU
 G WH
WFIN ;--FINISH UP VARIABLES
 S EDATA=DATA
 ;--need to set the right item to hilighted and restore the others.
 S NLINE=0
 F  S NLINE=$O(Q(NLINE)) Q:NLINE=""  D:NLINE'=DATA REVCPT^PXBCC(NLINE,0)
WHIX ;--EXIT AND CLEANUP
 W IORC,IOEDEOP
 K Q
 Q
