ICDTBL0B ;ALB/EG/MRY - GROUPER UTILITY FUNCTIONS ; 11/5/07 11:07am
 ;;18.0;DRG Grouper;**45,46**;Oct 20, 2000;Build 5
DRG1 ;line tag for 001,002
DRG2 ;
 S ICDRG=$S(ICDMCC=2:1,1:2) Q
DRG3 ;S ICDRG=3 Q
DRG4 I ICDOR["O" S ICDRG=3 Q
 S ICDRG=$S(ICDMCC=1:3,1:4) Q
DRG5 ;line tag for 005,006
DRG6 S ICDRG=$S(ICDMCC=2:5,1:6) D  Q
 . I $D(ICDOP(" 46.97")) S ICDRG=5
DRG7 S ICDRG=7 Q
DRG8 S ICDRG=8 Q
DRG9 S ICDRG=9 Q
DRG10 S ICDRG=10 Q
DRG11 ;line tag for 011,012,013
DRG12 ;
DRG13 ;
 S ICDRG=$S(ICDMCC=2:11,ICDMCC=1:12,1:13) Q
DRG20 ;line tag for 020,021,022
DRG21 ;
DRG22 ;
 S ICDRG=$S((ICDPD["K")&(ICDOR["K")&(ICDMCC=2):20,(ICDPD["K")&(ICDOR["K")&(ICDMCC=1):21,(ICDPD["K")&(ICDOR["K"):22,1:ICDRG) Q
DRG23 ;line tag for 023,024
DRG24 ;
 I ICDPD["k"!(ICDSD["k") D DRG97 Q
 S ICDRG=$S($D(ICDOP(" 00.10")):23,1:ICDRG) Q:(ICDRG=23)
 S ICDRG=$S((ICDPD["Q")&(ICDOR["Q")&(ICDMCC=2):23,(ICDPD["Q")&(ICDOR["Q"):24,1:ICDRG) Q:(ICDRG=23)
 S ICDRG=$S($D(ICDOP(" 02.93"))&($D(ICDOP(" 86.95")))&(ICDMCC=2):23,$D(ICDOP(" 02.93"))&($D(ICDOP(" 86.95"))):24,1:ICDRG)
 Q
DRG25 ;line tag for 025,026,027
DRG26 ;
DRG27 ;
 I ICDMAJ["B" D DRG28 Q
 D DRG20 D:'((ICDRG>19)&(ICDRG<23)) DRG23 D:'((ICDRG>22)&(ICDRG<25)) DRG31
 S ICDRG=$S(ICDRG=20:20,ICDRG=21:21,ICDRG=22:22,ICDRG=23:23,ICDRG=24:24,ICDRG=31:31,ICDRG=32:32,ICDRG=33:33,ICDMCC=2:25,ICDMCC=1:26,1:27)
 I $D(ICDOP(" 00.62")),'$D(ICDOP(" 00.65")) S ICDRG=999
 Q
DRG28 ;line tag for 028,029,030
DRG29 ;
DRG30 ;S ICDRG=$S((ICDMAJ["B")&ICDMCC=2:28,(ICDMAJ["B")&ICDMCC=1:29,ICDMAJ["B":30,1:ICDRG) D  Q
 S ICDRG=$S(ICDMCC=2:28,ICDMCC=1:29,1:30) D  Q
 . I $D(ICDOP(" 03.93")),$D(ICDOP(" 86.94")) S ICDRG=29
 . I $D(ICDOP(" 03.93")),$D(ICDOP(" 86.95")) S ICDRG=29
 . I $D(ICDOP(" 03.93")),$D(ICDOP(" 86.97")) S ICDRG=29
 . I $D(ICDOP(" 03.93")),$D(ICDOP(" 86.98")) S ICDRG=29
 . Q
DRG31 ;line tag for 031,032,033
DRG32 ;
DRG33 ;
  S ICDRG=$S((ICDOR["S")&(ICDMCC=2):31,(ICDOR["S")&(ICDMCC=1):32,ICDOR["S":33,1:ICDRG)  Q
DRG34 ;line tag for 034,035,036
DRG35 ;
DRG36 ;
 S ICDRG=$S(ICDMCC=2:34,ICDMCC=1:35,1:36) Q
DRG37 ;line tag for 037,038,039
DRG38 ;
DRG39 ;
 S ICDRG=$S(ICDMCC=2:37,ICDMCC=1:38,1:39) D  Q
 . I $D(ICDOP(" 00.61"))&($D(ICDOP(" 00.63")))&(ICDMCC=2) S ICDRG=34
 . I $D(ICDOP(" 00.61"))&($D(ICDOP(" 00.63")))&(ICDMCC=1) S ICDRG=35
 . I $D(ICDOP(" 00.61"))&($D(ICDOP(" 00.63"))) S ICDRG=36
DRG40 ;line tag for 040,041,042
DRG41 ;
DRG42 ;
 S ICDRG=$S(ICDMCC=2:40,ICDMCC=1:41,1:42) D  Q
 . I $D(ICDOP(" 04.92")),$D(ICOP(" 86.94")) S ICDRG=41
 . I $D(ICDOP(" 04.92")),$D(ICOP(" 86.95")) S ICDRG=41
 . I $D(ICDOP(" 04.92")),$D(ICOP(" 86.97")) S ICDRG=41
 . I $D(ICDOP(" 04.92")),$D(ICOP(" 86.98")) S ICDRG=41
DRG52 ;line tag for 052,053
DRG53 ;
 S ICDRG=$S(ICDMCC>0:52,1:53) Q
DRG54 ;line tag for 054,055
DRG55 ;
 S ICDRG=$S(ICDMCC=2:54,1:55) Q
DRG56 ;line taf for 056,057
DRG57 ;
 S ICDRG=$S(ICDMCC=2:56,1:57) Q
DRG58 ;line tag for 058,059,060
DRG59 ;
DRG60 ;
 S ICDRG=$S(ICDMCC=2:58,ICDMCC=1:59,1:60) Q
DRG61 ;line tag for 061,062,063
DRG62 ;
DRG63 ;
 S ICDRG=$S(ICDMCC=2:61,ICDMCC=1:62,1:63) Q
DRG64 ;line tag for 064,065,066
DRG65 ;
DRG66 ;
 I ICDOR="" S ICDRG=$S(ICDMCC=2:64,ICDMCC=1:65,1:66) Q
 S ICDRG=$S(ICDOR["O"&(ICDMCC)=2:64,ICDOR["O"&(ICDMCC=1):65,1:66) Q 
DRG67 ;line tag for 067,068
DRG68 ;
 S ICDRG=$S(ICDMCC=2:67,1:68) Q
DRG69 S ICDRG=69 Q
DRG70 ;line tag for 070,071,072
DRG71 ;
DRG72 ;
 S ICDRG=$S(ICDMCC=2:70,ICDMCC=1:71,1:72) Q
DRG73 ;line tag for 073,074
DRG74 ;
 S ICDRG=$S(ICDMCC=2:73,1:74) Q
DRG75 ;line tag for 075,076
DRG76 ;
 S ICDRG=$S(ICDMCC>0:75,1:76) Q
DRG77 ;line tag for 077,078,079
DRG78 ;
DRG79 ;
 S ICDRG=$S(ICDMCC=2:77,ICDMCC=1:78,1:79) Q
DRG80 ;line tag for 080,081
DRG81 ;
 S ICDRG=$S(ICDMCC=2:80,1:81) Q
DRG82 ;line tag for 082,083,084
DRG83 ;
DRG84 ;
 I ICDSD'["1" D DRG85 Q
 S ICDRG=$S(ICDMCC=2:82,ICDMCC=1:83,1:84) Q
DRG85 ;line tag for 085,086,087
DRG86 ;
DRG87 ;
 S ICDRG=$S(ICDMCC=2:85,ICDMCC=1:86,1:87) Q
DRG88 ;line tag for 088,089,090
DRG89 ;
DRG90 ;
 S ICDRG=$S(ICDMCC=2:88,ICDMCC=1:89,1:90) Q
DRG91 ;line tag for 091,092,093
DRG92 ;
DRG93 ;
 S ICDRG=$S(ICDMCC=2:91,ICDMCC=1:92,1:93) Q
DRG94 ;line tag for 094,095,096
DRG95 ;
DRG96 ;
 S ICDRG=$S(ICDMCC=2:94,ICDMCC=1:95,1:96) Q
DRG97 ;line tag for 097,098,099
DRG98 ;
DRG99 ;
 S ICDRG=$S(ICDMCC=2:97,ICDMCC=1:98,1:99) Q
 Q
