LAZPMAX ;SLC/RWF - PARAMAX  ;7/20/90  10:01 ; [ 10/15/96  1:40 PM ]
 ;;5.2;AUTOMATED LAB INSTRUMENTS;;Sep 27, 1994
 ;CROSS LINK BY ID (BARCODE #) OR IDE (ACCESSION #)
LA1 S:$D(ZTQUEUED) ZTREQ="@" S LANM=$T(+0),TSK=$O(^LAB(62.4,"C",LANM,0)) Q:TSK<1
 Q:'$D(^LA(TSK,"I",0))
 K LATOP D ^LASET Q:'TSK  S X="TRAP^"_LANM,@^%ZOSF("TRAP"),TRAY=1
LA2 K TV S TOUT=0 D IN G QUIT:TOUT,LA2:$E(IN,1)'="#",LA2:IN'["="
 S (CUP,IDE)=+$P($P(IN,"#",2),"$"),ID=+$P($P(IN,"$",2),","),IN=$P($P(IN,",",2,99),"%")
 F PIECE=1:1 S LUMP=$P(IN,",",PIECE) Q:'$L(LUMP)  S TEST=+$P(LUMP,"="),V=$P($P(LUMP,"=",2),"("),STATUS=+$P(LUMP,"(",2) I $D(TC(TEST)),'STATUS S @TC(TEST,1)=V
LA3 X LAGEN G LA2:'ISQN
 F I=0:0 S I=$O(TV(I)) Q:I<1  S:TV(I,1)]"" ^LAH(LWL,1,ISQN,I)=TV(I,1)
 G LA2
NUM S X="" F JJ=1:1:$L(V) S:$A(V,JJ)>32 X=X_$E(V,JJ)
 S V=X Q
IN S CNT=^LA(TSK,"I",0)+1 IF '$D(^(CNT)) S TOUT=TOUT+1 Q:TOUT>9  H 5 G IN
 S ^LA(TSK,"I",0)=CNT,IN=^(CNT),TOUT=0
 S:IN["~" CTRL=$P(IN,"~",2),IN=$P(IN,"~",1)
 Q
QUIT G LA2:^LA(TSK,"I")>^("I",0)
 I $D(^LA(TSK,"O",0)) G QUIT:^LA(TSK,"O")>^("O",0)
 L ^LA(TSK) H 1 K ^LA(TSK),^LA("LOCK",TSK)
 Q
TRAP D ^LABERR S T=TSK D SET^LAB G @("LA2^"_LANM) ;ERROR TRAP
