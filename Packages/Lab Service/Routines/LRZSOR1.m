LRSOR1 ;SLC/RWF,CJS- SOME SPECIAL OUTPUT ROUTINES ; 6/2/86  8:43 AM ;
 ;;5.1;LAB;;04/11/91 11:06
COAG D DT^LRX K ^UTILITY("LR",$J,"T"),LRTSTS S U="^",LRCW=8,LRSSP=1,LRTP=0,LRSC=0,LRTSTS=0,LRIX=1,LRIX(1)=0
 S LRTEST=LRONETST,LREXPD="S S9=$P(^UTILITY(""LR"",$J,""T"",X),U,5),S8=+$O(^LAB(60,""C"",S9,0)),^UTILITY(""LR"",$J,""TMP"",S9)=^LAB(60,S8,.1),^UTILITY(""LR"",$J,""TMP"",S9,1)=$S($D(^LAB(60,S8,1,LRTP,0)):^(0),1:"""")"
 D ^LREXPD,SPC^LRGEN2,EN2^LRGEN S LRSC=0
 K S9,S8 Q
