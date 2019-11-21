YTSRAID ;SLC/DJE- ANSWERS SPECIAL HANDLING - WHOQOL-BREF ; 10/16/18 9:35am
 ;;5.01;MENTAL HEALTH;**139**;;Build 134
 ;
 ;Public, Supported ICRs
 ; #2056 - Fileman API - $$GET1^DIQ
 ;
DATA1(SCORE) ;expects YSDATA, returns SCORE, multiple scales so we use nodes i.e. SCORE(SCALEIEN)=###
 ;specialized DATA1 uses SCOREDAT table to map question to score relationships
 N LINE,TEXT,SKIP
 F LINE=1:1 S TEXT=$P($T(SCOREDAT+LINE),";",2) Q:TEXT="QUIT"  D
 .N SCALE,RAWTYPE,QUESTIONS,I
 .S SCALE=$P(TEXT,"|",1) S RAWTYPE=$P(TEXT,"|",2) S QUESTIONS=$P(TEXT,"|",3)
 .F I=1:1:$L(QUESTIONS,U) D
 ..N NODE,DATA,RAW
 ..S NODE=$P(QUESTIONS,U,I)+2 ;YSDATA question nodes start at 3
 ..S DATA=YSDATA(NODE)
 ..;retrieval method section. For each RAWTYPE assign a value to RAW
 ..;typical case, YSDATA piece 3 has the MH CHOICE IEN and raw value is in LEGACY field
 ..I RAWTYPE="LEGACY" S RAW=$$GET1^DIQ(601.75,$P($G(DATA),U,3)_",",4,"I")
 ..;raw score is stored directly in YSDATA piece 3 - trackbars do this.
 ..I RAWTYPE="DIRECT" S RAW=$P($G(DATA),U,3)
 ..I RAW="X" S SKIP(SCALE)=$G(SKIP(SCALE))+1 ;Some reports need to keep count of skipped questions
 ..S SCORE(SCALE)=$G(SCORE(SCALE))+RAW
 ;
 ;add logic to handle skipped questions.
 ;
 ;add logic to handle transformed scores
 ;
 Q
 ;
 ;SCOREDAT maps questions to their scale and to the method we use to retrieve the raw value. 
 ;A scale can have multiple lines and does not need to match the scalegroup order
 ;You can create your own RAW RETRIEVAL METHOD, just make sure we handle the case in DATA1
SCOREDAT ; SCALE IEN|RAW RETRIEVAL METHOD|QUES#^QUES#...
 ;1319|LEGACY|3^4^5^6^7^8^9^10^11^12^13^14^15^16^17^18^19^20
 ;QUIT
 Q
 ;
SCORESV(SCORE) ;Expects SCORE to be in format SCORE(SCALE_IEN)=###.  Also expects ^TMP($J,"YSG")
 N YSCORNODE,YSGNODE
 I $D(^TMP($J,"YSG",1)),^TMP($J,"YSG",1)="[ERROR]" D  Q  ;-->out
 .K ^TMP($J,"YSCOR")
 .S ^TMP($J,"YSCOR",1)="[ERROR]"
 .S ^TMP($J,"YSCOR",2)="No Scale found for ADMIN"
 ;
 K ^TMP($J,"YSCOR")
 S ^TMP($J,"YSCOR",1)="[DATA]"
 S YSCORNODE=2
 S YSGNODE=2 F  S YSGNODE=$O(^TMP($J,"YSG",YSGNODE)) Q:YSGNODE=""  D
 .N SCALEIEN
 .I $E(^TMP($J,"YSG",YSGNODE),1,5)'="Scale" Q  ;only read the lines for scales
 .S SCALEIEN=+$P(^TMP($J,"YSG",YSGNODE),"=",2) ;grab the first number after "=" sign
 .S ^TMP($J,"YSCOR",YSCORNODE)=$$GET1^DIQ(601.87,SCALEIEN_",",3,"I")_"="_SCORE(SCALEIEN)
 .S YSCORNODE=YSCORNODE+1
 ;
 Q
 ;
DLLSTR(YSDATA,YS,YSTRNG) ;
 ;  YSTRNG = 1 Score Instrument
 ;  YSTRNG = 2 get Report Answers and Text
 N SCORE,TSARR
 ;
 I YSTRNG=1 D DATA1(.SCORE),SCORESV(.SCORE)
 ;LDSCORES is not retrieving t-scores for TMP($J,"YSCOR") so we need to run LDTSCOR.
 I YSTRNG=2 D LDTSCOR^YTSCORE(.TSARR,YS("AD")),BUILDANS(.TSARR,.YSDATA)
 Q
 ;
BUILDANS(TSARR,YSDATA) ;
 N SCORE,I
 ;
 I YSDATA(4)="8381^2^1156" S YSDATA(4)="8381^2;1^" ;check if other field has "not asked" mh choice entry and replace with blank text
 S YSDATA($O(YSDATA(""),-1)+1)=999999999999_U_U_$P(TSARR("RAID Total Score"),U,2)
 ;logic to add special answers to YSDATA
 ;
 Q
 ;
