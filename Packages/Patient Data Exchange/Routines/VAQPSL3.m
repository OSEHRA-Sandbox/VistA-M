VAQPSL3 ;ALB/List Manager - INITIALIZE ENTRIES IN LIST TEMPLATE FILE; 25-OCT-1993
 ;;1.5;PATIENT DATA EXCHANGE;;NOV 17, 1993
 W !,"'VAQ PROCESS PDX4' List Template..."
 S DA=$O(^SD(409.61,"B","VAQ PROCESS PDX4",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="VAQ PROCESS PDX4" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="VAQ PROCESS PDX4^1^^^6^16^1^1^Entry^VAQ PDX4 (MENU)^PDX V1.5 - MANUAL RELEASE^1"
 .S ^SD(409.61,VALM,1)="^VALM HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""VAQR4"",$J)"
 .S ^SD(409.61,VALM,"COL",0)="^409.621^5^5"
 .S ^SD(409.61,VALM,"COL",1,0)="ENTRY^2^7^Entry"
 .S ^SD(409.61,VALM,"COL",2,0)="LOCAL PATIENT NAME^9^30^Local Patient Name"
 .S ^SD(409.61,VALM,"COL",3,0)="DOB^41^11^DOB"
 .S ^SD(409.61,VALM,"COL",4,0)="SSN^54^11^SSN"
 .S ^SD(409.61,VALM,"COL",5,0)="PID^67^11^PID"
 .S ^SD(409.61,VALM,"EXP")="D EXP^VAQEXT02"
 .S ^SD(409.61,VALM,"FNL")="D EXIT^VAQEXT02"
 .S ^SD(409.61,VALM,"HDR")="D HD^VAQEXT02"
 .S ^SD(409.61,VALM,"HLP")="S X=""?"" D DISP^XQORM1 W !!"
 .S ^SD(409.61,VALM,"INIT")="D INIT^VAQEXT02"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK
 .W "Filed."
 ;
 W !,"'VAQ REQUEST PDX2' List Template..."
 S DA=$O(^SD(409.61,"B","VAQ REQUEST PDX2",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="VAQ REQUEST PDX2" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="VAQ REQUEST PDX2^1^^^5^16^1^1^Entry^VAQ PDX2 (MENU)^PDX V1.5 - REQUEST^1"
 .S ^SD(409.61,VALM,1)="^VALM HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""VAQR2"",$J)"
 .S ^SD(409.61,VALM,"COL",0)="^409.621^3^3"
 .S ^SD(409.61,VALM,"COL",1,0)="ENTRY^2^5^Entry"
 .S ^SD(409.61,VALM,"COL",2,0)="DOMAIN^9^25^Domain"
 .S ^SD(409.61,VALM,"COL",3,0)="SEGMENTS^36^45^Data Segment(s) [Time:Occurrence]"
 .S ^SD(409.61,VALM,"EXP")="D EXPAND^VAQREQ02"
 .S ^SD(409.61,VALM,"FNL")="D EXIT^VAQREQ02"
 .S ^SD(409.61,VALM,"HDR")="D HD^VAQREQ02"
 .S ^SD(409.61,VALM,"HLP")="S X=""?"" D DISP^XQORM1 W !!"
 .S ^SD(409.61,VALM,"INIT")="D INIT^VAQREQ02"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK
 .W "Filed."
 ;
 W !,"'VAQ STATUS PDX1' List Template..."
 S DA=$O(^SD(409.61,"B","VAQ STATUS PDX1",0)),DIK="^SD(409.61," D ^DIK:DA
 K DO,DD S DIC(0)="L",DIC="^SD(409.61,",X="VAQ STATUS PDX1" D FILE^DICN S VALM=+Y
 I VALM>0 D
 .S ^SD(409.61,VALM,0)="VAQ STATUS PDX1^1^^^5^18^1^1^PDX TRANSMISSIONS^VAQ PDX1 (MENU)^PDX V1.5 - STATUS^1"
 .S ^SD(409.61,VALM,1)="^VALM HIDDEN ACTIONS"
 .S ^SD(409.61,VALM,"ARRAY")=" ^TMP(""VAQR1"",$J)"
 .S ^SD(409.61,VALM,"EXP")=""
 .S ^SD(409.61,VALM,"FNL")="D EXIT^VAQREQ01"
 .S ^SD(409.61,VALM,"HDR")="D HD^VAQREQ01"
 .S ^SD(409.61,VALM,"HLP")="S X=""?"" D DISP^XQORM1 W !!"
 .S ^SD(409.61,VALM,"INIT")="D INIT^VAQREQ01"
 .S DA=VALM,DIK="^SD(409.61," D IX1^DIK
 .W "Filed."
 ;
 K DIC,DIK,VALM,X,DA Q
