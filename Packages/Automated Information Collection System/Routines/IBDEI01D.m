IBDEI01D ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1284,1,3,0)
 ;;=3^Thrombo Cor,Inc Cor Angio Iv Inf
 ;;^UTILITY(U,$J,358.3,1285,0)
 ;;=92978^^17^119^7^^^^1
 ;;^UTILITY(U,$J,358.3,1285,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1285,1,2,0)
 ;;=2^92978
 ;;^UTILITY(U,$J,358.3,1285,1,3,0)
 ;;=3^Intr Vasc U/S During Diag Eval
 ;;^UTILITY(U,$J,358.3,1286,0)
 ;;=92979^^17^119^8^^^^1
 ;;^UTILITY(U,$J,358.3,1286,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1286,1,2,0)
 ;;=2^92979
 ;;^UTILITY(U,$J,358.3,1286,1,3,0)
 ;;=3^     Each Add'L Vessel (W/92978)
 ;;^UTILITY(U,$J,358.3,1287,0)
 ;;=36010^^17^119^5^^^^1
 ;;^UTILITY(U,$J,358.3,1287,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1287,1,2,0)
 ;;=2^36010
 ;;^UTILITY(U,$J,358.3,1287,1,3,0)
 ;;=3^Cath Place Svc/Ivc (Sheath)
 ;;^UTILITY(U,$J,358.3,1288,0)
 ;;=36011^^17^119^6^^^^1
 ;;^UTILITY(U,$J,358.3,1288,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1288,1,2,0)
 ;;=2^36011
 ;;^UTILITY(U,$J,358.3,1288,1,3,0)
 ;;=3^Cath Place Vein 1St Order(Sheath
 ;;^UTILITY(U,$J,358.3,1289,0)
 ;;=92970^^17^119^3^^^^1
 ;;^UTILITY(U,$J,358.3,1289,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1289,1,2,0)
 ;;=2^92970
 ;;^UTILITY(U,$J,358.3,1289,1,3,0)
 ;;=3^Cardio Assist Dev Insert
 ;;^UTILITY(U,$J,358.3,1290,0)
 ;;=76930^^17^119^26^^^^1
 ;;^UTILITY(U,$J,358.3,1290,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1290,1,2,0)
 ;;=2^76930
 ;;^UTILITY(U,$J,358.3,1290,1,3,0)
 ;;=3^U/S Guide (W/33010)
 ;;^UTILITY(U,$J,358.3,1291,0)
 ;;=76000^^17^119^2^^^^1
 ;;^UTILITY(U,$J,358.3,1291,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1291,1,2,0)
 ;;=2^76000
 ;;^UTILITY(U,$J,358.3,1291,1,3,0)
 ;;=3^Cardiac Fluoro<1Hr(No Cath Proc)
 ;;^UTILITY(U,$J,358.3,1292,0)
 ;;=92973^^17^119^23^^^^1
 ;;^UTILITY(U,$J,358.3,1292,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1292,1,2,0)
 ;;=2^92973
 ;;^UTILITY(U,$J,358.3,1292,1,3,0)
 ;;=3^Perc Coronary Thrombectomy Mechanical
 ;;^UTILITY(U,$J,358.3,1293,0)
 ;;=92974^^17^119^4^^^^1
 ;;^UTILITY(U,$J,358.3,1293,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1293,1,2,0)
 ;;=2^92974
 ;;^UTILITY(U,$J,358.3,1293,1,3,0)
 ;;=3^Cath Place Cardio Brachytx
 ;;^UTILITY(U,$J,358.3,1294,0)
 ;;=92920^^17^119^18^^^^1
 ;;^UTILITY(U,$J,358.3,1294,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1294,1,2,0)
 ;;=2^92920
 ;;^UTILITY(U,$J,358.3,1294,1,3,0)
 ;;=3^PRQ Cardia Angioplast 1 Art
 ;;^UTILITY(U,$J,358.3,1295,0)
 ;;=92921^^17^119^19^^^^1
 ;;^UTILITY(U,$J,358.3,1295,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1295,1,2,0)
 ;;=2^92921
 ;;^UTILITY(U,$J,358.3,1295,1,3,0)
 ;;=3^PRQ Cardiac Angio Addl Art
 ;;^UTILITY(U,$J,358.3,1296,0)
 ;;=92924^^17^119^9^^^^1
 ;;^UTILITY(U,$J,358.3,1296,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1296,1,2,0)
 ;;=2^92924
 ;;^UTILITY(U,$J,358.3,1296,1,3,0)
 ;;=3^PRQ Card Angio/Athrect 1 Art
 ;;^UTILITY(U,$J,358.3,1297,0)
 ;;=92925^^17^119^10^^^^1
 ;;^UTILITY(U,$J,358.3,1297,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1297,1,2,0)
 ;;=2^92925
 ;;^UTILITY(U,$J,358.3,1297,1,3,0)
 ;;=3^PRQ Card Angio/Athrect Addl Art
 ;;^UTILITY(U,$J,358.3,1298,0)
 ;;=92928^^17^119^16^^^^1
 ;;^UTILITY(U,$J,358.3,1298,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1298,1,2,0)
 ;;=2^92928
 ;;^UTILITY(U,$J,358.3,1298,1,3,0)
 ;;=3^PRQ Card Stent w/ Angio 1 Vsl
 ;;^UTILITY(U,$J,358.3,1299,0)
 ;;=92929^^17^119^17^^^^1
 ;;^UTILITY(U,$J,358.3,1299,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1299,1,2,0)
 ;;=2^92929
 ;;^UTILITY(U,$J,358.3,1299,1,3,0)
 ;;=3^PRQ Card Stent w/ Angio Addl Vsl
 ;;^UTILITY(U,$J,358.3,1300,0)
 ;;=92933^^17^119^14^^^^1
 ;;^UTILITY(U,$J,358.3,1300,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1300,1,2,0)
 ;;=2^92933
 ;;^UTILITY(U,$J,358.3,1300,1,3,0)
 ;;=3^PRQ Card Stent Ath/Angio
 ;;^UTILITY(U,$J,358.3,1301,0)
 ;;=92934^^17^119^15^^^^1
 ;;^UTILITY(U,$J,358.3,1301,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1301,1,2,0)
 ;;=2^92934
 ;;^UTILITY(U,$J,358.3,1301,1,3,0)
 ;;=3^PRQ Card Stent Ath/Angio Ea Addl Branch
 ;;^UTILITY(U,$J,358.3,1302,0)
 ;;=92937^^17^119^20^^^^1
 ;;^UTILITY(U,$J,358.3,1302,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1302,1,2,0)
 ;;=2^92937
 ;;^UTILITY(U,$J,358.3,1302,1,3,0)
 ;;=3^PRQ Revasc Byp Graft 1 Vsl
 ;;^UTILITY(U,$J,358.3,1303,0)
 ;;=92938^^17^119^21^^^^1
 ;;^UTILITY(U,$J,358.3,1303,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1303,1,2,0)
 ;;=2^92938
 ;;^UTILITY(U,$J,358.3,1303,1,3,0)
 ;;=3^PRQ Revasc Byp Graft Addl Vsl
 ;;^UTILITY(U,$J,358.3,1304,0)
 ;;=92941^^17^119^13^^^^1
 ;;^UTILITY(U,$J,358.3,1304,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1304,1,2,0)
 ;;=2^92941
 ;;^UTILITY(U,$J,358.3,1304,1,3,0)
 ;;=3^PRQ Card Revasc MI 1 Vsl
 ;;^UTILITY(U,$J,358.3,1305,0)
 ;;=92943^^17^119^11^^^^1
 ;;^UTILITY(U,$J,358.3,1305,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1305,1,2,0)
 ;;=2^92943
 ;;^UTILITY(U,$J,358.3,1305,1,3,0)
 ;;=3^PRQ Card Revasc Chronic 1 Vsl
 ;;^UTILITY(U,$J,358.3,1306,0)
 ;;=92944^^17^119^12^^^^1
 ;;^UTILITY(U,$J,358.3,1306,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1306,1,2,0)
 ;;=2^92944
 ;;^UTILITY(U,$J,358.3,1306,1,3,0)
 ;;=3^PRQ Card Revasc Chronic Addl Vsl
 ;;^UTILITY(U,$J,358.3,1307,0)
 ;;=93600^^17^120^3^^^^1
 ;;^UTILITY(U,$J,358.3,1307,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1307,1,2,0)
 ;;=2^93600
 ;;^UTILITY(U,$J,358.3,1307,1,3,0)
 ;;=3^Bundle Of His Record
 ;;^UTILITY(U,$J,358.3,1308,0)
 ;;=93602^^17^120^20^^^^1
 ;;^UTILITY(U,$J,358.3,1308,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1308,1,2,0)
 ;;=2^93602
 ;;^UTILITY(U,$J,358.3,1308,1,3,0)
 ;;=3^Intra-Atrial Record
 ;;^UTILITY(U,$J,358.3,1309,0)
 ;;=93603^^17^120^23^^^^1
 ;;^UTILITY(U,$J,358.3,1309,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1309,1,2,0)
 ;;=2^93603
 ;;^UTILITY(U,$J,358.3,1309,1,3,0)
 ;;=3^R Vent Record
 ;;^UTILITY(U,$J,358.3,1310,0)
 ;;=93609^^17^120^22^^^^1
 ;;^UTILITY(U,$J,358.3,1310,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1310,1,2,0)
 ;;=2^93609
 ;;^UTILITY(U,$J,358.3,1310,1,3,0)
 ;;=3^Mapping Of Tachycardia
 ;;^UTILITY(U,$J,358.3,1311,0)
 ;;=93610^^17^120^19^^^^1
 ;;^UTILITY(U,$J,358.3,1311,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1311,1,2,0)
 ;;=2^93610
 ;;^UTILITY(U,$J,358.3,1311,1,3,0)
 ;;=3^Intra-Atrial Pacing
 ;;^UTILITY(U,$J,358.3,1312,0)
 ;;=93612^^17^120^21^^^^1
 ;;^UTILITY(U,$J,358.3,1312,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1312,1,2,0)
 ;;=2^93612
 ;;^UTILITY(U,$J,358.3,1312,1,3,0)
 ;;=3^Intraventricular Pacing
 ;;^UTILITY(U,$J,358.3,1313,0)
 ;;=93615^^17^120^16^^^^1
 ;;^UTILITY(U,$J,358.3,1313,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1313,1,2,0)
 ;;=2^93615
 ;;^UTILITY(U,$J,358.3,1313,1,3,0)
 ;;=3^Esoph Record
 ;;^UTILITY(U,$J,358.3,1314,0)
 ;;=93616^^17^120^17^^^^1
 ;;^UTILITY(U,$J,358.3,1314,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1314,1,2,0)
 ;;=2^93616
 ;;^UTILITY(U,$J,358.3,1314,1,3,0)
 ;;=3^Esoph Record W/Pacing
 ;;^UTILITY(U,$J,358.3,1315,0)
 ;;=93618^^17^120^18^^^^1
 ;;^UTILITY(U,$J,358.3,1315,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1315,1,2,0)
 ;;=2^93618
 ;;^UTILITY(U,$J,358.3,1315,1,3,0)
 ;;=3^Induct Of Arryth By Elec Pace
 ;;^UTILITY(U,$J,358.3,1316,0)
 ;;=93619^^17^120^13^^^^1
 ;;^UTILITY(U,$J,358.3,1316,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1316,1,2,0)
 ;;=2^93619
 ;;^UTILITY(U,$J,358.3,1316,1,3,0)
 ;;=3^Eps Eval, No Induct Of Arryth
 ;;^UTILITY(U,$J,358.3,1317,0)
 ;;=93620^^17^120^10^^^^1
 ;;^UTILITY(U,$J,358.3,1317,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1317,1,2,0)
 ;;=2^93620
 ;;^UTILITY(U,$J,358.3,1317,1,3,0)
 ;;=3^Eps Eval W/Induct Of Arryth
 ;;^UTILITY(U,$J,358.3,1318,0)
 ;;=93621^^17^120^9^^^^1
 ;;^UTILITY(U,$J,358.3,1318,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1318,1,2,0)
 ;;=2^93621
 ;;^UTILITY(U,$J,358.3,1318,1,3,0)
 ;;=3^Eps Eval W/Atrial Record
 ;;^UTILITY(U,$J,358.3,1319,0)
 ;;=93622^^17^120^11^^^^1
 ;;^UTILITY(U,$J,358.3,1319,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1319,1,2,0)
 ;;=2^93622
 ;;^UTILITY(U,$J,358.3,1319,1,3,0)
 ;;=3^Eps Eval W/L Vent Record
 ;;^UTILITY(U,$J,358.3,1320,0)
 ;;=93623^^17^120^12^^^^1
 ;;^UTILITY(U,$J,358.3,1320,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1320,1,2,0)
 ;;=2^93623
 ;;^UTILITY(U,$J,358.3,1320,1,3,0)
 ;;=3^Eps Eval W/Prog Stim Drug (W/
 ;;^UTILITY(U,$J,358.3,1321,0)
 ;;=93624^^17^120^14^^^^1
 ;;^UTILITY(U,$J,358.3,1321,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1321,1,2,0)
 ;;=2^93624
 ;;^UTILITY(U,$J,358.3,1321,1,3,0)
 ;;=3^Eps F/U Study W/Pace W/Record
 ;;^UTILITY(U,$J,358.3,1322,0)
 ;;=93640^^17^120^7^^^^1
 ;;^UTILITY(U,$J,358.3,1322,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1322,1,2,0)
 ;;=2^93640
 ;;^UTILITY(U,$J,358.3,1322,1,3,0)
 ;;=3^Eps Eval Sgl/Dual Lead
 ;;^UTILITY(U,$J,358.3,1323,0)
 ;;=93641^^17^120^15^^^^1
 ;;^UTILITY(U,$J,358.3,1323,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1323,1,2,0)
 ;;=2^93641
 ;;^UTILITY(U,$J,358.3,1323,1,3,0)
 ;;=3^Eps Test Sgl/Dual Pulse Gen
 ;;^UTILITY(U,$J,358.3,1324,0)
 ;;=93642^^17^120^8^^^^1
 ;;^UTILITY(U,$J,358.3,1324,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1324,1,2,0)
 ;;=2^93642
 ;;^UTILITY(U,$J,358.3,1324,1,3,0)
 ;;=3^Eps Eval Sgl/Dual W/Pgm,Repgm
 ;;^UTILITY(U,$J,358.3,1325,0)
 ;;=93650^^17^120^2^^^^1
 ;;^UTILITY(U,$J,358.3,1325,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1325,1,2,0)
 ;;=2^93650
 ;;^UTILITY(U,$J,358.3,1325,1,3,0)
 ;;=3^Ablate Av,Create Chb
 ;;^UTILITY(U,$J,358.3,1326,0)
 ;;=93653^^17^120^4^^^^1
 ;;^UTILITY(U,$J,358.3,1326,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1326,1,2,0)
 ;;=2^93653
 ;;^UTILITY(U,$J,358.3,1326,1,3,0)
 ;;=3^EP & Ablate Supravent Arrhyt
 ;;^UTILITY(U,$J,358.3,1327,0)
 ;;=93654^^17^120^5^^^^1
 ;;^UTILITY(U,$J,358.3,1327,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1327,1,2,0)
 ;;=2^93654
 ;;^UTILITY(U,$J,358.3,1327,1,3,0)
 ;;=3^EP & Ablate Ventric Tachy
 ;;^UTILITY(U,$J,358.3,1328,0)
 ;;=93655^^17^120^1^^^^1
 ;;^UTILITY(U,$J,358.3,1328,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,1328,1,2,0)
 ;;=2^93655
