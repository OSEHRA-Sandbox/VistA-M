IBDEI01M ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1111,0)
 ;;=01620^^8^94^6^^^^1
 ;;^UTILITY(U,$J,358.3,1111,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1111,1,2,0)
 ;;=2^ANESTH SHLDR PROC,CLSD PROC SHLDR JT
 ;;^UTILITY(U,$J,358.3,1111,1,4,0)
 ;;=4^01620
 ;;^UTILITY(U,$J,358.3,1112,0)
 ;;=01820^^8^94^5^^^^1
 ;;^UTILITY(U,$J,358.3,1112,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1112,1,2,0)
 ;;=2^ANESTH LOWER ARM PROCEDURE
 ;;^UTILITY(U,$J,358.3,1112,1,4,0)
 ;;=4^01820
 ;;^UTILITY(U,$J,358.3,1113,0)
 ;;=01820^^8^94^15^^^^1
 ;;^UTILITY(U,$J,358.3,1113,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1113,1,2,0)
 ;;=2^MUA WRIST
 ;;^UTILITY(U,$J,358.3,1113,1,4,0)
 ;;=4^01820
 ;;^UTILITY(U,$J,358.3,1114,0)
 ;;=J2001^^8^94^14^^^^1
 ;;^UTILITY(U,$J,358.3,1114,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1114,1,2,0)
 ;;=2^LIDOCAINE,10 mg
 ;;^UTILITY(U,$J,358.3,1114,1,4,0)
 ;;=4^J2001
 ;;^UTILITY(U,$J,358.3,1115,0)
 ;;=S0020^^8^94^9^^^^1
 ;;^UTILITY(U,$J,358.3,1115,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1115,1,2,0)
 ;;=2^BUPIVICAINE HYDRO,30 ml
 ;;^UTILITY(U,$J,358.3,1115,1,4,0)
 ;;=4^S0020
 ;;^UTILITY(U,$J,358.3,1116,0)
 ;;=01112^^8^95^2^^^^1
 ;;^UTILITY(U,$J,358.3,1116,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1116,1,2,0)
 ;;=2^BONE ASPIRATE/BX,ANESTH
 ;;^UTILITY(U,$J,358.3,1116,1,4,0)
 ;;=4^01112
 ;;^UTILITY(U,$J,358.3,1117,0)
 ;;=01120^^8^95^1^^^^1
 ;;^UTILITY(U,$J,358.3,1117,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1117,1,2,0)
 ;;=2^ANESTH PELVIS SURG,CLSD PROC UPPER FEMUR
 ;;^UTILITY(U,$J,358.3,1117,1,4,0)
 ;;=4^01120
 ;;^UTILITY(U,$J,358.3,1118,0)
 ;;=01931^^8^96^2^^^^1
 ;;^UTILITY(U,$J,358.3,1118,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1118,1,2,0)
 ;;=2^INTRAHEPATIC CIRCULATION (TIPS)
 ;;^UTILITY(U,$J,358.3,1118,1,4,0)
 ;;=4^01931
 ;;^UTILITY(U,$J,358.3,1119,0)
 ;;=01922^^8^96^6^^^^1
 ;;^UTILITY(U,$J,358.3,1119,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1119,1,2,0)
 ;;=2^NON-INVASIVE IMAGE (CT/MRI)/RAD TX
 ;;^UTILITY(U,$J,358.3,1119,1,4,0)
 ;;=4^01922
 ;;^UTILITY(U,$J,358.3,1120,0)
 ;;=01924^^8^96^3^^^^1
 ;;^UTILITY(U,$J,358.3,1120,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1120,1,2,0)
 ;;=2^INTRVN RAD ARTERIAL SYS NOS
 ;;^UTILITY(U,$J,358.3,1120,1,4,0)
 ;;=4^01924
 ;;^UTILITY(U,$J,358.3,1121,0)
 ;;=01925^^8^96^4^^^^1
 ;;^UTILITY(U,$J,358.3,1121,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1121,1,2,0)
 ;;=2^INTRVN RAD CAROTID/CORONARY
 ;;^UTILITY(U,$J,358.3,1121,1,4,0)
 ;;=4^01925
 ;;^UTILITY(U,$J,358.3,1122,0)
 ;;=01926^^8^96^5^^^^1
 ;;^UTILITY(U,$J,358.3,1122,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1122,1,2,0)
 ;;=2^INTRVN RAD INTRA-CRANIAL/CARDIAC/AORTA
 ;;^UTILITY(U,$J,358.3,1122,1,4,0)
 ;;=4^01926
 ;;^UTILITY(U,$J,358.3,1123,0)
 ;;=00700^^8^96^1^^^^1
 ;;^UTILITY(U,$J,358.3,1123,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1123,1,2,0)
 ;;=2^ANESTH ABD WALL SURG,PERCUT GASTROSTOMY
 ;;^UTILITY(U,$J,358.3,1123,1,4,0)
 ;;=4^00700
 ;;^UTILITY(U,$J,358.3,1124,0)
 ;;=01670^^8^97^1^^^^1
 ;;^UTILITY(U,$J,358.3,1124,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1124,1,2,0)
 ;;=2^VENOUS ANGIO SHOULDER,ANESTH
 ;;^UTILITY(U,$J,358.3,1124,1,4,0)
 ;;=4^01670
 ;;^UTILITY(U,$J,358.3,1125,0)
 ;;=64490^^8^98^2^^^^1
 ;;^UTILITY(U,$J,358.3,1125,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1125,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,C/T 1ST LEVEL
 ;;^UTILITY(U,$J,358.3,1125,1,4,0)
 ;;=4^64490
 ;;^UTILITY(U,$J,358.3,1126,0)
 ;;=64491^^8^98^3^^^^1
 ;;^UTILITY(U,$J,358.3,1126,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1126,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,C/T 2ND LEVEL
 ;;^UTILITY(U,$J,358.3,1126,1,4,0)
 ;;=4^64491
 ;;^UTILITY(U,$J,358.3,1127,0)
 ;;=64492^^8^98^4^^^^1
 ;;^UTILITY(U,$J,358.3,1127,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1127,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,C/T 3RD LEVEL
 ;;^UTILITY(U,$J,358.3,1127,1,4,0)
 ;;=4^64492
 ;;^UTILITY(U,$J,358.3,1128,0)
 ;;=64493^^8^98^5^^^^1
 ;;^UTILITY(U,$J,358.3,1128,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1128,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,L/S 1ST LEVEL
 ;;^UTILITY(U,$J,358.3,1128,1,4,0)
 ;;=4^64493
 ;;^UTILITY(U,$J,358.3,1129,0)
 ;;=64494^^8^98^6^^^^1
 ;;^UTILITY(U,$J,358.3,1129,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1129,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,L/S 2ND LEVEL
 ;;^UTILITY(U,$J,358.3,1129,1,4,0)
 ;;=4^64494
 ;;^UTILITY(U,$J,358.3,1130,0)
 ;;=64495^^8^98^7^^^^1
 ;;^UTILITY(U,$J,358.3,1130,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1130,1,2,0)
 ;;=2^INJ PARAVERTABRAL FACET,L/S 3RD LEVEL
 ;;^UTILITY(U,$J,358.3,1130,1,4,0)
 ;;=4^64495
 ;;^UTILITY(U,$J,358.3,1131,0)
 ;;=00635^^8^99^1^^^^1
 ;;^UTILITY(U,$J,358.3,1131,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1131,1,2,0)
 ;;=2^ANESTH LUMBAR PUNCTURE
 ;;^UTILITY(U,$J,358.3,1131,1,4,0)
 ;;=4^00635
 ;;^UTILITY(U,$J,358.3,1132,0)
 ;;=01996^^8^99^3^^^^1
 ;;^UTILITY(U,$J,358.3,1132,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1132,1,2,0)
 ;;=2^DAILY INPT MGMT CONT DRUG-EPIDURAL
 ;;^UTILITY(U,$J,358.3,1132,1,4,0)
 ;;=4^01996
 ;;^UTILITY(U,$J,358.3,1133,0)
 ;;=00620^^8^99^2^^^^1
 ;;^UTILITY(U,$J,358.3,1133,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1133,1,2,0)
 ;;=2^ANESTH THORAC SPINE & CORD NOS
 ;;^UTILITY(U,$J,358.3,1133,1,4,0)
 ;;=4^00620
 ;;^UTILITY(U,$J,358.3,1134,0)
 ;;=93313^^8^100^1^^^^1
 ;;^UTILITY(U,$J,358.3,1134,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1134,1,2,0)
 ;;=2^ECHO TRANSESOPHAGEAL INTRA-OP
 ;;^UTILITY(U,$J,358.3,1134,1,4,0)
 ;;=4^93313
 ;;^UTILITY(U,$J,358.3,1135,0)
 ;;=00410^^8^101^1^^^^1
 ;;^UTILITY(U,$J,358.3,1135,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1135,1,2,0)
 ;;=2^CARDIOVERSION/ICD TEST,ANESTH
 ;;^UTILITY(U,$J,358.3,1135,1,4,0)
 ;;=4^00410
 ;;^UTILITY(U,$J,358.3,1136,0)
 ;;=00740^^8^102^1^^^^1
 ;;^UTILITY(U,$J,358.3,1136,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1136,1,2,0)
 ;;=2^ENDOSCOPIC UPPER GI/ERCP,ANESTH
 ;;^UTILITY(U,$J,358.3,1136,1,4,0)
 ;;=4^00740
 ;;^UTILITY(U,$J,358.3,1137,0)
 ;;=00790^^8^102^2^^^^1
 ;;^UTILITY(U,$J,358.3,1137,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1137,1,2,0)
 ;;=2^INT/EXT BILIARY DRAINAGE
 ;;^UTILITY(U,$J,358.3,1137,1,4,0)
 ;;=4^00790
 ;;^UTILITY(U,$J,358.3,1138,0)
 ;;=00770^^8^102^3^^^^1
 ;;^UTILITY(U,$J,358.3,1138,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1138,1,2,0)
 ;;=2^PORTAL VEIN/EMBOLIZATION
 ;;^UTILITY(U,$J,358.3,1138,1,4,0)
 ;;=4^00770
 ;;^UTILITY(U,$J,358.3,1139,0)
 ;;=00790^^8^102^4^^^^1
 ;;^UTILITY(U,$J,358.3,1139,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1139,1,2,0)
 ;;=2^UPPER ABDOMEN (LIVER ABLATION),ANESTH
 ;;^UTILITY(U,$J,358.3,1139,1,4,0)
 ;;=4^00790
 ;;^UTILITY(U,$J,358.3,1140,0)
 ;;=01780^^8^103^1^^^^1
 ;;^UTILITY(U,$J,358.3,1140,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1140,1,2,0)
 ;;=2^VENOUS ANGIO UPPER ARM/ELBOW,ANESTH
 ;;^UTILITY(U,$J,358.3,1140,1,4,0)
 ;;=4^01780
 ;;^UTILITY(U,$J,358.3,1141,0)
 ;;=01260^^8^104^6^^^^1
 ;;^UTILITY(U,$J,358.3,1141,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1141,1,2,0)
 ;;=2^VENOUS ANGIO UPPER LEG,ANESTH
 ;;^UTILITY(U,$J,358.3,1141,1,4,0)
 ;;=4^01260
 ;;^UTILITY(U,$J,358.3,1142,0)
 ;;=01220^^8^104^4^^^^1
 ;;^UTILITY(U,$J,358.3,1142,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1142,1,2,0)
 ;;=2^ANESTH PROCEDURE ON FEMUR
 ;;^UTILITY(U,$J,358.3,1142,1,4,0)
 ;;=4^01220
 ;;^UTILITY(U,$J,358.3,1143,0)
 ;;=01230^^8^104^5^^^^1
 ;;^UTILITY(U,$J,358.3,1143,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1143,1,2,0)
 ;;=2^ANESTH SURGERY OF FEMUR
 ;;^UTILITY(U,$J,358.3,1143,1,4,0)
 ;;=4^01230
 ;;^UTILITY(U,$J,358.3,1144,0)
 ;;=01380^^8^104^3^^^^1
 ;;^UTILITY(U,$J,358.3,1144,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,1144,1,2,0)
 ;;=2^ANESTH KNEE JT PROC,MUA KNEE
 ;;^UTILITY(U,$J,358.3,1144,1,4,0)
 ;;=4^01380
 ;;^UTILITY(U,$J,358.3,1145,0)
 ;;=01382^^8^104^1^^^^1
 ;;^UTILITY(U,$J,358.3,1145,1,0)
 ;;=^358.31IA^4^2
