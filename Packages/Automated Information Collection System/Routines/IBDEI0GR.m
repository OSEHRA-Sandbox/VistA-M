IBDEI0GR ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.4,1175,0)
 ;;=HISTORY/STATUS CODES^4^135
 ;;^UTILITY(U,$J,358.4,1176,0)
 ;;=ESTABLISHED PATIENT^2^136
 ;;^UTILITY(U,$J,358.4,1177,0)
 ;;=TREATMENT^17^137
 ;;^UTILITY(U,$J,358.4,1178,0)
 ;;=GROUP TREATMENT^11^137
 ;;^UTILITY(U,$J,358.4,1179,0)
 ;;=VOICE PROSTHESES^18^137
 ;;^UTILITY(U,$J,358.4,1180,0)
 ;;=AAC DEVICES^1^137
 ;;^UTILITY(U,$J,358.4,1181,0)
 ;;=OTHER PROCEDURES^14^137
 ;;^UTILITY(U,$J,358.4,1182,0)
 ;;=LARYNGOSCOPY PROCEDURES^12^137
 ;;^UTILITY(U,$J,358.4,1183,0)
 ;;=NASOPHARYNGOSCOPY^13^137
 ;;^UTILITY(U,$J,358.4,1184,0)
 ;;=DYSPHAGIA EVALUATION^2^137
 ;;^UTILITY(U,$J,358.4,1185,0)
 ;;=SPEECH/LANGUAGE/HEARING EVALUATION^16^137
 ;;^UTILITY(U,$J,358.4,1186,0)
 ;;=SCREENING^15^137
 ;;^UTILITY(U,$J,358.4,1187,0)
 ;;=FUNCTIONAL RPTING ATTENTION^3^137
 ;;^UTILITY(U,$J,358.4,1188,0)
 ;;=FUNCTIONAL RPTING MEMORY^4^137
 ;;^UTILITY(U,$J,358.4,1189,0)
 ;;=FUNCTIONAL RPTING OTH SLP^6^137
 ;;^UTILITY(U,$J,358.4,1190,0)
 ;;=FUNCTIONAL RPTING VOICE^7^137
 ;;^UTILITY(U,$J,358.4,1191,0)
 ;;=FUNCTIONAL RPTING MOTOR SPEECH^5^137
 ;;^UTILITY(U,$J,358.4,1192,0)
 ;;=FUNCTIONAL RPTING SPEECH/LANG COMP^8^137
 ;;^UTILITY(U,$J,358.4,1193,0)
 ;;=FUNCTIONAL RPTING SPOKEN LANG EXP^9^137
 ;;^UTILITY(U,$J,358.4,1194,0)
 ;;=FUNCTIONAL RPTING SWALLOWING^10^137
 ;;^UTILITY(U,$J,358.4,1195,0)
 ;;=ADJUSTMENT DISORDERS^1^138
 ;;^UTILITY(U,$J,358.4,1196,0)
 ;;=ANXIETY DISORDERS^3^138
 ;;^UTILITY(U,$J,358.4,1197,0)
 ;;=AMNESTICS^2^138
 ;;^UTILITY(U,$J,358.4,1198,0)
 ;;=ORGANIC DISORDERS^9^138
 ;;^UTILITY(U,$J,358.4,1199,0)
 ;;=DEMENTIA^5^138
 ;;^UTILITY(U,$J,358.4,1200,0)
 ;;=DELIRIUM^4^138
 ;;^UTILITY(U,$J,358.4,1201,0)
 ;;=BIPOLAR DISORDERS^3.5^138
 ;;^UTILITY(U,$J,358.4,1202,0)
 ;;=PSYCHOSIS/OTHER^12^138
 ;;^UTILITY(U,$J,358.4,1203,0)
 ;;=PERSONALITY DISORDERS^11^138
 ;;^UTILITY(U,$J,358.4,1204,0)
 ;;=SEXUAL DISORDERS^14^138
 ;;^UTILITY(U,$J,358.4,1205,0)
 ;;=SLEEP DISORDERS^15^138
 ;;^UTILITY(U,$J,358.4,1206,0)
 ;;=OTHER DISORDERS^18^138
 ;;^UTILITY(U,$J,358.4,1207,0)
 ;;=SUBSTANCE RELATED DISORDERS^19^138
 ;;^UTILITY(U,$J,358.4,1208,0)
 ;;=V CODES^17^138
 ;;^UTILITY(U,$J,358.4,1209,0)
 ;;=PREVENTION/COUNSELING/SCREEN^21^138
 ;;^UTILITY(U,$J,358.4,1210,0)
 ;;=DEPRESSION^6^138
 ;;^UTILITY(U,$J,358.4,1211,0)
 ;;=MOOD DISORDERS (OTHER)^8^138
 ;;^UTILITY(U,$J,358.4,1212,0)
 ;;=SCHIZOPHRENIA^13^138
 ;;^UTILITY(U,$J,358.4,1213,0)
 ;;=SOMATIFORM DISORDERS^16^138
 ;;^UTILITY(U,$J,358.4,1214,0)
 ;;=EATING DISORDERS^7^138
 ;;^UTILITY(U,$J,358.4,1215,0)
 ;;=NEUROLEPTIC-INDUCED MOVEMENT DISORD^10^138
 ;;^UTILITY(U,$J,358.4,1216,0)
 ;;=PSYCHIATRIC THERAPY^8^139
 ;;^UTILITY(U,$J,358.4,1217,0)
 ;;=OTHER^7^139
 ;;^UTILITY(U,$J,358.4,1218,0)
 ;;=CASE MANAGEMENT SERVICES^1^139
 ;;^UTILITY(U,$J,358.4,1219,0)
 ;;=HOME HEATLH VISIT^5^139
 ;;^UTILITY(U,$J,358.4,1220,0)
 ;;=COMMUNITY RESIDENTIAL CARE^2^139
 ;;^UTILITY(U,$J,358.4,1221,0)
 ;;=SMOKING/TOBACCO USE CESSATION^9^139
 ;;^UTILITY(U,$J,358.4,1222,0)
 ;;=NON-EMER PATIENT TRANSPORT^6^139
 ;;^UTILITY(U,$J,358.4,1223,0)
 ;;=HEALTH AND BEHAVIOR ASSESSMENT^4^139
 ;;^UTILITY(U,$J,358.4,1224,0)
 ;;=EDUCATION/TRAINING^3^139
 ;;^UTILITY(U,$J,358.4,1225,0)
 ;;=NEW PATIENT^2^140
 ;;^UTILITY(U,$J,358.4,1226,0)
 ;;=ESTABLISHED PATIENT^1^140
 ;;^UTILITY(U,$J,358.4,1227,0)
 ;;=CONSULTATIONS^3^140
 ;;^UTILITY(U,$J,358.4,1228,0)
 ;;=NEURO^4^141
 ;;^UTILITY(U,$J,358.4,1229,0)
 ;;=MENTAL HEALTH^3^141
 ;;^UTILITY(U,$J,358.4,1230,0)
 ;;=HISTORY OF ILLNESS^2^141
 ;;^UTILITY(U,$J,358.4,1231,0)
 ;;=PREVENTIVE HEALTH/SCREENINGS^5^141
 ;;^UTILITY(U,$J,358.4,1232,0)
 ;;=SIGNS, SYMPTOMS, CONDITIONS^6^141
 ;;^UTILITY(U,$J,358.4,1233,0)
 ;;=OTHER REASONS^8^141
 ;;^UTILITY(U,$J,358.4,1234,0)
 ;;=TBI SCREENING^7^141
 ;;^UTILITY(U,$J,358.4,1235,0)
 ;;=CAUSES OF INJURY-SECONDARY^1^141
 ;;^UTILITY(U,$J,358.4,1236,0)
 ;;=PROCEDURES^1^142
 ;;^UTILITY(U,$J,358.4,1237,0)
 ;;=NEW PATIENT^2^143
 ;;^UTILITY(U,$J,358.4,1238,0)
 ;;=ESTABLISHED PATIENT^1^143
 ;;^UTILITY(U,$J,358.4,1239,0)
 ;;=CONSULTATIONS^3^143
 ;;^UTILITY(U,$J,358.4,1240,0)
 ;;=TELEHEALTH FACILITY FEE^1^144
 ;;^UTILITY(U,$J,358.4,1241,0)
 ;;=ABRASION w/o INFECTION^1^145
 ;;^UTILITY(U,$J,358.4,1242,0)
 ;;=ABRASION w/ INFECTION^2^145
 ;;^UTILITY(U,$J,358.4,1243,0)
 ;;=ABSCESS/CELLULITIS^3^145
 ;;^UTILITY(U,$J,358.4,1244,0)
 ;;=ACNE^4^145
 ;;^UTILITY(U,$J,358.4,1245,0)
 ;;=ALOPEICA^5^145
 ;;^UTILITY(U,$J,358.4,1246,0)
 ;;=BURNS^6^145
 ;;^UTILITY(U,$J,358.4,1247,0)
 ;;=CANDIDIASIS^7^145
 ;;^UTILITY(U,$J,358.4,1248,0)
 ;;=CELLULITIS^8^145
 ;;^UTILITY(U,$J,358.4,1249,0)
 ;;=DERMATITIS^9^145
 ;;^UTILITY(U,$J,358.4,1250,0)
 ;;=DERMATOPHYTOSIS^10^145
 ;;^UTILITY(U,$J,358.4,1251,0)
 ;;=ECZEMA^11^145
 ;;^UTILITY(U,$J,358.4,1252,0)
 ;;=EDEMA^12^145
 ;;^UTILITY(U,$J,358.4,1253,0)
 ;;=ERYTHEMA^13^145
 ;;^UTILITY(U,$J,358.4,1254,0)
 ;;=EXPOSURE TO HEALTH HAZARDS^14^145
 ;;^UTILITY(U,$J,358.4,1255,0)
 ;;=FOLLICULITIS^15^145
 ;;^UTILITY(U,$J,358.4,1256,0)
 ;;=FURUNCLE^16^145
 ;;^UTILITY(U,$J,358.4,1257,0)
 ;;=GRANULOMA^17^145
 ;;^UTILITY(U,$J,358.4,1258,0)
 ;;=HERPES^35^145
 ;;^UTILITY(U,$J,358.4,1259,0)
 ;;=HISTORY^36^145
 ;;^UTILITY(U,$J,358.4,1260,0)
 ;;=ICHTHYOSIS^38^145
 ;;^UTILITY(U,$J,358.4,1261,0)
 ;;=INSECT BITE w/o INFECTION^39^145
 ;;^UTILITY(U,$J,358.4,1262,0)
 ;;=INSECT BITE w/INFECTION^40^145
 ;;^UTILITY(U,$J,358.4,1263,0)
 ;;=KERATOSIS^42^145
 ;;^UTILITY(U,$J,358.4,1264,0)
 ;;=LACERATION^44^145
 ;;^UTILITY(U,$J,358.4,1265,0)
 ;;=LESION^46^145
 ;;^UTILITY(U,$J,358.4,1266,0)
 ;;=LICHENIFICATION^47^145
 ;;^UTILITY(U,$J,358.4,1267,0)
 ;;=LUPUS^48^145
 ;;^UTILITY(U,$J,358.4,1268,0)
 ;;=MELANOMA^50^145
 ;;^UTILITY(U,$J,358.4,1269,0)
 ;;=NAIL DISEASE^53^145
 ;;^UTILITY(U,$J,358.4,1270,0)
 ;;=NEOPLASM-BENIGN^54^145
 ;;^UTILITY(U,$J,358.4,1271,0)
 ;;=NEOPLASM-IN SITU^56^145
 ;;^UTILITY(U,$J,358.4,1272,0)
 ;;=NEOPLASM-OTHER^57^145
 ;;^UTILITY(U,$J,358.4,1273,0)
 ;;=NEURODERMATITIS^58^145
 ;;^UTILITY(U,$J,358.4,1274,0)
 ;;=PEDICULOSIS^61^145
 ;;^UTILITY(U,$J,358.4,1275,0)
 ;;=PITYRIASIS^62^145
 ;;^UTILITY(U,$J,358.4,1276,0)
 ;;=PRURITUS^63^145
 ;;^UTILITY(U,$J,358.4,1277,0)
 ;;=PSORIASIS^64^145
 ;;^UTILITY(U,$J,358.4,1278,0)
 ;;=STASIS^66^145
 ;;^UTILITY(U,$J,358.4,1279,0)
 ;;=ULCER^68^145
 ;;^UTILITY(U,$J,358.4,1280,0)
 ;;=ULCER-LOWER EXTREMITY^69^145
 ;;^UTILITY(U,$J,358.4,1281,0)
 ;;=URTICARIA^70^145
 ;;^UTILITY(U,$J,358.4,1282,0)
 ;;=VARICOSE VEINS^71^145
 ;;^UTILITY(U,$J,358.4,1283,0)
 ;;=WOUND^73^145
 ;;^UTILITY(U,$J,358.4,1284,0)
 ;;="A" MISC DIAGNOSIS^5.1^145
 ;;^UTILITY(U,$J,358.4,1285,0)
 ;;="B" MISC DIAGNOSIS^6.1^145
 ;;^UTILITY(U,$J,358.4,1286,0)
 ;;="C" MISC DIAGNOSIS^8.1^145
 ;;^UTILITY(U,$J,358.4,1287,0)
 ;;="D" MISC DIAGNOSIS^10.1^145
 ;;^UTILITY(U,$J,358.4,1288,0)
 ;;="E" MISC DIAGNOSIS^14.1^145
 ;;^UTILITY(U,$J,358.4,1289,0)
 ;;="F" MISC DIAGNOSIS^16.1^145
 ;;^UTILITY(U,$J,358.4,1290,0)
 ;;="H" MISC DIAGNOSIS^37^145
 ;;^UTILITY(U,$J,358.4,1291,0)
 ;;="I" MISC DIAGNOSIS^41^145
 ;;^UTILITY(U,$J,358.4,1292,0)
 ;;="K" MISC DIAGNOSIS^43^145
 ;;^UTILITY(U,$J,358.4,1293,0)
 ;;="L" MISC DIAGNOSIS^49^145
 ;;^UTILITY(U,$J,358.4,1294,0)
 ;;="M" MISC DIAGNOSIS^51^145
 ;;^UTILITY(U,$J,358.4,1295,0)
 ;;="N" MISC DIAGNOSIS^59^145
 ;;^UTILITY(U,$J,358.4,1296,0)
 ;;="P" MISC DIAGNOSIS^65^145
 ;;^UTILITY(U,$J,358.4,1297,0)
 ;;="X" MISC DIAGNOSIS^75^145
 ;;^UTILITY(U,$J,358.4,1298,0)
 ;;="V" MISC DIAGNOSIS^72^145
 ;;^UTILITY(U,$J,358.4,1299,0)
 ;;="W" MISC DIAGNOSIS^74^145
 ;;^UTILITY(U,$J,358.4,1300,0)
 ;;=AFTERCARE CODES^0^145
 ;;^UTILITY(U,$J,358.4,1301,0)
 ;;="S" MISC DIAGNOSIS^67^145
 ;;^UTILITY(U,$J,358.4,1302,0)
 ;;=CANCER (NOT MELANOMA)^6.5^145
 ;;^UTILITY(U,$J,358.4,1303,0)
 ;;=BOIL/FURUNCLE^5.5^145
 ;;^UTILITY(U,$J,358.4,1304,0)
 ;;=CHEILITIS^8.03^145
 ;;^UTILITY(U,$J,358.4,1305,0)
 ;;=CYST^8.07^145
 ;;^UTILITY(U,$J,358.4,1306,0)
 ;;=DRUG REACTION^10.02^145
 ;;^UTILITY(U,$J,358.4,1307,0)
 ;;="G" MISC DIAGNOSIS^17.5^145
 ;;^UTILITY(U,$J,358.4,1308,0)
 ;;=LENTIGO^45^145
 ;;^UTILITY(U,$J,358.4,1309,0)
 ;;="O" DIAGNOSES^60^145
 ;;^UTILITY(U,$J,358.4,1310,0)
 ;;=PSYCHO CUTEANOUS DERMATITIS^64.5^145
 ;;^UTILITY(U,$J,358.4,1311,0)
 ;;="R" DIAGNOSES^65.5^145
 ;;^UTILITY(U,$J,358.4,1312,0)
 ;;="T" DIAGNOSES^67.5^145
 ;;^UTILITY(U,$J,358.4,1313,0)
 ;;=SCREENING^65.6^145
 ;;^UTILITY(U,$J,358.4,1314,0)
 ;;=NEOPLASM-MALIGNANT^55^145
 ;;^UTILITY(U,$J,358.4,1315,0)
 ;;="A" MISC DIAGNOSIS^1^146
 ;;^UTILITY(U,$J,358.4,1316,0)
 ;;="B" MISC DIAGNOSIS^2^146
 ;;^UTILITY(U,$J,358.4,1317,0)
 ;;="C" MISC DIAGNOSIS^3^146
 ;;^UTILITY(U,$J,358.4,1318,0)
 ;;="D" MISC DIAGNOSIS^5^146
 ;;^UTILITY(U,$J,358.4,1319,0)
 ;;="E/F" MISC DIAGNOSIS^6^146
 ;;^UTILITY(U,$J,358.4,1320,0)
 ;;="H" MISC DIAGNOSIS^8^146
 ;;^UTILITY(U,$J,358.4,1321,0)
 ;;="I" MISC DIAGNOSIS^9^146
 ;;^UTILITY(U,$J,358.4,1322,0)
 ;;="J/K/L" MISC DIAGNOSIS^10^146
 ;;^UTILITY(U,$J,358.4,1323,0)
 ;;="M" MISC DIAGNOSIS^11^146
 ;;^UTILITY(U,$J,358.4,1324,0)
 ;;="N/O" MISC DISGNOSIS^12^146
 ;;^UTILITY(U,$J,358.4,1325,0)
 ;;="P" MISC DIAGNOSIS^13^146
 ;;^UTILITY(U,$J,358.4,1326,0)
 ;;="V/W/X/Y/Z" MISC DIAGNOSIS^17^146
 ;;^UTILITY(U,$J,358.4,1327,0)
 ;;="S" MISC DIAGNOSIS^15^146
 ;;^UTILITY(U,$J,358.4,1328,0)
 ;;=CANCER (NOT MELANOMA)^4^146
 ;;^UTILITY(U,$J,358.4,1329,0)
 ;;="G" MISC DIAGNOSIS^7^146
 ;;^UTILITY(U,$J,358.4,1330,0)
 ;;="R" DIAGNOSES^14^146
 ;;^UTILITY(U,$J,358.4,1331,0)
 ;;="T/U" MISC DIAGNOSIS^16^146
 ;;^UTILITY(U,$J,358.4,1332,0)
 ;;=TELEHEALTH FACILITY FEE^1^147
 ;;^UTILITY(U,$J,358.4,1333,0)
 ;;=CARDIOVASCULAR^2^148
 ;;^UTILITY(U,$J,358.4,1334,0)
 ;;=RESPIRATORY^7^148
 ;;^UTILITY(U,$J,358.4,1335,0)
 ;;=SIGNS, SYMPTOMS, CONDITIONS^8^148
 ;;^UTILITY(U,$J,358.4,1336,0)
 ;;=COMMON DIAGNOSES^1^148
 ;;^UTILITY(U,$J,358.4,1337,0)
 ;;=POST-OP COMPLICATIONS^6^148
 ;;^UTILITY(U,$J,358.4,1338,0)
 ;;=AFTERCARE POST SURGERY^4^148
 ;;^UTILITY(U,$J,358.4,1339,0)
 ;;=METASTATIC CANCER SITES^5^148
