IBDEI161 ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.4,1297,0)
 ;;=ELIMINATION DISORDERS^11^100
 ;;^UTILITY(U,$J,358.4,1298,0)
 ;;=GAMBLING DISORDER^13^100
 ;;^UTILITY(U,$J,358.4,1299,0)
 ;;=OTHER MENTAL DISORDERS^20^100
 ;;^UTILITY(U,$J,358.4,1300,0)
 ;;=NEURODEVELOPMENTAL DISORDERS^18^100
 ;;^UTILITY(U,$J,358.4,1301,0)
 ;;=SUBSTANCE USE DISORDER-CAFFEINE^32^100
 ;;^UTILITY(U,$J,358.4,1302,0)
 ;;=SUICIDAL IDEATIONS^41^100
 ;;^UTILITY(U,$J,358.4,1303,0)
 ;;=SUBSTANCE USE DISORDER-UNKNOWN SUBSTANCE^40^100
 ;;^UTILITY(U,$J,358.4,1304,0)
 ;;=SUBSTANCE USE DISORDER-STIMULANTS^38^100
 ;;^UTILITY(U,$J,358.4,1305,0)
 ;;=PSYCHOTHERAPY (INPT OR OUTPT)^6^101
 ;;^UTILITY(U,$J,358.4,1306,0)
 ;;=PSYCHOTHERAPY FOR CRISIS^7^101
 ;;^UTILITY(U,$J,358.4,1307,0)
 ;;=OTHER CODES^3^101
 ;;^UTILITY(U,$J,358.4,1308,0)
 ;;=HEALTH/BEHAVIOR ASSESSMENT/INTERVENTION^1^101
 ;;^UTILITY(U,$J,358.4,1309,0)
 ;;=TEAM CONFERENCE^9^101
 ;;^UTILITY(U,$J,358.4,1310,0)
 ;;=INTERACTIVE COMPLEXITY ADD-ON^2^101
 ;;^UTILITY(U,$J,358.4,1311,0)
 ;;=SUBSTANCE USE^8^101
 ;;^UTILITY(U,$J,358.4,1312,0)
 ;;=PSYCHIATRIC DIAGNOSTIC EVALUATION^5^101
 ;;^UTILITY(U,$J,358.4,1313,0)
 ;;=PROLONGED SERVICE^4^101
 ;;^UTILITY(U,$J,358.4,1314,0)
 ;;=TOBACCO CESSATION COUNSELING^10^101
 ;;^UTILITY(U,$J,358.4,1315,0)
 ;;=ABUSE,NEGLECT, & VIOLENCE^1^102
 ;;^UTILITY(U,$J,358.4,1316,0)
 ;;=ANXIETY DISORDERS^2^102
 ;;^UTILITY(U,$J,358.4,1317,0)
 ;;=BIPOLAR DISORDERS^3^102
 ;;^UTILITY(U,$J,358.4,1318,0)
 ;;=NEUROCOGNITIVE DISORDERS^17^102
 ;;^UTILITY(U,$J,358.4,1319,0)
 ;;=DEPRESSIVE DISORDERS^7^102
 ;;^UTILITY(U,$J,358.4,1320,0)
 ;;=DISSOCIATIVE DISORDERS ^9^102
 ;;^UTILITY(U,$J,358.4,1321,0)
 ;;=FEEDING & EATING DISORDERS^12^102
 ;;^UTILITY(U,$J,358.4,1322,0)
 ;;=EDUCATIONAL/OCCUPATIONAL PROBLEMS^10^102
 ;;^UTILITY(U,$J,358.4,1323,0)
 ;;=GENDER DYSPHORIA^14^102
 ;;^UTILITY(U,$J,358.4,1324,0)
 ;;=HOUSING/ECONOMIC PROBLEMS^15^102
 ;;^UTILITY(U,$J,358.4,1325,0)
 ;;=MEDICATION-INDUCED MOVEMENT DISORDERS^16^102
 ;;^UTILITY(U,$J,358.4,1326,0)
 ;;=OBSESSIVE-COMPULSIVE & RELATED DISORDERS^19^102
 ;;^UTILITY(U,$J,358.4,1327,0)
 ;;=PERSONAL HX & CURRENT CIRCUMSTANCES^22^102
 ;;^UTILITY(U,$J,358.4,1328,0)
 ;;=COUNSELING/MEDICAL ADVICE^5^102
 ;;^UTILITY(U,$J,358.4,1329,0)
 ;;=SOCIAL ENVIRONMENT PROBLEMS^29^102
 ;;^UTILITY(U,$J,358.4,1330,0)
 ;;=PARAPHILIC DISORDERS^21^102
 ;;^UTILITY(U,$J,358.4,1331,0)
 ;;=PERSONALITY DISORDERS^23^102
 ;;^UTILITY(U,$J,358.4,1332,0)
 ;;=CRIME/LEGAL SYSTEM PROBLEMS^6^102
 ;;^UTILITY(U,$J,358.4,1333,0)
 ;;=PSYCHOSOCIAL/PERSONAL/ENVIRONMENTAL^24^102
 ;;^UTILITY(U,$J,358.4,1334,0)
 ;;=RELATIONAL PROBLEMS^25^102
 ;;^UTILITY(U,$J,358.4,1335,0)
 ;;=SCHIZOPHRENIA/OTH PSYCHOTIC DISORDERS^26^102
 ;;^UTILITY(U,$J,358.4,1336,0)
 ;;=SEXUAL DYSFUNCTIONS^27^102
 ;;^UTILITY(U,$J,358.4,1337,0)
 ;;=SLEEP-WAKE DISORDERS^28^102
 ;;^UTILITY(U,$J,358.4,1338,0)
 ;;=SUBSTANCE USE DISORDER-ALCOHOL^31^102
 ;;^UTILITY(U,$J,358.4,1339,0)
 ;;=SUBSTANCE USE DISORDER-CANNABIS^33^102
 ;;^UTILITY(U,$J,358.4,1340,0)
 ;;=SUBSTANCE USE DISORDER-HALLUCINOGEN^34^102
 ;;^UTILITY(U,$J,358.4,1341,0)
 ;;=SUBSTANCE USE DISORDER-OPIOID^36^102
 ;;^UTILITY(U,$J,358.4,1342,0)
 ;;=SUBSTANCE USE DISORDER-SEDATIVE/HYPNOTIC^37^102
 ;;^UTILITY(U,$J,358.4,1343,0)
 ;;=SUBSTANCE USE DISORDER-TOBACCO^39^102
 ;;^UTILITY(U,$J,358.4,1344,0)
 ;;=TRAUMA/STRESSOR-RELATED DISORDERS^42^102
 ;;^UTILITY(U,$J,358.4,1345,0)
 ;;=SUBSTANCE USE DISORDER-INHALANT^35^102
 ;;^UTILITY(U,$J,358.4,1346,0)
 ;;=CLINICAL RESEARCH PROGRAM^4^102
 ;;^UTILITY(U,$J,358.4,1347,0)
 ;;=SOMATIC SYMPTOM & RELATED DISORDERS^30^102
 ;;^UTILITY(U,$J,358.4,1348,0)
 ;;=DISRUPTIVE,IMPULSE-CONTROL DISORDERS^8^102
 ;;^UTILITY(U,$J,358.4,1349,0)
 ;;=ELIMINATION DISORDERS^11^102
 ;;^UTILITY(U,$J,358.4,1350,0)
 ;;=GAMBLING DISORDER^13^102
 ;;^UTILITY(U,$J,358.4,1351,0)
 ;;=OTHER MENTAL DISORDERS^20^102
 ;;^UTILITY(U,$J,358.4,1352,0)
 ;;=NEURODEVELOPMENTAL DISORDERS^18^102
 ;;^UTILITY(U,$J,358.4,1353,0)
 ;;=SUBSTANCE USE DISORDER-CAFFEINE^32^102
 ;;^UTILITY(U,$J,358.4,1354,0)
 ;;=SUICIDAL IDEATIONS^41^102
 ;;^UTILITY(U,$J,358.4,1355,0)
 ;;=SUBSTANCE USE DISORDER-UNKNOWN SUBSTANCE^40^102
 ;;^UTILITY(U,$J,358.4,1356,0)
 ;;=SUBSTANCE USE DISORDER-STIMULANTS^38^102
 ;;^UTILITY(U,$J,358.4,1357,0)
 ;;=SPECIAL SERVICES^9^103
 ;;^UTILITY(U,$J,358.4,1358,0)
 ;;=CHEMO NURSE PROCEDURES^2^103
 ;;^UTILITY(U,$J,358.4,1359,0)
 ;;=INFUSION DRUGS^6^103
 ;;^UTILITY(U,$J,358.4,1360,0)
 ;;=BLOOD PRODUCTS^1^103
 ;;^UTILITY(U,$J,358.4,1361,0)
 ;;=DIAGNOSTIC PROCEDURES^3^103
 ;;^UTILITY(U,$J,358.4,1362,0)
 ;;=OTHER DRUGS^7^103
 ;;^UTILITY(U,$J,358.4,1363,0)
 ;;=IMMUNIZATION ADMINISTRATION^4^103
 ;;^UTILITY(U,$J,358.4,1364,0)
 ;;=IMMUNIZATIONS^5^103
 ;;^UTILITY(U,$J,358.4,1365,0)
 ;;=PROLONGED SERVICES^8^103
 ;;^UTILITY(U,$J,358.4,1366,0)
 ;;=NEW PATIENT^2^104
 ;;^UTILITY(U,$J,358.4,1367,0)
 ;;=ESTABLISHED PATIENT^1^104
 ;;^UTILITY(U,$J,358.4,1368,0)
 ;;=CONSULTATIONS^3^104
 ;;^UTILITY(U,$J,358.4,1369,0)
 ;;=ANEMIA^1^105
 ;;^UTILITY(U,$J,358.4,1370,0)
 ;;=ARTIFICIAL OPENING STATUS^2^105
 ;;^UTILITY(U,$J,358.4,1371,0)
 ;;=BREASTS and GYN NEOPLASMS^3^105
 ;;^UTILITY(U,$J,358.4,1372,0)
 ;;=COAGULATION DISORDERS^4^105
 ;;^UTILITY(U,$J,358.4,1373,0)
 ;;=COUNSELING AND SCREENING^5^105
 ;;^UTILITY(U,$J,358.4,1374,0)
 ;;=GENITOURINARY NEOPLASMS^6^105
 ;;^UTILITY(U,$J,358.4,1375,0)
 ;;=GI CANCER^7^105
 ;;^UTILITY(U,$J,358.4,1376,0)
 ;;=HEAD NECK AND LUNGS NEOPLASMS^8^105
 ;;^UTILITY(U,$J,358.4,1377,0)
 ;;=HEMOGLOBINOPATHIES^9^105
 ;;^UTILITY(U,$J,358.4,1378,0)
 ;;=LYMPHOID NEOPLASMS^10^105
 ;;^UTILITY(U,$J,358.4,1379,0)
 ;;=METASTATIC SITES^11^105
 ;;^UTILITY(U,$J,358.4,1380,0)
 ;;=MALIGNANT NEOPLASMS OF OTHER SITES^12^105
 ;;^UTILITY(U,$J,358.4,1381,0)
 ;;=MYELOID NEOPLASMS AND DISORDERS^13^105
 ;;^UTILITY(U,$J,358.4,1382,0)
 ;;=NEOPLASM OF UNCERTAIN BEHAVIOR^14^105
 ;;^UTILITY(U,$J,358.4,1383,0)
 ;;=NEOPLASM OF UNSPECIFIED NATURE^15^105
 ;;^UTILITY(U,$J,358.4,1384,0)
 ;;=PERSONAL HISTORY^16^105
 ;;^UTILITY(U,$J,358.4,1385,0)
 ;;=BLIND REHAB SERVICE CODES^1^106
 ;;^UTILITY(U,$J,358.4,1386,0)
 ;;=REFRACTION^6^106
 ;;^UTILITY(U,$J,358.4,1387,0)
 ;;=GLASSES/CONTACT LENSES^3^106
 ;;^UTILITY(U,$J,358.4,1388,0)
 ;;=VISUAL FIELDS^9^106
 ;;^UTILITY(U,$J,358.4,1389,0)
 ;;=PHOTOGRAPHY^5^106
 ;;^UTILITY(U,$J,358.4,1390,0)
 ;;=LOW VISION^4^106
 ;;^UTILITY(U,$J,358.4,1391,0)
 ;;=SPECIAL PROCEDURES^7^106
 ;;^UTILITY(U,$J,358.4,1392,0)
 ;;=TEAM MEETINGS/CASE MANAGEMENT^8^106
 ;;^UTILITY(U,$J,358.4,1393,0)
 ;;=DILATED EXAM DIABETIC^2^106
 ;;^UTILITY(U,$J,358.4,1394,0)
 ;;=ESTABLISHED PATIENT^1^107
 ;;^UTILITY(U,$J,358.4,1395,0)
 ;;=NEW PATIENT^2^107
 ;;^UTILITY(U,$J,358.4,1396,0)
 ;;=CONSULTATION^3^107
 ;;^UTILITY(U,$J,358.4,1397,0)
 ;;=PROLONGED SERVICE^4^107
 ;;^UTILITY(U,$J,358.4,1398,0)
 ;;=CONJUNCTIVA/CORNEA/SCLERA^2^108
 ;;^UTILITY(U,$J,358.4,1399,0)
 ;;=DIABETES^3^108
 ;;^UTILITY(U,$J,358.4,1400,0)
 ;;=GLAUCOMA^4^108
 ;;^UTILITY(U,$J,358.4,1401,0)
 ;;=LENS/VITREOUS^5^108
 ;;^UTILITY(U,$J,358.4,1402,0)
 ;;=LIDS/LASHES^6^108
 ;;^UTILITY(U,$J,358.4,1403,0)
 ;;=MACULA/OPTIC NERVE^7^108
 ;;^UTILITY(U,$J,358.4,1404,0)
 ;;=PUPILS/IRIS^8^108
 ;;^UTILITY(U,$J,358.4,1405,0)
 ;;=RETINA^9^108
 ;;^UTILITY(U,$J,358.4,1406,0)
 ;;=VF HA EOM OTHER^10^108
 ;;^UTILITY(U,$J,358.4,1407,0)
 ;;=VA VF^1^108
 ;;^UTILITY(U,$J,358.4,1408,0)
 ;;=GLOBE/OTHER^25^108
 ;;^UTILITY(U,$J,358.4,1409,0)
 ;;=REFRACTIVE DDX^26^108
 ;;^UTILITY(U,$J,358.4,1410,0)
 ;;=INJECTION/ASPIRATION^3^109
 ;;^UTILITY(U,$J,358.4,1411,0)
 ;;=FRACTURE DISLOCATION^2^109
 ;;^UTILITY(U,$J,358.4,1412,0)
 ;;=APPLICATION CASTS/STRAPPING^1^109
 ;;^UTILITY(U,$J,358.4,1413,0)
 ;;=OTHER/REMOVAL/REVISION^4^109
 ;;^UTILITY(U,$J,358.4,1414,0)
 ;;=SUPPLIES/SOFT GOODS^7^109
 ;;^UTILITY(U,$J,358.4,1415,0)
 ;;=SUPPLIES/DRUGS^6^109
