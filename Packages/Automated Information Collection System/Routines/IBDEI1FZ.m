IBDEI1FZ ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.4,1471,0)
 ;;=STASIS^66^148
 ;;^UTILITY(U,$J,358.4,1472,0)
 ;;=ULCER^68^148
 ;;^UTILITY(U,$J,358.4,1473,0)
 ;;=ULCER-LOWER EXTREMITY^69^148
 ;;^UTILITY(U,$J,358.4,1474,0)
 ;;=URTICARIA^70^148
 ;;^UTILITY(U,$J,358.4,1475,0)
 ;;=VARICOSE VEINS^71^148
 ;;^UTILITY(U,$J,358.4,1476,0)
 ;;=WOUND^73^148
 ;;^UTILITY(U,$J,358.4,1477,0)
 ;;="A" MISC DIAGNOSIS^5.1^148
 ;;^UTILITY(U,$J,358.4,1478,0)
 ;;="B" MISC DIAGNOSIS^6.1^148
 ;;^UTILITY(U,$J,358.4,1479,0)
 ;;="C" MISC DIAGNOSIS^8.1^148
 ;;^UTILITY(U,$J,358.4,1480,0)
 ;;="D" MISC DIAGNOSIS^10.1^148
 ;;^UTILITY(U,$J,358.4,1481,0)
 ;;="E" MISC DIAGNOSIS^14.1^148
 ;;^UTILITY(U,$J,358.4,1482,0)
 ;;="F" MISC DIAGNOSIS^16.1^148
 ;;^UTILITY(U,$J,358.4,1483,0)
 ;;="H" MISC DIAGNOSIS^37^148
 ;;^UTILITY(U,$J,358.4,1484,0)
 ;;="I" MISC DIAGNOSIS^41^148
 ;;^UTILITY(U,$J,358.4,1485,0)
 ;;="K" MISC DIAGNOSIS^43^148
 ;;^UTILITY(U,$J,358.4,1486,0)
 ;;="L" MISC DIAGNOSIS^49^148
 ;;^UTILITY(U,$J,358.4,1487,0)
 ;;="M" MISC DIAGNOSIS^51^148
 ;;^UTILITY(U,$J,358.4,1488,0)
 ;;="N" MISC DIAGNOSIS^59^148
 ;;^UTILITY(U,$J,358.4,1489,0)
 ;;="P" MISC DIAGNOSIS^65^148
 ;;^UTILITY(U,$J,358.4,1490,0)
 ;;="X" MISC DIAGNOSIS^75^148
 ;;^UTILITY(U,$J,358.4,1491,0)
 ;;="V" MISC DIAGNOSIS^72^148
 ;;^UTILITY(U,$J,358.4,1492,0)
 ;;="W" MISC DIAGNOSIS^74^148
 ;;^UTILITY(U,$J,358.4,1493,0)
 ;;=AFTERCARE CODES^0^148
 ;;^UTILITY(U,$J,358.4,1494,0)
 ;;="S" MISC DIAGNOSIS^67^148
 ;;^UTILITY(U,$J,358.4,1495,0)
 ;;=CANCER (NOT MELANOMA)^6.5^148
 ;;^UTILITY(U,$J,358.4,1496,0)
 ;;=BOIL/FURUNCLE^5.5^148
 ;;^UTILITY(U,$J,358.4,1497,0)
 ;;=CHEILITIS^8.03^148
 ;;^UTILITY(U,$J,358.4,1498,0)
 ;;=CYST^8.07^148
 ;;^UTILITY(U,$J,358.4,1499,0)
 ;;=DRUG REACTION^10.02^148
 ;;^UTILITY(U,$J,358.4,1500,0)
 ;;=LENTIGO^45^148
 ;;^UTILITY(U,$J,358.4,1501,0)
 ;;="O" DIAGNOSES^60^148
 ;;^UTILITY(U,$J,358.4,1502,0)
 ;;=PSYCHO CUTEANOUS DERMATITIS^64.5^148
 ;;^UTILITY(U,$J,358.4,1503,0)
 ;;="R" DIAGNOSES^65.5^148
 ;;^UTILITY(U,$J,358.4,1504,0)
 ;;="T" DIAGNOSES^67.5^148
 ;;^UTILITY(U,$J,358.4,1505,0)
 ;;=SCREENING^65.6^148
 ;;^UTILITY(U,$J,358.4,1506,0)
 ;;=NEOPLASM-MALIGNANT^55^148
 ;;^UTILITY(U,$J,358.4,1507,0)
 ;;=PHYSICIAN TELEPHONE CODES^1^149
 ;;^UTILITY(U,$J,358.4,1508,0)
 ;;=NON-PHYSICIAN TELEPHONE CALLS^2^149
 ;;^UTILITY(U,$J,358.4,1509,0)
 ;;=TELEPHONE DIAGNOSES^1^150
 ;;^UTILITY(U,$J,358.4,1510,0)
 ;;=PENIS & SCROTUM^5^151
 ;;^UTILITY(U,$J,358.4,1511,0)
 ;;=URINARY TRACT DRAINAGE & IMAGING^7^151
 ;;^UTILITY(U,$J,358.4,1512,0)
 ;;=URODYNAMICS^8^151
 ;;^UTILITY(U,$J,358.4,1513,0)
 ;;=INJ & INTRAVESICAL MEDS^2^151
 ;;^UTILITY(U,$J,358.4,1514,0)
 ;;=CYSTOURETHROSCOPY^1^151
 ;;^UTILITY(U,$J,358.4,1515,0)
 ;;=PROSTATE BIOPSY & IMAGING^6^151
 ;;^UTILITY(U,$J,358.4,1516,0)
 ;;=NURSING PROCEDURES^4^151
 ;;^UTILITY(U,$J,358.4,1517,0)
 ;;=MISCELLANEOUS^3^151
 ;;^UTILITY(U,$J,358.4,1518,0)
 ;;=NEW PATIENT^2^152
 ;;^UTILITY(U,$J,358.4,1519,0)
 ;;=ESTABLISHED PATIENT^1^152
 ;;^UTILITY(U,$J,358.4,1520,0)
 ;;=CONSULTATIONS^3^152
 ;;^UTILITY(U,$J,358.4,1521,0)
 ;;=POST OP VISIT (GLOBAL)^4^152
 ;;^UTILITY(U,$J,358.4,1522,0)
 ;;=NEURO-UROLOGY^6^153
 ;;^UTILITY(U,$J,358.4,1523,0)
 ;;=BLADDER/URETHRA^1^153
 ;;^UTILITY(U,$J,358.4,1524,0)
 ;;=KIDNEY/URETER^4^153
 ;;^UTILITY(U,$J,358.4,1525,0)
 ;;=PENIS/EXTERNAL GENITALIA^7^153
 ;;^UTILITY(U,$J,358.4,1526,0)
 ;;=POST OP^8^153
 ;;^UTILITY(U,$J,358.4,1527,0)
 ;;=MISCELLANEOUS/SYMPTOMS^5^153
 ;;^UTILITY(U,$J,358.4,1528,0)
 ;;=INFECTIONS^3^153
 ;;^UTILITY(U,$J,358.4,1529,0)
 ;;=PROSTATE^9^153
 ;;^UTILITY(U,$J,358.4,1530,0)
 ;;=HISTORY/STATUS^2^153
 ;;^UTILITY(U,$J,358.4,1531,0)
 ;;=NEW PATIENT^2^154
