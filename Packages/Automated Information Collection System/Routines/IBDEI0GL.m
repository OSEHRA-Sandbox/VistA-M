IBDEI0GL ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.4,174,0)
 ;;=GENITOURINARY NEOPLASMS^6^23
 ;;^UTILITY(U,$J,358.4,175,0)
 ;;=BREAST & GYN NEOPLASMS^3^23
 ;;^UTILITY(U,$J,358.4,176,0)
 ;;=ARTIFICIAL OPENING STATUS^2^23
 ;;^UTILITY(U,$J,358.4,177,0)
 ;;=PERSONAL HX OF CANCER^15^23
 ;;^UTILITY(U,$J,358.4,178,0)
 ;;=NEOPLASMS OF UNCERTAIN BEHAVIOR^13^23
 ;;^UTILITY(U,$J,358.4,179,0)
 ;;=NEOPLASMS OF UNSPECIFIED NATURE^14^23
 ;;^UTILITY(U,$J,358.4,180,0)
 ;;=NEW PATIENT^2^24
 ;;^UTILITY(U,$J,358.4,181,0)
 ;;=ESTABLISHED PATIENT^1^24
 ;;^UTILITY(U,$J,358.4,182,0)
 ;;=CONSULTATIONS^3^24
 ;;^UTILITY(U,$J,358.4,183,0)
 ;;=MUSCULOSKELETAL/CONNECTIVE TISSUE^1^25
 ;;^UTILITY(U,$J,358.4,184,0)
 ;;=MISCELLANEOUS TREATMENT^4^26
 ;;^UTILITY(U,$J,358.4,185,0)
 ;;=CHIROPRACTIC MANIPULATIVE SERVICES^2^26
 ;;^UTILITY(U,$J,358.4,186,0)
 ;;=OSTEOPATHIC MANIPULATIVE TREATMENT^3^26
 ;;^UTILITY(U,$J,358.4,187,0)
 ;;=ACUPUNCTURE^1^26
 ;;^UTILITY(U,$J,358.4,188,0)
 ;;=SECONDARY V CODES AS APPROPRIATE^1^27
 ;;^UTILITY(U,$J,358.4,189,0)
 ;;=C&P EXAM^1^28
 ;;^UTILITY(U,$J,358.4,190,0)
 ;;=IDES C&P EXAM^3^28
 ;;^UTILITY(U,$J,358.4,191,0)
 ;;=ACE C&P EXAM^2^28
 ;;^UTILITY(U,$J,358.4,192,0)
 ;;=LENGTHY/COMPLEX C-FILE REVIEW^1^29
 ;;^UTILITY(U,$J,358.4,193,0)
 ;;=PRIMARY DIAGNOSIS^1^30
 ;;^UTILITY(U,$J,358.4,194,0)
 ;;=INPATIENT ADMISSION VISIT^2^31
 ;;^UTILITY(U,$J,358.4,195,0)
 ;;=INPATIENT CONSULTS^1^31
 ;;^UTILITY(U,$J,358.4,196,0)
 ;;=INPATIENT SUBSEQUENT VISIT^3^31
 ;;^UTILITY(U,$J,358.4,197,0)
 ;;=DISCHARGE VISIT^4^31
 ;;^UTILITY(U,$J,358.4,198,0)
 ;;=OBSERVATION ADMISSION^5^31
 ;;^UTILITY(U,$J,358.4,199,0)
 ;;=OBSERVATION DISCHARGE^6^31
 ;;^UTILITY(U,$J,358.4,200,0)
 ;;=INTENSIVE CARE^7^31
 ;;^UTILITY(U,$J,358.4,201,0)
 ;;=CARDIAC^2^32
 ;;^UTILITY(U,$J,358.4,202,0)
 ;;=ENDOCRINE/METABOLIC^5^32
 ;;^UTILITY(U,$J,358.4,203,0)
 ;;=PULMONARY^20^32
 ;;^UTILITY(U,$J,358.4,204,0)
 ;;=EENT^4^32
 ;;^UTILITY(U,$J,358.4,205,0)
 ;;=MUSCULOSKELETAL^13^32
 ;;^UTILITY(U,$J,358.4,206,0)
 ;;=NEUROLOGIC^14^32
 ;;^UTILITY(U,$J,358.4,207,0)
 ;;=MENTAL HEALTH^12^32
 ;;^UTILITY(U,$J,358.4,208,0)
 ;;=GASTROENTEROLOGY^6^32
 ;;^UTILITY(U,$J,358.4,209,0)
 ;;=GU / RENAL^7^32
 ;;^UTILITY(U,$J,358.4,210,0)
 ;;=CANCER^3^32
 ;;^UTILITY(U,$J,358.4,211,0)
 ;;=HEMATOLOGIC & LYMPHATIC^8^32
 ;;^UTILITY(U,$J,358.4,212,0)
 ;;=INFECTIOUS DISEASE^10^32
 ;;^UTILITY(U,$J,358.4,213,0)
 ;;=SKIN^24^32
 ;;^UTILITY(U,$J,358.4,214,0)
 ;;=SIGNS, SYMPTOMS, CONDITIONS^22^32
 ;;^UTILITY(U,$J,358.4,215,0)
 ;;=INTERSTITIAL LUNG DISEASE^11^32
 ;;^UTILITY(U,$J,358.4,216,0)
 ;;=PAIN^18^32
 ;;^UTILITY(U,$J,358.4,217,0)
 ;;=OTHER^17^32
 ;;^UTILITY(U,$J,358.4,218,0)
 ;;=COMMON PULMONARY DIAGNOSES^1^32
 ;;^UTILITY(U,$J,358.4,219,0)
 ;;=NEUROMUSCULAR/SKELETAL^15^32
 ;;^UTILITY(U,$J,358.4,220,0)
 ;;=OCCUPATIONAL^16^32
 ;;^UTILITY(U,$J,358.4,221,0)
 ;;=PLEURAL DISEASE^19^32
 ;;^UTILITY(U,$J,358.4,222,0)
 ;;=RADIATION^21^32
 ;;^UTILITY(U,$J,358.4,223,0)
 ;;=SKELETAL^23^32
 ;;^UTILITY(U,$J,358.4,224,0)
 ;;=HEPATIC & BILIARY^9^32
 ;;^UTILITY(U,$J,358.4,225,0)
 ;;=SUTURES/DRESSINGS^25^32
 ;;^UTILITY(U,$J,358.4,226,0)
 ;;=VASCULAR^26^32
 ;;^UTILITY(U,$J,358.4,227,0)
 ;;=BEDSIDE PROCEDURES^1^33
 ;;^UTILITY(U,$J,358.4,228,0)
 ;;=BLOODS,ARTERIAL & VENOUS^2^33
 ;;^UTILITY(U,$J,358.4,229,0)
 ;;=BRONCHOSCOPY^3^33
 ;;^UTILITY(U,$J,358.4,230,0)
 ;;=EXERCISE TESTING^4^33
 ;;^UTILITY(U,$J,358.4,231,0)
 ;;=OXIMETRY^5^33
 ;;^UTILITY(U,$J,358.4,232,0)
 ;;=PULMONARY FUCTION TESTING^6^33
 ;;^UTILITY(U,$J,358.4,233,0)
 ;;=RESPIRATORY THERAPY^7^33
 ;;^UTILITY(U,$J,358.4,234,0)
 ;;=THORACENTESIS^8^33
 ;;^UTILITY(U,$J,358.4,235,0)
 ;;=NEW PATIENT^2^34
 ;;^UTILITY(U,$J,358.4,236,0)
 ;;=ESTABLISHED PATIENT^1^34
 ;;^UTILITY(U,$J,358.4,237,0)
 ;;=CONSULTATIONS^3^34
 ;;^UTILITY(U,$J,358.4,238,0)
 ;;=POST-OP FOLLOW-UP^4^34
 ;;^UTILITY(U,$J,358.4,239,0)
 ;;=DEBRIDEMENT^1^35
 ;;^UTILITY(U,$J,358.4,240,0)
 ;;=DESTRUCTION-BENIGN OR PREMALIG LESIONS^2^35
 ;;^UTILITY(U,$J,358.4,241,0)
 ;;=DEST MAL LESION-TRUNK,ARMS,LEGS^3^35
 ;;^UTILITY(U,$J,358.4,242,0)
 ;;=EXC-BEN LESION-TRUNK,ARM,LEG^6^35
 ;;^UTILITY(U,$J,358.4,243,0)
 ;;=EXC-MAL LESION-TRUNK,ARM,LEG^9^35
 ;;^UTILITY(U,$J,358.4,244,0)
 ;;=INCISION & DRAINAGE^12^35
 ;;^UTILITY(U,$J,358.4,245,0)
 ;;=MISCELLANEOUS^13^35
 ;;^UTILITY(U,$J,358.4,246,0)
 ;;=NAILS^14^35
 ;;^UTILITY(U,$J,358.4,247,0)
 ;;=SHAVING-EPIDERMAL/TRUNK,ARM,LEG^15^35
 ;;^UTILITY(U,$J,358.4,248,0)
 ;;=REPAIR-SIMPLE-SCALP,NK,TRUNK^18^35
 ;;^UTILITY(U,$J,358.4,249,0)
 ;;=REPAIR-INTERM-SCALP,TRUNK^20^35
 ;;^UTILITY(U,$J,358.4,250,0)
 ;;=DEST MAL LES-SCALP,NK,HAND,FEET,GENITALS^4^35
 ;;^UTILITY(U,$J,358.4,251,0)
 ;;=DEST MAL LESION-FACE,MUCOUS^5^35
 ;;^UTILITY(U,$J,358.4,252,0)
 ;;=EXC-BEN LESION-SCALP,HANDS,FT,GENITALS^7^35
 ;;^UTILITY(U,$J,358.4,253,0)
 ;;=EXC-BEN LESION-FACE,MUCOUS^8^35
 ;;^UTILITY(U,$J,358.4,254,0)
 ;;=EXC-MAL LESION-SCALP,NK,HAND,FT,GENITALS^10^35
 ;;^UTILITY(U,$J,358.4,255,0)
 ;;=EXC-MAL LESION-FACE,MUCOUS^11^35
 ;;^UTILITY(U,$J,358.4,256,0)
 ;;=SHAVING-EPIDERM-SCALP,NK,HAND,FT,GENITAL^16^35
 ;;^UTILITY(U,$J,358.4,257,0)
 ;;=SHAVING-EPIDERM-FACE,MUCOUS^17^35
 ;;^UTILITY(U,$J,358.4,258,0)
 ;;=REPAIR-SIMPLE-FACE,MUCOUS^19^35
 ;;^UTILITY(U,$J,358.4,259,0)
 ;;=REPAIR-INTERM-NK,HAND,FT,GENITALIA^21^35
 ;;^UTILITY(U,$J,358.4,260,0)
 ;;=REPAIR-INTERM-FACE,MUCOUS^22^35
 ;;^UTILITY(U,$J,358.4,261,0)
 ;;=ABRASION w/o INFECTION^1^36
 ;;^UTILITY(U,$J,358.4,262,0)
 ;;=ABRASION w/ INFECTION^2^36
 ;;^UTILITY(U,$J,358.4,263,0)
 ;;=ABSCESS/CELLULITIS^3^36
 ;;^UTILITY(U,$J,358.4,264,0)
 ;;=ACNE^4^36
 ;;^UTILITY(U,$J,358.4,265,0)
 ;;=ALOPEICA^5^36
 ;;^UTILITY(U,$J,358.4,266,0)
 ;;=BURNS^6^36
 ;;^UTILITY(U,$J,358.4,267,0)
 ;;=CANDIDIASIS^7^36
 ;;^UTILITY(U,$J,358.4,268,0)
 ;;=CELLULITIS^8^36
 ;;^UTILITY(U,$J,358.4,269,0)
 ;;=DERMATITIS^9^36
 ;;^UTILITY(U,$J,358.4,270,0)
 ;;=DERMATOPHYTOSIS^10^36
 ;;^UTILITY(U,$J,358.4,271,0)
 ;;=ECZEMA^11^36
 ;;^UTILITY(U,$J,358.4,272,0)
 ;;=EDEMA^12^36
 ;;^UTILITY(U,$J,358.4,273,0)
 ;;=ERYTHEMA^13^36
 ;;^UTILITY(U,$J,358.4,274,0)
 ;;=EXPOSURE TO HEALTH HAZARDS^14^36
 ;;^UTILITY(U,$J,358.4,275,0)
 ;;=FOLLICULITIS^15^36
 ;;^UTILITY(U,$J,358.4,276,0)
 ;;=FURUNCLE^16^36
 ;;^UTILITY(U,$J,358.4,277,0)
 ;;=GRANULOMA^17^36
 ;;^UTILITY(U,$J,358.4,278,0)
 ;;=HERPES^35^36
 ;;^UTILITY(U,$J,358.4,279,0)
 ;;=HISTORY^36^36
 ;;^UTILITY(U,$J,358.4,280,0)
 ;;=ICHTHYOSIS^38^36
 ;;^UTILITY(U,$J,358.4,281,0)
 ;;=INSECT BITE w/o INFECTION^39^36
 ;;^UTILITY(U,$J,358.4,282,0)
 ;;=INSECT BITE w/INFECTION^40^36
 ;;^UTILITY(U,$J,358.4,283,0)
 ;;=KERATOSIS^42^36
 ;;^UTILITY(U,$J,358.4,284,0)
 ;;=LACERATION^44^36
 ;;^UTILITY(U,$J,358.4,285,0)
 ;;=LESION^46^36
 ;;^UTILITY(U,$J,358.4,286,0)
 ;;=LICHENIFICATION^47^36
 ;;^UTILITY(U,$J,358.4,287,0)
 ;;=LUPUS^48^36
 ;;^UTILITY(U,$J,358.4,288,0)
 ;;=MELANOMA^50^36
 ;;^UTILITY(U,$J,358.4,289,0)
 ;;=NAIL DISEASE^53^36
 ;;^UTILITY(U,$J,358.4,290,0)
 ;;=NEOPLASM-BENIGN^54^36
 ;;^UTILITY(U,$J,358.4,291,0)
 ;;=NEOPLASM-IN SITU^56^36
 ;;^UTILITY(U,$J,358.4,292,0)
 ;;=NEOPLASM-OTHER^57^36
 ;;^UTILITY(U,$J,358.4,293,0)
 ;;=NEURODERMATITIS^58^36
 ;;^UTILITY(U,$J,358.4,294,0)
 ;;=PEDICULOSIS^61^36
 ;;^UTILITY(U,$J,358.4,295,0)
 ;;=PITYRIASIS^62^36
 ;;^UTILITY(U,$J,358.4,296,0)
 ;;=PRURITUS^63^36
 ;;^UTILITY(U,$J,358.4,297,0)
 ;;=PSORIASIS^64^36
 ;;^UTILITY(U,$J,358.4,298,0)
 ;;=STASIS^66^36
 ;;^UTILITY(U,$J,358.4,299,0)
 ;;=ULCER^68^36
 ;;^UTILITY(U,$J,358.4,300,0)
 ;;=ULCER-LOWER EXTREMITY^69^36
 ;;^UTILITY(U,$J,358.4,301,0)
 ;;=URTICARIA^70^36
 ;;^UTILITY(U,$J,358.4,302,0)
 ;;=VARICOSE VEINS^71^36
 ;;^UTILITY(U,$J,358.4,303,0)
 ;;=WOUND^73^36
 ;;^UTILITY(U,$J,358.4,304,0)
 ;;="A" MISC DIAGNOSIS^5.1^36
 ;;^UTILITY(U,$J,358.4,305,0)
 ;;="B" MISC DIAGNOSIS^6.1^36
 ;;^UTILITY(U,$J,358.4,306,0)
 ;;="C" MISC DIAGNOSIS^8.1^36
 ;;^UTILITY(U,$J,358.4,307,0)
 ;;="D" MISC DIAGNOSIS^10.1^36
 ;;^UTILITY(U,$J,358.4,308,0)
 ;;="E" MISC DIAGNOSIS^14.1^36
 ;;^UTILITY(U,$J,358.4,309,0)
 ;;="F" MISC DIAGNOSIS^16.1^36
 ;;^UTILITY(U,$J,358.4,310,0)
 ;;="H" MISC DIAGNOSIS^37^36
 ;;^UTILITY(U,$J,358.4,311,0)
 ;;="I" MISC DIAGNOSIS^41^36
 ;;^UTILITY(U,$J,358.4,312,0)
 ;;="K" MISC DIAGNOSIS^43^36
 ;;^UTILITY(U,$J,358.4,313,0)
 ;;="L" MISC DIAGNOSIS^49^36
 ;;^UTILITY(U,$J,358.4,314,0)
 ;;="M" MISC DIAGNOSIS^51^36
 ;;^UTILITY(U,$J,358.4,315,0)
 ;;="N" MISC DIAGNOSIS^59^36
 ;;^UTILITY(U,$J,358.4,316,0)
 ;;="P" MISC DIAGNOSIS^65^36
 ;;^UTILITY(U,$J,358.4,317,0)
 ;;="X" MISC DIAGNOSIS^75^36
 ;;^UTILITY(U,$J,358.4,318,0)
 ;;="V" MISC DIAGNOSIS^72^36
 ;;^UTILITY(U,$J,358.4,319,0)
 ;;="W" MISC DIAGNOSIS^74^36
 ;;^UTILITY(U,$J,358.4,320,0)
 ;;=AFTERCARE CODES^0^36
 ;;^UTILITY(U,$J,358.4,321,0)
 ;;="S" MISC DIAGNOSIS^67^36
 ;;^UTILITY(U,$J,358.4,322,0)
 ;;=CANCER (NOT MELANOMA)^6.5^36
 ;;^UTILITY(U,$J,358.4,323,0)
 ;;=BOIL/FURUNCLE^5.5^36
 ;;^UTILITY(U,$J,358.4,324,0)
 ;;=CHEILITIS^8.03^36
 ;;^UTILITY(U,$J,358.4,325,0)
 ;;=CYST^8.07^36
 ;;^UTILITY(U,$J,358.4,326,0)
 ;;=DRUG REACTION^10.02^36
 ;;^UTILITY(U,$J,358.4,327,0)
 ;;="G" MISC DIAGNOSIS^17.5^36
 ;;^UTILITY(U,$J,358.4,328,0)
 ;;=LENTIGO^45^36
 ;;^UTILITY(U,$J,358.4,329,0)
 ;;="O" DIAGNOSES^60^36
 ;;^UTILITY(U,$J,358.4,330,0)
 ;;=PSYCHO CUTEANOUS DERMATITIS^64.5^36
 ;;^UTILITY(U,$J,358.4,331,0)
 ;;="R" DIAGNOSES^65.5^36
 ;;^UTILITY(U,$J,358.4,332,0)
 ;;="T" DIAGNOSES^67.5^36
 ;;^UTILITY(U,$J,358.4,333,0)
 ;;=SCREENING^65.6^36
 ;;^UTILITY(U,$J,358.4,334,0)
 ;;=NEOPLASM-MALIGNANT^55^36
 ;;^UTILITY(U,$J,358.4,335,0)
 ;;=NEW PATIENT^3^37
 ;;^UTILITY(U,$J,358.4,336,0)
 ;;=ESTABLISHED PATIENT^1^37
 ;;^UTILITY(U,$J,358.4,337,0)
 ;;=CONSULTATIONS^2^37
 ;;^UTILITY(U,$J,358.4,338,0)
 ;;=PREVENTIVE MEDICINE^4^37
 ;;^UTILITY(U,$J,358.4,339,0)
 ;;=PREVENTIVE MEDICINE COUNSELING^3^38
 ;;^UTILITY(U,$J,358.4,340,0)
 ;;=OTHER^4^38
 ;;^UTILITY(U,$J,358.4,341,0)
 ;;=EDUCATION/TRAINING^1^38
 ;;^UTILITY(U,$J,358.4,342,0)
 ;;=MEDICAL NUTRITION THERAPY^2^38
 ;;^UTILITY(U,$J,358.4,343,0)
 ;;=DIABETES W/O COMPLICATION^1^39
