IBDEI1FP ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.4)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.4,869,0)
 ;;=PREVENTIVE MEDICINE CODES^20^90
 ;;^UTILITY(U,$J,358.4,870,0)
 ;;=CONTACT W/ HAZARDOUS SUBSTANCES^1^90
 ;;^UTILITY(U,$J,358.4,871,0)
 ;;=CAUSES OF INJURY (SECONDARY ONLY)^4^90
 ;;^UTILITY(U,$J,358.4,872,0)
 ;;=OTHER CODES^12^91
 ;;^UTILITY(U,$J,358.4,873,0)
 ;;=MH INTAKE CODES^11^91
 ;;^UTILITY(U,$J,358.4,874,0)
 ;;=OTHER PSYCHOTHERAPY^13^91
 ;;^UTILITY(U,$J,358.4,875,0)
 ;;=MEDICATION MANAGEMENT^10^91
 ;;^UTILITY(U,$J,358.4,876,0)
 ;;=HOME VISITS-EST PT^5^91
 ;;^UTILITY(U,$J,358.4,877,0)
 ;;=BIOFEEDBACK^1^91
 ;;^UTILITY(U,$J,358.4,878,0)
 ;;=CASE MANAGEMENT^2^91
 ;;^UTILITY(U,$J,358.4,879,0)
 ;;=HOME VISITS-NEW PT^6^91
 ;;^UTILITY(U,$J,358.4,880,0)
 ;;=INPT OR OTPT PSYTX W/O E&M^8^91
 ;;^UTILITY(U,$J,358.4,881,0)
 ;;=INTERACTIVE COMLEXITY^9^91
 ;;^UTILITY(U,$J,358.4,882,0)
 ;;=OTPT PSYTX W E&M^14^91
 ;;^UTILITY(U,$J,358.4,883,0)
 ;;=PSYCHOTHERAPY FOR CRISIS^19^91
 ;;^UTILITY(U,$J,358.4,884,0)
 ;;=PROLONGED SERVICES^18^91
 ;;^UTILITY(U,$J,358.4,885,0)
 ;;=DOMICILIARY-EST PT^3^91
 ;;^UTILITY(U,$J,358.4,886,0)
 ;;=DOMICILIARY-NEW PT^4^91
 ;;^UTILITY(U,$J,358.4,887,0)
 ;;=INJECTIONS/INJECTABLES^7^91
 ;;^UTILITY(U,$J,358.4,888,0)
 ;;=PREVENTIVE COUNSELING^15^91
 ;;^UTILITY(U,$J,358.4,889,0)
 ;;=PREVENTIVE MEDICINE-EST PT^16^91
 ;;^UTILITY(U,$J,358.4,890,0)
 ;;=PREVENTIVE MEDICINE-NEW PT^17^91
 ;;^UTILITY(U,$J,358.4,891,0)
 ;;=ESTABLISHED PATIENT^1^92
 ;;^UTILITY(U,$J,358.4,892,0)
 ;;=CONSULTATIONS^3^92
 ;;^UTILITY(U,$J,358.4,893,0)
 ;;=NEW PATIENT^2^92
 ;;^UTILITY(U,$J,358.4,894,0)
 ;;=INPATIENT CONSULTATIONS^4^92
 ;;^UTILITY(U,$J,358.4,895,0)
 ;;=INITIAL HOSPITAL CARE^5^92
 ;;^UTILITY(U,$J,358.4,896,0)
 ;;=HOSPITAL CARE DISCHARGE SERVICES^7^92
 ;;^UTILITY(U,$J,358.4,897,0)
 ;;=INITIAL OBSERVATION CARE^8^92
 ;;^UTILITY(U,$J,358.4,898,0)
 ;;=OBSERVATION CARE DISCHARGE SERVICES^10^92
 ;;^UTILITY(U,$J,358.4,899,0)
 ;;=INPT/OBS ADMIT/DISCHARGE SAME DATE^11^92
 ;;^UTILITY(U,$J,358.4,900,0)
 ;;=SUBSEQUENT HOSPITAL CARE^6^92
 ;;^UTILITY(U,$J,358.4,901,0)
 ;;=OBSERVATION SUBSEQUENT CARE^9^92
 ;;^UTILITY(U,$J,358.4,902,0)
 ;;=ADJUSTMENT DISORDERS^2^93
 ;;^UTILITY(U,$J,358.4,903,0)
 ;;=ANXIETY DISORDERS^4^93
 ;;^UTILITY(U,$J,358.4,904,0)
 ;;=AMNESTICS^3^93
 ;;^UTILITY(U,$J,358.4,905,0)
 ;;=ORGANIC DISORDERS^13^93
 ;;^UTILITY(U,$J,358.4,906,0)
 ;;=DEMENTIA^8^93
 ;;^UTILITY(U,$J,358.4,907,0)
 ;;=DELIRIUM^7^93
 ;;^UTILITY(U,$J,358.4,908,0)
 ;;=BIPOLAR DISORDERS^5^93
 ;;^UTILITY(U,$J,358.4,909,0)
 ;;=PSYCHOSIS/OTHER^17^93
 ;;^UTILITY(U,$J,358.4,910,0)
 ;;=PERSONALITY DISORDERS^15^93
 ;;^UTILITY(U,$J,358.4,911,0)
 ;;=SEXUAL DISORDERS^19^93
 ;;^UTILITY(U,$J,358.4,912,0)
 ;;=SLEEP DISORDERS^20^93
 ;;^UTILITY(U,$J,358.4,913,0)
 ;;=OTHER DISORDERS^14^93
 ;;^UTILITY(U,$J,358.4,914,0)
 ;;=SUBSTANCE RELATED DISORDERS^22^93
 ;;^UTILITY(U,$J,358.4,915,0)
 ;;=V CODES^23^93
 ;;^UTILITY(U,$J,358.4,916,0)
 ;;=PREVENTION/COUNSELING/SCREEN^16^93
 ;;^UTILITY(U,$J,358.4,917,0)
 ;;=DEPRESSION^9^93
 ;;^UTILITY(U,$J,358.4,918,0)
 ;;=MOOD DISORDERS (OTHER)^11^93
 ;;^UTILITY(U,$J,358.4,919,0)
 ;;=SCHIZOPHRENIA^18^93
 ;;^UTILITY(U,$J,358.4,920,0)
 ;;=SOMATIFORM DISORDERS^21^93
 ;;^UTILITY(U,$J,358.4,921,0)
 ;;=EATING DISORDERS^10^93
 ;;^UTILITY(U,$J,358.4,922,0)
 ;;=NEUROLEPTIC-INDUCED MOVEMENT DISORD^12^93
 ;;^UTILITY(U,$J,358.4,923,0)
 ;;=CONTACT W/ HAZARDOUS SUBSTANCES^1^93
 ;;^UTILITY(U,$J,358.4,924,0)
 ;;=BRAIN DISORDERS^6^93
 ;;^UTILITY(U,$J,358.4,925,0)
 ;;=OUTPT NEW PATIENT^3^94
 ;;^UTILITY(U,$J,358.4,926,0)
 ;;=OUTPT ESTABLISHED PATIENT^2^94
 ;;^UTILITY(U,$J,358.4,927,0)
 ;;=OUTPT CONSULTATIONS^4^94
