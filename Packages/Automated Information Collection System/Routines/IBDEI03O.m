IBDEI03O ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,4555,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4555,1,4,0)
 ;;=4^V03.82
 ;;^UTILITY(U,$J,358.3,4555,1,5,0)
 ;;=5^Pneumovax
 ;;^UTILITY(U,$J,358.3,4555,2)
 ;;=^303387
 ;;^UTILITY(U,$J,358.3,4556,0)
 ;;=V06.5^^41^368^11
 ;;^UTILITY(U,$J,358.3,4556,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4556,1,4,0)
 ;;=4^V06.5
 ;;^UTILITY(U,$J,358.3,4556,1,5,0)
 ;;=5^TD Vaccination
 ;;^UTILITY(U,$J,358.3,4556,2)
 ;;=^330094
 ;;^UTILITY(U,$J,358.3,4557,0)
 ;;=V06.1^^41^368^12
 ;;^UTILITY(U,$J,358.3,4557,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4557,1,4,0)
 ;;=4^V06.1
 ;;^UTILITY(U,$J,358.3,4557,1,5,0)
 ;;=5^TdaP Vaccination
 ;;^UTILITY(U,$J,358.3,4557,2)
 ;;=^330093
 ;;^UTILITY(U,$J,358.3,4558,0)
 ;;=V05.3^^41^368^3
 ;;^UTILITY(U,$J,358.3,4558,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4558,1,4,0)
 ;;=4^V05.3
 ;;^UTILITY(U,$J,358.3,4558,1,5,0)
 ;;=5^Hepatitis A Vaccination
 ;;^UTILITY(U,$J,358.3,4558,2)
 ;;=^295814
 ;;^UTILITY(U,$J,358.3,4559,0)
 ;;=V05.3^^41^368^4
 ;;^UTILITY(U,$J,358.3,4559,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4559,1,4,0)
 ;;=4^V05.3
 ;;^UTILITY(U,$J,358.3,4559,1,5,0)
 ;;=5^Hepatitis B Vaccination
 ;;^UTILITY(U,$J,358.3,4559,2)
 ;;=^295814
 ;;^UTILITY(U,$J,358.3,4560,0)
 ;;=V05.3^^41^368^5
 ;;^UTILITY(U,$J,358.3,4560,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4560,1,4,0)
 ;;=4^V05.3
 ;;^UTILITY(U,$J,358.3,4560,1,5,0)
 ;;=5^Hepatitis C Vaccination
 ;;^UTILITY(U,$J,358.3,4560,2)
 ;;=^295814
 ;;^UTILITY(U,$J,358.3,4561,0)
 ;;=V03.81^^41^368^1
 ;;^UTILITY(U,$J,358.3,4561,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4561,1,4,0)
 ;;=4^V03.81
 ;;^UTILITY(U,$J,358.3,4561,1,5,0)
 ;;=5^HIB Vaccination
 ;;^UTILITY(U,$J,358.3,4561,2)
 ;;=^295181
 ;;^UTILITY(U,$J,358.3,4562,0)
 ;;=V04.89^^41^368^2
 ;;^UTILITY(U,$J,358.3,4562,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4562,1,4,0)
 ;;=4^V04.89
 ;;^UTILITY(U,$J,358.3,4562,1,5,0)
 ;;=5^HPV Vaccination
 ;;^UTILITY(U,$J,358.3,4562,2)
 ;;=^329966
 ;;^UTILITY(U,$J,358.3,4563,0)
 ;;=V03.89^^41^368^9
 ;;^UTILITY(U,$J,358.3,4563,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4563,1,4,0)
 ;;=4^V03.89
 ;;^UTILITY(U,$J,358.3,4563,1,5,0)
 ;;=5^Meningococcus Vaccination
 ;;^UTILITY(U,$J,358.3,4563,2)
 ;;=^303388
 ;;^UTILITY(U,$J,358.3,4564,0)
 ;;=V06.4^^41^368^7
 ;;^UTILITY(U,$J,358.3,4564,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4564,1,4,0)
 ;;=4^V06.4
 ;;^UTILITY(U,$J,358.3,4564,1,5,0)
 ;;=5^MMR Vaccination
 ;;^UTILITY(U,$J,358.3,4564,2)
 ;;=^295194
 ;;^UTILITY(U,$J,358.3,4565,0)
 ;;=V04.89^^41^368^15
 ;;^UTILITY(U,$J,358.3,4565,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4565,1,4,0)
 ;;=4^V04.89
 ;;^UTILITY(U,$J,358.3,4565,1,5,0)
 ;;=5^Zoster Vaccination
 ;;^UTILITY(U,$J,358.3,4565,2)
 ;;=^329966
 ;;^UTILITY(U,$J,358.3,4566,0)
 ;;=V05.4^^41^368^14
 ;;^UTILITY(U,$J,358.3,4566,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4566,1,4,0)
 ;;=4^V05.4
 ;;^UTILITY(U,$J,358.3,4566,1,5,0)
 ;;=5^Varicella Vaccination
 ;;^UTILITY(U,$J,358.3,4566,2)
 ;;=^295815
 ;;^UTILITY(U,$J,358.3,4567,0)
 ;;=V04.2^^41^368^8
 ;;^UTILITY(U,$J,358.3,4567,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4567,1,4,0)
 ;;=4^V04.2
 ;;^UTILITY(U,$J,358.3,4567,1,5,0)
 ;;=5^Measles Vaccination
 ;;^UTILITY(U,$J,358.3,4567,2)
 ;;=^124883
 ;;^UTILITY(U,$J,358.3,4568,0)
 ;;=V03.2^^41^368^13
 ;;^UTILITY(U,$J,358.3,4568,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,4568,1,4,0)
 ;;=4^V03.2
 ;;^UTILITY(U,$J,358.3,4568,1,5,0)
 ;;=5^Tuberculosis Vaccination
 ;;^UTILITY(U,$J,358.3,4568,2)
 ;;=^295178
 ;;^UTILITY(U,$J,358.3,4569,0)
 ;;=90471^^42^369^1^^^^1
 ;;^UTILITY(U,$J,358.3,4569,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4569,1,2,0)
 ;;=2^90471
 ;;^UTILITY(U,$J,358.3,4569,1,3,0)
 ;;=3^Immunization Admin (Use w/ Vaccs)
 ;;^UTILITY(U,$J,358.3,4570,0)
 ;;=90472^^42^369^2^^^^1
 ;;^UTILITY(U,$J,358.3,4570,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4570,1,2,0)
 ;;=2^90472
 ;;^UTILITY(U,$J,358.3,4570,1,3,0)
 ;;=3^Immunization Admin, ea add
 ;;^UTILITY(U,$J,358.3,4571,0)
 ;;=90632^^42^369^3^^^^1
 ;;^UTILITY(U,$J,358.3,4571,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4571,1,2,0)
 ;;=2^90632
 ;;^UTILITY(U,$J,358.3,4571,1,3,0)
 ;;=3^Hepatitis A Vaccine
 ;;^UTILITY(U,$J,358.3,4572,0)
 ;;=90746^^42^369^5^^^^1
 ;;^UTILITY(U,$J,358.3,4572,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4572,1,2,0)
 ;;=2^90746
 ;;^UTILITY(U,$J,358.3,4572,1,3,0)
 ;;=3^Hepatitis B Vaccine
 ;;^UTILITY(U,$J,358.3,4573,0)
 ;;=90636^^42^369^4^^^^1
 ;;^UTILITY(U,$J,358.3,4573,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4573,1,2,0)
 ;;=2^90636
 ;;^UTILITY(U,$J,358.3,4573,1,3,0)
 ;;=3^Hepatitis A&B Vaccine
 ;;^UTILITY(U,$J,358.3,4574,0)
 ;;=90707^^42^369^7^^^^1
 ;;^UTILITY(U,$J,358.3,4574,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4574,1,2,0)
 ;;=2^90707
 ;;^UTILITY(U,$J,358.3,4574,1,3,0)
 ;;=3^MMR Vaccine
 ;;^UTILITY(U,$J,358.3,4575,0)
 ;;=90658^^42^369^6^^^^1
 ;;^UTILITY(U,$J,358.3,4575,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4575,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,4575,1,3,0)
 ;;=3^Influenza Vaccine
 ;;^UTILITY(U,$J,358.3,4576,0)
 ;;=90732^^42^369^8^^^^1
 ;;^UTILITY(U,$J,358.3,4576,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4576,1,2,0)
 ;;=2^90732
 ;;^UTILITY(U,$J,358.3,4576,1,3,0)
 ;;=3^Pneumococcal Vaccine
 ;;^UTILITY(U,$J,358.3,4577,0)
 ;;=90715^^42^369^10^^^^1
 ;;^UTILITY(U,$J,358.3,4577,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4577,1,2,0)
 ;;=2^90715
 ;;^UTILITY(U,$J,358.3,4577,1,3,0)
 ;;=3^TdaP Vaccine 
 ;;^UTILITY(U,$J,358.3,4578,0)
 ;;=90716^^42^369^11^^^^1
 ;;^UTILITY(U,$J,358.3,4578,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4578,1,2,0)
 ;;=2^90716
 ;;^UTILITY(U,$J,358.3,4578,1,3,0)
 ;;=3^Varicella Vaccine
 ;;^UTILITY(U,$J,358.3,4579,0)
 ;;=90714^^42^369^9^^^^1
 ;;^UTILITY(U,$J,358.3,4579,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4579,1,2,0)
 ;;=2^90714
 ;;^UTILITY(U,$J,358.3,4579,1,3,0)
 ;;=3^TD Vaccine
 ;;^UTILITY(U,$J,358.3,4580,0)
 ;;=86485^^42^370^1^^^^1
 ;;^UTILITY(U,$J,358.3,4580,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4580,1,2,0)
 ;;=2^86485
 ;;^UTILITY(U,$J,358.3,4580,1,3,0)
 ;;=3^Candida
 ;;^UTILITY(U,$J,358.3,4581,0)
 ;;=86580^^42^370^6^^^^1
 ;;^UTILITY(U,$J,358.3,4581,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4581,1,2,0)
 ;;=2^86580
 ;;^UTILITY(U,$J,358.3,4581,1,3,0)
 ;;=3^TB
 ;;^UTILITY(U,$J,358.3,4582,0)
 ;;=95024^^42^370^4^^^^1
 ;;^UTILITY(U,$J,358.3,4582,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4582,1,2,0)
 ;;=2^95024
 ;;^UTILITY(U,$J,358.3,4582,1,3,0)
 ;;=3^Intradermals (each)
 ;;^UTILITY(U,$J,358.3,4583,0)
 ;;=86510^^42^370^3^^^^1
 ;;^UTILITY(U,$J,358.3,4583,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4583,1,2,0)
 ;;=2^86510
 ;;^UTILITY(U,$J,358.3,4583,1,3,0)
 ;;=3^Histoplasmosis
 ;;^UTILITY(U,$J,358.3,4584,0)
 ;;=86486^^42^370^5^^^^1
 ;;^UTILITY(U,$J,358.3,4584,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4584,1,2,0)
 ;;=2^86486
 ;;^UTILITY(U,$J,358.3,4584,1,3,0)
 ;;=3^Mumps
 ;;^UTILITY(U,$J,358.3,4585,0)
 ;;=86706^^42^370^2^^^^1
 ;;^UTILITY(U,$J,358.3,4585,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4585,1,2,0)
 ;;=2^86706
 ;;^UTILITY(U,$J,358.3,4585,1,3,0)
 ;;=3^Hep B Surface Antibody
 ;;^UTILITY(U,$J,358.3,4586,0)
 ;;=12001^^42^371^5^^^^1
 ;;^UTILITY(U,$J,358.3,4586,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4586,1,2,0)
 ;;=2^12001
 ;;^UTILITY(U,$J,358.3,4586,1,3,0)
 ;;=3^Trunk/Ext/Scalp <2.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4587,0)
 ;;=12002^^42^371^6^^^^1
 ;;^UTILITY(U,$J,358.3,4587,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4587,1,2,0)
 ;;=2^12002
 ;;^UTILITY(U,$J,358.3,4587,1,3,0)
 ;;=3^Trunk/Ext/Scalp 2.6-7.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4588,0)
 ;;=12011^^42^371^1^^^^1
 ;;^UTILITY(U,$J,358.3,4588,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4588,1,2,0)
 ;;=2^12011
 ;;^UTILITY(U,$J,358.3,4588,1,3,0)
 ;;=3^Face/Ears/Lips <2.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4589,0)
 ;;=12004^^42^371^7^^^^1
 ;;^UTILITY(U,$J,358.3,4589,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4589,1,2,0)
 ;;=2^12004
 ;;^UTILITY(U,$J,358.3,4589,1,3,0)
 ;;=3^Trunk/Ext/Scalp 7.6-12.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4590,0)
 ;;=12013^^42^371^2^^^^1
 ;;^UTILITY(U,$J,358.3,4590,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4590,1,2,0)
 ;;=2^12013
 ;;^UTILITY(U,$J,358.3,4590,1,3,0)
 ;;=3^Face/Ears/Lips 2.6-5.0cm,Simp
 ;;^UTILITY(U,$J,358.3,4591,0)
 ;;=12014^^42^371^3^^^^1
 ;;^UTILITY(U,$J,358.3,4591,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4591,1,2,0)
 ;;=2^12014
 ;;^UTILITY(U,$J,358.3,4591,1,3,0)
 ;;=3^Face/Ears/Lips 5.1-7.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4592,0)
 ;;=12015^^42^371^4^^^^1
 ;;^UTILITY(U,$J,358.3,4592,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4592,1,2,0)
 ;;=2^12015
 ;;^UTILITY(U,$J,358.3,4592,1,3,0)
 ;;=3^Face/Ears/Lips 7.6-12.5cm,Simp
 ;;^UTILITY(U,$J,358.3,4593,0)
 ;;=92506^^42^372^1^^^^1
 ;;^UTILITY(U,$J,358.3,4593,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4593,1,2,0)
 ;;=2^92506
 ;;^UTILITY(U,$J,358.3,4593,1,3,0)
 ;;=3^Speech/Hearing Evaluation
 ;;^UTILITY(U,$J,358.3,4594,0)
 ;;=96372^^42^372^2^^^^1
 ;;^UTILITY(U,$J,358.3,4594,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4594,1,2,0)
 ;;=2^96372
 ;;^UTILITY(U,$J,358.3,4594,1,3,0)
 ;;=3^Ther/Proph/Diag Inj SC/IM
 ;;^UTILITY(U,$J,358.3,4595,0)
 ;;=10120^^42^373^1^^^^1
 ;;^UTILITY(U,$J,358.3,4595,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4595,1,2,0)
 ;;=2^10120
 ;;^UTILITY(U,$J,358.3,4595,1,3,0)
 ;;=3^Remove FB,SQ Tissue,Simp
 ;;^UTILITY(U,$J,358.3,4596,0)
 ;;=10121^^42^373^2^^^^1
 ;;^UTILITY(U,$J,358.3,4596,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4596,1,2,0)
 ;;=2^10121
 ;;^UTILITY(U,$J,358.3,4596,1,3,0)
 ;;=3^Remove FB,SQ Tissue,Compl
 ;;^UTILITY(U,$J,358.3,4597,0)
 ;;=65205^^42^373^3^^^^1
 ;;^UTILITY(U,$J,358.3,4597,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,4597,1,2,0)
 ;;=2^65205
 ;;^UTILITY(U,$J,358.3,4597,1,3,0)
 ;;=3^Remove FB,Conjunc,External Eye
 ;;^UTILITY(U,$J,358.3,4598,0)
 ;;=65210^^42^373^4^^^^1
