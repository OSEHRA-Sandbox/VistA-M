IBDEI1BL ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,22442,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22442,1,3,0)
 ;;=3^Melanoma in Situ,Unspec
 ;;^UTILITY(U,$J,358.3,22442,1,4,0)
 ;;=4^D03.9
 ;;^UTILITY(U,$J,358.3,22442,2)
 ;;=^5001908
 ;;^UTILITY(U,$J,358.3,22443,0)
 ;;=C45.0^^87^981^166
 ;;^UTILITY(U,$J,358.3,22443,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22443,1,3,0)
 ;;=3^Mesothelioma of Pleura
 ;;^UTILITY(U,$J,358.3,22443,1,4,0)
 ;;=4^C45.0
 ;;^UTILITY(U,$J,358.3,22443,2)
 ;;=^5001095
 ;;^UTILITY(U,$J,358.3,22444,0)
 ;;=C90.01^^87^981^168
 ;;^UTILITY(U,$J,358.3,22444,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22444,1,3,0)
 ;;=3^Multiple Myeloma,In Remission
 ;;^UTILITY(U,$J,358.3,22444,1,4,0)
 ;;=4^C90.01
 ;;^UTILITY(U,$J,358.3,22444,2)
 ;;=^267515
 ;;^UTILITY(U,$J,358.3,22445,0)
 ;;=C90.00^^87^981^169
 ;;^UTILITY(U,$J,358.3,22445,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22445,1,3,0)
 ;;=3^Multiple Myeloma,Not in Remission
 ;;^UTILITY(U,$J,358.3,22445,1,4,0)
 ;;=4^C90.00
 ;;^UTILITY(U,$J,358.3,22445,2)
 ;;=^5001752
 ;;^UTILITY(U,$J,358.3,22446,0)
 ;;=C94.6^^87^981^170
 ;;^UTILITY(U,$J,358.3,22446,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22446,1,3,0)
 ;;=3^Myelodysplastic Disease NEC
 ;;^UTILITY(U,$J,358.3,22446,1,4,0)
 ;;=4^C94.6
 ;;^UTILITY(U,$J,358.3,22446,2)
 ;;=^5001846
 ;;^UTILITY(U,$J,358.3,22447,0)
 ;;=D61.82^^87^981^171
 ;;^UTILITY(U,$J,358.3,22447,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22447,1,3,0)
 ;;=3^Myelophthisis
 ;;^UTILITY(U,$J,358.3,22447,1,4,0)
 ;;=4^D61.82
 ;;^UTILITY(U,$J,358.3,22447,2)
 ;;=^334037
 ;;^UTILITY(U,$J,358.3,22448,0)
 ;;=D47.9^^87^981^172
 ;;^UTILITY(U,$J,358.3,22448,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22448,1,3,0)
 ;;=3^Neop Lymphoid,Hematopoietic & Related Tissue,Uncertain Behavior
 ;;^UTILITY(U,$J,358.3,22448,1,4,0)
 ;;=4^D47.9
 ;;^UTILITY(U,$J,358.3,22448,2)
 ;;=^5002260
 ;;^UTILITY(U,$J,358.3,22449,0)
 ;;=D53.9^^87^981^176
 ;;^UTILITY(U,$J,358.3,22449,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22449,1,3,0)
 ;;=3^Nutritional Anemia,Unspec
 ;;^UTILITY(U,$J,358.3,22449,1,4,0)
 ;;=4^D53.9
 ;;^UTILITY(U,$J,358.3,22449,2)
 ;;=^5002298
 ;;^UTILITY(U,$J,358.3,22450,0)
 ;;=D47.4^^87^981^177
 ;;^UTILITY(U,$J,358.3,22450,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22450,1,3,0)
 ;;=3^Osteomyelofibrosis
 ;;^UTILITY(U,$J,358.3,22450,1,4,0)
 ;;=4^D47.4
 ;;^UTILITY(U,$J,358.3,22450,2)
 ;;=^5002259
 ;;^UTILITY(U,$J,358.3,22451,0)
 ;;=D59.1^^87^981^28
 ;;^UTILITY(U,$J,358.3,22451,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22451,1,3,0)
 ;;=3^Autoimmune Hemolytic Anemia,Other
 ;;^UTILITY(U,$J,358.3,22451,1,4,0)
 ;;=4^D59.1
 ;;^UTILITY(U,$J,358.3,22451,2)
 ;;=^5002324
 ;;^UTILITY(U,$J,358.3,22452,0)
 ;;=D51.3^^87^981^191
 ;;^UTILITY(U,$J,358.3,22452,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22452,1,3,0)
 ;;=3^Vitamin B12 Deficiency Anemia,Other
 ;;^UTILITY(U,$J,358.3,22452,1,4,0)
 ;;=4^D51.3
 ;;^UTILITY(U,$J,358.3,22452,2)
 ;;=^5002287
 ;;^UTILITY(U,$J,358.3,22453,0)
 ;;=D77.^^87^981^57
 ;;^UTILITY(U,$J,358.3,22453,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22453,1,3,0)
 ;;=3^Disorder of Blood/Blood-Forming Organs in Diseases Classified Elsewhere,Other
 ;;^UTILITY(U,$J,358.3,22453,1,4,0)
 ;;=4^D77.
 ;;^UTILITY(U,$J,358.3,22453,2)
 ;;=^5002396
 ;;^UTILITY(U,$J,358.3,22454,0)
 ;;=D58.2^^87^981^83
 ;;^UTILITY(U,$J,358.3,22454,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,22454,1,3,0)
 ;;=3^Hemoglobinopathies,Other
 ;;^UTILITY(U,$J,358.3,22454,1,4,0)
 ;;=4^D58.2
 ;;^UTILITY(U,$J,358.3,22454,2)
 ;;=^87629
 ;;^UTILITY(U,$J,358.3,22455,0)
 ;;=C88.8^^87^981^103
 ;;^UTILITY(U,$J,358.3,22455,1,0)
 ;;=^358.31IA^4^2
