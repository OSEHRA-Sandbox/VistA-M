IBDEI01L ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,1621,1,4,0)
 ;;=4^V53.32
 ;;^UTILITY(U,$J,358.3,1621,1,5,0)
 ;;=5^Reprogramming of AICD
 ;;^UTILITY(U,$J,358.3,1621,2)
 ;;=Reprogramming of AICD^303454
 ;;^UTILITY(U,$J,358.3,1622,0)
 ;;=V53.39^^20^140^12
 ;;^UTILITY(U,$J,358.3,1622,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1622,1,4,0)
 ;;=4^V53.39
 ;;^UTILITY(U,$J,358.3,1622,1,5,0)
 ;;=5^Cardiac Device Reprogramming
 ;;^UTILITY(U,$J,358.3,1622,2)
 ;;=^303455
 ;;^UTILITY(U,$J,358.3,1623,0)
 ;;=433.00^^20^141^19
 ;;^UTILITY(U,$J,358.3,1623,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1623,1,4,0)
 ;;=4^433.00
 ;;^UTILITY(U,$J,358.3,1623,1,5,0)
 ;;=5^Athero Basilar W/O CVA
 ;;^UTILITY(U,$J,358.3,1623,2)
 ;;=^295800
 ;;^UTILITY(U,$J,358.3,1624,0)
 ;;=433.01^^20^141^18
 ;;^UTILITY(U,$J,358.3,1624,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1624,1,4,0)
 ;;=4^433.01
 ;;^UTILITY(U,$J,358.3,1624,1,5,0)
 ;;=5^Athero Basilar W/CVA
 ;;^UTILITY(U,$J,358.3,1624,2)
 ;;=^295730
 ;;^UTILITY(U,$J,358.3,1625,0)
 ;;=433.10^^20^141^20
 ;;^UTILITY(U,$J,358.3,1625,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1625,1,4,0)
 ;;=4^433.10
 ;;^UTILITY(U,$J,358.3,1625,1,5,0)
 ;;=5^Athero Carotid W/CVA
 ;;^UTILITY(U,$J,358.3,1625,2)
 ;;=^295801
 ;;^UTILITY(U,$J,358.3,1626,0)
 ;;=433.11^^20^141^21
 ;;^UTILITY(U,$J,358.3,1626,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1626,1,4,0)
 ;;=4^433.11
 ;;^UTILITY(U,$J,358.3,1626,1,5,0)
 ;;=5^Athero Carotid W/CVA
 ;;^UTILITY(U,$J,358.3,1626,2)
 ;;=^295731
 ;;^UTILITY(U,$J,358.3,1627,0)
 ;;=433.20^^20^141^35
 ;;^UTILITY(U,$J,358.3,1627,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1627,1,4,0)
 ;;=4^433.20
 ;;^UTILITY(U,$J,358.3,1627,1,5,0)
 ;;=5^Athero Vertebral W/O Cva
 ;;^UTILITY(U,$J,358.3,1627,2)
 ;;=^295802
 ;;^UTILITY(U,$J,358.3,1628,0)
 ;;=433.21^^20^141^34
 ;;^UTILITY(U,$J,358.3,1628,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1628,1,4,0)
 ;;=4^433.21
 ;;^UTILITY(U,$J,358.3,1628,1,5,0)
 ;;=5^Athero Vertebral W/CVA
 ;;^UTILITY(U,$J,358.3,1628,2)
 ;;=^295732
 ;;^UTILITY(U,$J,358.3,1629,0)
 ;;=433.30^^20^141^32
 ;;^UTILITY(U,$J,358.3,1629,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1629,1,4,0)
 ;;=4^433.30
 ;;^UTILITY(U,$J,358.3,1629,1,5,0)
 ;;=5^Athero Multi/Bi W/O CVA
 ;;^UTILITY(U,$J,358.3,1629,2)
 ;;=^295803
 ;;^UTILITY(U,$J,358.3,1630,0)
 ;;=433.31^^20^141^22
 ;;^UTILITY(U,$J,358.3,1630,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1630,1,4,0)
 ;;=4^433.31
 ;;^UTILITY(U,$J,358.3,1630,1,5,0)
 ;;=5^Athero Ero Multi/Bi W/Cva
 ;;^UTILITY(U,$J,358.3,1630,2)
 ;;=^295733
 ;;^UTILITY(U,$J,358.3,1631,0)
 ;;=435.9^^20^141^48
 ;;^UTILITY(U,$J,358.3,1631,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1631,1,4,0)
 ;;=4^435.9
 ;;^UTILITY(U,$J,358.3,1631,1,5,0)
 ;;=5^TIA
 ;;^UTILITY(U,$J,358.3,1631,2)
 ;;=^21635
 ;;^UTILITY(U,$J,358.3,1632,0)
 ;;=440.0^^20^141^17
 ;;^UTILITY(U,$J,358.3,1632,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1632,1,4,0)
 ;;=4^440.0
 ;;^UTILITY(U,$J,358.3,1632,1,5,0)
 ;;=5^Athero Aorta
 ;;^UTILITY(U,$J,358.3,1632,2)
 ;;=^269759
 ;;^UTILITY(U,$J,358.3,1633,0)
 ;;=440.1^^20^141^33
 ;;^UTILITY(U,$J,358.3,1633,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1633,1,4,0)
 ;;=4^440.1
 ;;^UTILITY(U,$J,358.3,1633,1,5,0)
 ;;=5^Athero Renal
 ;;^UTILITY(U,$J,358.3,1633,2)
 ;;=^269760
 ;;^UTILITY(U,$J,358.3,1634,0)
 ;;=440.20^^20^141^23
 ;;^UTILITY(U,$J,358.3,1634,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1634,1,4,0)
 ;;=4^440.20
 ;;^UTILITY(U,$J,358.3,1634,1,5,0)
 ;;=5^Athero Exrem
 ;;^UTILITY(U,$J,358.3,1634,2)
 ;;=^294064
 ;;^UTILITY(U,$J,358.3,1635,0)
 ;;=440.21^^20^141^30
 ;;^UTILITY(U,$J,358.3,1635,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1635,1,4,0)
 ;;=4^440.21
 ;;^UTILITY(U,$J,358.3,1635,1,5,0)
 ;;=5^Athero Extrem Claud
 ;;^UTILITY(U,$J,358.3,1635,2)
 ;;=^293885
 ;;^UTILITY(U,$J,358.3,1636,0)
 ;;=440.22^^20^141^28
 ;;^UTILITY(U,$J,358.3,1636,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1636,1,4,0)
 ;;=4^440.22
 ;;^UTILITY(U,$J,358.3,1636,1,5,0)
 ;;=5^Athero Extr Rest Pain
 ;;^UTILITY(U,$J,358.3,1636,2)
 ;;=^293886
 ;;^UTILITY(U,$J,358.3,1637,0)
 ;;=440.23^^20^141^31
 ;;^UTILITY(U,$J,358.3,1637,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1637,1,4,0)
 ;;=4^440.23
 ;;^UTILITY(U,$J,358.3,1637,1,5,0)
 ;;=5^Athero Extrem Ulcer
 ;;^UTILITY(U,$J,358.3,1637,2)
 ;;=^295739
 ;;^UTILITY(U,$J,358.3,1638,0)
 ;;=440.24^^20^141^27
 ;;^UTILITY(U,$J,358.3,1638,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1638,1,4,0)
 ;;=4^440.24
 ;;^UTILITY(U,$J,358.3,1638,1,5,0)
 ;;=5^Athero Extr Gangrene
 ;;^UTILITY(U,$J,358.3,1638,2)
 ;;=^295740
 ;;^UTILITY(U,$J,358.3,1639,0)
 ;;=440.30^^20^141^29
 ;;^UTILITY(U,$J,358.3,1639,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1639,1,4,0)
 ;;=4^440.30
 ;;^UTILITY(U,$J,358.3,1639,1,5,0)
 ;;=5^Athero Extr Unsp Graft
 ;;^UTILITY(U,$J,358.3,1639,2)
 ;;=^303286
 ;;^UTILITY(U,$J,358.3,1640,0)
 ;;=440.31^^20^141^24
 ;;^UTILITY(U,$J,358.3,1640,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1640,1,4,0)
 ;;=4^440.31
 ;;^UTILITY(U,$J,358.3,1640,1,5,0)
 ;;=5^Athero Ext Aut Vein Gr
 ;;^UTILITY(U,$J,358.3,1640,2)
 ;;=^303287
 ;;^UTILITY(U,$J,358.3,1641,0)
 ;;=440.32^^20^141^25
 ;;^UTILITY(U,$J,358.3,1641,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1641,1,4,0)
 ;;=4^440.32
 ;;^UTILITY(U,$J,358.3,1641,1,5,0)
 ;;=5^Athero Ext Nonaut Grft
 ;;^UTILITY(U,$J,358.3,1641,2)
 ;;=^303288
 ;;^UTILITY(U,$J,358.3,1642,0)
 ;;=440.8^^20^141^26
 ;;^UTILITY(U,$J,358.3,1642,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1642,1,4,0)
 ;;=4^440.8
 ;;^UTILITY(U,$J,358.3,1642,1,5,0)
 ;;=5^Athero Ext Other Arter
 ;;^UTILITY(U,$J,358.3,1642,2)
 ;;=^11234
 ;;^UTILITY(U,$J,358.3,1643,0)
 ;;=441.00^^20^141^15
 ;;^UTILITY(U,$J,358.3,1643,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1643,1,4,0)
 ;;=4^441.00
 ;;^UTILITY(U,$J,358.3,1643,1,5,0)
 ;;=5^Aortic Dissect Unsp
 ;;^UTILITY(U,$J,358.3,1643,2)
 ;;=^35660
 ;;^UTILITY(U,$J,358.3,1644,0)
 ;;=441.01^^20^141^14
 ;;^UTILITY(U,$J,358.3,1644,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1644,1,4,0)
 ;;=4^441.01
 ;;^UTILITY(U,$J,358.3,1644,1,5,0)
 ;;=5^Aortic Dissect Thorac
 ;;^UTILITY(U,$J,358.3,1644,2)
 ;;=^303289
 ;;^UTILITY(U,$J,358.3,1645,0)
 ;;=441.02^^20^141^13
 ;;^UTILITY(U,$J,358.3,1645,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1645,1,4,0)
 ;;=4^441.02
 ;;^UTILITY(U,$J,358.3,1645,1,5,0)
 ;;=5^Aortic Dissect Abd
 ;;^UTILITY(U,$J,358.3,1645,2)
 ;;=^303290
 ;;^UTILITY(U,$J,358.3,1646,0)
 ;;=441.03^^20^141^11
 ;;^UTILITY(U,$J,358.3,1646,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1646,1,4,0)
 ;;=4^441.03
 ;;^UTILITY(U,$J,358.3,1646,1,5,0)
 ;;=5^Aort Dissect Thoracoabd
 ;;^UTILITY(U,$J,358.3,1646,2)
 ;;=^303291
 ;;^UTILITY(U,$J,358.3,1647,0)
 ;;=441.1^^20^141^51
 ;;^UTILITY(U,$J,358.3,1647,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1647,1,4,0)
 ;;=4^441.1
 ;;^UTILITY(U,$J,358.3,1647,1,5,0)
 ;;=5^Thoracic Aneurysm Rupt
 ;;^UTILITY(U,$J,358.3,1647,2)
 ;;=^269762
 ;;^UTILITY(U,$J,358.3,1648,0)
 ;;=441.2^^20^141^50
 ;;^UTILITY(U,$J,358.3,1648,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1648,1,4,0)
 ;;=4^441.2
 ;;^UTILITY(U,$J,358.3,1648,1,5,0)
 ;;=5^Thor Aneurysm W/O Rupt
 ;;^UTILITY(U,$J,358.3,1648,2)
 ;;=^269765
 ;;^UTILITY(U,$J,358.3,1649,0)
 ;;=441.3^^20^141^2
 ;;^UTILITY(U,$J,358.3,1649,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1649,1,4,0)
 ;;=4^441.3
 ;;^UTILITY(U,$J,358.3,1649,1,5,0)
 ;;=5^Abd Aneurysm Rupture
 ;;^UTILITY(U,$J,358.3,1649,2)
 ;;=^269766
 ;;^UTILITY(U,$J,358.3,1650,0)
 ;;=441.4^^20^141^3
 ;;^UTILITY(U,$J,358.3,1650,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1650,1,4,0)
 ;;=4^441.4
 ;;^UTILITY(U,$J,358.3,1650,1,5,0)
 ;;=5^Abd Aneurysm W/O Rupt
 ;;^UTILITY(U,$J,358.3,1650,2)
 ;;=^269769
 ;;^UTILITY(U,$J,358.3,1651,0)
 ;;=441.5^^20^141^12
 ;;^UTILITY(U,$J,358.3,1651,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1651,1,4,0)
 ;;=4^441.5
 ;;^UTILITY(U,$J,358.3,1651,1,5,0)
 ;;=5^Aortic Aneurysm Rupt
 ;;^UTILITY(U,$J,358.3,1651,2)
 ;;=^9279
 ;;^UTILITY(U,$J,358.3,1652,0)
 ;;=441.6^^20^141^52
 ;;^UTILITY(U,$J,358.3,1652,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1652,1,4,0)
 ;;=4^441.6
 ;;^UTILITY(U,$J,358.3,1652,1,5,0)
 ;;=5^Thoracoabd Aneur Rupt
 ;;^UTILITY(U,$J,358.3,1652,2)
 ;;=^295742
 ;;^UTILITY(U,$J,358.3,1653,0)
 ;;=441.7^^20^141^53
 ;;^UTILITY(U,$J,358.3,1653,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1653,1,4,0)
 ;;=4^441.7
 ;;^UTILITY(U,$J,358.3,1653,1,5,0)
 ;;=5^Thoracoabd Aneur-Rupt
 ;;^UTILITY(U,$J,358.3,1653,2)
 ;;=^295743
 ;;^UTILITY(U,$J,358.3,1654,0)
 ;;=442.0^^20^141^9
 ;;^UTILITY(U,$J,358.3,1654,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1654,1,4,0)
 ;;=4^442.0
 ;;^UTILITY(U,$J,358.3,1654,1,5,0)
 ;;=5^Aneurysm Artery Ue
 ;;^UTILITY(U,$J,358.3,1654,2)
 ;;=^269771
 ;;^UTILITY(U,$J,358.3,1655,0)
 ;;=442.1^^20^141^7
 ;;^UTILITY(U,$J,358.3,1655,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1655,1,4,0)
 ;;=4^442.1
 ;;^UTILITY(U,$J,358.3,1655,1,5,0)
 ;;=5^Aneurysm Artery Renal
 ;;^UTILITY(U,$J,358.3,1655,2)
 ;;=^269773
 ;;^UTILITY(U,$J,358.3,1656,0)
 ;;=442.2^^20^141^4
 ;;^UTILITY(U,$J,358.3,1656,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1656,1,4,0)
 ;;=4^442.2
 ;;^UTILITY(U,$J,358.3,1656,1,5,0)
 ;;=5^Aneurysm Artery Iliac
 ;;^UTILITY(U,$J,358.3,1656,2)
 ;;=^269775
 ;;^UTILITY(U,$J,358.3,1657,0)
 ;;=442.3^^20^141^5
 ;;^UTILITY(U,$J,358.3,1657,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1657,1,4,0)
 ;;=4^442.3
 ;;^UTILITY(U,$J,358.3,1657,1,5,0)
 ;;=5^Aneurysm Artery Le
 ;;^UTILITY(U,$J,358.3,1657,2)
 ;;=^269777
 ;;^UTILITY(U,$J,358.3,1658,0)
 ;;=442.81^^20^141^6
 ;;^UTILITY(U,$J,358.3,1658,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1658,1,4,0)
 ;;=4^442.81
 ;;^UTILITY(U,$J,358.3,1658,1,5,0)
 ;;=5^Aneurysm Artery Neck
 ;;^UTILITY(U,$J,358.3,1658,2)
 ;;=^269779
 ;;^UTILITY(U,$J,358.3,1659,0)
 ;;=442.83^^20^141^8
 ;;^UTILITY(U,$J,358.3,1659,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,1659,1,4,0)
 ;;=4^442.83
 ;;^UTILITY(U,$J,358.3,1659,1,5,0)
 ;;=5^Aneurysm Artery Splen
