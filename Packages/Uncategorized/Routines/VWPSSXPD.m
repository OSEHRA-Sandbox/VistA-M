VWPSSXPD ; VW/SMH - Update the Drug File and friends... ; 4/16/12 12:15pm
 ;;1.0;WorldVista Modifications;;;Build 1
 ; (C) Sam Habiel
 ; Licensed under AGPL
 ;
 ; This routine contains utilities to remove a drug file and install a new drug
 ; file in VISTA originating in another system. As of Build 3, you can use KIDS 
 ; to transport the file.
 ;
 ; Workflow (Manual, without using KIDS)
 ; On the Origin System:
 ; - Create a Global Output containing:
 ; ^PSDRUG      DRUG
 ; ^PS(50.7)    PHARMACY ORDERABLE ITEM
 ; ^PS(51.7)    DRUG TEXT
 ; ^PS(52.6)    IV ADDITIVES
 ; ^PS(52.7)    IV SOLUTIONS
 ; ^PS(50.4)    DRUG ELECTROLYTES
 ; 
 ; On the Destination System:
 ; 1. Call KILLDRUG to remove all Drug Data
 ; 2. Restore the global created above.
 ; 3. Call RESTOCK to sync CPRS files back with drug files.
 ;
 ; Workflow (using KIDS):
 ; On the Origin System:
 ; - Create a KIDS build that calls the follwing:
 ;  1. This Routine Name to call from top is the Environment Check for the Build
 ;  2. TRAN is the Pre-Transport Routine for your originating system drug data
 ;  3. PRE is the Pre-Init for the Destination System
 ;  4. POST is the Post-Init for the Destination System
 ; - Generate the KIDS Build. The file will have all the Drug Data from the Origin
 ;   System.
 ; On the Destination System:
 ; - Install the KIDS Build
 ;
 ; Limitations:
 ; - If the Administration Schedules from the originating system and destination
 ;   system don't match, you need to either change the schedules on the orderable
 ;   items, or make the schedules on the destination system the same as the
 ;   ones from incoming drug file, otherwise, the problems you run into is this:
 ;   - Orders do not calculate frequency correctly if the schedule is not present
 ;     in the destination system. This means that the proper number of pills cannot
 ;     be calculated.
 ;   - In Outpatient Pharmacy, Schedules do not expand into patient readable
 ;     instructions
 ; - If the National Drug Files are not up to date in the destination system, there
 ;   will be some broken pointers. All you have to do is update them.
 ; - If some drugs are linked with lab tests, these need to be re-linked. Preferably,
 ;   these should be removed prior to transporting the drug file.
 ;
 ; --- ()()() ---
 ;
 ; PEPs: KILLDRUG to remove all Drugs
 ;       RESTOCK to add the drugs back to CPRS
 ;
 ; KIDS EPs:
 ;       Top EP Fall through  -> Environment Check (call the routine from the top)
 ;       PRE -> Pre-Init for the Destination System
 ;       POST -> Post-Init for the Destination System
 ;       TRAN -> Pre-Transport Routine (to get data from original system)
 ;
 ; KIDS EPs call PEPs to do their work.
 ;
 ; Environment Check falls through from the top
 ;
 ; -- START KIDS HOOKS --
 ;
ENV ; Environment Check Routine; KIDS EP; top fallthough
 W $$CJ^XLFSTR("------   WARNING WARNING WARNING   -----",80),!!  ; Center Justify
 W "This package will delete your drug file and add a new drug file contained",!
 W "in the distribution.",!!
 W "If you have patient pharmacy data, this will CORRUPT your database.",!!
 N VWPROD S VWPROD=$$PROD^XUPROD() ; Check if a production acc; +ve val is yes
 W "You are running in a "_$S(VWPROD:"Production",1:"Test")_" Environment.",!!
 W "Are you sure you want to continue? Type a full YES/yes if you want to proceed)",!
 N X R "ANSWER: ",X:60
 S X=$$UP^XLFSTR(X) ; uppercase
 I X'="YES" S XPDQUIT=1 ; Quit if the user doesn't say yes
 QUIT
 ;
TRAN ; Pre-Transport routine for KIDS to ship off the 50, 50.7, 51.7 globals
 M @XPDGREF@("VWPSSXPD",50)=^PSDRUG             ; DRUG
 M @XPDGREF@("VWPSSXPD",50.7)=^PS(50.7)         ; PHARMACY ORDERABLE ITEM
 M @XPDGREF@("VWPSSXPD",51.7)=^PS(51.7)         ; DRUG TEXT
 M @XPDGREF@("VWPSSXPD",52.6)=^PS(52.6)         ; IV ADDITIVES
 M @XPDGREF@("VWPSSXPD",52.7)=^PS(52.7)         ; IV SOLUTIONS
 M @XPDGREF@("VWPSSXPD",50.4)=^PS(50.4)         ; DRUG ELECTROLYTES
 QUIT
 ;
PRE ; Pre-Init routine for KIDS
 DO KILLDRUG ; Remove old drugs from system.
 QUIT
 ;
POST ; Post-Init routine for KIDS
 ; Load new drugs into system
 M ^PSDRUG=@XPDGREF@("VWPSSXPD",50)             ; DRUG
 M ^PS(50.7)=@XPDGREF@("VWPSSXPD",50.7)         ; PHARMACY ORDERABLE ITEM
 M ^PS(51.7)=@XPDGREF@("VWPSSXPD",51.7)         ; DRUG TEXT
 M ^PS(52.6)=@XPDGREF@("VWPSSXPD",52.6)         ; IV ADDITIVES
 M ^PS(52.7)=@XPDGREF@("VWPSSXPD",52.7)         ; IV SOLUTIONS
 M ^PS(50.4)=@XPDGREF@("VWPSSXPD",50.4)         ; DRUG ELECTROLYTES
 ; Restock the CPRS files from the new drug files
 DO RESTOCK
 QUIT
 ;
 ; -- END KIDS HOOKS --
 ;
 ; -- BEGIN Public Entry Points --
KILLDRUG ; Remove all Drug Data. PEP. Use this to call the routine.
 D DT^DICRW ; Min FM Vars
 D MES^XPDUTL("Killing Drug (50)") D DRUG
 D MES^XPDUTL("Killing Pharmacy Orderable Item (OI) (50.7)") D PO
 D MES^XPDUTL("Killing Drug Text (51.7)") D DRUGTEXT
 D MES^XPDUTL("Killing IV Additives (52.6)") D IVADD
 D MES^XPDUTL("Killing IV Solutions (52.7)") D IVSOL
 D MES^XPDUTL("Killing Drug Electrolytes (50.4)") D DRUGELEC
 D MES^XPDUTL("Removing Pharmacy OIs from the Orderable Item (101.43)") D O
 D MES^XPDUTL("Syncing the Order Quick View (101.44)") D CPRS
 QUIT
 ;
RESTOCK ; Restock CPRS Orderable Items from new Drug & Pharmacy Orderable Item 
 ; File. Public Entry Point. 
 ; Call this after repopulating the drug file (50) and the pharmacy orderable 
 ; item file (50.7)
 N PSOIEN ; Looper variable
 D DT^DICRW ; Establish FM Basic Variables
 ; 
 ; Loop through Orderable Item file and call 
 ; 1. The Active/Inactive Updater for the Orderable Item
 ; 2. the protocol file updater to CPRS Files
 S PSOIEN=0 F  S PSOIEN=$O(^PS(50.7,PSOIEN)) Q:'PSOIEN  D 
 . D MES^XPDUTL("Syncing Pharamcy Orderable Item "_PSOIEN)
 . D EN^PSSPOIDT(PSOIEN),EN2^PSSHL1(PSOIEN,"MUP")
 D CPRS ; Update Orderable Item View files 
 QUIT
 ;
 ; -- END Public Entry Points --
 ; 
 ; -- The rest is private --
DRUG ; Kill Drug File; Private
 N %1 S %1=^PSDRUG(0)
 K ^PSDRUG
 S ^PSDRUG(0)=%1
 S $P(^PSDRUG(0),"^",3,4)=""
 QUIT
 ;
PO ; Kill Pharmacy Orderable Items; Private
 N %1 S %1=^PS(50.7,0)
 K ^PS(50.7)
 S ^PS(50.7,0)=%1
 S $P(^PS(50.7,0),"^",3,4)=""
 QUIT
 ;
DRUGTEXT ; Kill Drug Text Entries ; Private
 N %1 S %1=^PS(51.7,0)
 K ^PS(51.7)
 S ^PS(51.7,0)=%1
 S $P(^PS(51.7,0),"^",3,4)=""
 QUIT
 ;
IVADD ; Kill IV Additives ; Private
 N %1 S %1=^PS(52.6,0)
 K ^PS(52.6)
 S ^PS(52.6,0)=%1
 S $P(^PS(52.6,0),"^",3,4)=""
 QUIT
 ;
IVSOL ; Kill IV Solutions ; Private
 N %1 S %1=^PS(52.7,0)
 K ^PS(52.7)
 S ^PS(52.7,0)=%1
 S $P(^PS(52.7,0),"^",3,4)=""
 QUIT
 ;
DRUGELEC ; Kill Drug Electrolytes ; Private
 N %1 S %1=^PS(50.4,0)
 K ^PS(50.4)
 S ^PS(50.4,0)=%1
 S $P(^PS(50.4,0),"^",3,4)=""
 QUIT
 ;
O ; Kill off Pharamcy Order Items (Only!) in the Orderable Item file; Private
 N DA ; Used in For loop below
 N DIK S DIK="^ORD(101.43,"
 N I S I=0
 FOR  S I=$O(^ORD(101.43,"ID",I)) QUIT:I=""  DO
 . I I["PSP" S DA=$O(^ORD(101.43,"ID",I,"")) D ^DIK
 QUIT
 ;
CPRS ; Now, update the CPRS lists (sync with Orderable Item file) - 
 ; Uses a CPRS API to do this; Private
 ; Next 3 variables are required as inputs
 N ATTEMPT S ATTEMPT=0 ; Attempt to Update
 N UPDTIME S UPDTIME=$HOROLOG ; Update Time
 N DGNM ; Dialog Name
 ; IVA RX -> Additives; IVB RX -> Solutions
 ; IVM RX -> Inpatient Meds for Outpatients
 ; NV RX -> Non-VA Meds ; O RX -> Outpatient
 ; UD RX -> Unit Dose
 FOR DGNM="IVA RX","IVB RX","IVM RX","NV RX","O RX","UD RX" DO
 . D MES^XPDUTL(" --> Rebuilding "_DGNM)
 . D FVBLD^ORWUL
 QUIT
 ;
