      PROGRAM WEDOG2
C-Title  : Program WEDOG2
C-Purpose: Calculate 2-group reaction rates from WIMS output
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1993)
C-Version: 1993 Original code
C-V  00/06 Allow Gr.1 flux from CELL edit for homog.cases (A.Trkov)
C-V  00/12 Allow for the absence of reaction rates (k-eff only)
C-V  01/01 Minor fix (D.L.Aldama).
C-V  01/02 Identify materials in "Reactions edit" by Mat.ID (A.Trkov).
C-V  01/05 Define spectral indices for Th232-U233 system (A.Trkov).
C-M
C-M  Manual for the program WEDOG2
C-M  =============================
C-M
C-M  WIMS output is processed. It is assumed that 2-group reaction
C-M  rates have been requested, namely for the principal fuel
C-M  constituents (U-235 and U-238 or U-233 and Th-232).
C-M  Reaction rates are identified from the last three digits of
C-M  the material ident number (i.e. 235 for U-235 and 238 for
C-M  U-238 or 233 for U-233 and 232 for Th-232, respectively).
C-M  This is necessary if more reactions are specified
C-M  on input or when the nuclides are ordered differently in
C-M  the library (for example, the WIMS-ANL library). A correction
C-M  for the (n,2n) reaction is made, making use of the calculated
C-M  flux in the fuel. Since the (n,2n) cross section is not
C-M  available from the WIMS library and to simplify the code, the
C-M  group-1 (n,2n) cross sections for U-235, U-238, U-233, Th-232
C-M  are defined with the DATA statement in the Z2N variable. The
C-M  cross sections were derived from the ENDF/B libraries. The
C-M  sensitivity of the integral parameters to the precise values
C-M  of the (n,2n) cross section should not be very large, but the
C-M  user should check for any significant differences of the cross
C-M  sections for the library currently being processed.
C-M  NOTE: Cross sections for the (n,2n) reactions must be consistent
C-M        with the transport group structure. For example, if calcul-
C-M        ations are done in full 69 groups it is sufficient to enter
C-M        gr.1 cross section of the 69 group set. If the first
C-M        transport group spans over several groups, an appropriate
C-M        correction to the (n,2n) cross sections must be made.
C-M
C-M    Two output files are produced: the extended output WEDOUT
C-M  contains the uncorrected and (n,2n) corrected reaction rates for
C-M  the K-inf and the K-eff edit and also the spectral indices. The
C-M  summary output WEDSMR contains only k-eff and the spectral indices
C-M  from the K-eff edit in 10-column format.
C-M
C-M  Files used: (Unit / Default filename / Description)
C-M    1  WIMOUT  WIMS output file to be processed
C-M    2  WEDSMR  WEDOG2 summary output file
C-M    3  WEDOUT  WEDOG2 descriptive output filename
C-M
C-M  Input instructions:
C-M  The WIMS output filename is specified from keyboard interactively.
C-M  The output files are defined outomatically by changing the last
C-M  three non-blank characters to "WED" and "SMR" respectively.
C-M    Alternately, (if blank filename or EOF is specified) the default
C-M  filenames are used. This option may also be used to define files
C-M  with the ASSIGN statement.
C-M  Example:
C-M    ASSIGN/USER "wimsoutput" WIMOUT
C-M    ASSIGN/USER "wedoutput"  WEDOUT
C-M    ASSIGN/USER "wedsummary" WEDSMR
C-M  where "wimsoutput" is the WIMS output filename and "wedoutput"
C-M  and "wedsummary" are WEDOG2 output filenames
C-M
C-
      PARAMETER   (MXMT=200)
      CHARACTER*2  C28,C25
      CHARACTER*16 COM(2)
      CHARACTER*40 REC,ORD,STAR,BLNK,FLNM,FLWI,FLSM,FLWE
      DIMENSION    CPF(10),CPT(10),FSF(10),FST(10)
     1            ,MATI(MXMT),DNS(MXMT),Z2N(10)
C*
C* Cross section (n,2n) for correcting reaction rates
C*
C* Two data statement are included in the code.
C* One for ENDF-B/VI and another one for ENDF-B/IV library.
C* They exclude each other !!!.
C*
C* The first  number is the U-235 (n,2n) XS in WIMS library group 1.
C* The second number is the U-238 (n,2n) XS in WIMS library group 1.
C*
C* Next data statement should be used for ENDF-B/VI library
C*        sig_U_235(n,2n) = 0.4051
C*        sig_U_238(n,2n) = 0.5422
C*        sig_U_233(n,2n) = 0.2033
C*        sig_Th232(n,2n) = 0.5487
C*
       DATA Z2N/ 0.4051, 0.5422, 0.2033, 0.5487, 6*0./
C*
C* Next data statement should be used for ENDF-B/IV library.
C*        sig_U235(n,2n) = 0.2740
C*        sig_U238(n,2n) = 0.6268
C*
C     DATA Z2N/ 0.2740, 0.6268, 8*0./
C*
C* Default filenames
      DATA BLNK/'                                        '/
     1    ,FLWI/'WIMOUT'/
     2    ,FLSM/'WEDSMR'/
     3    ,FLWE/'WEDOUT'/
C* Initialize
      DATA COM/'DIRECT          ','CORRECTED (n,2n)'/
      DATA STAR/' ***************************************'/
C* Default flux values
      DATA FX1,F1I,F1E / 0., 1., 1. /
      KE=0
      KI=0
      IE=0
      IB=0
      RHO28=0
      DEL25=0
      DEL28=0
      CONVR=0
C* Write banner
      WRITE(6,91)
      WRITE(6,91) ' WEDOG2 - Post-process WIMS output      '
      WRITE(6,91) ' =================================      '
      WRITE(6,91)
C* Define the filenames
      GO TO 12
   11 IE=1
      WRITE(6,91) ' ERROR in filename - redo from start    ',FLNM
   12 WRITE(6,91) '$Enter the WIMS output filename       : '
      READ (5,91,END=82) FLNM
      IF(FLNM.EQ.BLNK) GO TO 19
C* From the filename root, generate output filenames
      DO 14 I=1,38
      IP=41-I
      IF(FLNM(IP:IP).NE.' ') GO TO 18
   14 CONTINUE
   18 FLWI=FLNM
      FLSM=FLNM
      FLWE=FLNM
      FLSM(IP-2:IP)='SMR'
      FLWE(IP-2:IP)='WED'
   19 OPEN (UNIT=1,FILE=FLWI,STATUS='OLD',ERR=11)
      OPEN (UNIT=2,FILE=FLSM,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')
      OPEN (UNIT=3,FILE=FLWE,STATUS='UNKNOWN',CARRIAGECONTROL='LIST')
C*
C* Search the output file and pick miscellaneous quantities
   20 READ (1,91,END=80) REC,ORD
      IF(REC( 1: 2).NE.' *' .AND.
     1   REC( 1: 7).NE.'      *') GO TO 21
C* Copy comments from WIMS output
      IF(REC(3:3).EQ.' ' .OR. REC(3:3).EQ.'*' .OR.
     1   REC(7:7).EQ.'*') WRITE(3,91) REC,ORD
      GO TO 20
C* Check for boundary conditions (set IB=1 if FREE)
   21 IF(REC( 1: 5).NE.' FREE' .AND.
     1   REC( 1:10).NE.'      FREE' .AND.
     1   REC( 1:10).NE.'      free') GO TO 22
      IB=1
      GO TO 20
C* Reset KI flag if new case is encountered
   22 IF(REC(16:34).NE.'ENTRY INTO CHAIN  1' .AND.
     1   REC(16:34).NE.'entry into chain  1') GO TO 23
      IF(KI.NE.0) WRITE(2,106) AKE
      KI=0
      KE=0
      IE=0
      IB=0
      DU3=0
      DU5=0
      DU8=0
      DTH=0
      RHO28=0
      DEL25=0
      DEL28=0
      CONVR=0
      GO TO 20
C* Retrieve the MAT numbers and number densities of the first material
   23 IF(REC( 1:10).NE.'0ISOTOPE N' .AND.
     1   REC( 1:10).NE.'0isotope n') GO TO 26
      IRE=0
      READ (1,91)
      READ (1,91)
      READ (1,91)
      NMAT=0
   24 READ (1,96,ERR=20) MATI(NMAT+1),DNS(NMAT+1)
      NMAT=NMAT+1
      IF(NMAT.GE.MXMT) STOP 'WEDOG2 ERROR - MXMT Limit exceeded'
      GO TO 24
C* Retrieve K-eff from DSN transport calculation
   26 IF(ORD(6:17).NE.'  EIGENVALUE') GO TO 28
      READ (ORD(20:33),102) AKE
      IF(AKE.NE.0) AKE=1./AKE
      GO TO 20
C* Retrieve K from the Leakage edit
   28 IF((REC(21:30).NE.'...  K-INF' .AND.
     1    REC(21:30).NE.'...  k-inf') .OR. KI.NE.0) GO TO 30
C* Write k-inf and k-eff to WED output
      KI=1
      IF(IB.EQ.0) READ (ORD(24:37),102) AKE
      WRITE(ORD(24:37),104) AKE
      WRITE(3,91)
      WRITE(3,91) REC,ORD
      READ (1,91)
      READ (1,91)
      READ (1,91)
      READ (1,91)
      READ (1,91)
      IF(FX1.NE.0) READ (1,93) F1E,F1I
C* Read K-inf
      ORD(24:28)=REC(36:40)
      ORD(29:37)=ORD( 1: 9)
      READ (ORD(24:37),102) AKI
      GO TO 20
C* Retrieve group-1 flux in the fuel for the (n,2n) correction
   30 IF(REC( 1:10).NE.'0REGION  1' .AND.
     1   REC( 1:10).NE.' CELL     ') GO TO 40
C*     Skip CELL edit if REGION edit was present
      IF(IRE.EQ.1) GO TO 20
      READ (1,91)
      READ (1,91)
      READ (1,95) FX1
      IRE=0
      IF(REC( 1:10).EQ.'0REGION  1') IRE=1
      GO TO 20
C* Search the file for Reactions and K-Inf. edit
   40 IF(REC(16:25).EQ.'REACTIONS ' .OR.
     1   REC(16:25).EQ.'reactions ') GO TO 42
      IF(REC(1:10).NE.'1K-INF EDI' .AND.
     1   REC(1:10).NE.'1k-inf edi' .AND.
     2   REC(1:10).NE.'1K-EFF EDI' .AND.
     2   REC(1:10).NE.'1k-eff edi') GO TO 20
      IF(REC(1:10).EQ.'1K-EFF EDI' .OR.
     1   REC(1:10).EQ.'1k-eff edi') KE=1
      NMT=0
      REC(1:1)=' '
      WRITE(3,91)
      WRITE(3,91) STAR,STAR
      WRITE(3,91) REC,ORD
      GO TO 20
C* Process reactions edit
   42 REC(1:1)=' '
      IF(KE.NE.1) WRITE(3,91)
      WRITE(3,91) REC
      READ (REC,101) MAT
      JMAT=MAT-1000*(MAT/1000)
      IF     (JMAT.EQ.235) THEN
        MT=1
      ELSE IF(JMAT.EQ.238) THEN
        MT=2
      ELSE IF(JMAT.EQ.233) THEN
        MT=3
      ELSE IF(JMAT.EQ.232) THEN
        MT=4
      ELSE
        GO TO 20
      END IF
      NMT=MAX(NMT,MT)
      READ (1,91)
      READ (1,91)
      READ (1,94) AB1
      READ (1,94) AB2
      READ (1,91)
      READ (1,91)
      READ (1,91)
      READ (1,94) FS1
      READ (1,94) FS2
C*
      CP1=AB1-FS1
      CP2=AB2-FS2
C*
      IC=0
      DNMT=0
      IF(NMAT.LE.0) GO TO 44
C* Define nuclide number density for (n,2n) correction
      DO 43 I=1,NMAT
      IF(MAT.NE.MATI(I)) GO TO 43
      DNMT=DNS(I)
      IF(MT.EQ.1) DU5=DNMT
      IF(MT.EQ.2) DU8=DNMT
      IF(MT.EQ.3) DU3=DNMT
      IF(MT.EQ.4) DTH=DNMT
      GO TO 44
   43 CONTINUE
C* Calculate the reactions
   44 IC=IC+1
      CPF(MT)=CP1
      CPT(MT)=CP2
      FSF(MT)=FS1
      FST(MT)=FS2
      WRITE(3,97) COM(IC)
      WRITE(3,98) 'Capt.',CPF(MT),CPT(MT)
      WRITE(3,98) 'Fiss.',FSF(MT),FST(MT)
      IF(IC.EQ.2) GO TO 50
      FF=FX1
      IF(KE.EQ.1) FF=FF*F1E/F1I
      CP1=CP1+FF*DNMT*Z2N(MT)
      IF(Z2N(MT).NE.0) GO TO 44
   50 IF(MT .EQ.2 .AND. DU5.LE.0) GO TO 20
      IF(MT .EQ.4 .AND. DU3.LE.0) GO TO 20
      IF(NMT.EQ.4 .AND. DTH.LT.DU8) NMT=2
      IF     (NMT.EQ.2) THEN
        RHO28=CPF(2)/CPT(2)
        DEL25=FSF(1)/FST(1)
        DEL28=(FSF(2)+FST(2)) / (FSF(1)+FST(1))
        CONVR=(CPF(2)+CPT(2)) / (FSF(1)+FST(1))
        C28='28'
        C25='25'
      ELSE IF(NMT.EQ.4) THEN
        RHO28=CPF(4)/CPT(4)
        DEL25=FSF(3)/FST(3)
        DEL28=(FSF(4)+FST(4)) / (FSF(3)+FST(3))
        CONVR=(CPF(4)+CPT(4)) / (FSF(3)+FST(3))
        C28='02'
        C25='23'
      ELSE
        GO TO 20
      END IF
      IF(RHO28.GT.99.999 )   RHO28=99.999
      IF(DEL25.GT. 9.99999)  DEL25= 9.99999
      IF(KE.EQ.0) WRITE(3, 99) AKI,C28,RHO28,C25,DEL25,C28,DEL28,CONVR
      IF(KE.EQ.1) WRITE(3, 99) AKE,C28,RHO28,C25,DEL25,C28,DEL28,CONVR
      IF(KE.EQ.1) WRITE(2,106) AKE,RHO28,DEL25,DEL28,CONVR
      DU3=0
      DU5=0
      DU5=0
      DU8=0
      DTH=0
      KI=0
      GO TO 20
C* Normal termination
   80 IF(KI.NE.0) WRITE(2,106) AKE
      CLOSE(UNIT=2)
      CLOSE(UNIT=3)
      STOP 'WEDOG2 Completed'
C* Error trap when WIMS output file can not be opened
   82 IF(IE.EQ.0) GO TO 19
      STOP 'WEDOG2 - ERROR in filename'
C*
   91 FORMAT(2A40)
   93 FORMAT(77X,2F14.0)
   94 FORMAT(13X,F12.0)
   95 FORMAT(52X,F12.0)
   96 FORMAT(I6,F11.0)
   97 FORMAT(/1X,A16/' Group','        Fast','     Thermal')
   98 FORMAT(1X,A5,2F12.6,4F12.5)
   99 FORMAT(/' K=',F8.5,'  Rho',A2,'=',F6.3,'    Del',A2,'=',F8.5
     1      ,'    Del',A2,'=',F8.5,'    C*=',F7.4)
  101 FORMAT(8X,I6)
  102 FORMAT(F14.0)
  104 FORMAT(F10.5,4X)
  106 FORMAT(F10.5,F10.4,2F10.5,F10.4)
      END
