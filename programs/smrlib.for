      PROGRAM SMRLIB
C-Title  : SMRLIB Program
C-Purpose: Compare SMRDIF summary results for different libraries
C-Author : A.Trkov, IAEA-NDS, Vienna
C-Version: Nov-2000 Original code.
C-M
C-M  Manual for Program SMRLIB
C-M  -------------------------
C-M
C-M  The program compares SMRDIF outputs from the analysis of the
C-M  WLUP series of benchmarks. More than one SMRDIF output may
C-M  appear on the same file. The program also recognises and is
C-M  able to process D2OSMR and WEDB1B outputs on the same file,
C-M  but at least one SMRDIF output must appear at the beginning.
C-M  The source library names are extracted from it.
C-M
C-M  Comparison consists of copying the heading information and
C-M  the reference result, followed by the corresponding record
C-M  from each of the SMRDIF output files. In the first 10 columns
C-M  the library identification string is added, extracted from
C-M  the first SMRLIB output set on each file.
C-M
C-M  Instructions:
C-M  The following input parameters are entered interactively:
C-M  - Output list filename (default SMRLIB.LST).
C-M  - SMRLIB files to be compared. Up to MXFL files can be
C-M    specified. The list is terminated by blank or "-".
C-
      PARAMETER     (MXFL=15,MXIS=400)
      CHARACTER*100   REC,RC2
      CHARACTER*61   LINE
      CHARACTER*40   BLNK,FLNM,FLIN(MXFL),FLOU
      CHARACTER*20   SRCH(14)
      CHARACTER*10   CH10,BLBL(MXFL)
      CHARACTER*8    ISOT(MXIS),JSOT
      CHARACTER*1    C1
      DIMENSION      NLBL(MXFL)
      DIMENSION      ERX(MXFL,MXIS),ELM(MXIS),REF(MXIS)
C*
      DATA NLBL/MXFL*0/
      DATA LIN,LOU,LKB,LTT/ 20, 2, 5, 6 /
      DATA BLNK/'                                        '/
     1     FLOU/'Smrlib.lst'/
C* Search strings
      DATA SRCH/' SMRDIF - Compare la'
     &         ,' D2OSMR - Summarize '
     &         ,' WEDB1B - Isotopic c'
     &         ,' D2OE3B comparison A'
     &         ,' DOPPLER comparison '
     &         ,' RTC-DIFFS.BETWEEN C'
     &         ,' MTR-OWR comparison '
     &         ,' Thermal Neutron Flu'
     &         ,' Therm.Flux Distr.H2'
     &         ,' Therm.Flux Distr.AI'
     &         ,' Therm.Flux Distr.12'
     &         ,' Therm.Flux Distr.-A'
     &         ,' PWR THORIUM PIN CEL'
     &         ,' SMRDIF Integral par'/
C*
C* Write the banner
      WRITE(LTT,901) ' SMRLIB - Comparison of SMRDIF outputs  '
      WRITE(LTT,901) ' =====================================  '
      WRITE(LTT,901)
C*
C* Define the filenames
      WRITE(LTT,901) '$Enter the output summary filename    : '
      READ (LKB,901) FLNM
      IF(FLNM.NE.BLNK) FLOU=FLNM
      OPEN (UNIT=LOU,FILE=FLOU,STATUS='UNKNOWN')
      NSMR=0
      GO TO 14
C* Trap illegal or non-existent files
   12 WRITE(LTT,901) ' ERROR - Invalid summary file      : ',FLNM
   14 KIN=LIN+NSMR+1
      WRITE(LTT,901) '$Enter the input summary filename     : '
      READ (LKB,901,END=40) FLNM
      IF(FLNM.EQ.BLNK .OR. FLNM(1:1).EQ.'-') GO TO 40
      OPEN (UNIT=KIN,FILE=FLNM,STATUS='OLD',ERR=12)
C*
C* Find the appropriate search string
   20 READ (KIN,902,END=22) REC
      IF(NSMR.EQ.0 .AND. REC(2:2).EQ.'#') WRITE(LOU,902) REC
      IF(REC(1:20).NE.SRCH(1).AND.REC(1:20).NE.SRCH(4).AND.REC(1:20).NE.
     1SRCH(5).AND.REC(1:20).NE.SRCH(6).AND.REC(1:20).NE.SRCH(7).AND.
     2REC(1:20).NE.SRCH(8).AND.REC(1:20).NE.SRCH(9).AND.REC(1:20).NE.
     3SRCH(10).AND.REC(1:20).NE.SRCH(11).AND.REC(1:20).NE.SRCH(12).AND.
     4REC(1:20).NE.SRCH(13).AND.REC(1:20).NE.SRCH(14))GO TO 20
      GO TO 30
C*
C* Trap unrecognised/invalid file
   22 WRITE(LTT,901) ' WARNING - Not SMRDIF output file  : ',FLNM
      CLOSE(UNIT=KIN)
      GO TO 12
C*
C* SMRDIF output located in this file - try next file
   30 NSMR=NSMR+1
      FLIN(NSMR)=FLNM
      IF(NSMR.LT.MXFL) GO TO 14
C*
C* All files opened - begin comparison
   40 IF(NSMR.LT.2) THEN
        WRITE(LTT,901) ' WARNING - Nothing to compare - 1 file  '
        GO TO 900
      END IF
      WRITE(LTT,901)
      WRITE(LTT,901) ' Following files summarised in          ',FLOU
      DO 41 I=1,NSMR
      WRITE(LTT,901) BLNK,FLIN(I)
   41 CONTINUE
C*
C* Start assembling the output file
C* Write heading information from SMRDIF output
      WRITE(LOU,902) REC
      DO 44 K=1,3
      IDIF=0
      DO 42 I=1,NSMR
        KIN=LIN+I
        READ (KIN,902) RC2
        IF(I.EQ.1) THEN
          REC=RC2
          WRITE(LOU,902) REC
        END IF
C* Check if same reference IDIF=flag indicating differences
        IF(RC2.NE.REC) THEN
          IF(IDIF.EQ.0)
     1    WRITE(LTT,901) ' WARNING - '//FLIN(1)(1:29),REC(41:70)
          WRITE(LTT,901) '           '//FLIN(I)(1:29),RC2(41:70)
          IDIF=IDIF+1
        END IF
   42 CONTINUE
   44 CONTINUE
C* Identify the source library into BLBL 10-character string
      DO 48 I=1,NSMR
        KIN=LIN+I
        READ (KIN,902) RC2
        WRITE(LOU,902) RC2
        N2=81
   45   N2=N2-1
        IF(N2.LT.1) THEN
          WRITE(LTT,901) ' SMRLIB ERROR reading file            : '
     &                  ,FLIN(I)
          STOP 'SMRLIB ERROR - Reading file'
        END IF
        IF(RC2(N2:N2).EQ.' ') GO TO 45
        N1=N2
   46   N1=N1-1
        IF(RC2(N1:N1).EQ.'.') THEN
          N2=N1-1
          GO TO 46
        END IF
        IF(N2-N1.LT.0) THEN
          WRITE(LTT,901) ' SMRLIB ERROR reading file            : '
     &                  ,FLIN(I)
          WRITE(LTT,901) '              Invalid library name      '
          STOP 'SMRLIB ERROR - Reading file'
        END IF
        IF(RC2(N1:N1).NE.' ') GO TO 46
        NN=MIN(10,N2-N1)
        CH10='          '
        CH10(11-NN:10)=RC2(N1+1:N1+NN)
        BLBL(I)=CH10
   48 CONTINUE
C* Copy the header
      DO 52 I=1,3
      DO 51 J=1,NSMR
      READ (LIN+J,902) REC
      IF(J.EQ.1) WRITE(LOU,902) REC
   51 CONTINUE
   52 CONTINUE
C*
C* Compare benchmark results from different files (SMRDIF & D2OSMR)
C* IBN flag to suppress blank line printout between sets
C* IEN flag to mark end of data on reference file
      IBN=0
   60 READ (LIN+1,902) REC
      IF(REC(1:20).EQ.'                    ') GO TO 60
      IF(REC(1:20).EQ.'  ------------------' .OR.
     1   REC(1:20).EQ.' -------------------' .OR.
     2   REC(1:20).EQ.' ===================') THEN
        WRITE(LOU,902) REC
        IBN=0
        NLBL(1)=1
        GO TO 60
      END IF
      IF(IBN.NE.0) WRITE(LOU,902)
      IBN=IBN+1
      WRITE(LOU,902) REC
      READ (LIN+1,902) RC2
      RC2(1:10)=BLBL(1)
      WRITE(LOU,902) RC2
      DO 64 J=2,NSMR
      IF(NLBL(J).NE.0) THEN
        IF(NLBL(1).NE.0) THEN
          GO TO 62
        ELSE
          GO TO 64
        END IF
      END IF
   61 READ (LIN+J,902) RC2
      IF(RC2(1:10).EQ.REC(1:10)) GO TO 62
      IF(RC2(1:10).EQ.'   Average'.OR.RC2(1:10).EQ.' Average  ')THEN
        NLBL(J)=1
        GO TO 64
      END IF
      IF(RC2(1:10).NE.REC(1:10)) GO TO 61
   62 READ (LIN+J,902) RC2
      RC2(1:10)=BLBL(J)
      WRITE(LOU,902) RC2
   64 CONTINUE
      IF(NLBL(1).EQ.0) GO TO 60
      DO 66 J=1,NSMR
      NLBL(J)=0
   66 CONTINUE
C*
C* Check for any additional data sets on the same file
   70 KIN=LIN+1
   71 READ (KIN,902,END=900) REC
      IF(REC(2:2).EQ.'#') WRITE(LOU,902) REC
      IF(REC(1:20).EQ.SRCH(1)) GO TO 72
      IF(REC(1:20).EQ.SRCH(2)) GO TO 72
      IF(REC(1:20).EQ.SRCH(3)) GO TO 80
      IF(REC(1:20).EQ.SRCH(4)) GO TO 72
      IF(REC(1:20).EQ.SRCH(5)) GO TO 72
      IF(REC(1:20).EQ.SRCH(6)) GO TO 72
      IF(REC(1:20).EQ.SRCH(7)) GO TO 72
      IF(REC(1:20).EQ.SRCH(8)) GO TO 72
      IF(REC(1:20).EQ.SRCH(9)) GO TO 72
      IF(REC(1:20).EQ.SRCH(10)) GO TO 72
      IF(REC(1:20).EQ.SRCH(11)) GO TO 72
      IF(REC(1:20).EQ.SRCH(12)) GO TO 72
      IF(REC(1:20).EQ.SRCH(13)) GO TO 72
      IF(REC(1:20).EQ.SRCH(14)) GO TO 72
      GO TO 71
C* Next SMRDIF or D2OSMR output identified
   72 WRITE(LOU,902)
      WRITE(LOU,902)
      WRITE(LOU,902) REC
      READ (KIN,902) RC2
      WRITE(LOU,902) RC2
   73 READ (KIN,902,END=900) RC2
      IF(RC2(1:10).NE.'   LATTICE'.AND.RC2(1:10).NE.'       BUP'.AND.
     1RC2(1:10).NE.'     E(wt%'.AND.RC2(1:10).NE.'     Case '.AND.
     2RC2(1:10).NE.'       CYC'.AND.RC2(1:10).NE.'NAME      '.AND.
     3RC2(1:10).NE.' Case  COO'.AND.RC2(1:10).NE.' R(cm)    '.AND.
     4RC2(1:10).NE.' Case     '.AND.RC2(1:10).NE.' B(MWd/kgH'.AND.
     5RC2(1:10).NE.'    ISOTOP') GO TO 73
      WRITE(LOU,902)
      WRITE(LOU,902) RC2
      READ (KIN,902) RC2
      WRITE(LOU,902) RC2
      DO 78 J=2,NSMR
      KIN=LIN+J
   76 READ (KIN,902,END=900) RC2
      IF(RC2(1:20).NE.REC(1:20)) GO TO 76
   77 READ (KIN,902) RC2
      IF(RC2(1:10).NE.'   LATTICE'.AND.RC2(1:10).NE.'       BUP'.AND.
     1RC2(1:10).NE.'     E(wt%'.AND.RC2(1:10).NE.'     Case '.AND.
     2RC2(1:10).NE.'       CYC'.AND.RC2(1:10).NE.'NAME      '.AND.
     3RC2(1:10).NE.' Case  COO'.AND.RC2(1:10).NE.' R(cm)    '.AND.
     4RC2(1:10).NE.' Case     '.AND.RC2(1:10).NE.' B(MWd/kgH'.AND.
     5RC2(1:10).NE.'    ISOTOP') GO TO 77
      READ (KIN,902) RC2
   78 CONTINUE
      IBN=0
      GO TO 60
C*
C* WEDB1B output identified
   80 WRITE(LOU,902)
      WRITE(LOU,902)
      WRITE(LOU,902) REC
C* Copy comments
   82 READ (KIN,902,END=900) REC
      IF(REC(1:10).EQ.' Calculate') GO TO 82
      IF(REC(1:10).EQ.' Isotope  ') GO TO 84
      WRITE(LOU,902) REC
      GO TO 82
C* Read the isotope, rel.diff. and uncertainty from the first file
   84 NIS=0
      READ (KIN,902,END=900)
      READ (KIN,902,END=900)
   86 NIS=NIS+1
   87 READ (KIN,982,ERR=88,END=88) ISOT(NIS),CAL,ERX(1,NIS)
     1                            ,REF(NIS),ELM(NIS)
      IF(CAL.EQ.0 .AND. REF(NIS).EQ.0) GO TO 87
      IF(ISOT(NIS).EQ.'        ') GO TO 88
      GO TO 86
   88 NIS=NIS-1
C* Process the remaining files
      DO 120 J=2,NSMR
      KIN=LIN+J
   92 READ (KIN,902,END=900) REC
      IF(REC(1:20).NE.SRCH(3)) GO TO 92
   94 READ (KIN,902,END=900) REC
      IF(REC(1:10).NE.' Isotope  ') GO TO 94
      READ (KIN,902,END=900)
      READ (KIN,902,END=900)
      JIS=0
   96 READ (KIN,982,ERR=98,END=98) JSOT,DMY,ERJ
      IF(JSOT.EQ.ISOT(JIS+1)) THEN
        JIS=JIS+1
        ERX(J,JIS)=ERJ
      END IF
      IF(JIS.LT.NIS) GO TO 96
   98 IF(JIS.LT.NIS)
     &WRITE(LTT,901) ' WARNING - Unmatching isotopes on file  ',FLIN(J)
  120 CONTINUE
C* Print the differences
      WRITE(LOU,901) ' Legend: --- Uncertainty                '
      WRITE(LOU,901) '         *   Multiple points            '
      DO 122 J=1,NSMR
      WRITE(LOU,901) '        '//CHAR(48+J)//'  '//BLBL(J)
     1             //'                  '
  122 CONTINUE
      WRITE(LOU,901)
      WRITE(LOU,903)'  Isot. ','Ref.[mg/g]'
     1,'V_-20% _______________________V________________________+20%_V'
      NCHW=61
      EMN=-20.
      EMX= 20.
      DO 130 I=1,NIS
      CALL LNPLT1(LINE,NCHW,NSMR,ERX(1,I),ELM(I),EMN,EMX)
      WRITE(LOU,904) ISOT(I),REF(I),LINE
  130 CONTINUE
      GO TO 70
C*
C* File processing completed
  900 STOP 'SMRLIB Completed'
C*
  901 FORMAT(2A40)
  902 FORMAT(A100)
  903 FORMAT(A8,   A10  ,A61)
  904 FORMAT(A8,1P,E10.3,A61)
  982 FORMAT(A8,F12.0,F7.0,F12.0,F6.0)
      END
      SUBROUTINE LNPLT1(LINE,NCHW,NER,ERX,ELM,EMN,EMX)
C-Title  : Subroutine LNPLOT
C-Purpose: Set plot line-bar character string
C-Description:
C-D  The routine generates a character line LINE of length NCHW
C-D  to display NER sets of errors contained in ER and the
C-D  reference uncertainty. The routine is designed for the
C-D  purpose of comparing errors in the predicted isotopic
C-D  concentrations after burnup using various data libraries
C-D  in comparison with the uncertainty in the measured
C-D  concentrations.
C-D  - The line string represents error range from EMN (usually
C-D    negative) to EMX (usually positive).
C-D  - The measured uncertainty ELM (if given) is represented by
C-D    a sequence of strings "---" from minus to plus the
C-D    uncertainty.
C-D  - Errors of individual data sets are superimposed by
C-D    placing the numeral "n" into the appropriate place in the
C-D    string where "n" is the consecutive number of the data set.
C-
      CHARACTER*120 LINE
      CHARACTER*1   C1,H1
      DIMENSION     ERX(1)
C* Clear the output string
      DO 10 I=1,NCHW
      LINE(I:I)=' '
   10 CONTINUE
C* Define limits
      LINE(  1 :  1 )='|'
      LINE(NCHW:NCHW)='|'
C* Set the measured uncertainty range
      F0=FLOAT(NCHW-1)*(-ELM-EMN)/(EMX-EMN)
      L0=1+NINT(F0)
        IF(L0.LT.  1 ) L0=1
        IF(L0.GT.NCHW) L0=NCHW
      F1=FLOAT(NCHW-1)*( ELM-EMN)/(EMX-EMN)
      L1=1+NINT(F1)
        IF(L1.LT.  1 ) L1=1
        IF(L1.GT.NCHW) L1=NCHW
      LA=MIN(L0,L1)
      LB=MAX(L0,L1)
      DO 20 I=LA,LB
        LINE(I:I)='-'
   20 CONTINUE
C* Set centreline
      F0=FLOAT(NCHW-1)*(    -EMN)/(EMX-EMN)
      L0=1+NINT(F0)
      LINE(L0:L0)='|'
C* Set the errors for the data sets
      DO 60 I=1,NER
      ER=ERX(I)
      IF     (ER.LT.EMN) THEN
C* Mark error out of lower bound
        LINE(  1 :  1 )='<'
      ELSE IF(ER.GT.EMX) THEN
C* Mark error out of upper bound
        LINE(NCHW:NCHW)='>'
      ELSE
        FL=FLOAT(NCHW-1)*( ER-EMN)/(EMX-EMN)
        LL=1+NINT(FL)
        C1=CHAR(48+I)
        H1=LINE(LL:LL)
C* Mark multiple points with a star
        IF(H1.GE.CHAR(48+1) .AND.
     &     H1.LT.C1) THEN
           LINE(LL:LL)='*'
        ELSE
           IF(H1.NE.'*') LINE(LL:LL)=C1
        END IF
      END IF
   60 CONTINUE
      RETURN
      END
 
