      PROGRAM NJINJB
C-Title  : NJINJB Program
C-Purpose: Edit the F.P. yields data in NJOY input
C-Author : A.Trkov, Institute Jozef Stefan, Ljubljana, Slovenia
C-Version: October 1999
C-V  01/12 A.Trkov: Fix bug when MAT ID missing from the list
C-M
C-M  Manual for Program NJINJB
C-M  =========================
C-M
C-M  The program searches the NJOY input for the WIMSR module
C-M  input instructions. If burnup data are present, if they
C-M  refer to a fissile material and if new yields for this
C-M  material are available, the yields are replaced.
C-M
C-M  The program reads the energy released per fission and the
C-M  capture and decay data, from the original NJOY input file.
C-M  It then adds all new fission product yield data. Finally,
C-M  any nuclide on the original NJOY input that is not yet
C-M  present on the list is appended. Care is taken that the
C-M  total number of nuclides on the list is corrected as
C-M  necessary.
C-M
C-M  Instructions
C-M  ------------
C-M  The input directives can be entered from keyboard in
C-M  response to the prompts on the terminal screen. The requested
C-M  input parameters are the following:
C-M    FLLS  - WIMS-D material ident to ZA designation conversion
C-M            table filename (see format description below).
C-M    FLNJ  - Source NJOY input filename.
C-M    FLNO  - Updated NJOY input filename.
C-M
C-M  It is implicitly assumed that the fission product yields
C-M  data for the fissile actinides are available on files with
C-M  filenames of the form:
C-M    "zzaaa???.njb"
C-M  where "zzaaa" is the ZA designation of the fissile nuclide.
C-M  Usually these files are produced in a batch procedure of WLUP.
C-M  The first record in the file contains the number of fission
C-M  products, which are given on the records that follow. Each
C-M  record contains a pair of numbers, giving the WIMS-D isotope
C-M  identification number and the corresponding yield. Free format
C-M  may be used.
C-M
C-M  FLLS File Format Specifications
C-M  -------------------------------
C-M  The first record is interpreted as comment and is ignored.
C-M  The following records contain pairs of numbers:
C-M    ZA    material za designation, 1000*Z + A
C-M          where Z is the atomic number and A is the mass number.
C-M    WMT   WIMS-D material identifier.
C-M  Pairs of numbers are read until a blank line or an end-of-file
C-M  is encountered.
C-
      PARAMETER    (MXMT=200)
C*
      CHARACTER*80  REC
      CHARACTER*40  BLNK,FLNM,FLNJ,FLNO,FLLS,FLLG
      CHARACTER*6   FNAME
      DIMENSION     ZA(MXMT),WMT(MXMT),FPID(MXMT),FPYL(MXMT)
C*
      DATA BLNK/'                                        '/
     1     FLNJ/'ENDFB6.NJI'/
     2     FLNO/'WLUP.NJI'/
     3     FLLS/'ACTLST.DAT'/
     4     FLLG/'NJINJB.LST'/
      DATA LNI,LNO,LLS,LBI,LKB,LTT,LLG / 1, 2, 3, 4, 5, 6, 7 /
C*
      WRITE(LTT,991) ' NJINJB - Edit F.P.Yields in NJOY input '
      WRITE(LTT,991) ' ====================================== '
      WRITE(LTT,991)
C*
C* Define the material ZA to WIMS-D ident conversion table file
   40 WRITE(LTT,991) '$Enter MAT ID to ZA table filename    : '
      READ (LKB,991) FLNM
      IF(FLNM.NE.BLNK) FLLS=FLNM
      OPEN (UNIT=LLS,FILE=FLLS,STATUS='OLD',ERR=40)
      READ (LLS,801) REC
      NAC=0
      NFY=0
      MFY=0
   42 READ (LLS,801,END=44) REC
      IF(REC(1:22).EQ.'                      ') GO TO 44
      NAC=NAC+1
      IF(NAC.GT.MXMT) STOP 'NJINJB ERROR - MXMT limit exceeded'
      READ (REC, * ) ZA(NAC),WMT(NAC)
      GO TO 42
   44 CLOSE(UNIT=LLS)
      IF(NAC.LE.0) STOP 'NJINJB ERROR - WIMS-D MAT table empty'
C*
C* Define the source NJOY input file to be edited
   60 WRITE(LTT,991) '$Enter the source NJOY input filename : '
      READ (LKB,991) FLNM
      IF(FLNM.NE.BLNK) FLNJ=FLNM
      OPEN (UNIT=LNI,FILE=FLNJ,STATUS='OLD',ERR=60)
C*
C* Define the updated NJOY input filename
   80 WRITE(LTT,991) '$Enter the updated NJOY input filename: '
      READ (LKB,991) FLNM
      IF(FLNM.NE.BLNK) FLNO=FLNM
      OPEN (UNIT=LNO,FILE=FLNO,STATUS='UNKNOWN',ERR=80)
C* Define the log file
      OPEN (UNIT=LLG,FILE=FLLG,STATUS='UNKNOWN')
      WRITE(LLG,991) ' NJINJB - Edit F.P.Yields in NJOY input '
      WRITE(LLG,991) ' ====================================== '
      WRITE(LLG,991)
      WRITE(LLG,991) ' Material ID / ZA table file          : ',FLLS
      WRITE(LLG,991) ' Source  NJOY input file              : ',FLNJ
      WRITE(LLG,991) ' Updated NJOY input file              : ',FLNO
      WRITE(LLG,991)
C*
C* Copy records in the NJOY input file
  200 READ (LNI,801,END=600) REC
      IF (REC(1:2).EQ.'--' .AND. REC(4:9).NE.'      ') THEN
        FNAME=REC(4:9)
        WRITE(LTT,*) ' Processing material deck: ',FNAME
      ENDIF
      WRITE(LNO,801) REC
C* Identify the beginning of WIMSR input
      IF(REC(1:5).NE.'wimsr') GO TO 200
C* Copy next 4 records
      READ (LNI,801) REC
      WRITE(LNO,801) REC
      READ (LNI,801) REC
      WRITE(LNO,801) REC
      READ (LNI,801) REC
      WRITE(LNO,801) REC
      READ (LNI,801) REC
      WRITE(LNO,801) REC
C* Interpret the 4-th WIMSR input record with MAT ident and burnup flag
      READ (REC, * ) MAT,DMY,RMT,JBU
      IF(JBU.NE.1) GO TO 200
C* Copy next record as is
      READ (LNI,801) REC
      WRITE(LNO,801) REC
C* Check if fissile material data exist
      READ (LNI,801) REC
      READ (REC, * ) KFPR,EFIS
      IF(KFPR.LE.2 .AND. EFIS.LE.0) THEN
        WRITE(LNO,801) REC
        GO TO 200
      END IF
C* Check if new yields for this isotope exist
      MFY=MFY+1
      DO 220 I=1,NAC
      JAC=I
      IF(ABS(RMT-WMT(I)).LT.0.01) GO TO 240
  220 CONTINUE
      IRMT=RMT+0.01
      WRITE(LTT, * ) ' WARNING - Fissile material ident not found',IRMT
      WRITE(LLG, * ) ' WARNING - Fissile material ident not found',IRMT
      WRITE(LLG, * ) ' Valid list:'
      WRITE(LLG, * ) (NINT(WMT(J)),J=1,NAC)
      WRITE(LNO,801) REC
      GO TO 200
C* Construct the default yields filename for this isotope
  240 WRITE(FLNM,890) NINT(ZA(JAC)),FNAME(6:6)
      OPEN (UNIT=LBI,FILE=FLNM,STATUS='OLD',ERR=242)
      GO TO 244
C*
  242 WRITE(LTT, * ) ' WARNING - F.P. yields file missing : ',FLNM
      WRITE(LLG, * ) ' WARNING - F.P. yields file missing : ',FLNM
      WRITE(LNO,801) REC
      GO TO 200
C*
  244 WRITE(LTT, * ) ' Processing ZA MAT ID',ZA(JAC),MAT,NINT(RMT)
      WRITE(LLG, * ) ' Processing ZA MAT ID',ZA(JAC),MAT,NINT(RMT)
      NFY=NFY+1
C*
C* Read in the new fission product yields
      LFPR=2
  250 READ (LBI,801,END=260) REC
      IF(REC(1:20).EQ.'                    ') GO TO 260
      LFPR=LFPR+1
      IF(LFPR.GT.MXMT) STOP 'NJINJB ERROR - MXMT limit exceeded'
      READ (REC, * ,ERR=252) FPID(LFPR),FPYL(LFPR)
      GO TO 250
  252 STOP 'NJINJB ERROR - Reading NJB file'
  260 WRITE(LTT, * ) '         No.of F.P',LFPR
      CLOSE(UNIT=LBI)
C*
C* Begin replacement the burnup data
C* Read the capture and decay products
      READ (LNI, * ) FPID(1),FPYL(1)
      READ (LNI, * ) FPID(2),FPYL(2)
C* Check the remaining nuclides from the original input
      DO 340 I=3,KFPR
      READ (LNI, * ) FPJ,YLJ
      DO 330 J=1,LFPR
      IF(ABS(FPJ-FPID(J)).LT.0.01) GO TO 340
  330 CONTINUE
C* Add the nuclide to the list
      LFPR=LFPR+1
      FPID(LFPR)=FPJ
      FPYL(LFPR)=YLJ
  340 CONTINUE
C* Write updated data to the new NJOY input file
      WRITE(LNO,802) LFPR,EFIS
      DO 360 I=1,LFPR
      WRITE(LNO,802) NINT(FPID(I)),FPYL(I)
  360 CONTINUE
      GO TO 200
C*
  600 WRITE(LTT,992) NFY,MFY
      WRITE(LLG,992) NFY,MFY
      STOP 'NJINJB Completed'
C*
  801 FORMAT(A80)
  802 FORMAT(I6,1P,E11.4)
  890 FORMAT(I5,A1,'I.NJB')
  991 FORMAT(2A40)
  992 FORMAT(' F.P. yields changed for ',I3,' out of',I3,' materials')
      END
