      PROGRAM PLTWLI
C-Title  : Program PLTWLI
C-Purpose: Prepare WILLIE input to plot WIMS-D cross sections
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1999)
C-Version: 1999 original code.
C-V  00/11 Convert Mat.ID to explicit label from "matlst" file.
C-M
C-M  Manual for Program PLTWLI
C-M  =========================
C-M
C-M  WILLIE input to plot the cross sections from a WIMS-D library
C-M  is prepared. One or more WIMS-D library filenames can be
C-M  specified. The reaction cross sections can also be selected.
C-M  When several libraries are specified, a single reaction
C-M  cross section can be chosen for comparison between different
C-M  libraries. If a single library is specified, several reaction
C-M  cross sections are processed for each material.
C-M
C-M  Apart from the WILLIE input, an input file for the PLOTTAB
C-M  graphics package is also prepared.
C-M
C-M  Instructions
C-M  ------------
C-M  In response to the prompt, define:
C-M  - WIMS-D library filenames list terminated by blank.
C-M  - Label for each file (by default the label is equal to
C-M    the filename). This label is transferred to the WILLIE
C-M    input to appear in the PLOTTAB curves file and is used
C-M    to label curves in the legend box of the plots.
C-M  - Filename containing material designation labels (columns
C-M    1-12) and the corresponding ID numbers (columns 13-18).
C-M    Records preceeding the one beginning with ' ---------'
C-M    are assumed to be comments and are ignored.
C-M  - Reaction number code:
C-M    POTENTIAL CROSS SECTION                                    1
C-M    SLOWING DOWN POWER                                         2
C-M    TRANSPORT CROSS SECTION                                    3
C-M    ABSORPTION CROSS SECTION                                   4
C-M    FISSION YIELD (FISS.CROSS SECT TIMES NU-BAR)               5
C-M    FISSION CROSS SECTION                                      6
C-M    OUTSCATTERING CROSS SECTION                                7
C-M    SCATTERING CROSS SECTION                                   8
C-M    DIFFERENTIAL SCATERING CROSS SECTION (SCATT.MATRIX ROW)    9
C-M    ABSORPTION RESONANCE INTEGRAL                             10
C-M    FISSION RESONANCE INTEGRAL                                11
C-M    FISSION SPECTRUM                                          12
C-M When more than one WIMS-D libraries are processed, the label for
C-M each library can be redefined. By default, the filename is used.
C-M
C-M  Two output files with fixed filenames are written:
C-M    PLTWLI.WLI - WILLIE input instructions to extract cross
C-M                 sections from the specified WIMS-D libraries
C-M                 onto the PLTWLI.CUR file.
C-M    PLTWLI.P92 - PLOTTAB input file to plot the cross sections.
C-M
C-M  Note that the cross section curves file PLTWLI.CUR file is
C-M  produced after running WILLIE with PLTWLI.WLI as input.
C-
      PARAMETER      (MXWL=10,MXSL=12,MXMT=400)
      CHARACTER*10    MSEL(MXSL)
      CHARACTER*12    ISOT
      CHARACTER*40    BLNK,FLNM,FLMA,FLWI,FCUR,FP92
     1               ,FLWL(MXWL),LABL(MXWL)
      DIMENSION       MAT(2*MXMT),ISEL(MXSL)
C* Plot area specifications
C*  IXA - Abscisa offset of the plot area
C*  IXB - Abscisa upper limit of plot area (-ve for portrait orientation)
C*  IYA - Ordinate offset of the plot area
C*  IYB - Ordinate upper limit of plot area
C*  IXP - Number of plots in the x-direction
C*  IYP - Number of plots in the y-direction
C*  ISZ - Character size scaling factor
      DATA IXA,IXB,IYA,IYB,IXP,IYP,ISZ
     1    /  2, 24,  2, 16,  2,  2,  2 /
C*
      DATA LIB,LWI,L92,LMA,LKB,LTT/ 1, 2, 3,-4, 5, 6 /
      DATA BLNK   /'                                        '/
     1     FLWI   /'PLTWLI.WLI'/
     2     FCUR   /'PLTWLI.CUR'/
     3     FP92   /'PLTWLI.P92'/
     4     FLMA   /'MATLST.TXT'/
     1     FLWL(1)/'JENDL32.LIB'/
 
      DATA LABL(1)/'JENDL-3.2'/
      DATA ISEL/ 4, 3, 8, 6, 0, 0, 0, 0, 0, 0, 0, 0 /
 
      DATA MSEL/
     1 'POTENTIAL ','SLOW.DN.PW','TRANSPORT ','ABSORPTION','FISS.YIELD'
     1,'FISSION   ','OUTSCATT. ','SCATTERING','DIFF.SCAT.','ABSORPT.RI'
     2,'FISSION RI','FISS.SPECT'/
 
C*
      NLIB=0
      NSEL=0
C*
      WRITE(LTT,691) ' PLTWLI - WIMS-D library WILLIE Pre-Pro.'
      WRITE(LTT,691) ' ======================================='
      WRITE(LTT,691)
      WRITE(LTT,691) ' To compare cross sections, enter a sing'
     1              ,'le WIMS-D library filename              '
      WRITE(LTT,691) ' To compare libraries, enter several fil'
     1              ,'enames.                                 '
      WRITE(LTT,691) ' Terminate list by blank or "-".        '
      WRITE(LTT,691)
      GO TO 22
   20 WRITE(LTT,691) ' ERROR - Incorrect or illegal filename  ',FLNM
   22 WRITE(LTT,691) '$       Enter WIMS-D library filename : '
      READ (LKB,691) FLNM
      IF(FLNM.EQ.BLNK .OR. FLNM(1:1).EQ.'-') GO TO 30
C* Check that the file exists
      OPEN (UNIT=LIB,FILE=FLNM,STATUS='OLD',ERR=20)
      CLOSE(UNIT=LIB)
      NLIB=NLIB+1
      IF(NLIB.GT.MXWL) STOP 'PLTWLI ERROR - MXWL limit exceeded'
      FLWL(NLIB)=FLNM
      LABL(NLIB)=FLNM
      GO TO 22
C* Re-assign labels if several libraries present
   30 IF(NLIB.LE.1) GO TO 34
      DO 32 I=1,NLIB
      WRITE(LTT,691) '        Default label is the filename : ',FLWL(I)
      WRITE(LTT,691) '$         Enter new label to redefine : '
      READ (LKB,691,END=40) FLNM
      IF(FLNM.NE.BLNK) LABL(I)=FLNM
   32 CONTINUE
C*
   34 WRITE(LTT,691) '$   Enter WIMS-D Material ID filename : '
      READ (LKB,691,END=40) FLMA
      IF(FLMA.EQ.BLNK) GO TO 40
      LMA=ABS(LMA)
      OPEN (UNIT=LMA,FILE=FLMA,STATUS='OLD',ERR=34)
C* All WIMS-D library filenames entered
   40 IF(NLIB.LT.1) GO TO 600
      WRITE(LTT,691) ' Select the reaction from the list    : '
      DO 42 I=1,MXSL
      WRITE(LTT,693) BLNK(1:30)//MSEL(I),I
   42 CONTINUE
   44 WRITE(LTT,691) '$Enter the reaction cross section flag: '
      READ (LKB,603) JSEL
      IF(JSEL.GT.0) THEN
        IF(JSEL.GT.12) GO TO 44
        NSEL=NSEL+1
        ISEL(NSEL)=JSEL
        IF(NLIB.EQ.1) GO TO 44
      END IF
      IF(NSEL.LT.1) NSEL=1
C*
      WRITE(LTT,691)
      WRITE(LTT,693) '            WIMS-D Libraries selected : ',NLIB
      DO 62 I=1,NLIB
      WRITE(LTT,691) BLNK(1:20)//LABL(I),FLWL(I)
   62 CONTINUE
      WRITE(LTT,691)
      WRITE(LTT,693) '                   Reactions selected : ',NSEL
      DO 64 I=1,NSEL
      WRITE(LTT,691) BLNK(1:30)//MSEL(ISEL(I))
   64 CONTINUE
C*
C* Open the file to write the WILLIE input instructions
      OPEN (UNIT=LWI,FILE=FLWI,STATUS='UNKNOWN')
C*
C* Use material list from the first WIMS-D library
      OPEN (UNIT=LIB,FILE=FLWL(1),STATUS='OLD')
      READ (LIB,605) NEL,NG,NG0,NG1,NG2,NG3,NNFD,NNFPD
      IF(NEL.GT.MXMT) STOP 'PLTWLI ERROR - MXMT limit exceeded'
      READ (LIB,605) (MAT(J),J=1,NEL)
      CLOSE (UNIT=LIB)
      IF(NLIB.LE.1) GO TO 80
C* Make material list a union of MAT numbers from all files
      KEL=NEL
      DO 79 L=2,NLIB
      OPEN (UNIT=LIB,FILE=FLWL(L),STATUS='OLD')
      READ (LIB,605) JEL,NG,NG0,NG1,NG2,NG3,NNFD,NNFPD
      IF(KEL+JEL.GT.MXMT) STOP 'PLTWLI ERROR - MXMT limit exceeded'
      READ (LIB,605) (MAT(KEL+J),J=1,JEL)
      CLOSE (UNIT=LIB)
      IEL=0
   74 IEL=IEL+1
      IF(IEL.GT.NEL) GO TO 79
   75 DO 76 I=1,JEL
      IF(MAT(KEL+I).EQ.MAT(IEL)) GO TO 74
   76 CONTINUE
C* Material not found - eliminate from the comparison list
      WRITE(LTT,691) ' WARNING : Processing WIMS-D library    ',FLWL(L)
      WRITE(LTT,693) '             Exclude from comparison MAT',MAT(IEL)
      NEL=NEL-1
      DO 78 I=IEL,NEL
      MAT(I)=MAT(I+1)
   78 CONTINUE
      IF(IEL.LT.NEL) GO TO 75
   79 CONTINUE
C*
C* Write PLOTTAB input general information
   80 OPEN (UNIT=L92,FILE=FP92,STATUS='UNKNOWN')
      WRITE(L92,611) IXA,IXB,IYA,IYB,IXP,IYP,ISZ
      WRITE(L92,611) MAX(NSEL,NLIB),0,1,0,0,0,0
      WRITE(L92,691) 'Energy                                  '
     1              ,'eV                                      '
      WRITE(L92,691) 'Cross Section                           '
     1              ,'barns                                   '
C*
      DO 400 I=1,NEL
      WRITE(LWI,693)
      WRITE(LWI,693) '                    Process material MAT',MAT(I)
C*
      WRITE(LWI,610) 'PTAB'
      WRITE(LWI,691) FLWL(1)
      DO 210 J=1,NLIB
      WRITE(LWI,691) FLWL(J)
  210 CONTINUE
      WRITE(LWI,691)
      DO 220 J=1,NLIB
      WRITE(LWI,691) LABL(J)
  220 CONTINUE
C* Select cross sections
      JSEL=1
      WRITE(LWI,601) ISEL(JSEL)
      WRITE(LWI,691) FCUR
C* Select MAT Number
      DO 222 J=1,NLIB
      WRITE(LWI,601) MAT(I)
      WRITE(LWI,601) 0
  222 CONTINUE
  230 JSEL=JSEL+1
      IF(JSEL.GT.NSEL .OR. ISEL(JSEL).LE.0) GO TO 240
C* Select next cross section
      WRITE(LWI,601) ISEL(JSEL)
      WRITE(LWI,691)
      WRITE(LWI,601) 296
      GO TO 230
C* Finish the selection
  240 WRITE(LWI,601)-1
C*
      IF(NLIB.EQ.1) THEN
        WRITE(L92,691) 'WIMS-D Library Cross Section Comparison '
        WRITE(L92,620) MAT(I),FLWL(1)
        WRITE(L92,691)
        WRITE(L92,691)
      ELSE
        CALL MATISO(LMA,MAT(I),ISOT)
        WRITE(L92,691) 'WIMS-D Library Cross Section Comparison '
        WRITE(L92,630) MSEL(ISEL(1)),ISOT
        WRITE(L92,691)
        WRITE(L92,691)
      END IF
  400 CONTINUE
 
      WRITE(LWI,610) 'END '
      WRITE(L92,691)
C*
  600 STOP 'PLTWLI Completed'
C*
  601 FORMAT(I5)
  603 FORMAT(BN,I10)
  605 FORMAT(5I15)
  610 FORMAT(A4)
  611 FORMAT(6I11,I4)
  620 FORMAT('MAT ID',I5,' FROM ',A40)
  630 FORMAT(A10,' For Material ',A12)
  691 FORMAT(2A40)
  693 FORMAT(A40,I5)
C*
      END
      SUBROUTINE MATISO(LMA,MAT,ISOT)
C-Title  : Subroutine MATISO
C-Purpose: Extract Isotope name form MAT Ident on file unit LMA
C-Description:
C-D  The material ident table is given on unit LMA, consisting of:
C-D  Column Description
C-D    1-10 material or nuclide label given in terms of zz-XX-aaa
C-D         where zz is the atomic number, XX the chemical symbol
C-D         and aaa the mass number, which is zero to flag natural
C-D         elements.
C-D   11-18 Material ID number (WIMS-D convention).
C-D  The material ident table needs to be read once. After the
C-D  first pass the unit number LMA is flagged negative.
C-D    Given the input MAT ident number, this is compared agains
C-D  input table and the material label is placed into ISOT string.
C-
      PARAMETER   (MXLS=200)
      CHARACTER*80 REC
      CHARACTER*12 ISOT,ILST(MXLS)
      DIMENSION    ALST(MXLS)
      DATA NMA/0/
      SAVE
C* Read material ID table
      IF(LMA.LE.0) GO TO 40
      NMA=0
      REWIND LMA
C* Skip heading text
   20 READ (LMA,910,ERR=40) REC
      IF(REC(1:10).NE.' ---------') GO TO 20
C* Process the table
   22 READ (LMA,910,ERR=40,END=40) REC
      IF(REC(1:10).EQ.'          ') GO TO 40
      READ (REC,912,ERR=40) ILST(NMA+1),ALST(NMA+1)
      NMA=NMA+1
      GO TO 22
C* Define material string from the given ident
   40 LMA=-ABS(LMA)
      WRITE(ISOT,901) MAT
      IF(NMA.LE.0) RETURN
      DO 60 I=1,NMA
      MLST=ALST(I)+0.01
      IF(MAT.NE.MLST) GO TO 60
      ISOT=ILST(I)
      GO TO 80
   60 CONTINUE
   80 RETURN
C*
  901 FORMAT(I6,6X)
  910 FORMAT(A80)
  912 FORMAT(A12,F6.0)
      END
