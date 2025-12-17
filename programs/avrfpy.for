      PROGRAM AVRFPY
C-Title  : Program AVRFPY
C-Purpose: Calculate Spectrum Averaged F.P Yields
C-Author : A.Trkov, Institute J.Stefan, Ljubljana, Slovenia (1995)
C-Version: 1995 Original distribution
C-V  99/02 Minor corrections to fix bugs.
C-V  99/05 A.Trkov: Fix bugs in comparison mode.
C-V  01/11 A.Trkov: Upgrade to support INTER 6.12 output.
C-V  05/06 A.Trkov & D. L. Aldama: fix bugs processing f.p yields
C-V                                and decay data from JEFF-3.1
C-V  12/14 D. L. Aldama: Converted to Real*8
C-V  2021/07 A. Trkov: Fix long, filenames, double precision and
C-M                    skip Pm-147 contribution from Eu-151
C-V
C-M
C-M  Manual for Program AVRFPY
C-M  =========================
C-M
C-M  The fission product yields data (ENDF-5, ENDF-6 format File 8)
C-M  are analysed. Energy dependent fission yields are accepted.
C-M  They are averaged, making some assumptions about the averaging
C-M  fission reaction rate. For all nuclides in the fission product
C-M  list the decay data are retrieved from the appropriate ENDF
C-M  file. The INTER (Version 6.11 and 6.12) output is also processed
C-M  to extract the thermal (Maxwellian average) cross section, the
C-M  resonance integral and the fission spectrum averaged cross
C-M  section. All information is written onto an intermediate file,
C-M  defined from input (default name AVRFPY.FPY).
C-M    A record on the intermediate file contains the nuclide
C-M  identification, fission yield [fraction], half-life [s], thermal
C-M  cross section [barns], resonance integral [barns], reaction
C-M  fraction (defined as the product of the yield and the average
C-M  cross section) [barns], the number of decay modes (which is zero
C-M  for unknown or stable nuclides) and the capture branching ratio
C-M  for the metastable isomer (if present). For decaying nuclides,
C-M  additional records follow (one for each decay mode), giving the
C-M  decay mode, the branching ratio and the product nuclide. Various
C-M  decay modes are represented by the following symbols:
C-M    g - gamma decay,
C-M    b - beta decay,
C-M    + - positron emission,
C-M    I - isomeric transition,
C-M    a - alpha decay,
C-M    n - neutron emission,
C-M    F - fission,
C-M    p - proton emission.
C-M  In addition, the capture products are identified by the symbol
C-M  "c". These are treated only approximately, assuming that the
C-M  product is in the ground state, except for a few specific cases
C-M  such as Pm-147 where Pm-148 and Pm-148m are produced. The
C-M  branching ratio is built into the code because the appropriate
C-M  data (ENDF file 9 data) are seldom present.
C-M
C-M  The intermediate file which is produced can be processed to
C-M  print the data according to some specified selection criteria
C-M  or to compare the data from different intermediate files.
C-M  The following criteria are applicable:
C-M    EPU - Long decay rate threshold [barn.days] is the criterion
C-M          on the product of the half-life and the capture cross
C-M          section. Above this threshold the fission products
C-M          are treated as stable and their decay influences the
C-M          equillibrium concentration by less then a fraction EPS.
C-M          The EPU criterion depends on the neutron flux level FLUX.
C-M          It is given by:
C-M            EPU = (1.E24/(24*3600)) * DLOG(2) * EPS / FLX
C-M          Typical value: EPU=4.E8 [barn.days]
C-M          Assuming     : EPS=2E-3, FLUX=1E+13 [n/cm2/s]
C-M    EPD - Short decay rate threshold [barn.days] is the criterion
C-M          on the product of the half-life and the capture cross
C-M          section. Below this threshold the fission products
C-M          are very short-lived and their capture reaction rate
C-M          represents less than a fraction EPS of the fission rate.
C-M          The yields of nuclides which fall below this criterion
C-M          are added to their decay products.
C-M          Parameter EPD is given by:
C-M            EPD = (1.E24/(24*3600)) * DLOG(2) / (EPS*FLUX)
C-M          Typical value: EPD=1600 [barn.days]
C-M          Assuming     : EPS=2E-3, FLUX=1E+13 [n/cm2/s]
C-M    EPR - Reaction rate threshold [barns] is the criterion
C-M          on the product of the yield and the capture cross
C-M          section. Below this threshold the capture reaction rate
C-M          represents less than a fraction EPS of the fission rate
C-M          so the fission product is removed from the output
C-M          list. It depends on the neutron flux FLUX and the
C-M          fuel irradiation time TRES.
C-M          Parameter EPR is given by:
C-M            EPR=(2.E24/(24*3600))*(EPS/(FLUX*TRES)
C-M          Typical value: EPR=0.12 [barns]
C-M          Assuming     : EPS=0.5E-4, FLUX=1E+13 [n/cm2/s]
C-M                        ,TRES=1000 [days]
C-M    EPC - Capture product reaction rate threshold [barns**2] is the
C-M          criterion on the product of the yield, the capture cross
C-M          section of the parent nuclide and the capture cross
C-M          section of the capture product. Below this threshold the
C-M          capture reaction rate by the capture product represents
C-M          less than a fraction EPS of the fission rate. Capture
C-M          products, which pass this test, are added to the list
C-M          of nuclides for printout even if their cumulative yield
C-M          from fission is small. The EPC criterion depends on the
C-M          neutron flux FLUX and the fuel irradiation time TRES.
C-M          It is givn by:
C-M            EPC=(6.E48/(24*3600)**2)*(EPS/(FLUX*TRES)**2)
C-M          Typical value: EPC=400 [barns**2]
C-M          Assuming     : EPS=0.5E-4, FLUX=1E+13 [n/cm2/s]
C-M                        ,TRES=1000 [days]
C-M    EPY - lower fission yield threshold [%] to suppress printout
C-M          of the fission product data with low yields.
C-M          Typical value: 0.5%. (See also the EPR criterion).
C-M    EPX - lower cross section threshold [barns] to suppress the
C-M          printout of the fission products with unknown or zero
C-M          cross sections. Typical value: 100 barns. (See also
C-M          the EPR criterion).
C-M
C-M  NOTE: cumulative yields are calculated automatically in the
C-M  case when the independent yields are processed from the source
C-M  ENDF file.
C-M
C-M  AVERAGING SPECTRUM: is required to interpret the cross sections
C-M  and the weighting function for the fission product yields. It
C-M  is defined in three broad groups:
C-M    1  1.E-5 eV  to  5.5 eV
C-M    2  5.5 eV    to  0.1 MeV
C-M    3  0.1 MeV   to  10. MeV
C-M  In group-2 the spectrum is assumed to have a "1/E" shape. The
C-M  average thermal spectrum is given in terms of the spectral
C-M  ratio SPR, defined as the ratio of the spectrum integral in
C-M  groups-2 and -3 to that in group-1. Parameter SPR=2.78 is
C-M  fixed and defined in the DATA statement. Similarly, the fast
C-M  spectrum is given in terms of the ratio of the epithermal to
C-M  fast flux EFR and is defined as the ratio of the spectrum
C-M  integral in group-2 to that in group-3. Parameter EFR=0.928
C-M  is fixed and defined in the DATA statement.
C-M
C-M    The fission product yields must be averaged by the fission
C-M  reaction rate. Since the yields are usually given on a very
C-M  coarse energy grid, a crude approximation to the weighting
C-M  function is adequate. For the parent fissile nucleus the
C-M  thermal cross section, the resonance integral and the fission
C-M  spectrum averaged cross section are read crom the INTER output
C-M  fule. The averaging spectrum is calculated at the coarse group
C-M  energy boundaries defined above, such that the integrals are
C-M  conserved assuming log-log interpolation (the log-log
C-M  interpolation rule is chosen to reproduce correctly the 1/E
C-M  behaviour in the epithermal range). Similarly, the cross
C-M  sections are calculated at the same energy points. The cross
C-M  section at the thermal to epithermal boundary is calculated
C-M  by the log-log interpolation from the 2200 m/s value (assuming
C-M  a 1/v behaviour in the thermal range) and the epithermal
C-M  average cross section at the average energy. When the fission
C-M  product yields are energy-dependent, histogram interpolation
C-M  is assumed, although in principle the interpolation flag is
C-M  given in the file.
C-M
C-M  Instructions:
C-M  -------------
C-M  Data are processed in two steps. In the first one the basic
C-M  files are processed and the intermediate file is written.
C-M  In the second step the intermediate file is processed. The
C-M  printout criteria are defined and the main output files are
C-M  written.
C-M
C-M  Input Intermediate file:
C-M  By default, program proceeds from step two, requesting the
C-M  name of the intermediate file to be processed. Responding
C-M  with "-" for the filename results in processing the basic
C-M  files to produce a new intermediate file on output.
C-M
C-M  Input Data Files:
C-M  To generate a new intermediate file, the filenames and other
C-M  relevant information is requested as follows:
C-M  - FLDY Decay data ENDF file. This file is mandatory.
C-M  - FLNI Fission product yields ENDF file. If blank is entered,
C-M    the selected nuclides are expected from a list file
C-M    (see below).
C-M  - MAT Material number of the parent fissile nuclide for
C-M    which the fission product yields are processed. If blank
C-M    is entered, selection is done through the ZA entry.
C-M  - ZA Parent fissile nuclide designation (ZA=1000*Z+A). This
C-M    is an alternative input to the MAT entry, described above.
C-M  - MT Fission yields reaction number: MT=454 for the independent
C-M    yields and MT=459 for cumulative yields. If independent
C-M    yields are selected, they are summed internally to calculate
C-M    the cumulative yields.
C-M  - EPU Long decay rate threshold [barn.days]. This entry
C-M    request appears only when independent fission product yields
C-M    are to be processed (see also the nuclide selection
C-M    parameters description).
C-M    Typical value: EPU=8.E7 [barn.days]
C-M    Assuming     : EPS=1E-4, FLUX=1E+13 [n/cm2/s]
C-M  - FLLS nuclide list filename, defined when fission product
C-M    yields are not processed. The first  record of this list
C-M    file is a comment header, followed by a list of nuclides
C-M    defined by their ZA value in columns 1-11, one nuclide per
C-M    record, until a blank line or an end-of-file is encountered.
C-M  - FLXS Filename of the INTER output from which the thermal cross
C-M    section, the resonance integral and the fission spectrum
C-M    average cross sections are read. A variant of the format
C-M    is recognised in which the ZA designation has an additional
C-M    decimal digit to identify the metastable isomers. If blank
C-M    name is entered, cross section processing is suppressed.
C-M  - MTX the relevant MT number for the cross sections is specified.
C-M    By default, MTX=102 for radiative capture is assumed.
C-M
C-M  Output files:
C-M  The following user defined output files are defined:
C-M  - FLYI Filename of the intermediate file (default AVRFPY.FPY).
C-M    The request is issued only when a new file is generated.
C-M  - FLNO Main output filename. The file contains the fission yields,
C-M    decay constants and other data for the selected list of
C-M    nuclides. The output is suppressed if "-" is entered.
C-M  - FLYA Auxiliary output filename. The information is the same
C-M    as in the main output filename, except that it is
C-M    intended for reading by the program (default AUXFPY.FPY).
C-M    The output is suppressed if "-" is entered.
C-M  In addition, the log file AVRFPY.LOG is opened unconditionally.
C-M  All error and warning messages are recorded there.
C-M
C-M  Nuclide selection criteria:
C-M  Nuclides to appear on the output can be selected by
C-M  specifying the list of requested nuclides. In this case the
C-M  input request is:
C-M  - FLLS Filename of the nuclide list file. The first
C-M    record of this list file is a comment header, followed by
C-M    a list of nuclides defined by their ZA value in columns
C-M    1-11, one nuclide per record, until a blank line or an
C-M    end-of-file is encountered.
C-M  Alternatively, a set of selection criteria can be
C-M  specified:
C-M  - EPU Long decay rate threshold [barn.days]. This entry
C-M    request appears unless it has been defined when specifying
C-M    the request to process independent fiss.prod. yields.
C-M    Typical value: EPU=8.E7 [barn.days]
C-M    Assuming     : EPS=1E-4, FLUX=1E+13 [n/cm2/s]
C-M  - EPD Short decay rate threshold [barn.days]
C-M    Typical value: EPD=8000 [barn.days]
C-M    Assuming     : EPS=1E-4, FLUX=1E+13 [n/cm2/s]
C-M  - EPR Reaction rate threshold [barns]
C-M    Typical value: EPR=0.25 [barns]
C-M    Assuming     : EPS=1E-4, FLUX=1E+13 [n/cm2/s]
C-M                  ,TRES=1000 [days]
C-M  - EPC Capture product reaction rate threshold [barns**2]
C-M    Typical value: EPC=800 [barns**2]
C-M    Assuming     : EPS=1E-4, FLUX=1E+13 [n/cm2/s]
C-M                        ,TRES=1000 [days]
C-M  - EPY lower fission yield threshold [%]
C-M    Typical value: 0.1%. (See also the EPR criterion).
C-M  - EPX lower cross section threshold [barns]
C-M    Typical value: 100 barns. (See also the EPR criterion).
C-M
C-References:
C-R [1] A.Trkov: Processing of the Fission Product Yield and Decay
C-R     Data, Institite J.Stefan, Ljubljana, Slovenia,
C-R     IJS-DP-draft, (1998).
C-R [2] R.W.Mills: Recommendation for the Use of Energy Dependent
C-R     JEF-2.2 Fission Product Yields, BNFL Sellafield,
C-R     OECD/NEA Data Bank, JEF/DOC-629.
C-
      implicit real*8 (a-h,o-z)
      PARAMETER   (MXFP=5000, MXDK=4, MXST=10)
      CHARACTER*1  CHD, RC(10), FLAG(MXFP), CHR1(MXDK,MXFP)
      CHARACTER*2  MX(10)
      CHARACTER*2  CH2,CS(100)
      CHARACTER*40 BLNK
      CHARACTER*80 FLNM,FLNI,FLXS,FLDY,FLNO,FLYA,FLYI,FLOG
     1            ,FLWI,FLNJ,FLLS
      CHARACTER*80 REC
      DIMENSION    EGB (4),    FRR (4),    FIN (4),  FAV(4)
     1            ,ZAP (MXFP), YLD (MXFP), HLF (MXFP),ZAC(MXFP)
     2            ,PAR1(MXFP), PAR2(MXFP), PAR3(MXFP), IPAR(MXFP)
     3            ,PAR4(MXDK,MXFP),        PAR5(MXDK,MXFP)
     4            ,PAR6(MXFP), PAR7(MXFP), PAR8(MXFP)
     5            ,AMT (MXFP), YDDP(MXST), NDDP(MXST) ,NDDK(MXST)
      DATA  LIN,LDY,LXS,LOU,LKB,LTT,LER,LYA,LYI,LWI,LNJ
     1     /  1,  2,  3,  4,  5,  6,  7,  8,  9, 16, 17 /
      DATA  BLNK/'                                        '/
     1      FLNI/'FPYLD.DAT'/
     2      FLDY/'DECAY.DAT'/
     3      FLXS/'INTER.OUT'/
     4      FLNO/'AVRFPY.OUT'/
     7      FLOG/'AVRFPY.LOG'/
     8      FLYA/'AVRFPY.AUX'/
     9      FLYI/'AVRFPY.FPY'/
     6      FLWI/'AVRFPY.WIM'/
      DATA MX/'  ','M ','M2','M3','M4','M5','M6','M7','M8','Mn'/
      DATA CS/   'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     1 'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca','Sc','Ti',
     2 'V ','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se',
     3 'Br','Kr','Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd',
     4 'Ag','Cd','In','Sn','Sb','Te','I ','Xe','Cs','Ba','La','Ce',
     5 'Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',
     6 'Pr','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl','Pb',
     7 'Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U ','Np','Pu',
     8 'Am','Cm','Bk','Cf','Es','Fm'/
C* Upper removal threshold [barn.days] identifying stable nuclides
      DATA EPU/0/
C* Thermal, epithermal and fast group boundaries and average flux
      DATA EGB/ 1.D-5, 0.55D0, 1.D+5, 1.D+7 /
C* Define group flux integrals with respect to the epithermal flux
      DATA SPR,EFR/ 5.785, 0.9279 /
      FIN(2)= DLOG(EGB(3)/EGB(2))
      FIN(3)= FIN(2)/EFR
      FIN(1)=(FIN(2)+FIN(3))/SPR
      IFPY=0
C*
C* Write banner to terminal screen
      WRITE(LTT,891)
C* Open the log file
      OPEN (UNIT=LER,FILE=FLOG,STATUS='UNKNOWN')
      WRITE(LER,891)
C*
C* Analyse a processed F.P. yields file or generate a new one
    1 WRITE(LTT,692) ' Define or select F.P. parameters file  '
      WRITE(LTT,692) ' -------------------------------------  '
      WRITE(LTT,692)
      WRITE(LTT,692) ' Default F.P.param.file to be processed:',FLYI
      WRITE(LTT,692) '        Enter new name to redefine    or'
      WRITE(LTT,692) '$       Enter "-" to generate new file: '
      READ (LKB,691) FLNM
      IYI=0
      IF(FLNM(1:1).EQ.'-') GO TO 20
      IF(FLNM.NE.BLNK) FLYI=FLNM
      OPEN (UNIT=LYI,FILE=FLYI,STATUS='OLD',ERR=1)
C* Read the processed yields file
      WRITE(LTT,692)
      WRITE(LTT,692) ' Reading F.P. parameters file         : ',FLYI
C*
      READ (LYI,692) FLNM,FLNI
      READ (LYI,692) FLNM,FLDY
      READ (LYI,692) FLNM,FLXS
      READ (LYI,803) FLNM,MT0
      READ (LYI,803) FLNM,MTX
      READ (LYI,804) FLNM,FIN1
      READ (LYI,804) FLNM,FIN2
      READ (LYI,804) FLNM,FIN3
      READ (LYI,803) FLNM,MAT
      READ (LYI,804) FLNM,ZA
      READ (LYI,803) FLNM,NFP
      READ (LYI,802) FLNM,EPU
      READ (LYI,803) FLNM
      IF(FIN1.NE.0) FIN(1)=FIN1
      IF(FIN2.NE.0) FIN(2)=FIN2
      IF(FIN3.NE.0) FIN(3)=FIN3
      DO 3 I=1,NFP
        READ (LYI,874)
     1   ZAP(I),YLD(I),HLF(I),PAR1(I),PAR2(I),PAR3(I)
     1  ,IPAR(I),PAR6(I),ZAC(I)
        AMT(I)=ZAP(I)
        NDK=MIN(MXDK,IPAR(I))
        IF(NDK.LE.0) GO TO 3
        DO 2 J=1,NDK
          READ (LYI,875) CHR1(J,I),PAR4(J,I),PAR5(J,I)
    2   CONTINUE
    3 CONTINUE
      CLOSE(UNIT=LYI)
      IYI=1
      IF(MT0.GT.0) IFPY=1
C*
C* Define the output list file
    4 WRITE(LTT,692)
      WRITE(LTT,692) ' Define the output files                '
      WRITE(LTT,692) ' -----------------------                '
      WRITE(LTT,692)
      WRITE(LTT,692) ' Default output list filename         : ',FLNO
      WRITE(LTT,692) '$       Enter new name or "-" to quit : '
      READ (LKB,692) FLNM
      IF(FLNM(1:1).EQ.'-') GO TO 90
      IF(FLNM(1:40).NE.BLNK) FLNO=FLNM
      OPEN (UNIT=LOU,FILE=FLNO,STATUS='UNKNOWN')
C*
C* Auxilliary processed F.P. yields file
    5 WRITE(LTT,692) ' Short F.P. parameters filename       : ',FLYA
      WRITE(LTT,692) '$     Enter new name or "-" to ignore : '
      READ (LKB,691) FLNM
      IYA=0
      IF(FLNM(1:1).NE.'-') THEN
      IF(FLNM(1:40).NE.BLNK) FLYA=FLNM
        WRITE(LTT,692) '               Compare reference file : ',FLYI
        WRITE(LTT,692) '                                 to   : ',FLYA
        WRITE(LTT,692) '$                             (y=Yes) : '
      READ (LKB,691) FLNM
        IF(FLNM(1:1).EQ.'y' .OR. FLNM(1:1).EQ.'Y') THEN
          LYC=ABS(LYC)
          OPEN (UNIT=LYC,FILE=FLYA,STATUS='OLD',ERR=5)
          GO TO 80
        END IF
C        WRITE(LTT,692) '$            Write a new file (y=Yes) : '
C        READ (LKB,691) FLNM
C        IF(FLNM(1:1).NE.'y' .AND. FLNM(1:1).NE.'Y') GO TO 5
        LYA=ABS(LYI)
        OPEN (UNIT=LYA,FILE=FLYA,STATUS='UNKNOWN',ERR=5)
        IYA=1
      END IF
C*
C* Define the thresholds for printout
      IF(MT0.NE.0) THEN
        WRITE(LTT,692)
        WRITE(LTT,692) ' Define nuclide selection criteria      '
        WRITE(LTT,692) ' ---------------------------------      '
        WRITE(LTT,692)
C*
    6   WRITE(LTT,692) '$Enter nuclide list file/blank=ignore : '
        READ (LKB,691) FLLS
        ILST=0
        IF(FLLS(1:40).EQ.BLNK) GO TO 16
C* Read the list of nuclides from a file
        OPEN (UNIT=LIN,FILE=FLLS,STATUS='OLD',ERR=21)
        READ (LIN,691,END=20) FLNM
        WRITE(LTT,692) '                  Nuclide list header : ',FLNM
        ILST=1
        DO I=1,NFP
          FLAG(I)='L'
        END DO
    8   READ (LIN,691,END=12) FLNM
        IF(FLNM(1:40).EQ.BLNK) GO TO 12
        READ (FLNM,694) ZAPI,AMTI
        IF(AMTI.EQ.0) AMTI=ZAPI
        DO 9 I=1,NFP
          IF(NINT(10.*ZAP(I)).NE.NINT(10.*ZAPI)) GO TO 9
          FLAG(I)='9'
          AMT (I)=AMTI
          GO TO 8
    9   CONTINUE
        WRITE(LTT,806) ' AVRFPY WARNING - No fiss. prod.  ZA  : '
     1                ,IDINT(ZAPI)
        GO TO 8
   12   CLOSE(UNIT=LIN)
        GO TO 19
C* Define nuclide selection criteria explicitly
   16   WRITE(LTT,692)
        WRITE(LTT,692) ' Enter main printout selection criteria '
        WRITE(LTT,692) ' -------------------------------------- '
        WRITE(LTT,692)
        IF(EPU.LE.0) THEN
          WRITE(LTT,692) '$Long  decay rate thresh. [barn.days] : '
          READ (LKB,694,ERR=16) EPU
        END IF
        WRITE(LTT,692) '$Short decay rate thresh. [barn.days] : '
        READ (LKB,694,ERR=16) EPD
        WRITE(LTT,692) '$Reaction rate thresh. [barns]        : '
        READ (LKB,694,ERR=16) EPR
        WRITE(LTT,692) '$Capt.Prod.Reac.rate thresh.[barns**2]: '
        READ (LKB,694,ERR=16) EPC
C*
   17   WRITE(LTT,692)
        WRITE(LTT,692) ' Auxiliary  printout selection criteria '
        WRITE(LTT,692)
        WRITE(LTT,692) '$Enter fission yield threshold   [%]  : '
        READ (LKB,694,ERR=17) EPY
        WRITE(LTT,692) '$Enter cross sect. threshold  [barns] : '
        READ (LKB,694,ERR=17) EPX
      END IF
   19 IF(IYA.NE.1) GO TO 60
C*
C* Write the auxiliary file
      WRITE(LYA,692) ' Fission product yields ENDF file     : ',FLNI
      WRITE(LYA,692) ' Decay data ENDF file                 : ',FLDY
      WRITE(LYA,692) ' INTER output file processed          : ',FLXS
      WRITE(LYA,803) ' Fission product yields             MT: ',MT0
      WRITE(LYA,803) ' Cross sections for reaction        MT: ',MTX
      WRITE(LYA,804) ' Thermal flux integral                : ',FIN(1)
      WRITE(LYA,804) ' Epithermal flux integral             : ',FIN(2)
      WRITE(LYA,804) ' Fast flux integral                   : ',FIN(3)
      WRITE(LYA,803) ' Fissile parent MAT number            : ',MAT
      WRITE(LYA,804) '                ZA  designation       : ',ZA
C     WRITE(LYA,803) ' Number of fission product nuclides   : ',NFP
      GO TO 60
C*
C* Define the F.P. yields ENDF file to be processed
   20 WRITE(LTT,692) ' Define ENDF files to generate F.P.param'
      WRITE(LTT,692) ' ---------------------------------------'
      WRITE(LTT,692)
      IDE=0
      WRITE(LTT,692) '$Enter Decay data file/blank to ignore: '
      READ (LKB,691)   FLDY
      IF(FLDY.NE.BLNK)
     1OPEN (UNIT=LDY,FILE=FLDY,STATUS='OLD',ERR=20)
      IDE=1
   21 WRITE(LTT,692) '$Enter F.P.yields file/blank to ignore: '
      READ (LKB,691) FLNI
      IF(FLNI(1:40).NE.BLNK) THEN
C*
C* Case: Fission products yield ENDF file specified
      OPEN (UNIT=LIN,FILE=FLNI,STATUS='OLD',ERR=21)
C* Select the material number and reaction type
   22 WRITE(LTT,692) ' Select parent fissile nuclide MAT/ZA   '
      WRITE(LTT,692) '$      Enter MAT or zero to select ZA : '
      READ (LKB,693,ERR=22) MAT
      IF(MAT.LE.0) THEN
   23   WRITE(LTT,692) '$                  Enter ZA =1000*Z+A : '
        READ (LKB,694,ERR=23) ZA
      ELSE
C* Extract the ZA value from the F.P. Yields file
   26   READ (LIN,812) REC,MM
        IF(MM.LT.0) STOP 'AVRFPY ERROR - Material not in FPY file'
        IF(MM.NE.MAT) GO TO 26
        READ (REC,814) ZA
        READ (LIN,812) REC
        READ (REC,814) DMY,DMY,DMY,ZA1
        ZA=ZA+0.1*ZA1
        REWIND LIN
      END IF
   24 WRITE(LTT,692) '$Enter the  MT number (454 OR 459)    : '
      READ (LKB,693,ERR=24) MT0
      IFPY=1
      IF(MT0.LE.0) GO TO 34
      IF(MT0.EQ.454) THEN
   25 WRITE(LTT,692) '$Long  decay rate thresh. [barn.days] : '
      READ (LKB,694,ERR=25) EPU
      END IF
      IF(MT0.NE.454 .AND. MT0.NE.459) GO TO 24
      ELSE
C*
C* Case: Nuclide list specified
      WRITE(LTT,692) '$Enter nuclide selection list file    : '
      READ (LKB,691) FLLS
      IF(FLLS(1:40).EQ.BLNK) GO TO 21
      OPEN (UNIT=LIN,FILE=FLLS,STATUS='OLD',ERR=21)
      READ (LIN,691,END=20) FLNM
      WRITE(LTT,692) '                  Nuclide list header : ',FLNM
      IFPY=0
      NFP =0
   27 READ (LIN,691,END=28) FLNM
      IF(FLNM(1:40).EQ.BLNK) GO TO 28
      NFP=NFP+1
      READ (FLNM,694) ZAP(NFP)
      YLD(NFP)=0.
      FLAG(NFP)='9'
      GO TO 27
   28 CLOSE(UNIT=LIN)
      END IF
C*
C* Define the cross sections file (INTER output)
   34 MTX=0
      WRITE(LTT,692) '$Enter INTER output /blank to ignore  : '
      READ (LKB,691)   FLXS
      IF(FLXS(1:40).NE.BLNK) THEN
      OPEN (UNIT=LXS,FILE=FLXS,STATUS='OLD',ERR=34)
C* Define the reaction type number MT
   35 WRITE(LTT,692) ' Default reaction is Capture (MT=102)   '
      WRITE(LTT,692) '$            Enter new MT to redefine : '
      READ (LKB,693,ERR=35) MT
      MTX=102
      IF(MT.NE.0) MTX=MT
      END IF
C*
C* Define the filename to store the processed F.P. yields
   36 WRITE(LTT,692) ' Processed F.P. parameters output file: ',FLYI
      WRITE(LTT,692) '$           Enter new name to redefine: '
      READ (LKB,692) FLNM
      IF(FLNM(1:40).NE.BLNK) FLYI=FLNM
      OPEN (UNIT=LYI,FILE=FLYI,STATUS='UNKNOWN')
C* Label the Log-file
      WRITE(LER,692) ' Decay data ENDF file                 : ',FLDY
      IF(IFPY.EQ.1) THEN
      WRITE(LER,692) ' Fission product yields ENDF file     : ',FLNI
      WRITE(LER,803) ' Fission product yields             MT: ',MT0
      WRITE(LER,892) MAT,IDINT(ZA+0.1)
      ELSE
      WRITE(LER,692) ' Fission product list file            : ',FLLS
      END IF
      WRITE(LER,692)
C*
C* Define the weighting function for the F.P yields
      FF=0
      DO IG=1,3
        FAV(IG)=FIN(IG)
        FRR(IG)=0
        FF=FF+FAV(IG)
      END DO
      FRR( 4)=0
      FRR( 2)=(1/EGB(2))*FIN(2)/DLOG(EGB(3)/EGB(2))
      IF(MTX.LE.0 .OR. IFPY.LE.0) GO TO 38
C* If cross sections file is given, find the fissile parent fiss.x-sect.
      CALL REACRT(LXS,LER,18,1,ZA,FTH,FRI,FFS ,IER)
      REWIND LXS
      IF(IER.LT.0) THEN
        WRITE(LER,883) MAT,INT(ZA)
        WRITE(LTT,883) MAT,INT(ZA)
      ELSE
        FAV(1)=FAV(1)*FTH
        FAV(2)=FAV(2)*FRI/FIN(2)
        FAV(3)=FAV(3)*FFS
        FF    =FAV(1)+FAV(2)+FAV(3)
        xfct=(2.d0/DSQRT(3.1415926d0))
        FRR(2)=FRR(2)*FTH*xfct*DSQRT(0.0253d0/EGB(2))
        IF(FTH.LE.0) FRR(3)=FAV(3)/(EGB(4)-EGB(3))
      END IF
C* Define other points log-log interpolable conserving the integrals
   38 CALL FLXPNT(3,EGB,FRR,FAV,IER)
      WRITE(LER,881)
      DO 39 I=1,4
      WRITE(LER,882) EGB(I),FRR(I)/FF
   39 CONTINUE
      WRITE(LER,692)
C*
C* Begin processing the data
      IF(IFPY.NE.1) GO TO 41
      MT =MT0
C* Process the fission products yields data
      CALL FPRYLD(LIN,LER,MAT,MT,ZA,NFP,ZAP,YLD
     1           ,PAR1,PAR2,PAR3,PAR4,PAR5 ,EGB,FRR ,MXFP,IER)
      IF(IER.GT.0) THEN
        WRITE(LTT,692) ' ERROR reading file                     ',FLNI
        WRITE(LTT,803) ' IER                                    ',IER
        GO TO 90
      END IF
      WRITE(LTT,892) MAT,IDINT(ZA+0.1)
C*
   41 CLOSE(UNIT=LIN)
C* Assign thermal x-sect. and res.int. to the list of F.P. nuclides
      DO I=1,NFP
        PAR1(I)=0
        PAR2(I)=0
        PAR3(I)=0
      END DO
      IF(MTX.GT.0) CALL REACRT(LXS,LER,MTX,NFP,ZAP,PAR1,PAR2,PAR3 ,IER)
      IF(IER.NE.0) WRITE(LTT,896) IER
      CLOSE(UNIT=LXS)
C* Assign the decay data to the list of F.P. nuclides
      DO I=1,NFP
        HLF (I)=0
        IPAR(I)=0
      END DO
      IF(IDE.GT.0) CALL DECYMD(LDY,LER,NFP,ZAP,HLF,IPAR
     1                        ,MXDK,CHR1,PAR4,PAR5 ,IER)
C* Assign capture product branching ratio for the isomer
      CALL CAPBRR(NFP,ZAP,PAR6)
C* Save the results
      WRITE(LYI,692) ' Fission product yields ENDF file     : ',FLNI
      WRITE(LYI,692) ' Decay data ENDF file                 : ',FLDY
      WRITE(LYI,692) ' INTER output file processed          : ',FLXS
      WRITE(LYI,803) ' Fission product yields             MT: ',MT0
      WRITE(LYI,803) ' Cross sections for reaction        MT: ',MTX
      WRITE(LYI,804) ' Thermal flux integral                : ',FIN(1)
      WRITE(LYI,804) ' Epithermal flux integral             : ',FIN(2)
      WRITE(LYI,804) ' Fast flux integral                   : ',FIN(3)
      WRITE(LYI,803) ' Fissile parent MAT number            : ',MAT
      WRITE(LYI,804) '                ZA  designation       : ',ZA
      WRITE(LYI,803) ' Number of fission product nuclides   : ',NFP
      WRITE(LYI,802) ' Long decay rate threshold [barn.days]: ',EPU
      WRITE(LYI,692) '        ZA     yield    T(1/2)      Sig0'
     1              ,'      R.I.        RR D      B.R.        '
      DO 48 I=1,NFP
C* Special print sequence for an extra digit in yield
c     REC=BLNK//BLNK
c     WRITE(REC,874)
c    1 ZAP(I),YLD(I),HLF(I),PAR1(I),PAR2(I),PAR3(I)
C    1,IPAR(I),PAR6(I),ZAC(I)
c     REC(12:17)=REC(11:16)
c     REC(11:11)=' '
c     WRITE(LYI,691) REC
      WRITE(LYI,874)
     1 ZAP(I),YLD(I),HLF(I),PAR1(I),PAR2(I),PAR3(I)
     1,IPAR(I),PAR6(I),ZAC(I)
C*
      NDK=MIN(MXDK,IPAR(I))
      IF(NDK.LE.0) GO TO 48
      DO 47 J=1,NDK
      WRITE(LYI,875) CHR1(J,I),PAR4(J,I),PAR5(J,I)
   47 CONTINUE
   48 CONTINUE
      CLOSE(UNIT=LYI)
      GO TO 4
C*
C* Write the results to output - label the output file
   60 WRITE(LOU,891)
      WRITE(LOU,692) ' Fission product yields ENDF file     : ',FLNI
      WRITE(LOU,692) ' Decay data ENDF file                 : ',FLDY
      WRITE(LOU,692) ' INTER output file processed          : ',FLXS
      WRITE(LOU,692)
      IF(MT0.EQ.454)
     1WRITE(LOU,803) ' Independent fission product yields MT: ',MT0
      IF(MT0.EQ.459)
     1WRITE(LOU,803) ' Cumulative fission product yields  MT: ',MT0
      IF(MTX.NE.0)
     1WRITE(LOU,803) ' Cross sections for reaction        MT: ',MTX
      WRITE(LOU,804) ' Epith.+Fast/thermal spectral ratio   : '
     1              ,(FIN(2)+FIN(3))/FIN(1)
      WRITE(LOU,804) ' Epithermal/fast spectral ratio       : '
     1              , FIN(2) / FIN(3)
      WRITE(LOU,692)
      WRITE(LOU,692) ' Reactions imply the fraction of reactio'
     1              ,'n rates on the specific fission product '
     2              ,' per unit flux (reaction=yield*aver.cros'
     2              ,'s-section).                             '
C*
      IF(ILST.EQ.1) THEN
        WRITE(LOU,692)
        WRITE(LOU,692) ' Nuclide list read from file          : ',FLLS
      ELSE
        IPU=EPU
        WRITE(LOU,692)
        WRITE(LOU,804) ' Printout constraint thresholds:        '
        IF(EPU.GT.0)
     1  WRITE(LOU,806) '   Long  decay rate thresh.[barn.days] :',IPU
        IF(EPD.GT.0)
     1  WRITE(LOU,804) '   Short decay rate thresh.[barn.days] :',EPD
        IF(EPR.GT.0)
     1  WRITE(LOU,804) '   Reaction rate thresh.       [barns] :',EPR
        IF(EPC.GT.0)
     1  WRITE(LOU,804) '   Capt.Prod.Reac.Rate thr. [barns**2] :',EPC
        IF(EPY.GT.0)
     1  WRITE(LOU,804) '   Fission product yield           [%] :',EPY
        IF(EPX.GT.0)
     1  WRITE(LOU,804) '   Cross sections(th,epith,av) [barns] :',EPX
      END IF
C*
      WRITE(LOU,892) MAT,IDINT(ZA+0.1)
      WRITE(LOU,893)
C* Label the Log-file
      WRITE(LER,692) ' Fission product yields ENDF file     : ',FLNI
      WRITE(LER,692) ' Decay data ENDF file                 : ',FLDY
      WRITE(LER,803) ' Fission product yields             MT: ',MT0
      WRITE(LER,892) MAT,IDINT(ZA+0.1)
      WRITE(LER,692)
C* Check the internal consistency of the cumulative yields data
      IF(MT0.EQ.459)
     1  CALL CHKCUM(LER,NFP,EPU,ZAP,YLD,HLF,PAR1,PAR2,PAR3,IPAR
     1             ,MXDK,PAR4,PAR5,CHR1,FIN)
C* Calculate cumulative yields if independent yields are given
      EPUC=EPU
      IF(MT0.EQ.454) THEN
        CALL YLDCUM(LER,NFP,EPU,ZAP,YLD,HLF,PAR1,PAR2,PAR3,IPAR
     1             ,MXDK,PAR4,PAR5,PAR7,PAR8,FIN)
        EPUC=0
      END IF
C* Check the printout criteria
      IF(IFPY.EQ.1 .AND. ILST.EQ.0)
     1CALL CHKFPY(LER,NFP,ZAP,YLD,HLF,PAR1,PAR2,PAR3,IPAR,MXDK
     1           ,PAR4,PAR5,PAR6,FLAG,EPY,EPX,EPR,EPD,EPUC,EPC,FIN)
C* Loop over all fission product niclides
      NMT=0
      DO I=1,NFP
        IF(FLAG(I).LE.'9') NMT=NMT+1
      END DO
      IF(IYA.GT.0) THEN
        WRITE(LYA,803) ' Number of fission product nuclides   : ',NMT
        WRITE(LYA,692) '        ZA     yield    T(1/2)      Sig0'
     1                ,'      R.I.        RR D      B.R.        '
      END IF
      FPT=0
      RRT=0
      DO 65 I=1,NFP
      IF(FLAG(I).GT.'9') GO TO 65
      IZA=ZAP(I)+0.01
      IZ =IZA/1000
      IA =IZA-1000*IZ
      MM=MIN(10., 10.*(ZAP(I)-FLOAT(IZA))+1.01)
      YI =YLD(I)*100
      XF =PAR3(I)
      RI =PAR2(I)
      XT =PAR1(I)
      XE =RI/FIN(2)
      XA =XSAV3G(XT,XE,XF,FIN)
      XX =MAX(XT,XE,XF,XA)
      RR =YLD(I)*XA
      HL =HLF(I)
C* Decay constant
      DC =0
      IF(HL.GT.0) DC=DLOG(2.d0)/HL
C* Half-life in days
      HD =HL/FLOAT(24*3600)
      ND =IPAR(I)
C* Chemical symbol
        CH2='  '
        IF(IZ.GT.0 .AND. (IA.EQ.0 .OR. IA.LE.3*IZ)) CH2=CS(IZ)
C* Print record to output  (check Format 893)
      WRITE(LOU,894) IZ,CH2,IA,MX(MM),YI,XT,NINT(RI),RR,DC,FLAG(I)
C     WRITE(LOU,894) IZ,CH2,IA,MX(MM),YI,XT,NINT(RI),RR,HL,FLAG(I)
C     WRITE(LOU,894) IZ,CH2,IA,MX(MM),YI,XT,NINT(RI),RR,HD,FLAG(I)
C* Print the capture product when present in the ZAC field
      IF(ZAC(I).GT.0 .AND. PAR6(I).GT.-1.) THEN
        IZC=ZAC(I)/1000.
        IAC=ZAC(I)-1000*IZC
        CH2='  '
        IF(IZC.GT.0 .AND. (IAC.EQ.0.OR.IAC.LE.3*IZC)) CH2=CS(IZC)
        WRITE(LOU,898) 'c',1.0-PAR6(I),IZC,CH2,IAC
      END IF
C* Print the capture product when the FLAG is set
      IF(FLAG(I).EQ.'3' .OR. FLAG(I).EQ.'5') THEN
        IA1=IA+1
        ZAC(I)=IZ*1000+IA1
        WRITE(LOU,898) 'c',1.0-PAR6(I),IZ,CH2,IA1
        IF(PAR6(I).NE.0)
     1  WRITE(LOU,898) 'c',    PAR6(I),IZ,CH2,IA1,MX(2)
      END IF
      IF(IYA.GT.0)
     1WRITE(LYA,874)
     1 ZAP(I),YLD(I),HLF(I),PAR1(I),PAR2(I),PAR3(I)
     1,IPAR(I),PAR6(I),ZAC(I)
      IF(ND.LT.1) GO TO 64
      DO 63 J=1,ND
      JZA=PAR5(J,I)
      JZ =JZA/1000
      JA =JZA-1000*JZ
      JM =MIN(10., 10.*(PAR5(J,I)-FLOAT(JZA))+1.01)
      CH2='  '
      IF(JZ.GT.0 .AND. (JA.EQ.0 .OR. JA.LE.3*JZ)) CH2=CS(JZ)
      WRITE(LOU,898) CHR1(J,I),PAR4(J,I),JZ,CH2,JA,MX(JM)
      IF(IYA.GT.0)
     1WRITE(LYA,875) CHR1(J,I),PAR4(J,I),PAR5(J,I)
   63 CONTINUE
   64 FPT=FPT+YI
      RRT=RRT+RR
   65 CONTINUE
      IF(IYA.GT.0) THEN
        CLOSE(UNIT=LYA)
        IYA=0
      END IF
C     IF(NMT.GT.0) WRITE(LOU,895) NMT,FPT,RRT
      IF(NMT.GT.0) WRITE(LOU,895) NMT
C* Write the yields for NJOY and in WIMS-D library format
      IF(FLWI.EQ.BLNK .OR. ZA.LE.0) GO TO 78
C* Subtract the yields from explicitly represented decay products
      DO 75 I=1,NFP
      IF(FLAG(I).LE.'0' .OR. FLAG(I).GT.'9') GO TO 75
      IF(IPAR(I).LE.0) GO TO 75
      K  =I
      JD =0
      YDI=YLD(K)
C* Next nuclide
   71 ND =IPAR(K)
      IF(ND.LE.0) GO TO 72
C*     Check for nearly stable nuclides
      XA =XSAV3G(PAR1(K),PAR2(K)/FIN(2),PAR3(K),FIN)
      HL =HLF(K)/FLOAT(3600 * 24)
      IF(XA.GT.0) HL=XA*HL
      IF(EPU.GT.0 .AND. (HL.LE.0 .OR. HL.GT.EPU)) GO TO 72
        IF (JD .GE. MXST) THEN
          WRITE(LTT,884) JD, ZAP(I)
          NDDP(JD)=0
          GO TO 72
        END IF
C* If decaying, increase level JD
        JD=JD+1
        NDDP(JD)=ND
        NDDK(JD)=K
        YDDP(JD)=YDI
C* NDDP contains No. of remaining decay products in level JD
   72 J =NDDP(JD)
      K =NDDK(JD)
      IF(J.LT.1) THEN
C* No more decay products in this level - decrease level
        JD=JD-1
        IF(JD.LT.1) GO TO 75
        GO TO 72
      END IF
      YDI=YDDP(JD)
      NDDP(JD)=J-1
C* Process the yield for this nuclide
      YDI=YDI*PAR4(J,K)
      ZAM=PAR5(J,K)
      CALL SRCIDX(NFP,ZAP,ZAM,KK)
      IF(ABS(ZAP(KK)-ZAM).GT.0.05) THEN
        WRITE(LTT,886) ZAM,YDI,PAR4(J,K)
        GO TO 72
      END IF
      K=KK
      IF(FLAG(K).GT.'0' .AND. FLAG(K).LE.'9') THEN
        YLD(K)=MAX(0., YLD(K)-YDI)
      END IF
      GO TO 71
   75 CONTINUE
      DO I=1,8
        PAR8(I)=0
      END DO
      NP=8
      AMW=ZA
      DO 77 I=1,NFP
      IF(ABS(ZAP(I)-ZA).LT.0.01) AMW=AMT(I)
      IF(FLAG(I).LE.'0' .OR. FLAG(I).GT.'9') GO TO 77
      PAR8(NP+1)=YLD(I)
      PAR8(NP+2)=AMT(I)
      NP=NP+2
   77 CONTINUE
      ICH=1
      ISTOP=0
      FLNJ=BLNK
      DO WHILE (ISTOP.NE.1)
        IF(FLNO(ICH:ICH).NE.'.') THEN
          FLNJ(ICH:ICH)=FLNO(ICH:ICH)
          ICH=ICH+1
        ELSE IF (FLNO(ICH+1:ICH+1).EQ.'.') THEN
          FLNJ(ICH:ICH)=FLNO(ICH:ICH)
          ICH=ICH+1
          FLNJ(ICH:ICH)=FLNO(ICH:ICH)
          ICH=ICH+1
        ELSE
          ISTOP=1
          FLNJ(ICH:ICH)=FLNO(ICH:ICH)
          FLNJ(ICH+1:ICH+1)='N'
          FLNJ(ICH+2:ICH+2)='J'
          FLNJ(ICH+3:ICH+3)='B'
        ENDIF
        IF (ICH.GT.37) THEN
          FLNJ='AVRFPY.NJB'
          ISTOP=1
        ENDIF
      END DO
      OPEN (UNIT=LWI,FILE=FLWI,STATUS='UNKNOWN')
      PAR8(2)=AMW
      WRITE(LWI,809) NP
      WRITE(LWI,808) (PAR8(J),IDINT(PAR8(J+1)),J=1,NP,2)
      CLOSE(UNIT=LWI)
C* Write the fission product yields for NJOY input
      OPEN (UNIT=LNJ,FILE=FLNJ,STATUS='UNKNOWN')
      WRITE(LNJ,807) NP/2,0.
      WRITE(LNJ,807) (IDINT(PAR8(J+1)),PAR8(J),J=9,NP,2)
      CLOSE(UNIT=LNJ)
C*
C* Sort and list the rejected nuclides
   78 K=0
      DO 79 I=1,NFP
      IF(FLAG(I).LE.'9') GO TO 79
      K=K+1
      ZAP(K)=ZAP(I)
      FLAG(K)=FLAG(I)
   79 CONTINUE
      IF(K.GT.0) THEN
        WRITE(LER,688)
        WRITE(LER,689) (ZAP(I),FLAG(I),I=1,K)
      END IF
      WRITE(LTT,692)
      WRITE(LTT,803) ' Total number of fission products     : ',NFP
      WRITE(LTT,803) ' Number of nuclides printed           : ',NMT
      WRITE(LTT,803) ' Number of nuclides skipped           : ',K
      GO TO 90
C* Processed F.P. yields file comparison
   80 WRITE(LOU,891)
      WRITE(LOU,692)
      WRITE(LOU,692) ' REFERENCE data file                  : ',FLYI
      WRITE(LOU,692) ' COMPARED  data file                  : ',FLYA
      WRITE(LOU,692)
      WRITE(LOU,692) ' The reference data are quoted, followed'
     1              ,' by the compared data, expressed        '
      WRITE(LOU,692) ' as % difference where relevant.        '
      WRITE(LOU,692)
C*
      WRITE(LOU,692) ' Fission product yields ENDF file     : ',FLNI
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,692) ' Decay data ENDF file                 : ',FLDY
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,692) ' INTER output file processed          : ',FLXS
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,803) ' Fission product yields             MT: ',MT0
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,803) ' Cross sections for reaction        MT: ',MTX
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,804) ' Thermal flux integral                : ',FIN(1)
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,804) ' Epithermal flux integral             : ',FIN(2)
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,804) ' Fast flux integral                   : ',FIN(3)
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,803) ' Fissile parent MAT number            : ',MAT
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,804) '                ZA  designation       : ',ZA
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
      WRITE(LOU,803) ' Number of fission product nuclides   : ',NFP
      READ (LYC,692) FLNM,FLNM
      WRITE(LOU,692) BLNK,FLNM
C*
      WRITE(LOU,893)
      NDF=0
   82 READ (LYC,874,ERR=82,END=88)
     1 ZAPI,YLDI,HLFI,PAR1I,PAR2I,PAR3I,IPARI,PAR6I,ZACI
      IF(ZAPI.LE.0) GO TO 82
      CALL SRCIDX(NFP,ZAP,ZAPI,I)
      IF(ABS(ZAP(I)-ZAPI).GT.0.05) GO TO 82
      IZA=ZAP(I)+0.01
      IZ =IZA/1000
      IA =IZA-1000*IZ
      MM=MIN(10., 10.*(ZAP(I)-FLOAT(IZA))+1.01)
      YI =YLD(I)*100.
      XF =PAR3(I)
      RI =PAR2(I)
      XT =PAR1(I)
      XE =RI/FIN(2)
      XA =XSAV3G(XT,XE,XF,FIN)
      RR =YLD(I)*XA
      HL =HLF(I)
C* Decay constant
      DC =0
      IF(HL.GT.0) DC=DLOG(2.d0)/HL
C* Half-life in days
      HD =HL/FLOAT(24*3600)
C*
      CHD=' '
      IF(IPAR(I).NE.0) CHD='d'
C*
      WRITE(LOU,894) IZ,CS(IZ),IA,MX(MM),YI,XT,NINT(RI),RR,DC,CHD
C     WRITE(LOU,894) IZ,CS(IZ),IA,MX(MM),YI,XT,NINT(RI),RR,HL,CHD
C     WRITE(LOU,894) IZ,CS(IZ),IA,MX(MM),YI,XT,NINT(RI),RR,HD,CHD
      RRI=YLDI*XSAV3G(PAR1I,PAR2I/FIN(2),PAR3I,FIN)
      PYI=0
      PXT=0
      PRI=0
      PRR=0
      PHL=0
      IF(YI.GT.0) PYI =100.*( YLDI/YLD(I)-1.)
      IF(XT.GT.0) PXT =100.*(PAR1I/XT    -1.)
      IF(RI.GT.0) PRI =100.*(PAR2I/RI    -1.)
      IF(RR.GT.0) PRR =100.*(  RRI/RR    -1.)
      IF(HL.GT.0) PHL =100.*( HLFI/HL    -1.)
C* Upper bound
      IF(PYI.GT. 9999.9) PYI = 9999.9
      IF(PXT.GT. 9999.9) PXT = 9999.9
      IF(PRI.GT. 9999.9) PRI = 9999.9
      IF(PRR.GT. 9999.9) PRR = 9999.9
      IF(PHL.GT. 9999.9) PHL = 9999.9
C* Lower bound
      IF(PYI.LT.-9999.9) PYI =-9999.9
      IF(PXT.LT.-9999.9) PXT =-9999.9
      IF(PRI.LT.-9999.9) PRI =-9999.9
      IF(PRR.LT.-9999.9) PRR =-9999.9
      IF(PHL.LT.-9999.9) PHL =-9999.9
C*
      PMX=MAX(ABS(PYI),ABS(PXT),ABS(PRI),ABS(PRR),ABS(PHL))
      CHD=' '
      IF(IPARI.NE.0) CHD='d'
      IF(PMX.GT.0.05) THEN
        WRITE(REC,897) PYI,PXT,PRI,PRR,PHL,CHD
        IF( YLDI.EQ.0 .OR. YLD(I).EQ.0) REC(12:20)='         '
        IF(PAR1I.EQ.0 .OR. XT    .EQ.0) REC(21:30)='          '
        IF(PAR2I.EQ.0 .OR. RI    .EQ.0) REC(31:40)='          '
        IF(  RRI.EQ.0 .OR. RR    .EQ.0) REC(41:50)='          '
        IF( HLFI.EQ.0 .OR. HL    .EQ.0) REC(51:60)='          '
        WRITE(LOU,691) REC
        NDF=NDF+1
      END IF
      DO 86 J=1,IPARI
      READ (LYC,875)
   86 CONTINUE
      GO TO 82
   88 WRITE(LOU,885) NDF
      CLOSE(UNIT=LOU)
      CLOSE(UNIT=LYC)
C* All data processed
   90 STOP 'AVRFPY Completed'
C*
  691 FORMAT(A80)
  692 FORMAT(2A40)
  693 FORMAT(BN,I11)
  694 FORMAT(BN,2F11.0)
  688 FORMAT(' List of rejected nuclides (ZA X) where X is the flag:'/
     1       '   U - nearly stable precursor'/
     2       '   Y - low yield'/
     3       '   X - small cross section'/
     4       '   R - small reaction rate (yield*cross-section)'/
     5       '   D - short decay time'/
     6       '   H - long-lived precursor of a stable nuclide'/
     7       '   S - small cross section of a stable nuclide'/
     8       '  ------------------------------------------------'
     8      ,'----------')
  689 FORMAT(5(F10.1,1X,A1))
  802 FORMAT(A40,1PE12.4)
  803 FORMAT(A40,I4)
  804 FORMAT(A40,F12.4)
  806 FORMAT(A40,I12)
  807 FORMAT(I6,1P,E11.4)
  808 FORMAT(3(1P,E15.7,I6))
  809 FORMAT(5I15)
  812 FORMAT(A66,I4,I2,I3,I5)
  814 FORMAT(6F11.0)
  874 FORMAT(F9.1,1P,E11.4,4E10.3,I2,E10.3,0P,F8.1)
  875 FORMAT(9X,A1,1P,E10.3,0P,F10.2)
  881 FORMAT( ' Log-log interpolable weight function for F.P yields'/
     1        '     Energy     Weight'/
     2        '  --------------------')
  882 FORMAT(1P,2E11.4)
  883 FORMAT(' WARNING - Parent material MAT',I6,'    ZA',I6
     1      ,' cross sections not found'/)
  884 FORMAT(' WARNING - decay level stack exceeded',I4,' for ',F12.1)
  885 FORMAT(
     3'  ----------------------------------------------------------',
     3'--------------------'/
     4'  Differences in',I4,' nuclides encountered')
  886 FORMAT(' WARNING - Decay product',F8.1,' yield',1P,E10.3
     1      , ' BR=',E10.3,' not in yield list')
  891 FORMAT(//' AVRFPY - Average fission product yields'/
     1         ' ======================================='/)
  892 FORMAT(/' Parent material MAT',I6,'    ZA',I6
     1       ,'  F.P. yields processed')
C...Print decay constant in [s-1]
  893 FORMAT(/
     1'               Yield   Sig(th)  Res.Int. Reactions Dcy.const',
     1' D     BR   Product'/
     2'    Isotope      [%]    [barn] [barn.eV] [n/s/cm3]   [s-1]  '/
     3'  ----------------------------------------------------------',
     3'--------------------')
C...Print half-life in [s]
C 893 FORMAT(/
C    1'               Yield   Sig(th)  Res.Int. Reactions Half life',
C    1' D     BR   Product'/
C    2'    Isotope      [%]    [barn] [barn.eV] [n/s/cm3]     [s]  '/
C    3'  ----------------------------------------------------------',
C    3'--------------------')
C...Print half-life in [days]
C 893 FORMAT(/
C    1'               Yield   Sig(th)  Res.Int. Reactions Half life',
C    1' D     BR   Product'/
C    2'    Isotope      [%]    [barn] [barn.eV] [n/s/cm3]   [days] '/
C    3'  ----------------------------------------------------------',
C    3'--------------------')
C...
  894 FORMAT(I4,'-',A2,'-',I3,A2,F7.5,F10.1,I10,F10.2,1P,E10.3,1X
     1      ,A1,0P,F7.4,I3,'-',A2,'-',I3,A2)
  895 FORMAT(
     3'  ----------------------------------------------------------',
     3'--------------------'/
     4'  Tot.',I5,2X,F7.3,20X,F10.2)
  896 FORMAT(' Cross sections for',I4,' nuclides processed')
  897 FORMAT(11X,F8.1,'%',4(F9.1,'%'),1X,A1)
  898 FORMAT(61X,A1,F7.4,I3,'-',A2,'-',I3,A2)
      END
      SUBROUTINE FLXPNT(NG,EGB,FRR,FIN,IER)
C-Title  : FLXPNT Subroutine
C-Purpose: Determine log-log interpolable points from integral values
      implicit real*8 (a-h,o-z)
      DIMENSION EGB(*),FRR(*),FIN(*)
      IER=0
   10 IER=IER+1
      DO 60 IG=1,NG
      FQ =FIN(IG)
      E1 =EGB(IG)
      E2 =EGB(IG+1)
      F1 =FRR(IG)
      F2 =FRR(IG+1)
      IF(FQ.EQ.0) GO TO 60
      IF(F1.NE.0) GO TO 20
      IF(F2.EQ.0) GO TO 60
      IF(IG.EQ.1) IER=0
C* Iterate on the lower point to match the average (log-log)
      F1 = F2*E2/E1
      Q  = F1*E1*DLOG(E2/E1)
      IF(Q-FQ.EQ.0) GO TO 18
      IF(Q-FQ.LT.0) GO TO 14
C*      Iteration sequence on F1 (case: B+1<0)
      F1 = 0.5*F1
   12 F  = F1
      B  =(E2*F2-E1*F )/FQ - 1.
      F1 = F2*(E1/E2)**B
      IF(ABS(F1-F).GT.1E-5*F) GO TO 12
      GO TO 18
C*      Iteration sequence on B  (case: B+1>0)
   14 F1 = 1.5*F1
      AE = DLOG(E2/E1)
   16 F  = F1
      B  = DLOG(F2/F )/AE
      F1 =(E2*F2-FQ*(B+1.))/E1
      IF(ABS(F1-F).GT.1E-5*F) GO TO 16
   18 FRR(IG)=F1
      GO TO 60
C* Iterate on the upper point to match the average (log-log)
   20 IF(F2.NE.0) GO TO 60
      IF(F1.EQ.0) GO TO 60
      IF(IG.EQ.1) IER=0
      F2 = F1*E1/E2
      Q  = F1*E1*DLOG(E2/E1)
      IF(Q-FQ.EQ.0) GO TO 28
      IF(Q-FQ.LT.0) GO TO 24
C*      Iteration sequence on F2 (case: B+1<0)
      F2 = 0.5*F2
   22 F  = F2
      B  =(E2*F -E1*F1)/FQ - 1.
      F2 = F1*(E2/E1)**B
      IF(ABS(F2-F).GT.1E-5*F) GO TO 22
      GO TO 28
C*      Iteration sequence on B  (case: B+1>0)
   24 F2 = 1.5*F2
      AE = DLOG(E2/E1)
   26 F  = F2
      B  = DLOG(F /F1)/AE
      F2 =(E1*F1+FQ*(B+1.))/E2
      IF(ABS(F2-F).GT.1E-5*F) GO TO 26
   28 FRR(IG+1)=F2
   60 CONTINUE
      IF(IER.GT.0 .AND. IER.LT.NG) GO TO 10
      RETURN
      END
      FUNCTION XSAV3G(XT,XE,XF,FIN)
C-Title  : XSAV3G Function
C-Purpose: Calculate average from 3-group cross sections
      implicit real*8 (a-h, o-z)
      DIMENSION FIN(*)
      FF=FIN(1)+FIN(2)+FIN(3)
      XSAV3G =(XT*FIN(1) + XE*FIN(2) + XF*FIN(3))/FF
      RETURN
      END
      SUBROUTINE CHKFPY(LER,NFP,ZAP,YLD,HLF,XTH,RIN,XFS,NDK,MXDK,BRR,ZAD
     1                 ,BRC,FLAG,EPY,EPX,EPR,EPD,EPU,EPC,FIN)
C-Title  : CHKFPY Subroutine
C-Purpose: Set flag for nuclides which violate the selection criteria
C-Description:
C-D  FIRST LOOP
C-D  - Check for nearly-stable nuclides and remove their contribution
C-D    from the cumulative yield of the decay product.
C-D    Nearly-stable nuclides are those for which the product of
C-D    their average cross section [barns] and half-life [days]
C-D    exceeds EPU. If the cross section is not known, it is
C-D    assumed 1 barn.
C-D  SECOND LOOP
C-D  The following conditions define nuclides eligible for printout:
C-D  1. Nuclides with yields greater than EPY
C-D  2. Nuclides with large cross sections (x-sect > EPX)
C-D  3. (Not defined)
C-D  4. Nuclides with high capture reaction rates:
C-D     - not short-lived, (x-sect*half-life > EPD)
C-D     - high yield and capture cross section
C-D       (yield*x-sect > EPR)
C-D  5. Nuclides with high cross sections capture products
C-D     - not short-lived, (x-sect*half-life > EPD)
C-D     - high yield and high capture cross sections of both
C-D       the parent and the capture product
C-D       (yield*x-sect-parent*x-sect-product > EPC)
C-D     Isomer production on capture is taken into account.
C-D  8. Capture products from item 5.
C-
      implicit real*8 (a-h,o-z)
      CHARACTER*1 FLG,    FLAG(*)
      DIMENSION   ZAP(*), YLD(*), HLF(*), XTH(*), RIN(*), XFS(*)
     1           ,BRC(*), NDK(*), BRR(MXDK,*)   , ZAD(MXDK,*), FIN(*)
C*
C* Process "nearly stable" nuclides
      DO 28 I=1,NFP
C* Preset the printout flag
      FLAG(I)='0'
      IF(EPU.LE.0) GO TO 28
C* Remove contribution from the cumulative yields of decay products
      XA =XSAV3G(XTH(I),RIN(I)/FIN(2),XFS(I),FIN)
      HL =HLF(I)/(3600. * 24.)
      IF(XA.GT.0) HL=XA*HL
      IF(HL.LE.EPU) GO TO 28
C* Loop over the decay products
      ND =MIN(MXDK, NDK(I))
      IF(ND.LE.0) GO TO 28
      DO 26 J=1,ND
      ZAM=ZAD(J,I)
      IF(ZAM.LE.0) GO TO 26
      CALL SRCIDX(NFP,ZAP,ZAM,K)
      IF(ABS(ZAP(K)-ZAM).GT.0.5) GO TO 26
      BY=BRR(J,I)*YLD(I)
      YI=YLD(K)-BY
C* Remove the decay product from the direct contribution
      IF(YI.GT.-1.E-5*YLD(I)) GO TO 22
        WRITE(LER,94) ZAP(I),YLD(I),HLF(I),ZAP(K),YLD(K)
        GO TO 26
   22 YLD(K) =MAX(0., YI)
      IF(YLD(K).LT.0.01*YLD(I)) FLAG(K)='U'
      XA =XSAV3G(XTH(K),RIN(K)/FIN(2),XFS(K),FIN)
      HK =HLF(K)/(3600. * 24.)
      IF(XA.GT.0) HK =XA*HK
C* Check if decay product itself is unstable
      IF(HK.GT.EPD) GO TO 26
      KD =MIN(MXDK, NDK(K))
C* Apply corrections on one more level of decay products
      DO 24 M=1,KD
      ZAM=ZAD(M,K)
      IF(ZAM.LE.0) GO TO 24
      CALL SRCIDX(NFP,ZAP,ZAM,L)
      IF(ABS(ZAP(L)-ZAM).GT.0.5) GO TO 24
      BY=BRR(M,K)*BY
      YI=YLD(L)-BY
C* Remove the decay product from the direct contribution
      IF(YI.GT.-1.D-5*YLD(K)) THEN
        YLD(L) =MAX(0.D0, YI)
      ELSE
        WRITE(LER,94) ZAP(K),YLD(K),HLF(K),ZAP(L),YLD(L)
      END IF
   24 CONTINUE
   26 CONTINUE
   28 CONTINUE
C*
C* Check the printout criteria
   40 DO 68 I=1,NFP
      YI =YLD(I)*100.
      XT =XTH(I)
      XE =RIN(I)/FIN(2)
      XF =XFS(I)
      XA =XSAV3G(XT,XE,XF,FIN)
      XX =MAX(XT,XE,XF,XA)
      RR =YLD(I)*XA
      HI =HLF(I)/FLOAT(3600 * 24)
      IF(XA.GT.0) HI=XA*HI
      ND =NDK(I)
      FLG=FLAG(I)
C* Check if a specific print request has already been issued
      IF(FLG.GT.'0' .AND. FLG.LE.'9') GO TO 68
C* 1.: Test on high yield
      IF(EPY.LE.0) GO TO 42
      IF( YI.GT.EPY) THEN
        FLAG(I)='1'
      ELSE
        FLG='Y'
      END IF
C* 2.: Test on high cross sections
   42 IF(EPX.LE.0) GO TO 52
      IF( XA.GE.EPX) THEN
        FLAG(I)='2'
      ELSE
        FLG='X'
      END IF
C* 4.: Test on high capture reaction rate
   52 IF(EPR.LE. 0 ) GO TO 53
C*     Exclude short-lived products
      IF(HI.GT.0 .AND. HI.LT.EPD) THEN
        FLG='D'
        GO TO 53
      END IF
      IF( RR.GE.EPR) THEN
        FLAG(I)='4'
      ELSE
        FLG='R'
      END IF
C* 5.: Test on high capture reaction rate of the capture product
   53 IF(EPC.LE.0) GO TO 65
      RL =RR
      HL =HI
      L  =I
   54 ZAM=IDINT(ZAP(L))+1
   55 CALL SRCIDX(NFP,ZAP,ZAM,K)
      IF(ABS(ZAP(K)-ZAM).GT.0.5) GO TO 65
        XC =XSAV3G(XTH(K),RIN(K)/FIN(2),XFS(K),FIN)
        RC =RL*XC
        HC =HLF(K)/(3600. * 24.)
        IF(XC.GT.0) HC=XC*HC
        IF(RC.GE.EPC .AND. (HC.LE.0 .OR. HC.GT.EPD)) THEN
          FLAG(L)='5'
          FLAG(K)='8'
C*      Isomer production on capture
          IF(BRC(L).NE.0 .AND. ZAM-ZAP(L)-1.LT.0.05) THEN
            ZAM=ZAM+0.1
            GO TO 55
          END IF
          RL =YLD(I)*XC
          HL =HC
          L  =K
          GO TO 54
        END IF
   65 IF(FLAG(I).LE.'0' .OR. FLAG(I).GT.'9') FLAG(I)=FLG
   68 CONTINUE
C* All criteria checked
   90 RETURN
   94 FORMAT(' WARNING - Inconsistent yields for ZA',0P,F8.1
     1      ,' yield',1P,E10.3,' Th[s]',E10.3/
     2       '                 and decay product ZA',0P,F8.1
     2      ,' yield',1P,E10.3)
      END
      SUBROUTINE YLDCUM(LER,NFP,EPU,ZAP,YLD,HLF,XTH,RIN,XFS,MDD
     1                 ,MXDK,BRR,ZAD,YL1,YL2,FIN)
C-Title  : YLDCUM Subroutine
C-Purpose: Calculate cumulative F.P yields from independent yields
      implicit real*8 (a-h,o-z)
      DIMENSION  MDD(*), ZAP(*), YLD(*), HLF(*), XTH(*), RIN(*), XFS(*)
     1          ,YL1(*), YL2(*), BRR(MXDK,*),    ZAD(MXDK,*),    FIN(*)
C* Clear the scratch yields arrays
      DO I=1,NFP
        YL1(I)=0
        YL2(I)=0
      END DO
C* Process direct decay products
      DO 40 I=1,NFP
      ND = MIN(MXDK,MDD(I))
      YI = YLD(I)
      XA =XSAV3G(XTH(I),RIN(I)/FIN(2),XFS(I),FIN)
      HL = HLF(I)/FLOAT(3600 * 24)
      IF(XA.GT.0) HL=XA*HL
      IF(HL.LE.0 .OR. (HL.GT.EPU .AND. EPU.GT.0)) GO TO 40
      IF(ND.LT.1) GO TO 40
      DO 24 J=1,ND
      BR = BRR(J,I)
      ZAM= ZAD(J,I)
      IF(ZAM.LE.0) GO TO 24
      CALL SRCIDX(NFP,ZAP,ZAM,K)
      IF(ABS(ZAP(K)-ZAM).GT.0.5) GO TO 24
      YL1(K)=YL1(K)+BR*YI
   24 CONTINUE
   40 CONTINUE
C* Process secondary decay products
   50 ID =  0
      DO 60 I=1,NFP
      YI = YL1(I)
      IF(YI.LE.0) GO TO 60
      ND = MIN(MXDK,MDD(I))
      XA =XSAV3G(XTH(I),RIN(I)/FIN(2),XFS(I),FIN)
      HL = HLF(I)/FLOAT(3600 * 24)
      IF(XA.GT.0) HL=XA*HL
      YL1(I)=0.
      YLD(I)=YLD(I)+YI
      IF(ND.LE.0) GO TO 60
      IF(HL.LE.0 .OR. (HL.GE.EPU .AND. EPU.GT.0)) GO TO 60
C* Case: Calculate yields of secondary decay products
      DO 56 J=1,ND
      BR = BRR(J,I)
      ZAM= ZAD(J,I)
      IF(ZAM.LE.0) GO TO 56
      CALL SRCIDX(NFP,ZAP,ZAM,K)
      IF(ABS(ZAP(K)-ZAM).GT.0.5) GO TO 56
C*       Guard against erroneous data
      IF(K.EQ.I) THEN
        WRITE(LER,94) ZAM
        GO TO 56
      END IF
      YL2(K)=YL2(K)+BR*YI
      ID =ID+1
   56 CONTINUE
   60 CONTINUE
C* Prepare for the next generation of decay products
      DO 62 I=1,NFP
      YL1(I)=YL2(I)
      YL2(I)=0.
   62 CONTINUE
C* Repeat the procedure if more daughter product generations exist
      WRITE(LER,96) ID
      IF(ID.NE.0) GO TO 50
      RETURN
   94 FORMAT(' WARNING - Erroneous decay data for ZA',F8.1)
   96 FORMAT(' Number of secondary daughter-products',I5)
      END
      SUBROUTINE CHKCUM(LER,NFP,EPU,ZAP,YLD,HLF,XTH,RIN,XFS,MDD
     1                 ,MXDK,BRR,ZAD,DKM,FIN)
C-Title  : CHKCUM Subroutine
C-Purpose: Check the consistency of the cumulative F.P yields
      implicit real*8 (a-h,o-z)
      PARAMETER  (MXP=10)
      CHARACTER*1 RC(10)  ,RCK(MXP),DKM(MXDK,*)
      DIMENSION   ZAK(MXP),YLK(MXP),BRK(MXP),HLK(MXP)
      DIMENSION   MDD(*), ZAP(*), YLD(*), HLF(*), XTH(*), RIN(*), XFS(*)
     1           ,BRR(MXDK,*), ZAD(MXDK,*), FIN(*)
      DATA RC/'g','b','+','I','a','n','F','p','8','x'/
C* Process each fission product
      DO 60 I=1,NFP
      ZAM= ZAP(I)
      YI = YLD(I)
      YK = 0.
      NP = 0
      DO 40 K=1,NFP
      ND = MIN(MXDK,MDD(K))
      IF(ND.LE.0) GO TO 40
      DO 24 J=1,ND
      ZAJ= ZAD(J,K)
      IF(ZAJ.LE.0) GO TO 24
      IF(ABS(ZAJ-ZAM).GT.0.05) GO TO 24
        XA =XSAV3G(XTH(K),RIN(K)/FIN(2),XFS(K),FIN)
        HL = HLF(K)/(3600. * 24.)
        IF(XA.GT.0) HL=XA*HL
        IF(HL.LE.0 .OR. (HL.GT.EPU .AND. EPU.GT.0)) GO TO 24
        YK = YK + YLD(K)*BRR(J,K)
        IF(NP.GE.MXP) GO TO 24
        NP=NP+1
        ZAK(NP)=ZAP(K)
        YLK(NP)=YLD(K)
        HLK(NP)=HLF(K)
        BRK(NP)=BRR(J,K)
        RCK(NP)=DKM(J,K)
   24 CONTINUE
   40 CONTINUE
      IF(YK.LE.YI) GO TO 60
      ER=999.99
      IF(YI.GT. 0) ER=MIN(ER, 100.*(YK/YI-1.))
      IF(ER.LT.0.01) GO TO 60
      WRITE(LER,94) ZAM,YI,ER
      IF(NP.LT. 1) GO TO 60
      DO 52 J=1,NP
      WRITE(LER,96) ZAK(J),YLK(J),HLK(J),RCK(J),BRK(J)
   52 CONTINUE
   60 CONTINUE
      RETURN
   94 FORMAT(' WARNING - ZA',F8.1,' yield',1P,E10.3
     1      ,' cumulative yield inconsistent',0P,F6.2,'%')
   96 FORMAT('    precursor',F8.1,' yield',1P,E10.3
     1      ,' T(1/2)',E10.3,' React. ',A1,' BR',0P,F8.5)
      END
      SUBROUTINE DECYMD(LDY,LER,NFP,ZAP,HLF,NDK,LDK,DKM,BRR,ZAD,IER)
C-Title  : DECYMD Subroutine
C-Purpose: Assign the decay data to the list of F.P. nuclides
      implicit real*8 (a-h,o-z)
      PARAMETER   (MXDK=10)
      CHARACTER*1  RC(MXDK), DKM(LDK,*)
      DIMENSION    BR(MXDK),MD(MXDK),IZAP(MXDK),IZOP(MXDK)
      DIMENSION    BRR(LDK,*),ZAD(LDK,*), ZAP(*),HLF(*),NDK(*)
      DATA RC/'g','b','+','I','a','n','F','p','8','x'/
   10 MAT=0
      IZA=-1
      LIZ=-1
      IZO=-1
      CALL IZADCY(LDY,MAT,IZA,LIZ,IZO,HL,ND,BR,MD,IZAP,IZOP,MXDK,IEF)
      ZAM =FLOAT(IZA)+0.1*FLOAT(IZO)
      IF(IEF.GE.10) GO TO 90
C* Find the index of the current nuclide on the list
      CALL SRCIDX(NFP,ZAP,ZAM,K)
      IF(ABS(ZAP(K)-ZAM).GT.0.05) GO TO 10
C* Transfer the data to the output array
      NDK(K)=ND
      HLF(K)=HL
      ND=MIN(LDK, ND)
      DO I=1,ND
        JR=MIN(MXDK,MD(I)+1)
        DKM(I,K)=RC(JR)
        BRR(I,K)=BR(I)
        ZAD(I,K)=FLOAT(IZAP(I))+0.1*FLOAT(IZOP(I))
      END DO
      IF(IEF.LT.10) GO TO 10
C* All file processed
   90 IER=0
      IF(IEF.GE.20) IER=-1
      RETURN
      END
      SUBROUTINE IZADCY(LIN,MAT0,IZA0,LIZ0,IZO0
     1                 ,HLF,NDK,BRR,MDD,IZAP,IZOP,MXDK,IER)
C-Title  : IZADCY Subroutine
C-Purpose: Assign decay modes to the selected nuclide
      implicit real*8 (a-h,o-z)
      CHARACTER*66   REC
      DIMENSION      BRR(*), MDD(*), IZAP(*), IZOP(*)
   20 READ (LIN,622,END=92,ERR=93) REC,MAT,MF,MT
      IF(MAT.LT.0) GO TO 91
C* Find the decay data
      IF(MF.NE.8 .OR. MT.NE.457) GO TO 20
      READ (REC,623) ZA,AWR,LIZ,IZO
      IZA = ZA+0.1
C* Skip if not the right material
      IF(MAT0.LT.0) GO TO 40
      IF(IZA0.LT.0) GO TO 40
      IF(MAT0.GT.0 .AND. MAT0.EQ.MAT) GO TO 40
      IF(IZA0.GT.0 .AND. IZA0.EQ.IZA) GO TO 40
      IF(LIZ0.GE.0 .AND. LIZ0.EQ.LIZ) GO TO 40
      IF(IZO0.GE.0 .AND. IZO0.EQ.IZO) GO TO 40
   22 READ (LIN,622,END=92,ERR=93) REC,MAT,MF,MT
      IF(MAT.NE.0) GO TO 22
      GO TO 20
C* Process the decay data
   40 READ (LIN,623) HLF, DHLF, I, I, NC2, I
      READ (LIN,624) (xx,i=1,nc2)
      READ (LIN,623) SPI,PAR, I, I, I, NDK
      JDK=NDK
      IF(NDK.LE.MXDK) GO TO 41
      IER=9
      JDK=MXDK
   41 DO 42 I=1,JDK
      READ (LIN,624) RTY,RFS,QQ,DQ,BRR(I)
      IZOP(I)=NINT(RFS)
      IZAP(I)=IZA
C* Process decay type flag - allow up to 4 digits
      ITYI=NINT(RTY*1000)
      ITYO=1
      DO WHILE (ITYI.GT.0)
C*      Consider last digit of the decay type flag
        ITY =ITYI-10*(ITYI/10)
        ITYI=ITYI/10
        IF(ITY.GT.0) THEN
          ITYO=ITYO*10
          IF(ITY.EQ.1) IZAP(I)=IZAP(I)+1000
          IF(ITY.EQ.2) IZAP(I)=IZAP(I)-1000
          IF(ITY.EQ.3) IZAP(I)=IZAP(I)
          IF(ITY.EQ.4) IZAP(I)=IZAP(I)-2004
          IF(ITY.EQ.5) IZAP(I)=IZAP(I)-   1
          IF(ITY.EQ.7) IZAP(I)=IZAP(I)-1001
        END IF
      END DO
      ITYO=NINT(RTY*ITYO/10)
      MDD(I)=ITYO
      IF(ITYO.EQ.0) IZAP(I)=0
   42 CONTINUE
C* Skip the rest of the data for this material
   44 READ (LIN,622,END=92,ERR=93) REC,MAT,MF,MT
      IF(MAT.NE.0) GO TO 44
C* Reassign the nuclide identifiers and finish
      MAT0=MAT
      IZA0=IZA
      LIZ0=LIZ
      IZO0=IZO
      RETURN
C* Last material processed
   91 IER=10
      RETURN
C* EOF error trap
   92 IER=12
      RETURN
C* Format read error trap
   93 IER=20
      RETURN
  622 FORMAT(A66,I4,I2,I3)
  623 FORMAT(2F11.0,4I11)
  624 FORMAT(6F11.0)
      END
      SUBROUTINE REACRT(LXS,LER,MTX,NFP,ZAP,XTH,RIN,XFS ,IER)
C-Title  : REACRT Subroutine
C-Purpose: Assign cross sections to a list of nuclides
C-Version:
C-V  01/11 A.Trkov: Upgrade to support INTER 6.12 output
C-Description:
C-D  A record is read from a file, assuming it is in the INTER output
C-D  format. Version of INTER is identified from the first record.
C-D  A modification of the 6.11 format is allowed where an extra
C-D  digit is read for ZA to identify the isomer state. Records
C-D  which can not be read or where MAT number is zero are ignored
C-
      implicit real*8 (a-h,o-z)
      CHARACTER*120 REC
      CHARACTER*11  RCT
      CHARACTER*2   LISO
      DIMENSION     ZAP(*),XTH(*),RIN(*),XFS(*)
C*
      DATA RPI2/ 0.886226925D0 /
      NM  =0
      IZA0=0
      IVER=612
C* Try to identify the INTER version
      READ (LXS,860,END=90) REC
      IF(REC(39:51).EQ.'INTER VERSION') THEN
        READ (REC(52:56),864,ERR=42) VER
        IVER=NINT(VER*100)
        WRITE(*,*) "Detected INTER version", IVER
        WRITE(LER,*) "Detected INTER version", IVER
C* Position the file to the beginning of the data
   30   READ (LXS,860,END=90) REC
        IF((IVER.GE.612) .AND. (IVER.LE.800)) THEN
          IF(REC(1:16).NE.' Material number') GO TO 30
          READ (REC(26:31),865) MAT
        ELSE IF(IVER.GT.800) THEN
          IF(REC(1:23).NE.'   Z   A LISO  LFS  MT ') GO TO 30
C* Read the line with dashes
          READ (LXS,860,END=90) REC
        ELSE
          IF(REC(1:16).NE.'  MAT    ZA    M') GO TO 30
        END IF
      ELSE
C* Header record missing - assume INTER Version 6.12 or higher
        GO TO 42
      END IF
C* Process each entry in turn on the INTER output file
   40 READ (LXS,860,END=90) REC
   42 IF(REC(1:20).EQ.'                    ') GO TO 40
      IF((IVER.GE.612) .AND. (IVER.LE.800)) THEN
        IF(REC(1:16).EQ.'  Z    A  LISO  ') GO TO 40
        IF(REC(1:16).EQ.' Material number') THEN
          READ (REC(26:31),865) MAT
          GO TO 40
        END IF
        READ (REC,861) IZ,IA,LIS0,LFS,MT,RCT,SG0,SE0,STH,GWE,RI,SFS,S14
        IZA=IZ*10000+IA*10+LIS0
        ZA =IZA*0.1
        ZAM=ZA
        STH=STH*RPI2
C* Assume format INTER 8.09 or newer - read all records that conform to
      ELSE IF(IVER.GT.800) THEN
        READ(REC,866) IZ,IA,LISO,LFS,MT,RCT,SG0,
     &                SE0,STH,GWE,RI,SFS,S14,MAT
C*        WRITE(LER,866) IZ,IA,LISO,LFS,MT,RCT,SG0,
C*     &                 SE0,STH,GWE,RI,SFS,S14,MAT
C* Ignore LIS0 for now, since it is a string in the format
        ILIS0=0
        IF (LISO.EQ." m") THEN
            ILISO=1
        ELSE IF (LISO.EQ." n") THEN
            ILISO=2
        ELSE
            ILISO=0
        END IF
        IZA=IZ*10000+IA*10 + ILISO
        ZA =IZA*0.1
        ZAM=ZA
        STH=STH*RPI2
      ELSE
C* Assume format INTER 6.11 - read all records that conform to format
        IF(REC(13:13).EQ.' ') THEN
C*        INTER 6.11 regular format (ZA)
          READ (REC,862,ERR=40) MAT,ZA,MT,RCT,SG0,SE0,STH,GWE,RI,SFS,S14
        ELSE
C*        INTER 6.11 MODIFIED format (ZA.m with decimal isomer state m)
          READ (REC,863,ERR=40) MAT,ZA,MT,RCT,SG0,SE0,STH,GWE,RI,SFS,S14
        END IF
        IF(MAT.LE. 0 ) GO TO 40
        IZA=NINT(ZA*10)
      END IF
C* Test for the specified MT number
      IF( MT.NE.MTX) GO TO 40
C* If same ZA consecutively for this MT, assume it is an isomer
      IF(IVER.LT.612 .AND. IZA.EQ.IZA0) THEN
        ZAM =ZAM+0.1
      ELSE
        ZAM =ZA
        IZA0=NINT(ZA*10)
      END IF
C* Find the index of the current nuclide on the list
      CALL SRCIDX(NFP,ZAP,ZAM,K)
C* Isomer data are approximated by the ground-state data
      IF(ABS(ZAP(K)-ZAM).GT.0.5) GO TO 40
C* Enter the thermal x-sect. and res.int. into the output array
      XTH(K)=STH
      RIN(K)=RI
      XFS(K)=SFS
      NM =NM+1
      GO TO 40
C* End-of-file check
   90 IER=0
      IF(NM.NE.NFP) IER=NM
      IF(NM.LE.0)   IER=-1
      RETURN
  860 FORMAT(A120)
  861 FORMAT(I4,I5,I4,2I5,1X,A11,2F12.0,F11.0,F8.0,F13.0,2F12.0)
  862 FORMAT(I5,F7.0,I4,A11,2E12.5,E11.4,F8.5,E13.5,2E12.5)
  863 FORMAT(I5,F8.1,I4,A11,2E12.5,E11.4,F8.5,E13.5,2E12.5)
  864 FORMAT(F5.0)
  865 FORMAT(I5.0)
  866 FORMAT(I4,I4,A2,6X,A2,I4,2X,A8,2X,2(1PE12.5),1PE11.4,
     &       0PF8.5,1PE13.5,2(1PE12.5),I5)
      END
      SUBROUTINE SRCIDX(N,X,R,K)
C-Title  : SRCIDX
C-Purpose: Find index K in array X that most closely matches R
      implicit real*8 (a-h,o-z)
      DIMENSION X(*)
      K=1
      IF(N.LT.2) RETURN
      E=X(1)
      DO 40 I=2,N
      IF(ABS(X(I)-R).GT.E) GO TO 40
      K=I
      E=ABS(X(I)-R)
   40 CONTINUE
      RETURN
      END
      SUBROUTINE FPRYLD(LIN,LER,MAT,MT,ZA,NFP,ZAP,YLD,FPS
     1          ,ZAP1,YLD1,ZAP2,YLD2 ,EGB,FRR ,MXFP,IER)
C-Title  : FPRYLD Subroutine
C-Purpose: Process fission products yields ENDF file
      implicit real*8 (a-h, o-z)
      CHARACTER*66 REC
      DIMENSION    EGB(4)    , FRR(4)
     1            ,ZAP (MXFP), YLD (MXFP), FPS(MXFP)
     2            ,ZAP1(MXFP), YLD1(MXFP)
     3            ,ZAP2(MXFP), YLD2(MXFP)
      IER=0
C* Process the ENDF file
   20 READ (LIN,695,END=90) REC,MAT0,MF0,MT0
C*      Check the MAT,MF,MT numbers
      IF(MAT0.LT.0) GO TO 90
      IF(MAT0.NE.MAT .AND. MAT.GT. 0 ) GO TO 20
      IF( MF0.NE.8) GO TO 20
      IF( MT0.NE.454  .AND.  MT0.NE.459) GO TO 20
      IF( MT0.NE.MT   .AND.  MT .GT.  0) GO TO 20
      IF( ZA.EQ.0) GO TO 32
C*      Check the ZA number
      READ (REC,696) ZA0,AWR,NE
      IF(ABS(ZA0-ZA).LT.0.5) GO TO 32
C*      In case if an incorrect ZA number skip to the end of material
   22 READ (LIN,695,END=90) REC,MAT0,MF0,MT0
      IF(MAT0.NE.0) GO TO 22
      GO TO 20
C* Fission product yields data found
   32 MAT=MAT0
      MT =MT0
      READ (REC,696) ZA,AWR,NE
      READ (LIN,696) EN,DD ,INT,II,II,NFP
      IF(NFP.GT.MXFP) STOP 'AVRFPY ERROR - Array capacity exceeded'
C* Read the data at the first energy point
      READ (LIN,698) (ZAP (I),FPS(I) ,YLD (I),DD,I=1,NFP)
      DO 41 I=1,NFP
      ZAP(I)=ZAP(I)+0.1*FPS(I)
      IF(I.LE.1) GO TO 41
      IF(ZAP(I-1).GE.ZAP(I)) WRITE(LER,803) ZAP(I-1),ZAP(I)
   41 CONTINUE
      CALL SRTBB2(NFP,ZAP,YLD,IES)
      IF(IES.GT.0) WRITE(LER,802) IES,EN
      SF=1.
      IF(NE.LE.1) GO TO 80
C* Save the data if more energy points present
      DO 42 I=1,NFP
      ZAP2(I)=ZAP(I)
      YLD2(I)=YLD(I)
      YLD (I)=0
   42 CONTINUE
      NFP2=NFP
      SF=0
      E2=EN
      JE=1
C* Read the data at the next energy point
   50 IF(JE.GE.NE) GO TO 80
      JE=JE+1
      E1=E2
      NFP1=NFP2
      DO 54 I=1,NFP1
      ZAP1(I)=ZAP2(I)
      YLD1(I)=YLD2(I)
   54 CONTINUE
      READ (LIN,696) E2,DD ,INT,II,II,NFP2
      IF(NFP2.GT.MXFP) STOP 'AVRFPY ERROR - Array capacity exceeded'
      READ (LIN,698) (ZAP2(I),FPS(I),YLD2(I),DD,I=1,NFP2)
      DO 56 I=1,NFP2
      ZAP2(I)=ZAP2(I)+0.1*FPS(I)
      IF(I.LE.1) GO TO 56
      IF(ZAP2(I-1).GE.ZAP2(I)) WRITE(LER,803) ZAP2(I-1),ZAP2(I)
   56 CONTINUE
      CALL SRTBB2(NFP2,ZAP2,YLD2,IES)
      IF(IES.GT.0) WRITE(LER,802) IES,E2
C*
C* Integral of the assumed spectrum (assume log-log interpolable)
C*   SG - spectrum contribution in the interval E1 - E2
C*   SF - total spectrum
      SG=0.
      DO 58 IG=1,3
      EA=MAX(E1, EGB(IG))
      EB=MIN(E2, EGB(IG+1))
      IF(EB.LE.EA) GO TO 58
      B =DLOG(FRR(IG+1)/FRR(IG))/DLOG(EGB(IG+1)/EGB(IG))
      B1=B+1.
      IF(ABS(B1).LT.1.E-4) THEN
        SG=SG+FRR(IG)*EA*DLOG(EB/EA)
      ELSE
        SG=SG+( EB*(EB/EGB(IG))**B - EA*(EA/EGB(IG))**B )*FRR(IG)/B1
      END IF
   58 CONTINUE
      SF=SF+SG
      I =1
      J =1
      K =1
C* Add the contribution to the average
   60 IF(K.GT.NFP) GO TO 50
      Y1=0.
      Y2=0.
      L =J
      IF(J.GT.NFP2) GO TO 62
      IF(ZAP2(L).GT.ZAP1(I)) GO TO 61
      ZP=ZAP2(J)
      Y2=YLD2(J)
      J =J+1
      GO TO 62
C* Print warning when yield at E2 for the current isotope is missing
   61 IF(LER.GT. 0) WRITE(LER,801) 2,IDINT(ZAP1(I)+0.1),E2
   62 IF(I.GT.NFP1) GO TO 64
      IF(ZAP1(I).GT.ZAP2(L)) GO TO 63
      ZP=ZAP1(I)
      Y1=YLD1(I)
      I =I+1
      GO TO 64
C* Print warning when yield at E1 for the current isotope is missing
   63 IF(LER.GT. 0) WRITE(LER,801) 1,IDINT(ZAP2(L)+0.1),E1
      ZP=ZAP2(L)
C* Weighted integral of the yield over the current energy interval
   64 Y =0.
      DO 68 IG=1,3
      EA=MAX(E1, EGB(IG  ))
      EB=MIN(E2, EGB(IG+1))
      IF(EB.LE.EA) GO TO 68
      IF(FRR(IG)*FRR(IG+1).LE.0) GO TO 68
C* Fission yields assumed log-log interpolable
C     B =DLOG(FRR(IG+1)/FRR(IG))/DLOG(EGB(IG+1)/EGB(IG))
C     B1=B+1.
C     B2=B+2.
C     A =FRR(IG)/EGB(IG)**B
C     G =(Y2-Y1)/(E2-E1)
C     C =Y1-E1*G
C     IF(ABS(B1).LT.1.E-4) THEN
C       Y = Y + C*A*DLOG(EB/EA)
C     ELSE IF(ABS(B2).LT.1.E-4) THEN
C       Y = Y + G*A*DLOG(EB/EA) - C*A*(1./EB - 1./EA)
C     ELSE
C       Y =Y + A*( (EB*G/(B+2.)+C/(B+1.))*EB**(B+1.)
C    1            -(EA*G/(B+2.)+C/(B+1.))*EA**(B+1.) )
C     END IF
C* Integral of the weighting function over the energy interval
      B =DLOG(FRR(IG+1)/FRR(IG))/DLOG(EGB(IG+1)/EGB(IG))
      B1=B+1.
      IF(ABS(B1).LT.1.E-4) THEN
        SG=FRR(IG)*EA*DLOG(EB/EA)
      ELSE
        SG=( EB*(EB/EGB(IG))**B - EA*(EA/EGB(IG))**B )*FRR(IG)/B1
      END IF
C* Fission yields assumed in histogram form
        Y =Y +Y1*SG
   68 CONTINUE
C* Check the isotope list in the master array
   70 IF(ZAP(K).EQ.ZP) GO TO 76
      IF(ZAP(K).LT.ZP) GO TO 77
C* Insert an entry in the list
      IF(NFP.GE.MXFP) STOP 'AVRFPY ERROR - Array capacity exceeded'
      DO 75 L=K,NFP
      ZAP(NFP+K-L+1)=ZAP(NFP+K-L)
      YLD(NFP+K-L+1)=YLD(NFP+K-L)
   75 CONTINUE
      ZAP(K)=ZP
      YLD(K)=0.
      NFP=NFP+1
C* Cae: Add the contribution for the current isotope
   76 YLD(K)=YLD(K) + Y
      K  =K+1
      GO TO 60
C* Case: No data for the current isotope in this interval
   77 K  =K+1
      IF(K.LE.NFP) GO TO 70
      NFP=K
      IF(NFP.GE.MXFP) STOP 'AVRFPY ERROR - Array capacity exceeded'
      ZAP(NFP)=ZP
      YLD(NFP)=Y
      IF(I.LE.NFP1 .OR. J.LE.NFP2) GO TO 60
C* Normalize by the integral of the spectrum and finish
   80 DO 81 I=1,NFP
      YLD(I)=YLD(I)/SF
   81 CONTINUE
      RETURN
C* End-of-file encountered
   90 IER=1
      RETURN
  695 FORMAT(A66,I4,I2,I3)
  696 FORMAT(2F11.0,4I11)
  698 FORMAT(6F11.0)
  801 FORMAT(' WARNING',I2,'  ZA',I6,' at',1P,E9.2,' eV missing.')
  802 FORMAT(' WARNING -',I4,' swaps at',1P,E9.2,' eV were necessary.')
  803 FORMAT(' WARNING - material',F8.1,' appears before',F8.1
     1      ,' in the F.P. yields data')
      END
      SUBROUTINE SRTBB2(N,X,Y,IER)
C-Title  : SRTBB2 Subroutine
C-Purpose: Bubble sort of arrays X,Y in ascending order of X
C-Description:
C-D  Element pairs of arrays X(i) and Y(i) of length N are sorted
C-D  in ascending order of X by the bubble sort method. On exit, IER
C-D  is the number of swap operations performed.
      implicit real*8 (a-h,o-z)
      DIMENSION X(*),Y(*)
      IER=0
      IF(N.LT.2) RETURN
      DO 40 K=2,N
      I=K-1
      DO 20 J=K,N
      IF(X(I).LE.X(J)) GO TO 20
      XI  =X(I)
      YI  =Y(I)
      X(I)=X(J)
      Y(I)=Y(J)
      X(J)=XI
      Y(J)=YI
      IER =IER+1
   20 CONTINUE
   40 CONTINUE
      RETURN
      END
      SUBROUTINE CAPBRR(NFP,ZAP,BRC)
C-Title  : CAPBRR Subroutine
C-Purpose: Assign capture product isomer branching ratio
C-Description:
C-D  The branching ratios for isomer production on capture should be
C-D  obtained from ENDF File-9 data but the relevant information is
C-D  usually lacking. This is an ad-hoc routine which defines the
C-D  branching ratios for some known cases.
      implicit real*8 (a-h,o-z)
      DIMENSION ZAP(*),BRC(*)
      DO 20 I=1,NFP
      BRC(I)=0
C* 61-Pm-147 giving 43% 61-Pm-148m and 57% 61-Pm-148
      IF(ABS(ZAP(I)-61147.D0).LT.0.05D0) BRC(I)=0.47D0
   20 CONTINUE
      RETURN
      END
