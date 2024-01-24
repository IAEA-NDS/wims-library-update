      program wimsie
c-title  : program wimsie
c-purpose: prepare a wimsd dosimetry file
c-author : d. l. aldama
c-version: 08/2001 original code
c-m
c-m  manual for program wimsie
c-m  =========================
c-m  the required data is read as input and a WIMSD formatted
c-m  dosimetry file is written.
c-m  especial dosimetry material options are also available.
c-m  (see below)
c-m
c-m  input data:
c-m  the following parameters should be entered from keyboard:
c-m   1. (ng, ng1, ng2) : library energy structure, where
c-m                       ng:  number of energy groups
c-m                       ng1: number of fast groups
c-m                       ng2: number of resonance groups
c-m
c-m   2. IDWIMS : wims identification number
c-m
c-m   3. ZA0: ZA Trigger
c-m       if ZA0= +ZA number general dosimetry material assumed
c-m
c-m       if ZA0 < 0  special options are available:
c-m
c-m        if ZA0= -1  the resonance part of the input WIMSD formatted
c-m                    cross section file is extracted.
c-m                    fast and thermal cross section are set to zero.
c-m        if Za0= -2  change the sign of the input WIMSD
c-m                    formatted dosimetry file.
c-m        if Za0= -3  a dosimetry file is created containing
c-m                    the inverse lethargy intervals.
c-m                    (energy boundaries should be supplied
c-m                    on the input data file)
c-m        if Za0= -4  a dosimetry file is created for
c-m                    a constant = 1.0 absorber.
c-m
c-m   4. output WIMSD fomatted dosimetry file name (*.xsw)
c-m
c-m   5. log (listing) output file name (*.lst)
c-m
c-m   6. input data file (only if ZA0 > -4)
c-m      if ZA0 > 0 then the file is an endfb6 histogram formatted file
c-m      if ZA0 =-1 or -2: WIMSD formatted dosimetry cross section file
c-m      if ZA0=-3 : WILLIE formatted energy boundary data file
c-m      if ZA0=-4 : NOT REQUIRED !!!
c-m
c-m   7. mfrr,mtrr: file and section numbers of the dosimetry reaction
c-m                 (only if ZA0=+ZA number)
c-m
c-m  files used:
c-m       input file:
c-m          1. input data file (unit lib=1)
c-m              a) endfb6 histogram formatted file (if ZA0 = +ZA)
c-m              b) WIMSD formatted dosimetry cross section
c-m                 file (if ZA0=-1 or -2)
c-m              c) WILLIE formatted energy boundary data file
c-m                 (if ZA0=-3)
c-m       output files:
c-m          1.  WIMSD formatted dosimetry file
c-m          2.  listing file
c-m
c-m    maximum number of energy groups: 200  ==> ngg=201
c-m
c-
       parameter(lib=1,nout=2,nlog=3,lkb=5,ltt=6,nng=201)
       dimension e(nng),xs(nng)
       character*66 line,tape
       character*40 fli,fou,flo,flnm,blnk
       character*11 zasym
       data blnk/'                                        '/
       data fli/'wimsie.dat'/
       data fou/'wimsie.xsw'/
       data flo/'wimsie.lst'/
       data zasym/'           '/,temp/300.0/,aw/.9914095/
       write(ltt,15) ' wimsie: prepare wimsd dosimetry file   '
       write(ltt,15) '         from groupie endfb6 output     '
       write(ltt,15) ' ======================================='
       write(ltt,15)
c
c      read energy structure and wims id
c
       write(ltt,15) '   Enter energy structure (NG,NG1,NG2): '
       write(ltt,15) '   -----------------------------------  '
       write(ltt,15) '$             enter=default (69 14 13): '
       read(lkb,15)flnm
       if(flnm.eq.blnk) then
         ngrp=69
         nfast=14
         nreso=13
       else
         read(flnm,*)ngrp,nfast,nreso
       endif
       write(ltt,15) '                         enter wims id: '
       write(ltt,15) '                         -------------  '
       write(ltt,15) '$                enter=default (9999) : '
       read(lkb,15)flnm
       if(flnm.eq.blnk) then
        idwims=9999
       else
        read(flnm,*)idwims
       endif
c
c      read ZA0 trigger
c
       write(ltt,15) '                     enter ZA0 trigger: '
       write(ltt,15) '                     -----------------  '
       write(ltt,15) '    general dosimetry material: za0=ZA  '
       write(ltt,15) '    resonance part of input xs: za0=-1  '
       write(ltt,15) '                  xs(g)=-xs(g): za0=-2  '
       write(ltt,15) '    inverse lethargy intervals: za0=-3  '
       write(ltt,15) '$                 constant=1.0: za0=-4  '
       read(lkb,15)flnm
       if(flnm.eq.blnk) then
         za0=-4.0
       else
        read(flnm,*)za0
       endif
c
c    open i/o files
c
       flnm=blnk
    2  write(ltt,15) 'enter output dosimetry file name(.xsw): '
       write(ltt,15) '--------------------------------------  '
       write(ltt,15) '                     default file name: ',fou
       write(ltt,15) '$       enter new name (enter=default): '
       read (lkb,15) flnm
       if(flnm.ne.blnk) fou=flnm
       open(unit=nout,file=fou,err=2)
       flnm=blnk
    3  write(ltt,15) ' enter output listing file name (.lst): '
       write(ltt,15) ' -------------------------------------  '
       write(ltt,15) '                     default file name: ',flo
       write(ltt,15) '$       enter new name (enter=default): '
       read (lkb,15) flnm
       if(flnm.ne.blnk) flo=flnm
       open(unit=nlog,file=flo,err=3)
       if (za0.ne.-4) then
         flnm=blnk
    4    write(ltt,15) '            enter input data file name: '
         write(ltt,15) '            --------------------------  '
         write(ltt,15) '                     default file name: ',fli
         write(ltt,15) '$       enter new name (enter=default): '
         read (lkb,15) flnm
         if(flnm.ne.blnk) fli=flnm
         open(unit=lib, file=fli, status='old',err=4)
       else
        fli=blnk
       endif
       write(nlog,*)' WIMSIE listing file'
       write(nlog,*)' Input data file: ',fli
       write(nlog,*)' Output WIMSD formatted dosimetry file: ',fou
       write(nlog,*)idwims,ngrp,nfast,nreso
       write(nlog,*)za0
       if (za0.lt.0.0) then
c
c        special dosimetry materials
c
c        za0= -1   resonant part of the input dosimetry xs
c        za0= -2   xs(i) = -xs(i)
c        za0= -3   inverse lethargy intervals
c        za0= -4   constant = 1.0
c
         nza0=INT(abs(za0)+0.01)
         za0=0.0
         if(nza0.le.3) then
           call readxs(lib,xs,ngr1,ngf1,ngres1,temp,n3)
           if (n3.ne.4.and.n3.ne.1) then
             write(nlog,*)' Error reading input data file. n3= ',n3
             write(ltt,*)' Error reading input data file. n3= ',n3
             stop
           endif
           if (ngr1.ne.ngrp) then
             write(nlog,*)' Incompatible energy structure.',ngr1,ngrp
             write(ltt,*)' Incompatible energy structure.',ngr1,ngrp
             stop
           endif
           if(nza0.eq.1) then
c
c            resonant part of the input dosimetry xs
c
             if (nfast.ne.ngf1) then
              write(nlog,*)' Incompatible fast energy range.',nfast,ngf1
              write(ltt,*)' Incompatible fast energy range.',nfast,ngf1
              stop
             endif
             if (nreso.ne.ngres1) then
              write(nlog,*)' Incompatible resonance range.',nreso,ngres1
              write(ltt,*)' Incompatible resonance range.',nreso,ngres1
              stop
             endif
             do i=1,nfast
               xs(i)=0.0
             enddo
             nepi1=nfast+nreso+1
             do i=nepi1,ngrp
               xs(i)=0.0
             enddo
           else if (nza0.eq.2) then
c
c          xs(i)=-xs(i)
c
             do i=1,ngrp
               xs(i)=-xs(i)
             enddo
           else
c
c          inverse lethargy intervals
c
             do i=1,ngrp
               xs(i)=1.0/log(xs(i)/xs(i+1))
             enddo
           endif
         else
c
c        constant = 1.0
c
           do i=1,ngrp
             xs(i)=1.0
           enddo
         endif
         call wimsxs(nout,za0,aw,xs,ngrp,nfast,nreso,idwims,temp)
         write(nlog,*)za0,aw,temp
         write(nlog,*)' Cross section = '
         write(nlog,'(1x,1p5e15.8)')(xs(i),i=1,ngrp)
       else
c
c        general dosimetry material
c
         write(ltt,15) '     endfb6 dosimetry reaction (mf,mt): '
         write(ltt,15) '     ---------------------------------  '
         write(ltt,15) '$               enter=default (3 102) : '
         read(lkb,15)flnm
         if(flnm.eq.blnk) then
           mfrr=3
           mtrr=102
         else
           read(flnm,*)mfrr,mtrr
         endif
         write(nlog,*)mfrr,mtrr
c
c        process evaluated groupwise nuclear data file
c
         call rdhead(lib,tape,mat,mf,mt,ns)
         write(nlog,*)tape
         imf=0
         do while (mat.ne.-1 .and. imf.eq.0)
           call rdhead(lib,line,mat,mf,mt,ns)
           if (mf.eq.1.and.mt.eq.451) then
             backspace(lib)
 
c
c           get general data from 1/451 if any
c
             call getgd(lib,za0,zasym,temp)
           else if (mf.eq.mfrr.and.mt.eq.mtrr) then
             backspace(lib)
c
c          get cross section data form mfrr/mtrr
c
             call getxs(lib,za0,aw,e,xs,ngrp,imf)
             if (imf.gt.0) then
               write(nlog,*)za0,aw,temp,zasym
               if (imf.gt.1) then
                 write(nlog,*)' Warning: check data and energies'
                 write(nlog,*)' Energies'
                 write(nlog,'(1x,1p5e15.8)')(e(i),i=1,ngrp+1)
                 write(ltt,*)' Warning: check data and energies'
               endif
c
c              prepare a wimsd cross section file
c              for dosimetry materials
c
               call wimsxs(nout,za0,aw,xs,ngrp,nfast,nreso,idwims,temp)
               write(nlog,*)' Cross section (mf/mt) = ',mfrr,mtrr
               write(nlog,'(1x,1p5e15.8)')(xs(i),i=1,ngrp)
             else if (imf.lt.0) then
               write(nlog,*)' Error reading input data file. ERRC= ',imf
               write(ltt,*)' Error reading input data file. ERRC= ',imf
               stop
             endif
           endif
         end do
         if (imf.eq.0) then
           write(nlog,*)' Material data not found ERRC= -2'
           write(ltt,*)' Material data not found ERRC= -2'
         endif
       endif
       close(lib)
       close(nout)
       close(nlog)
15     format(2a40)
20     format(a5,i5,a5,a12,a5,f7.1,a5,f10.6,a5,i7)
      end
c
c      getgd: get general data from file 1
c
      subroutine getgd(lib,za0,zsymam,temp)
       character*66 text
       character*11 zsymam, zsym
       call rdcont(lib,za,c2,l1,l2,n1,n2,mat,mf,mt,ns)
       call rdhead(lib,text,mat,mf,mt,ns)
       call rdhead(lib,text,mat,mf,mt,ns)
       call rdcont(lib,c1,c2,l1,l2,n1,n2,mat,mf,mt,ns)
       call rdhead(lib,text,mat,mf,mt,ns)
       read(text,10)zsym
       if (za0.eq.0.0) za0=za
       delta=abs(za0-za)
       if (delta.le.0.01) then
          temp=c1
          zsymam=zsym
       endif
       do while(mt.eq.451)
         call rdhead(lib,text,mat,mf,mt,ns)
       enddo
       return
10     format(a11)
      end
c
c     getxs :  get cross section data in ENDFB-6 histogram format
c
      subroutine getxs(lib,za0,aw,e,xs,ngrp,imf)
       parameter (ntt=10, ngg=201,e69=10.0e6,e172=19.64e6,erel0=0.01)
       dimension e(*),xs(*),y(ngg),nb(ntt),itl(ntt)
       do i=1,ngrp+1
         e(i)=0.
         y(i)=0.
         xs(i)=0.
       enddo
       call rdcont(lib,za,aw,l1,l2,n1,n2,mat,mf,mt,ns)
       call rdtab1(lib,c1,c2,l1,l2,n1,n2,nb,itl,e,y,mat,mf,mt,ns)
       if (za0.eq.0.0) za0=za
       delta=abs(za0-za)
       if (delta.gt.0.01) return
       if (ngrp.eq.69) then
         erel=abs(e(n2)/e69 - 1.0)
       else if (ngrp.eq.172) then
         erel=abs(e(n2)/e172 - 1.0)
       else
         erel=0.0
       endif
       if (itl(1).eq.1.and.n1.eq.1.and.n2.eq.nb(1)) then
         do ig=1,n2-1
          xs(ig)=y(n2-ig)
         enddo
         if (erel.gt.erel0) then
           imf=10
         else
           imf=1
         endif
         return
       else
         imf=-1
         return
       endif
      end
c
c     rdhead, rdcont, rdtab1: read ENDFB6 records
c
      subroutine rdhead(lib,text,mat,mf,mt,ns)
       character*66 text
       read(lib,10)text,mat,mf,mt,ns
       return
10     format(a66,i4,i2,i3,i5)
      end
      subroutine rdcont(lib,c1,c2,l1,l2,n1,n2,mat,mf,mt,ns)
       read(lib,10)c1,c2,l1,l2,n1,n2,mat,mf,mt,ns
       return
10     format(2e11.0,4i11,i4,i2,i3,i5)
      end
      subroutine rdtab1(lib,c1,c2,l1,l2,n1,n2,nb,itl,x,y,mat,mf,mt,ns)
       dimension nb(*),itl(*),x(*),y(*)
       read(lib,10)c1,c2,l1,l2,n1,n2,mat,mf,mt,ns
       read(lib,20)(nb(i),itl(i),i=1,n1)
       read(lib,30)(x(i),y(i),i=1,n2)
       return
10     format(2e11.0,4i11,i4,i2,i3,i5)
20     format(6i11)
30     format(6e11.0)
      end
c
c     readxs: read a WIMSD formatted cross section file
c             or an energy boundary data file
c
      subroutine readxs(lib,xs,ngr1,ngf1,ngres1,temp,n3)
       dimension xs(*)
       read(lib,30)nz,n3,ngf1,ngres1,nth1
       if (n3.eq.1) then
        ngr1=ngf1-1
        read(lib,20)(xs(i),i=1,ngf1)
        return
       endif
       if(n3.eq.3) then
         read(lib,30)nz
         nz=nz/2
         read(lib,40)(z0,n2,i=1,nz)
         read(lib,30)nz,n3,ngf1,ngres1,nth1
       endif
       if (n3.eq.4) then
         nepi=ngf1+ngres1
         nres2=2*ngres1
         ngr1=nepi+nth1
         nepi1=nepi+1
         read(lib,10)nz,xz,nz,n2,nz
         read(lib,20)(z0,i=1,nres2),
     &                (xs(i),i=1,nepi),(xs(i),i=1,nepi),
     &                (z0,i=1,nres2)
         if (n2.gt.1) then
           read(lib,20)(z0,i=1,nepi),(z0,i=1,nepi)
         endif
         read(lib,30)nz
         read(lib,20)(z0,i=1,nz)
         read(lib,20)z0
         temp=z0
         read(lib,20)(xs(i),i=nepi1,ngr1),(xs(i),i=nepi1,ngr1)
         return
       endif
       n3=-1
       return
10     format(i6,e15.0,4i6)
20     format(5e15.0)
30     format(5i15)
40     format(3(e15.0,i6))
      end
c
c     wimsxs: prepare a wimsd formatted cross section file
c             for dosimetry materials
c
      subroutine wimsxs(nout,za0,aw,xs,ngrp,nfast,nreso,idwims,temp)
       parameter(xmass=1.008665,z0=0.,z1=1.,naldo=999999999,n2=2,nt=1)
       dimension xs(*)
       nz=za0/1000
       nepi=nfast+nreso
       nepi1=nepi+1
       nth=ngrp-nepi
       nres2=2*nreso
       write(nout,30)naldo,3
       write(nout,30)n2
       write(nout,40)z0,idwims
       write(nout,30)naldo,4,nfast,nreso,nth
       write(nout,10)idwims,aw*xmass,nz,0,nt,0
       write(nout,20)(z0,i=1,nres2),
     &               (xs(i),i=1,nepi),(xs(i),i=1,nepi),
     &               (z0,i=1,nres2)
       write(nout,30)3*nepi
       write(nout,20)(z1,z1,z0,i=1,nepi)
       write(nout,20)temp
       write(nout,20)(xs(i),i=nepi1,ngrp),(xs(i),i=nepi1,ngrp)
       write(nout,30)3*nth
       write(nout,20)(z1,z1,z0,i=1,nth)
       write(nout,30)naldo
10     format(i6,1pe15.8,4i6)
20     format(1p5e15.8)
30     format(5i15)
40     format(3(1pe15.8,i6))
      end
