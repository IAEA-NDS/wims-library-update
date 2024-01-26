      program updbrn
c
c     updbrn: Update energy released per fission and decay data
c             d.l.aldama, iaea/nds consultant
c             December 2006
c
c     Input files: (enter interactively from the default input unit)
c            enfiss.lst: output from enfiss.for
c                        (energy released per fission data)
c            endfdy.dat: evaluated nuclear decay data file
c            endfnt.dat: evaluated nuclear neutron data file
c            njoyin.nji: NJOY input option file
c
c     Output file:
c            updated.nji: updated NJOY input file
c
c     Description
c            The program asks for the input file names.
c            Extract energy released per fission and decay data
c            from the evaluated nuclear data files. Finally,
c            update and write the new NJOY input option file.
c
      character*20  tok(20)
      character*80  fn,line,lline
      dimension matef(200),ef(200)
      dimension ddza(5000),hldd(5000)
      dimension matn(500),zan(500)
c
c     Read fission energy released per fission
c     Output of enfiss.for code
c
      write(*,*)' Program updbrn: Update E-fission and Decay Data'
      write(*,*)' ==============================================='
      write(*,*)
      write(*,*)' Enter enfiss output data file: '
      read(*,*)fn
      write(*,*)' Energy released per fission read from: ', fn
      open(1,file=fn)
      ifind=0
      do while (ifind.eq.0)
        read(1,'(a80)',end=100)line
        if (line(54:61).eq.'[J/mole]') ifind=1
      enddo
 100  if (ifind.ne.0) then
        i=0
        do while (.true.)
          read(1,'(a80)', end=200)line
          i=i+1
          read(line,'(i6,45x,e11.0)')matef(i),ef(i)
          write(*,'(i6,1pe12.5)')matef(i),ef(i)
          if (i .eq. 200) then
            write(*,*)' Too many materials in enfiss output file'
            stop
        enddo
      endif
 200  nmatef=i
      close(1)
      write(*,*)
c
c    Read evaluated decay data file
c
      write(*,*)' Enter evaluated decay data file: '
      read(*,*)fn
      write(*,*)' Decay data read from: ',fn
      open(1, file=fn)
      read(1,'(a66,i4,i2,i3,i5)')line,matdd,mf,mt,is
      matdd=10000
      j=0
      do while(matdd.ge.0)
        read(1,'(a66,i4,i2,i3,i5)')line,matdd,mf,mt,is
        if (mf.eq.8) then
          j=j+1
          read(line,'(2e11.0,4i11)')ddza(j),dummy,lisdd,lis0dd
          ddza(j)=ddza(j)+0.1*lis0dd
          read(1,'(2e11.0)')hldd(j),dummy
          write(*,'(f11.1,1pe13.5)')ddza(j),hldd(j)
          do while (mf.eq.8)
            read(1,'(a66,i4,i2,i3,i5)')line,matdd,mf,mt,is
          enddo
        endif
      enddo
      ndd=j
      close(1)
      write(*,*)
c
c     Evaluated nuclear data file for XS
c
      write(*,*)' Enter evaluated nuclear data file for XS: '
      read(*,*)fn
      write(*,*)' Evaluated nuclear data read from: ',fn
      open(1,file=fn)
        read(1,'(a66,i4,i2,i3,i5)')line,mat,mf,mt,is
        mat=10000
        j=0
        do while(mat.ge.0)
          read(1,'(a66,i4,i2,i3,i5)')line,mat,mf,mt,is
          if (mf.eq.1 .and. mt.eq.451) then
            j=j+1
            matn(j)=mat
            read(line,'(e11.0)')zan(j)
            read(1,'(22x,4i11)')lisn,lis0n
            zan(j)=zan(j)+0.1*lis0n
            write(*,'(i6,f10.1)')matn(j),zan(j)
            do while (mt.eq.451)
              read(1,'(a66,i4,i2,i3,i5)')line,mat,mf,mt,is
            enddo
          endif
        enddo
      close(1)
      nmatn=j
      write(*,*)
c
c     Updating energy released per fission and decay data
c
      write(*,*)' Enter NJOY input data file: '
      read(*,*)fn
      write(*,*)' Njoy input options read from: ',fn
      open(1,file=fn)
      open(2,file='updated.nji')
      do while (.true.)
        read(1,'(a80)',END=300)lline
        call gettok(lline,20,ntok,tok)
        if (tok(1)(1:2).eq.'--'.and.lline(4:9).ne.'      ') then
          write (*,'(a17,a6)')'  Updating deck: ',tok(2)(1:6)
          call writelt(2,lline)
        else if (tok(1)(1:5).eq.'wimsr') then
          call writelt(2,lline)
          read(1,'(a80)')lline
          call writelt(2,lline)
          read(1,'(a80)')lline
          call writelt(2,lline)
          call gettok(lline,20,ntok,tok)
          igroup=0
          if (ntok.ge.3) igroup=itok(tok(3))
          if (igroup.ne.0) then
            read(1,'(a80)')lline
            call writelt(2,lline)
          endif
          read(1,'(a80)')lline
          call writelt(2,lline)
          call gettok(lline,20,ntok,tok)
          if (ntok.ge.1)imat=itok(tok(1))
          zanji=-1000
          ifind=0
          j=1
          do while (j.le.nmatn.and.ifind.eq.0)
            if (imat.eq.matn(j)) then
              ifind=1
              zanji=zan(j)
            else
              j=j+1
            endif
          enddo
          iburn=4
          if (ntok.ge.4) iburn=itok(tok(4))
          if (iburn.gt.0) then
            read(1,'(a80)')lline
            call writelt(2,lline)
            call gettok(lline,20,ntok,tok)
            ifprod=0
            if (ntok.ge.11) ifprod=itok(tok(11))
            read(1,'(a80)')lline
            if (ifprod.eq.0) then
              call gettok(lline,20,ntok,tok)
              nn1=2
              if (ntok.ge.1) nn1=itok(tok(1))
              eff=0.0
              if (ntok.ge.2) eff=ftok(tok(2))
              ifind=0
              j=1
              do while (j.le.nmatef.and.ifind.eq.0)
                if (imat.eq.matef(j)) then
                  eff=ef(j)
                  ifind=1
                else
                   j=j+1
                endif
              enddo
              if (nn1.eq.2) then
                write(line,'(i6,1pe11.4)')nn1,eff
                lline=adjustl(line)
                call writelt(2,lline)
              else
                write(2,'(i6,1pe11.4)')nn1,eff
              endif
            else
              call writelt(2,lline)
            endif
            read(1,'(a80)')lline
            call writelt(2,lline)
            read(1,'(a80)')lline
            call gettok(lline,20,ntok,tok)
            nn=itok(tok(1))
            decay=0.0
            if (ntok.ge.2)decay=ftok(tok(2))
            if (.not.(decay .eq. 0.0 .and. nn.eq.0)) then
              if (zanji.ge.0.0) then
                kfind=0
                j=1
                do while (j.le.ndd.and.kfind.eq.0)
                  if (abs(zanji-ddza(j)).lt.0.01) then
                    kfind=1
                    decay=0.6931472/hldd(j)
                  else
                    j=j+1
                  endif
                enddo
              endif
              if (ifprod.ne.0 .or. nn1.eq.2) then
                 write(line,'(i6,1pe11.4)')nn,decay
                 lline=adjustl(line)
                 call writelt(2,lline)
              else
                write(2,'(i6,1pe11.4)')nn,decay
              endif
            else
              call writelt(2,lline)
            endif
          endif
        else
          call writelt(2,lline)
        endif
      enddo
 300  close(1)
      close(2)
      stop
      end
c
c       gettok, ftok, itok
c
      subroutine gettok(line,marg,narg,arg)
c
c     version : 1.00
c
c     compiler: lahey fortran 77. version 4.00
c
c     written by : d. l. aldama
c                  la habana, cuba
c                  setp./94
c
c     input parameters:
c       marg : maximum number of tokens,
c              equal to the array arg dimension.
c
c     output parameters:
c       narg : actual number of tokens,
c              (narg.le.marg).
c       arg  : character array of tokens.
c
c
      character*(*) line
      character*(*) arg(marg)
      kmax=len(line)
      k=len(arg(1))
c      write(*,*)kmax,line,k
      do i=1,marg
        do j=1,k
          arg(i)(j:j)=' '
        end do
      end do
      narg=0
      i=1
      do while (i.le.kmax.and.line(i:i).ne.'/')
        if (line(i:i).eq.' ') then
          i=i+1
        else
          narg=narg+1
          if (narg.gt.marg) then
            narg=marg
c            write(*,*)narg,arg
c            return
          endif
          k=1
          do while (line(i:i).ne.' '.and.line(i:i).ne.'/')
            arg(narg)(k:k)=line(i:i)
            k=k+1
            i=i+1
          end do
        endif
      end do
c      write(*,*)narg,arg
      return
      end
      real*4 function ftok(string)
      character*(*) string
      k=len(string)
      read(string(1:k),*)ftok
      return
      end
      integer*4 function itok(string)
      character*(*) string
      k=len(string)
      read(string(1:k),*)itok
      return
      end
      subroutine writelt(nunit,lstr)
        character*(*) lstr
        ltrim=len_trim(lstr)
        write(nunit,'(a)')lstr(1:ltrim)
        return
      end
