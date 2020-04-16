program interp_intermediate

! interp-intermediate VERSION 1.3 2020-04-16
!
! Written by Bart Brashers, Ramboll (bbrashers@ramboll.com).
! If you find any bugs, please send them to me.
!
! Reads two files in WPS Intermediate Format version 5, interpolates 
! in time between them, and writes out the resulting file.
!
! Revision History:
! 2015-01-23 Bug fix: interpolotion f vs. (1-f) was wrong.
! 2020-04-13 Add ability to process multiple blocks of data. 
!
!----------------------------------------------------------------------
! This program is free software, using GNU General Public License v3.0.
! See the file COPYING, or https://www.gnu.org/licenses/gpl-3.0.en.html
!----------------------------------------------------------------------

  implicit none
  integer, external  :: iargc
  integer :: version                  ! Format version (must =5 for WPS format)
  integer :: year,month,day,hour      ! output time-stamp
  integer :: nx, ny, iproj            ! size of the arrays, projection
  integer :: nx2, ny2, iproj2         ! for infile2, should == nx,ny
  integer :: i,j, num_arg, hours, ii,jj, ios
  integer :: ipt = 1                  ! in debug mode, print the value at 
  integer :: jpt = 1                  ! this point

  real, allocatable,dimension(:,:) :: input1, input2 ! input data
  real, allocatable,dimension(:,:) :: output         ! output data

  real :: bad,bad1,bad2               ! missing/bad value flags
  real :: earth_radius                ! radius of the earth in the files
  real :: nlats                       ! Number of latitudes north of equator
  real :: xfcst                       ! Forecast hour of data
  real :: xlvl                        ! Vertical level of data in 2-d array
  real :: startlat, startlon          ! Lat/lon of point in array indicated by startloc
  real :: deltalat, deltalon          ! Grid spacing, degrees
  real :: dx, dy                      ! Grid spacing, km
  real :: xlonc                       ! Standard longitude of projection
  real :: tlat1, tlat2                ! True latitudes of projection
  real :: f                           ! fraction for interpolation

  real :: startlat2, startlon2, deltalat2, deltalon2
  real :: dx2, dy2, xlonc2, tlat12, tlat22, nlats2

  logical :: ok
  logical :: debug = .false.          ! set to .true. to debug
  logical :: is_wind_grid_rel         ! Flag indicating whether winds are  
                                      !   relative to source grid (TRUE) or
                                      !   relative to earth (FALSE)
  character (len=8)   :: startloc     ! Which point in array is given by
                                      !   startlat/startlon; set either 
                                      !   to 'SWCORNER' or 'CENTER  '
  character (len=8)   :: startloc2    
  character (len=9)   :: field        ! Name of the field
  character (len=9)   :: field2       
  character (len=24)  :: hdate        ! Valid date for data YYYY:MM:DD_HH:00:00
  character (len=24)  :: hdate1,hdate2
  character (len=25)  :: units        ! Units of data
  character (len=32)  :: map_source   ! Source model / originating center
  character (len=46)  :: desc         ! Short description of data

  character (len=256) :: arg,infile1, infile2, outfile

  bad = -1.e+30 ! default bad value flag
  bad1 = bad    ! default to using the same for both input files
  bad2 = bad
  ii = -999     ! for printing the values
  jj = -999 

  num_arg = command_argument_count() ! FORTRAN 2003 intrinsic: PGI,ifort,gfortran
! num_arg = iargc()    ! FORTRAN 90 external function
! num_arg = NARGS()    ! COMPAQ, Microsoft, or HP compiler

  if (num_arg == 0) call usage

  i = 0
  do while (i < num_arg)
     i = i + 1
     call getarg(i,arg) ! PGI, Intel ifort, or GNU gfortran, or Sun compiler

! Other compilers use similar calls:
!     call getcl(arg)            ! Lahey compiler
!     call getarg(i,arg)         ! COMPAQ compiler
!     call getarg(i,arg,istat)   ! Microsoft compiler
!     call getarg(i,arg)         ! HP compiler: needs +U77 switch to compile

     if (adjustl(arg) == "--help" .or. adjustl(arg) == "-h") then
        call usage
     else if (adjustl(arg) == '-d' .or. adjustl(arg) == '--debug') then
        debug = .true.
        write(*,*) "Debug mode turned on."
     else if (adjustl(arg) == '--ij') then
        i = i + 1
        call getarg(i,arg) ; read(arg,*) ipt
        i = i + 1
        call getarg(i,arg) ; read(arg,*) jpt
     elseif (adjustl(arg) == "-i") then
        i = i + 1
        call getarg(i,infile1)
        i = i + 1
        call getarg(i,infile2)
     elseif (adjustl(arg) == "-o") then
        i = i + 1
        call getarg(i,outfile)
     elseif (adjustl(arg) == "-p") then
        i = i + 1
        call getarg(i,arg)
        read(arg,*) ii,jj ! print at this point
     elseif (adjustl(arg) == "-m") then
        i = i + 1
        call getarg(i,arg)
        read(arg,*) bad
        bad1 = bad
        bad2 = bad
     elseif (adjustl(arg) == "-m1") then
        i = i + 1
        call getarg(i,arg)
        read(arg,*) bad1
     elseif (adjustl(arg) == "-m2") then
        i = i + 1
        call getarg(i,arg)
        read(arg,*) bad2
     else
        write(*,*) "Error: unknown switch: ",trim(arg)
        call usage
     endif
     
  end do

! Make sure the files exist and open them

  inquire(file=infile1, exist=ok)
  if (.not. ok) then
     write(*,*) "Error: file not found: ",trim(infile1)
     stop
  endif
  open(10,file=infile1,form='unformatted',convert='big_endian', &
       status='old',err=99)

  inquire(file=infile2, exist=ok)
  if (.not. ok) then
     write(*,*) "Error: file not found: ",trim(infile2)
     stop
  endif
  open(11,file=infile2,form='unformatted',convert='big_endian',  &
       status='old',err=99)

! Open the output file

  open(12,file=outfile,form='unformatted',convert='big_endian',err=99)

!*********************************************************************
! LOOP OVER FIELDS FOUND IN INPUT FILES
!*********************************************************************

  ios = 0 ! a leap of faith

  do while (ios == 0)

! Read the 1st input file

     read(10, iostat=ios) version

     if (ios /= 0) exit ! should be triggered by end-of-file

     if (version /= 5) then
        write(*,*) "This code only works for Intermediate files version 5 (WPS)."
        write(*,*) trim(infile1)," is version ",version
        stop
     endif
     read(10) hdate1,xfcst,map_source,field,units,desc,xlvl,nx,ny,iproj

     if (iproj .eq. 0) then        ! Cylindrical equidistant
        read(10) startloc,startlat,startlon,deltalat,deltalon,earth_radius
     else if (iproj .eq. 1) then   ! Mercator
        read(10) startloc,startlat,startlon,dx,dy,tlat1,earth_radius
     else if (iproj .eq. 3) then   ! Lambert conformal
        read(10) startloc,startlat,startlon,dx,dy,xlonc,tlat1,tlat2,earth_radius
     else if (iproj .eq. 4) then   ! Gaussian
        read(10) startloc,startlat,startlon,nlats,deltalon,earth_radius
     else if (iproj .eq. 5) then   ! Polar stereographic
        read(10) startloc,startlat,startlon,dx,dy,xlonc,tlat1,earth_radius
     end if

     read(10) is_wind_grid_rel     ! rotation flag

     if (debug) then
        write(*,*) "Read infile1:"
        write(*,*) "  hdate = ",hdate1
        write(*,*) "  field = ",field
        write(*,*) "  units = ",units
        write(*,*) "  desc  = ",desc 
        write(*,*) "  nx,ny = ",nx,ny
        write(*,*) "  iproj = ",iproj
     endif

     if (.not. allocated(input1)) allocate( input1(nx,ny) )
     read(10) input1               ! array of data

     if (debug) then
        write(*,*) "  I,J   = ",ipt,jpt
        write(*,*) "  data  = ",input1(ipt,jpt)
     endif

! Read the 2nd input file

     read(11) version
     if (version /= 5) then
        write(*,*) "This code only works for Intermediate file version 5."
        write(*,*) trim(infile1)," is version ",version
        stop
     endif
     read(11) hdate2,xfcst,map_source,field2,units,desc,xlvl,nx2,ny2,iproj2

! Do some checks to make sure the interpolation is valid

     if (iproj2 /= iproj) then
        write(*,*) "Error: projection of input files are not the same."
        write(*,*) "  iproj = ",iproj, " for ",trim(infile1)
        write(*,*) "  iproj = ",iproj2," for ",trim(infile2)
        stop
     endif
     if (nx2 /= nx .or. ny2 /= ny) then
        write(*,*) "Error: dimensions of input files are not the same."
        write(*,*) "  nx,ny = ",nx,ny,  " for ",trim(infile1)
        write(*,*) "  nx,ny = ",nx2,ny2," for ",trim(infile2)
        stop
     endif
     if (field /= field2) then
        write(*,*) "Error: parameter in input files are not the same."
        write(*,*) "  field = ",field, " for ",trim(infile1)
        write(*,*) "  field = ",field2," for ",trim(infile2)
        stop
     endif

! If it's ok, continue to read, but check the details of the projections

     if (iproj .eq. 0) then        ! Cylindrical equidistant

        read(11) startloc2,startlat2,startlon2,deltalat2,deltalon2,earth_radius
        if (startloc /= startloc2 .or. startlat /= startlat2 .or.  &
             startlon /= startlon2 .or. deltalat /= deltalat2 .or. &
             deltalon /= deltalon2) then
           write(*,*) "Error: grids in input files are not the same."
           write(*,*) "  startloc = ",startloc, " for ",trim(infile1)
           write(*,*) "  startloc = ",startloc2," for ",trim(infile2)
           write(*,*) "  startlat = ",startlat, " for ",trim(infile1)
           write(*,*) "  startlat = ",startlat2," for ",trim(infile2)
           write(*,*) "  startlon = ",startlon, " for ",trim(infile1)
           write(*,*) "  startlon = ",startlon2," for ",trim(infile2)
           write(*,*) "  deltalat = ",deltalat, " for ",trim(infile1)
           write(*,*) "  deltalat = ",deltalat2," for ",trim(infile2)
           write(*,*) "  deltalon = ",deltalon, " for ",trim(infile1)
           write(*,*) "  deltalon = ",deltalon2," for ",trim(infile2)
           stop
        endif

     else if (iproj .eq. 1) then   ! Mercator

        read(11) startloc2,startlat2,startlon2,dx2,dy2,tlat12,earth_radius
        if (startloc /= startloc2 .or. startlat /= startlat2 .or.     &
             startlon /= startlon2 .or. dx /= dx2 .or. dy /= dy2 .or. &
             tlat1 /= tlat12) then
           write(*,*) "Error: grids in input files are not the same."
           write(*,*) "  startloc = ",startloc, " for ",trim(infile1)
           write(*,*) "  startloc = ",startloc2," for ",trim(infile2)
           write(*,*) "  startlat = ",startlat, " for ",trim(infile1)
           write(*,*) "  startlat = ",startlat2," for ",trim(infile2)
           write(*,*) "  startlon = ",startlon, " for ",trim(infile1)
           write(*,*) "  startlon = ",startlon2," for ",trim(infile2)
           write(*,*) "  dx       = ",dx, " for ",trim(infile1)
           write(*,*) "  dx       = ",dx2," for ",trim(infile2)
           write(*,*) "  dy       = ",dy, " for ",trim(infile1)
           write(*,*) "  dy       = ",dy2," for ",trim(infile2)
           write(*,*) "  tlat1    = ",tlat1, " for ",trim(infile1)
           write(*,*) "  tlat1    = ",tlat12," for ",trim(infile2)
           stop
        endif

     else if (iproj .eq. 3) then   ! Lambert conformal

        read(11) startloc2,startlat2,startlon2,dx2,dy2,xlonc2,tlat12,tlat22, &
             earth_radius
        if (startloc /= startloc2 .or. startlat /= startlat2 .or.     &
             startlon /= startlon2 .or. dx /= dx2 .or. dy /= dy2 .or. &
             xlonc /= xlonc2 .or. tlat1 /= tlat12 .or. tlat2 /= tlat22) then
           write(*,*) "Error: grids in input files are not the same."
           write(*,*) "  startloc = ",startloc, " for ",trim(infile1)
           write(*,*) "  startloc = ",startloc2," for ",trim(infile2)
           write(*,*) "  startlat = ",startlat, " for ",trim(infile1)
           write(*,*) "  startlat = ",startlat2," for ",trim(infile2)
           write(*,*) "  startlon = ",startlon, " for ",trim(infile1)
           write(*,*) "  startlon = ",startlon2," for ",trim(infile2)
           write(*,*) "  dx       = ",dx, " for ",trim(infile1)
           write(*,*) "  dx       = ",dx2," for ",trim(infile2)
           write(*,*) "  dy       = ",dy, " for ",trim(infile1)
           write(*,*) "  dy       = ",dy2," for ",trim(infile2)
           write(*,*) "  xlonc    = ",xlonc, " for ",trim(infile1)
           write(*,*) "  xlonc    = ",xlonc2," for ",trim(infile2)
           write(*,*) "  tlat1    = ",tlat1, " for ",trim(infile1)
           write(*,*) "  tlat1    = ",tlat12," for ",trim(infile2)
           write(*,*) "  tlat2    = ",tlat2, " for ",trim(infile1)
           write(*,*) "  tlat2    = ",tlat22," for ",trim(infile2)
           stop
        endif

     else if (iproj .eq. 4) then   ! Gaussian

        read(11) startloc2,startlat2,startlon2,nlats2,deltalon2,earth_radius
        if (startloc /= startloc2 .or. startlat /= startlat2 .or.  &
             startlon /= startlon2 .or. nlats /= nlats2 .or.       &
             deltalon /= deltalon2 ) then
           write(*,*) "Error: grids in input files are not the same."
           write(*,*) "  startloc = ",startloc, " for ",trim(infile1)
           write(*,*) "  startloc = ",startloc2," for ",trim(infile2)
           write(*,*) "  startlat = ",startlat, " for ",trim(infile1)
           write(*,*) "  startlat = ",startlat2," for ",trim(infile2)
           write(*,*) "  startlon = ",startlon, " for ",trim(infile1)
           write(*,*) "  startlon = ",startlon2," for ",trim(infile2)
           write(*,*) "  nlats    = ",nlats, " for ",trim(infile1)
           write(*,*) "  nlats    = ",nlats2," for ",trim(infile2)
           write(*,*) "  deltalon = ",deltalon, " for ",trim(infile1)
           write(*,*) "  deltalon = ",deltalon2," for ",trim(infile2)
           stop
        endif

     else if (iproj .eq. 5) then   ! Polar stereographic

        read(11) startloc2,startlat2,startlon2,dx2,dy2,xlonc2,tlat12,earth_radius
        if (startloc /= startloc2 .or. startlat /= startlat2 .or.     &
             startlon /= startlon2 .or. dx /= dx2 .or. dy /= dy2 .or. &
             xlonc /= xlonc2 .or. tlat1 /= tlat12) then
           write(*,*) "Error: grids in input files are not the same."
           write(*,*) "  startloc = ",startloc, " for ",trim(infile1)
           write(*,*) "  startloc = ",startloc2," for ",trim(infile2)
           write(*,*) "  startlat = ",startlat, " for ",trim(infile1)
           write(*,*) "  startlat = ",startlat2," for ",trim(infile2)
           write(*,*) "  startlon = ",startlon, " for ",trim(infile1)
           write(*,*) "  startlon = ",startlon2," for ",trim(infile2)
           write(*,*) "  dx       = ",dx, " for ",trim(infile1)
           write(*,*) "  dx       = ",dx2," for ",trim(infile2)
           write(*,*) "  dy       = ",dy, " for ",trim(infile1)
           write(*,*) "  dy       = ",dy2," for ",trim(infile2)
           write(*,*) "  xlonc    = ",xlonc, " for ",trim(infile1)
           write(*,*) "  xlonc    = ",xlonc2," for ",trim(infile2)
           write(*,*) "  tlat1    = ",tlat1, " for ",trim(infile1)
           write(*,*) "  tlat1    = ",tlat12," for ",trim(infile2)
           stop
        endif

     end if

     read(11) is_wind_grid_rel     ! rotation flag

     if (debug) then
        write(*,*) "Read infile2:"
        write(*,*) "  hdate = ",hdate2
        write(*,*) "  field = ",field2
        write(*,*) "  units = ",units
        write(*,*) "  desc  = ",desc 
        write(*,*) "  nx,ny = ",nx2,ny2
        write(*,*) "  iproj = ",iproj2
     endif

     if (.not. allocated(input2)) allocate( input2(nx,ny) )
     read(11) input2               ! array of data

     if (debug) then
        write(*,*) "  I,J   = ",ipt,jpt
        write(*,*) "  data  = ",input2(ipt,jpt)
     endif

  
! Interpolate, weighted by the time difference
  
     i = index(outfile,":",.true.) ! find last ":", time-stamp follows that
     call TimeStamp2ymdh(outfile(i+1:i+13), year,month,day,hour)
     write(hdate,'(i4.4,"-",i2.2,"-",i2.2,"_",i2.2,":00:00")') year,month,day,hour

     call TimeStampDiff(hdate1,hdate2,hours) ! time between inputs

     call TimeStampDiff(hdate1,hdate, i)
     call TimeStampDiff(hdate, hdate2,j)
     if (sign(1, hours) /= sign(1, i) .or.   &
          sign(1, hours) /= sign(1, j)) then  ! hdate not between hdate1 and hdate2
        write(*,*) "Error: output time-stamp is not between input time-stamps."
        write(*,*) "  hdate = ",trim(hdate1)," for input  ",trim(infile1)
        write(*,*) "  hdate = ",trim(hdate) ," for OUTPUT ",trim(outfile)
        write(*,*) "  hdate = ",trim(hdate2)," for input  ",trim(infile2)
        stop
     endif

     f  = real(i)/real(hours)             ! interpolation factor

     if (.not. allocated(output)) allocate( output(nx,ny) )
     output = bad
     where (input1 /= bad1 .and. input2 /= bad2) output = (1.-f)*input1 + f*input2

     if (ii > 0 .and. ii < nx .and. jj > 0 .and. jj < ny) then
        print*,"val before: ",input1(ii,jj)
        print*,"val interp: ",output(ii,jj)
        print*,"val after : ",input2(ii,jj)
     end if

! Write the output file

     write(12) version
     write(12) hdate,xfcst,map_source,field,units,desc,xlvl,nx,ny,iproj

     if (iproj .eq. 0) then ! Cylindrical equidistant
        write(12) startloc,startlat,startlon,deltalat,deltalon,earth_radius
     else if (iproj .eq. 1) then ! Mercator
        write(12) startloc,startlat,startlon,dx,dy,tlat1,earth_radius
     else if (iproj .eq. 3) then ! Lambert conformal
        write(12) startloc,startlat,startlon,dx,dy,xlonc,tlat1,tlat2,earth_radius
     else if (iproj .eq. 4) then ! Gaussian
        write(12) startloc,startlat,startlon,nlats,deltalon,earth_radius
     else if (iproj .eq. 5) then ! Polar stereographic
        write(12) startloc,startlat,startlon,dx,dy,xlonc,tlat1,earth_radius
     end if

     write(12) is_wind_grid_rel   !  3) WRITE WIND ROTATION FLAG

     if (debug) then
        write(*,*) "Writing to outfile:"
        write(*,*) "  hdate = ",hdate
        write(*,*) "  field = ",field
        write(*,*) "  units = ",units
        write(*,*) "  desc  = ",desc 
        write(*,*) "  nx,ny = ",nx,ny
        write(*,*) "  iproj = ",iproj
     endif

     write(12) output             !  4) WRITE 2-D ARRAY OF DATA

     if (debug) then
        write(*,*) "  I,J   = ",ipt,jpt
        write(*,*) "  data  = ",output(ipt,jpt)
        write(*,*) 
     endif

  end do ! main loop over blocks in the input files

  close(10) ! infile1
  close(11) ! infile2
  close(12) ! outfile

  deallocate( input1, input2, output )

  stop

97 write(*,*) "Error opening ",trim(infile1)
  stop
98 write(*,*) "Error opening ",trim(infile2)
  stop
99 write(*,*) "Error opening ",trim(outfile)
  stop

end program interp_intermediate
!
!*************************************************************************
!
subroutine usage

  write(*,*) 'Usage: interp-intermediate [options] -i file1 file2 -o file3'
  write(*,*) 'Options:'
  write(*,*) "  -m  val       specify input1, input2, and output's missing_value"
  write(*,*) "                 (default = -1.e+30)"
  write(*,*) "  -m1 val       specify just file1's missing_value"
  write(*,*) "  -m2 val       specify just file2's missing_value"
  write(*,*) "  -d | --debug  print some debugging info"
  write(*,*) "  --ij I J      in debug mode, print the values at this point"
  write(*,*) "  -h | --help   print this help message"
  write(*,*) 
  write(*,*) 'Examples: '
  write(*,*) 'interp-intermediate -i SNOWH:2011-01-01_12 SNOWH:2011-01-02_12'
  write(*,*) '   -o SNOWH:2011-01-01_18'
  write(*,*) 
  write(*,*) 'interp-intermediate -i subdir/SNOWH:2011-01-01_12 '
  write(*,*) '   subdir/SNOWH:2011-01-02_12 -o other_dir/SNOWH:2011-01-02_00'
  write(*,*) 
  write(*,*) 'The time-stamp of the output is extracted from the filename.'
  write(*,*) 'Using the -m flag will also set -m1 and -m2, so if the two input'
  write(*,*) 'files have different missing_values, give them in the following '
  write(*,*) 'order: interp-intermediate -m -1.e30 -m1 0. -m2 1.e30 ...'
  stop

end subroutine usage
!
!------------------------------------------------------------------------------
! The following subroutines are from MMIFv3.0, available at
!    http://www.epa.gov/ttn/scram/dispersion_related.htm#mmif
!------------------------------------------------------------------------------
!
subroutine TimeStamp2ymdh(TimeStamp,iy,im,id,ih)
!
!------Returns year, month, day, hour from a WRF/MM5 time-stamp string, 
!      e.g. 2001-01-01_10:00:00 (WRF) or 2004-12-31_14:00:00.0000 (MM5)
!
  character (len=*), intent(in) :: TimeStamp
  integer, intent(out) :: iy,im,id,ih

  read(TimeStamp,'(i4,x,i2,x,i2,x,i2)') iy, im, id, ih

end subroutine TimeStamp2ymdh
!
!------------------------------------------------------------------------------
!
subroutine TimeStampDiff(TimeStamp1,TimeStamp2,hrs)
!
!------Returns the number of hours between two WRF/MM5 time-stamp strings
!
  character (len=*), intent(in) :: TimeStamp1, TimeStamp2
  integer,          intent(out) :: hrs
  integer iy1,im1,id1,ih1, iy2,im2,id2,ih2 ! local variables

  call TimeStamp2ymdh(TimeStamp1,iy1,im1,id1,ih1)
  call TimeStamp2ymdh(TimeStamp2,iy2,im2,id2,ih2)

  call TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)

  return
end subroutine TimeStampDiff
!
!------------------------------------------------------------------------------
!
subroutine TimeDiff(iy1,im1,id1,ih1, iy2,im2,id2,ih2, hrs)
!
!------Returns the number of hours between two WRF/MM5 time-stamp strings
!
  integer, intent(in)  :: iy1,im1,id1,ih1, iy2,im2,id2,ih2
  integer, intent(out) :: hrs
  integer minyr, hrs1,hrs2 ! local variales
  logical Leap_yr
!
!-----Calculate the number of hours since the beginning of the earliest year:
!
  minyr = min(iy1,iy2) - 1

  hrs1 = 24*(id1-1) + ih1 ! days of this month, plus hours for today
  do i = minyr, iy1-1     ! possibly zero times through the loop
     hrs1 = hrs1 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs1 = hrs1 + 24
  end do
  if (im1 > 1) then       ! add the hours for all the months until last month
     do i = 1, im1-1
        hrs1 = hrs1 + iDaysInMth(i,iy1) * 24
     end do
  end if

  hrs2 = 24*(id2-1) + ih2 ! days of this month, plus hours for today
  do i = minyr, iy2-1     ! possibly zero times through the loop
     hrs2 = hrs2 + 8760   ! add the hours for all the years until this year
     if (Leap_yr(i)) hrs2 = hrs2 + 24
  end do
  if (im2 > 1) then       ! add the hours for all the months until last month
     do i = 1, im2-1
        hrs2 = hrs2 + iDaysInMth(i,iy2) * 24
     end do
  end if

  hrs = hrs2 - hrs1       ! answer is the difference

end subroutine TimeDiff
!
!------------------------------------------------------------------------------
!
logical function leap_yr(Yr) ! Y2K correct.

  integer yr

! If integer divide and float divide give the same result, then it's
! evenly divisible.  Could have used mod() here, but I think this is
! more portable.  Might give problems on some old Pentium systems
! with that pesky 4./2. = 1.9999998 error.

! The year is 365.2422 days long.
! Rules for leap years: if year is evenly divisible by 4, then it's
! a leap year, except those evenly divisible by 100, but there is a
! leap year in those evenly divisible by 400.  This will give a mean
! year 365.2425 days long.  Error is .0003 days (25.92 seconds) so
! it will take 3333 years for the calendar to get off by one day.

  if ((float(Yr/4) .eq. Yr/4.) .and. ((float(Yr/100) .ne. Yr/100.) &
       .or. (float(Yr/400) .eq. Yr/400.))) then
     leap_yr = .true.
  else
     leap_yr = .false.
  end if
  
  return
end function leap_yr
!
!------------------------------------------------------------------------------
!
integer function iDaysInMth(im,iy)
!
!-----Sets the number of days in the month
!
  integer mon(12),iy,im
  logical Leap_yr
  data mon/31,28,31,30,31,30,31,31,30,31,30,31/

  if (Leap_yr(iy) .and. im == 2) then
     iDaysInMth = 29
  else
     iDaysInMth = mon(im)
  end if
  return

end function iDaysInMth
!
!------------------------------------------------------------------------------
!
