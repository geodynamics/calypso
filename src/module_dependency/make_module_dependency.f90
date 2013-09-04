!make_module_dependency.f90
!
!      subroutine alloc_mod_list
!      subroutine make_module_dependency(dirname, filename)
!
!
!> @brief      Constract module dependency for Makefile
!>        Usage: kemo_module_dep MAKEFILE SOURCE_DIRECTORY SOURCE_FILES
!>        for single file: kemo_module_dep MAKEFILE SOURCE_FILES
!>        Output command list for source with .f90 extension
!>	$(F90) -c $(F90OPTFLAGS) $<
!>        Output command list for source with .F90 extension
!>	$(F90) -c $(F90FLAGS) $<
!>
!>
!>     Warning: To use this program, module name
!>              must be same as the file name!!
!
      module module_dependency
!
      implicit none
!
      integer :: ntot, num
      integer, parameter :: id_makefile = 21
      character(len=64), allocatable :: mod_name(:)
      character(len=28), parameter, private :: command                  &
     &   = '	$(F90) -c $(F90OPTFLAGS) $<'
      character(len=25), parameter, private :: command_cpp              &
     &   = '	$(F90) -c $(F90FLAGS) $<'
!
      integer :: num_exclude = 1
      character(len=31) ::  exclude_list(1) = (/'hdf5'/)
!
      private :: num_exclude
      private :: extend_mod_list, const_module_list
      private :: write_module_list
!
!      subroutine getarg_k(i, argc)
!      integer function iargc_kemo() result(oresult)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_mod_list
!
      allocate(mod_name(ntot))
!
      end subroutine alloc_mod_list
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_mod_list
!
      deallocate(mod_name)
!
      end subroutine dealloc_mod_list
!
!   --------------------------------------------------------------------
!
      subroutine extend_mod_list
!
      character(len=64), allocatable :: mod_tmp(:)
!
      allocate(mod_tmp(ntot))
      mod_tmp(1:ntot) = mod_name(1:ntot)
      deallocate(mod_name)
!
      ntot = 2*ntot
      allocate(mod_name(ntot))
      mod_name(1:num) = mod_tmp(1:num)
      deallocate(mod_tmp)
!
      end subroutine extend_mod_list
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_module_list(filename)
!
      character(len=255), intent(in) :: filename
!
      character(len=255) :: tmpchara, mod_input
      character(len=31) ::  module_name
      integer :: iflag, j
!
!
!      write(*,*) 'open: ', trim(filename)
      open(7,file=filename, status='old')
      num =  0
      do
        read(7,'(a255)',end=99) tmpchara
        if( len(trim(tmpchara)) .gt. 0) then
          read(tmpchara,*) mod_input
          if(mod_input == 'use' .or. mod_input == 'USE') then
            read(tmpchara,*) mod_input, module_name
!
            iflag = 1
            do j = 1, num_exclude
              if(module_name .eq. exclude_list(j)) then
                write(*,*) 'module dependency for ',                    &
     &               trim(exclude_list(j)), ' is excluded'
                iflag = 0
                exit
              end if
            end do
!
            if(iflag .eq. 1) then
              num = num + 1
              read(tmpchara,*) mod_input, mod_name(num)
!            write(*,*) num, trim(tmpchara), ':  ', trim(mod_name(num))
!
              if(num .eq. ntot) call extend_mod_list
            end if
          end if
        end if
      end do
  99  continue
      close(7)
!
      end subroutine const_module_list
!
!   --------------------------------------------------------------------
!
      subroutine write_module_list(objfile, filename,                   &
     &          dirname, iflag_cpp)
!
      character(len=255), intent(in) :: objfile, filename, dirname
      integer, intent(in) ::  iflag_cpp
!
      integer :: i, j, iflag
!
!
      write(id_makefile,'(a,a2)',advance='no') trim(objfile), ': '
!
      write(id_makefile,'(a,a1,a)',advance='no')                        &
     &                  trim(dirname), '/', trim(filename)
!
      do i = 1, num
        iflag = 0
        do j = 1, i-1
          if(mod_name(i) .eq. mod_name(j)) then
            iflag = 1
            exit
          end if
        end do
        if(iflag .eq. 0) write(id_makefile,'(a1,a,a2)',advance='no')    &
     &             ' ', trim(mod_name(i)), '.o'
      end do
      write(id_makefile,'(a1)',advance='no') char(10)
!
      if(iflag_cpp .eq. 1) then
        write(id_makefile,'(a25,a1)',advance='no')                      &
     &         command_cpp, char(10)
      else
        write(id_makefile,'(a28,a1)',advance='no') command, char(10)
      end if
!
      end subroutine write_module_list
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine make_module_dependency(dirname, filename)
!
      character(len=255), intent(in) :: filename
      character(len=255), intent(in) :: dirname
!
      character(len=255) :: objfile
      character(len=255) :: filehead
      character(len=255) :: fileext
!
      integer :: lobj
!
!
      lobj = len(trim(filename))
      write(filehead,'(a)') filename(1:lobj-4)
      write(fileext,'(a)')  filename(lobj-2:lobj)
      write(objfile,'(a,a2)') trim(filehead), '.o'
!
!      write(*,*) 'lobj: ',  lobj
!      write(*,*) 'objfile: ',  trim(objfile)
!      write(*,*) 'filehead: ', trim(filehead)
!      write(*,*) 'filename: ', trim(filename)
!      write(*,*) 'fileext: ', trim(fileext)
      call const_module_list(filename)
!
      if(fileext .eq. 'F90') then
        call write_module_list(objfile, filename, dirname, 1)
      else
        call write_module_list(objfile, filename, dirname, 0)
      end if
!
      end subroutine make_module_dependency
!
!   --------------------------------------------------------------------
!
      end module module_dependency
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      program make_multi_mod_dependency
!
      use module_dependency
!
      implicit none
!
      integer :: icount, num_file, i
      character(len=255) :: fname_makefile
      character(len=255) :: filelist
      character(len=255) :: dirname
      character(len=255) :: tmpchara
      character(len=255), allocatable :: filenames(:)
!
!
      icount = iargc_kemo()
!      write(*,*) 'icount: ',  icount
!
      if(icount .le. 2) then
        write(*,*)                                                      &
     &     'kemo_module_dep MAKEFILE SOURCE_DIRECTORY LIST_OF_SORCE'
        stop
      end if
!
      num_file = icount - 2
      allocate( filenames(num_file) )
!
      call getarg_k(1, fname_makefile)
      call getarg_k(2, dirname)
      do i = 1, num_file
        call getarg_k( (i+2), filenames(i) )
      end do
!
      ntot = 1
      call alloc_mod_list
!
      open(id_makefile, file = fname_makefile)
!
      do
        read (id_makefile,'(a)', end=99, err=98) tmpchara
      end do
!
  99  continue
      backspace(id_makefile)
  98  continue
!
      do i = 1, num_file
        call make_module_dependency(dirname, filenames(i))
      end do
!
      close(id_makefile)
!
      end program make_multi_mod_dependency
