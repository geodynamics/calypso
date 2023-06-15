      program psf_file_to_VTK
!
      use m_precision
      use m_constants
      use m_field_file_format
      use m_section_file_extensions
!
      use t_file_IO_parameter
      use t_ucd_data
      use ucd_IO_select
!
      implicit none
! 
      integer(kind = kint) :: istep, istart, iend, increment
      integer(kind = kint) :: ifmt_input = iflag_vtk
      integer :: np_ucd
      type(field_IO_params) :: ucd_param
      type(time_data), save :: t_IO
      type(ucd_data), save :: ucd
!
      character(len = kchara) :: file_ext, tmpchara
!
      if(iargc_kemo() .lt. 5) then
        write(*,*) 'section_to_vtk ',                                   &
     &    'FILE_PREFIX EXTENSION_WITH_GZ START_STEP END_STEP INCREMENT'
        stop
      end if
      call getarg_k(1, ucd_param%file_prefix)
      call getarg_k(2, file_ext)
!
      call getarg_k(3, tmpchara)
      read(tmpchara,*) istart
      call getarg_k(4, tmpchara)
      read(tmpchara,*) iend
      call getarg_k(5, tmpchara)
      read(tmpchara,*) increment
!
      ifmt_input = psf_to_vtk_format_id_from_input(file_ext)
      if(ifmt_input .eq. iflag_vtk) then
        write(*,*) 'Set correct file extension from the followings'
        write(*,*)  psf_to_vtk_format_list()
        stop 'Convert failed'
      end if
!
      ucd_param%iflag_IO = 0
      do istep = istart, iend, increment
        ucd_param%iflag_format = ifmt_input
        call sel_read_alloc_ucd_file(-1, np_ucd, istep,                 &
     &                               ucd_param, t_IO, ucd)
!
        ucd_param%iflag_format = iflag_vtk
        call sel_write_ucd_file(-1, istep, ucd_param, t_IO, ucd)
        call deallocate_ucd_mesh(ucd)
      end do
!
      stop 'Convert finished'
!
!   --------------------------------------------------------------------
!
      contains
!
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
!
      end program psf_file_to_VTK
