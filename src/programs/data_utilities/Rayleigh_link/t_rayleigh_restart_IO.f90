!>@file   t_rayleigh_restart_IO.f90
!!@brief  module t_rayleigh_restart_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Structure of Rayleigh paramters
!!
!!@verbatim
!!      subroutine alloc_rayleigh_radial_grid(ra_rst)
!!      subroutine dealloc_rayleigh_radial_grid(ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      subroutine check_rayleigh_rst_params(id_file, ra_rst)
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!
!!      subroutine read_rayleigh_restart_params(dir, i_step, ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      character(len = kchara) function set_rayleigh_file_name         &
!!     &                               (dir, int_id, postfix)
!!          Version 1.x:  "[dir]/[int_id]/[field_flag]"
!!@endverbatim
!
      module t_rayleigh_restart_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use t_field_data_IO
      use set_parallel_file_name
!
      implicit  none
!
      character(len = kchara), parameter :: paramchar = "grid_etc"
!
      character(len = kchara), parameter :: wchar = "W"
      character(len = kchara), parameter :: zchar = "Z"
      character(len = kchara), parameter :: pchar = "P"
      character(len = kchara), parameter :: tchar = "T"
!
      character(len = kchara), parameter :: cchar = "C"
      character(len = kchara), parameter :: achar = "A"
!
      character(len = kchara), parameter :: wabchar = "WAB"
      character(len = kchara), parameter :: zabchar = "ZAB"
      character(len = kchara), parameter :: pabchar = "PAB"
      character(len = kchara), parameter :: tabchar = "TAB"
!
      character(len = kchara), parameter :: cabchar = "CAB"
      character(len = kchara), parameter :: aabchar = "AAB"
!
!>      Structure for Rayleigh restart data
      type rayleigh_restart
!>        Version ID
        integer :: i_version = 0
!>        Endian swap flag
        integer :: iflag_swap = 0
!
!>        truncation degree
        integer :: i_version_from_file
!>        truncation degree
        integer(kind = kint) :: ltr_org
!
!>        Radial grid type
        integer(kind = kint) :: iflag_rtype
!>        Number of radial grid
        integer(kind = kint) :: nri_org
!>        radial
        real(kind = kreal), allocatable :: r_org(:)
!
!>        forward transform matrix
        real(kind = kreal), allocatable :: Cheby_fwd(:,:)
!
!>        Original delta t
        real(kind = kreal) :: dt_org
!>        Original time
        real(kind = kreal) :: time_org
!>        new delta t
        real(kind = kreal) :: dt_new
!>        new original delta t
        real(kind = kreal) :: new_dt_org
!>        time step
        integer(kind = kint) :: i_step_org
      end type rayleigh_restart
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_radial_grid(ra_rst)
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      allocate(ra_rst%r_org(ra_rst%nri_org))
      if(ra_rst%nri_org .gt. 0) ra_rst%r_org = 0.0d0
!
      end subroutine alloc_rayleigh_radial_grid
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_radial_grid(ra_rst)
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      deallocate(ra_rst%r_org)
!
      end subroutine dealloc_rayleigh_radial_grid
!
!-----------------------------------------------------------------------
!
      subroutine check_rayleigh_rst_params(id_file, ra_rst)
!
      integer, intent(in) :: id_file
      type(rayleigh_restart), intent(in) :: ra_rst
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) 'iflag_swap', ra_rst%iflag_swap
      write(id_file,*) 'version ID', ra_rst%i_version_from_file
      write(id_file,*) 'ltr_org',  ra_rst%ltr_org
!
      write(id_file,*) 'iflag_rtype',  ra_rst%iflag_rtype
      write(id_file,*) 'nri_org',  ra_rst%nri_org
      do i = 1,  ra_rst%nri_org
        write(id_file,*) i,  ra_rst%r_org(i)
      end do
!
      write(id_file,*) 'time_org', ra_rst%time_org
      write(id_file,*) 'dt_org',  ra_rst%dt_org
      write(id_file,*) 'dt_new',  ra_rst%dt_new,  ra_rst%new_dt_org
!
      end subroutine check_rayleigh_rst_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_restart_params(dir, i_step, ra_rst)
!
      use binary_IO
      use t_binary_IO_buffer
      use binary_file_access
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      type(binary_IO_buffer) :: bbuf_rgh
      character(len = kchara) :: file_name
      integer(kind = kint) :: ilength
      integer :: int_tmp(1)
      real(kind = kreal) :: rtmp(1)
!
      integer, parameter :: iflag_pi = 314
!
      write(*,*) 'i_step', i_step
      file_name =  set_rayleigh_file_name(dir, i_step, paramchar)
      file_name =  add_null_character(file_name)
      write(*,*) 'read Rayleigh checkpoint paramter file: ',            &
     &          trim(file_name)
      call open_rd_rawfile_f(file_name, bbuf_rgh)
!
      bbuf_rgh%iflag_swap = iendian_KEEP
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      if(int_tmp(1) .ne. iflag_pi) bbuf_rgh%iflag_swap = iendian_FLIP
      ra_rst%iflag_swap = bbuf_rgh%iflag_swap
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%i_version_from_file = int(int_tmp(1),KIND(ra_rst%nri_org))
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%nri_org = int(int_tmp(1),KIND(ra_rst%nri_org))
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%iflag_rtype = int(int_tmp(1),KIND(ra_rst%nri_org))
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%ltr_org = int(int_tmp(1),KIND(ra_rst%nri_org))
!
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%dt_org = rtmp(1)
      write(*,*) 'ra_rst%dt_org', ra_rst%dt_org
!
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%dt_new = rtmp(1)
      write(*,*) 'ra_rst%dt_new', ra_rst%dt_new
!
!      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!      call rawread_real_f(1, rtmp, bbuf_rgh)
!      ra_rst%new_dt_org = rtmp(1)
!      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call alloc_rayleigh_radial_grid(ra_rst)
!
      ilength =  int(ra_rst%nri_org)
      call rawread_real_f(ilength, ra_rst%r_org(1), bbuf_rgh)
!
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%time_org = rtmp(1)
      write(*,*) 'ra_rst%time_org', ra_rst%time_org
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%i_step_org = int(int_tmp(1),KIND(ra_rst%nri_org))
      write(*,*) 'ra_rst%i_step', ra_rst%i_step_org
!
      call close_binary_file
!
      end subroutine read_rayleigh_restart_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh_file_name           &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh_file_name,1000)                                &
     &                        trim(dir), int_id, trim(postfix)
 1000 format(a, '/', i8.8, '/', a)
!
      end function set_rayleigh_file_name
!
!-----------------------------------------------------------------------
!
      end module t_rayleigh_restart_IO
