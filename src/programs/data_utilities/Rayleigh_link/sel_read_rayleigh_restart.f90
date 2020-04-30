!>@file   sel_read_rayleigh_restart.f90
!!@brief  module sel_read_rayleigh_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine sel_read_rayleigh_rst_params(dir, i_step, ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      subroutine set_rayleigh_rst_file_name(i_version, dir, i_step,   &
!!     &          field_name, iflag_ncomp, file_name)
!!      character(len = kchara) function sel_rayleigh_file_name         &
!!     &                               (i_version, dir, int_id, postfix)
!!          Version 0.99: "[dir]/[int_id]_[field_flag]"
!!          Version 1.x:  "[dir]/[int_id]/[field_flag]"
!!@endverbatim
!
      module sel_read_rayleigh_restart
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_file_format_switch
      use t_rayleigh_restart_IO
!
      implicit  none
!
      private :: read_rayleigh99_restart_params
      private :: set_rayleigh99_file_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_rayleigh_rst_params(dir, i_step, ra_rst)
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
!
      if(ra_rst%i_version .eq. id_Rayleigh99) then
        call read_rayleigh99_restart_params(dir, i_step, ra_rst)
      else
        call read_rayleigh_restart_params(dir, i_step, ra_rst)
      end if
!
      end subroutine sel_read_rayleigh_rst_params
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function sel_rayleigh_file_name           &
     &                               (i_version, dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: i_version, int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      if(i_version .eq. id_Rayleigh99) then
        sel_rayleigh_file_name                                          &
     &      = set_rayleigh99_file_name(dir, int_id, postfix)
      else
        sel_rayleigh_file_name                                          &
     &      = set_rayleigh_file_name(dir, int_id, postfix)
      end if
!
      end function sel_rayleigh_file_name
!
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_rst_file_name(i_version, dir, i_step,     &
     &          field_name, iflag_ncomp, file_name)
!
      use m_phys_labels
      use t_field_data_IO
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: field_name
!
      integer(kind = kint), intent(inout) :: iflag_ncomp
      character(len = kchara), intent(inout) :: file_name(2)
!
      if(field_name .eq. velocity%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         wchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         zchar)
      else if(field_name .eq. pressure%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         pchar)
      else if(field_name .eq. temperature%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         tchar)
      else if(field_name .eq. magnetic_field%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         cchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         achar)
!
      else if(field_name .eq. previous_momentum%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         wabchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         zabchar)
!      else if(field_name .eq. previous_pressure%name) then
!        iflag_ncomp = 1
!        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step, &
!     &                                         cchar)
      else if(field_name .eq. previous_heat%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         tabchar)
      else if(field_name .eq. previous_induction%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         cabchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         aabchar)
      end if
!
      end subroutine set_rayleigh_rst_file_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh99_restart_params(dir, i_step, ra_rst)
!
      use binary_IO
      use t_binary_IO_buffer
      use binary_file_access
      use set_parallel_file_name
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
!
      write(*,*) 'i_step', i_step
      file_name =  set_rayleigh99_file_name(dir, i_step, paramchar)
      file_name =  add_null_character(file_name)
      write(*,*) 'read Rayleigh checkpoint paramter file: ',            &
     &          trim(file_name)
      call open_rd_rawfile_f(file_name, bbuf_rgh)
!
      bbuf_rgh%iflag_swap = iendian_KEEP
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      if(int_tmp(1) .ne. 4) bbuf_rgh%iflag_swap = iendian_FLIP
      ra_rst%iflag_swap = bbuf_rgh%iflag_swap
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%nri_org = int(int_tmp(1),KIND(ra_rst%nri_org))
      call rawread_int4_f(kint, int_tmp, bbuf_rgh)
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%iflag_rtype = int(int_tmp(1),KIND(ra_rst%nri_org))
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      ra_rst%ltr_org = int(int_tmp(1),KIND(ra_rst%nri_org))
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%dt_org = rtmp(1)
      write(*,*) 'ra_rst%dt_org', ra_rst%dt_org
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%dt_new = rtmp(1)
      write(*,*) 'ra_rst%dt_new', ra_rst%dt_new
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
!      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!      call rawread_real_f(1, rtmp, bbuf_rgh)
!      ra_rst%new_dt_org = rtmp(1)
!      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call alloc_rayleigh_radial_grid(ra_rst)
!
      ilength =  int(ra_rst%nri_org)
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_real_f(ilength, ra_rst%r_org(1), bbuf_rgh)
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
      call rawread_real_f(1, rtmp, bbuf_rgh)
      ra_rst%time_org = rtmp(1)
      write(*,*) 'ra_rst%time_org', ra_rst%time_org
      call rawread_int4_f(1, int_tmp, bbuf_rgh)
!
      call close_binary_file
!
      end subroutine read_rayleigh99_restart_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh99_file_name         &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh99_file_name,1000)                              &
     &                        trim(dir), int_id, trim(postfix)
 1000 format(a, '/', i8.8, '_', a)
!
      end function set_rayleigh99_file_name
!
!-----------------------------------------------------------------------
!
      end module sel_read_rayleigh_restart
