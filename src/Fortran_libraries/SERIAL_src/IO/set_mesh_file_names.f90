!>@file   set_mesh_file_names.f90
!!@brief  module set_mesh_file_names
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Set file names for field data file
!!
!!@verbatim
!!      subroutine set_mesh_file_name(file_header, itype_file,          &
!!     &          my_rank, file_name)
!!
!!      subroutine set_sph_rtp_file_name(file_header, itype_file,       &
!!     &          my_rank, file_name)
!!      subroutine set_sph_rtm_file_name(file_header, itype_file,       &
!!     &          my_rank, file_name)
!!      subroutine set_sph_rlm_file_name(file_header, itype_file,       &
!!     &          my_rank, file_name)
!!      subroutine set_sph_rj_file_name(file_header, itype_file,        &
!!     &          my_rank, file_name)
!!
!!      subroutine set_ele_comm_file_name(file_header, itype_file,      &
!!     &          my_rank, file_name)
!!      subroutine set_surf_mesh_file_name(file_header, itype_file,     &
!!     &          my_rank, file_name)
!!      subroutine set_edge_mesh_file_name(file_header, itype_file,     &
!!     &          my_rank, file_name)
!!@endverbatim
!
      module set_mesh_file_names
!
      use m_precision
      use m_constants
!
      use set_parallel_file_name
      use m_file_format_switch
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_mesh_file_name(file_header, itype_file,            &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_gfb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_gfm_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_gfb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_gfm_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_mesh_file_name
!
!------------------------------------------------------------------
!
      subroutine set_sph_rtp_file_name(file_header, itype_file,         &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_btp_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_rtp_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_btp_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_rtp_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_sph_rtp_file_name
!
!------------------------------------------------------------------
!
      subroutine set_sph_rtm_file_name(file_header, itype_file,         &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_btm_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_rtm_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_btm_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_rtm_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_sph_rtm_file_name
!
!------------------------------------------------------------------
!
      subroutine set_sph_rlm_file_name(file_header, itype_file,         &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_blm_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_rlm_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_blm_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_rlm_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_sph_rlm_file_name
!
!------------------------------------------------------------------
!
      subroutine set_sph_rj_file_name(file_header, itype_file,          &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_brj_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_rj_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_brj_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_rj_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_sph_rj_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_ele_comm_file_name(file_header, itype_file,        &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_elb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_gel_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_elb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_gel_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_ele_comm_file_name
!
!------------------------------------------------------------------
!
      subroutine set_surf_mesh_file_name(file_header, itype_file,       &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_sfb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_gsf_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_sfb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_gsf_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_surf_mesh_file_name
!
!------------------------------------------------------------------
!
      subroutine set_edge_mesh_file_name(file_header, itype_file,       &
     &          my_rank, file_name)
!
      integer(kind=kint), intent(in) :: itype_file, my_rank
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara), intent(inout) :: file_name
      character(len=kchara) :: fname_tmp
!
!
      if((itype_file/iflag_single) .eq. 0) then
        call add_int_suffix(my_rank, file_header, file_name)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        call add_edb_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        call add_ged_extension(file_name, fname_tmp)
        call add_gzip_extension(fname_tmp, file_name)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        call add_edb_extension(file_name, fname_tmp)
        file_name = fname_tmp
      else
        call add_ged_extension(file_name, fname_tmp)
        file_name = fname_tmp
      end if
!
      end subroutine set_edge_mesh_file_name
!
!------------------------------------------------------------------
!
      end module set_mesh_file_names
