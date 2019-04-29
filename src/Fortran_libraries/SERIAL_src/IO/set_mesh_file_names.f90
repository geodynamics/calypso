!>@file   set_mesh_file_names.f90
!!@brief  module set_mesh_file_names
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Set file names for field data file
!!
!!@verbatim
!!      character(len=kchara) function                                  &
!!     &       set_mesh_file_name(file_header, itype_file, id_rank)
!!
!!      character(len=kchara) function                                  &
!!     &       set_sph_rtp_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_sph_rtm_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_sph_rlm_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_sph_rj_file_name(file_header, itype_file, id_rank)
!!
!!      character(len=kchara) function                                  &
!!     &       set_ele_comm_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_surf_mesh_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_edge_mesh_file_name(file_header, itype_file, id_rank)
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
      character(len=kchara) function                                    &
     &       set_mesh_file_name(file_header, itype_file, id_rank)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_gfb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_gfm_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_gfb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_gfm_extension(file_name)
        file_name = fname_tmp
      end if
      set_mesh_file_name = file_name
!
      end function set_mesh_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_sph_rtp_file_name(file_header, itype_file, id_rank)
!
      use set_sph_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_btp_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_rtp_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_btp_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_rtp_extension(file_name)
        file_name = fname_tmp
      end if
      set_sph_rtp_file_name = file_name
!
      end function set_sph_rtp_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_sph_rtm_file_name(file_header, itype_file, id_rank)
!
      use set_sph_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_btm_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_rtm_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_btm_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_rtm_extension(file_name)
        file_name = fname_tmp
      end if
      set_sph_rtm_file_name = file_name
!
      end function set_sph_rtm_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_sph_rlm_file_name(file_header, itype_file, id_rank)
!
      use set_sph_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_blm_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_rlm_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_blm_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_rlm_extension(file_name)
        file_name = fname_tmp
      end if
      set_sph_rlm_file_name = file_name
!
      end function set_sph_rlm_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_sph_rj_file_name(file_header, itype_file, id_rank)
!
      use set_sph_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_brj_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_rj_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_brj_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_rj_extension(file_name)
        file_name = fname_tmp
      end if
      set_sph_rj_file_name = file_name
!
      end function set_sph_rj_file_name
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &        set_ele_comm_file_name(file_header, itype_file, id_rank)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_elb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_gel_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_elb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp =  add_gel_extension(file_name)
        file_name = fname_tmp
      end if
      set_ele_comm_file_name = file_name
!
      end function set_ele_comm_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_surf_mesh_file_name(file_header, itype_file, id_rank)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_sfb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_gsf_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_sfb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_gsf_extension(file_name)
        file_name = fname_tmp
      end if
      set_surf_mesh_file_name = file_name
!
      end function set_surf_mesh_file_name
!
!------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &       set_edge_mesh_file_name(file_header, itype_file, id_rank)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      integer(kind=kint), intent(in) :: itype_file
      character(len=kchara), intent(in) ::    file_header
      character(len=kchara) :: fname_tmp, file_name
!
!
      if((itype_file/iflag_single) .eq. 0) then
        file_name = add_process_id(id_rank, file_header)
      else
        file_name = file_header
      end if
!
      if     (mod(itype_file,iten) .eq. id_gzip_bin_file_fmt) then
        fname_tmp = add_edb_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_gzip_txt_file_fmt) then
        fname_tmp = add_ged_extension(file_name)
        file_name = add_gzip_extension(fname_tmp)
      else if(mod(itype_file,iten) .eq. id_binary_file_fmt) then
        fname_tmp = add_edb_extension(file_name)
        file_name = fname_tmp
      else
        fname_tmp = add_ged_extension(file_name)
        file_name = fname_tmp
      end if
      set_edge_mesh_file_name = file_name
!
      end function set_edge_mesh_file_name
!
!------------------------------------------------------------------
!
      end module set_mesh_file_names
