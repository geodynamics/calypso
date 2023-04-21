!>@file   set_element_mesh_file_names.f90
!!@brief  module set_element_mesh_file_names
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  Add extensions for element, surface, and edge mesh
!!
!!@verbatim
!!      character(len=kchara) function                                  &
!!     &       set_ele_comm_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_surf_mesh_file_name(file_header, itype_file, id_rank)
!!      character(len=kchara) function                                  &
!!     &       set_edge_mesh_file_name(file_header, itype_file, id_rank)
!!@endverbatim
!
      module set_element_mesh_file_names
!
      use m_precision
      use m_constants
!
      use m_file_format_switch
!
      implicit none
!
      character(len=3), parameter, private :: gel_ext = "gel"
      character(len=3), parameter, private :: elb_ext = "elb"
!
      character(len=3), parameter, private :: sfb_ext = "sfb"
      character(len=3), parameter, private :: gsf_ext = "gsf"
!
      character(len=3), parameter, private :: edb_ext = "edb"
      character(len=3), parameter, private :: ged_ext = "ged"
!
      private :: add_sfb_extension
      private :: add_ged_extension, add_edb_extension
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &        set_ele_comm_file_name(file_header, itype_file, id_rank)
!
      use set_parallel_file_name
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
      use set_parallel_file_name
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
      use set_parallel_file_name
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
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_elb_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_elb_extension = add_3chara_extension(file_head, elb_ext)
!
      end function add_elb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gel_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_gel_extension = add_3chara_extension(file_head, gel_ext)
!
      end function add_gel_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_sfb_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_sfb_extension = add_3chara_extension(file_head, sfb_ext)
!
      end function add_sfb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gsf_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_gsf_extension = add_3chara_extension(file_head, gsf_ext)
!
      end function add_gsf_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_edb_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_edb_extension = add_3chara_extension(file_head, edb_ext)
!
      end function add_edb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_ged_extension(file_head)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: file_head
!
      add_ged_extension = add_3chara_extension(file_head, ged_ext)
!
      end function add_ged_extension
!
!-----------------------------------------------------------------------
!
      end module set_element_mesh_file_names
