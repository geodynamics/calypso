!>@file   set_mesh_extensions.f90
!!@brief  module set_mesh_extensions
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len=kchara) function add_fld_extension(file_header)
!!                put ".fld" at the end
!!      character(len=kchara) function add_flb_extension(file_head)
!!                put ".flb" at the end
!!
!!      character(len=kchara) function add_gfm_extension(file_head)
!!                put ".gfm" at the end
!!      character(len=kchara) function add_gfb_extension(file_head)
!!                put ".gfb" at the end
!!
!!      character(len=kchara) function add_ksm_extension(file_head)
!!                put ".ksm" at the end
!!
!!      character(len=kchara) function add_gel_extension(file_head)
!!                put ".gel" at the end
!!      character(len=kchara) function add_gsf_extension(file_head)
!!                put ".gsf" at the end
!!      character(len=kchara) function add_ged_extension(file_head)
!!                put ".ged" at the end
!!      character(len=kchara) function add_elb_extension(file_head)
!!                put ".elb" at the end
!!      character(len=kchara) function add_sfb_extension(file_head)
!!                put ".sfb" at the end
!!      character(len=kchara) function add_edb_extension(file_head)
!!                put ".edb" at the end
!!@endverbatim
!!
!!@n @param dir_file_name    file name (header) including directory name
!!@n @param int_id           integer to be added at the end of prefix
!!@n @param int_val          integer to be tranfered to character
!!@n @param int_string       output character
!
      module set_mesh_extensions
!
      use m_precision
      use set_parallel_file_name
!
      implicit  none
!
      character(len=3), parameter, private :: fld_ext = "fld"
      character(len=3), parameter, private :: flb_ext = "flb"
!
      character(len=3), parameter, private :: gfm_ext = "gfm"
      character(len=3), parameter, private :: gfb_ext = "gfb"
      character(len=3), parameter, private :: ksm_ext = "ksm"
      character(len=3), parameter, private :: gel_ext = "gel"
      character(len=3), parameter, private :: gsf_ext = "gsf"
      character(len=3), parameter, private :: ged_ext = "ged"
      character(len=3), parameter, private :: elb_ext = "elb"
      character(len=3), parameter, private :: sfb_ext = "sfb"
      character(len=3), parameter, private :: edb_ext = "edb"
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_fld_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_fld_extension = add_3chara_extension(file_head, fld_ext)
!
      end function add_fld_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_flb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_flb_extension = add_3chara_extension(file_head, flb_ext)
!
      end function add_flb_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gfm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_gfm_extension = add_3chara_extension(file_head, gfm_ext)
!
      end function add_gfm_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gfb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_gfb_extension = add_3chara_extension(file_head, gfb_ext)
!
      end function add_gfb_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_ksm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_ksm_extension = add_3chara_extension(file_head, ksm_ext)
!
      end function add_ksm_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gel_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_gel_extension = add_3chara_extension(file_head, gel_ext)
!
      end function add_gel_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_gsf_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_gsf_extension = add_3chara_extension(file_head, gsf_ext)
!
      end function add_gsf_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_ged_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_ged_extension = add_3chara_extension(file_head, ged_ext)
!
      end function add_ged_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_elb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_elb_extension = add_3chara_extension(file_head, elb_ext)
!
      end function add_elb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_sfb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_sfb_extension = add_3chara_extension(file_head, sfb_ext)
!
      end function add_sfb_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_edb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_edb_extension = add_3chara_extension(file_head, edb_ext)
!
      end function add_edb_extension
!
!-----------------------------------------------------------------------
!
      end module set_mesh_extensions
