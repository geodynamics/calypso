!>@file   set_sph_extensions.f90
!!@brief  module set_sph_extensions
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len=kchara) function add_fst_extension(file_head)
!!                put ".fst" at the end
!!      character(len=kchara) function add_fsb_extension(file_head)
!!                put ".fsb" at the end
!!
!!      character(len=kchara) function add_rtp_extension(file_head)
!!                put ".rtp" at the end
!!      character(len=kchara) function add_rtm_extension(file_head)
!!                put ".rtm" at the end
!!      character(len=kchara) function add_rlm_extension(file_head)
!!                put ".rlm" at the end
!!      character(len=kchara) function add_rj_extension(file_head)
!!                put ".rj" at the end
!!
!!      character(len=kchara) function add_btp_extension(file_head)
!!                put ".btp" at the end
!!      character(len=kchara) function add_btm_extension(file_head)
!!                put ".btm" at the end
!!      character(len=kchara) function add_blm_extension(file_head)
!!                put ".blm" at the end
!!      character(len=kchara) function add_brj_extension(file_head)
!!                put ".brj" at the end
!!@endverbatim
!!
!!@n @param dir_file_name    file name (header) including directory name
!!@n @param int_id           integer to be added at the end of prefix
!!@n @param int_val          integer to be tranfered to character
!!@n @param int_string       output character
!
      module set_sph_extensions
!
      use m_precision
      use set_parallel_file_name
!
      implicit  none
!
      character(len=3), parameter, private :: fst_ext = "fst"
      character(len=3), parameter, private :: fsb_ext = "fsb"
!
      character(len=3), parameter, private :: rtp_ext = "rtp"
      character(len=3), parameter, private :: rtm_ext = "rtm"
      character(len=3), parameter, private :: rlm_ext = "rlm"
      character(len=2), parameter, private :: rj_ext =  "rj"
      character(len=3), parameter, private :: btp_ext = "btp"
      character(len=3), parameter, private :: btm_ext = "btm"
      character(len=3), parameter, private :: blm_ext = "blm"
      character(len=3), parameter, private :: brj_ext = "brj"
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_fst_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_fst_extension = add_3chara_extension(file_head, fst_ext)
!
      end function add_fst_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_fsb_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_fsb_extension = add_3chara_extension(file_head, fsb_ext)
!
      end function add_fsb_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_rtp_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_rtp_extension = add_3chara_extension(file_head, rtp_ext)
!
      end function add_rtp_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_rtm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_rtm_extension = add_3chara_extension(file_head, rtm_ext)
!
      end function add_rtm_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_rlm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_rlm_extension = add_3chara_extension(file_head, rlm_ext)
!
      end function add_rlm_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_rj_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_rj_extension = trim(add_2chara_extension(file_head, rj_ext))
!
      end function add_rj_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_btp_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_btp_extension = add_3chara_extension(file_head, btp_ext)
!
      end function add_btp_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_btm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_btm_extension = add_3chara_extension(file_head, btm_ext)
!
      end function add_btm_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_blm_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_blm_extension = add_3chara_extension(file_head, blm_ext)
!
      end function add_blm_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function add_brj_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_brj_extension = add_3chara_extension(file_head, brj_ext)
!
      end function add_brj_extension
!
!-----------------------------------------------------------------------
!
      end module set_sph_extensions
