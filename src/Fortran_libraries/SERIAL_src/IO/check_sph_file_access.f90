!>@file   check_sph_file_access.f90
!!@brief  module check_sph_file_access
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Spectr data IO selector
!!
!!@verbatim
!!      subroutine set_sph_mesh_file_fmt_prefix                         &
!!     &         (iflag_fmt, file_head, file_param)
!!        type(field_IO_params), intent(inout) :: file_param
!!
!!      logical function check_exsist_rtp_file(id_rank, file_param)
!!      logical function check_exsist_rj_file(id_rank, file_param)
!!      logical function check_exsist_rtm_file(id_rank, file_param)
!!      logical function check_exsist_rlm_file(id_rank, file_param)
!!        type(field_IO_params), intent(in) :: file_param
!!
!!      logical function check_writable_rtp_file(id_rank, file_param)
!!      logical function check_writable_rj_file(id_rank, file_param)
!!      logical function check_writable_rtm_file(id_rank, file_param)
!!      logical function check_writable_rlm_file(id_rank, file_param)
!!        type(field_IO_params), intent(in) :: file_param
!!@endverbatim
!!
!!@param id_rank    Process ID
!!@param file_name  file name for IO (.gz is appended in this module)
!
      module check_sph_file_access
!
      use m_precision
!
      use m_file_format_switch
      use t_file_IO_parameter
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_sph_mesh_file_fmt_prefix                           &
     &         (iflag_fmt, file_head, file_param)
!
      integer(kind = kint), intent(in) :: iflag_fmt
      character(len=kchara), intent(in) :: file_head
      type(field_IO_params), intent(inout) :: file_param
!
      file_param%iflag_format = iflag_fmt
      write(file_param%file_prefix,'(a)') trim(file_head)
!
      end subroutine set_sph_mesh_file_fmt_prefix
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      logical function check_exsist_rtp_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rtp_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_exsist_rtp_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtp_file
!
!------------------------------------------------------------------
!
      logical function check_exsist_rj_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rj_file_name(file_param%file_prefix,      &
     &               file_param%iflag_format, id_rank)
      check_exsist_rj_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rj_file
!
!------------------------------------------------------------------
!
      logical function check_exsist_rtm_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rtm_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_exsist_rtm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rtm_file
!
!------------------------------------------------------------------
!
      logical function check_exsist_rlm_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rlm_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_exsist_rlm_file = check_file_exist(sph_file_name)
!
      end function check_exsist_rlm_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      logical function check_writable_rtp_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rtp_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_writable_rtp_file                                           &
     &     = check_file_writable(id_rank, sph_file_name)
!
      end function check_writable_rtp_file
!
!------------------------------------------------------------------
!
      logical function check_writable_rj_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rj_file_name(file_param%file_prefix,      &
     &               file_param%iflag_format, id_rank)
      check_writable_rj_file                                            &
     &     = check_file_writable(id_rank, sph_file_name)
!
      end function check_writable_rj_file
!
!------------------------------------------------------------------
!
      logical function check_writable_rtm_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rtm_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_writable_rtm_file                                           &
     &     = check_file_writable(id_rank, sph_file_name)
!
      end function check_writable_rtm_file
!
!------------------------------------------------------------------
!
      logical function check_writable_rlm_file(id_rank, file_param)
!
      use set_parallel_file_name
      use set_mesh_file_names
      use delete_data_files
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) :: file_param
!
      character(len=kchara) :: sph_file_name
!
      sph_file_name = set_sph_rlm_file_name(file_param%file_prefix,     &
     &               file_param%iflag_format, id_rank)
      check_writable_rlm_file                                           &
     &     = check_file_writable(id_rank, sph_file_name)
!
      end function check_writable_rlm_file
!
!------------------------------------------------------------------
!
      end module check_sph_file_access
