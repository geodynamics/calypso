!>@file   bcast_control_data_pvrs.f90
!!@brief  module bcast_control_data_pvrs
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!
!>@brief structure of control data for multiple PVRs
!!
!!@verbatim
!!      subroutine bcast_files_4_pvr_ctl(pvr_ctls)
!!        type(volume_rendering_controls), intent(in) :: pvr_ctls
!!@endverbatim
!
      module bcast_control_data_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_pvrs
      use calypso_mpi
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_files_4_pvr_ctl(pvr_ctls)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_data_4_pvr
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      call calypso_mpi_bcast_character(pvr_ctls%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(pvr_ctls%num_pvr_ctl, 0)
      if(pvr_ctls%num_pvr_ctl .le. 0) return
!
      if(my_rank .gt. 0)  call alloc_pvr_ctl_struct(pvr_ctls)
!
      call calypso_mpi_bcast_character(pvr_ctls%fname_pvr_ctl,          &
     &    cast_long(kchara*pvr_ctls%num_pvr_ctl), 0)
!
      end subroutine bcast_files_4_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      end module bcast_control_data_pvrs
