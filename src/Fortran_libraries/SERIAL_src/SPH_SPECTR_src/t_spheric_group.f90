!>@file   t_spheric_group.f90
!!@brief  module t_spheric_group
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure of group data for spherical shell model
!!
!!@verbatim
!!      subroutine dealloc_sph_grid_group(sph_grps)
!!      subroutine dealloc_sph_mode_group(sph_grps)
!!        type(sph_group_data), intent(inout) :: sph_grps
!!@endverbatim
!
      module t_spheric_group
!
      use m_precision
!
      use t_group_data
!
      implicit none
!
!
!> Structure of group data for spherical transform
      type sph_group_data
!>         node group for grid space
        type(group_data) :: bc_rtp_grp
!>         radial group for grid space
        type(group_data) :: radial_rtp_grp
!>         meridional group for grid space
        type(group_data) :: theta_rtp_grp
!>         zonal group for grid space
        type(group_data) :: zonal_rtp_grp
!
!>         radial group for sprctrum space
        type(group_data) :: radial_rj_grp
!>         spherical harmonics group for sprctrum space
        type(group_data) :: sphere_rj_grp
      end type sph_group_data
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_sph_grid_group(sph_grps)
!
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call dealloc_group(sph_grps%bc_rtp_grp)
      call dealloc_group(sph_grps%radial_rtp_grp)
      call dealloc_group(sph_grps%theta_rtp_grp)
      call dealloc_group(sph_grps%zonal_rtp_grp)
!
      end subroutine dealloc_sph_grid_group
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_mode_group(sph_grps)
!
      type(sph_group_data), intent(inout) :: sph_grps
!
!
      call dealloc_group(sph_grps%radial_rj_grp)
      call dealloc_group(sph_grps%sphere_rj_grp)
!
      end subroutine dealloc_sph_mode_group
!
! -----------------------------------------------------------------------
!
      end module t_spheric_group
