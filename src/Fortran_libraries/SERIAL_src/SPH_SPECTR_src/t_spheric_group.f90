!>@file   t_spheric_group.f90
!!@brief  module t_spheric_group
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure of group data for spherical shell model
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
!
      end module t_spheric_group
