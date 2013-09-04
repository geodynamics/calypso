!>@file   t_spheric_mesh.f90
!!@brief  module t_spheric_mesh
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!
      module t_spheric_mesh
!
      use m_precision
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
!
      implicit none
!
!
!> Structure for grid and comm table for spherical transform
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
      type sph_mesh_data
!>         spherical harmonics indexing data
        type(sph_grids) ::       sph_mesh
!>         communication tables for spherical transform
        type(sph_comm_tables) :: sph_comms
!>         grouping data for harmonics indices
        type(sph_group_data) ::  sph_grps
      end type sph_mesh_data
!
!
      end module t_spheric_mesh
