!>@file   t_const_spherical_grid.f90
!!@brief  module t_const_spherical_grid
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Structure to constract spherical shell mesh
!!
!!@verbatim
!!@endverbatim
!
      module t_const_spherical_grid
!
      use m_precision
!
      use t_spheric_global_ranks
      use t_sph_1d_global_index
      use t_sph_local_parameter
      use t_control_1D_layering
!
      implicit none
!
!
      type construct_spherical_grid
!>        Structure of subdomain information
        type(spheric_global_rank) :: s3d_ranks
!>        Structure of radial information
        type(spheric_global_radius) :: s3d_radius
!>        Structure of number of modes in each domain
        type(sph_local_parameters) :: sph_lcp
!
!>        Structure of 1-D local adresses for spherical shell grid
        type(sph_1d_index_stack) :: stk_lc1d
!>        Structure of 1-D global adresses for spherical shell grid
        type(sph_1d_global_index) :: sph_gl1d
!
!>        Structure of additional radial group
        type(layering_group_list) :: added_radial_grp
!
!>        Structure of radial group for SGS model
        type(layering_group_list) :: r_layer_grp
!>        Structure of meridional group for SGS model
        type(layering_group_list) :: med_layer_grp
      end type construct_spherical_grid
!
!
      end module t_const_spherical_grid
