!>@file   t_const_spherical_grid.f90
!!@brief  module t_const_spherical_grid
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Structure to constract spherical shell mesh
!!
!!@verbatim
!!      subroutine dealloc_gen_mesh_params(gen_sph)
!!      subroutine dealloc_gen_sph_fem_mesh_param(gen_sph)
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
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
      use t_group_data
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
!
!>        Structure of radial group data in grid space
        type(group_data) :: radial_rtp_grp_lc
!>        Structure of meridionala group data in grid space
        type(group_data) :: theta_rtp_grp_lc
!>        Structure of radial group data in sprectrum space
        type(group_data) :: radial_rj_grp_lc
!
!>        Sleeve leavel of FEM mesh
        integer(kind = kint) :: num_FEM_sleeve = 1
      end type construct_spherical_grid
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_gen_mesh_params(gen_sph)
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      call dealloc_sph_ranks(gen_sph%s3d_ranks)
      call dealloc_sph_1d_domain_id(gen_sph%s3d_ranks)
!
      call dealloc_sph_gl_parameter(gen_sph%sph_lcp)
!
      call dealloc_sph_1d_global_idx(gen_sph%sph_gl1d)
      call dealloc_sph_1d_global_stack(gen_sph%stk_lc1d)
!
      call dealloc_radius_1d_gl(gen_sph%s3d_radius)
!
      call dealloc_layering_group(gen_sph%med_layer_grp)
      call dealloc_layering_group(gen_sph%r_layer_grp)
      call dealloc_layering_group(gen_sph%added_radial_grp)
!
      end subroutine dealloc_gen_mesh_params
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gen_sph_fem_mesh_param(gen_sph)
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
     call dealloc_group(gen_sph%radial_rj_grp_lc)
     call dealloc_group(gen_sph%radial_rtp_grp_lc)
     call dealloc_group(gen_sph%theta_rtp_grp_lc)
!
      end subroutine dealloc_gen_sph_fem_mesh_param
!
! -----------------------------------------------------------------------
!
      end module t_const_spherical_grid
