!>@file   initial_magne_dynamobench.f90
!!@brief  module initial_magne_dynamobench
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial magnetic field for 
!!        pseudo vacuume boundary banchmark
!!
!!@verbatim
!!      subroutine initial_magne_sph_dbench_case1                       &
!!     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_sph_dbench_case2                       &
!!     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_sph_dbench_qcv                         &
!!     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_B
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!!@endverbatim
!
      module initial_magne_dynamobench
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_case1                         &
     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!
      use initial_magne_sph_vector
      use initial_magne_sph_mhd
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
!!!!!     Clear magnetic field and current density
      call reset_initial_sph_vector(n_point, d_rj_magne)
      call reset_initial_sph_vector(n_point, d_rj_current)
!
!!!!!     Y_{1}^{0} component of poloidal magnetic field
      call initial_magne_shell_dipole(sph, sph_bc_B, ione, izero,       &
     &    n_point, d_rj_magne, d_rj_current)
!
!!!!!     Y_{2}^{0} component of toroidal magnetic field
      call initial_magne_shell_toroidal(sph, sph_bc_B, itwo, izero,     &
     &    n_point, d_rj_magne, d_rj_current)
!
      end subroutine initial_magne_sph_dbench_case1
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_case2                         &
     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!
      use initial_magne_sph_vector
      use initial_magne_sph_mhd
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
!!!!!     Clear magnetic field and current density
      call reset_initial_sph_vector(n_point, d_rj_magne)
      call reset_initial_sph_vector(n_point, d_rj_current)
!
!!!!!     Y_{1}^{0} component of poloidal magnetic field
      call initial_magne_sphere_dipole(sph, sph_bc_B, ione, izero,      &
     &    n_point, d_rj_magne, d_rj_current)
!
!!!!!     Y_{2}^{0} component of toroidal magnetic field
      call initial_magne_sphere_toroidal(sph, sph_bc_B, itwo, izero,    &
     &    n_point, d_rj_magne, d_rj_current)
!
      end subroutine initial_magne_sph_dbench_case2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine initial_magne_sph_dbench_qcv                           &
     &         (sph, sph_bc_B, n_point, d_rj_magne, d_rj_current)
!
      use initial_magne_sph_vector
      use initial_magne_sph_mhd
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
!!!!!     Clear magnetic field and current density
      call reset_initial_sph_vector(n_point, d_rj_magne)
      call reset_initial_sph_vector(n_point, d_rj_current)
!
!!!!!     Y_{1}^{0} component of poloidal magnetic field
      call initial_magne_qvc_dipole(sph, sph_bc_B, ione, izero,         &
     &    n_point, d_rj_magne, d_rj_current)
!
!!!!!     Y_{2}^{0} component of toroidal magnetic field
      call initial_magne_qvc_toroidal(sph, sph_bc_B, itwo, izero,       &
     &    n_point, d_rj_magne, d_rj_current)
!
      end subroutine initial_magne_sph_dbench_qcv
!
!-----------------------------------------------------------------------
!
      end module initial_magne_dynamobench
