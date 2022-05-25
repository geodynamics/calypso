!>@file   cal_self_buoyancies_sph.f90
!!@brief  module cal_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy
!!
!!@verbatim
!!      subroutine cal_radial_self_gravity(sph_bc_U)
!!      subroutine cal_double_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj, &
!!     &          coef_t_buo, is_t, coef_c_buo, is_c, it_res)
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!!@param kr_in     Radial ID for inner boundary
!!@param kr_out    Radial ID for outer boundary
!
      module cal_self_buoyancies_sph
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit  none
!
      private :: cal_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_radial_self_gravity(sph_bc_U)
!
      use m_machine_parameter
      use m_schmidt_poly_on_rtm
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
!
      if(ipol%i_buoyancy .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_buoyancy_sph_MHD', ipol%i_temp
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      g_sph_rj, coef_buo, ipol%i_temp, ipol%i_buoyancy)
      end if
!
      if(ipol%i_comp_buo .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_buoyancy_sph_MHD', ipol%i_light
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      g_sph_rj, coef_comp_buo, ipol%i_light, ipol%i_comp_buo)
      end if
!
      if(ipol%i_filter_buo .gt. 0) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_buoyancy_sph_MHD', ipol%i_filter_temp
        call cal_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &      g_sph_rj, coef_buo, ipol%i_filter_buo,                      &
     &      ipol%i_rot_filter_buo)
      end if
!
      end subroutine cal_radial_self_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_double_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj,   &
     &          coef_t_buo, is_t, coef_c_buo, is_c, it_res)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: it_res
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
!
        d_rj(inod,it_res) =  ( coef_t_buo * d_rj(inod,is_t)             &
     &                       + coef_c_buo * d_rj(inod,is_c)  )          &
     &                       * g_sph_rj(j,13) * radius_1d_rj_r(k)**3
      end do
!$omp end parallel do
!
      end subroutine cal_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_sph_MHD(kr_in, kr_out, g_sph_rj, coef,    &
     &                                is_fld, is_buo)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, is_buo
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: g_sph_rj(nidx_rj(2),17)
!
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,is_buo) = coef * g_sph_rj(j,13)                       &
     &                      * d_rj(inod,is_fld) * radius_1d_rj_r(k)**3
      end do
!$omp end parallel do
!
      end subroutine cal_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_self_buoyancies_sph
