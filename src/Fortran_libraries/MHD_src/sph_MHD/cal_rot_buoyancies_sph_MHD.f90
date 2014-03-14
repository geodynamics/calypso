!>@file   cal_rot_buoyancies_sph_MHD.f90
!!@brief  module cal_rot_buoyancies_sph_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy
!!
!!@verbatim
!!      subroutine cal_rot_radial_self_gravity(sph_bc_U)
!!      subroutine cal_boussinesq_density_sph(kr_in, kr_out)
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!!@param kr_in     Radial ID for inner boundary
!!@param kr_out    Radial ID for outer boundary
!
      module cal_rot_buoyancies_sph_MHD
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
      private :: cal_rot_double_buoyancy_sph_MHD
      private :: cal_rot_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_radial_self_gravity(sph_bc_U)
!
      use m_machine_parameter
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1)                                           &
     &      write(*,*)'cal_rot_double_buoyancy_sph_MHD', ipol%i_temp
          call cal_rot_double_buoyancy_sph_MHD                          &
     &       (sph_bc_U%kr_in, sph_bc_U%kr_out, coef_buo, ipol%i_temp,   &
     &        coef_comp_buo, ipol%i_light, itor%i_rot_buoyancy)
!
      else if ( iflag_4_gravity .gt. id_turn_OFF) then
!
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD', ipol%i_temp
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      coef_buo, ipol%i_temp, itor%i_rot_buoyancy)
!
      else if ( iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD', ipol%i_light
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      coef_comp_buo, ipol%i_light, itor%i_rot_comp_buo)
!
      else if (iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'cal_rot_buoyancy_sph_MHD', ipol%i_filter_temp
        call cal_rot_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out,  &
     &      coef_buo, ipol%i_filter_temp, itor%i_rot_filter_buo)
      end if
!
      end subroutine cal_rot_radial_self_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_rot_double_buoyancy_sph_MHD(kr_in, kr_out,         &
     &          coef_t_buo, is_t, coef_c_buo, is_c, it_res)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_t, is_c
      integer(kind= kint), intent(in) :: it_res
      real(kind = kreal), intent(in) :: coef_t_buo, coef_c_buo
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
     &                      * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_rot_buoyancy_sph_MHD(kr_in, kr_out, coef, is_fld,  &
     &          it_res)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      integer(kind= kint), intent(in) :: is_fld, it_res
      real(kind = kreal), intent(in) :: coef
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp parallel do private (inod,j,k)
      do inod = ist, ied
        j = mod((inod-1),nidx_rj(2)) + 1
        k = 1 + (inod- j) / nidx_rj(2)
        d_rj(inod,it_res)                                               &
     &          =  coef * d_rj(inod,is_fld) * radius_1d_rj_r(k)
      end do
!$omp end parallel do
!
      end subroutine cal_rot_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_boussinesq_density_sph(kr_in, kr_out)
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
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
        d_rj(inod,ipol%i_density)                                       &
     &          = -(d_rj(inod,ipol%i_temp)                              &
     &           + coef_comp_buo/coef_buo* d_rj(inod,ipol%i_light)  )
      end do
!$omp end parallel do
!
      end subroutine cal_boussinesq_density_sph
!
!-----------------------------------------------------------------------
!
      end module cal_rot_buoyancies_sph_MHD
