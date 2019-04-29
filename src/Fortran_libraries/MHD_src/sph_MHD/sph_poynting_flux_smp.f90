!>@file   sph_poynting_flux_smp.f90
!!@brief  module sph_poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux for nodal field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine copy_vectors_rtp_4_grad                              &
!!     &         (sph, b_trns, fn_trns, trns_b_MHD, trns_f_ngSGS)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(sph_boundary_type), intent(in)  :: sph_bc_U
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine copy_vect_to_grad_vect_rtp                           &
!!     &         (sph_rtp, ib_vect, if_grad_vx, if_grad_vy, if_grad_vz, &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!      subroutine sel_scalar_from_trans(sph_rtp, v_rtp, d_sph)
!!@endverbatim
!
      module sph_poynting_flux_smp
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
!
      private :: copy_grad_vect_to_m_stretch
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_vectors_rtp_4_grad                                &
     &         (sph, b_trns, fn_trns, trns_b_MHD, trns_f_ngSGS)
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: b_trns, fn_trns
!
      type(address_each_sph_trans), intent(in) :: trns_b_MHD
      type(address_each_sph_trans), intent(inout) :: trns_f_ngSGS
!
!
      if(b_trns%i_velo .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp, b_trns%i_velo,     &
     &      fn_trns%i_grad_vx, fn_trns%i_grad_vy, fn_trns%i_grad_vz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_vort .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp, b_trns%i_vort,     &
     &      fn_trns%i_grad_wx, fn_trns%i_grad_wy, fn_trns%i_grad_wz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_vecp .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp, b_trns%i_vecp,     &
     &      fn_trns%i_grad_ax, fn_trns%i_grad_ay, fn_trns%i_grad_az,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_magne .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp, b_trns%i_magne,    &
     &      fn_trns%i_grad_bx, fn_trns%i_grad_by, fn_trns%i_grad_bz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns%i_current .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp, b_trns%i_current,  &
     &      fn_trns%i_grad_jx, fn_trns%i_grad_jy, fn_trns%i_grad_jz,    &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
!
      end subroutine copy_vectors_rtp_4_grad
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_vect_to_grad_vect_rtp                             &
     &         (sph_rtp, ib_vect, if_grad_vx, if_grad_vy, if_grad_vz,   &
     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: ib_vect
      integer(kind = kint), intent(in) :: if_grad_vx, if_grad_vy
      integer(kind = kint), intent(in) :: if_grad_vz
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_rtp_2_rj
!
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_rtp(sph_rtp%nnod_rtp,ncomp_rtp_2_rj)
!
!
      if(if_grad_vx .gt. 0) call sel_scalar_from_trans                  &
     &     (sph_rtp, fld_rtp(1,ib_vect  ), frc_rtp(1,if_grad_vx) )
      if(if_grad_vy .gt. 0) call sel_scalar_from_trans                  &
     &      (sph_rtp, fld_rtp(1,ib_vect+1), frc_rtp(1,if_grad_vy) )
      if(if_grad_vz .gt. 0) call sel_scalar_from_trans                  &
     &      (sph_rtp, fld_rtp(1,ib_vect+2), frc_rtp(1,if_grad_vz) )
!
      end subroutine copy_vect_to_grad_vect_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_grad_of_velocities_sph                             &
     &         (sph_rj, r_2nd, sph_bc_U, g_sph_rj, ipol, rj_fld)
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use const_sph_radial_grad
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in)  :: sph_bc_U
      type(phys_address), intent(in) :: ipol
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol%i_mag_stretch .eq. 0) return
!
      call copy_grad_vect_to_m_stretch                                  &
     &   (ipol,  rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc_U, g_sph_rj,  &
     &   (ipol%i_mag_stretch  ), ipol%i_grad_vx, rj_fld)
      call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc_U, g_sph_rj,  &
     &   (ipol%i_mag_stretch+1), ipol%i_grad_vy, rj_fld)
      call const_sph_gradient_no_bc(sph_rj, r_2nd, sph_bc_U, g_sph_rj,  &
     &   (ipol%i_mag_stretch+2), ipol%i_grad_vz, rj_fld)
!
      end subroutine cal_grad_of_velocities_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sel_scalar_from_trans(sph_rtp, v_rtp, d_sph)
!
      use t_spheric_rtp_data
      use copy_field_4_sph_trans
!
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      real(kind = kreal), intent(in) :: v_rtp(sph_rtp%nnod_rtp)
      real(kind = kreal), intent(inout) :: d_sph(sph_rtp%nnod_rtp)
!
!
!$omp parallel
      call copy_scalar_from_trans_smp(sph_rtp%nnod_rtp, ione,           &
     &    sph_rtp%nnod_rtp, v_rtp, d_sph)
!$omp end parallel
!
      end subroutine sel_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_grad_vect_to_m_stretch                            &
     &         (ipol, nnod, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod,ntot_phys_rj)
!
!
      if(ipol%i_mag_stretch .eq. 0) return
!
!$omp parallel workshare
      d_rj(1:nnod,ipol%i_mag_stretch  ) = d_rj(1:nnod,ipol%i_grad_vx)
      d_rj(1:nnod,ipol%i_mag_stretch+1) = d_rj(1:nnod,ipol%i_grad_vy)
      d_rj(1:nnod,ipol%i_mag_stretch+2) = d_rj(1:nnod,ipol%i_grad_vz)
!$omp end parallel workshare
!
      end subroutine copy_grad_vect_to_m_stretch
!
! -----------------------------------------------------------------------
!
      end module sph_poynting_flux_smp
