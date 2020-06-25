!>@file   copy_rtp_vectors_4_grad.f90
!!@brief  module copy_rtp_vectors_4_grad
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine copy_vectors_rtp_4_grad(sph,                         &
!!     &          b_trns_base, fn_trns_dvec, trns_b_MHD, trns_f_ngSGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(diff_vector_address), intent(in) :: fn_trns_dvec
!!        type(spherical_transform_data), intent(in) :: trns_b_MHD
!!        type(spherical_transform_data), intent(inout) :: trns_f_ngSGS
!!      subroutine copy_vect_to_grad_vect_rtp                           &
!!     &         (sph_rtp, ib_vect, if_grad_vx, if_grad_vy, if_grad_vz, &
!!     &          ncomp_rj_2_rtp, ncomp_rtp_2_rj, fld_rtp, frc_rtp)
!!      subroutine sel_scalar_from_trans(sph_rtp, v_rtp, d_sph)
!!@endverbatim
!
      module copy_rtp_vectors_4_grad
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: copy_vect_to_grad_vect_rtp, sel_scalar_from_trans
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_vectors_rtp_4_grad(sph,                           &
     &          b_trns_base, fn_trns_dvec, trns_b_MHD, trns_f_ngSGS)
!
      type(sph_grids), intent(in) :: sph
      type(base_field_address), intent(in) :: b_trns_base
      type(diff_vector_address), intent(in) :: fn_trns_dvec
!
      type(spherical_transform_data), intent(in) :: trns_b_MHD
      type(spherical_transform_data), intent(inout) :: trns_f_ngSGS
!
!
      if(b_trns_base%i_velo .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      b_trns_base%i_velo,     fn_trns_dvec%i_grad_vx,             &
     &      fn_trns_dvec%i_grad_vy, fn_trns_dvec%i_grad_vz,             &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns_base%i_vort .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      b_trns_base%i_vort,     fn_trns_dvec%i_grad_wx,             &
     &      fn_trns_dvec%i_grad_wy, fn_trns_dvec%i_grad_wz,             &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns_base%i_vecp .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      b_trns_base%i_vecp,     fn_trns_dvec%i_grad_ax,             &
     &      fn_trns_dvec%i_grad_ay, fn_trns_dvec%i_grad_az,             &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns_base%i_magne .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      b_trns_base%i_magne,    fn_trns_dvec%i_grad_bx,             &
     &      fn_trns_dvec%i_grad_by, fn_trns_dvec%i_grad_bz,             &
     &      trns_b_MHD%ncomp, trns_f_ngSGS%ncomp,                       &
     &      trns_b_MHD%fld_rtp, trns_f_ngSGS%fld_rtp)
      end if
      if(b_trns_base%i_current .gt. 0) then
        call copy_vect_to_grad_vect_rtp(sph%sph_rtp,                    &
     &      b_trns_base%i_current,  fn_trns_dvec%i_grad_jx,             &
     &      fn_trns_dvec%i_grad_jy, fn_trns_dvec%i_grad_jz,             &
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
      end module copy_rtp_vectors_4_grad
