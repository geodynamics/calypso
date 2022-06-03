!>@file   set_reference_temp_sph.f90
!!@brief  module set_reference_temp_sph
!!
!!@author H. Matsui
!!@date Programmed June., 1994
!!@date Modified Apr., 2009
!
!> @brief Set diffusive temperature profile
!!@n      with fixed temperature boundary
!!
!!@verbatim
!!      subroutine no_ref_temp_sph_mhd(nri, r_ICB, r_CMB,               &
!!     &          depth_top, depth_bottom, reftemp_r, refgrad_r)
!!      subroutine set_ref_temp_sph_mhd                                 &
!!     &         (low_temp, depth_top, high_temp, depth_bottom,         &
!!     &          nidx_rj, r_1d_rj, ar_1d_rj, reftemp_r, refgrad_r)
!!      subroutine set_stratified_sph_mhd                               &
!!     &        (stratified_sigma, stratified_width, stratified_outer_r,&
!!     &         nidx_rj, r_ICB, r_CMB, kr_ICB, kr_CMB, r_1d_rj,        &
!!     &         reftemp_r, refgrad_r)
!!
!!      subroutine set_reftemp_4_sph(idx_rj_degree_zero, inod_rj_center,&
!!     &          nnod_rj, nidx_rj, reftemp_r, refgrad_r,               &
!!     &          reference_rj, ref_grad_rj)
!!***********************************************************************
!!*
!!*     ref_temp(k,0) : reference of temperature  (output)
!!*     ref_temp(k,1) : dT_0 / dr
!!*     ref_temp(k,2) : d^2 T_0 / dr^2
!!*
!!*                          c2
!!*      ref_temp(k) = c1 + ------
!!*                          r(k)
!!*
!!*                      dto(k)
!!*     dref_temp(k) = ---------
!!*                        dr
!!*                         c2
!!*                  = - --------
!!*                        rs(k)
!!*
!!***********************************************************************
!!@endverbatim
!
      module set_reference_temp_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine no_ref_temp_sph_mhd(nri, r_ICB, r_CMB,                 &
     &          depth_top, depth_bottom, reftemp_r, refgrad_r)
!
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind = kreal), intent(inout) :: depth_top, depth_bottom
      real(kind=kreal), intent(inout) :: reftemp_r(0:nri)
      real(kind=kreal), intent(inout) :: refgrad_r(0:nri)
!
!
      reftemp_r(1:nri) = zero
      refgrad_r(1:nri) = zero
      depth_bottom = r_ICB
      depth_top =    r_CMB
!
      end subroutine no_ref_temp_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_ref_temp_sph_mhd                                   &
     &         (low_temp, depth_top, high_temp, depth_bottom,           &
     &          nidx_rj, r_1d_rj, ar_1d_rj, reftemp_r, refgrad_r)
!
      real (kind = kreal), intent(in) :: low_temp, high_temp
      real (kind = kreal), intent(in) :: depth_top, depth_bottom
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind=kreal), intent(in) :: r_1d_rj(nidx_rj(1))
      real(kind=kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
!
      real(kind=kreal), intent(inout) :: reftemp_r(0:nidx_rj(1))
      real(kind=kreal), intent(inout) :: refgrad_r(0:nidx_rj(1))
!
      integer (kind = kint) :: k
!
! set reference temperature (for spherical shell)
!
      reftemp_r(0) = high_temp
      refgrad_r(0) = zero
      do k = 1, nidx_rj(1)
        if(r_1d_rj(k) .lt. depth_bottom) then
          reftemp_r(k) = high_temp
          refgrad_r(k) = zero
        else if(r_1d_rj(k) .gt. depth_top) then
          reftemp_r(k) = low_temp
          refgrad_r(k) = zero
        else
          reftemp_r(k) = (depth_bottom*depth_top*ar_1d_rj(k,1)          &
     &                   * (high_temp - low_temp)                       &
     &                    - depth_bottom*high_temp                      &
     &                    + depth_top* low_temp )                       &
     &                     / (depth_top - depth_bottom)
          refgrad_r(k) = - depth_bottom*depth_top*ar_1d_rj(k,2)         &
     &                   * (high_temp - low_temp)                       &
     &                     / (depth_top - depth_bottom)
!          refgrad_r2(k) = two * depth_bottom*depth_top                 &
!     &                   * ar_1d_rj(k,2)*ar_1d_rj(k,1)                 &
!     &                   * (high_temp - low_temp)                      &
!     &                     / (depth_top - depth_bottom)
        end if
      end do
!
      end subroutine set_ref_temp_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_stratified_sph_mhd                                 &
     &        (stratified_sigma, stratified_width, stratified_outer_r,  &
     &         nidx_rj, r_ICB, r_CMB, kr_ICB, kr_CMB, r_1d_rj,          &
     &         reftemp_r, refgrad_r)
!
      real  (kind=kreal), intent(in) :: stratified_sigma
      real  (kind=kreal), intent(in) :: stratified_width
      real  (kind=kreal), intent(in) :: stratified_outer_r
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: kr_ICB, kr_CMB
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
      real(kind=kreal), intent(in) :: r_1d_rj(nidx_rj(1))
!
      real(kind=kreal), intent(inout) :: reftemp_r(0:nidx_rj(1))
      real(kind=kreal), intent(inout) :: refgrad_r(0:nidx_rj(1))
!
      integer (kind = kint) :: k
      real(kind = kreal) :: alpha, beta
!
!
      alpha = (r_ICB-stratified_outer_r) / stratified_width
      reftemp_r(0) = - half * (r_ICB + stratified_sigma)                &
     &                 * (one - tanh(alpha)) + stratified_sigma
      refgrad_r(0) = zero
      do k = 1, nidx_rj(1)
        if(k .lt. kr_ICB) then
          alpha = (r_ICB-stratified_outer_r) / stratified_width
          reftemp_r(k) = - half * (r_ICB + stratified_sigma)            &
     &                     * (one - tanh(alpha)) + stratified_sigma
          refgrad_r(k) = zero
        else if(k .gt. kr_CMB) then
          alpha = (r_CMB-stratified_outer_r) / stratified_width
          reftemp_r(k) = - half * (r_CMB + stratified_sigma)            &
     &                     * (one - tanh(alpha)) + stratified_sigma
          refgrad_r(k) = zero
        else
          alpha = (r_1d_rj(k)-stratified_outer_r) / stratified_width
          beta =  (r_1d_rj(k) + stratified_sigma) / stratified_width
          reftemp_r(k) = - half * (r_1d_rj(k) + stratified_sigma)       &
     &                     * (one - tanh(alpha)) + stratified_sigma
          refgrad_r(k) = half * (-one + beta)                           &
     &                     + half * tanh(alpha)                         &
     &                     - half * beta * tanh(alpha) * tanh(alpha)
!          refgrad_r2(k) = (one - tanh(alpha)*tanh(alpha))              &
!     &                     * (one - beta * tanh(alpha))                &
!     &                     / stratified_width
        end if
      end do
!
      end subroutine set_stratified_sph_mhd
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_reftemp_4_sph(idx_rj_degree_zero, inod_rj_center,  &
     &          nnod_rj, nidx_rj, reftemp_r, refgrad_r,                 &
     &          reference_rj, ref_grad_rj)
!
      integer(kind = kint), intent(in) ::  nnod_rj
      integer(kind = kint), intent(in) ::  nidx_rj(2)
      integer(kind = kint), intent(in) ::  idx_rj_degree_zero
      integer(kind = kint), intent(in) ::  inod_rj_center
      real(kind=kreal), intent(in) :: reftemp_r(0:nidx_rj(1))
      real(kind=kreal), intent(in) :: refgrad_r(0:nidx_rj(1))
!
      real (kind=kreal), intent(inout) :: reference_rj(nnod_rj)
      real (kind=kreal), intent(inout) :: ref_grad_rj(nnod_rj,3)
!
      integer(kind = kint) ::  kk, inod
!
!
      if (idx_rj_degree_zero .le. izero) return
!
      do kk = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (kk-1) * nidx_rj(2)
        reference_rj(inod) =  reftemp_r(kk)
        ref_grad_rj(inod,1) = refgrad_r(kk)
      end do
!
      if(inod_rj_center .gt. 0) then
        reference_rj(inod) =  reftemp_r(0)
      end if
!
      end subroutine set_reftemp_4_sph
!
!  -------------------------------------------------------------------
!
      end module set_reference_temp_sph
