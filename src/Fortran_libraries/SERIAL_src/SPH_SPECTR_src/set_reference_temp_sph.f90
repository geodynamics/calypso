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
!!      subroutine no_ref_temp_sph_mhd(depth_top, depth_bottom,         &
!!     &          nri, r_ICB, r_CMB, reftemp_rj)
!!      subroutine set_ref_temp_sph_mhd                                 &
!!     &         (low_temp, depth_top, high_temp, depth_bottom,         &
!!     &          nidx_rj, r_1d_rj, ar_1d_rj, reftemp_rj)
!!      subroutine set_stratified_sph_mhd                               &
!!     &        (stratified_sigma, stratified_width, stratified_outer_r,&
!!     &         nidx_rj, r_ICB, r_CMB, kr_ICB, kr_CMB, r_1d_rj,        &
!!     &         reftemp_rj)
!!
!!      subroutine set_reftemp_4_sph(idx_rj_degree_zero, nidx_rj,       &
!!     &         reftemp_rj, i_ref, i_gref, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol
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
      subroutine no_ref_temp_sph_mhd(depth_top, depth_bottom,           &
     &          nri, r_ICB, r_CMB, reftemp_rj)
!
      real (kind = kreal), intent(inout) :: depth_top, depth_bottom
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r_ICB, r_CMB
!
      real(kind=kreal), intent(inout) :: reftemp_rj(nri,0:2)
!
!
      reftemp_rj(1:nri,0) = zero
      reftemp_rj(1:nri,1) = zero
      reftemp_rj(1:nri,2) = zero
      depth_bottom = r_ICB
      depth_top =    r_CMB
!
      end subroutine no_ref_temp_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_ref_temp_sph_mhd                                   &
     &         (low_temp, depth_top, high_temp, depth_bottom,           &
     &          nidx_rj, r_1d_rj, ar_1d_rj, reftemp_rj)
!
      real (kind = kreal), intent(in) :: low_temp, high_temp
      real (kind = kreal), intent(inout) :: depth_top, depth_bottom
!
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind=kreal), intent(in) :: r_1d_rj(nidx_rj(1))
      real(kind=kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
!
      real(kind=kreal), intent(inout) :: reftemp_rj(nidx_rj(1),0:2)
!
      integer (kind = kint) :: k
!
! set reference temperature (for spherical shell)
!
      do k = 1, nidx_rj(1)
        if(r_1d_rj(k) .lt. depth_bottom) then
          reftemp_rj(k,0) = high_temp
          reftemp_rj(k,1) = zero
          reftemp_rj(k,2) = zero
        else if(r_1d_rj(k) .gt. depth_top) then
          reftemp_rj(k,0) = low_temp
          reftemp_rj(k,1) = zero
          reftemp_rj(k,2) = zero
        else
          reftemp_rj(k,0) = (depth_bottom*depth_top*ar_1d_rj(k,1)       &
     &                   * (high_temp - low_temp)                       &
     &                    - depth_bottom*high_temp                      &
     &                    + depth_top* low_temp )                       &
     &                     / (depth_top - depth_bottom)
          reftemp_rj(k,1) = - depth_bottom*depth_top*ar_1d_rj(k,2)      &
     &                   * (high_temp - low_temp)                       &
     &                     / (depth_top - depth_bottom)
          reftemp_rj(k,2) = two * depth_bottom*depth_top                &
     &                   * ar_1d_rj(k,2)*ar_1d_rj(k,1)                  &
     &                   * (high_temp - low_temp)                       &
     &                     / (depth_top - depth_bottom)
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
     &         reftemp_rj)
!
      use m_physical_property
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
      real(kind=kreal), intent(inout) :: reftemp_rj(nidx_rj(1),0:2)
!
      integer (kind = kint) :: k
      real(kind = kreal) :: alpha, beta
!
!
      do k = 1, nidx_rj(1)
        if(k .lt. kr_ICB) then
          alpha = (r_ICB-stratified_outer_r) / stratified_width
          beta =  (r_ICB + stratified_sigma) / stratified_width
          reftemp_rj(k,0) = - half * (r_ICB + stratified_sigma)         &
     &                     * (one - tanh(alpha)) + stratified_sigma
          reftemp_rj(k,1) = zero
          reftemp_rj(k,2) = zero
        else if(k .gt. kr_CMB) then
          alpha = (r_CMB-stratified_outer_r) / stratified_width
          beta =  (r_CMB + stratified_sigma) / stratified_width
          reftemp_rj(k,0) = - half * (r_CMB + stratified_sigma)         &
     &                     * (one - tanh(alpha)) + stratified_sigma
          reftemp_rj(k,1) = zero
          reftemp_rj(k,2) = zero
        else
          alpha = (r_1d_rj(k)-stratified_outer_r) / stratified_width
          beta =  (r_1d_rj(k) + stratified_sigma) / stratified_width
          reftemp_rj(k,0) = - half * (r_1d_rj(k) + stratified_sigma)    &
     &                     * (one - tanh(alpha)) + stratified_sigma
          reftemp_rj(k,1) = half * (-one + beta)                        &
     &                     + half * tanh(alpha)                         &
     &                     - half * beta * tanh(alpha) * tanh(alpha)
          reftemp_rj(k,2) = (one - tanh(alpha)*tanh(alpha))             &
     &                     * (one - beta * tanh(alpha))                 &
     &                     / stratified_width
        end if
      end do
!
      end subroutine set_stratified_sph_mhd
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_reftemp_4_sph(idx_rj_degree_zero, nidx_rj,         &
     &         reftemp_rj, i_ref, i_gref, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) ::  nidx_rj(2)
      integer(kind = kint), intent(in) ::  idx_rj_degree_zero
      integer(kind = kint), intent(in) ::  nnod_rj, ntot_phys_rj
      integer(kind = kint), intent(in) ::  i_ref, i_gref
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) ::  kk, inod
!
!
      if (i_ref*i_gref .le. izero) return
      if (idx_rj_degree_zero .le. izero) return
!
      do kk = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (kk-1) * nidx_rj(2)
        d_rj(inod,i_ref) =  reftemp_rj(kk,0)
        d_rj(inod,i_gref) = reftemp_rj(kk,1)
      end do
!
      end subroutine set_reftemp_4_sph
!
!  -------------------------------------------------------------------
!
      end module set_reference_temp_sph
