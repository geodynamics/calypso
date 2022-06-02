!>@file   extend_potential_field.f90
!!@brief  module extend_potential_field
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Extend magnetic field by potential field
!!
!!@verbatim
!!      subroutine ext_outside_scalar(kr_out, nidx_rj, idx_gl_1d_rj_j,  &
!!     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj, scalar_rj)
!!      subroutine ext_inside_scalar(kr_in, nidx_rj, idx_gl_1d_rj_j,    &
!!     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj, scalar_rj)
!!        integer(kind = kint), intent(in):: kr_out
!!        integer(kind = kint), intent(in):: kr_in
!!        integer(kind = kint), intent(in):: nidx_rj(2)
!!        integer(kind = kint), intent(in):: idx_gl_1d_rj_j(nidx_rj(2),3)
!!        integer(kind = kint), intent(in):: nnod_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(inout) :: scalar_rj(nnod_rj)
!!
!!      subroutine ext_outside_potential(kr_out,                        &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, d_rj)
!!      subroutine ext_inside_potential(kr_in,                          &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, d_rj)
!!        integer(kind = kint), intent(in):: kr_out
!!        integer(kind = kint), intent(in) :: kr_in
!!        integer(kind = kint), intent(in):: nidx_rj(2)
!!        integer(kind = kint), intent(in):: idx_gl_1d_rj_j(nidx_rj(2),3)
!!        integer(kind = kint), intent(in):: nnod_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(inout) :: d_rj(nnod_rj,3)
!!
!!      subroutine ext_outside_potential_with_j(kr_out,                 &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, d_magne, d_current)
!!        integer(kind = kint), intent(in):: kr_out
!!        integer(kind = kint), intent(in):: nidx_rj(2)
!!        integer(kind = kint), intent(in):: idx_gl_1d_rj_j(nidx_rj(2),3)
!!        integer(kind = kint), intent(in):: nnod_rj
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!!        real(kind = kreal), intent(inout) :: d_magne(nnod_rj,3)
!!        real(kind = kreal), intent(inout) :: d_current(nnod_rj,3)
!!      subroutine ext_inside_potential_with_j(kr_in,                   &
!!     &          is_magne, is_current, nidx_rj, idx_gl_1d_rj_j,        &
!!     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj,                 &
!!     &          ntot_phys_rj, d_rj)
!!        Output: d_rj(1:kr_in,is_magne)
!!        Output: d_rj(1:kr_in,is_current)
!!
!!      subroutine gauss_to_poloidal_out(kr_out, ltr_w, r_gauss,        &
!!     &          w_gauss, index_w, sph_rj, n_point, d_rj)
!!      subroutine gauss_to_poloidal_in(kr_in, ltr_w, r_gauss,          &
!!     &          w_gauss, index_w, sph_rj, n_point, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module extend_potential_field
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine ext_outside_scalar(kr_out, nidx_rj, idx_gl_1d_rj_j,    &
     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj, scalar_rj)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: scalar_rj(nnod_rj)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, nidx_rj(1)
        ratio = radius_1d_rj_r(kr_out) * a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, nidx_rj(2)
          inod = j + (k-1) * nidx_rj(2)
          inod_cmb = j + (kr_out-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          scalar_rj(inod) = scalar_rj(inod_cmb) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_scalar
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_scalar(kr_in, nidx_rj, idx_gl_1d_rj_j,      &
     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj, scalar_rj)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: scalar_rj(nnod_rj)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = radius_1d_rj_r(k) * a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, nidx_rj(2)
          inod =     j + (k-1) * nidx_rj(2)
          inod_icb = j + (kr_in-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          scalar_rj(inod) = scalar_rj(inod_icb) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_scalar
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential(kr_out,                          &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, nidx_rj(1)
        ratio = radius_1d_rj_r(kr_out) * a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, nidx_rj(2)
          inod = j + (k-1) * nidx_rj(2)
          inod_cmb = j + (kr_out-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          d_rj(inod,1) =  d_rj(inod_cmb,1) * ratio**l_gl
          d_rj(inod,2) = -d_rj(inod_cmb,1) * ratio**l_gl                &
     &                  * dble(l_gl)*a_r_1d_rj_r(k)
          d_rj(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential(kr_in,                            &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = radius_1d_rj_r(k) * a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, nidx_rj(2)
          inod =     j + (k-1) * nidx_rj(2)
          inod_icb = j + (kr_in-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          d_rj(inod,1) = d_rj(inod_icb,1) * ratio**(l_gl+1)
          d_rj(inod,2) = d_rj(inod_icb,2) * ratio**l_gl                 &
     &                  * dble(l_gl+1)*a_r_1d_rj_r(kr_in)
          d_rj(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_potential
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential_with_j(kr_out,                   &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, d_magne, d_current)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_magne(nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_current(nnod_rj,3)
!
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, nidx_rj(1)
        ratio = radius_1d_rj_r(kr_out) * a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, nidx_rj(2)
          inod = j + (k-1) * nidx_rj(2)
          inod_cmb = j + (kr_out-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          d_magne(inod,1) =  d_magne(inod_cmb,1) * ratio**l_gl
          d_magne(inod,2) = -d_magne(inod_cmb,1) * ratio**l_gl          &
     &                     * dble(l_gl) * a_r_1d_rj_r(k)
          d_magne(inod,3) =   zero
          d_current(inod,1) = zero
          d_current(inod,2) = zero
          d_current(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential_with_j
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential_with_j(kr_in,                     &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, d_magne, d_current)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_magne(nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_current(nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = radius_1d_rj_r(k) * a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, nidx_rj(2)
          inod =     j + (k-1) * nidx_rj(2)
          inod_icb = j + (kr_in-1) * nidx_rj(2)
          l_gl = idx_gl_1d_rj_j(j,2)
!
          d_magne(inod,1) = d_magne(inod_icb,1) * ratio**(l_gl+1)
          d_magne(inod,2) = d_magne(inod_icb,1) * ratio**l_gl           &
     &                     * dble(l_gl+1) * a_r_1d_rj_r(kr_in)
          d_magne(inod,3) =   zero
          d_current(inod,1) = zero
          d_current(inod,2) = zero
          d_current(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_potential_with_j
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_out(kr_out, ltr_w, r_gauss,          &
     &          w_gauss, index_w, sph_rj, n_point, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: kr_out, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,3)
!
      real(kind = kreal) :: ratio, al
      integer(kind = kint) :: inod, j, j_gl, k
      integer :: l_gl, m_gl
!
!
!$omp parallel do private(j_gl,l_gl,m_gl,j,inod,ratio,al)
      do j_gl = 1, ltr_w*(ltr_w+2)
        l_gl = int(index_w(j_gl,1))
        m_gl = int(index_w(j_gl,2))
        j = find_local_sph_address(sph_rj, l_gl, m_gl)
        if(j .eq. 0) cycle
        al = one / dble(l_gl)
!
        do k = kr_out, sph_rj%nidx_rj(1)
          inod = j + (k-1) * sph_rj%nidx_rj(2)
          ratio = r_gauss * sph_rj%a_r_1d_rj_r(k)
!
          d_rj(inod,1  ) =  al*w_gauss(j_gl) * ratio**l_gl * r_gauss
          d_rj(inod,2) = - w_gauss(j_gl) * ratio**(l_gl+1)
          d_rj(inod,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_out
!
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_in(kr_in, ltr_w, r_gauss,            &
     &          w_gauss, index_w, sph_rj, n_point, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: kr_in, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,3)
!
      real(kind = kreal) :: ratio, ar_gauss, al1
      integer(kind = kint) :: inod, j, j_gl, k
      integer :: l_gl, m_gl
!
!
      ar_gauss = one / r_gauss
!
!$omp parallel do private(j_gl,l_gl,m_gl,j,al1,inod,ratio)
      do j_gl = 1, ltr_w*(ltr_w+2)
        l_gl = int(index_w(j_gl,1))
        m_gl = int(index_w(j_gl,2))
        j = find_local_sph_address(sph_rj, l_gl, m_gl)
        if(j .eq. 0) cycle
        al1 = one / dble(l_gl+1)
!
        do k = 1, kr_in
          inod = j + (k-1) * sph_rj%nidx_rj(2)
          ratio = sph_rj%radius_1d_rj_r(k) * ar_gauss
!
          d_rj(inod,1) = - al1 * w_gauss(j_gl) * ratio**(l_gl+1)        &
     &                         * r_gauss
          d_rj(inod,2) = - w_gauss(j_gl) * ratio**l_gl
          d_rj(inod,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_in
!
!  -------------------------------------------------------------------
!
      end module extend_potential_field
