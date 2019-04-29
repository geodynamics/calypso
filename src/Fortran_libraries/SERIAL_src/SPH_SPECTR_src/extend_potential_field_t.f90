!>@file   extend_potential_field_t.f90
!!@brief  module extend_potential_field_t
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Extend magnetic field by potential field structure
!!
!!@verbatim
!!      subroutine ext_outside_scalar_t(rj, kr_out, d_rj)
!!      subroutine ext_inside_scalar_t(rj, kr_in, d_rj)
!!
!!      subroutine ext_outside_potential_t(rj, kr_out, d_rj)
!!        Output: d_rj(kr_out+1:,1:3)
!!      subroutine ext_inside_potential_t(rj, kr_in, d_rj)
!!        Output: d_rj(1:kr_in,1:3)
!!
!!      subroutine ext_outside_potential_t_with_j(rj, kr_out,           &
!!     &          d_rj_b, d_rj_j)
!!        Output: d_rj_b(kr_out+1:,1:3)
!!        Output: d_rj_j(kr_out+1:,1:3)
!!      subroutine ext_inside_potential_t_with_j(rj, kr_in,             &
!!     &          d_rj_b, d_rj_j)
!!        Output: d_rj_b(1:kr_in,1:3)
!!        Output: d_rj_j(1:kr_in,1:3)
!!
!!      subroutine gauss_to_poloidal_out_t(rj, kr_out, ltr_w, r_gauss,  &
!!     &          w_gauss, index_w, d_rj)
!!      subroutine gauss_to_poloidal_in_t(rj, kr_in, ltr_w, r_gauss,    &
!!     &          w_gauss, index_w, d_rj)
!!        type(sph_rj_grid), intent(in) :: rj
!!@endverbatim
!
      module extend_potential_field_t
!
      use m_precision
!
      use m_constants
      use t_spheric_parameter
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine ext_outside_scalar_t(rj, kr_out, d_rj)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, rj%nidx_rj(1)
        ratio = rj%radius_1d_rj_r(kr_out) * rj%a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, rj%nidx_rj(2)
          inod = j + (k-1) * rj%nidx_rj(2)
          inod_cmb = j + (kr_out-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj(inod) =  d_rj(inod_cmb) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_scalar_t
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_scalar_t(rj, kr_in, d_rj)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = rj%radius_1d_rj_r(k) * rj%a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, rj%nidx_rj(2)
          inod =     j + (k-1) * rj%nidx_rj(2)
          inod_icb = j + (kr_in-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj(inod) = d_rj(inod_icb) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_scalar_t
!
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential_t(rj, kr_out, d_rj)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, rj%nidx_rj(1)
        ratio = rj%radius_1d_rj_r(kr_out) * rj%a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, rj%nidx_rj(2)
          inod = j + (k-1) * rj%nidx_rj(2)
          inod_cmb = j + (kr_out-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj(inod,1) =  d_rj(inod_cmb,1) * ratio**l_gl
          d_rj(inod,2) = -d_rj(inod_cmb,1) * ratio**l_gl                &
     &                          * dble(l_gl)*rj%a_r_1d_rj_r(k)
          d_rj(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential_t
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential_t(rj, kr_in, d_rj)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = rj%radius_1d_rj_r(k) * rj%a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, rj%nidx_rj(2)
          inod =     j + (k-1) * rj%nidx_rj(2)
          inod_icb = j + (kr_in-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj(inod,1) = d_rj(inod_icb,1) * ratio**(l_gl+1)
          d_rj(inod,2) = d_rj(inod_icb,1) * ratio**l_gl                 &
     &                          * dble(l_gl+1)*rj%a_r_1d_rj_r(kr_in)
          d_rj(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_potential_t
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential_t_with_j(rj, kr_out,             &
     &          d_rj_b, d_rj_j)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(inout) :: d_rj_b(rj%nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_rj_j(rj%nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_cmb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = kr_out+1, rj%nidx_rj(1)
        ratio = rj%radius_1d_rj_r(kr_out) * rj%a_r_1d_rj_r(k)
!$omp do private(j,l_gl,inod,inod_cmb)
        do j = 1, rj%nidx_rj(2)
          inod = j + (k-1) * rj%nidx_rj(2)
          inod_cmb = j + (kr_out-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj_b(inod,1) =  d_rj_b(inod_cmb,1) * ratio**l_gl
          d_rj_b(inod,2) = -d_rj_b(inod_cmb,1) * ratio**l_gl            &
     &                          * dble(l_gl)*rj%a_r_1d_rj_r(k)
          d_rj_b(inod,3) = zero
          d_rj_j(inod,1) = zero
          d_rj_j(inod,2) = zero
          d_rj_j(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential_t_with_j
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential_t_with_j(rj, kr_in,               &
     &          d_rj_b, d_rj_j)
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(inout) :: d_rj_b(rj%nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_rj_j(rj%nnod_rj,3)
!
      real(kind = kreal) :: ratio
      integer(kind = kint) :: inod, inod_icb
      integer(kind = kint) :: j, l_gl, k
!
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in-1
        ratio = rj%radius_1d_rj_r(k) * rj%a_r_1d_rj_r(kr_in)
!$omp do private(j,l_gl,inod,inod_icb)
        do j = 1, rj%nidx_rj(2)
          inod =     j + (k-1) * rj%nidx_rj(2)
          inod_icb = j + (kr_in-1) * rj%nidx_rj(2)
          l_gl = rj%idx_gl_1d_rj_j(j,2)
!
          d_rj_b(inod,1) = d_rj_b(inod_icb,1) * ratio**(l_gl+1)
          d_rj_b(inod,2) = d_rj_b(inod_icb,1) * ratio**l_gl             &
     &                          * dble(l_gl+1)*rj%a_r_1d_rj_r(kr_in)
          d_rj_b(inod,3) = zero
          d_rj_j(inod,1  ) = zero
          d_rj_j(inod,2) = zero
          d_rj_j(inod,3) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_potential_t_with_j
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_out_t(rj, kr_out, ltr_w, r_gauss,    &
     &          w_gauss, index_w, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_out, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj,3)
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
        j = find_local_sph_address(rj, l_gl, m_gl)
        if(j .eq. 0) cycle
        al = one / dble(l_gl)
!
        do k = kr_out, rj%nidx_rj(1)
          inod = j + (k-1) * rj%nidx_rj(2)
          ratio = r_gauss * rj%a_r_1d_rj_r(k)
!
          d_rj(inod,1) =  al*w_gauss(j_gl) * ratio**l_gl * r_gauss
          d_rj(inod,2) = - w_gauss(j_gl) * ratio**(l_gl+1)
          d_rj(inod,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_out_t
!
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_in_t(rj, kr_in, ltr_w, r_gauss,      &
     &          w_gauss, index_w, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint), intent(in) :: kr_in, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(rj%nnod_rj,3)
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
        j = find_local_sph_address(rj, l_gl, m_gl)
        if(j .eq. 0) cycle
        al1 = one / dble(l_gl+1)
!
        do k = 1, kr_in
          inod = j + (k-1) * rj%nidx_rj(2)
          ratio = rj%radius_1d_rj_r(k) * ar_gauss
!
          d_rj(inod,1) = - al1 * w_gauss(j_gl) * ratio**(l_gl+1)        &
     &                         * r_gauss
          d_rj(inod,2) = - w_gauss(j_gl) * ratio**l_gl
          d_rj(inod,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_in_t
!
!  -------------------------------------------------------------------
!
      end module extend_potential_field_t
