!extend_potential_field.f90
!      module extend_potential_field
!
!      modified by H. Matsuiui on Apr., 2009
!
!      subroutine ext_outside_potential(is_fld, kr_out)
!        Output: d_rj(kr_out+1:,is_fld:is_fld+2)
!      subroutine ext_inside_potential(is_fld, kr_in)
!        Output: d_rj(1:kr_in,is_fld:is_fld+2)
!
!      subroutine ext_outside_potential_with_j(is_fld, is_rot, kr_out)
!        Output: d_rj(kr_out+1:,is_fld:is_fld+2)
!        Output: d_rj(kr_out+1:,is_rot:is_rot+2)
!      subroutine ext_inside_potential_with_j(is_fld, is_rot, kr_in)
!        Output: d_rj(1:kr_in,is_fld:is_fld+2)
!        Output: d_rj(1:kr_in,is_rot:is_rot+2)
!
!      subroutine gauss_to_poloidal_out(is_fld, kr_out,                 &
!     &          ltr_w, r_gauss, w_gauss)
!      subroutine gauss_to_poloidal_in(is_fld, kr_in,                   &
!     &          ltr_w, r_gauss, w_gauss)
!
      module extend_potential_field
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential(is_fld, kr_out)
!
      integer(kind = kint), intent(in) :: is_fld, kr_out
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
          d_rj(inod,is_fld  ) =  d_rj(inod_cmb,is_fld) * ratio**l_gl
          d_rj(inod,is_fld+1) = -d_rj(inod_cmb,is_fld) * ratio**l_gl    &
     &                          * dble(l_gl)*a_r_1d_rj_r(k)
          d_rj(inod,is_fld+2) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential(is_fld, kr_in)
!
      integer(kind = kint), intent(in) :: is_fld, kr_in
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
          d_rj(inod,is_fld  ) = d_rj(inod_icb,is_fld) * ratio**(l_gl+1)
          d_rj(inod,is_fld+1) = d_rj(inod_icb,is_fld) * ratio**l_gl     &
     &                          * dble(l_gl+1)*a_r_1d_rj_r(kr_in)
          d_rj(inod,is_fld+2) = zero
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
      subroutine ext_outside_potential_with_j(is_fld, is_rot, kr_out)
!
      integer(kind = kint), intent(in) :: is_fld, is_rot, kr_out
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
          d_rj(inod,is_fld  ) =  d_rj(inod_cmb,is_fld) * ratio**l_gl
          d_rj(inod,is_fld+1) = -d_rj(inod_cmb,is_fld) * ratio**l_gl    &
     &                          * dble(l_gl)*a_r_1d_rj_r(k)
          d_rj(inod,is_fld+2) = zero
          d_rj(inod,is_rot  ) = zero
          d_rj(inod,is_rot+1) = zero
          d_rj(inod,is_rot+2) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_potential_with_j
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_potential_with_j(is_fld, is_rot, kr_in)
!
      integer(kind = kint), intent(in) :: is_fld, is_rot, kr_in
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
          d_rj(inod,is_fld  ) = d_rj(inod_icb,is_fld) * ratio**(l_gl+1)
          d_rj(inod,is_fld+1) = d_rj(inod_icb,is_fld) * ratio**l_gl     &
     &                          * dble(l_gl+1)*a_r_1d_rj_r(kr_in)
          d_rj(inod,is_fld+2) = zero
          d_rj(inod,is_rot  ) = zero
          d_rj(inod,is_rot+1) = zero
          d_rj(inod,is_rot+2) = zero
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
      subroutine gauss_to_poloidal_out(is_fld, kr_out,                  &
     &          ltr_w, r_gauss, w_gauss)
!
      integer(kind = kint), intent(in) :: is_fld, kr_out, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+1) )

      real(kind = kreal) :: ratio, al
      integer(kind = kint) :: inod
      integer(kind = kint) :: j, j_gl, l_gl, k, jst, jed
!
!
      jst = 1
      jed = 0
      do j = 1, nidx_rj(2)
        j_gl = idx_gl_1d_rj_j(j,1)
        if(j_gl .gt. ltr_w*(ltr_w+1)) exit
        jed = j
      end do
      if(idx_gl_1d_rj_j(1,1) .eq. izero) jst = 2
!
!$omp parallel private(k,ratio)
      do k = kr_out, nidx_rj(1)
        ratio = r_gauss * a_r_1d_rj_r(k)
!$omp do private(j,j_gl,l_gl,inod,al)
        do j = jst, jed
          inod = j + (k-1) * nidx_rj(2)
          j_gl = idx_gl_1d_rj_j(j,1)
          l_gl = idx_gl_1d_rj_j(j,2)
          al = one / dble(l_gl)
!
          d_rj(inod,is_fld  ) =  al*w_gauss(j_gl) * ratio**l_gl         &
     &                         * r_gauss
          d_rj(inod,is_fld+1) = - w_gauss(j_gl) * ratio**(l_gl+1)
          d_rj(inod,is_fld+2) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine gauss_to_poloidal_out
!
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_in(is_fld, kr_in,                    &
     &          ltr_w, r_gauss, w_gauss)
!
      integer(kind = kint), intent(in) :: is_fld, kr_in, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+1) )
!
      real(kind = kreal) :: ratio, ar_gauss, al1
      integer(kind = kint) :: inod
      integer(kind = kint) :: j, j_gl, l_gl, k, jst, jed
!
!
      ar_gauss = one / r_gauss
!
      jed = 0
      do j = 1, nidx_rj(2)
        j_gl = idx_gl_1d_rj_j(j,1)
        if(j_gl .gt. ltr_w*(ltr_w+1)) exit
        jed = j
      end do
      if(idx_gl_1d_rj_j(1,1) .eq. izero) jst = 2
!
!$omp parallel private(k,ratio)
      do k = 1, kr_in
        ratio = radius_1d_rj_r(k) * ar_gauss
!$omp do private(j,j_gl,l_gl,inod,al1)
        do j = jst, jed
          inod =     j + (k-1) * nidx_rj(2)
          j_gl = idx_gl_1d_rj_j(j,1)
          l_gl = idx_gl_1d_rj_j(j,2)
          al1 = one / dble(l_gl+1)
!
          d_rj(inod,is_fld  ) = - al1 * w_gauss(j_gl) * ratio**(l_gl+1) &
     &                         * r_gauss
          d_rj(inod,is_fld+1) = - w_gauss(j_gl) * ratio**l_gl
          d_rj(inod,is_fld+1) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine gauss_to_poloidal_in
!
!  -------------------------------------------------------------------
!
      end module extend_potential_field
