!>@file   extend_potential_field.f90
!!@brief  module extend_potential_field
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Extend magnetic field by potential field
!!
!!@verbatim
!!      subroutine ext_outside_scalar(kr_out, is_fld,                   &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!      subroutine ext_inside_scalar(kr_in, is_fld,                     &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!
!!      subroutine ext_outside_potential(kr_out, is_fld,                &
!!     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r, &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
!!     &         (kr_out, is_fld, ntot_phys_rj, d_rj)
!!        Output: d_rj(kr_out+1:,is_fld)
!!      subroutine ext_inside_potential(kr_in, is_fld, nidx_rj,         &
!!     &         idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,           &
!!     &         nnod_rj, ntot_phys_rj, d_rj)
!!        Output: d_rj(1:kr_in,is_fld)
!!
!!      subroutine ext_outside_potential_with_j(kr_out,                 &
!!     &          is_magne, is_current, nidx_rj, idx_gl_1d_rj_j,        &
!!     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj,                 &
!!     &          ntot_phys_rj, d_rj)
!!        Output: d_rj(1:kr_in,is_magne)
!!        Output: d_rj(1:kr_in,is_current)
!!      subroutine ext_inside_potential_with_j(kr_in,                   &
!!     &          is_magne, is_current, nidx_rj, idx_gl_1d_rj_j,        &
!!     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj,                 &
!!     &          ntot_phys_rj, d_rj)
!!        Output: d_rj(1:kr_in,is_magne)
!!        Output: d_rj(1:kr_in,is_current)
!!
!!      subroutine gauss_to_poloidal_out(kr_out, ltr_w, r_gauss,        &
!!     &          w_gauss, index_w, is_fld, sph_rj,                     &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine gauss_to_poloidal_in(kr_in, ltr_w, r_gauss,          &
!!     &          w_gauss, index_w, is_fld, sph_rj,                     &
!!     &          n_point, ntot_phys_rj, d_rj)
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
      subroutine ext_outside_scalar(kr_out, is_fld,                     &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_fld) =  d_rj(inod_cmb,is_fld) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_outside_scalar
!
!  -------------------------------------------------------------------
!
      subroutine ext_inside_scalar(kr_in, is_fld,                       &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_fld) = d_rj(inod_icb,is_fld) * ratio**l_gl
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine ext_inside_scalar
!
!  -------------------------------------------------------------------
!
      subroutine ext_outside_potential(kr_out, is_fld,                  &
     &          nidx_rj, idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,   &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_fld  ) =  d_rj(inod_cmb,is_fld  ) * ratio**l_gl
          d_rj(inod,is_fld+1) = -d_rj(inod_cmb,is_fld  ) * ratio**l_gl  &
     &                         * dble(l_gl)*a_r_1d_rj_r(k)
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
      subroutine ext_inside_potential(kr_in, is_fld, nidx_rj,           &
     &         idx_gl_1d_rj_j, radius_1d_rj_r, a_r_1d_rj_r,             &
     &         nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_fld  ) = d_rj(inod_icb,is_fld  ) * ratio**(l_gl+1)
          d_rj(inod,is_fld+1) = d_rj(inod_icb,is_fld+1) * ratio**l_gl   &
     &                         * dble(l_gl+1)*a_r_1d_rj_r(kr_in)
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
      subroutine ext_outside_potential_with_j(kr_out,                   &
     &          is_magne, is_current, nidx_rj, idx_gl_1d_rj_j,          &
     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj,                   &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_out
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_magne  ) =  d_rj(inod_cmb,is_magne  )            &
     &                            * ratio**l_gl
          d_rj(inod,is_magne+1) = -d_rj(inod_cmb,is_magne  )            &
     &                            * ratio**l_gl * dble(l_gl)            &
     &                             *a_r_1d_rj_r(k)
          d_rj(inod,is_magne+2) = zero
          d_rj(inod,is_current  ) = zero
          d_rj(inod,is_current+1) = zero
          d_rj(inod,is_current+2) = zero
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
     &          is_magne, is_current, nidx_rj, idx_gl_1d_rj_j,          &
     &          radius_1d_rj_r, a_r_1d_rj_r, nnod_rj,                   &
     &          ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(nidx_rj(2),3)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: a_r_1d_rj_r(nidx_rj(1))
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
          d_rj(inod,is_magne  ) = d_rj(inod_icb,is_magne  )            &
     &                           * ratio**(l_gl+1)
          d_rj(inod,is_magne+1) = d_rj(inod_icb,is_magne  )             &
     &                           * ratio**l_gl * dble(l_gl+1)           &
     &                           * a_r_1d_rj_r(kr_in)
          d_rj(inod,is_magne+2) = zero
          d_rj(inod,is_current  ) = zero
          d_rj(inod,is_current+1) = zero
          d_rj(inod,is_current+2) = zero
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
     &          w_gauss, index_w, is_fld, sph_rj,                       &
     &          n_point, ntot_phys_rj, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: kr_out, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: ratio, al
      integer(kind = kint) :: inod, j, j_gl, k
      integer(kind = 4) :: l_gl, m_gl
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
          d_rj(inod,is_fld  ) =  al*w_gauss(j_gl) * ratio**l_gl         &
     &                                            * r_gauss
          d_rj(inod,is_fld+1) = - w_gauss(j_gl) * ratio**(l_gl+1)
          d_rj(inod,is_fld+2) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_out
!
!  -------------------------------------------------------------------
!
      subroutine gauss_to_poloidal_in(kr_in, ltr_w, r_gauss,            &
     &          w_gauss, index_w, is_fld, sph_rj,                       &
     &          n_point, ntot_phys_rj, d_rj)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      integer(kind = kint), intent(in) :: kr_in, ltr_w
      real(kind = kreal), intent(in) :: r_gauss
      real(kind = kreal), intent(in) :: w_gauss( ltr_w*(ltr_w+2) )
      integer(kind = kint), intent(in) :: index_w( ltr_w*(ltr_w+2),2 )
!
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal) :: ratio, ar_gauss, al1
      integer(kind = kint) :: inod, j, j_gl, k
      integer(kind = 4) :: l_gl, m_gl
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
          d_rj(inod,is_fld  ) = - al1 * w_gauss(j_gl)                   &
     &                          * ratio**(l_gl+1) * r_gauss
          d_rj(inod,is_fld+1) = - w_gauss(j_gl) * ratio**l_gl
          d_rj(inod,is_fld+2) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine gauss_to_poloidal_in
!
!  -------------------------------------------------------------------
!
      end module extend_potential_field
