!>@file   m_gaunt_coriolis_rlm.f90
!!@brief  module m_gaunt_coriolis_rlm
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2013
!
!>@brief Adams-Gaunt integrals for Coriolis term
!!
!!
!!@verbatim
!!      subroutine alloacte_gaunt_coriolis_rlm
!!      subroutine dealloacte_gaunt_coriolis_rlm
!!      subroutine cal_gaunt_coriolis_rlm
!!@endverbatim
!!
!!@param   nri_rlm   Number of radial points 
!
      module m_gaunt_coriolis_rlm
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
!>      Local address for Coriolis term using Gaunt integral
      integer(kind = kint), allocatable :: jgi_cor_rlm(:,:)
!>      Local address for Coriolis term using Elsasser integral
      integer(kind = kint), allocatable :: jei_cor_rlm(:,:)
!
!>      Gaunt integral for Coriolis term
      real(kind = kreal), allocatable :: gi_cor_rlm(:,:)
!>      Elsasser integral for Coriolis term
      real(kind = kreal), allocatable :: ei_cor_rlm(:,:)
!
!>      1d @f$1 / r @f$ for @f$ f(r,l,m) @f$
!!@n@see  set_radius_func_cheby or set_radius_func_cheby
      real(kind = kreal), allocatable :: ar_1d_rlm(:,:)
!
!>     rotation spectr in @f$ f(r,l,m) @f$
!!@verbatim
!!        omega(kr,0) ... Omaga_z
!!        omega(kr,1) ... d Omaga_z / dr
!!        omega(kr,2) ... d^2 Omaga_z / dr^2
!!@endverbatim
      real(kind = kreal), allocatable :: omega_rlm(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloacte_gaunt_coriolis_rlm(nri_rlm, jmax_rlm)
!
      integer(kind = kint), intent(in) :: nri_rlm, jmax_rlm
!
!
      allocate(ar_1d_rlm(nri_rlm,2))
      allocate(omega_rlm(nri_rlm,0:2))
      ar_1d_rlm = 0.0d0
      omega_rlm = 0.0d0
!
      allocate( jgi_cor_rlm(jmax_rlm,2) )
      allocate( jei_cor_rlm(jmax_rlm,1) )
      allocate( gi_cor_rlm(jmax_rlm,2) )
      allocate( ei_cor_rlm(jmax_rlm,1) )
!
      jgi_cor_rlm = 0
      jei_cor_rlm = 0
      gi_cor_rlm = 0.0d0
      ei_cor_rlm = 0.0d0
!
      end subroutine alloacte_gaunt_coriolis_rlm
!
!-----------------------------------------------------------------------
!
      subroutine dealloacte_gaunt_coriolis_rlm
!
!
      deallocate(ar_1d_rlm, omega_rlm)
!
      deallocate(jgi_cor_rlm, gi_cor_rlm)
      deallocate(jei_cor_rlm, ei_cor_rlm)
!
      end subroutine dealloacte_gaunt_coriolis_rlm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_gaunt_coriolis_rlm(l_truncation,                   &
     &          nri_rlm, jmax_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: nri_rlm, jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nri_rlm)
!
      integer(kind = kint) :: l3, m3, j3, l2, m2, j_rlm, k_rlm
      integer(kind = kint) :: j2_gl_k1, j2_gl_k2, j2_gl_l1
!
!
      do k_rlm = 1, nri_rlm
        ar_1d_rlm(k_rlm,1) = one / radius_1d_rlm_r(k_rlm)
        ar_1d_rlm(k_rlm,2) = ar_1d_rlm(k_rlm,1)*ar_1d_rlm(k_rlm,1)
      end do
!
      do k_rlm = 1, nri_rlm
        omega_rlm(k_rlm,0) = half * radius_1d_rlm_r(k_rlm)**2
        omega_rlm(k_rlm,1) =        radius_1d_rlm_r(k_rlm)
        omega_rlm(k_rlm,2) = one
      end do
!
      do j_rlm = 1, jmax_rlm
        j3 = idx_gl_1d_rlm_j(j_rlm,1)
        l3 = idx_gl_1d_rlm_j(j_rlm,2)
        m3 = idx_gl_1d_rlm_j(j_rlm,3)
!
        l2 = l3 - 1
        if(l2.ge.1 .and. l2.le.l_truncation .and. abs(m3).le.l2) then
          gi_cor_rlm(j_rlm,1) = leadki(ione, izero, l2, m3, l3, m3)
          j2_gl_k1 = l2*(l2+1) + m3
        else
          gi_cor_rlm(j_rlm,1) = zero
          j2_gl_k1 = j3
        end if
!
        l2 = l3 + 1
        if(l2.ge.1 .and. l2.le.l_truncation .and. abs(m3).le.l2) then
          gi_cor_rlm(j_rlm,2) = leadki(ione, izero, l2, m3, l3, m3)
          j2_gl_k2 = l2*(l2+1) + m3
        else
          gi_cor_rlm(j_rlm,2) = zero
          j2_gl_k2 = j3
        end if
!
        if(l3 .ge. 1) then
          m2 = -m3
          ei_cor_rlm(j_rlm,1) = leadli(ione, izero, l3, m2, l3, m3)
          j2_gl_l1 = l3*(l3+1) + m2
        end if
!
        if(j3 .eq. 0) then
          j2_gl_k1 = izero
          j2_gl_k2 = izero
          j2_gl_l1 = izero
          gi_cor_rlm(j_rlm,2) = zero
          ei_cor_rlm(j_rlm,1) = zero
        end if
!
        jgi_cor_rlm(j_rlm,1) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, j2_gl_k1)
        jgi_cor_rlm(j_rlm,2) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, j2_gl_k2)
        jei_cor_rlm(j_rlm,1) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, j2_gl_l1)
      end do
!
      end subroutine cal_gaunt_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      integer function find_local_sph_rlm_address(jmax_rlm,             &
     &       idx_gl_1d_rlm_j, j_gl)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
      integer(kind = kint), intent(in) :: j_gl
!
      integer(kind = kint) :: j, l, m
!
!
      l = int( aint(sqrt(dble(j_gl))) )
      m = j_gl - l*(l+1)
!
      find_local_sph_rlm_address = 0
      do j = 1, jmax_rlm
        if (   idx_gl_1d_rlm_j(j,2) .eq. l                              &
     &   .and. idx_gl_1d_rlm_j(j,3) .eq. m) then
          find_local_sph_rlm_address = j
          return
        end if
      end do
!
      end function find_local_sph_rlm_address
!
!-----------------------------------------------------------------------
!
      end module m_gaunt_coriolis_rlm
