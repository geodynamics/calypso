!>@file   zonal_wavenumber_4_legendre.f90
!!@brief  module zonal_wavenumber_4_legendre
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set order of spheherical harmonics modes
!!
!!@verbatim
!!      subroutine set_domain_by_eq_leg_trns                            &
!!     &         (ndomain_m, ltr, m_folding, ip_tmp)
!!      subroutine set_domain_by_eq_leg_modes                           &
!!     &         (ndomain_m, ltr, m_folding, ip_tmp)
!!
!!      subroutine set_local_sph_fwd_order(ndomain_m, ltr, m_folding,   &
!!     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!!      subroutine set_local_sph_back_order(ndomain_m, ltr, m_folding,  &
!!     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!!      subroutine set_local_sph_neib_order(ndomain_m, ltr, m_folding,  &
!!     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!!@endverbatim
!
      module zonal_wavenumber_4_legendre
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_domain_by_eq_leg_trns                              &
     &         (ndomain_m, ltr, m_folding, ip_tmp)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: ndomain_m
!
      integer(kind = kint), intent(inout) :: ip_tmp(0:ltr)
!
      integer(kind = kint) :: m, ip, imark, ltr_half
!
!
      ip = 1
      imark = 1
      ltr_half = ( ltr-mod(ltr,2) ) / (2*m_folding)
!
      do m = 0, ltr_half
        ip_tmp(m) = ip
        ip = ip + imark
        if (ip .gt. ndomain_m) then
          ip =    ndomain_m
          imark = -1
        else if (ip .lt. 1) then
          ip = 1
          imark =  1
        end if
      end do
!
      do m = ltr_half+1, ltr/m_folding
        ip_tmp(m) = ip
        ip = ip + imark
        if (ip .gt. ndomain_m) then
          ip =    ndomain_m
          imark = -1
        else if (ip .lt. 1) then
          ip = 1
          imark =  1
        end if
      end do
!
      end subroutine set_domain_by_eq_leg_trns
!
! -----------------------------------------------------------------------
!
      subroutine set_domain_by_eq_leg_modes                             &
     &         (ndomain_m, ltr, m_folding, ip_tmp)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: ndomain_m
!
      integer(kind = kint), intent(inout) :: ip_tmp(0:ltr)
!
      integer(kind = kint) :: m, ip, imark, ltr_half
!
!
      ip = 1
      imark = 1
      ltr_half = ( ltr-mod(ltr,2) ) / (2*m_folding)
!
      do m = 1, ltr_half
        ip_tmp(m) = ip
        ip = ip + imark
        if (ip .gt. ndomain_m) then
          ip =    ndomain_m
          imark = -1
        else if (ip .lt. 1) then
          ip = 1
          imark =  1
        end if
      end do
!
      m = 0
      ip_tmp(m) = ip
      ip = ip + imark
      if (ip .gt. ndomain_m) then
        ip =    ndomain_m
        imark = -1
      else if (ip .lt. 1) then
        ip = 1
        imark =  1
      end if
!
      do m = ltr_half+1, ltr/m_folding
        ip_tmp(m) = ip
        ip = ip + imark
        if (ip .gt. ndomain_m) then
          ip =    ndomain_m
          imark = -1
        else if (ip .lt. 1) then
          ip = 1
          imark =  1
        end if
      end do
!
      end subroutine set_domain_by_eq_leg_modes
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_local_sph_fwd_order(ndomain_m, ltr, m_folding,     &
     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(in) :: ip_tmp(0:ltr)
!
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, ma, mm
!
!
      mm = 0
      do ip = 1, ndomain_m
        do m = ltr/m_folding, -ltr/m_folding, -1
          ma = abs(m)
          if (ip_tmp(ma) .eq. ip) then
            jdx_fsph(m) = mm
            mm = mm + 1
          end if
        end do
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      end subroutine set_local_sph_fwd_order
!
! -----------------------------------------------------------------------
!
      subroutine set_local_sph_back_order(ndomain_m, ltr, m_folding,    &
     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(in) :: ip_tmp(0:ltr)
!
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, ma, mm
!
!
      mm = 0
      do ip = 1, ndomain_m
        do m = ltr/m_folding, -ltr/m_folding, -1
          ma = abs(m)
          if (ip_tmp(ma) .eq. ip) then
            jdx_fsph(m) = mm
            mm = mm + 1
          end if
        end do
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      end subroutine set_local_sph_back_order
!
! -----------------------------------------------------------------------
!
      subroutine set_local_sph_neib_order(ndomain_m, ltr, m_folding,    &
     &          nth, nph, ip_tmp, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(in) :: ip_tmp(0:ltr)
!
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, mm
!
!
      mm = 0
      do ip = 1, ndomain_m
        do m = ltr/m_folding, 1, -1
          if(ip_tmp(m) .eq. ip) then
            jdx_fsph( m) = mm
            jdx_fsph(-m) = mm+1
            mm = mm + 2
          end if
        end do
!
        if(ip_tmp(0) .eq. ip) then
          jdx_fsph( m) = mm
          mm = mm + 1
        end if
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      end subroutine set_local_sph_neib_order
!
! -----------------------------------------------------------------------
!
      end module zonal_wavenumber_4_legendre
