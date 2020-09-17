!>@file   zonal_wavenumber_4_legendre.f90
!!@brief  module zonal_wavenumber_4_legendre
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set order of spheherical harmonics modes
!!
!!@verbatim
!!      subroutine zonal_wavenum_eq_leg_trns(ndomain_m,                 &
!!     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!!      subroutine zonal_wavenum_eq_leg_modes(ndomain_m,                &
!!     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!!      subroutine zonal_leg_wavenum_simple_dist(ndomain_m,             &
!!     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!!@endverbatim
!
      module zonal_wavenumber_4_legendre
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: id_cyclic_eq_transform =   0
      integer(kind = kint), parameter :: id_cyclic_eq_mode =        1
      integer(kind = kint), parameter :: id_simple_rlm_distribute = 2
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine zonal_wavenum_eq_leg_trns(ndomain_m,                   &
     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, mm, imark, ltr_half
      integer(kind = kint), allocatable :: ip_tmp(:)
!
!
      allocate( ip_tmp(-ltr:ltr) )
      ip_tmp = 0
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
      do m = 1, ltr
        ip_tmp(-m) = ip_tmp(m)
      end do
!
!        write(8,*) 'm, ip_tmp(m)'
!      do m = -ltr, ltr
!        write(8,*) m, ip_tmp(m)
!      end do
!
      mm = -1
      do ip = 1, ndomain_m
        do m = -ltr/m_folding, ltr/m_folding
          if (ip_tmp(m) .eq. ip) then
            mm = mm + 1
            jdx_fsph(m) = mm
          end if
        end do
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      deallocate( ip_tmp )
!
!        write(*,*) 'm, jdx_fsph(m)'
!      do m = -ltr, ltr
!        write(*,*) m, jdx_fsph(m)
!      end do
!        write(*,*) 'mm, mdx_4_lgd(mm)'
!      do mm = 0,nph
!        write(*,*) mm, mdx_4_lgd(mm)
!      end do
!
!
      end subroutine zonal_wavenum_eq_leg_trns
!
! -----------------------------------------------------------------------
!
      subroutine zonal_wavenum_eq_leg_modes(ndomain_m,                  &
     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, mm, imark, ltr_half
      integer(kind = kint), allocatable :: ip_tmp(:)
!
!
      allocate( ip_tmp(0:ltr) )
      ip_tmp = 0
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
!        write(8,*) 'm, ip_tmp(m)'
!      do m = 0, ltr
!        write(8,*) m, ip_tmp(m)
!      end do
!
      mm = 0
      do ip = 1, ndomain_m
        do m = ltr/m_folding, 1, -1
          if (ip_tmp(m) .eq. ip) then
            jdx_fsph( m) = mm
            jdx_fsph(-m) = mm + 1
            mm = mm + 2
          end if
        end do
!
        if (ip_tmp(m) .eq. ip) then
          jdx_fsph(0) = mm
          mm = mm + 1
        end if
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      deallocate( ip_tmp )
!
!        write(*,*) 'm, jdx_fsph(m)'
!      do m = -ltr, ltr
!        write(*,*) m, jdx_fsph(m)
!      end do
!        write(*,*) 'mm, mdx_4_lgd(mm)'
!      do mm = 0,nph
!        write(*,*) mm, mdx_4_lgd(mm)
!      end do
!
!
      end subroutine zonal_wavenum_eq_leg_modes
!
! -----------------------------------------------------------------------
!
      subroutine zonal_leg_wavenum_simple_dist(ndomain_m,               &
     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: ltr, m_folding
      integer(kind = kint), intent(in) :: nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, mm, ltr_half
      integer(kind = kint), allocatable :: ip_tmp(:)
!
!
      ltr_half = ( ltr-mod(ltr,2) ) / (2*m_folding)
!
      jdx_fsph(0) = 0
      jdx_fsph( ltr/m_folding) = 1
      jdx_fsph(-ltr/m_folding) = 2
      mm = 2
      do m = 1, ltr/m_folding
        jdx_fsph( m) = mm + 1
        jdx_fsph(-m) = mm + 2
        if(mm+2 .ge. 2*ltr/m_folding+1) exit
!
        jdx_fsph( ltr/m_folding-m) = mm + 3
        jdx_fsph(-ltr/m_folding+m) = mm + 4
        mm = mm + 4
        if(mm+4 .ge. 2*ltr/m_folding+1) exit
      end do
!
      do m = -ltr/m_folding, ltr/m_folding
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
!      if(my_rank .gt. 0) return
!        write(*,*) 'm, jdx_fsph(m)'
!      do m = -ltr, ltr
!        write(*,*) m, jdx_fsph(m)
!      end do
!        write(*,*) 'mm, mdx_4_lgd(mm)'
!      do mm = 0,nph
!        write(*,*) mm, mdx_4_lgd(mm)
!      end do
!
      end subroutine zonal_leg_wavenum_simple_dist
!
! -----------------------------------------------------------------------
!
      end module zonal_wavenumber_4_legendre
