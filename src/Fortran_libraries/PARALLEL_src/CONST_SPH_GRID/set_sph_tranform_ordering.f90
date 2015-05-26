!>@file   set_sph_tranform_ordering.f90
!!@brief  module set_sph_tranform_ordering
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set order of spheherical harmonics modes
!!
!!@verbatim
!!      subroutine set_wavenumber_4_ispack_fft(nth, nph, m_folding,     &
!!     &          mspec_4_ispack, mdx_ispack)
!!        output: mspec_4_ispack, mdx_ispack
!!      subroutine set_zonal_wavenum_4_legendre(ndomain_m,              &
!!     &          ltr, m_folding, nth, nph, jdx_fsph, mdx_4_lgd)
!!        output: jdx_fsph, mdx_4_lgd
!!
!!      subroutine set_merged_index_4_sph_trans(ndomain_m, ltr, jmax,   &
!!     &          nph, m_folding, istack_m, mdx_4_lgd, nidx_ml,         &
!!     &          istack_ml, jdx_lag)
!!        output: nidx_ml, istack_ml, jdx_lag
!!      subroutine set_merged_index_4_sph_rj(ndomain_r, ndomain_m,      &
!!     &          ndomain_rj, jmax, istack_ml, jdx_lag,                 &
!!     &          nidx_j, istack_j, jdx_rj)
!!        output:  nidx_j, istack_j, jdx_rj
!!
!!      subroutine set_trans_table_fft_2_lgd(ltr, nth, nph, m_folding,  &
!!     &          mspec_4_ispack, jdx_fsph, mtbl_fft_2_lgd)
!!        output: mtbl_fft_2_lgd
!!@endverbatim
!
      module set_sph_tranform_ordering
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
      subroutine set_wavenumber_4_ispack_fft(nth, nph, m_folding,       &
     &          mspec_4_ispack, mdx_ispack)
!
      integer(kind = kint), intent(in) :: nth, nph, m_folding
      integer(kind = kint), intent(inout) :: mspec_4_ispack(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_ispack(nph)
      integer(kind = kint) :: m, mm, mnum
!
!
      mnum = nth/m_folding
      mspec_4_ispack(0) =   1
      mspec_4_ispack(nth/m_folding) = 2
      do m = 1, nth/m_folding-1
        mspec_4_ispack( m) = 2*m+1
        mspec_4_ispack(-m) = 2*m+2
      end do
!
      do m = -nth/m_folding+1, nth/m_folding
        mm = mspec_4_ispack(m)
        mdx_ispack(mm) = m
      end do
!
      end subroutine set_wavenumber_4_ispack_fft
!
! -----------------------------------------------------------------------
!
      subroutine set_zonal_wavenum_4_legendre(ndomain_m,                &
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
      end subroutine set_zonal_wavenum_4_legendre
!
! -----------------------------------------------------------------------
!
      subroutine set_merged_index_4_sph_trans(ndomain_m, ltr, jmax,     &
     &          nph, m_folding, istack_m, mdx_4_lgd, nidx_ml,           &
     &          istack_ml, jdx_lag)
!
      integer(kind = kint), intent(in) :: ndomain_m, ltr, jmax
      integer(kind = kint), intent(in) :: nph, m_folding
      integer(kind = kint), intent(in) :: istack_m(0:ndomain_m)
      integer(kind = kint), intent(in) :: mdx_4_lgd(0:nph)
      integer(kind = kint), intent(inout) :: jdx_lag(0:jmax,3)
      integer(kind = kint), intent(inout) :: nidx_ml(ndomain_m)
      integer(kind = kint), intent(inout) :: istack_ml(0:ndomain_m)
!
      integer(kind = kint) :: ip, mst, med
      integer(kind = kint) :: j, l, m, mm, ls, ll, ms
!
      nidx_ml = 0
      istack_ml(0) = -1
      j = -1
      do ip = 1, ndomain_m
        mst = istack_m(ip-1) + 1
        med = istack_m(ip)
        do mm = mst, med
          m = mdx_4_lgd(mm)*m_folding
          ms = m / m_folding
          do l = abs(m), ltr
            j = j + 1
!
            ls = mod(l,m_folding)
            ll = (l - ls)/m_folding
!
            jdx_lag(j,2) = l
            jdx_lag(j,3) = m
            jdx_lag(j,1) = m_folding*ll**2 + ls*(2*ll+1) + ll + ms
            nidx_ml(ip) = nidx_ml(ip) + 1
          end do
        end do
      end do
!
      istack_ml(0) = -1
      do ip = 1, ndomain_m
        istack_ml(ip) = istack_ml(ip-1) + nidx_ml(ip)
      end do
!
      end subroutine set_merged_index_4_sph_trans
!
! -----------------------------------------------------------------------
!
      subroutine set_merged_index_4_sph_rj(ndomain_r, ndomain_m,        &
     &          ndomain_rj, jmax, istack_ml, jdx_lag,                   &
     &          nidx_j, istack_j, jdx_rj)
!
      use set_indices_4_sph_tranform
!
      integer(kind = kint), intent(in) :: ndomain_r, ndomain_m
      integer(kind = kint), intent(in) :: ndomain_rj, jmax
      integer(kind = kint), intent(inout) :: istack_ml(0:ndomain_m)
      integer(kind = kint), intent(in) :: jdx_lag(0:jmax,3)
!
      integer(kind = kint), intent(inout) :: nidx_j(ndomain_rj)
      integer(kind = kint), intent(inout) :: istack_j(0:ndomain_rj)
      integer(kind = kint), intent(inout) :: jdx_rj(0:jmax,3)
!
      integer(kind = kint) :: ip, ip_lm, ip_r, ist, ied, icou, inum
      integer(kind = kint), allocatable :: num_tmp(:)
      integer(kind = kint), allocatable :: istack_tmp(:)
!
!
      allocate(num_tmp(ndomain_r))
      allocate(istack_tmp(0:ndomain_r))
      num_tmp = 0
      istack_tmp = 0
!
      do ip_lm = 1, ndomain_m
        ist = istack_ml(ip_lm-1) + 1
        ied = istack_ml(ip_lm  )
        call cal_local_nums(ndomain_r, ist, ied,                   &
     &      num_tmp, istack_tmp)
!
        do ip_r = 1, ndomain_r
          ip = ip_lm + (ip_r-1) * ndomain_m
          nidx_j(ip) = num_tmp(ip_r)
        end do
      end do
!
      istack_j(0) = -1
      do ip_r = 1, ndomain_r
        do ip_lm = 1, ndomain_m
          ip = ip_lm + (ip_r-1) * ndomain_m
          istack_j(ip) = istack_j(ip-1) + nidx_j(ip)
        end do
      end do
!
      do ip_lm = 1, ndomain_m
        icou = istack_ml(ip_lm-1)
        do ip_r = 1, ndomain_r
          ip = ip_lm + (ip_r-1) * ndomain_m
          ist = istack_j(ip-1) + 1
          ied = istack_j(ip)
          do inum = ist, ied
            icou = icou + 1
            jdx_rj(inum,1) = jdx_lag(icou,1)
            jdx_rj(inum,2) = jdx_lag(icou,2)
            jdx_rj(inum,3) = jdx_lag(icou,3)
          end do
        end do
      end do
!
      deallocate(num_tmp, istack_tmp)
!
      end subroutine set_merged_index_4_sph_rj
!
! -----------------------------------------------------------------------
!
      subroutine set_trans_table_fft_2_lgd(ltr, nth, nph, m_folding,    &
     &          mspec_4_ispack, jdx_fsph, mtbl_fft_2_lgd)
!
      integer(kind = kint), intent(in) :: ltr, nth, nph, m_folding
      integer(kind = kint), intent(in) :: mspec_4_ispack(-nth:nth)
      integer(kind = kint), intent(in) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mtbl_fft_2_lgd(0:nph)
!
      integer(kind = kint) :: m, m0, mm, mnum
!
      mnum = ltr/m_folding
      do m = -mnum, mnum
        m0 = mspec_4_ispack(m)
        mm = jdx_fsph(m)
        mtbl_fft_2_lgd(mm) = m0
      end do
!
      end subroutine set_trans_table_fft_2_lgd
!
! -----------------------------------------------------------------------
!
      end module set_sph_tranform_ordering
