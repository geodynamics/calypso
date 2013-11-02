!>@file   set_indices_4_sph_tranform.f90
!!@brief  module set_indices_4_sph_tranform
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set control parameters for spherical harmonics dynamo from IO
!!
!!@verbatim
!!      subroutine cal_local_nums(n_div, ist, ied, num, istack)
!!      subroutine cal_local_nums_rev(n_div, ist, ied, num, istack)
!!      subroutine cal_local_num_rtm_m(ndomain_m, ltr, num, istack)
!!
!!      subroutine merge_num_3_local_layers(n_div, num_1, num_2, num_3, &
!!     &          ist, num, istack)
!!
!!      subroutine set_aria_id(n_div, ist, ied, n_local, ip_local)
!!
!!      subroutine set_wavenumber_4_ispack_fft(nth, nph,                &
!!     &          mspec_4_ispack, mdx_ispack)
!!        output: mspec_4_ispack, mdx_ispack
!!      subroutine set_zonal_wavenum_4_legendre(ndomain_m,              &
!!     &          ltr, nth, nph, jdx_fsph, mdx_4_lgd)
!!        output: jdx_fsph, mdx_4_lgd
!!
!!      subroutine set_merged_index_4_sph_trans(ndomain_m, ltr, jmax,   &
!!     &          nph, istack_m, mdx_4_lgd, nidx_ml, istack_ml, jdx_lag)
!!        output: nidx_ml, istack_ml, jdx_lag
!!
!!      subroutine set_trans_table_fft_2_lgd(ltr, nth, nph,             &
!!     &          mspec_4_ispack, jdx_fsph, mtbl_fft_2_lgd)
!!        output: mtbl_fft_2_lgd
!!      subroutine set_trans_table_lgd_2_sph(jmax, jtbl_fsph, jtbl_isph)
!!        output: jtbl_isph
!!@endverbatim
!
      module set_indices_4_sph_tranform
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable, private :: ip_tmp(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_local_nums(n_div, ist, ied, num, istack)
!
      integer(kind = kint), intent(in) :: n_div, ist, ied
      integer(kind = kint), intent(inout) :: num(n_div)
      integer(kind = kint), intent(inout) :: istack(0:n_div)
!
      integer(kind = kint) :: ic, ip
!
!
      ic = ist - 1
      num = 0
      do
        do ip = 1, n_div
          num(ip) = num(ip) + 1
          ic = ic + 1
          if (ic .ge. ied) exit
        end do
        if (ic .ge. ied) exit
      end do
!
      istack(0) = ist - 1
      do ip = 1, n_div
        istack(ip) = istack(ip-1) + num(ip)
      end do
!
      end subroutine cal_local_nums
!
! -----------------------------------------------------------------------
!
      subroutine cal_local_nums_st(n_div, ist, ied, num, ist_list)
!
      integer(kind = kint), intent(in) :: n_div, ist, ied
      integer(kind = kint), intent(inout) :: num(n_div)
      integer(kind = kint), intent(inout) :: ist_list(n_div)
!
      integer(kind = kint) :: ic, ip
!
!
      ic = ist - 1
      num = 0
      do
        do ip = 1, n_div
          num(ip) = num(ip) + 1
          ic = ic + 1
          if (ic .ge. ied) exit
        end do
        if (ic .ge. ied) exit
      end do
!
      ist_list(1) = ist - 1
      do ip = 2, n_div
        ist_list(ip) = ist_list(ip-1) + num(ip-1)
      end do
!
      end subroutine cal_local_nums_st
!
! -----------------------------------------------------------------------
!
      subroutine cal_local_nums_rev(n_div, ist, ied, num, ist_list)
!
      integer(kind = kint), intent(in) :: n_div, ist, ied
      integer(kind = kint), intent(inout) :: num(n_div)
      integer(kind = kint), intent(inout) :: ist_list(n_div)
!
      integer(kind = kint) :: ic, ip
!
!
      ic = ist - 1
      num = 0
      do
        do ip = n_div, 1, -1
          num(ip) = num(ip) + 1
          ic = ic + 1
          if (ic .ge. ied) exit
        end do
        if (ic .ge. ied) exit
      end do
!
      ist_list(n_div) = ist - 1
      do ip = n_div-1, 1, -1
        ist_list(ip) = ist_list(ip+1) + num(ip+1)
      end do
!
      end subroutine cal_local_nums_rev
!
! ----------------------------------------------------------------------
!
      subroutine cal_local_num_rtm_m(ndomain_m, ltr, num, istack)
!
      integer(kind = kint), intent(in) :: ndomain_m, ltr
      integer(kind = kint), intent(inout) :: num(ndomain_m)
      integer(kind = kint), intent(inout) :: istack(0:ndomain_m)
!
      integer(kind = kint) :: m, ip, imark, ltr_half
!
!
      m = -1
      num = 0
!
      ltr_half = ( ltr-mod(ltr,2) ) / 2
      ip = 1
      imark = 1
!
      do m = 1, ltr_half
        num(ip) = num(ip) + 2
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
      num(ip) = num(ip) + 1
      ip = ip + imark
      if (ip .gt. ndomain_m) then
        ip =    ndomain_m
        imark = -1
      else if (ip .lt. 1) then
        ip = 1
        imark =  1
      end if
!
      do m = ltr_half+1, ltr
        num(ip) = num(ip) + 2
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
      istack(0) = -1
      do ip = 1, ndomain_m
        istack(ip) = istack(ip-1) + num(ip)
      end do
!
!      write(8,*) 'num(1:ndomain_m)', num(1:ndomain_m)
!
      end subroutine cal_local_num_rtm_m
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine merge_num_3_local_layers(n_div, num_1, num_2, num_3,   &
     &          ist, num, istack)
!
      integer(kind = kint), intent(in) :: n_div, ist
      integer(kind = kint), intent(in) :: num_1(n_div)
      integer(kind = kint), intent(in) :: num_2(n_div)
      integer(kind = kint), intent(in) :: num_3(n_div)
!
      integer(kind = kint), intent(inout) :: num(n_div)
      integer(kind = kint), intent(inout) :: istack(0:n_div)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, n_div
        num(ip) =  num_1(ip) + num_2(ip) + num_3(ip)
      end do
!
      istack(0) = ist - 1
      do ip = 1, n_div
        istack(ip) =  istack(ip-1)  + num(ip)
      end do
!
      end subroutine merge_num_3_local_layers
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_aria_id(n_div, ist, ied, n_local, ip_local)
!
      integer(kind = kint), intent(in) :: n_div, ist, ied
      integer(kind = kint), intent(in) :: n_local(n_div)
      integer(kind = kint), intent(inout) :: ip_local(ist:ied)
!
      integer(kind = kint) :: j, jj, ip
!
!
      j = ist-1
      do ip = 1, n_div
        do jj = 1, n_local(ip)
          j = j + 1
          ip_local(j) = ip
        end do
      end do
!
!
      end subroutine set_aria_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_wavenumber_4_ispack_fft(nth, nph,                  &
     &          mspec_4_ispack, mdx_ispack)
!
      integer(kind = kint), intent(in) :: nth, nph
      integer(kind = kint), intent(inout) :: mspec_4_ispack(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_ispack(nph)
      integer(kind = kint) :: m, mm
!
      mspec_4_ispack(0) =   1
      mspec_4_ispack(nth) = 2
      do m = 1, nth-1
        mspec_4_ispack( m) = 2*m+1
        mspec_4_ispack(-m) = 2*m+2
      end do
!
      do m = -nth+1, nth
        mm = mspec_4_ispack(m)
        mdx_ispack(mm) = m
      end do
!
      end subroutine set_wavenumber_4_ispack_fft
!
! -----------------------------------------------------------------------
!
      subroutine set_zonal_wavenum_4_legendre(ndomain_m,                &
     &          ltr, nth, nph, jdx_fsph, mdx_4_lgd)
!
      integer(kind = kint), intent(in) :: ltr, nth, nph, ndomain_m
      integer(kind = kint), intent(inout) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mdx_4_lgd(0:nph)
!
      integer(kind = kint) :: m, ip, mm, imark, ltr_half
!
!
      allocate( ip_tmp(-ltr:ltr) )
      ip_tmp = 0
!
      ip = 1
      imark = 1
      ltr_half = ( ltr-mod(ltr,2) ) / 2
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
      ip_tmp(0) = ip
      ip = ip + imark
      if (ip .gt. ndomain_m) then
        ip =    ndomain_m
        imark = -1
      else if (ip .lt. 1) then
        ip = 1
        imark =  1
      end if
!
      do m = ltr_half+1, ltr
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
        do m = -ltr, ltr
          if (ip_tmp(m) .eq. ip) then
            mm = mm + 1
            jdx_fsph(m) = mm
          end if
        end do
      end do
!
      do m = -ltr, ltr
        mm = jdx_fsph(m)
        mdx_4_lgd(mm) = m
      end do
!
      deallocate( ip_tmp )
!
!        write(8,*) 'm, jdx_fsph(m)'
!      do m = -ltr, ltr
!        write(8,*) m, jdx_fsph(m)
!      end do
!        write(8,*) 'mm, mdx_4_lgd(mm)'
!      do mm = 0,nph
!        write(8,*) mm, mdx_4_lgd(mm)
!      end do
!
!
      end subroutine set_zonal_wavenum_4_legendre
!
! -----------------------------------------------------------------------
!
      subroutine set_merged_index_4_sph_trans(ndomain_m, ltr, jmax,     &
     &          nph, istack_m, mdx_4_lgd, nidx_ml, istack_ml, jdx_lag)
!
      integer(kind = kint), intent(in) :: ndomain_m, ltr, jmax, nph
      integer(kind = kint), intent(in) :: istack_m(0:ndomain_m)
      integer(kind = kint), intent(in) :: mdx_4_lgd(0:nph)
      integer(kind = kint), intent(inout) :: jdx_lag(0:jmax,3)
      integer(kind = kint), intent(inout) :: nidx_ml(ndomain_m)
      integer(kind = kint), intent(inout) :: istack_ml(0:ndomain_m)
!
      integer(kind = kint) :: ip, mst, med
      integer(kind = kint) :: j, l, m, mm
!
      nidx_ml = 0
      istack_ml(0) = -1
      j = -1
      do ip = 1, ndomain_m
        mst = istack_m(ip-1) + 1
        med = istack_m(ip)
        do mm = mst, med
          m = mdx_4_lgd(mm)
          do l= abs(m), ltr
            j = j + 1
            jdx_lag(j,2) = l
            jdx_lag(j,3) = m
            jdx_lag(j,1) = jdx_lag(j,2) * (jdx_lag(j,2)+1)              &
     &                    + jdx_lag(j,3)
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
      subroutine set_trans_table_fft_2_lgd(ltr, nth, nph,               &
     &          mspec_4_ispack, jdx_fsph, mtbl_fft_2_lgd)
!
      integer(kind = kint), intent(in) :: ltr, nth, nph
      integer(kind = kint), intent(in) :: mspec_4_ispack(-nth:nth)
      integer(kind = kint), intent(in) :: jdx_fsph(-nth:nth)
      integer(kind = kint), intent(inout) :: mtbl_fft_2_lgd(0:nph)
!
      integer(kind = kint) :: m, m0, mm
!
      do m = -ltr, ltr
        m0 = mspec_4_ispack(m)
        mm = jdx_fsph(m)
        mtbl_fft_2_lgd(mm) = m0
      end do
!
      end subroutine set_trans_table_fft_2_lgd
!
! -----------------------------------------------------------------------
!
      subroutine set_trans_table_lgd_2_sph(jmax, jtbl_fsph, jtbl_isph)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: jtbl_fsph(0:jmax)
      integer(kind = kint), intent(inout) :: jtbl_isph(0:jmax)
!
      integer(kind = kint) :: j, jj
!
      do j = 0, jmax
        jj = jtbl_fsph(j)
        jtbl_isph(jj) = j
      end do
!
      end subroutine set_trans_table_lgd_2_sph
!
! -----------------------------------------------------------------------
!
      end module set_indices_4_sph_tranform
