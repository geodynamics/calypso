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
!!      subroutine cal_local_num_rtm_m(ndomain_m, ltr, m_folding,       &
!!     &          num, istack)
!!
!!      subroutine merge_num_3_local_layers(n_div, num_1, num_2, num_3, &
!!     &          ist, num, istack)
!!
!!      subroutine set_aria_id(n_div, ist, ied, n_local, ip_local)
!!@endverbatim
!
      module set_indices_4_sph_tranform
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
      subroutine cal_local_num_rtm_m(ndomain_m, ltr, m_folding,         &
     &          num, istack)
!
      integer(kind = kint), intent(in) :: ndomain_m, ltr, m_folding
      integer(kind = kint), intent(inout) :: num(ndomain_m)
      integer(kind = kint), intent(inout) :: istack(0:ndomain_m)
!
      integer(kind = kint) :: m, ip, imark, ltr_half
!
!
      m = -1
      num = 0
!
      ip = 1
      imark = 1
      ltr_half = ( ltr-mod(ltr,2) ) / (2*m_folding)
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
      do m = ltr_half+1, ltr/m_folding
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
!
      end module set_indices_4_sph_tranform
