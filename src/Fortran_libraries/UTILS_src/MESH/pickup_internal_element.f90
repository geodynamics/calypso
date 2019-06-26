!>@file   pickup_internal_element.f90
!!@brief  module pickup_internal_element
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!
!>@brief  Subroutines to read control arrays
!!
!!@verbatim
!!      integer(kind = kint) function num_internal_element_4_IO         &
!!     &                   (internal_node, numele, nnod_4_ele, ie)
!!      subroutine set_internal_element_4_IO(nprocs, istack_internod,   &
!!     &          numnod, internal_node, numele, nnod_4_ele, ie,        &
!!     &          inod_local, irank_home, nele_IO, nnod_4_ele_IO, ie_IO)
!!@endverbatim
!!
      module pickup_internal_element
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
      integer(kind = kint) function num_internal_element_4_IO           &
     &                   (internal_node, numele, nnod_4_ele, ie)
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: iele, nele_IO
!
!
      nele_IO = 0
      do iele = 1, numele
        if(ie(iele,1) .le. internal_node) nele_IO = nele_IO + 1
      end do
      num_internal_element_4_IO = nele_IO
!
      end function num_internal_element_4_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_internal_element_4_IO(nprocs, istack_internod,     &
     &          numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_local, irank_home, nele_IO, nnod_4_ele_IO, ie_IO)
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: inod_local(numnod)
      integer(kind = kint), intent(in) :: irank_home(numnod)
!
      integer(kind = kint_gl), intent(in) :: nele_IO
      integer(kind = kint), intent(in) :: nnod_4_ele_IO
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: ie_IO(nele_IO,nnod_4_ele_IO)
!
      integer(kind = kint) :: iele, icou, k1, inod, i_lc, irank
!
!
      icou = 0
      do iele = 1, numele
        if(ie(iele,1) .gt. internal_node) cycle
!
        icou = icou + 1
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          i_lc =  inod_local(inod)
          irank = irank_home(inod)
          ie_IO(icou,k1) = i_lc + istack_internod(irank)
        end do
      end do
!
      end subroutine set_internal_element_4_IO
!
! -----------------------------------------------------------------------
!
      end module pickup_internal_element
