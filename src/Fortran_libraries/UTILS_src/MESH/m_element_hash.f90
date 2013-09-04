!
!      module m_element_hash
!
!      Written by H. Matsui on March, 2006
!
      module m_element_hash
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) :: nr_ele_hash, nth_ele_hash, nphi_ele_hash
      integer(kind=kint ) :: ncomp_ele_hash
      integer(kind=kint ) :: iend_ele_hash
      integer(kind=kint ), allocatable :: inum_ele_hash(:)
      integer(kind=kint ), allocatable :: istack_ele_hash(:)
      integer(kind=kint ), allocatable :: iele_hash(:)
      integer(kind=kint ), allocatable :: iele_flag(:)
!
      real(kind = kreal), allocatable :: theta_4_hash(:)
      real(kind = kreal), allocatable :: phi_4_hash(:)
      real(kind = kreal), allocatable :: r_4_hash(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_hash_by_nod(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
!
      ncomp_ele_hash = numnod
!
      allocate(inum_ele_hash(ncomp_ele_hash))
      allocate(istack_ele_hash(0:ncomp_ele_hash))
      allocate(iele_hash(numele) )
      allocate(iele_flag(numele) )
      inum_ele_hash = 0
      istack_ele_hash = 0
      iele_hash = 0
      iele_flag = 0
!
      end subroutine allocate_ele_hash_by_nod
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_hash_by_shell(numnod, numele)
!
      integer(kind = kint), intent(in) :: numnod, numele
!
!
      nphi_ele_hash = 2*nth_ele_hash
      ncomp_ele_hash = nr_ele_hash*nth_ele_hash*nphi_ele_hash
!
      allocate(inum_ele_hash(ncomp_ele_hash))
      allocate(istack_ele_hash(0:ncomp_ele_hash))
      allocate(iele_hash(numele) )
      allocate(iele_flag(numele) )
!
      allocate(r_4_hash(nr_ele_hash))
      allocate(theta_4_hash(nth_ele_hash+1))
      allocate(phi_4_hash(nphi_ele_hash))
!
      inum_ele_hash = 0
      istack_ele_hash = 0
      iele_hash = 0
      iele_flag = 0
!
      r_4_hash = 0.0d0
      theta_4_hash = 0.0d0
      phi_4_hash = 0.0d0
!
      end subroutine allocate_ele_hash_by_shell
!
!------------------------------------------------------------------
!
      subroutine deallocate_elment_hash
!
      deallocate(iele_hash)
      deallocate(iele_flag)
      deallocate(inum_ele_hash)
      deallocate(istack_ele_hash)
!
      end subroutine deallocate_elment_hash
!
!------------------------------------------------------------------
!
      subroutine deallocate_position_4_hash
!
      deallocate(r_4_hash)
      deallocate(theta_4_hash)
      deallocate(phi_4_hash)
!
      end subroutine deallocate_position_4_hash
!
!------------------------------------------------------------------
!
      end module m_element_hash
