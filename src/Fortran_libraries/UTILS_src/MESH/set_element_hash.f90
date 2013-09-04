!set_element_hash.f90
!      module set_element_hash
!
!      Written by H. Matsui on March, 2006
!
!      subroutine count_ele_hash_by_nod(numnod, numele, nnod_4_ele, ie)
!      subroutine set_element_hash_by_nod(numele, nnod_4_ele, ie)
!
!      subroutine set_border_4_hash_sph
!      subroutine count_element_hash_by_sph(numnod, numele, nnod_4_ele, &
!     &          radius, theta, phi, ie)
!      subroutine set_element_hash_by_sph(numnod, numele, nnod_4_ele,   &
!     &          radius, theta, phi, ie)
!
      module set_element_hash
!
      use m_precision
!
      use m_constants
      use m_element_hash
!
      implicit none
!
      private :: set_hash_id_by_sph
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_ele_hash_by_nod(numnod, numele, nnod_4_ele, ie)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: ihash
!
!
      inum_ele_hash = 0
      do iele = 1, numele
!
          ihash = ie(iele,1)
          inum_ele_hash(ihash) = inum_ele_hash(ihash) + 1
!
      end do
!
      istack_ele_hash = 0
      do ihash = 1, numnod
        istack_ele_hash(ihash) = istack_ele_hash(ihash-1)               &
     &                           + inum_ele_hash(ihash)
        if ( istack_ele_hash(ihash) .le. numele ) then
          iend_ele_hash = ihash
        end if
      end do
!
!
      end subroutine count_ele_hash_by_nod
!
!------------------------------------------------------------------
!
      subroutine set_element_hash_by_nod(numele, nnod_4_ele, ie)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint) :: j
      integer(kind = kint) :: iele
      integer(kind = kint) :: ihash
!
!
      inum_ele_hash = 0
      iele_hash = 0
      do iele = 1, numele
!
        ihash = ie(iele,1)
        inum_ele_hash(ihash) = inum_ele_hash(ihash) + 1
        j = iele_hash(ihash-1) + inum_ele_hash(ihash)
        iele_hash(j) = iele
!
      end do
!
      end subroutine set_element_hash_by_nod
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_border_4_hash_sph
!
      integer(kind = kint) :: k
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
!
      do k = 1, nth_ele_hash+1
        theta_4_hash(k) = pi * dble(k-1) / dble(nth_ele_hash)
      end do
!
      do k = 1, nphi_ele_hash
        theta_4_hash(k) = two * pi * dble(k-1) / dble(nphi_ele_hash)
      end do
!
      end subroutine set_border_4_hash_sph
!
!------------------------------------------------------------------
!
      subroutine count_element_hash_by_sph(numnod, numele, nnod_4_ele,  &
     &          radius, theta, phi, ie)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: theta(numnod)
      real(kind = kreal), intent(in) :: phi(numnod)
!
      integer(kind = kint) :: inod, iele, ihash
!
!
      inum_ele_hash = 0
      do iele = 1, numele
!
        inod = ie(iele,1)
        call set_hash_id_by_sph(radius(inod), theta(inod), phi(inod),   &
     &      ihash)
        inum_ele_hash(ihash) = inum_ele_hash(ihash) + 1
!
      end do
!
      istack_ele_hash = 0
      do ihash = 1, numnod
        istack_ele_hash(ihash) = istack_ele_hash(ihash-1)               &
     &                           + inum_ele_hash(ihash)
        if ( istack_ele_hash(ihash) .le. numele ) then
          iend_ele_hash = ihash
        end if
      end do
!
      end subroutine count_element_hash_by_sph
!
!------------------------------------------------------------------
!
      subroutine set_element_hash_by_sph(numnod, numele, nnod_4_ele,    &
     &          radius, theta, phi, ie)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      real(kind = kreal), intent(in) :: radius(numnod)
      real(kind = kreal), intent(in) :: theta(numnod)
      real(kind = kreal), intent(in) :: phi(numnod)
!
      integer(kind = kint) :: inod, iele, ihash, j
!
!
      inum_ele_hash = 0
      iele_hash = 0
      do iele = 1, numele
!
        inod = ie(iele,1)
        call set_hash_id_by_sph(radius(inod), theta(inod), phi(inod),   &
     &      ihash)
!
        inum_ele_hash(ihash) = inum_ele_hash(ihash) + 1
        j = iele_hash(ihash-1) + inum_ele_hash(ihash)
        iele_hash(j) = iele
!
      end do
!
      end subroutine set_element_hash_by_sph
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_hash_id_by_sph(r_ele, theta_ele, phi_ele, ihash)
!
      real(kind = kreal), intent(in) :: r_ele, theta_ele, phi_ele
      integer(kind = kint), intent(inout)  :: ihash
!
      integer(kind = kint) :: i0, i_r, j_theta, k_phi
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
!
        k_phi = int(aint( phi_ele * dble(nphi_ele_hash)                 &
     &     / (two * pi) ))
        j_theta = int( aint( theta_ele * dble(nth_ele_hash) / pi ) )
!
        i_r = 0
        do i0 = 1, nr_ele_hash-1
          if (r_ele.le.r_4_hash(i0+1)                                   &
     &       .and. r_ele.gt.r_4_hash(i0) ) then
            i_r = i0
            exit
          end if
        end do
        if (i_r .eq. 0) i_r = nr_ele_hash
!
        ihash =                                      k_phi              &
     &         + nphi_ele_hash *                    (j_theta-1)         &
     &         + nphi_ele_hash * (nth_ele_hash+1) * (i_r-1)
!
      end subroutine set_hash_id_by_sph
!
!------------------------------------------------------------------
!
      end module set_element_hash
