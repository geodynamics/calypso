!set_and_cal_udt_data.f90
!      module set_and_cal_udt_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine set_udt_local_nodes(numnod, xx)
!
!      subroutine count_udt_elements(internal_node, nele, nnod_ele, ie)
!      subroutine set_udt_local_connect(internal_node,                  &
!     &          nele, nnod_ele, ie)
!      subroutine set_udt_global_connect(internal_node,                 &
!     &          nele, nnod_ele, iele_global, ie)
!
!      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,        &
!     &          num_comp, phys_name, d_nod)
!      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,        &
!     &          num_comp, phys_name, d_nod)
!      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,   &
!     &          num_comp, phys_name, d_nod)
!
      module set_and_cal_udt_data
!
      use m_precision
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_nodes(numnod, xx)
!
      integer(kind=kint), intent(in)  :: numnod
      real(kind=kreal), intent(in)  :: xx(numnod,3)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
        inod_gl_ucd(inod) = inod
        xx_ucd(inod,1) = xx(inod,1)
        xx_ucd(inod,2) = xx(inod,2)
        xx_ucd(inod,3) = xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine set_udt_local_nodes
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_udt_elements(internal_node, nele, nnod_ele, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele
!
!
      nnod_4_ele_ucd = nnod_ele
!
      nele_ucd = 0
!$omp parallel do reduction(+:nele_ucd)
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) nele_ucd = nele_ucd + 1
      end do
!$omp end parallel do
!
      end subroutine count_udt_elements
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_connect(internal_node,                   &
     &          nele, nnod_ele, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele, k1, icou
!
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          iele_gl_ucd(icou) = icou
          do k1 = 1, nnod_ele
            ie_ucd(icou,k1) = ie(iele,k1)
          end do
        end if
      end do
!
      end subroutine set_udt_local_connect
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_global_connect(internal_node,                  &
     &          nele, nnod_ele, iele_global, ie)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: iele_global(nele)
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      integer(kind = kint) :: iele, k1, inod, icou
!
!
      call count_udt_elements(internal_node, nele, nnod_ele, ie)
      call allocate_ucd_ele
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          iele_gl_ucd(icou) = iele_global(iele)
          do k1 = 1, nnod_ele
            inod = ie(iele,k1)
            ie_ucd(icou,k1) = inod_gl_ucd(inod)
          end do
        end if
      end do
!
      end subroutine set_udt_global_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, num_field_ucd
        jcomp = 0
        do j = 1, num_fld
          if ( phys_name_ucd(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = d_nod_ucd(inod,icomp+nd)
              end do
            end do
            exit
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + num_comp_ucd(i)
      end do
!
      end subroutine set_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, num_field_ucd
        jcomp = 0
        do j = 1, num_fld
          if ( phys_name_ucd(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = d_nod(inod,jcomp+nd)             &
     &                                + d_nod_ucd(inod,icomp+nd)
              end do
            end do
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + num_comp_ucd(i)
      end do
!
      end subroutine add_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,    &
     &          num_comp, phys_name, d_nod)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: num_comp(num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: i, j, icomp, jcomp
!
!
      icomp = 0
      do i = 1, num_field_ucd
        jcomp = 0
        do j = 1, num_fld
          if ( phys_name_ucd(i) .eq. phys_name(j) ) then
            do nd = 1, num_comp(j)
              do inod = 1, nnod
                d_nod(inod,jcomp+nd) = d_nod(inod,jcomp+nd)             &
     &                                - d_nod_ucd(inod,icomp+nd)
              end do
            end do
          end if
          jcomp = jcomp + num_comp(j)
        end do
        icomp = icomp + num_comp_ucd(i)
      end do
!
      end subroutine subtract_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      end module set_and_cal_udt_data
