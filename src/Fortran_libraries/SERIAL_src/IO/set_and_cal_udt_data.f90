!>@file  set_and_cal_udt_data.f90
!!       module set_and_cal_udt_data
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui on July, 2006
!!@n           Modified by H.Matsui on July, 2013
!
!>@brief set mesh and field data from UCD data input
!!
!!@verbatim
!!      subroutine const_udt_local_nodes(numnod, xx, ucd)
!!      subroutine const_udt_local_connect(internal_node,               &
!!     &          nele, nnod_ele, ie, ucd)
!!      subroutine const_udt_global_connect(internal_node,              &
!!     &          nele, nnod_ele, iele_global, ie, ucd)
!!
!!      subroutine set_one_field_to_udt_data(nnod, numdir, i_field,     &
!!     &          d_nod, ucd)
!!      subroutine set_one_field_by_udt_data(nnod, numdir, i_field,     &
!!     &          d_nod, ucd)
!!
!!      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,       &
!!     &          istack_comp, phys_name, d_nod, ucd)
!!      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,       &
!!     &          istack_comp, phys_name, d_nod, ucd)
!!      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,  &
!!     &          istack_comp, phys_name, d_nod, ucd)
!!
!!      subroutine find_field_id_in_ucd(ucd, field_name)
!!@endverbatim
!
      module set_and_cal_udt_data
!
      use m_precision
      use t_ucd_data
!
      implicit none
!
      private :: set_udt_local_nodes
      private :: set_udt_local_connect
      private :: set_udt_global_connect
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_udt_local_nodes(numnod, xx, ucd)
!
      integer(kind=kint), intent(in)  :: numnod
      real(kind=kreal), intent(in)  :: xx(numnod,3)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call allocate_ucd_node(ucd)
      call set_udt_local_nodes(numnod, xx, ucd)
!
      end subroutine const_udt_local_nodes
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_udt_local_connect(internal_node,                 &
     &          nele, nnod_ele, ie, ucd)
!
      use count_overlap
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod_4_ele = nnod_ele
      ucd%nele = count_interier_element(internal_node, nele, ie(1,1))
      call allocate_ucd_ele(ucd)
!
      call set_udt_local_connect(internal_node, nele, nnod_ele,         &
     &    ie, ucd)
!
      end subroutine const_udt_local_connect
!
!-----------------------------------------------------------------------
!
      subroutine const_udt_global_connect(internal_node,                &
     &          nele, nnod_ele, iele_global, ie, ucd)
!
      use count_overlap
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint_gl), intent(in)  :: iele_global(nele)
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod_4_ele = nnod_ele
      ucd%nele = count_interier_element(internal_node, nele, ie(1,1))
      call allocate_ucd_ele(ucd)
!
      call set_udt_global_connect(internal_node, nele, nnod_ele,        &
     &    iele_global, ie, ucd)
!
      end subroutine const_udt_global_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_nodes(numnod, xx, ucd)
!
      integer(kind=kint), intent(in)  :: numnod
      real(kind=kreal), intent(in)  :: xx(numnod,3)
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, numnod
        ucd%inod_global(inod) = inod
        ucd%xx(inod,1) = xx(inod,1)
        ucd%xx(inod,2) = xx(inod,2)
        ucd%xx(inod,3) = xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine set_udt_local_nodes
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_udt_local_connect(internal_node,                   &
     &          nele, nnod_ele, ie, ucd)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: iele, k1, icou
!
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          ucd%iele_global(icou) = icou
          do k1 = 1, nnod_ele
            ucd%ie(icou,k1) = ie(iele,k1)
          end do
        end if
      end do
!
      end subroutine set_udt_local_connect
!
!-----------------------------------------------------------------------
!
      subroutine set_udt_global_connect(internal_node,                  &
     &          nele, nnod_ele, iele_global, ie, ucd)
!
      integer(kind=kint), intent(in)  :: internal_node
      integer(kind=kint), intent(in)  :: nele, nnod_ele
      integer(kind=kint_gl), intent(in)  :: iele_global(nele)
      integer(kind=kint), intent(in)  :: ie(nele, nnod_ele)
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: iele, k1, inod, icou
!
!
      icou = 0
      do iele = 1, nele
        if(ie(iele,1) .le. internal_node) then
          icou = icou + 1
          ucd%iele_global(icou) = iele_global(iele)
          do k1 = 1, nnod_ele
            inod = ie(iele,k1)
            ucd%ie(icou,k1) = ucd%inod_global(inod)
          end do
        end if
      end do
!
      end subroutine set_udt_global_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_one_field_to_udt_data(nnod, numdir, i_field,       &
     &          d_nod, ucd)
!
      integer(kind=kint), intent(in)  :: nnod, i_field, numdir
      real(kind = kreal), intent(in) :: d_nod(nnod,numdir)
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: inod, nd
!
!
      do nd = 1, numdir
!$omp parallel do
       do inod = 1, nnod
         ucd%d_ucd(inod,nd+i_field-1) = d_nod(inod,nd)
       end do
!$omp end parallel do
      end do
!
      end subroutine set_one_field_to_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine set_one_field_by_udt_data(nnod, numdir, i_field,       &
     &          d_nod, ucd)
!
      type(ucd_data), intent(in) :: ucd
!
      integer(kind=kint), intent(in)  :: nnod, i_field, numdir
      real(kind = kreal), intent(inout) :: d_nod(nnod,numdir)
!
      integer(kind = kint) :: inod, nd
!
!
      do nd = 1, numdir
!$omp parallel do
       do inod = 1, nnod
         d_nod(inod,nd) = ucd%d_ucd(inod,nd+i_field-1)
       end do
!$omp end parallel do
      end do
!
      end subroutine set_one_field_by_udt_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          istack_comp, phys_name, d_nod, ucd)
!
      type(ucd_data), intent(in) :: ucd
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: j, i_fld, j_fld, ncomp
!
!
      do j = 1, num_fld
        j_fld = istack_comp(j-1) + 1
        call find_field_id_in_ucd(ucd, phys_name(j), i_fld, ncomp)
        if(i_fld .gt. 0) then
          do nd = 0, ncomp-1
!$omp parallel do
            do inod = 1, nnod
              d_nod(inod,j_fld+nd) = ucd%d_ucd(inod,i_fld+nd)
            end do
!$omp end parallel do
          end do
        end if
      end do
!
      end subroutine set_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine add_field_by_udt_data(nnod, num_fld, ntot_cmp,         &
     &          istack_comp, phys_name, d_nod, ucd)
!
      type(ucd_data), intent(in) :: ucd
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: j, i_fld, j_fld, ncomp
!
!
      do j = 1, num_fld
        j_fld = istack_comp(j-1) + 1
        call find_field_id_in_ucd(ucd, phys_name(j), i_fld, ncomp)
        if(i_fld .gt. 0) then
          do nd = 0, ncomp-1
!$omp parallel do
            do inod = 1, nnod
              d_nod(inod,j_fld+nd) = d_nod(inod,j_fld+nd)             &
     &                              + ucd%d_ucd(inod,i_fld+nd)
            end do
!$omp end parallel do
          end do
        end if
      end do
!
      end subroutine add_field_by_udt_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_field_by_udt_data(nnod, num_fld, ntot_cmp,    &
     &          istack_comp, phys_name, d_nod, ucd)
!
      type(ucd_data), intent(in) :: ucd
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_cmp
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_cmp)
!
      integer(kind = kint) :: inod, nd
      integer (kind = kint) :: j, i_fld, j_fld, ncomp
!
!
      do j = 1, num_fld
        j_fld = istack_comp(j-1) + 1
        call find_field_id_in_ucd(ucd, phys_name(j), i_fld, ncomp)
        if(i_fld .gt. 0) then
          do nd = 0, ncomp-1
!$omp parallel do
            do inod = 1, nnod
              d_nod(inod,j_fld+nd) = d_nod(inod,j_fld+nd)             &
     &                              - ucd%d_ucd(inod,i_fld+nd)
            end do
!$omp end parallel do
          end do
        end if
      end do
!
      end subroutine subtract_field_by_udt_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_field_id_in_ucd(ucd, field_name, i_field, ncomp)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(inout) :: i_field, ncomp
      integer(kind = kint) :: i, istack
!
      type(ucd_data), intent(in) :: ucd
!
!
      i_field = 0
      istack =  0
      do i = 1, ucd%num_field
        if (field_name .eq. ucd%phys_name(i) ) then
          i_field = istack + 1
          ncomp = ucd%num_comp(i)
          exit
        end if
        istack = istack + ucd%num_comp(i)
      end do
!
      end subroutine find_field_id_in_ucd
!
!-----------------------------------------------------------------------
!
      end module set_and_cal_udt_data
