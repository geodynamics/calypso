!>@file  count_overlap.f90
!!       module count_overlap
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Sep., 2006
!
!> @brief Check overlapped element
!!
!!@verbatim
!!      subroutine set_overlap_flag(np_smp, inum_smp_stack,             &
!!     &          internal_node, numele, ie, internal_n, interior_flag)
!!
!!      subroutine set_original_domiain_by_comm(my_rank, nnod,          &
!!     &          num_neib, ntot_import, id_neib, istack_import,        &
!!     &          item_import, idomain_nod)
!!      subroutine set_original_domiain_by_node(nnod, nele, ie,         &
!!     &          idomain_nod, idomain_ele)
!!@endverbatim
!
      module count_overlap
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_overlap_flag(np_smp, inum_smp_stack,               &
     &          internal_node, numele, ie, internal_n, interior_flag)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: inum_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: internal_node, numele
      integer(kind = kint), intent(in) :: ie(numele,1)
!
      integer(kind = kint), intent(inout) :: internal_n
      integer(kind = kint), intent(inout) :: interior_flag(numele)
!
      integer (kind = kint) :: ip, inod, inum
!
!$omp parallel do private(inum, inod)
      do ip = 1, np_smp
        do inum = inum_smp_stack(ip-1)+1, inum_smp_stack(ip)
          inod = ie(inum,1)
          if (inod .le. internal_node) then
            interior_flag(inum) = 1
          else
            interior_flag(inum) = 0
          end if
        end do
      end do
!$omp end parallel do
!
      internal_n = 0
      do inum = 1, numele
        internal_n = internal_n + interior_flag(inum)
      end do
!
      end subroutine set_overlap_flag
!
! ----------------------------------------------------------------------
!
      subroutine set_original_domiain_by_comm(my_rank, nnod,            &
     &          num_neib, ntot_import, id_neib, istack_import,          &
     &          item_import, idomain_nod)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_neib, ntot_import
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in) :: item_import(ntot_import)
!
      integer(kind = kint), intent(inout) :: idomain_nod(nnod)
!
      integer (kind = kint) :: ip, ist, ied, inum, inod
!
!
      idomain_nod(1:nnod) = -1
!
      do ip = 1, num_neib
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        do inum = ist, ied
          inod = item_import(inum)
          idomain_nod(inod) = id_neib(inum)
        end do
      end do
!
      do inod = 1, nnod
        if(idomain_nod(inod) .eq. -1) idomain_nod(inod) = my_rank
      end do
!
      end subroutine set_original_domiain_by_comm
!
! ----------------------------------------------------------------------
!
      subroutine set_original_domiain_by_node(nnod, nele, ie,           &
     &          idomain_nod, idomain_ele)
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: ie(nele,1)
      integer(kind = kint), intent(in) :: idomain_nod(nnod)
!
      integer(kind = kint), intent(inout) :: idomain_ele(nele)
!
      integer (kind = kint) :: inod, iele
!
!$omp parallel do private(inod)
      do iele = 1, nele
        inod = ie(iele,1)
        idomain_ele(iele) = idomain_nod(inod)
      end do
!$omp end parallel do
!
      end subroutine set_original_domiain_by_node
!
! ----------------------------------------------------------------------
!
      end module count_overlap
