!>@file   send_recv_loop_tests.f90
!!@brief  module send_recv_loop_tests
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!!     Modified in  Apr., 2008
!!     Modified in  Nov., 2018
!
!>@brief Routines for communication tests
!!
!!@verbatim
!!      subroutine copy_to_send_by_comm_table1(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_comm_table1(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_rev_table1                         &
!!     &         (node, nod_comm, NB, irev_import, xx4)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!
!!      subroutine copy_to_send_by_comm_lgloop1(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_comm_lgloop1                       &
!!     &         (node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_rev_lgloop1                        &
!!     &         (node, nod_comm, NB, irev_import, xx4)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!
!!      subroutine copy_to_send_by_comm_table2(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_comm_table2(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_rev_table2                         &
!!     &         (node, nod_comm, NB, irev_import, xx4)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!
!!      subroutine copy_to_send_by_comm_lgloop2(node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_comm_lgloop2                       &
!!     &         (node, nod_comm, NB, xx4)
!!      subroutine copy_from_recv_by_rev_lgloop2                        &
!!     &         (node, nod_comm, NB, irev_import, xx4)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!@endverbatim
!
      module send_recv_loop_tests
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_by_comm_table1(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        ied = nod_comm%istack_export(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
                 ii   = NB * (nod_comm%item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             SR_r1%WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
!
      end subroutine copy_to_send_by_comm_table1
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_comm_table1(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (nod_comm%item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= SR_r1%WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_comm_table1
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_rev_table1                           &
     &         (node, nod_comm, NB, irev_import, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in)                                  &
     &     :: irev_import(nod_comm%istack_import(nod_comm%num_neib))
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= ist+1, ied
            ii   = NB * (node%internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= SR_r1%WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_rev_table1
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_by_comm_lgloop1(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        ied = nod_comm%istack_export(neib  )
!$omp do private(k,nd,ii,ix)
        do k= ist+1, ied
          do nd = 1, NB
                 ii   = NB * (nod_comm%item_export(k)-1) + nd
                 ix   = NB * (k-1) + nd
             SR_r1%WS(ix)= xx4(ii)
           end do
         end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_to_send_by_comm_lgloop1
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_comm_lgloop1                         &
     &         (node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1) 
        ied = nod_comm%istack_import(neib  )
!$omp do private(k,nd,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (nod_comm%item_import(k)-1) + nd
            ix   = NB * (k-1) + nd
            xx4(ii)= SR_r1%WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_comm_lgloop1
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_rev_lgloop1                          &
     &         (node, nod_comm, NB, irev_import, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in)                                  &
     &     :: irev_import(nod_comm%istack_import(nod_comm%num_neib))
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, ied
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,ied)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        ied = nod_comm%istack_import(neib  )
!$omp do private(k,ii,ix)
        do nd = 1, NB
          do k= ist+1, ied
            ii   = NB * (node%internal_node+k-1) + nd
            ix   = NB * (irev_import(k)-1) + nd
            xx4(ii)= SR_r1%WR(ix)
          end do
        enddo
!$omp end do nowait
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_rev_lgloop1
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_by_comm_table2(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(neib,nd,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        num = nod_comm%istack_export(neib  )                            &
     &       - nod_comm%istack_export(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
                 ii   = NB * (nod_comm%item_export(k+ist) - 1) + nd
                 ix   = k + (nd-1) * num + NB*ist
             SR_r1%WS(ix)= xx4(ii)
           end do
!$omp end do nowait
         end do
      end do
!$omp end parallel
!
      end subroutine copy_to_send_by_comm_table2
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_comm_table2(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (nod_comm%item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= SR_r1%WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_comm_table2
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_rev_table2                           &
     &         (node, nod_comm, NB, irev_import, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in)                                  &
     &     :: irev_import(nod_comm%istack_import(nod_comm%num_neib))
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
        do nd = 1, NB
!$omp do private(k,ii,ix)
          do k= 1, num
            ii   = NB * (node%internal_node+k+ist-1) + nd
            ix   = NB * (irev_import(k+ist)-1) + nd
            xx4(ii)= SR_r1%WR(ix)
          end do
!$omp end do nowait
        enddo
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_rev_table2
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_to_send_by_comm_lgloop2(node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, inum, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(neib,ist,num)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_export(neib-1)
        num = nod_comm%istack_export(neib  )                            &
     &       - nod_comm%istack_export(neib-1)
!$omp do private(k,nd,ii,ix)
        do inum = 1, NB*num
          k = mod(inum-ione,num) + ione
          nd = (inum-k) / NB + ione
                 ii   = NB * (nod_comm%item_export(k+ist) - 1) + nd
                 ix   = inum + NB*ist
             SR_r1%WS(ix)= xx4(ii)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_to_send_by_comm_lgloop2
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_comm_lgloop2                         &
     &         (node, nod_comm, NB, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, inum, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(nd,neib,ist,num,inum)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
!$omp do private(inum,k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (nod_comm%item_import(k+ist)-1) + nd
            ix   = k + (nd-1) * num + NB*ist
            xx4(ii)= SR_r1%WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_comm_lgloop2
!
! ----------------------------------------------------------------------
!
      subroutine copy_from_recv_by_rev_lgloop2                          &
     &         (node, nod_comm, NB, irev_import, xx4)
!
      use m_solver_SR
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in)                                  &
     &     :: irev_import(nod_comm%istack_import(nod_comm%num_neib))
!
      real(kind = kreal), intent(inout) :: xx4(NB*node%numnod)
!
      integer (kind = kint) :: neib, ist, inum, num
      integer (kind = kint) :: k, ii, ix, nd
!
!
!$omp parallel private(neib,ist,num,inum)
      do neib= 1, nod_comm%num_neib
        ist = nod_comm%istack_import(neib-1)
        num = nod_comm%istack_import(neib  )                            &
     &       - nod_comm%istack_import(neib-1)
!$omp do private(nd,k,ii,ix)
        do inum = 1, NB*num
          nd = mod(inum-ione,NB) + ione
          k = (inum-nd) / NB + ione
            ii   = NB * (node%internal_node+k+ist-1) + nd
            ix   = (irev_import(k)-ist) + (nd-1) * num + NB*ist
            xx4(ii)= SR_r1%WR(ix)
        end do
!$omp end do nowait
      enddo
!$omp end parallel
!
      end subroutine copy_from_recv_by_rev_lgloop2
!
! ----------------------------------------------------------------------
!
      end module send_recv_loop_tests
