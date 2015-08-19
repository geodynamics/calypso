!>@file  m_merged_ucd_data.f90
!!       module m_merged_ucd_data
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine allocate_merged_ucd_num(m_ucd)
!!      subroutine allocate_merged_ucd_data(numnod, ntot_comp)
!!      subroutine deallocate_merged_ucd_data(m_ucd)
!!
!!      subroutine set_node_double_address                              &
!!     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,          &
!!     &          STACK_EXPORT, NOD_EXPORT)
!!      subroutine update_ele_by_double_address(m_ucd, ucd)
!!@endverbatim
!
      module m_merged_ucd_data
!
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      implicit none
!
!>        number of node for each subdomain
      integer(kind = kint) :: nnod_ucd_local
!>        local node ID
      integer(kind = kint), allocatable :: inod_local_ucd(:)
!>        belonged subdomains ID for each node
      integer(kind = kint), allocatable :: ihome_pe_ucd(:)
!
      private :: nnod_ucd_local, ihome_pe_ucd, inod_local_ucd
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_merged_ucd_data(numnod)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod
!
!
      nnod_ucd_local = numnod
      allocate(inod_local_ucd(nnod_ucd_local))
      allocate(ihome_pe_ucd(nnod_ucd_local))
      if(nnod_ucd_local .gt. 0) then
        inod_local_ucd = 0
        ihome_pe_ucd =   0
      end if
!
      end subroutine allocate_merged_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_merged_ucd_data(m_ucd)
!
      use t_ucd_data
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      deallocate(inod_local_ucd, ihome_pe_ucd)
!
      call disconnect_merged_ucd_stack(m_ucd)
!
      end subroutine deallocate_merged_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_node_double_address                                &
     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,            &
     &          STACK_EXPORT, NOD_EXPORT)
!
      use t_ucd_data
      use solver_SR_int
!
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_IMPORT(STACK_IMPORT(NEIBPETOT))
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT(STACK_EXPORT(NEIBPETOT))
!
      integer(kind = kint) :: inod
!
!$omp parallel do
      do inod = 1, nnod_ucd_local
        inod_local_ucd(inod) = inod
        ihome_pe_ucd(inod) =   my_rank + 1
      end do
!$omp end parallel do
!
      call solver_send_recv_i(nnod_ucd_local, NEIBPETOT, NEIBPE,        &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            inod_local_ucd)
      call solver_send_recv_i(nnod_ucd_local, NEIBPETOT, NEIBPE,        &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            ihome_pe_ucd)
!
      end subroutine set_node_double_address
!
! -----------------------------------------------------------------------
!
      subroutine update_ele_by_double_address(m_ucd, ucd)
!
      use t_ucd_data
      use m_geometry_data
!
      type(merged_ucd_data), intent(in) :: m_ucd
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: ip, k1
      integer(kind = kint_gl) :: inod, iele
!
      do ip = 1, nprocs
        if(m_ucd%istack_merged_intnod(ip-1)                             &
     &      .ne. node1%istack_internod(ip-1)) then
           write(*,*) 'aho', ip, m_ucd%istack_merged_intnod(ip-1) ,     &
     &                           node1%istack_internod(ip-1)
        end if
      end do
!
!$omp parallel private(iele)
      do k1 = 1, ucd%nnod_4_ele
!$omp do private(inod,ip)
        do iele = 1, ucd%nele
          inod = ucd%ie(iele,k1)
          ip = ihome_pe_ucd(inod)
          ucd%ie(iele,k1) = m_ucd%istack_merged_intnod(ip-1)            &
     &                 + inod_local_ucd(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine update_ele_by_double_address
!
! -----------------------------------------------------------------------
!
      end module m_merged_ucd_data
