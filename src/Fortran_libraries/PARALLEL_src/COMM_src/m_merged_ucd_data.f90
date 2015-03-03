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
!!      subroutine allocate_merged_ucd_data(numnod, nnod_ele, ntot_comp)
!!      subroutine deallocate_merged_ucd_data(m_ucd)
!!
!!      subroutine count_merged_ucd(nnod, nele_ucd, m_ucd)
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
      integer(kind = kint) :: nmax_nod_ucd_list
      integer(kind = kint) :: nmax_ele_ucd_list
!
!>        number of node for each subdomain
      integer(kind = kint) :: nnod_ucd_local
!>        local node ID
      integer(kind = kint), allocatable :: inod_local_ucd(:)
!>        belonged subdomains ID for each node
      integer(kind = kint), allocatable :: ihome_pe_ucd(:)
!
      integer(kind = kint_gl), allocatable :: inod_single_ucd(:)
      integer(kind = kint_gl), allocatable :: iele_single_ucd(:)
      integer(kind = kint_gl), allocatable :: ie_single_ucd(:)
      real(kind = kreal), allocatable :: d_single_ucd(:)
!
      integer, allocatable :: sta1(:)
!       status flag for sending
      integer, allocatable :: sta2(:)
!       status flag for recieving
      integer :: req1
!       status flag for sending
      integer :: req2
!       status flag for recieving
!
      private :: nmax_nod_ucd_list, nmax_ele_ucd_list
      private :: nnod_ucd_local, ihome_pe_ucd, inod_local_ucd
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_merged_ucd_num(m_ucd)
!
      use t_ucd_data
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      allocate (sta1(MPI_STATUS_SIZE))
      if(my_rank .eq. 0) allocate(sta2(MPI_STATUS_SIZE))
!
      call alloc_merged_ucd_stack(nprocs, m_ucd)
      nmax_nod_ucd_list = 0
      nmax_ele_ucd_list = 0
!
      end subroutine allocate_merged_ucd_num
!
! -----------------------------------------------------------------------
!
      subroutine allocate_merged_ucd_data(numnod, nnod_ele, ntot_comp)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod, nnod_ele, ntot_comp
      integer(kind = kint) :: ncomp
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
      if(my_rank .eq. 0) then
        ncomp = max(ntot_comp,n_vector)
!
        allocate( inod_single_ucd(nmax_nod_ucd_list) )
        allocate( d_single_ucd(nmax_nod_ucd_list*ncomp) )
        allocate( iele_single_ucd(nmax_ele_ucd_list) )
        allocate( ie_single_ucd(nmax_ele_ucd_list*nnod_ele) )
        if(nmax_nod_ucd_list .gt. 0) then
          inod_single_ucd = 0
          d_single_ucd =  0.0d0
        end if
        if(nmax_ele_ucd_list .gt. 0) then
          iele_single_ucd = 0
          ie_single_ucd = 0
        end if
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
      deallocate (sta1)
      if(my_rank .eq. 0) then
        deallocate (sta2)
        deallocate( d_single_ucd, ie_single_ucd)
        deallocate( inod_single_ucd, iele_single_ucd)
      end if
!
      deallocate(inod_local_ucd, ihome_pe_ucd)
!
      call dealloc_merged_ucd_stack(m_ucd)
!
      end subroutine deallocate_merged_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_merged_ucd(nnod, internal_node, nele_ucd, m_ucd)
!
      use t_ucd_data
!
      integer(kind = kint), intent(in) :: nnod, internal_node
      integer(kind = kint_gl), intent(inout) :: nele_ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
      integer(kind = kint), allocatable :: nnod_ucd_list(:)
      integer(kind = kint), allocatable :: nele_ucd_list(:)
      integer(kind = kint), allocatable :: internod_ucd_list(:)
      integer(kind = kint) :: ip
!
!
      allocate(nnod_ucd_list(nprocs))
      allocate(nele_ucd_list(nprocs))
      allocate(internod_ucd_list(nprocs))
      nnod_ucd_list = 0
      nele_ucd_list = 0
      internod_ucd_list = 0
!
      call MPI_Allgather(nnod, ione, CALYPSO_INTEGER,                   &
     &    nnod_ucd_list, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      call MPI_Allgather(nele_ucd, ione, CALYPSO_INTEGER,               &
     &    nele_ucd_list, ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      call MPI_Allgather(internal_node, ione, CALYPSO_INTEGER,          &
     &    internod_ucd_list, ione, CALYPSO_INTEGER, CALYPSO_COMM,       &
     &    ierr_MPI)
!
      do ip = 1,  nprocs
        m_ucd%istack_merged_nod(ip) = m_ucd%istack_merged_nod(ip-1)     &
     &                              + nnod_ucd_list(ip)
        m_ucd%istack_merged_ele(ip) = m_ucd%istack_merged_ele(ip-1)     &
     &                              + nele_ucd_list(ip)
        m_ucd%istack_merged_intnod(ip)                                  &
     &                    = m_ucd%istack_merged_intnod(ip-1)            &
     &                              + internod_ucd_list(ip)
        nmax_nod_ucd_list = max(nmax_nod_ucd_list,nnod_ucd_list(ip))
        nmax_ele_ucd_list = max(nmax_ele_ucd_list,nele_ucd_list(ip))
      end do
!
      deallocate(nnod_ucd_list, nele_ucd_list, internod_ucd_list)
!
      end subroutine count_merged_ucd
!
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
!
      type(merged_ucd_data), intent(in) :: m_ucd
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: ip, k1
      integer(kind = kint_gl) :: inod, iele
!
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
