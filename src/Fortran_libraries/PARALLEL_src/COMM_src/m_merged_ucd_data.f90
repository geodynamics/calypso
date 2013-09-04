!m_merged_ucd_data.f90
!      module m_merged_ucd_data
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine allocate_merged_ucd_num
!      subroutine allocate_merged_ucd_data(numnod, nnod_ele, ntot_comp)
!      subroutine deallocate_merged_ucd_data
!
!      subroutine count_merged_ucd(nnod, nele)
!      subroutine set_node_double_address                               &
!     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,           &
!     &          STACK_EXPORT, NOD_EXPORT)
!      subroutine update_ele_by_double_address(nele, nnod_ele, ie)
!
      module m_merged_ucd_data
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
!
      implicit none
!
      integer(kind = kint), allocatable :: nnod_ucd_list(:)
      integer(kind = kint), allocatable :: nele_ucd_list(:)
      integer(kind = kint), allocatable :: internod_ucd_list(:)
      integer(kind = kint), allocatable :: istack_nod_ucd_list(:)
      integer(kind = kint), allocatable :: istack_ele_ucd_list(:)
      integer(kind = kint), allocatable :: istack_internod_ucd_list(:)
      integer(kind = kint) :: nmax_nod_ucd_list
      integer(kind = kint) :: nmax_ele_ucd_list
      integer(kind = kint) :: nmax_intnod_ucd_list
!
      integer(kind = kint) :: nnod_ucd_local
      integer(kind = kint), allocatable :: inod_local_ucd(:)
      integer(kind = kint), allocatable :: ihome_pe_ucd(:)
!
      integer(kind = kint), allocatable :: inod_single_ucd(:)
      integer(kind = kint), allocatable :: iele_single_ucd(:)
      integer(kind = kint), allocatable :: ie_single_ucd(:)
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
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_merged_ucd_num
!
!
      allocate (sta1(MPI_STATUS_SIZE))
      if(my_rank .eq. 0) allocate(sta2(MPI_STATUS_SIZE))
!
      allocate(nnod_ucd_list(nprocs))
      allocate(nele_ucd_list(nprocs))
      allocate(internod_ucd_list(nprocs))
      allocate(istack_nod_ucd_list(0:nprocs))
      allocate(istack_ele_ucd_list(0:nprocs))
      allocate(istack_internod_ucd_list(0:nprocs))
!
      nnod_ucd_list = 0
      nele_ucd_list = 0
      internod_ucd_list = 0
      istack_nod_ucd_list = 0
      istack_ele_ucd_list = 0
      istack_internod_ucd_list = 0
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
      subroutine deallocate_merged_ucd_data
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
      deallocate(nnod_ucd_list, istack_nod_ucd_list)
      deallocate(nele_ucd_list, istack_ele_ucd_list)
      deallocate(internod_ucd_list, istack_internod_ucd_list)
!
      end subroutine deallocate_merged_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_merged_ucd(nnod, internal_node, nele)
!
      integer(kind = kint), intent(in) :: nnod, internal_node, nele
!
      integer(kind = kint) :: ip
!
      call MPI_Allgather(nnod, ione, MPI_INTEGER,                       &
     &    nnod_ucd_list, ione, MPI_INTEGER, SOLVER_COMM, ierr)
      call MPI_Allgather(nele, ione, MPI_INTEGER,                       &
     &    nele_ucd_list, ione, MPI_INTEGER, SOLVER_COMM, ierr)
      call MPI_Allgather(internal_node, ione, MPI_INTEGER,              &
     &    internod_ucd_list, ione, MPI_INTEGER, SOLVER_COMM, ierr)
!
      do ip = 1,  nprocs
        istack_nod_ucd_list(ip) = istack_nod_ucd_list(ip-1)             &
     &                              + nnod_ucd_list(ip)
        istack_ele_ucd_list(ip) = istack_ele_ucd_list(ip-1)             &
     &                              + nele_ucd_list(ip)
        istack_internod_ucd_list(ip) = istack_internod_ucd_list(ip-1)   &
     &                              + internod_ucd_list(ip)
        nmax_nod_ucd_list = max(nmax_nod_ucd_list,nnod_ucd_list(ip))
        nmax_ele_ucd_list = max(nmax_ele_ucd_list,nele_ucd_list(ip))
        nmax_intnod_ucd_list                                            &
     &             = max(nmax_intnod_ucd_list,internod_ucd_list(ip))
      end do
!
      end subroutine count_merged_ucd
!
! -----------------------------------------------------------------------
!
      subroutine set_node_double_address                                &
     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,            &
     &          STACK_EXPORT, NOD_EXPORT)
!
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
     &            inod_local_ucd, SOLVER_COMM, my_rank)
      call solver_send_recv_i(nnod_ucd_local, NEIBPETOT, NEIBPE,        &
     &            STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,   &
     &            ihome_pe_ucd, SOLVER_COMM, my_rank)
!
      end subroutine set_node_double_address
!
! -----------------------------------------------------------------------
!
      subroutine update_ele_by_double_address(nele, nnod_ele, ie)
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(inout) :: ie(nele,nnod_ele)
!
      integer(kind = kint) :: ip, k1, iele, inod
!
!
!$omp parallel private(iele)
      do k1 = 1, nnod_ele
!$omp do private(inod,ip)
        do iele = 1, nele
          inod = ie(iele,k1)
          ip = ihome_pe_ucd(inod)
          ie(iele,k1) = istack_internod_ucd_list(ip-1)                  &
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
