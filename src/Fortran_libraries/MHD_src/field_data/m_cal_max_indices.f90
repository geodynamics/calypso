!>@file   m_cal_max_indices.f90
!!@brief  module m_cal_max_indices
!!
!!@author H. Matsui
!!@date Programmed by H.Matsui and H.Okuda in July 2000
!!@n     Modified by H. Matsui on Aug., 2007
!
!>@brief  Find node positions of maximum values
!!
!!@verbatim
!!      subroutine allocate_phys_range(ncomp_viz)
!!      subroutine deallocate_phys_range
!!      subroutine cal_max_indices
!!@endverbatim
!
      module m_cal_max_indices
!
      use m_precision
!
      implicit  none
!
!>      Number of components for range data
      integer(kind=kint) :: ncomp_minmax
!
!>      Global node address for minimum value
      integer(kind=kint_gl), allocatable :: node_min(:)
!>      Global node address for maximum value
      integer(kind=kint_gl), allocatable :: node_max(:)
!>      Minimum value of field
      real(kind=kreal), allocatable :: phys_min(:)
!>      Maximum value of field
      real(kind=kreal), allocatable :: phys_max(:)
!
!>      local node address for minimum value in subdomain
      integer(kind=kint), allocatable :: inod_min_lc(:)
!>      local node address for minimum value in subdomain
      integer(kind=kint), allocatable :: inod_max_lc(:)
!>      Global node address for minimum value in subdomain
      integer(kind=kint_gl), allocatable :: node_min_local(:)
!>      Global node address for minimum value in subdomain
      integer(kind=kint_gl), allocatable :: node_max_local(:)
!>      Minimum value of field in subdomain
      real(kind=kreal), allocatable :: phys_min_local(:)
!>      Maximum value of field in subdomain
      real(kind=kreal), allocatable :: phys_max_local(:)
!
      private :: inod_min_lc, inod_max_lc
      private :: node_min_local, node_max_local
      private :: phys_min_local, phys_max_local
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine allocate_phys_range(ncomp_viz)
!
       integer(kind = kint), intent(in) :: ncomp_viz
!
!
       ncomp_minmax = ncomp_viz
       allocate(node_min(ncomp_minmax))
       allocate(node_max(ncomp_minmax))
       allocate(phys_min(ncomp_minmax))
       allocate(phys_max(ncomp_minmax))
!
       allocate(inod_min_lc(ncomp_minmax))
       allocate(inod_max_lc(ncomp_minmax))
       allocate(node_min_local(ncomp_minmax))
       allocate(node_max_local(ncomp_minmax))
       allocate(phys_min_local(ncomp_minmax))
       allocate(phys_max_local(ncomp_minmax))
!
       node_min = 0
       node_max = 0
       phys_min = 0.0d0
       phys_max = 0.0d0
!
       node_min_local = 0
       node_max_local = 0
       phys_min_local = 1.0d15
       phys_max_local =-1.0d15
!
       end subroutine allocate_phys_range
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_phys_range
!
       deallocate (node_min, node_min_local)
       deallocate (node_max, node_max_local)
       deallocate (phys_min, phys_min_local)
       deallocate (phys_max, phys_max_local)
       deallocate (inod_min_lc, inod_max_lc)
!
       end subroutine deallocate_phys_range
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_max_indices
!
      use calypso_mpi
      use m_geometry_data
      use m_node_phys_data
!
      integer (kind = kint) :: nd, inod
!
!$omp parallel do private(nd,inod)
      do nd = 1, ncomp_minmax
!
        inod_max_lc(nd) = 1
        do inod = 1, node1%numnod
          if (nod_fld1%d_fld(inod,nd)                                   &
     &      .gt. nod_fld1%d_fld(inod_max_lc(nd),nd)) then
            inod_max_lc(nd) = inod
          end if
        end do
!
        inod_min_lc(nd) = 1
        do inod = 1, node1%numnod
          if (nod_fld1%d_fld(inod,nd)                                   &
     &      .lt. nod_fld1%d_fld(inod_min_lc(nd),nd)) then
            inod_min_lc(nd) = inod
          end if
        end do
!
        phys_max_local(nd) = nod_fld1%d_fld(inod_max_lc(nd),nd)
        phys_min_local(nd) = nod_fld1%d_fld(inod_min_lc(nd),nd)
      end do
!$omp end parallel do
!
      call MPI_allREDUCE (phys_max_local(1), phys_max(1),               &
     &    ncomp_minmax, CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
!
      call MPI_allREDUCE (phys_min_local(1), phys_min(1),               &
     &    ncomp_minmax, CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
!
      node_max_local = 0
      node_min_local = 0
!
!$omp parallel do private(nd,inod)
      do nd = 1, ncomp_minmax
        if ( phys_max(nd) .eq. phys_max_local(nd) ) then
          inod = inod_max_lc(nd)
          node_max_local(nd) = node1%inod_global(inod)
        end if
        if ( phys_min(nd) .eq. phys_min_local(nd) ) then
          inod = inod_min_lc(nd)
          node_min_local(nd) = node1%inod_global(inod)
        end if
      end do
!$omp end parallel do
!
      call MPI_allREDUCE (node_max_local(1), node_max(1),               &
     &    ncomp_minmax, CALYPSO_GLOBAL_INT, MPI_SUM,                    &
     &    CALYPSO_COMM, ierr_MPI)
!
      call MPI_allREDUCE (node_min_local(1), node_min(1),               &
     &    ncomp_minmax, CALYPSO_GLOBAL_INT, MPI_SUM,                    &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine cal_max_indices
!
!  ---------------------------------------------------------------------
!
      end module m_cal_max_indices
