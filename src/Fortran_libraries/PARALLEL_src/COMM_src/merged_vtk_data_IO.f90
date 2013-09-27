!>@file  merged_vtk_data_IO.f90
!!       module merged_vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Output routine for merged VTK data segments
!!
!!@verbatim
!!      subroutine write_merged_vtk_fields(id_vtk, nnod, num_field,     &
!!     &          ntot_comp, ncomp_field, field_name, d_nod,            &
!!     &          istack_numnod, istack_intnod)
!!      subroutine write_merged_vtk_mesh(id_vtk, nnod, nele, nnod_ele,  &
!!     &          xx, ie, istack_numnod, istack_intnod, istack_numele)
!!@endverbatim
!
      module merged_vtk_data_IO
!
      use m_precision
      use calypso_mpi
      use m_constants
      use m_parallel_var_dof
      use vtk_data_IO
!
      implicit none
!
      private :: write_merged_vtk_connect
      private :: write_merged_vtk_each_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_fields(id_vtk, nnod, num_field,       &
     &          ntot_comp, ncomp_field, field_name, d_nod,              &
     &          istack_numnod, istack_intnod)
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
!
      integer(kind = kint) :: icou, j
!
!
      if(my_rank .eq. 0) then
        call write_vtk_fields_head(id_vtk, istack_intnod(nprocs))
      end if
!
      if(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          if(my_rank .eq. 0) then
             call write_vtk_each_field_head(id_vtk, ncomp_field(j),     &
     &           field_name(j) )
          end if
!
          call write_merged_vtk_each_field(id_vtk, nnod,                &
     &        ncomp_field(j), d_nod(1,icou), istack_numnod,   &
     &        istack_intnod)
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_merged_vtk_fields
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_mesh(id_vtk, nnod, nele, nnod_ele,    &
     &          xx, ie, istack_numnod, istack_intnod, istack_numele)
!
      use m_geometry_constants
      use m_phys_constants
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,n_vector)
!
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_numele(0:nprocs)
!
!
      if(my_rank .eq. 0) then
        call write_vtk_node_head(id_vtk, istack_intnod(nprocs) )
      end if
      call write_merged_vtk_each_field(id_vtk, nnod, n_vector, xx,      &
     &    istack_numnod, istack_intnod)
!
      call write_merged_vtk_connect(id_vtk, nele, nnod_ele, ie,         &
     &    istack_numele)
!
      end subroutine write_merged_vtk_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_connect(id_vtk, nele, nnod_ele, ie,   &
     &          istack_numele)
!
      use m_geometry_constants
      use m_merged_ucd_data
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      integer (kind=kint), intent(in) :: istack_numele(0:nprocs)
!
      integer(kind = kint) :: ip, num, isend_rank
!
!
      if(my_rank .eq. 0) then
        call write_vtk_connect_head(id_vtk, istack_numele(nprocs),      &
     &      nnod_ele)
        call write_vtk_connect_data(id_vtk, istack_numele(ione),        &
     &      nnod_ele, istack_numele(ione), ie(1,1))
      end if
!
!C
!C-- SEND
      do ip = 2, nprocs
        isend_rank = ip- 1
!
        if(my_rank .eq. isend_rank) then
          num = nele*nnod_ele
          call MPI_ISEND(ie(1,1), num, MPI_INTEGER,                     &
     &        izero, 0, SOLVER_COMM, req1, ierr)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = (istack_numele(ip) - istack_numele(ip-1)) * nnod_ele
          call MPI_IRECV(ie_single_ucd(1), num, MPI_INTEGER,            &
     &        isend_rank, 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          num = istack_numele(ip) - istack_numele(ip-1)
          call write_vtk_connect_data(id_vtk, num,                      &
     &        nnod_ele, num, ie_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
      end do 
!
      if(my_rank .eq. 0) then
          call write_vtk_cell_type(id_vtk,                              &
     &        istack_numele(nprocs),  nnod_ele)
      end if
      call  calypso_MPI_barrier(ierr)
!
      end subroutine write_merged_vtk_connect
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_each_field(id_vtk, numnod,            &
     &          ncomp_field, d_nod, istack_numnod, istack_intnod)
!
      use m_merged_ucd_data
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer (kind=kint), intent(in) :: numnod, ncomp_field
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_field)
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
!
      integer(kind = kint) :: ip, num, nnod, isend_rank
!
!
      if(my_rank .eq. 0) then
        call write_vtk_each_field(id_vtk, numnod, ncomp_field,          &
     &      istack_intnod(ione), d_nod)
      end if
!
      do ip = 2, nprocs
        isend_rank = ip- 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank ) then
          num = numnod*ncomp_field
          call MPI_ISEND(d_nod(1,1), num, MPI_DOUBLE_PRECISION,         &
     &        izero, 0, SOLVER_COMM, req1, ierr)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = (istack_numnod(ip) - istack_numnod(ip-1)) * ncomp_field
          call MPI_IRECV(d_single_ucd(1), num, MPI_DOUBLE_PRECISION,    &
     &        isend_rank, 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          nnod = istack_numnod(ip) - istack_numnod(ip-1)
          num =  istack_intnod(ip) - istack_intnod(ip-1)
          call write_vtk_each_field(id_vtk, nnod,                       &
     &        ncomp_field, num, d_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
      end do
      call calypso_MPI_barrier(ierr)
!
      end subroutine write_merged_vtk_each_field
!
! -----------------------------------------------------------------------
!
      end module merged_vtk_data_IO
