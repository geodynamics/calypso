!
!      module merged_vtk_data_IO
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine write_merged_vtk_fields(id_vtk, nnod, num_field,      &
!     &          ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine write_merged_vtk_mesh(id_vtk, nnod, nele, nnod_ele,   &
!     &          xx, ie)
!
      module merged_vtk_data_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_merged_ucd_data
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
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: icou, j
!
!
      if(my_rank .eq. 0) then
        call write_vtk_fields_head(id_vtk,                              &
     &      istack_internod_ucd_list(nprocs))
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
     &        ncomp_field(j), d_nod(1,icou))
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_merged_vtk_fields
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_mesh(id_vtk, nnod, nele, nnod_ele,    &
     &          xx, ie)
!
      use m_geometry_constants
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,n_vector)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      if(my_rank .eq. 0) then
        call write_vtk_node_head(id_vtk,                                &
     &      istack_internod_ucd_list(nprocs) )
      end if
      call write_merged_vtk_each_field(id_vtk, nnod, n_vector, xx)
!
      if(my_rank .eq. 0) then
        call write_vtk_connect_head(id_vtk,                             &
     &      istack_ele_ucd_list(nprocs), nnod_ele)
      end if
!
      call write_merged_vtk_connect(id_vtk, nele, nnod_ele, ie)
!
      if(my_rank .eq. 0) then
          call write_vtk_cell_type(id_vtk, istack_ele_ucd_list(nprocs), &
     &        nnod_ele)
      end if
!
      end subroutine write_merged_vtk_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_connect(id_vtk, nele, nnod_ele, ie)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: ip, num, isend_rank
!
!
      if(my_rank .eq. 0) then
        call write_vtk_connect_data(id_vtk, nele_ucd_list(ione),        &
     &      nnod_ele, ie(1,1))
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
          num = nele_ucd_list(ip)*nnod_ele
          call MPI_IRECV(ie_single_ucd(1), num, MPI_INTEGER,            &
     &        isend_rank, 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          call write_vtk_connect_data(id_vtk, nele_ucd_list(ip),        &
     &        nnod_ele, ie_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
      end do 
      call  time_prog_barrier
!
      end subroutine write_merged_vtk_connect
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_vtk_each_field(id_vtk, nnod, ncomp_field, &
     &          d_nod)
!
      integer (kind=kint), intent(in) :: nnod, ncomp_field
      real(kind = kreal), intent(in) :: d_nod(nnod,ncomp_field)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: ip, num, isend_rank
!
!
      if(my_rank .eq. 0) then
        call write_multi_vtk_each_field(id_vtk, nnod,                   &
     &      ione, internod_ucd_list(ione), ncomp_field, d_nod)
      end if
!
      do ip = 2, nprocs
        isend_rank = ip- 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank ) then
          num = nnod*ncomp_field
          call MPI_ISEND(d_nod(1,1), num, MPI_DOUBLE_PRECISION,         &
     &        izero, 0, SOLVER_COMM, req1, ierr)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = nnod_ucd_list(ip)*ncomp_field
          call MPI_IRECV(d_single_ucd(1), num, MPI_DOUBLE_PRECISION,    &
     &        isend_rank, 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          call write_multi_vtk_each_field(id_vtk, nnod_ucd_list(ip),    &
     &        ione, internod_ucd_list(ip), ncomp_field,                 &
     &        d_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
      end do
      call time_prog_barrier
!
      end subroutine write_merged_vtk_each_field
!
! -----------------------------------------------------------------------
!
      end module merged_vtk_data_IO
