!
!      module merged_ucd_data_IO
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine write_merged_ucd_fields(id_ucd, nnod, num_field,      &
!     &          ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine write_merged_ucd_mesh(id_ucd, nnod, nele, nnod_ele,   &
!     &           xx, ie, ntot_comp)
!
      module merged_ucd_data_IO
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
!
      use m_merged_ucd_data
!
      use udt_data_IO
!
      implicit none
!
      private :: write_merged_ucd_connect
      private :: write_merged_udt_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_ucd_fields(id_ucd, nnod, num_field,       &
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) ::  id_ucd
!
!
      if(my_rank .eq. 0) then
        call write_udt_field_header(id_ucd, num_field, ncomp_field,     &
     &      field_name)
      end if
!
      call write_merged_udt_field(id_ucd, nnod, ntot_comp, d_nod)
!
      end subroutine write_merged_ucd_fields
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_ucd_mesh(id_ucd, nnod, nele, nnod_ele,    &
     &           xx, ie, ntot_comp)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele, ntot_comp
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) ::  id_ucd
!
!
      if(my_rank .eq. 0) then
        call write_udt_mesh_header                                      &
     &     (id_ucd, istack_internod_ucd_list(nprocs),                   &
     &      istack_ele_ucd_list(nprocs), ntot_comp)
      end if
!
      call write_merged_udt_field(id_ucd, nnod, n_vector, xx)
      call write_merged_ucd_connect(id_ucd, nele, nnod_ele, ie)
!
      end subroutine write_merged_ucd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_merged_ucd_connect(id_ucd, nele, nnod_ele, ie)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
!
      integer(kind = kint), intent(in) ::  id_ucd
!
      integer(kind = kint) :: ip, num, iele, isend_rank
!
!
      if(my_rank .eq. 0) then
        do iele = 1, nele_ucd_list(ione)
          iele_single_ucd(iele) = iele
        end do
!
        call write_single_grd_connect(id_ucd, nnod_ele,                 &
     &      nele_ucd_list(1), iele_single_ucd(1), ie(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip- 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank ) then
        num = nele*nnod_ele
        call MPI_ISEND(ie(1,1), num, MPI_INTEGER,                       &
     &      izero, 0, SOLVER_COMM, req1, ierr)
        end if
!
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = nele_ucd_list(ip)*nnod_ele
          call MPI_IRECV(ie_single_ucd(1), num, MPI_INTEGER,            &
     &        (ip-1), 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          do iele = 1, nele_ucd_list(ip)
            iele_single_ucd(iele) = iele + istack_ele_ucd_list(ip-1)
          end do
!
          call write_single_grd_connect(id_ucd, nnod_ele,               &
     &        nele_ucd_list(ip), iele_single_ucd(1), ie_single_ucd(1) )
      end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
      end do 
      call  time_prog_barrier
!
      end subroutine write_merged_ucd_connect
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_udt_field(id_ucd, nnod, ncomp_field,      &
     &          d_nod)
!
      integer (kind=kint), intent(in) :: nnod, ncomp_field
      real(kind = kreal), intent(in) :: d_nod(nnod,ncomp_field)
!
      integer(kind = kint), intent(in) ::  id_ucd
!
      integer(kind = kint) :: ip, num, inod, isend_rank
!
!
      if(my_rank .eq. 0) then
        do inod = 1, internod_ucd_list(ip)
          inod_single_ucd(inod) = inod
        end do
!
        call write_multi_udt_data(id_ucd, nnod, ncomp_field,           &
     &      ione, internod_ucd_list(ione), inod_single_ucd(1),         &
     &      d_nod(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip - 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank) then
          num = nnod*ncomp_field
          call MPI_ISEND(d_nod(1,1), num, MPI_DOUBLE_PRECISION,         &
     &      izero, 0, SOLVER_COMM, req1, ierr)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = nnod_ucd_list(ip)*ncomp_field
          call MPI_IRECV(d_single_ucd(1), num, MPI_DOUBLE_PRECISION,    &
     &        (ip-1), 0, SOLVER_COMM, req2, ierr)
!
          call MPI_WAITALL (ione, req2, sta2, ierr)
!
          do inod = 1, internod_ucd_list(ip)
            inod_single_ucd(inod) = inod + istack_nod_ucd_list(ip-1)
          end do
!
          call write_multi_udt_data(id_ucd, nnod_ucd_list(ip),          &
     &        ione, internod_ucd_list(ip), ncomp_field,                 &
     &        inod_single_ucd(1), d_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr)
        end if
!
      end do 
      call  time_prog_barrier
!
      end subroutine write_merged_udt_field
!
! -----------------------------------------------------------------------
!
      end module merged_ucd_data_IO
