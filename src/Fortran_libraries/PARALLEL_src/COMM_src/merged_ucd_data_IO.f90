!>@file  merged_ucd_data_IO.f90
!!       module merged_ucd_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Output routine for merged UCD data segments
!!
!!@verbatim
!!      subroutine write_merged_ucd_fields(id_ucd, nnod, num_field,     &
!!     &          ntot_comp, ncomp_field, field_name, d_nod,            &
!!     &          istack_numnod, istack_intnod)
!!      subroutine write_merged_ucd_mesh(id_ucd, nnod, nele, nnod_ele,  &
!!     &          xx, ie, ntot_comp, istack_numnod, istack_intnod,      &
!!     &          istack_numele)
!!@endverbatim
!
      module merged_ucd_data_IO
!
      use m_precision
      use calypso_mpi
      use m_constants
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
     &          ntot_comp, ncomp_field, field_name, d_nod,              &
     &          istack_numnod, istack_intnod)
!
      integer(kind = kint), intent(in) ::  id_ucd
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
!
!
      if(my_rank .eq. 0) then
        call write_udt_field_header(id_ucd, num_field, ncomp_field,     &
     &      field_name)
      end if
!
      call write_merged_udt_field(id_ucd, nnod, ntot_comp, d_nod,       &
     &    istack_numnod, istack_intnod)
!
      end subroutine write_merged_ucd_fields
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_ucd_mesh(id_ucd, nnod, nele, nnod_ele,    &
     &          xx, ie, ntot_comp, istack_numnod, istack_intnod,        &
     &          istack_numele)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) ::  id_ucd
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele, ntot_comp
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_numele(0:nprocs)
!
!
      if(my_rank .eq. 0) then
        call write_udt_mesh_header(id_ucd, istack_intnod(nprocs),       &
     &      istack_numele(nprocs), ntot_comp)
      end if
!
      call write_merged_udt_field(id_ucd, nnod, n_vector, xx,           &
     &    istack_numnod, istack_intnod)
      call write_merged_ucd_connect(id_ucd, nele, nnod_ele, ie,         &
     &    istack_numele)
!
      end subroutine write_merged_ucd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_merged_ucd_connect(id_ucd, nele, nnod_ele, ie,   &
     &          istack_numele)
!
      use m_geometry_constants
      use m_merged_ucd_data
!
      integer(kind = kint), intent(in) ::  id_ucd
      integer(kind = kint), intent(in) :: nele, nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
!
      integer (kind=kint), intent(in) :: istack_numele(0:nprocs)
!
      integer(kind = kint) :: ip, num, iele, isend_rank
!
!
      if(my_rank .eq. 0) then
        do iele = 1, istack_numele(ione)
          iele_single_ucd(iele) = iele
        end do
!
        call write_ucd_mesh_connect(id_ucd, istack_numele(1), nnod_ele, &
     &      istack_numele(1), iele_single_ucd(1), ie(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip- 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank ) then
        num = nele*nnod_ele
        call MPI_ISEND(ie(1,1), num, CALYPSO_INTEGER,                   &
     &      izero, 0, CALYPSO_COMM, req1, ierr_MPI)
        end if
!
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = (istack_numele(ip) - istack_numele(ip-1)) * nnod_ele
          call MPI_IRECV(ie_single_ucd(1), num, CALYPSO_INTEGER,        &
     &        (ip-1), 0, CALYPSO_COMM, req2, ierr_MPI)
!
          call MPI_WAITALL (ione, req2, sta2, ierr_MPI)
!
          num = istack_numele(ip) - istack_numele(ip-1)
          do iele = 1, num
            iele_single_ucd(iele) = iele + istack_numele(ip-1)
          end do
!
          call write_ucd_mesh_connect(id_ucd, num, nnod_ele, num,       &
     &        iele_single_ucd(1), ie_single_ucd(1) )
      end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr_MPI)
        end if
      end do 
      call  calypso_MPI_barrier
!
      end subroutine write_merged_ucd_connect
!
! -----------------------------------------------------------------------
!
      subroutine write_merged_udt_field(id_ucd, numnod, ncomp_field,    &
     &          d_nod, istack_numnod, istack_intnod)
!
      use m_merged_ucd_data
!
      integer(kind = kint), intent(in) ::  id_ucd
      integer (kind=kint), intent(in) :: numnod, ncomp_field
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_field)
!
      integer (kind=kint), intent(in) :: istack_numnod(0:nprocs)
      integer (kind=kint), intent(in) :: istack_intnod(0:nprocs)
!
      integer(kind = kint) :: ip, num, inod, nnod, isend_rank
!
!
      if(my_rank .eq. 0) then
        do inod = 1, istack_intnod(1)
          inod_single_ucd(inod) = inod
        end do
!
        call write_ucd_field_data(id_ucd, numnod, ncomp_field,          &
     &      istack_intnod(ione), inod_single_ucd(1), d_nod(1,1) )
      end if
!
      do ip = 2, nprocs
        isend_rank = ip - 1
!C
!C-- SEND
        if(my_rank .eq. isend_rank) then
          num = numnod*ncomp_field
          call MPI_ISEND(d_nod(1,1), num, CALYPSO_REAL,                 &
     &      izero, 0, CALYPSO_COMM, req1, ierr_MPI)
        end if
!C
!C-- RECV
        if(my_rank .eq. 0) then
          num = (istack_numnod(ip) - istack_numnod(ip-1)) * ncomp_field
          call MPI_IRECV(d_single_ucd(1), num, CALYPSO_REAL,            &
     &        (ip-1), 0, CALYPSO_COMM, req2, ierr_MPI)
!
          call MPI_WAITALL (ione, req2, sta2, ierr_MPI)
!
          nnod = istack_numnod(ip) - istack_numnod(ip-1)
          num =  istack_intnod(ip) - istack_intnod(ip-1)
          do inod = 1, num
            inod_single_ucd(inod) = inod + istack_intnod(ip-1)
          end do
!
          call write_ucd_field_data(id_ucd, nnod,                       &
     &        ncomp_field, num, inod_single_ucd(1), d_single_ucd(1))
        end if
!
        if(my_rank .eq. isend_rank ) then
          call MPI_WAITALL (ione, req1, sta1, ierr_MPI)
        end if
!
      end do 
      call  calypso_MPI_barrier
!
      end subroutine write_merged_udt_field
!
! -----------------------------------------------------------------------
!
      end module merged_ucd_data_IO
