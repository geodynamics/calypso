!>@file   collect_fline_connectivity.f90
!!@brief  module collect_fline_connectivity
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line connectivity
!!
!!@verbatim
!!      subroutine collect_number_of_fline(fline_lc, fline_gl)
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!      subroutine collect_fline_connection(fline_lc, fline_gl)
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!@endverbatim
!!
      module collect_fline_connectivity
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use t_global_fieldline
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine collect_number_of_fline(fline_lc, fline_gl)
!
      use calypso_mpi_int
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip
!
!
      call calypso_mpi_allgather_one_int                                &
     &   (fline_lc%nnod_line_l, fline_gl%nnod_line_gl(1))
!
      call calypso_mpi_allgather_one_int                                &
     &   (fline_lc%nele_line_l, fline_gl%nele_line_gl(1))
!
      fline_gl%istack_nod_line_gl(0) = 0
      fline_gl%istack_ele_line_gl(0) = 0
      do ip = 1, nprocs
        fline_gl%istack_nod_line_gl(ip)                                 &
     &                = fline_gl%istack_nod_line_gl(ip-1)               &
     &                 + fline_gl%nnod_line_gl(ip)
        fline_gl%istack_ele_line_gl(ip)                                 &
     &                = fline_gl%istack_ele_line_gl(ip-1)               &
     &                 + fline_gl%nele_line_gl(ip)
      end do
      fline_gl%ntot_nod_line_gl = fline_gl%istack_nod_line_gl(nprocs)
      fline_gl%ntot_ele_line_gl = fline_gl%istack_ele_line_gl(nprocs)
!
      end subroutine collect_number_of_fline
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_connection(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, ist, ied, inum
      integer :: num, nneib_recv
!
!
      nneib_recv = 0
      num = int(2 * fline_lc%nele_line_l)
      call MPI_Isend(fline_lc%iedge_line_l(1,1), num, CALYPSO_INTEGER,  &
     &    0, 0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = int(nprocs)
        do ip = 1, nprocs
          ist = fline_gl%istack_ele_line_gl(ip-1) + 1
          num = int(2 * fline_gl%nele_line_gl(ip))
          call MPI_Irecv (fline_gl%iedge_line_gl(1,ist), num,           &
     &        CALYPSO_INTEGER, int(ip-1), 0, CALYPSO_COMM,              &
     &        fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(1, fline_gl%req1_fline(ione),                    &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      if(my_rank .eq. 0) then
        do ip = 1, nprocs
          ist = fline_gl%istack_ele_line_gl(ip-1) + 1
          ied = fline_gl%istack_ele_line_gl(ip)
          do inum = ist, ied
            fline_gl%iedge_line_gl(1:2,inum)                            &
     &                     = fline_gl%iedge_line_gl(1:2,inum)           &
     &                      + fline_gl%istack_nod_line_gl(ip-1)
          end do
        end do
      end if
!
      end subroutine collect_fline_connection
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_connectivity
