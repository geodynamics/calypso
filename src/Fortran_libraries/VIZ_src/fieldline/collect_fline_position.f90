!>@file   collect_fline_position.f90
!!@brief  module collect_fline_position
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line position and color
!!
!!@verbatim
!!      subroutine s_collect_fline_position(fline_lc, fline_gl)
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!      subroutine collect_fline_color(fline_lc, fline_gl)
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!@endverbatim
!
      module collect_fline_position
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
      subroutine s_collect_fline_position(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, ist
      integer :: num, nneib_recv
!
!
      nneib_recv = 0
      num = int(3 * fline_lc%nnod_line_l)
      call MPI_Isend(fline_lc%xx_line_l(1,1), num, CALYPSO_REAL, 0, &
     &    0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = int(nprocs)
        do ip = 1, nprocs
          ist = fline_gl%istack_nod_line_gl(ip-1) + 1
          num = int(3 * fline_gl%nnod_line_gl(ip))
          call MPI_Irecv                                                &
     &       (fline_gl%xx_line_gl(1,ist), num, CALYPSO_REAL, int(ip-1), &
     &        0, CALYPSO_COMM, fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(1, fline_gl%req1_fline(ione),                    &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      end subroutine s_collect_fline_position
!
!  ---------------------------------------------------------------------
!
      subroutine collect_fline_color(fline_lc, fline_gl)
!
      type(local_fieldline), intent(in) :: fline_lc
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      integer(kind = kint) :: ip, ist
      integer :: num, nneib_recv
!
!
      nneib_recv = 0
      num = int(fline_lc%nnod_line_l)
      call MPI_Isend(fline_lc%col_line_l(1), num, CALYPSO_REAL, 0,      &
     &    0, CALYPSO_COMM, fline_gl%req1_fline(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = int(nprocs)
        do ip = 1, nprocs
          ist = fline_gl%istack_nod_line_gl(ip-1) + 1
          num = int(fline_gl%nnod_line_gl(ip))
          call MPI_Irecv                                                &
     &       (fline_gl%col_line_gl(ist), num, CALYPSO_REAL, int(ip-1),  &
     &        0, CALYPSO_COMM, fline_gl%req2_fline(ip), ierr_MPI)
        end do
!
      end if
      call MPI_WAITALL(nneib_recv, fline_gl%req2_fline,                 &
     &    fline_gl%sta2_fline, ierr_MPI)
      call MPI_WAITALL(1, fline_gl%req1_fline(ione),                    &
     &    fline_gl%sta1_fline(ione,ione), ierr_MPI)
!
      end subroutine collect_fline_color
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_position
