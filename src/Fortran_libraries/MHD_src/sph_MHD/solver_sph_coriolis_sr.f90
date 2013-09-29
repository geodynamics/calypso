!> @file  solver_sph_coriolis_sr.f90
!!      module solver_sph_coriolis_sr
!!
!! @author  H. Matsui
!! @date Programmed in 2010
!
!
!> @brief Communication routines for Coriolis term
!!
!!@verbatim
!!      subroutine solver_sph_coriolis_sr_1                             &
!!     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!!      subroutine solver_sph_coriolis_sr_3                             &
!!     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!!      subroutine solver_sph_coriolis_sr_5                             &
!!     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!!
!!      subroutine solver_sph_coriolis_sr_int                           &
!!     &          (nshift_j_cor, jmax, idx_j, jmax_cor, idx_cor_j)
!!@endverbatim
!!
!!@n @param nshift_j_cor  Difference of start mode between d_rj and d_cor
!!@n @param nri           Number of radial grids
!!@n @param jmax          Number of local spherical harmonics modes
!!@n @param jmax_cor      Spectr data to evaluate Coriolis force 
!!                                    @f$ f(r,j) @f$
!!@n @param d_rj          Spectr data @f$ f(r,j) @f$
!!@n @param d_cor  time step
!
      module solver_sph_coriolis_sr
!
      use m_precision
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_comm_tbl_sph_coriolis
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine solver_sph_coriolis_sr_1                               &
     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!

      integer(kind = kint), intent(in) :: nri, jmax, nshift_j_cor
      real(kind = kreal), intent(in) :: d_rj(nri*jmax)
!
      integer(kind = kint), intent(in) :: jmax_cor
      real(kind = kreal), intent(inout) :: d_cor(nri*jmax_cor)
!
      integer(kind = kint) :: i, j, k, ist, num, inod, jnod
!
!
!C-- SEND
      do k = 1, nri
        do i = 1, ntot_send_cor
          j = idx_send_cor(i)
          jnod = j + (k-1)*jmax
          inod = k + (i-1)*nri
          send_sph_cor(inod) = d_rj(jnod)
        end do
      end do
!
      do i = 1, nneib_send_cor
        ist = nri*istack_send_cor(i-1) + 1
        num = nri*(istack_send_cor(i) - istack_send_cor(i-1))
        call MPI_ISEND(send_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_send_cor(i), 0, CALYPSO_COMM, req1_cor(i), ierr_MPI)
      end do
!C
!C-- RECEIVE
      do i= 1, nneib_recv_cor
        ist = nri*istack_recv_cor(i-1) + 1
        num = nri*(istack_recv_cor(i) - istack_recv_cor(i-1))
        call MPI_IRECV(recv_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_recv_cor(i), 0, CALYPSO_COMM, req2_cor(i), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv_cor, req2_cor, sta2_cor,  ierr_MPI)
!
      do k = 1, nri
        do i = 1, ntot_recv_cor
          j = idx_recv_cor(i)
          jnod = j + (k-1)*jmax_cor
          inod = k + (i-1)*nri
          d_cor(jnod  ) = recv_sph_cor(inod)
        end do
      end do
      do k = 1, nri
        do i = 1, jmax
          j = i + nshift_j_cor
          inod = i + (k-1)*jmax
          jnod = j + (k-1)*jmax_cor
          d_cor(jnod) = d_rj(inod)
        end do
      end do
!
      call MPI_WAITALL (nneib_send_cor, req1_cor, sta1_cor,  ierr_MPI)
!
      end subroutine solver_sph_coriolis_sr_1
!
! -----------------------------------------------------------------------
!
      subroutine solver_sph_coriolis_sr_3                               &
     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!
      integer(kind = kint), intent(in) :: nri, jmax, nshift_j_cor
      real(kind = kreal), intent(in) :: d_rj(nri*jmax,3)
!
      integer(kind = kint), intent(in) :: jmax_cor
      real(kind = kreal), intent(inout) :: d_cor(nri*jmax_cor,3)
!
      integer(kind = kint) :: i, j, k, ist, num, inod, jnod
!
!
!C-- SEND
      do k = 1, nri
        do i = 1, ntot_send_cor
          j = idx_send_cor(i)
          jnod = j + (k-1)*jmax
          inod = k + (i-1)*nri
          send_sph_cor(3*inod-2) = d_rj(jnod,1)
          send_sph_cor(3*inod-1) = d_rj(jnod,2)
          send_sph_cor(3*inod  ) = d_rj(jnod,3)
        end do
      end do
!
      do i = 1, nneib_send_cor
        ist = 3*nri*istack_send_cor(i-1) + 1
        num = 3*nri*(istack_send_cor(i) - istack_send_cor(i-1))
        call MPI_ISEND(send_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_send_cor(i), 0, CALYPSO_COMM, req1_cor(i), ierr_MPI)
      end do
!C
!C-- RECEIVE
      do i= 1, nneib_recv_cor
        ist = 3*nri*istack_recv_cor(i-1) + 1
        num = 3*nri*(istack_recv_cor(i) - istack_recv_cor(i-1))
        call MPI_IRECV(recv_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_recv_cor(i), 0, CALYPSO_COMM, req2_cor(i), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv_cor, req2_cor, sta2_cor,  ierr_MPI)
!
      do k = 1, nri
        do i = 1, ntot_recv_cor
          j = idx_recv_cor(i)
          jnod = j + (k-1)*jmax_cor
          inod = k + (i-1)*nri
          d_cor(jnod,1) = recv_sph_cor(3*inod-2)
          d_cor(jnod,2) = recv_sph_cor(3*inod-1)
          d_cor(jnod,3) = recv_sph_cor(3*inod  )
        end do
      end do
      do k = 1, nri
        do i = 1, jmax
          j = i + nshift_j_cor
          inod = i + (k-1)*jmax
          jnod = j + (k-1)*jmax_cor
          d_cor(jnod,1) = d_rj(inod,1)
          d_cor(jnod,2) = d_rj(inod,2)
          d_cor(jnod,3) = d_rj(inod,3)
        end do
      end do
!
      call MPI_WAITALL (nneib_send_cor, req1_cor, sta1_cor,  ierr_MPI)
!
      end subroutine solver_sph_coriolis_sr_3
!
! -----------------------------------------------------------------------
!
      subroutine solver_sph_coriolis_sr_5                               &
     &          (nshift_j_cor, jmax, nri, d_rj, jmax_cor, d_cor)
!
      integer(kind = kint), intent(in) :: nri, jmax, nshift_j_cor
      real(kind = kreal), intent(in) :: d_rj(nri*jmax,5)
!
      integer(kind = kint), intent(in) :: jmax_cor
      real(kind = kreal), intent(inout) :: d_cor(nri*jmax_cor,5)
!
      integer(kind = kint) :: i, j, k, ist, num, inod, jnod
!
!
!C-- SEND
      do k = 1, nri
        do i = 1, ntot_send_cor
          j = idx_send_cor(i)
          jnod = j + (k-1)*jmax
          inod = k + (i-1)*nri
          send_sph_cor(5*inod-4) = d_rj(jnod,1)
          send_sph_cor(5*inod-3) = d_rj(jnod,2)
          send_sph_cor(5*inod-2) = d_rj(jnod,3)
          send_sph_cor(5*inod-1) = d_rj(jnod,4)
          send_sph_cor(5*inod  ) = d_rj(jnod,5)
        end do
      end do
!
      do i = 1, nneib_send_cor
        ist = 5*nri*istack_send_cor(i-1) + 1
        num = 5*nri*(istack_send_cor(i) - istack_send_cor(i-1))
        call MPI_ISEND(send_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_send_cor(i), 0, CALYPSO_COMM, req1_cor(i), ierr_MPI)
      end do
!C
!C-- RECEIVE
      do i= 1, nneib_recv_cor
        ist = 5*nri*istack_recv_cor(i-1) + 1
        num = 5*nri*(istack_recv_cor(i) - istack_recv_cor(i-1))
        call MPI_IRECV(recv_sph_cor(ist), num, CALYPSO_REAL,            &
     &      ip_recv_cor(i), 0, CALYPSO_COMM, req2_cor(i), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv_cor, req2_cor, sta2_cor,  ierr_MPI)
!
      do k = 1, nri
        do i = 1, ntot_recv_cor
          j = idx_recv_cor(i)
          jnod = j + (k-1)*jmax_cor
          inod = k + (i-1)*nri
          d_cor(jnod,1) = recv_sph_cor(5*inod-4)
          d_cor(jnod,2) = recv_sph_cor(5*inod-3)
          d_cor(jnod,3) = recv_sph_cor(5*inod-2)
          d_cor(jnod,4) = recv_sph_cor(5*inod-1)
          d_cor(jnod,5) = recv_sph_cor(5*inod  )
        end do
      end do
      do k = 1, nri
        do i = 1, jmax
          j = i + nshift_j_cor
          inod = i + (k-1)*jmax
          jnod = j + (k-1)*jmax_cor
          d_cor(jnod,1) = d_rj(inod,1)
          d_cor(jnod,2) = d_rj(inod,2)
          d_cor(jnod,3) = d_rj(inod,3)
          d_cor(jnod,4) = d_rj(inod,4)
          d_cor(jnod,5) = d_rj(inod,5)
        end do
      end do
!
      call MPI_WAITALL (nneib_send_cor, req1_cor, sta1_cor,  ierr_MPI)
!
      end subroutine solver_sph_coriolis_sr_5
!
! -----------------------------------------------------------------------
!
      subroutine solver_sph_coriolis_sr_int                             &
     &          (nshift_j_cor, jmax, idx_j, jmax_cor, idx_cor_j)
!
      integer(kind = kint), intent(in) :: jmax, nshift_j_cor
      integer(kind = kint), intent(in) :: idx_j(jmax)
!
      integer(kind = kint), intent(in) :: jmax_cor
      integer(kind = kint), intent(inout) :: idx_cor_j(jmax_cor)
!
      integer(kind = kint) :: i, j, ist, num
!
!
!C-- SEND
      do i = 1, ntot_send_cor
        j = idx_send_cor(i)
        isend_sph_cor(i) = idx_j(j)
      end do
!
      do i = 1, nneib_send_cor
        ist = istack_send_cor(i-1) + 1
        num = istack_send_cor(i) - istack_send_cor(i-1)
        call MPI_ISEND(isend_sph_cor(ist), num, CALYPSO_INTEGER,        &
     &      ip_send_cor(i), 0, CALYPSO_COMM, req1_cor(i),  ierr_MPI)
      end do
!C
!C-- RECEIVE
      do i= 1, nneib_recv_cor
        ist = istack_recv_cor(i-1) + 1
        num = istack_recv_cor(i) - istack_recv_cor(i-1)
        call MPI_IRECV(irecv_sph_cor(ist), num, CALYPSO_INTEGER,        &
     &      ip_recv_cor(i), 0, CALYPSO_COMM, req2_cor(i), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv_cor, req2_cor, sta2_cor,  ierr_MPI)
!
      do i = 1, ntot_recv_cor
        j = idx_recv_cor(i)
        idx_cor_j(j) = irecv_sph_cor(i)
      end do
      do i = 1, jmax
        j = i + nshift_j_cor
        idx_cor_j(j) = idx_j(i)
      end do
!
      call MPI_WAITALL (nneib_send_cor, req1_cor, sta1_cor,  ierr_MPI)
!
      end subroutine solver_sph_coriolis_sr_int
!
! -----------------------------------------------------------------------
!
      end module solver_sph_coriolis_sr
