!>@file  psf_send_recv.f90
!!       module psf_send_recv
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Collect sectioning data to head node
!!
!!@verbatim
!!      subroutine psf_grids_send_recv(num_psf, nnod_psf, ntot_output,  &
!!     &         istack_nod_para, istack_nod_recv, xx_psf,              &
!!     &         send, recv, ucd_out)
!!      subroutine psf_hash_send_recv(num_psf, nnod_psf, ntot_output,   &
!!     &         istack_nod_para, istack_nod_recv, ihash_psf,           &
!!     &         isend, irecv, ihash_psf_gl)
!!
!!      subroutine psf_connect_send_recv(num_psf, nele_psf, ntot_output,&
!!     &         istack_nod_para, istack_ele_para, istack_ele_recv,     &
!!     &         ie_patch, isend, irecv, ucd_out)
!!      subroutine psf_results_send_recv(nnod_psf, ntot_output,         &
!!     &         istack_nod_para, istack_nod_recv, ncomp_dat,           &
!!     &         dat_psf, send, recv, ucd_out)
!!@endverbatim
!
      module psf_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_phys_constants
      use m_mpi_flags_4_section
!
      use t_ucd_data
!
      use copy_psf_data_to_SR
!
      implicit  none
!
      integer(kind = kint), parameter :: rank0 = 0
      private :: rank0
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine psf_grids_send_recv(num_psf, nnod_psf, ntot_output,    &
     &          istack_nod_para, istack_nod_recv, xx_psf,               &
     &          send, recv, ucd_out)
!
      integer(kind = kint), intent(in) :: nnod_psf, num_psf
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ntot_output
      real(kind = kreal), intent(in) :: xx_psf(nnod_psf,n_vector)
!
      real(kind = kreal), intent(inout) :: send(nnod_psf*n_vector)
      real(kind = kreal), intent(inout) :: recv(ntot_output*n_vector)
!
      type(ucd_data), intent(inout) :: ucd_out(num_psf)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist, i_psf
!
!
      call set_real_data_2_send_psf(nnod_psf, n_vector, xx_psf, send)
!
      num_send = n_vector*nnod_psf
      call MPI_ISEND (send(1), num_send, CALYPSO_REAL,                  &
     &      rank0, 0, CALYPSO_COMM,  req1_psf(1), ierr_MPI)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = n_vector*istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = n_vector * (istack_nod_recv(ip*num_psf)            &
     &                       - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (recv(ist), num_recv, CALYPSO_REAL,            &
     &        ip_sent, 0, CALYPSO_COMM, req2_psf(ip), ierr_MPI)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr_MPI)
!
        do i_psf = 1, num_psf
          ist = (i_psf-1)*nprocs
          call set_recv_2_real_data_psf(i_psf, nprocs, num_psf,         &
     &        ntot_output, istack_nod_para(ist), istack_nod_recv,       &
     &        n_vector, recv, ucd_out(i_psf)%nnod,                      &
     &        n_vector, ucd_out(i_psf)%xx)
        end do
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr_MPI)
!
      end subroutine psf_grids_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_hash_send_recv(num_psf, nnod_psf, ntot_output,     &
     &          istack_nod_para, istack_nod_recv, ihash_psf,            &
     &          isend, irecv, ihash_psf_gl)
!
      integer(kind = kint), intent(in) :: nnod_psf, num_psf
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in) :: ihash_psf(nnod_psf)
!
      integer(kind = kint), intent(inout) :: isend(nnod_psf)
      integer(kind = kint), intent(inout) :: irecv(ntot_output)
      integer(kind = kint), intent(inout)                               &
     &                   :: ihash_psf_gl(ntot_output)
!
      integer(kind = kint) :: num_recv
      integer(kind = kint) :: ip, ip_sent, ist, i_psf
!
!
      call set_int_data_2_send_psf(nnod_psf, ione, ihash_psf, isend(1))
!
      call MPI_ISEND(isend(1), nnod_psf, CALYPSO_INTEGER,               &
     &    rank0, 0, CALYPSO_COMM,  req1_psf(1), ierr_MPI)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = (istack_nod_recv(ip*num_psf)                       &
     &              - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (irecv(ist), num_recv, CALYPSO_INTEGER,        &
     &        ip_sent, 0, CALYPSO_COMM, req2_psf(ip), ierr_MPI)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr_MPI)
!
        do i_psf = 1, num_psf
          ist = (i_psf-1)*nprocs
          call set_recv_2_int_data_psf(i_psf, nprocs, num_psf,          &
     &      ntot_output, istack_nod_para(ist), istack_nod_recv,         &
     &      ione, irecv, ihash_psf_gl)
        end do
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr_MPI)
!
      end subroutine psf_hash_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_connect_send_recv(num_psf, nele_psf, ntot_output,  &
     &          istack_nod_para, istack_ele_para, istack_ele_recv,      &
     &          ie_patch, isend, irecv, ucd_out)
!
      integer(kind = kint), intent(in) :: num_psf, nele_psf
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: ie_patch(nele_psf,3)
!
      integer(kind = kint), intent(inout) :: isend(nele_psf*3)
      integer(kind = kint), intent(inout) :: irecv(ntot_output*3)
!
      type(ucd_data), intent(inout) :: ucd_out(num_psf)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist, i_psf
!
!
      call set_int_data_2_send_psf(nele_psf, ithree, ie_patch, isend)
!
      num_send = ithree*nele_psf
      call MPI_ISEND (isend(1), num_send, CALYPSO_INTEGER,              &
     &      rank0, 0, CALYPSO_COMM,  req1_psf(1), ierr_MPI)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = ithree*istack_ele_recv( (ip-1)*num_psf ) + 1
          num_recv = ithree * (istack_ele_recv(ip*num_psf)              &
     &                     - istack_ele_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (irecv(ist), num_recv, CALYPSO_INTEGER,        &
     &        ip_sent, 0, CALYPSO_COMM, req2_psf(ip), ierr_MPI)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr_MPI)
!
        do i_psf = 1, num_psf
          ist = (i_psf-1)*nprocs
          call set_recv_2_ele_connect_psf(i_psf, nprocs, num_psf,       &
     &        ntot_output, istack_nod_para(ist), istack_ele_para(ist),  &
     &        istack_ele_recv, ithree, irecv, ucd_out(i_psf))
        end do
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr_MPI)
!
      end subroutine psf_connect_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine psf_results_send_recv(num_psf, nnod_psf, ntot_output,  &
     &          istack_nod_para, istack_nod_recv, ncomp_dat,            &
     &          dat_psf, send, recv, ucd_out)
!
      integer(kind = kint), intent(in) :: num_psf, nnod_psf, ncomp_dat
      integer(kind = kint), intent(in) :: ntot_output
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      real(kind = kreal), intent(in) :: dat_psf(nnod_psf, ncomp_dat)
!
      real(kind = kreal), intent(inout) :: send(nnod_psf*ncomp_dat)
      real(kind = kreal), intent(inout) :: recv(ntot_output*ncomp_dat)
!
      type(ucd_data), intent(inout) :: ucd_out(num_psf)
!
      integer(kind = kint) :: num_send, num_recv
      integer(kind = kint) :: ip, ip_sent, ist, i_psf
!
!
!
      call set_real_data_2_send_psf(nnod_psf, ncomp_dat, dat_psf, send)
!
      num_send = ncomp_dat*nnod_psf
      call MPI_ISEND (send(1), num_send, CALYPSO_REAL,                  &
     &      rank0, 0, CALYPSO_COMM,  req1_psf(1), ierr_MPI)
!
      if (my_rank .eq. rank0) then
        do ip = 1, nprocs
          ip_sent = ip - 1
          ist = ncomp_dat*istack_nod_recv( (ip-1)*num_psf ) + 1
          num_recv = ncomp_dat * (istack_nod_recv(ip*num_psf)           &
     &                          - istack_nod_recv( (ip-1)*num_psf ) )
          call MPI_IRECV (recv(ist), num_recv, CALYPSO_REAL,            &
     &        ip_sent, 0, CALYPSO_COMM, req2_psf(ip), ierr_MPI)
        end do
!
        call MPI_WAITALL (nprocs, req2_psf, sta2_psf, ierr_MPI)
!
        do i_psf = 1, num_psf
          ist = (i_psf-1)*nprocs
          call set_recv_2_real_data_psf(i_psf, nprocs, num_psf,         &
     &        ntot_output, istack_nod_para(ist), istack_nod_recv,       &
     &        ncomp_dat, recv, ucd_out(i_psf)%nnod,                     &
     &        ucd_out(i_psf)%ntot_comp, ucd_out(i_psf)%d_ucd)
        end do
!
      end if
!
      call MPI_WAITALL (ione, req1_psf(ione), sta1_psf(ione,ione),      &
     &    ierr_MPI)
!
      end subroutine psf_results_send_recv
!
! ----------------------------------------------------------------------
!
      end module psf_send_recv
