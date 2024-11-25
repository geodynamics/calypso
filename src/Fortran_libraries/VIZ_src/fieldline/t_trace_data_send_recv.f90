!>@file   t_trace_data_send_recv.f90
!!@brief  module t_trace_data_send_recv
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine alloc_trace_data_SR_num(viz_fields, fln_SR)
!!      subroutine dealloc_trace_data_SR_num(fln_SR)
!!      subroutine s_trace_data_send_recv(fln_prm, fln_tce, fln_SR,     &
!!     &                                  SR_sig, nline_global)
!!@endverbatim
!
      module t_trace_data_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_para_double_numbering
      use t_control_params_4_fline
!
      implicit  none
!
      integer(kind= kint), parameter, private :: nitem8_export = 1
      integer(kind= kint), parameter, private :: nitem_export =  6
!
      type trace_data_send_recv
        integer(kind = kint) :: npe_send
        integer(kind = kint) :: npe_recv
        integer(kind = kint), allocatable :: num_send(:)
        integer(kind = kint), allocatable :: num_recv(:)
!
        integer(kind = kint), allocatable :: id_pe_send(:)
        integer(kind = kint), allocatable :: ineib_send(:)
        integer(kind = kint), allocatable :: istack_send(:)
        integer(kind = kint), allocatable :: istack_isend(:)
        integer(kind = kint), allocatable :: icou_send(:)
!
        integer(kind = kint), allocatable :: id_pe_recv(:)
        integer(kind = kint), allocatable :: ineib_recv(:)
        integer(kind = kint), allocatable :: istack_recv(:)
        integer(kind = kint), allocatable :: istack_irecv(:)
!
        integer(kind = kint) :: ncomp_export
        integer(kind = kint) :: ntot_send
        integer(kind = kint) :: ntot_sendbuf
        integer(kind = kint), allocatable ::    item_send(:)
        integer(kind = kint_gl), allocatable :: i8Send(:,:)
        integer(kind = kint), allocatable ::    iSend(:,:)
        real(kind = kreal),   allocatable ::    rSend(:,:)
        integer(kind = kint) :: ntot_recv
        integer(kind = kint) :: ntot_recvbuf
        integer(kind = kint), allocatable ::    item_recv(:)
        integer(kind = kint_gl), allocatable :: i8Recv(:,:)
        integer(kind = kint), allocatable ::    iRecv(:,:)
        real(kind = kreal),   allocatable ::    rRecv(:,:)
      end type trace_data_send_recv
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_data_SR_num(viz_fields, fln_SR)
!
      use t_ctl_params_viz_fields
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_sendbuf = 0
      fln_SR%ntot_recvbuf = 0
      fln_SR%ncomp_export = 9 + viz_fields%ntot_color_comp

      allocate(fln_SR%num_send(nprocs))
      allocate(fln_SR%num_recv(nprocs))
!
      allocate(fln_SR%id_pe_send(nprocs))
      allocate(fln_SR%ineib_send(nprocs))
      allocate(fln_SR%istack_send(0:nprocs))
      allocate(fln_SR%istack_isend(0:nprocs))
      allocate(fln_SR%icou_send(nprocs))

      allocate(fln_SR%id_pe_recv(nprocs))
      allocate(fln_SR%ineib_recv(nprocs))
      allocate(fln_SR%istack_recv(0:nprocs))
      allocate(fln_SR%istack_irecv(0:nprocs))
!
!$omp parallel workshare
      fln_SR%num_send(1:nprocs) =   0
      fln_SR%num_recv(1:nprocs) =   0
      fln_SR%ineib_send(1:nprocs) = 0
      fln_SR%ineib_recv(1:nprocs) = 0
      fln_SR%id_pe_send(1:nprocs) = -1
      fln_SR%id_pe_recv(1:nprocs) = -1
      fln_SR%icou_send(1:nprocs) =  0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%istack_send(0:nprocs) =   0
      fln_SR%istack_recv(0:nprocs) =   0
      fln_SR%istack_isend(0:nprocs) =  0
      fln_SR%istack_irecv(0:nprocs) =  0
!$omp end parallel workshare
!
      call alloc_trace_SR_export(ione, fln_SR)
      call alloc_trace_SR_import(ione, fln_SR)
!
      end subroutine alloc_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_data_send_recv(fln_prm, fln_tce, fln_SR,       &
     &                                  SR_sig, nline_global)
!
      use calypso_SR
      use calypso_SR_core
      use set_to_send_buffer
      use solver_SR_int
      use solver_SR_int8
      use set_to_send_buffer
      use select_copy_from_recv
      use t_solver_SR
      use t_tracing_data
!
      type(fieldline_paramter), intent(in) :: fln_prm
!
      integer(kind = kint), intent(inout) :: nline_global
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(send_recv_status), intent(inout) :: SR_sig
!
!      integer(kind = kint) :: i
!
      call count_trace_data_SR_npe(fln_tce, fln_SR)
      call count_trace_data_SR_num(fln_SR)
      call raise_trace_SR_export(fln_SR%ntot_send, fln_SR)
      call raise_trace_SR_import(fln_SR)

      call set_trace_data_to_SR(fln_tce, fln_SR)
!
      call resize_SR_flag(fln_SR%npe_send, fln_SR%npe_recv, SR_sig)
      call calypso_send_recv_core                                       &
     &   (fln_SR%ncomp_export, fln_SR%npe_send, fln_SR%id_pe_send,      &
     &    fln_SR%istack_send, fln_SR%rSend(1,1),                        &
     &    fln_SR%npe_recv, fln_SR%id_pe_recv,                           &
     &    fln_SR%istack_recv, 0, fln_SR%rRecv, SR_sig)
      call calypso_send_recv_fin(fln_SR%npe_send, 0, SR_sig)
!
      call calypso_send_recv_intcore                                    &
     &   (fln_SR%npe_send, fln_SR%id_pe_send,                           &
     &    fln_SR%istack_isend, fln_SR%iSend(1,1), 0,                    &
     &    fln_SR%npe_recv, fln_SR%id_pe_recv,                           &
     &    fln_SR%istack_irecv, fln_SR%iRecv(1,1), SR_sig)
      call calypso_send_recv_fin(fln_SR%npe_send, 0, SR_sig)
!
      call calypso_send_recv_i8core                                     &
     &   (fln_SR%npe_send, fln_SR%id_pe_send,                           &
     &    fln_SR%istack_isend, fln_SR%i8Send(1,1), 0,                   &
     &    fln_SR%npe_recv, fln_SR%id_pe_recv,                           &
     &    fln_SR%istack_irecv, fln_SR%i8Recv(1,1), SR_sig)
      call calypso_send_recv_fin(fln_SR%npe_send, 0, SR_sig)
!
      call set_trace_data_from_SR(fln_SR, fln_prm, fln_tce)
      nline_global = fln_tce%istack_current_fline(nprocs)               &
     &              - fln_tce%istack_current_fline(0)
!
      end subroutine s_trace_data_send_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_data_SR_num(fln_SR)
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      deallocate(fln_SR%istack_isend, fln_SR%istack_irecv)
      deallocate(fln_SR%istack_send,  fln_SR%istack_recv)
      deallocate(fln_SR%ineib_send,   fln_SR%ineib_recv)
      deallocate(fln_SR%id_pe_send,   fln_SR%id_pe_recv)
      deallocate(fln_SR%num_send,     fln_SR%num_recv)
      deallocate(fln_SR%icou_send)
!
      end subroutine dealloc_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine raise_trace_SR_export(nnod_org, fln_SR)
!
      integer(kind = kint), intent(in) :: nnod_org
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(fln_SR%ntot_sendbuf .gt. nnod_org) return
      call dealloc_trace_SR_export(fln_SR)
      call alloc_trace_SR_export(nnod_org, fln_SR)
!
      end subroutine raise_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine raise_trace_SR_import(fln_SR)
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(fln_SR%ntot_recvbuf .gt. fln_SR%ntot_recv) return
      call dealloc_trace_SR_import(fln_SR)
      call alloc_trace_SR_import(fln_SR%ntot_recv, fln_SR)
!
      end subroutine raise_trace_SR_import
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_SR_export(ntot, fln_SR)
!
      integer(kind = kint), intent(in) :: ntot
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_sendbuf = ntot
      allocate(fln_SR%item_send(ntot))
      allocate(fln_SR%iSend(nitem_export,ntot))
      allocate(fln_SR%rSend(fln_SR%ncomp_export,ntot))
      return
!
      if(ntot .le. 0) return
!$omp parallel workshare
      fln_SR%item_send(:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%iSend(:,:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%i8Send(:,:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%rSend(:,:) = 0
!$omp end parallel workshare
!
      end subroutine alloc_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_SR_import(ntot, fln_SR)
!
      integer(kind = kint), intent(in) :: ntot
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_recvbuf = ntot
      allocate(fln_SR%item_recv(ntot))
      allocate(fln_SR%iRecv(nitem_export,ntot))
      allocate(fln_SR%i8Recv(nitem8_export,ntot))
      allocate(fln_SR%rRecv(fln_SR%ncomp_export,ntot))
      return
!
      if(ntot .le. 0) return
!$omp parallel workshare
      fln_SR%item_recv(:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%iRecv(:,:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%i8Recv(:,:) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%rRecv(:,:) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_trace_SR_import
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_SR_export(fln_SR)
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(allocated(fln_SR%item_send) .eqv. .FALSE.) return
      deallocate(fln_SR%item_send)
      deallocate(fln_SR%iSend)
      deallocate(fln_SR%i8Send)
      deallocate(fln_SR%rSend)
!
      end subroutine dealloc_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_SR_import(fln_SR)
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(allocated(fln_SR%item_recv) .eqv. .FALSE.) return
      deallocate(fln_SR%item_recv)
      deallocate(fln_SR%rRecv)
      deallocate(fln_SR%i8Recv)
      deallocate(fln_SR%iRecv)
!
      end subroutine dealloc_trace_SR_import
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_trace_data_SR_npe(fln_tce, fln_SR)
!
      use calypso_mpi_int
      use t_tracing_data
!
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: i, ip
!
!$omp parallel workshare
      fln_SR%num_send(1:nprocs) =   0
      fln_SR%num_recv(1:nprocs) =   0
      fln_SR%ineib_send(1:nprocs) = 0
      fln_SR%ineib_recv(1:nprocs) = 0
      fln_SR%id_pe_send(1:nprocs) = -1
      fln_SR%id_pe_recv(1:nprocs) = -1
      fln_SR%icou_send(1:nprocs) =  0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%istack_send(0:nprocs) =   0
      fln_SR%istack_recv(0:nprocs) =   0
      fln_SR%istack_isend(0:nprocs) =  0
      fln_SR%istack_irecv(0:nprocs) =  0
!$omp end parallel workshare

      do i =  1, fln_tce%num_current_fline
        if(fln_tce%iflag_comm_start(i) .ne. ione) cycle
        if(fln_tce%isf_dbl_start(1,i) .eq. my_rank) cycle
!
        ip = fln_tce%isf_dbl_start(1,i) + 1
            fln_SR%num_send(ip) = fln_SR%num_send(ip) + 1
      end do
      call calypso_mpi_alltoall_one_int(fln_SR%num_send,                &
     &                                  fln_SR%num_recv)
!
      fln_SR%npe_send = 0
      do ip = 1, nprocs
        if(fln_SR%num_send(ip) .gt. 0) then
          fln_SR%npe_send = fln_SR%npe_send + 1
        end if
      end do
      fln_SR%npe_recv = 0
      do ip = 1, nprocs
        if(fln_SR%num_recv(ip) .gt. 0) then
          fln_SR%npe_recv = fln_SR%npe_recv + 1
        end if
      end do
!
      end subroutine count_trace_data_SR_npe
!
!  ---------------------------------------------------------------------
!
      subroutine count_trace_data_SR_num(fln_SR)
!
      use t_tracing_data
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: ip, icou
!
      icou = 0
      fln_SR%istack_send(0) = 0
      do ip = 1, nprocs
        if(fln_SR%num_send(ip) .gt. 0) then
          icou = icou + 1
          fln_SR%ineib_send(ip) = icou
          fln_SR%id_pe_send(icou) = ip-1
          fln_SR%istack_send(icou) = fln_SR%istack_send(icou-1)         &
     &                            + fln_SR%num_send(ip)
        end if
      end do
      fln_SR%ntot_send = fln_SR%istack_send(fln_SR%npe_send)

      icou = 0
      fln_SR%istack_recv(0) = 0
      do ip = 1, nprocs
        if(fln_SR%num_recv(ip) .gt. 0) then
          icou = icou + 1
          fln_SR%ineib_recv(ip) = icou
          fln_SR%id_pe_recv(icou) = ip-1
          fln_SR%istack_recv(icou) = fln_SR%istack_recv(icou-1)         &
     &                            + fln_SR%num_recv(ip)
        end if
      end do
      fln_SR%ntot_recv = fln_SR%istack_recv(fln_SR%npe_recv)

      fln_SR%istack_isend(0:fln_SR%npe_send)                            &
     &      = nitem_export * fln_SR%istack_send(0:fln_SR%npe_send)
      fln_SR%istack_irecv(0:fln_SR%npe_recv)                            &
     &      = nitem_export * fln_SR%istack_recv(0:fln_SR%npe_recv)
!
      end subroutine count_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine set_trace_data_to_SR(fln_tce, fln_SR)
!
      use t_tracing_data
!
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: inum, icou
      integer(kind = kint) :: irank_send, ineib_tmp
!
      do inum = 1, fln_SR%ntot_recv
        fln_SR%item_recv(inum) = inum
      end do

      fln_SR%icou_send(1:nprocs) = fln_SR%istack_send(0:nprocs-1)
      do inum = 1, fln_tce%num_current_fline
        if(fln_tce%iflag_comm_start(inum) .ne. ione) cycle
        if(fln_tce%isf_dbl_start(1,inum) .eq. my_rank) cycle
        
        irank_send = fln_tce%isf_dbl_start(1,inum)
        ineib_tmp = fln_SR%ineib_send(irank_send+1)
        fln_SR%icou_send(ineib_tmp) = fln_SR%icou_send(ineib_tmp) + 1
        icou = fln_SR%icou_send(ineib_tmp)
        fln_SR%item_send(icou) = inum
      end do
!
!$omp parallel do private(icou,inum)
      do icou = 1, fln_SR%ntot_send
        inum = fln_SR%item_send(icou)
!
        fln_SR%i8Send(1,icou) =  fln_tce%iline_original(inum)
!
        fln_SR%iSend(1,icou) =   my_rank
        fln_SR%iSend(2,icou) =   fln_tce%iflag_direction(inum)
        fln_SR%iSend(3,icou) =   fln_tce%icount_fline(inum)
        fln_SR%iSend(4:6,icou) = fln_tce%isf_dbl_start(1:3,inum)   
!
        fln_SR%rSend(1:4,icou) = fln_tce%xx_fline_start(1:4,inum)
        fln_SR%rSend(5:8,icou) = fln_tce%v_fline_start(1:4,inum)
        fln_SR%rSend(9,icou) =   fln_tce%trace_length(inum)
        fln_SR%rSend(9+1:fln_SR%ncomp_export,icou)                      &
    &         = fln_tce%c_fline_start(1:fln_SR%ncomp_export-9,inum)
      end do
!$omp end parallel do
!
      end subroutine set_trace_data_to_SR
!
!  ---------------------------------------------------------------------
!
      subroutine set_trace_data_from_SR(fln_SR, fln_prm, fln_tce)
!
      use calypso_mpi_int
      use t_tracing_data
!
      type(trace_data_send_recv), intent(in) :: fln_SR
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ip, i
!
      fln_tce%num_current_fline = fln_SR%ntot_recv
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%istack_current_fline(ip)
      end do
      do i = 1, fln_SR%ntot_recv
          fln_tce%iline_original(i) =      fln_SR%i8Recv(1,i)
!
          fln_tce%iflag_direction(i) =     fln_SR%iRecv(2,i)
          fln_tce%icount_fline(i) =        fln_SR%iRecv(3,i)
          fln_tce%isf_dbl_start(1:3,i) =   fln_SR%iRecv(4:6,i)
!
          fln_tce%xx_fline_start(1:4,i) = fln_SR%rRecv(1:4,i)
          fln_tce%v_fline_start(1:4,i) =  fln_SR%rRecv(5:8,i)
          fln_tce%trace_length(i) =       fln_SR%rRecv(9,i)
          fln_tce%c_fline_start(1:fln_SR%ncomp_export-9,i)              &
     &            = fln_SR%rRecv(9+1:fln_SR%ncomp_export,i)
      end do
!
      end subroutine set_trace_data_from_SR
!
!  ---------------------------------------------------------------------
!
      end module t_trace_data_send_recv

