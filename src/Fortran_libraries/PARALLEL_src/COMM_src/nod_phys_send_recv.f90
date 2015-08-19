!
!      module nod_phys_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!      subroutine phys_send_recv_all
!      subroutine phys_send_recv_4_viz
!
!      subroutine scalar_send_recv(id_phys)
!      subroutine vector_send_recv(id_phys)
!      subroutine sym_tensor_send_recv(id_phys)
!         id_phys:  field ID of nodal fields
!
      module nod_phys_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_node_phys_data
!
      implicit none
!
      private :: nod_fields_send_recv
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine phys_send_recv_all
!
!
      call  nod_fields_send_recv(num_nod_phys)
!
      end subroutine phys_send_recv_all
!
! ----------------------------------------------------------------------
!
      subroutine phys_send_recv_4_viz
!
!
      call  nod_fields_send_recv(num_nod_phys_vis)
!
      end subroutine phys_send_recv_4_viz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine nod_fields_send_recv(num_fld)
!
      use m_machine_parameter
      use m_phys_constants
!
      integer (kind=kint), intent(in) :: num_fld
      integer (kind=kint) :: i, ist
!
!
      do i = 1, num_fld
        ist = istack_nod_component(i-1) + 1
!
        if (num_nod_component(i) .eq. n_vector) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &      write(*,*) 'comm. for vector of ', trim(phys_nod_name(i))
          call vector_send_recv(ist)
!
        else if (num_nod_component(i) .eq. n_scalar) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &       write(*,*) 'comm. for scaler of ', trim(phys_nod_name(i))
          call scalar_send_recv(ist)
!
        else if (num_nod_component(i) .eq. n_sym_tensor) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &       write(*,*) 'comm. for tensor of ', trim(phys_nod_name(i))
          call sym_tensor_send_recv(ist)
        end if
      end do
!
      end subroutine nod_fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv(id_phys)
!
      use nodal_vector_send_recv
!
      integer (kind = kint), intent(in) :: id_phys
!
!
      call nod_scalar_send_recv( d_nod(1,id_phys) )
!
      end subroutine scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv(id_phys)
!
      use nodal_vector_send_recv
!
      integer (kind = kint), intent(in) :: id_phys
!
!
      call nod_vector_send_recv( d_nod(1,id_phys) )
!
      end subroutine vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_send_recv(id_phys)
!
      use nodal_vector_send_recv
!
      integer (kind = kint), intent(in) :: id_phys
!
!
      call nod_tensor_send_recv( d_nod(1,id_phys) )
!
      end subroutine sym_tensor_send_recv
!
! ----------------------------------------------------------------------
!
      end module nod_phys_send_recv
