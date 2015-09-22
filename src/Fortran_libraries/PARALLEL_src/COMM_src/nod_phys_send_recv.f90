!
!      module nod_phys_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!      subroutine phys_send_recv_all
!      subroutine phys_send_recv_4_viz
!
!      subroutine scalar_send_recv(ntot_comp, id_phys, d_nod)
!      subroutine vector_send_recv(ntot_comp, id_phys, d_nod)
!      subroutine sym_tensor_send_recv(ntot_comp, id_phys, d_nod)
!         id_phys:  field ID of nodal fields
!
      module nod_phys_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_geometry_data
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
      use m_node_phys_data
!
      call  nod_fields_send_recv(nod_fld1%num_phys)
!
      end subroutine phys_send_recv_all
!
! ----------------------------------------------------------------------
!
      subroutine phys_send_recv_4_viz
!
      use m_node_phys_data
!
      call  nod_fields_send_recv(nod_fld1%num_phys_viz)
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
      use m_node_phys_data
!
      integer (kind=kint), intent(in) :: num_fld
      integer (kind=kint) :: i, ist
!
!
      do i = 1, num_fld
        ist = nod_fld1%istack_component(i-1) + 1
!
        if (nod_fld1%num_component(i) .eq. n_vector) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for vector of ', trim(nod_fld1%phys_name(i))
          call vector_send_recv                                         &
     &       (nod_fld1%ntot_phys, ist, nod_fld1%d_fld)
!
        else if (nod_fld1%num_component(i) .eq. n_scalar) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for scaler of ', trim(nod_fld1%phys_name(i))
          call scalar_send_recv                                         &
     &       (nod_fld1%ntot_phys, ist, nod_fld1%d_fld)
!
        else if (nod_fld1%num_component(i) .eq. n_sym_tensor) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for tensor of ', trim(nod_fld1%phys_name(i))
          call sym_tensor_send_recv                                     &
     &       (nod_fld1%ntot_phys, ist, nod_fld1%d_fld)
        end if
      end do
!
      end subroutine nod_fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv(ntot_comp, id_phys, d_nod)
!
      use nodal_vector_send_recv
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call nod_scalar_send_recv( d_nod(1,id_phys) )
!
      end subroutine scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv(ntot_comp, id_phys, d_nod)
!
      use nodal_vector_send_recv
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call nod_vector_send_recv( d_nod(1,id_phys) )
!
      end subroutine vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_send_recv(ntot_comp, id_phys, d_nod)
!
      use nodal_vector_send_recv
!
      integer(kind = kint), intent(in) :: ntot_comp, id_phys
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node1%numnod,ntot_comp)
!
!
      call nod_tensor_send_recv( d_nod(1,id_phys) )
!
      end subroutine sym_tensor_send_recv
!
! ----------------------------------------------------------------------
!
      end module nod_phys_send_recv
