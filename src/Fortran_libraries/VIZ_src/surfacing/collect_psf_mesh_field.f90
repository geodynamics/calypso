!>@file   collect_psf_mesh_field.f90
!!@brief  module collect_psf_mesh_field
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine init_merge_psf_mesh(irank_draw, psf_mesh,            &
!!     &          psf_nod, psf_ele, psf_phys, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        type(psf_local_data), intent(in) :: psf_mesh
!!        type(node_data), intent(inout) ::    psf_nod
!!        type(element_data), intent(inout) :: psf_ele
!!        type(phys_data), intent(inout) ::    psf_phys
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,&
!!     &                              d_img, SR_sig)
!!        integer, intent(in) :: irank_draw
!!        integer(kind = kint) , intent(in) :: ifld_img
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: field
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_img(node%istack_internod(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
      module collect_psf_mesh_field
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_solver_SR
!
      implicit  none
!
      private :: collect_psf_node, collect_psf_element
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_merge_psf_mesh(irank_draw, psf_mesh,              &
     &          psf_nod, psf_ele, psf_phys, SR_sig)
!
      use t_psf_patch_data
!
      use append_phys_data
      use cal_mesh_position
      use set_ucd_data_to_type
!
      integer, intent(in) :: irank_draw
      type(psf_local_data), intent(in) :: psf_mesh
      type(node_data), intent(inout) ::    psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) ::    psf_phys
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: i
!
      psf_nod%numnod = 0
      psf_ele%numele = 0
      psf_ele%nnod_4_ele = psf_mesh%patch%nnod_4_ele
      if(my_rank .eq. irank_draw) then
        psf_nod%numnod = int(psf_mesh%node%istack_internod(nprocs))
        psf_ele%numele = int(psf_mesh%patch%istack_interele(nprocs))
      end if
      psf_nod%internal_node = psf_nod%numnod
!
      call alloc_node_geometry_w_sph(psf_nod)
      call collect_psf_node(irank_draw, psf_mesh%node,                  &
     &                      psf_nod%xx, SR_sig)
      call set_spherical_position(psf_nod)
!
      call copy_field_name(psf_mesh%field, psf_phys)
      call alloc_phys_data(psf_nod%numnod, psf_phys)
      call calypso_mpi_barrier
!
!$omp parallel do
      do i = 1, psf_nod%numnod
        psf_nod%inod_global(i) = i
      end do
!$omp end parallel do
!
!
      call alloc_element_types(psf_ele)
      psf_ele%first_ele_type = psf_mesh%patch%first_ele_type
!$omp parallel do
      do i = 1, psf_ele%numele
        psf_ele%iele_global(i) = i
        psf_ele%elmtyp(i) = psf_mesh%patch%elmtyp(1)
        psf_ele%nodelm(i) = psf_mesh%patch%nodelm(1)
      end do
!$omp end parallel do
!
      call alloc_ele_connectivity(psf_ele)
      call collect_psf_element(irank_draw, psf_mesh%patch,              &
     &                         psf_ele%ie, SR_sig)
      call calypso_mpi_barrier
!
      end subroutine init_merge_psf_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_scalar(irank_draw, ifld_img, node, field,  &
     &                              d_img, SR_sig)
!
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      integer(kind = kint) , intent(in) :: ifld_img
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: field
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_img(node%istack_internod(nprocs))
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nnod
!
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      call collect_send_recv_N(irank_draw, n_scalar, nnod,              &
     &                         field%d_fld(1,ifld_img),                 &
     &                         node%istack_internod, d_img(1), SR_sig)
!
      end subroutine collect_psf_scalar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_node(irank_draw, node, xx_out, SR_sig)
!
      use m_phys_constants
      use collect_SR_N
!
      integer, intent(in) :: irank_draw
      type(node_data), intent(in) :: node
!
      real(kind = kreal), intent(inout)                                 &
     &           :: xx_out(node%istack_internod(nprocs),n_vector)
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      integer(kind = kint) :: nnod, nd
!
      nnod = int(node%istack_internod(my_rank+1)                        &
     &          - node%istack_internod(my_rank))
      do nd = 1, n_vector
        call collect_send_recv_N(irank_draw, ione, nnod,                &
     &                           node%xx(1,nd), node%istack_internod,   &
     &                           xx_out(1,nd), SR_sig)
      end do
!
      end subroutine collect_psf_node
!
!  ---------------------------------------------------------------------
!
      subroutine collect_psf_element(irank_draw, ele, ie_out, SR_sig)
!
      use m_geometry_constants
      use t_solver_SR_int
      use collect_SR_int
!
      integer, intent(in) :: irank_draw
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(inout)                               &
     &              :: ie_out(ele%istack_interele(nprocs),num_triangle)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint) :: nele, nd
!
!
      nele = int(ele%istack_interele(my_rank+1)                         &
     &           - ele%istack_interele(my_rank))
      do nd = 1, num_triangle
        call collect_send_recv_int(irank_draw, ione, nele,              &
     &                             ele%ie(1,nd), ele%istack_interele,   &
     &                             ie_out(1,nd), SR_sig)
      end do
!
      end subroutine collect_psf_element
!
!  ---------------------------------------------------------------------
!
      end module collect_psf_mesh_field
