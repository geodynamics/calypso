!>@file   merge_output_4_psf.f90
!!@brief  module merge_output_4_psf
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Collect and output routines for surfacing module
!!
!!@verbatim
!!      subroutine merge_ucd_psf_data(irank_tgt, psf_mesh, psf_out)
!!      subroutine merge_ucd_psf_mesh(irank_tgt, psf_mesh, psf_ucd)
!!@endverbatim
!
      module merge_output_4_psf
!
      use m_precision
      use m_constants
      use m_phys_constants
      use calypso_mpi
!
      use t_psf_patch_data
      use t_ucd_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine merge_ucd_psf_data(irank_tgt, psf_mesh, psf_out)
!
      use collect_to_rank0
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: irank_tgt
      type(psf_local_data), intent(in) :: psf_mesh
      type(ucd_data), intent(inout) :: psf_out
!
      integer(kind = kint) :: nnod4, ntot4
      integer(kind = kint) :: istack4(0:nprocs)
!
!
      nnod4 = int(psf_mesh%node%numnod)
      ntot4 = int(psf_out%nnod)
      istack4(0:nprocs) = int(psf_mesh%node%istack_internod(0:nprocs))
!
      call collect_vectors_to_rank0(nnod4, psf_out%ntot_comp, istack4,  &
     &    psf_mesh%field%d_fld, irank_tgt, ntot4, psf_out%d_ucd)
!
      end subroutine merge_ucd_psf_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine merge_ucd_psf_mesh(irank_tgt, psf_mesh, psf_ucd)
!
      use collect_to_rank0
!
      integer(kind = kint), intent(in) :: irank_tgt
      type(psf_local_data), intent(in) :: psf_mesh
      type(ucd_data), intent(inout) :: psf_ucd
!
      integer(kind = kint) :: nnod4, ntot4
      integer(kind = kint) :: istack4(0:nprocs)
      integer(kind = kint), allocatable :: ie4(:,:)
!
!
      if(my_rank .eq. irank_tgt) then
        psf_ucd%nnod = psf_mesh%node%istack_internod(nprocs)
        psf_ucd%nele = psf_mesh%patch%istack_numele(nprocs)
      else
        psf_ucd%nnod = 0
        psf_ucd%nele = 0
      end if
      psf_ucd%nnod_4_ele = psf_mesh%patch%nnod_4_ele
      psf_ucd%num_field =  psf_mesh%field%num_phys
      psf_ucd%ntot_comp =  psf_mesh%field%ntot_phys
      psf_ucd%nnod_4_ele = psf_mesh%patch%nnod_4_ele
!
      call allocate_ucd_nodal_data(psf_ucd)
      call allocate_ucd_ele(psf_ucd)
      allocate(ie4(psf_ucd%nele,psf_ucd%nnod_4_ele))
!
      psf_ucd%num_comp(1:psf_ucd%num_field)                            &
     &        = psf_mesh%field%num_component(1:psf_ucd%num_field)
      psf_ucd%phys_name(1:psf_ucd%num_field)                           &
     &        = psf_mesh%field%phys_name(1:psf_ucd%num_field)
!
      nnod4 = int(psf_mesh%node%numnod)
      ntot4 = int(psf_ucd%nnod)
      istack4(0:nprocs) = int(psf_mesh%node%istack_internod(0:nprocs))
!
      call collect_int8s_to_rank0(nnod4, ione, istack4,                 &
     &    psf_mesh%node%inod_global, irank_tgt,                         &
     &    ntot4, psf_ucd%inod_global)
!
      call collect_vectors_to_rank0(nnod4, n_vector, istack4,           &
     &    psf_mesh%node%xx, irank_tgt, ntot4, psf_ucd%xx)
!
!
      nnod4 = int(psf_mesh%patch%numele)
      ntot4 = int(psf_ucd%nele)
      istack4(0:nprocs) = int(psf_mesh%patch%istack_numele(0:nprocs))
!
      call collect_int8s_to_rank0(nnod4, ione, istack4,                 &
     &    psf_mesh%patch%iele_global, irank_tgt,                        &
     &    ntot4, psf_ucd%iele_global)
!
      call collect_integers_to_rank0(nnod4, psf_ucd%nnod_4_ele,         &
     &    istack4, psf_mesh%patch%ie, irank_tgt, ntot4, ie4)
!
      if(my_rank .eq. irank_tgt) then
        psf_ucd%ie(1:psf_ucd%nele,1:psf_ucd%nnod_4_ele)                 &
     &              = ie4(1:psf_ucd%nele,1:psf_ucd%nnod_4_ele)
      end if
      deallocate(ie4)
!
      end subroutine merge_ucd_psf_mesh
!
! -----------------------------------------------------------------------
!
      end module merge_output_4_psf

