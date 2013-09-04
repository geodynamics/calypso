!m_merge_spheric_mesh.f90
!      module m_merge_spheric_mesh
!
!     Written by H. Matsui on Feb., 2011
!
!      subroutine alloc_sph_mesh_4_merge
!      subroutine dealloc_sph_mesh_4_merge
!
!      subroutine set_local_rj_mesh_4_merge(my_rank, sph_mesh)
!        integer(kind = kint), intent(in) :: my_rank
!        type(sph_mesh_data), intent(inout) :: sph_mesh
!      subroutine set_sph_boundary_4_merge(sph_grps,                    &
!     &          nlayer_ICB, nlayer_CMB)
!        type(sph_group_data), intent(in) ::  sph_grps
!        integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!
      module m_merge_spheric_mesh
!
      use m_precision
      use t_spheric_mesh
!
      implicit none
!
!
      integer(kind = kint) :: np_sph_org
      type(sph_mesh_data), pointer :: org_sph_mesh(:)
      integer(kind = kint) :: np_sph_new
      type(sph_mesh_data), pointer :: new_sph_mesh(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_mesh_4_merge
!
!
      allocate( org_sph_mesh(np_sph_org) )
      allocate( new_sph_mesh(np_sph_new) )
!
      end subroutine alloc_sph_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_mesh_4_merge
!
!
      deallocate(org_sph_mesh, new_sph_mesh)
!
      end subroutine dealloc_sph_mesh_4_merge
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_local_rj_mesh_4_merge(my_rank, sph_mesh_set)
!
      use m_node_id_spherical_IO
      use sph_file_IO_select
      use copy_sph_node_type_IO
      use copy_sph_comm_tbl_type_4_IO
      use copy_sph_grps_type_from_IO
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_mesh_data), intent(inout) :: sph_mesh_set
!
!
      call sel_read_spectr_modes_rj_file(my_rank)
!
      call copy_sph_node_type_rj_from_IO                                &
     &    (sph_mesh_set%sph_mesh%l_truncation,                          &
     &     sph_mesh_set%sph_mesh%sph_rj)
      call copy_comm_sph_type_from_IO(my_rank,                          &
     &    sph_mesh_set%sph_mesh%sph_rj%nnod_rj,                         &
     &    sph_mesh_set%sph_comms%comm_rj)
!
      call copy_rj_radial_grp_type_from_IO(sph_mesh_set%sph_grps)
      call copy_rj_sphere_grp_type_from_IO(sph_mesh_set%sph_grps)
!
      end subroutine set_local_rj_mesh_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_boundary_4_merge(sph_grps,                     &
     &          nlayer_ICB, nlayer_CMB)
!
      type(sph_group_data), intent(in) ::  sph_grps
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!
      integer(kind = kint) :: k, kk
!
!
      do k = 1, sph_grps%radial_rj_grp%num_grp
        if     (sph_grps%radial_rj_grp%grp_name(k) .eq. 'ICB'           &
     &     .or. sph_grps%radial_rj_grp%grp_name(k) .eq. 'icb') then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_ICB = sph_grps%radial_rj_grp%item_grp(kk)
        else if(sph_grps%radial_rj_grp%grp_name(k) .eq. 'CMB'           &
     &     .or. sph_grps%radial_rj_grp%grp_name(k) .eq. 'cmb') then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_CMB = sph_grps%radial_rj_grp%item_grp(kk)
        end if
      end do
!
      end subroutine set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
!
      end module m_merge_spheric_mesh
