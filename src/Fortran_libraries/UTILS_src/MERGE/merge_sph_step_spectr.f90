!merge_sph_step_spectr.f90
!      module merge_sph_step_spectr
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine set_sph_rj_mesh_4_merge
!      subroutine s_assenble_sph_step_spectr(istep)
!
      module merge_sph_step_spectr
!
      use m_precision
      use m_constants
      use m_field_data_IO
!
      implicit none
!
      integer(kind = kint) :: nlayer_ICB_org, nlayer_CMB_org
      integer(kind = kint) :: nlayer_ICB_new, nlayer_CMB_new
!
      private :: nlayer_ICB_org, nlayer_CMB_org
      private :: nlayer_ICB_new, nlayer_CMB_new
      private :: set_global_sph_grid_4_merge
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_rj_mesh_4_merge
!
      use m_control_param_newsph
      use m_merge_spheric_mesh
      use m_node_id_spherical_IO
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint) :: ip, my_rank
!
!
!  set original spectr data
!
      iflag_sph_file_fmt = ifmt_org_sph_file
      do ip = 1, np_sph_org
        my_rank = ip - 1
        call set_local_rj_mesh_4_merge(my_rank, org_sph_mesh(ip))
      end do
!
!  set new spectr data
!
      iflag_sph_file_fmt = ifmt_new_sph_file
      do ip = 1, np_sph_new
        my_rank = ip - 1
        sph_head =    new_sph_head
        call set_local_rj_mesh_4_merge(my_rank, new_sph_mesh(ip))
      end do
!
      call set_sph_boundary_4_merge(org_sph_mesh(1)%sph_grps,           &
     &    nlayer_ICB_org, nlayer_CMB_org)
      call set_sph_boundary_4_merge(new_sph_mesh(1)%sph_grps,           &
     &    nlayer_ICB_new, nlayer_CMB_new)
      write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
      write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
!
      call set_global_sph_grid_4_merge
!
!
      phys_file_head = org_sph_fst_head
      call sel_read_alloc_step_SPH_file(izero, istep_start)
      call copy_rj_phys_name_from_IO
      call deallocate_phys_data_IO
      call deallocate_phys_data_name_IO
!
      end subroutine set_sph_rj_mesh_4_merge
!
! -------------------------------------------------------------------
!
      subroutine s_assenble_sph_step_spectr(istep)
!
      use m_control_param_newsph
      use m_merge_spheric_mesh
      use field_IO_select
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint), intent(in) :: istep
!
      integer(kind = kint) :: ip, my_rank
!
!
      phys_file_head = org_sph_fst_head
!
      do ip = 1, np_sph_org
        my_rank = ip - 1
        call sel_read_alloc_step_SPH_file(my_rank, istep)
        call deallocate_phys_data_name_IO
!
        call copy_rj_merged_phys_from_IO                                &
     &      (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,                  &
     &       org_sph_mesh(ip)%sph_mesh%sph_rj%inod_global_rj)
!
        call deallocate_phys_data_IO
      end do
!
      if(b_sph_ratio.ne.0.0d0 .or. b_sph_ratio.ne.1.0d0) then
        call mul_sph_magne
      end if
!
!  set new spectr data
!
      call copy_rj_all_phys_name_to_IO
!
      phys_file_head = new_sph_fst_head
      do ip = 1, np_sph_new
        my_rank = ip - 1
!
        numgrid_phys_IO = new_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj
        call allocate_phys_data_IO
!
        call copy_rj_merged_phys_to_IO                                  &
     &      (new_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,                  &
     &       new_sph_mesh(ip)%sph_mesh%sph_rj%inod_global_rj)
!
        call sel_write_step_SPH_field_file(my_rank, istep)
        call deallocate_phys_data_IO
      end do
!
      call deallocate_phys_data_name_IO
!
      end subroutine s_assenble_sph_step_spectr
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_global_sph_grid_4_merge
!
      use m_merge_spheric_mesh
      use m_spheric_parameter
!
      integer(kind = kint) :: ip, j, k, inod
!
!
      l_truncation = new_sph_mesh(1)%sph_mesh%l_truncation
      nidx_rj(1) = new_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1)
      nidx_rj(2) = 0
      do ip = 1, np_sph_new
        nidx_rj(2) = nidx_rj(2)                                         &
     &              + new_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2)
      end do
      nnod_rj = nidx_rj(1) * nidx_rj(2)
      call allocate_spheric_param_rj
      call allocate_sph_1d_index_rj
!
      do j = 1, nidx_rj(2)
        do k = 1, nidx_rj(1)
          inod = k + (j-1) * nidx_rj(1)
          inod_global_rj(inod) = inod
          idx_global_rj(inod,1) = k
          idx_global_rj(inod,2) = j
        end do
      end do
!
      do k = 1, nidx_rj(1)
        idx_gl_1d_rj_r(k) = k
        radius_1d_rj_r(k)                                               &
     &             = new_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r(k)
      end do
!
      inod = 0
      do ip = 1, np_sph_new
        do j = 1, new_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2)
          inod = inod + 1
          idx_gl_1d_rj_j(inod,1:3)                                      &
     &         = new_sph_mesh(1)%sph_mesh%sph_rj%idx_gl_1d_rj_j(j,1:3)
        end do
      end do
!
      end subroutine set_global_sph_grid_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_merged_phys_from_IO(nnod_rj_org,               &
     &          inod_gl_rj_org)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: nnod_rj_org
      integer(kind = kint), intent(in) :: inod_gl_rj_org(nnod_rj_org)
      integer(kind = kint) :: nd, i, ist, ied, inod, inod_gl
!
!
      do i = 1, num_phys_rj
        ist = istack_phys_comp_rj(i-1)
        ied = istack_phys_comp_rj(i  )
        do nd = ist+1, ied
          do inod = 1, nnod_rj_org
            inod_gl = inod_gl_rj_org(inod)
            d_rj(inod_gl,nd) = phys_data_IO(inod,nd)
          end do
        end do
      end do
!
      end subroutine copy_rj_merged_phys_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_merged_phys_to_IO(nnod_rj_new, inod_gl_rj_new)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: nnod_rj_new
      integer(kind = kint), intent(in) :: inod_gl_rj_new(nnod_rj_new)
      integer(kind = kint) :: nd, i, ist, ied, inod, inod_gl
!
!
      do i = 1, num_phys_rj
        ist = istack_phys_comp_rj(i-1)
        ied = istack_phys_comp_rj(i  )
        do nd = ist+1, ied
           do inod = 1, nnod_rj_new
            inod_gl = inod_gl_rj_new(inod)
            phys_data_IO(inod,nd) = d_rj(inod_gl,nd)
          end do
        end do
      end do
!
      end subroutine copy_rj_merged_phys_to_IO
!
! -------------------------------------------------------------------
!
      subroutine mul_sph_magne
!
      use m_control_param_newsph
      use m_spheric_parameter
      use m_phys_labels
      use m_sph_spectr_data
!
      integer(kind = kint) :: nd, i, ist, ied, inod
!
!
      do i = 1, num_phys_rj
        if(    phys_name_rj(i) .eq. fhd_magne                           &
     &    .or. phys_name_rj(i) .eq. fhd_mag_potential                   &
     &    .or. phys_name_rj(i) .eq. fhd_pre_uxb) then
          ist = istack_phys_comp_rj(i-1)
          ied = istack_phys_comp_rj(i  )
          do nd = ist+1, ied
            do inod = 1, nnod_rj
              d_rj(inod,nd) = b_sph_ratio * d_rj(inod,nd)
            end do
          end do
        end if
      end do
!
      end subroutine mul_sph_magne
!
! -------------------------------------------------------------------
!
      end module merge_sph_step_spectr
