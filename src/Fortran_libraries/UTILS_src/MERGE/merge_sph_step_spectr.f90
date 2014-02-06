!>@file   merge_sph_step_spectr.f90
!!@brief  module merge_sph_step_spectr
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine set_sph_rj_mesh_4_merge
!!      subroutine s_assenble_sph_step_spectr(istep)
!!@endverbatim
!!
!!@param   istep  TIme step number
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
      use r_interpolate_marged_sph
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
      call sph_radial_interpolation_coef                                &
     &     (org_sph_mesh(1)%sph_mesh%sph_rj%nidx_rj(1),                 &
     &      org_sph_mesh(1)%sph_mesh%sph_rj%radius_1d_rj_r)
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
      use m_machine_parameter
      use m_control_param_newsph
      use m_merge_spheric_mesh
      use field_IO_select
      use copy_rj_phys_data_4_IO
      use r_interpolate_marged_sph
!
      integer(kind = kint), intent(in) :: istep
!
      integer(kind = kint) :: ip, my_rank
!
!
      call clear_merged_spectr
!
      phys_file_head = org_sph_fst_head
!
      do ip = 1, np_sph_org
        my_rank = ip - 1
        call sel_read_alloc_step_SPH_file(my_rank, istep)
        call deallocate_phys_data_name_IO
!
        if(iflag_same_rgrid .eq. 0) then
          call itp_rj_merged_phys_from_IO                               &
     &       (org_sph_mesh(ip)%sph_mesh%sph_rj%nnod_rj,                 &
     &        org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),              &
     &        org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j,          &
     &        phys_data_IO)
          call extend_potential_magne
        else
          call copy_rj_merged_phys_from_IO                              &
     &      (org_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),               &
     &       org_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j)
        end if
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
     &      (new_sph_mesh(ip)%sph_mesh%sph_rj%nidx_rj(2),               &
     &       new_sph_mesh(ip)%sph_mesh%sph_rj%idx_gl_1d_rj_j)
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
      subroutine clear_merged_spectr
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint) :: nd, inod
!
!
!$omp parallel
      do nd = 1, ntot_phys_rj
!$omp do
        do inod = 1, nnod_rj
          d_rj(inod,nd) = 0.0d0
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine clear_merged_spectr
!
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
        a_r_1d_rj_r(k) = 1.0d0 / radius_1d_rj_r(k)
      end do
!
      idx_gl_1d_rj_j(0,1:3) = 0
      do j = 2, nidx_rj(2)
        idx_gl_1d_rj_j(j,1) = j - 1
        idx_gl_1d_rj_j(j,2) = aint(sqrt(real(idx_gl_1d_rj_j(j,1))))
        idx_gl_1d_rj_j(j,3) =idx_gl_1d_rj_j(j,1)                        &
     &                  - idx_gl_1d_rj_j(j,2)*(idx_gl_1d_rj_j(j,2)+1)
      end do
!
      end subroutine set_global_sph_grid_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_merged_phys_from_IO(jmax_org,                  &
     &          idx_gl_1d_j_org)
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax_org
      integer(kind = kint), intent(in) :: idx_gl_1d_j_org(jmax_org,3)
!
      integer(kind = kint) :: nd, inod, inod_gl, kr, j, j_gl
!
!
!$omp parallel
      do nd = 1, ntot_phys_rj
!$omp do private(j,j_gl,kr,inod,inod_gl)
        do j = 1, jmax_org
          j_gl = idx_gl_1d_j_org(j,1)
          if(j_gl .ge. nidx_rj(2)) cycle
!
          do kr = 1, nidx_rj(1)
            inod = j + (kr - 1) * jmax_org
            inod_gl = 1 + j_gl + (kr - 1) * nidx_rj(2)
            d_rj(inod_gl,nd) = phys_data_IO(inod,nd)
          end do
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine copy_rj_merged_phys_from_IO
!
! -------------------------------------------------------------------
!
      subroutine copy_rj_merged_phys_to_IO(jmax_new, idx_gl_1d_j_new)
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: jmax_new
      integer(kind = kint), intent(in) :: idx_gl_1d_j_new(jmax_new,3)
!
      integer(kind = kint) :: nd, inod, inod_gl, kr, j, j_gl
!
!
!$omp parallel
      do nd = 1, ntot_phys_rj
!$omp do private(j,j_gl,kr,inod,inod_gl)
        do j = 1, jmax_new
          j_gl = idx_gl_1d_j_new(j,1)
          do kr = 1, nidx_rj(1)
            inod = j + (kr - 1) * jmax_new
            inod_gl = 1 + j_gl + (kr - 1) * nidx_rj(2)
            phys_data_IO(inod,nd) = d_rj(inod_gl,nd)
          end do
        end do
!$omp end do
      end do
!$omp end parallel
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
