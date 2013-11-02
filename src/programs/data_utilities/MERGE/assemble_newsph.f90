!assemble_newsph.f90
! ----- program  newspectr
!
      program newspectr
!
!    change restart data for different number of domains
!     By H. Matsui
!
!
      use m_precision
!
      use m_spheric_parameter
!
      use m_control_data_4_merge
      use m_control_param_newsph
      use m_merge_spheric_mesh
      use merge_sph_step_spectr
      use set_field_file_names
!
      implicit    none
!
      integer (kind = kint) :: istep
!
! ==============================================
! * get number of  nodes,elements for whole PES
! ==============================================
!
!
      call read_control_assemble_sph
      call set_control_4_newsph
!
      call alloc_sph_mesh_4_merge
!
!  set spectr data
!
      call set_sph_rj_mesh_4_merge
!
!   loop for time integration
!
      do istep = istep_start, istep_end, increment_step
        call s_assenble_sph_step_spectr(istep)
        write(*,*) 'step', istep, 'finish '
      end do
!
      if(iflag_delete_org_sph .gt. 0) then
        phys_file_head = org_sph_fst_head
        do istep = istep_start, istep_end, increment_step
          call delete_SPH_fld_file(iflag_field_data_fmt,                &
     &        np_sph_org, istep)
        end do
      end if
!
      call deallocate_sph_1d_index_rj
      call deallocate_spheric_param_rj
!
!
      stop ' //// program normally terminated //// '
!
      end program newspectr
