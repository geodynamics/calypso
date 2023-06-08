!>@file   dup_fields_on_circle_to_IO.f90
!!@brief  module dup_fields_on_circle_to_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine dup_field_on_circ_header_to_IO                       &
!!     &         (sph_params, leg_circ, circle, d_circle, sph_OUT)
!!      subroutine dup_spectr_on_circ_header_to_IO                      &
!!     &         (sph_params, leg_circ, circle, d_circle, sph_OUT)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(circle_transform_spectr) :: leg_circ
!!        type(circle_parameters), intent(in) :: circle
!!        type(phys_data), intent(in) :: d_circle
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module dup_fields_on_circle_to_IO
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_phys_address
      use t_base_field_labels
      use t_read_sph_spectra
      use t_time_data
      use t_phys_data
      use t_field_on_circle
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dup_field_on_circ_header_to_IO                         &
     &         (sph_params, leg_circ, circle, d_circle, sph_OUT)
!
      use m_time_labels
      use sel_comp_labels_by_coord
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(circle_transform_spectr) :: leg_circ
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: ifld, ist
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = leg_circ%ltr_circle
      sph_OUT%nri_sph = circle%mphi_circle
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = izero
      sph_OUT%r_inner =  circle%s_circle
      sph_OUT%r_outer =  circle%z_circle
!
      sph_OUT%nfield_sph_spec = d_circle%num_phys_viz
      sph_OUT%ntot_sph_spec =   d_circle%ntot_phys_viz
!
      sph_OUT%num_time_labels = 4
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec)                 &
     &           = d_circle%num_component(1:sph_OUT%nfield_sph_spec)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Longitude_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Longitude'
      do ifld = 1, d_circle%num_phys_viz
        ist = d_circle%istack_component(ifld-1)                         &
     &       + 1 + sph_OUT%num_time_labels
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call sel_coord_tensor_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call sel_coord_vector_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else
          write(sph_OUT%ene_sph_spec_name(ist),'(a)')                   &
     &                   trim(d_circle%phys_name(ifld))
        end if
      end do
!
      end subroutine dup_field_on_circ_header_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine dup_spectr_on_circ_header_to_IO                        &
     &         (sph_params, leg_circ, circle, d_circle, sph_OUT)
!
      use m_time_labels
      use sel_comp_labels_by_coord
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(circle_transform_spectr) :: leg_circ
      type(circle_parameters), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: ifld, ist
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = leg_circ%ltr_circle
      sph_OUT%nri_sph = circle%mphi_circle
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = izero
      sph_OUT%r_inner =  circle%s_circle
      sph_OUT%r_outer =  circle%z_circle
!
      sph_OUT%nfield_sph_spec = d_circle%num_phys_viz
      sph_OUT%ntot_sph_spec =   d_circle%ntot_phys_viz
!
      sph_OUT%num_time_labels = 3
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec)                 &
     &           = d_circle%num_component(1:sph_OUT%nfield_sph_spec)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'wave_number'
      do ifld = 1, d_circle%num_phys_viz
        ist = d_circle%istack_component(ifld-1)                         &
     &       + 1 + sph_OUT%num_time_labels
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call sel_coord_tensor_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call sel_coord_vector_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else
          write(sph_OUT%ene_sph_spec_name(ist),'(a)')                   &
     &                   trim(d_circle%phys_name(ifld))
        end if
      end do
!
      end subroutine dup_spectr_on_circ_header_to_IO
!
! ----------------------------------------------------------------------
!
      end module dup_fields_on_circle_to_IO
