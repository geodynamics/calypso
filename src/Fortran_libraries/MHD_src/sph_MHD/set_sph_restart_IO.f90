!> @file  set_sph_restart_IO.f90
!!      module set_sph_restart_IO
!!
!! @author  H. Matsui
!! @date Programmed in 2010
!
!> @brief Copy between field data and IO buffer for restarting
!!
!!@verbatim
!!      subroutine set_sph_restart_num_to_IO(rj_fld, fld_IO)
!!      subroutine set_sph_restart_data_to_IO(rj_fld, fld_IO)
!!        type(phys_data), intent(in) :: rj_fld
!!        type(field_IO), intent(inout) :: fld_IO
!!
!!      subroutine set_sph_restart_from_IO(fld_IO)
!!        type(field_IO), intent(in) :: fld_IO
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_sph_restart_IO
!
      use m_precision
!
      use m_constants
      use m_phys_constants
!
      use t_phys_data
      use t_field_data_IO
!
      implicit none
!
      private :: set_sph_restart_field_to_IO
      private :: set_sph_restart_comp_to_IO
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_num_to_IO(rj_fld, fld_IO)
!
      use calypso_mpi
      use const_global_element_ids
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%nnod_IO =   rj_fld%n_point
!
      call set_sph_restart_field_to_IO(rj_fld, fld_IO)
      call alloc_phys_name_IO(fld_IO)
!
      call set_sph_restart_comp_to_IO(rj_fld, fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call alloc_merged_field_stack(nprocs, fld_IO)
      call count_number_of_node_stack                                   &
     &   (fld_IO%nnod_IO, fld_IO%istack_numnod_IO)
!
      end subroutine set_sph_restart_num_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_field_to_IO(rj_fld, fld_IO)
!
      use m_phys_labels
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer (kind=kint) :: i_fld
!
!
      fld_IO%num_field_IO = 0
       do i_fld = 1, rj_fld%num_phys
         if   ( rj_fld%phys_name(i_fld) .eq. fhd_velo                   &
!     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_vort                  &
!     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_press                 &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_temp                   &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_light                  &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_magne                  &
!     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential         &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy                &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom                &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb                &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat               &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit           &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source            &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source           &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source         &
!
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_h_flux        &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_c_flux        &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_m_flux        &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_Lorentz       &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_induction     &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_buoyancy      &
     &     .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_comp_buo      &
     &     ) then
           fld_IO%num_field_IO = fld_IO%num_field_IO + 1
         end if
       end do
!
      end subroutine set_sph_restart_field_to_IO
!
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_comp_to_IO(rj_fld, fld_IO)
!
      use m_phys_labels
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer (kind=kint) :: i_fld, icou
!
!
      icou = 0
      fld_IO%istack_comp_IO(0) = 0
      do i_fld = 1, rj_fld%num_phys
        if         (rj_fld%phys_name(i_fld) .eq. fhd_velo               &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_vort              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_magne              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb            &
     &         ) then
          icou = icou + 1
          fld_IO%fld_name(icou) = rj_fld%phys_name(i_fld)
          fld_IO%num_comp_IO(icou) = n_vector
          fld_IO%istack_comp_IO(icou) = fld_IO%istack_comp_IO(icou-1)   &
     &                                 + fld_IO%num_comp_IO(icou)
        else if    (rj_fld%phys_name(i_fld) .eq. fhd_temp               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light              &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_press             &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy            &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential     &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat           &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source        &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source     &
!
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_h_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_c_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_m_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_Lorentz   &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_induction &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_buoyancy  &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_comp_buo  &
     &         ) then
          icou = icou + 1
          fld_IO%fld_name(icou) = rj_fld%phys_name(i_fld)
          fld_IO%num_comp_IO(icou) =  n_scalar
          fld_IO%istack_comp_IO(icou) = fld_IO%istack_comp_IO(icou-1)   &
     &                                 + fld_IO%num_comp_IO(icou)
        end if
      end do
      fld_IO%ntot_comp_IO = fld_IO%istack_comp_IO(fld_IO%num_field_IO)
!
      end subroutine set_sph_restart_comp_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_data_to_IO(rj_fld, fld_IO)
!
      use m_phys_labels
      use copy_rj_phys_data_4_IO
!
      type(phys_data), intent(in) :: rj_fld
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint) :: i_fld, j_IO
!
!
      do i_fld = 1, rj_fld%num_phys
        do j_IO = 1, fld_IO%num_field_IO
          if (rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            if     (rj_fld%phys_name(i_fld) .eq. fhd_velo               &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_vort              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_magne              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb            &
     &         ) then
              call copy_each_sph_vector_to_IO                           &
     &           (rj_fld, fld_IO, i_fld, j_IO)
!
            else if(rj_fld%phys_name(i_fld) .eq. fhd_temp               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light              &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_press             &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy            &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential     &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat           &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source        &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source     &
!
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_h_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_c_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_m_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_Lorentz   &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_induction &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_buoyancy  &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_comp_buo  &
     &         ) then
              call copy_each_sph_field_to_IO                            &
     &           (rj_fld, fld_IO, i_fld, j_IO)
            end if
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_restart_data_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_from_IO(fld_IO, rj_fld)
!
      use m_phys_labels
      use copy_rj_phys_data_4_IO
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_IO
!
!
      do i_fld = 1, rj_fld%num_phys
        do j_IO = 1, fld_IO%num_field_IO
          if (rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            if     (rj_fld%phys_name(i_fld) .eq. fhd_velo               &
 !    &         .or. rj_fld%phys_name(i_fld) .eq. fhd_vort              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_magne              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb            &
     &         ) then
              call copy_each_sph_vector_from_IO                         &
     &           (fld_IO, rj_fld, i_fld, j_IO)
!
            else if(rj_fld%phys_name(i_fld) .eq. fhd_temp               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light              &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_press             &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy            &
!     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential     &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat           &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source        &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source     &
!
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_h_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_c_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_m_flux    &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_Lorentz   &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_induction &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_buoyancy  &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_Csim_SGS_comp_buo  &
     &         ) then
              call copy_each_sph_field_from_IO                          &
     &           (fld_IO, rj_fld, i_fld, j_IO)
            end if
            exit
          end if
        end do
      end do
!
      end subroutine set_sph_restart_from_IO
!
! -------------------------------------------------------------------
!
      end module set_sph_restart_IO
