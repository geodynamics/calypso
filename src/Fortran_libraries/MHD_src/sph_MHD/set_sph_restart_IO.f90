!> @file  set_sph_restart_IO.f90
!!      module set_sph_restart_IO
!!
!! @author  H. Matsui
!! @date Programmed in 2010
!
!> @brief Copy between field data and IO buffer for restarting
!!
!!@verbatim
!!      subroutine set_sph_restart_num_to_IO
!!      subroutine set_sph_restart_data_to_IO
!!
!!      subroutine set_sph_restart_from_IO
!!@endverbatim
!
      module set_sph_restart_IO
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_field_data_IO
!
      implicit none
!
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
      subroutine set_sph_restart_num_to_IO
!
      use m_spheric_parameter
!
!
      numgrid_phys_IO =   nnod_rj
!
      call set_sph_restart_field_to_IO
      call allocate_phys_data_name_IO
!
      call set_sph_restart_comp_to_IO
      call allocate_phys_data_IO
!
      end subroutine set_sph_restart_num_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_field_to_IO
!
      use m_sph_spectr_data
      use m_phys_labels
!
      integer (kind=kint) :: i_fld
!
!
      num_phys_data_IO = 0
       do i_fld = 1, num_phys_rj
         if   ( phys_name_rj(i_fld) .eq. fhd_velo                       &
     &     .or. phys_name_rj(i_fld) .eq. fhd_vort                       &
     &     .or. phys_name_rj(i_fld) .eq. fhd_press                      &
     &     .or. phys_name_rj(i_fld) .eq. fhd_temp                       &
     &     .or. phys_name_rj(i_fld) .eq. fhd_light                      &
     &     .or. phys_name_rj(i_fld) .eq. fhd_magne                      &
     &     .or. phys_name_rj(i_fld) .eq. fhd_mag_potential              &
     &     .or. phys_name_rj(i_fld) .eq. fhd_entropy                    &
     &     .or. phys_name_rj(i_fld) .eq. fhd_pre_mom                    &
     &     .or. phys_name_rj(i_fld) .eq. fhd_pre_uxb                    &
     &     .or. phys_name_rj(i_fld) .eq. fhd_pre_heat                   &
     &     .or. phys_name_rj(i_fld) .eq. fhd_pre_composit               &
     &     .or. phys_name_rj(i_fld) .eq. fhd_heat_source                &
     &     .or. phys_name_rj(i_fld) .eq. fhd_light_source               &
     &     .or. phys_name_rj(i_fld) .eq. fhd_entropy_source             &
     &     ) then
           num_phys_data_IO = num_phys_data_IO + 1
         end if
       end do
!
      end subroutine set_sph_restart_field_to_IO
!
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_comp_to_IO
!
      use m_sph_spectr_data
      use m_phys_labels
!
      integer (kind=kint) :: i_fld, icou
!
!
      icou = 0
      istack_phys_comp_IO(0) = 0
      do i_fld = 1, num_phys_rj
        if         (phys_name_rj(i_fld) .eq. fhd_velo                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_vort                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_magne                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_mom                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_uxb                &
     &         ) then
          icou = icou + 1
          phys_data_name_IO(icou) = phys_name_rj(i_fld)
          num_phys_comp_IO(icou) = n_vector
          istack_phys_comp_IO(icou) = istack_phys_comp_IO(icou-1)       &
     &                               + num_phys_comp_IO(icou)
        else if    (phys_name_rj(i_fld) .eq. fhd_press                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_temp                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_mag_potential          &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_heat               &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_composit           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_heat_source            &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light_source           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy_source         &
     &         ) then
          icou = icou + 1
          phys_data_name_IO(icou) = phys_name_rj(i_fld)
          num_phys_comp_IO(icou) =  n_scalar
          istack_phys_comp_IO(icou) = istack_phys_comp_IO(icou-1)       &
     &                               + num_phys_comp_IO(icou)
        end if
      end do
      ntot_phys_data_IO = istack_phys_comp_IO(num_phys_data_IO)
!
      end subroutine set_sph_restart_comp_to_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_sph_restart_data_to_IO
!
      use m_t_step_parameter
      use m_t_int_parameter
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use copy_time_steps_4_restart
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint) :: i_fld, j_IO
!
!
      call copy_time_steps_to_restart
!
      do i_fld = 1, num_phys_rj
        do j_IO = 1, num_phys_data_IO
          if (phys_name_rj(i_fld) .eq. phys_data_name_IO(j_IO)) then
            if     (phys_name_rj(i_fld) .eq. fhd_velo                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_vort                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_magne                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_mom                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_uxb                &
     &         ) then
              call copy_each_sph_vector_to_IO(i_fld, j_IO)
!
            else if(phys_name_rj(i_fld) .eq. fhd_press                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_temp                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_mag_potential          &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_heat               &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_composit           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_heat_source            &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light_source           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy_source         &
     &         ) then
              call copy_each_sph_field_to_IO(i_fld, j_IO)
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
      subroutine set_sph_restart_from_IO
!
      use m_time_data_IO
      use m_t_step_parameter
      use m_t_int_parameter
      use m_phys_labels
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use copy_rj_phys_data_4_IO
!
      integer(kind = kint) :: i_fld, j_IO
!
!
      i_step_init = i_time_step_IO
      time_init =   time_IO
!
      if(dt .le.zero) dt = delta_t_IO
!
      do i_fld = 1, num_phys_rj
        do j_IO = 1, num_phys_data_IO
          if (phys_name_rj(i_fld) .eq. phys_data_name_IO(j_IO)) then
            if     (phys_name_rj(i_fld) .eq. fhd_velo                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_vort                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_magne                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_mom                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_uxb                &
     &         ) then
              call copy_each_sph_solenoid_from_IO(i_fld, j_IO)
!
            else if     (phys_name_rj(i_fld) .eq. fhd_press             &
     &         .or. phys_name_rj(i_fld) .eq. fhd_temp                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_mag_potential          &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_heat               &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_composit           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_heat_source            &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light_source           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy_source         &
     &         ) then
              call copy_each_sph_field_from_IO(i_fld, j_IO)
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
