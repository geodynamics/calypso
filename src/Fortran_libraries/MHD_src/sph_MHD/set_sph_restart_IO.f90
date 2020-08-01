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
!!
!!      logical function check_vector_4_restart(field_name)
!!      logical function check_scalar_4_restart(field_name)
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
         if(    check_vector_4_restart(rj_fld%phys_name(i_fld))         &
     &     .or. check_scalar_4_restart(rj_fld%phys_name(i_fld))         &
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
        if(check_vector_4_restart(rj_fld%phys_name(i_fld))) then
          icou = icou + 1
          fld_IO%fld_name(icou) = rj_fld%phys_name(i_fld)
          fld_IO%num_comp_IO(icou) = n_vector
          fld_IO%istack_comp_IO(icou) = fld_IO%istack_comp_IO(icou-1)   &
     &                                 + fld_IO%num_comp_IO(icou)
        else if(check_scalar_4_restart(rj_fld%phys_name(i_fld))         &
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
            if(check_vector_4_restart(rj_fld%phys_name(i_fld))) then
              call copy_each_sph_vector_to_IO                           &
     &           (rj_fld, fld_IO, i_fld, j_IO)
!
            else if(check_scalar_4_restart(rj_fld%phys_name(i_fld))     &
     &            ) then
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
      use calypso_mpi
      use m_base_field_labels
      use copy_rj_phys_data_4_IO
!
      type(field_IO), intent(in) :: fld_IO
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_IO
      integer(kind = kint) :: iflag
!
!
      do j_IO = 1, fld_IO%num_field_IO
        iflag = 0
        do i_fld = 1, rj_fld%num_phys
          if (rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_IO)) then
            iflag = 1
            if(check_vector_4_restart(rj_fld%phys_name(i_fld))) then
              call copy_each_sph_vector_from_IO                         &
     &           (fld_IO, rj_fld, i_fld, j_IO)
!
            else if(check_scalar_4_restart(rj_fld%phys_name(i_fld))     &
     &             ) then
              call copy_each_sph_field_from_IO                          &
     &           (fld_IO, rj_fld, i_fld, j_IO)
            end if
            exit
          end if
        end do
        if(iflag .eq. 0) then
          if(my_rank .eq. 0) write(*,*) 'Field ',                       &
     &        trim(fld_IO%fld_name(j_IO)),                              &
     &        ' is not defined in the control file,',                   &
     &        ' but exists in the restart file. Skip loading ',         &
     &        trim(fld_IO%fld_name(j_IO)), ' and continue.'
        end if
      end do
!
      end subroutine set_sph_restart_from_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      logical function check_vector_4_restart(field_name)
!
      use m_base_field_labels
      use m_explicit_term_labels
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_4_restart                                            &
     &   =    (field_name .eq. velocity%name)                           &
!     &   .or. (field_name .eq. vorticity%name)                         &
     &   .or. (field_name .eq. magnetic_field%name)                     &
!
     &   .or. (field_name .eq. previous_momentum%name)                  &
     &   .or. (field_name .eq. previous_induction%name)
!
      end function check_vector_4_restart
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_4_restart(field_name)
!
      use m_base_field_labels
      use m_explicit_term_labels
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_4_restart                                            &
     &   =    (field_name .eq. temperature%name)                        &
     &   .or. (field_name .eq. composition%name)                        &
     &   .or. (field_name .eq. entropy%name)                            &
!     &   .or. (field_name .eq. pressure%name)                          &
!     &   .or. (field_name .eq. magnetic_potential%name)                &
!
     &   .or. (field_name .eq. previous_heat%name)                      &
     &   .or. (field_name .eq. previous_composition%name)               &
!
     &   .or. (field_name .eq. heat_source%name)                        &
     &   .or. (field_name .eq. composition_source%name)                 &
     &   .or. (field_name .eq. entropy_source%name)
!
      end function check_scalar_4_restart
!
! ----------------------------------------------------------------------
!
      end module set_sph_restart_IO
