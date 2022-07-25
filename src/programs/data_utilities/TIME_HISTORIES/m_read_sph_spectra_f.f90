!>@file   m_read_sph_spectra_f.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Spherical harmonics spectrum data loading
!!
!!@verbatim
!!      subroutine open_init_volume_mean_file_f(input_prefix_c)         &
!!     &              bind(c, name="open_init_one_layer_mean_item_f")
!!      subroutine open_init_volume_spectr_file_f(input_prefix_c)       &
!!     &              bind(c, name="open_init_one_layer_mean_item_f")
!!      subroutine open_init_one_layer_mean_item_f(input_prefix_c)      &
!!     &              bind(c, name="open_init_one_layer_mean_item_f")
!!      subroutine open_init_layered_spectr_file_f(input_prefix_c)      &
!!     &              bind(c, name="open_init_layered_spectr_file_f")
!!        character(1,C_char), intent(in) :: input_prefix_c(*)
!!
!!      subroutine finalize_sph_series_file_f()                         &
!!     &          bind(c, name="finalize_sph_series_file_f")
!!
!!      integer(C_int) function load_one_volume_mean_item_f             &
!!     &             (id_pick, i_step, time, spectr)                    &
!!     &              bind(c, name="load_one_volume_mean_item_f")
!!      integer(C_int) function load_one_volume_spectr_item_f           &
!!     &             (id_pick, i_mode, i_step, time, spectr)            &
!!     &              bind(c, name="load_one_volume_spectr_item_f")
!!      integer(C_int) function load_one_layer_mean_item_f              &
!!     &             (id_pick, id_radius, i_step, time, spectr)         &
!!     &              bind(c, name="load_one_layer_mean_item_f")
!!      integer(C_int) function load_one_layer_spectr_item_f            &
!!     &             (id_pick, id_radius, i_mode, i_step, time, spectr) &
!!     &              bind(c, name="load_one_layer_spectr_item_f")
!!        integer(C_int), Value :: id_pick, id_radius, i_mode
!!        integer(C_int), intent(inout) :: i_step(1)
!!        real(c_double), intent(inout) :: spectr(1)
!!        real(c_double), intent(inout) :: time(1)
!!@endverbatim
      module m_read_sph_spectra_f
!
      use ISO_C_BINDING
!
      use m_precision
      use m_constants
      use t_read_sph_spectra
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_file_rms = 31
      type(read_sph_spectr_data), save, private :: sph_IN_f
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine open_init_volume_mean_file_f(input_prefix_c)           &
     &              bind(c, name="open_init_volume_mean_file_f")
!
      use sph_mean_square_IO_select
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
!
      open(id_file_rms, file = c_to_fstring(input_prefix_c))
      call select_input_sph_series_head(id_file_rms, flag_current_fmt,  &
     &    spectr_off, volume_on, sph_IN_f)
      call check_sph_spectr_name(sph_IN_f)
!
      end subroutine open_init_volume_mean_file_f
!
! -------------------------------------------------------------------
!
      subroutine open_init_volume_spectr_file_f(input_prefix_c)         &
     &              bind(c, name="open_init_volume_spectr_file_f")
!
      use sph_mean_square_IO_select
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
!
      open(id_file_rms, file = c_to_fstring(input_prefix_c))
      call select_input_sph_series_head(id_file_rms, flag_current_fmt,  &
     &    spectr_on, volume_on, sph_IN_f)
      call check_sph_spectr_name(sph_IN_f)
!
      end subroutine open_init_volume_spectr_file_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine check_one_layer_mean_item_f(input_prefix_c)            &
     &              bind(c, name="check_one_layer_mean_item_f")
!
      use sph_mean_square_IO_select
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
!
      open(id_file_rms, file = c_to_fstring(input_prefix_c))
      call select_input_sph_series_head(id_file_rms, flag_current_fmt,  &
     &    spectr_off, volume_off, sph_IN_f)
      call check_sph_spectr_name(sph_IN_f)
!
      end subroutine check_one_layer_mean_item_f
!
! -------------------------------------------------------------------
!
      subroutine check_layered_spectr_file_f(input_prefix_c)            &
     &              bind(c, name="check_layered_spectr_file_f")
!
      use sph_mean_square_IO_select
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
!
      open(id_file_rms, file = c_to_fstring(input_prefix_c))
      call select_input_sph_series_head(id_file_rms, flag_current_fmt,  &
     &    spectr_on, volume_off, sph_IN_f)
      call check_sph_spectr_name(sph_IN_f)
!
      end subroutine check_layered_spectr_file_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine finalize_sph_series_file_f()                           &
     &          bind(c, name="finalize_sph_series_file_f")
!
!
      close(id_file_rms)
      call dealloc_sph_espec_data(sph_IN_f)
!
      end subroutine finalize_sph_series_file_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function find_monitor_field_address_f(yname)       &
     &          bind(c, name="find_monitor_field_address_f")
!
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: yname(*)
!
      integer(kind = kint) :: i
      character(len=kchara) :: draw_name
!
!
      draw_name = c_to_fstring(yname)
      find_monitor_field_address_f = 0
      do i = 1, sph_IN_f%num_labels
        if(trim(draw_name) .eq. sph_IN_f%ene_sph_spec_name(i)) then
          find_monitor_field_address_f = i - sph_IN_f%num_time_labels
          exit
        end if
      end do
!
      if(find_monitor_field_address_f .le. 0) then
        write(*,*) 'Input field cannot be found.'
        return
      end if
!
      end function find_monitor_field_address_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_volume_mean_item_f               &
     &             (ncomp, id_pick, i_step, time, spectr)               &
     &              bind(c, name="load_one_volume_mean_item_f")
!
      use sph_mean_square_IO_select
!
      integer(C_int), Value :: ncomp
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      integer(kind = kint) :: ierr_read, i
!
!
      load_one_volume_mean_item_f = 0
      call select_input_sph_series_data(id_file_rms, flag_current_fmt,  &
     &    spectr_off, volume_on, sph_IN_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_volume_mean_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = sph_IN_f%spectr_IO(id_pick(i),0,1)
      end do
!
      end function load_one_volume_mean_item_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_volume_spectr_item_f             &
     &             (i_mode, ncomp, id_pick, i_step, time, spectr)       &
     &              bind(c, name="load_one_volume_spectr_item_f")
!
      use sph_mean_square_IO_select
!
      integer(C_int), Value :: ncomp, i_mode
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      integer(kind = kint) :: ierr_read, i
!
!
      load_one_volume_spectr_item_f = 0
      call select_input_sph_series_data(id_file_rms, flag_current_fmt,  &
     &    spectr_on, volume_on, sph_IN_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_volume_spectr_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = sph_IN_f%spectr_IO(id_pick(i),i_mode,1)
      end do
!
      end function load_one_volume_spectr_item_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_layer_mean_item_f                &
     &             (id_radius, ncomp, id_pick, i_step, time, spectr)    &
     &              bind(c, name="load_one_layer_mean_item_f")
!
      use sph_mean_square_IO_select
!
      integer(C_int), Value :: ncomp, id_radius
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      integer(kind = kint) :: ierr_read, i
!
!
      load_one_layer_mean_item_f = 0
      call select_input_sph_series_data(id_file_rms, flag_current_fmt,  &
     &    spectr_off, volume_off, sph_IN_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_layer_mean_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = sph_IN_f%spectr_IO(id_pick(i),0,id_radius)
      end do
!
      end function load_one_layer_mean_item_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_layer_spectr_item_f              &
     &             (id_radius, i_mode, ncomp, id_pick,                  &
     &              i_step, time, spectr)                               &
     &              bind(c, name="load_one_layer_spectr_item_f")
!
      use sph_mean_square_IO_select
!
      integer(C_int), Value :: ncomp, id_radius, i_mode
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      integer(kind = kint) :: ierr_read, i
!
!
      load_one_layer_spectr_item_f = 0
      call select_input_sph_series_data(id_file_rms, flag_current_fmt,  &
     &    spectr_on, volume_off, sph_IN_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_layer_spectr_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = sph_IN_f%spectr_IO(id_pick(i),i_mode,id_radius)
      end do
!
      end function load_one_layer_spectr_item_f
!
! -------------------------------------------------------------------
!
      end module m_read_sph_spectra_f
