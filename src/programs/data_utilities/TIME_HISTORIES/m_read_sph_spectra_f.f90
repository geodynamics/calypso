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
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_file_rms = 31
      type(read_sph_spectr_data), save, private :: sph_IN_f
      type(sph_spectr_head_labels), save, private :: sph_lbl_IN_f
      type(buffer_4_gzip), save, private :: zbuf_f
      character, pointer, private, save :: FPz_fsp
      logical, private, save :: flag_gzip_s
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
      use select_gz_stream_file_IO
      use count_monitor_time_series
      use sel_gz_input_sph_mtr_head
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
      character(len=kchara) :: file_name
!
      file_name = c_to_fstring(input_prefix_c)
      call sel_open_read_gz_stream_file(FPz_fsp, id_file_rms,           &
     &    file_name, flag_gzip_s, zbuf_f)
      call read_sph_volume_mean_head(FPz_fsp, id_file_rms,              &
     &    flag_gzip_s, sph_lbl_IN_f, sph_IN_f, zbuf_f)
      call check_sph_spectr_name(sph_IN_f)
!
      sph_IN_f%nri_dat = 1
      call alloc_sph_spectr_data(izero, sph_IN_f)
!
      end subroutine open_init_volume_mean_file_f
!
! -------------------------------------------------------------------
!
      subroutine open_init_volume_spectr_file_f(input_prefix_c)         &
     &              bind(c, name="open_init_volume_spectr_file_f")
!
      use select_gz_stream_file_IO
      use count_monitor_time_series
      use sel_gz_input_sph_mtr_head
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
      character(len=kchara) :: file_name
!
      file_name = c_to_fstring(input_prefix_c)
      call sel_open_read_gz_stream_file(FPz_fsp, id_file_rms,           &
     &    file_name, flag_gzip_s, zbuf_f)
      call read_sph_volume_spectr_head(FPz_fsp, id_file_rms,            &
     &    flag_gzip_s, sph_lbl_IN_f, sph_IN_f, zbuf_f)
      call check_sph_spectr_name(sph_IN_f)
!
      sph_IN_f%nri_dat = 1
      call alloc_sph_spectr_data(sph_IN_f%ltr_sph, sph_IN_f)
!
      end subroutine open_init_volume_spectr_file_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine check_one_layer_mean_item_f(input_prefix_c)            &
     &              bind(c, name="check_one_layer_mean_item_f")
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
      character(len=kchara) :: file_name
!
      file_name = c_to_fstring(input_prefix_c)
      call sel_open_read_gz_stream_file(FPz_fsp, id_file_rms,           &
     &    file_name, flag_gzip_s, zbuf_f)
!
      call read_sph_layer_mean_head                                     &
     &   (FPz_fsp, id_file_rms, flag_gzip_s, flag_current_fmt,          &
     &    sph_lbl_IN_f, sph_IN_f, zbuf_f)
      call check_sph_spectr_name(sph_IN_f)
!
      sph_IN_f%nri_dat = sph_IN_f%nri_sph
      call alloc_sph_spectr_data(izero, sph_IN_f)
!
      end subroutine check_one_layer_mean_item_f
!
! -------------------------------------------------------------------
!
      subroutine check_layered_spectr_file_f(input_prefix_c)            &
     &              bind(c, name="check_layered_spectr_file_f")
!
      use select_gz_stream_file_IO
      use sel_gz_input_sph_mtr_head
      use count_monitor_time_series
!
      character(1,C_char), intent(in) :: input_prefix_c(*)
!
      character(len=kchara) :: file_name
!
      file_name = c_to_fstring(input_prefix_c)
      call sel_open_read_gz_stream_file(FPz_fsp, id_file_rms,           &
     &    file_name, flag_gzip_s, zbuf_f)
!
      call read_sph_layer_spectr_head                                   &
     &   (FPz_fsp, id_file_rms, flag_gzip_s, flag_current_fmt,          &
     &    sph_lbl_IN_f, sph_IN_f, zbuf_f)
      call check_sph_spectr_name(sph_IN_f)
!
      sph_IN_f%nri_dat = sph_IN_f%nri_sph
      call alloc_sph_spectr_data(sph_IN_f%ltr_sph, sph_IN_f)
!
      end subroutine check_layered_spectr_file_f
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine finalize_sph_series_file_f()                           &
     &          bind(c, name="finalize_sph_series_file_f")
!
      use select_gz_stream_file_IO
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_fsp, id_file_rms, flag_gzip_s, zbuf_f)
      call dealloc_sph_espec_data(sph_IN_f)
      call dealloc_sph_espec_name(sph_IN_f)
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
      character(len=kchara) :: draw_name
!
      draw_name = c_to_fstring(yname)
      find_monitor_field_address_f                                      &
     &          = find_monitor_field_address(draw_name, sph_IN_f)
!
      end function find_monitor_field_address_f
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &              find_monitor_field_address(draw_name, sph_IN)
!
      use count_monitor_time_series
!
      character(len=kchara), intent(in) :: draw_name
      type(read_sph_spectr_data), intent(in) :: sph_IN
!
      integer(kind = kint) :: i
!
!
      find_monitor_field_address = 0
      do i = 1, sph_IN%num_labels
        if(trim(draw_name) .eq. sph_IN%ene_sph_spec_name(i)) then
          find_monitor_field_address = i - sph_IN%num_time_labels
          exit
        end if
      end do
!
      if(find_monitor_field_address .le. 0) then
        write(*,*) 'Input field cannot be found.'
        return
      end if
!
      end function find_monitor_field_address
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_volume_mean_item_f               &
     &             (ncomp, id_pick, i_step, time, spectr)               &
     &              bind(c, name="load_one_volume_mean_item_f")
!
      use gz_spl_sph_spectr_data_IO
!
      integer(C_int), Value :: ncomp
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      real(kind = kreal), allocatable :: spectr_IN(:)
      integer(kind = kint) :: ierr_read, i
!
!
      allocate(spectr_IN(sph_IN_f%ntot_sph_spec))
!$omp parallel workshare
      spectr_IN(1:sph_IN_f%ntot_sph_spec) = 0.0d0
!$omp end parallel workshare
!
      load_one_volume_mean_item_f = 0
      call gz_read_volume_pwr_sph(FPz_fsp, id_file_rms, flag_gzip_s,    &
     &    sph_IN_f%ntot_sph_spec, sph_IN_f%i_step, sph_IN_f%time,       &
     &    spectr_IN, zbuf_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_volume_mean_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = spectr_IN(id_pick(i))
      end do
      deallocate(spectr_IN)
!
      end function load_one_volume_mean_item_f
!
! -------------------------------------------------------------------
!
      integer(C_int) function load_one_volume_spectr_item_f             &
     &             (i_mode, ncomp, id_pick, i_step, time, spectr)       &
     &              bind(c, name="load_one_volume_spectr_item_f")
!
      use gz_volume_spectr_monitor_IO
!
      integer(C_int), Value :: ncomp, i_mode
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      real(kind = kreal), allocatable :: spectr_IN(:,:)
      integer(kind = kint) :: ierr_read, i
!
!
      allocate(spectr_IN(sph_IN_f%ntot_sph_spec,0:sph_IN_f%ltr_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN_f%ntot_sph_spec,0:sph_IN_f%ltr_sph) = 0.0d0
!$omp end parallel workshare
!
      load_one_volume_spectr_item_f = 0
      call sel_gz_read_volume_spectr_mtr(FPz_fsp, id_file_rms,          &
     &    flag_gzip_s, sph_IN_f%ltr_sph, sph_IN_f%ntot_sph_spec,        &
     &    sph_IN_f%i_step, sph_IN_f%time, sph_IN_f%i_mode,              &
     &    spectr_IN(1,0), zbuf_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_volume_spectr_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = spectr_IN(id_pick(i),i_mode)
      end do
      deallocate(spectr_IN)
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
      use gz_spl_sph_spectr_data_IO
!
      integer(C_int), Value :: ncomp, id_radius
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      real(kind = kreal), allocatable :: spectr_IN(:,:)
      integer(kind = kint) :: ierr_read, i
!
!
      allocate(spectr_IN(sph_IN_f%ntot_sph_spec,sph_IN_f%nri_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN_f%ntot_sph_spec,1:sph_IN_f%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      load_one_layer_mean_item_f = 0
      call sel_gz_input_sph_layer_mean                                  &
     &   (FPz_fsp, id_file_rms, flag_gzip_s, flag_current_fmt,          &
     &    sph_IN_f%nri_sph, sph_IN_f%ntot_sph_spec, sph_IN_f%i_step,    &
     &    sph_IN_f%time, sph_IN_f%kr_sph, sph_IN_f%r_sph,               &
     &    spectr_IN(1,1), zbuf_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_layer_mean_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = spectr_IN(id_pick(i),id_radius)
      end do
      deallocate(spectr_IN)
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
      use gz_spl_sph_spectr_data_IO
!
      integer(C_int), Value :: ncomp, id_radius, i_mode
      integer(C_int), intent(in) :: id_pick(ncomp)
      integer(C_int), intent(inout) :: i_step(1)
      real(c_double), intent(inout) :: time(1)
      real(c_double), intent(inout) :: spectr(ncomp)
!
      real(kind = kreal), allocatable :: spectr_IN(:,:,:)
      integer(kind = kint) :: ierr_read, i
!
!
      allocate(spectr_IN(sph_IN_f%ntot_sph_spec,                        &
     &                   0:sph_IN_f%ltr_sph,sph_IN_f%nri_sph))
!$omp parallel workshare
      spectr_IN(1:sph_IN_f%ntot_sph_spec,                               &
     &          0:sph_IN_f%ltr_sph,1:sph_IN_f%nri_sph) = 0.0d0
!$omp end parallel workshare
!
      load_one_layer_spectr_item_f = 0
      call sel_gz_input_sph_layer_spec                                  &
     &   (FPz_fsp, id_file_rms, flag_gzip_s, flag_current_fmt,          &
     &    sph_IN_f%nri_sph, sph_IN_f%ltr_sph, sph_IN_f%ntot_sph_spec,   &
     &    sph_IN_f%i_step, sph_IN_f%time, sph_IN_f%kr_sph,              &
     &    sph_IN_f%r_sph, sph_IN_f%i_mode, spectr_IN(1,0,1),            &
     &    zbuf_f, ierr_read)
      if(ierr_read .gt. 0) then
        load_one_layer_spectr_item_f = ierr_read
        return
      end if
!
      i_step(1) = sph_IN_f%i_step
      time(1) =   sph_IN_f%time
      do i = 1, ncomp
        spectr(i) = spectr_IN(id_pick(i),i_mode,id_radius)
      end do
      deallocate(spectr_IN)
!
      end function load_one_layer_spectr_item_f
!
! -------------------------------------------------------------------
!
      end module m_read_sph_spectra_f
