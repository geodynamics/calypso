!>@file   write_snap_pick_sph_spectr.f90
!!@brief  module write_snap_pick_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_tave_sph_spec_monitor                          &
!!     &         (file_name, i_step, end_time, true_start, picked_IO)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!        integer(kind = kint), intent(in) :: id_pick
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_spectrum_data_IO), intent(in) :: picked_IO
!!      subroutine write_picked_sph_snap                                &
!!     &         (id_pick, i_step, time, picked_IO)
!!        integer(kind = kint), intent(in) :: id_pick
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_spectrum_data_IO), intent(in) :: picked_IO
!!@endverbatim
!!
!!@n @param  id_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module write_snap_pick_sph_spectr
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
      use t_picked_sph_spectr_data_IO
      use t_buffer_4_gzip
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_tave_sph_spec_monitor                            &
     &         (file_name, i_step, end_time, true_start, picked_IO)
!
      use m_monitor_file_labels
      use write_field_labels
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: end_time, true_start
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
!
      if(picked_IO%num_mode .eq. izero) return
!
      open(id_pick_mode, file = file_name, form='formatted',            &
     &     position='append')
!
      write(*,'(a,3i16)') hd_pick_sph_head(), picked_IO%num_layer,      &
     &        picked_IO%num_mode
      write(*,'(a,i16)') hd_pick_sph_num(), picked_IO%ntot_comp
!
      write(*,'(a)')  '#   Start and end time'
      write(*,'(1p2e25.12)')  true_Start, end_time
!
      write(id_pick_mode,'(a)')    '#'
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(2i16)') picked_IO%num_layer,                 &
     &                            picked_IO%num_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') picked_IO%ntot_comp
!
      write(id_pick_mode,'(a)',advance='NO')  't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_pick_mode, picked_IO%ntot_comp,        &
     &    picked_IO%spectr_name)
      call write_picked_sph_snap                                        &
     &   (id_pick_mode, i_step, end_time, picked_IO)
      close(id_pick_mode)
!
      end subroutine write_tave_sph_spec_monitor
!
! -----------------------------------------------------------------------
!
      subroutine write_picked_sph_snap                                  &
     &         (id_pick, i_step, time, picked_IO)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
      integer(kind = kint) :: ipick, i_fld, ist
!
!
      do ipick = 1, picked_IO%num_layer * picked_IO%num_mode
          ist = (ipick-1) * picked_IO%ntot_comp
          write(id_pick,'(i16,1pe25.14e3)', advance='NO')               &
     &               i_step, time
          write(id_pick,'(i16,1pe25.14e3,2i16)', advance='NO')          &
     &            picked_IO%idx_sph(ipick,1), picked_IO%radius(ipick),  &
     &            picked_IO%idx_sph(ipick,3:4)
          do i_fld = 1, picked_IO%ntot_comp
            write(id_pick,'(1pe25.14e3)', advance='NO')                 &
     &            picked_IO%d_pk(ist+i_fld)
          end do
          write(id_pick,'(a)') ''
      end do
!
      end subroutine write_picked_sph_snap
!
! -----------------------------------------------------------------------
!
      end module write_snap_pick_sph_spectr
