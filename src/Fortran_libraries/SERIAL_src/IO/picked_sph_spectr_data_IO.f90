!>@file   picked_sph_spectr_data_IO.f90
!!@brief  module picked_sph_spectr_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_sph_spec_monitor(my_rank, i_step, time, picked)
!!
!!      subroutine open_sph_spec_read(id_pick, picked)
!!      subroutine read_sph_spec_monitor                                &
!!     &         (id_pick, i_step, time, picked, ierr)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module picked_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
!>      File ID for spectrum monitor file
      integer(kind = kint), parameter :: id_pick_mode = 22
!
      private :: open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_4_monitor(picked)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(picked_spectrum_data), intent(in) :: picked
!
      character(len = kchara) :: file_name
!
!
      call add_dat_extension(picked%file_prefix, file_name)
      open(id_pick_mode, file = file_name, form='formatted',            &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      close(id_pick_mode)
      open(id_pick_mode, file = file_name, form='formatted',            &
     &    status='replace')
!
      write(id_pick_mode,'(a)')    '#'
      write(id_pick_mode,'(a)')    '# num_layers, num_spectr'
      write(id_pick_mode,'(2i16)')                                      &
     &                           picked%num_layer, picked%num_sph_mode
      write(id_pick_mode,'(a)')    '# number of component'
      write(id_pick_mode,'(i16)') picked%ntot_comp_rj
!
      write(id_pick_mode,'(a)',advance='NO')  't_step    time    '
      write(id_pick_mode,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_pick_mode,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_pick_mode, picked%ntot_comp_rj,        &
     &    picked%spectr_name)

      write(id_pick_mode,'(a)') ''
!
      end subroutine open_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_spec_monitor(my_rank, i_step, time, picked)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, knum, ipick, i_fld
!
!
      if(picked%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      call open_sph_spec_4_monitor(picked)
!
      do inum = 1, picked%num_sph_mode
        do knum = 1, picked%num_layer
          ipick = knum + (inum-1) * picked%num_layer
          write(id_pick_mode,'(i16,1pe23.14e3)', advance='NO')          &
     &               i_step, time
          write(id_pick_mode,'(i16,1pe23.14e3,2i16)', advance='NO')     &
     &               picked%id_radius(knum), picked%radius_gl(knum),    &
     &               picked%idx_gl(inum,2:3)
          do i_fld = 1, picked%ntot_comp_rj
            write(id_pick_mode,'(1pe23.14e3)', advance='NO')            &
     &              picked%d_rj_gl(i_fld,ipick)
          end do
          write(id_pick_mode,'(a)') ''
        end do
      end do
!
      close(id_pick_mode)
!
      end subroutine write_sph_spec_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_sph_spec_read(id_pick, picked)
!
      use set_parallel_file_name
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_pick
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      integer(kind = kint) :: i
!
      character(len = kchara) :: file_name
      character(len=255) :: tmpchara
!
!
      call add_dat_extension(picked%file_prefix, file_name)
      open(id_pick, file = file_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked%num_layer, picked%num_sph_mode
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) picked%ntot_comp_rj
!
      call alloc_num_pick_layer(picked)
      call alloc_pick_sph_monitor(picked)
!
      read(id_pick,*) (tmpchara,i=1,6),                                 &
     &                 picked%spectr_name(1:picked%ntot_comp_rj)
!
      end subroutine open_sph_spec_read
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_spec_monitor                                  &
     &         (id_pick, i_step, time, picked, ierr)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: inum, knum, l, m, ipick
!
!
      ierr = 0
      do inum = 1, picked%num_sph_mode
        do knum = 1, picked%num_layer
          ipick = knum + (inum-1) * picked%num_layer
          read(id_pick,*,err=99,end=99) i_step, time,                   &
     &               picked%id_radius(knum), picked%radius_gl(knum),    &
     &               l, m, picked%d_rj_gl(1:picked%ntot_comp_rj,ipick)
          picked%idx_gl(inum,1) = get_idx_by_full_degree_order(l, m)
          picked%idx_gl(inum,2) = l
          picked%idx_gl(inum,3) = m
        end do
      end do
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_sph_spec_monitor
!
! -----------------------------------------------------------------------
!
      end module picked_sph_spectr_data_IO
