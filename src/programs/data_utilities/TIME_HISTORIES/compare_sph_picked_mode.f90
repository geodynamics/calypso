!>@file   compare_sph_picked_mode.f90
!!        program compare_sph_picked_mode
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!>@brief Compare picked mode data
!!@n
!!@n      Reference data: reference/picked_mode_l*_m*[sc].dat
!!@n      Compared data:          ./picked_mode_l*_m*[sc].dat
!!
!
      program compare_sph_picked_mode
!
      use m_precision
      use skip_comment_f
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_spectr_head_labels
!
      use set_parallel_file_name
      use select_gz_stream_file_IO
!
      use t_picked_sph_spectr_data_IO
      use t_pick_copy_monitor_data
      use picked_sph_spectr_data_IO
!
      implicit none
!
      character(len = kchara) :: fhead_rms_vol, fname_rms_vol
      character(len = kchara) :: fhead_rms_ref, fname_rms_ref
!
!
      integer(kind = kint), parameter :: id_file1 = 34, id_file2 = 36
      character, pointer :: FPz_f1, FPz_f2
      type(buffer_4_gzip) :: zbuf1, zbuf2
      type(read_sph_spectr_data) :: sph_IN1, sph_IN2
      type(picked_spectrum_data_IO) :: picked_IO1, picked_IO2
      type(monitor_field_pickup_table) :: comp_tbl12
      logical :: flag_gzip1, flag_gzip2
      logical :: flag_miss1, flag_miss2
      character(len = kchara) :: file_name
!
      logical :: error
      integer(kind = kint) :: ierr1, ierr2
      real(kind = kreal) :: diff
      real(kind = kreal), allocatable :: spectr_IN1(:)
      real(kind = kreal), allocatable :: spectr_IN2(:)
      integer(kind = kint) :: icomp, icomp2, icou
!
      integer(kind = kint) :: iflag_gl = 0
      character(len = kchara) :: charaint
      integer(kind = kint) :: lm, kr, ist, ipick
!
!
      if(iargc_kemo() .le. 1) then
        write(*,*) 'sph_picked_mode_check ',                            &
     &             'REFERENCE_FILE_PREFIX COMPARED_FILE_PREFIX'
        stop
      end if
      call getarg_k(1, fhead_rms_ref)
      call getarg_k(2, fhead_rms_vol)
      fname_rms_ref = add_dat_extension(fhead_rms_ref)
      fname_rms_vol = add_dat_extension(fhead_rms_vol)
!
!
      call sel_open_check_gz_stream_file(FPz_f1, id_file1,              &
     &   fname_rms_vol, flag_gzip1, flag_miss1, file_name, zbuf1)
      if(flag_miss1) then
        write(*,*) 'Data file ', trim(fname_rms_vol), ' is missing.'
        error = .TRUE.
        go to 99
      end if
      call read_pick_series_head(FPz_f1, id_file1, flag_gzip1,          &
     &                           picked_IO1, zbuf1)
      call alloc_pick_sph_monitor_IO(picked_IO1)
      call read_pick_series_comp_name(FPz_f1, id_file1, flag_gzip1,     &
     &                                picked_IO1, zbuf1)
!
      call sel_open_check_gz_stream_file(FPz_f2, id_file2,              &
     &   fname_rms_ref, flag_gzip2, flag_miss2, file_name, zbuf2)
      if(flag_miss2) then
        write(*,*) 'Data file ', trim(fname_rms_ref), ' is missing.'
        error = .TRUE.
        go to 99
      end if
      call read_pick_series_head(FPz_f2, id_file2, flag_gzip2,          &
     &                           picked_IO2, zbuf2)
      call alloc_pick_sph_monitor_IO(picked_IO2)
      call read_pick_series_comp_name(FPz_f2, id_file2, flag_gzip2,     &
     &                                picked_IO2, zbuf2)
!
      call init_pick_copy_sph_pwr_list                                  &
     &   (picked_IO1%ntot_comp, picked_IO2%ntot_comp,                   &
     &    picked_IO1%spectr_name(1), picked_IO2%spectr_name(1),         &
     &    comp_tbl12)
      if(comp_tbl12%fast_flag .eqv. .FALSE.) then
        write(*,*) 'Field list does not match'
        error = .TRUE.
        go to 99
      else
        write(*,*) 'Correct field list'
      end if
!
      do
        call read_sph_spec_monitor(FPz_f1, id_file1, flag_gzip1,        &
     &      sph_IN1%i_step, sph_IN1%time, picked_IO1, zbuf1, ierr1)
        call read_sph_spec_monitor(FPz_f2, id_file2, flag_gzip2,        &
     &      sph_IN2%i_step, sph_IN2%time, picked_IO2, zbuf2, ierr2)
        if(ierr1*ierr2 .gt. 0) exit
!
        error = .TRUE.
        do lm = 1, picked_IO1%num_mode
          do kr = 1, picked_IO1%num_layer
            ipick = kr + (lm-1) * picked_IO1%num_layer
            if(picked_IO1%idx_sph(ipick,1)                              &
                .ne. picked_IO2%idx_sph(ipick,1)) then
              write(*,*) 'Error in radial grid ID',                     &
     &                     picked_IO1%idx_sph(ipick,1),                 &
     &                     picked_IO2%idx_sph(ipick,1)
              go to 99
            end if
            if(picked_IO1%idx_sph(ipick,2)                              &
                .ne. picked_IO2%idx_sph(ipick,2)) then
              write(*,*) 'Error in merged harmonics ID',                &
     &                     picked_IO1%idx_sph(ipick,2),                 &
     &                     picked_IO2%idx_sph(ipick,2)
              go to 99
            end if
            if(picked_IO1%idx_sph(ipick,3)                              &
                .ne. picked_IO2%idx_sph(ipick,3)) then
              write(*,*) 'Error in hermonic degree',                    &
     &                     picked_IO1%idx_sph(ipick,3),                 &
     &                     picked_IO2%idx_sph(ipick,3)
              go to 99
            end if
!
            diff = compare_data(picked_IO1%radius(ipick),               &
     &                          picked_IO2%radius(ipick))
            if(abs(diff) .gt. 1.d-9) then
              write(*,*) 'Error in radius',                             &
     &                     picked_IO1%radius(ipick),                    &
     &                     picked_IO2%radius(ipick), diff
              go to 99
            end if
!
            ist = (ipick-1) * picked_IO1%ntot_comp
            do icomp = 1, picked_IO1%ntot_comp
              diff = compare_data(picked_IO1%d_pk(ist+icomp),           &
     &                            picked_IO2%d_pk(ist+icomp))
              if(abs(diff) .gt. 1.d-9) then
                write(*,*) 'Large error in ',                           &
     &           trim(picked_IO1%spectr_name(icomp)),                   &
     &           ': ', picked_IO1%d_pk(ist+icomp),                      &
     &                 picked_IO1%d_pk(ist+icomp), diff
                go to 99
              end if
            end do
          end do
        end do
      end do
      error = .FALSE.
!
  99  continue
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file1, flag_gzip1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f2, id_file2, flag_gzip2, zbuf2)
!
      if(error) then
        write(*,*) 'Picked hermonics data file ', trim(fname_rms_ref),  &
     &            ' and ', trim(fhead_rms_vol), ' does not match.'
        iflag_gl = 1
      else
        write(*,*) 'Picked hermonics data are consistent'
        iflag_gl = 0
      end if
!
      open(999,file='flag.txt')
      write(charaint,*) iflag_gl
      write(999,'(a)') trim(ADJUSTL(charaint))
      close(999)
!
      stop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine getarg_k(i, argc)
!
      integer, intent(in) :: i
      character(len=*), intent(out) :: argc
!
      call getarg(0, argc)
      if(argc == "") then
        call getarg(i + 1, argc)
      else
        call getarg(i, argc)
      end if
      end subroutine getarg_k
!
!   --------------------------------------------------------------------
!
      integer function iargc_kemo() result(oresult)
!
      integer :: iargc
      character(len=8) :: argc
      oresult = iargc()
      call getarg(0, argc)
      if(argc == "") then
        oresult = oresult - 1
      end if
      end function iargc_kemo
!
!   --------------------------------------------------------------------
!
      end program compare_sph_picked_mode
